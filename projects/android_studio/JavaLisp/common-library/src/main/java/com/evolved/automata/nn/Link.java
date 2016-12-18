package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public abstract class Link {

    public enum RANDOMIZATION_SCHEME
    {
        UNIFORM_RANGE,
        PERTRUBATION
    }

    public interface OnWeightUpdateListener
    {
        public void onUpdate(double oldWeight, double newWeight, int i, int j);
    }


    NodeGroup sourceLayer;
    NodeGroup targetLayer;
    WeightMatrix weights;
    WeightMatrix calculatedGradient;
    WeightMatrix elligibilityTrace;
    double DEFAULT_MIN_RANDOM_WEIGHT = -2;
    double DEFAULT_MAX_RANDOM_WEIGHT = 2;

    WeightMatrix minWeightDelta;
    WeightMatrix maxWeightDelta;

    double minWeight = Double.MIN_VALUE;
    double maxWeight = Double.MIN_VALUE;


    boolean allowLocalPerturbationOfWeightsP = true;
    OnWeightUpdateListener updateListener;
    public Link(NodeGroup source, NodeGroup target, WeightMatrix weights)
    {
        sourceLayer = source;
        targetLayer = target;
        this.weights = weights;

        init();
    }


    private void init()
    {
        calculatedGradient = new WeightMatrix(targetLayer.getDimen(), sourceLayer.getDimen());
        elligibilityTrace = new WeightMatrix(targetLayer.getDimen(), sourceLayer.getDimen());
        updateListener = getUpdateListener();

        if (allowLocalPerturbationOfWeightsP)
        {
            minWeightDelta = new WeightMatrix(targetLayer.getDimen(), sourceLayer.getDimen());
            maxWeightDelta = new WeightMatrix(targetLayer.getDimen(), sourceLayer.getDimen());
        }
        resetStats();
    }

    private OnWeightUpdateListener getUpdateListener()
    {
        return new OnWeightUpdateListener()
        {

            @Override
            public void onUpdate(double oldWeight, double newWeight, int i, int j)
            {
                minWeight = Math.min(newWeight, minWeight);
                maxWeight = Math.max(maxWeight, newWeight);

                minWeightDelta.setValue( Math.min(minWeightDelta.value(i, j), newWeight), i, j);
                maxWeightDelta.setValue(Math.max(maxWeightDelta.value(i, j), newWeight), i, j);
            }
        };
    }

    private void resetStats()
    {
        minWeight = Double.MIN_VALUE;
        maxWeight = Double.MIN_VALUE;
        if (allowLocalPerturbationOfWeightsP)
        {
            minWeightDelta.mapD(new MatrixMapper() {
                @Override
                public double map(double c, int i, int j)
                {
                    return Double.MAX_VALUE;
                }
            });

            maxWeightDelta.mapD(new MatrixMapper() {
                @Override
                public double map(double c, int i, int j)
                {
                    return Double.MIN_VALUE;
                }
            });


        }
    }

    public NodeGroup getSourceNodes()
    {
        return sourceLayer;
    }

    public NodeGroup getTargetNodes()
    {
        return targetLayer;
    }


    public WeightMatrix getWeights()
    {
        return weights;
    }

    public Link setWeights(WeightMatrix matrix)
    {
        weights = matrix;
        return this;
    }

    public Link randomizeWeights(final double lowerBound, final double upperBound, boolean reset)
    {
        if (reset)
            resetStats();
        weights.mapD(new MatrixMapper() {
            @Override
            public double map(double c, int i, int j)
            {
                return lowerBound + (upperBound - lowerBound)*Math.random();
            }
        });
        return this;

    }

    public Link randomizeWeights(final double lowerBound, final double upperBound)
    {
        return randomizeWeights(lowerBound, upperBound, true);
    }


    public Link randomizeWeights()
    {

        return randomizeWeights(getMinRandomWeightBound(), getMaxRandomWeightBound(), false);
    }

    public double getMinRandomWeightBound()
    {
        double min = minWeight;
        if (min == Double.MAX_VALUE)
            min = DEFAULT_MIN_RANDOM_WEIGHT;
        return min;
    }

    public double getMaxRandomWeightBound()
    {
        double max  = maxWeight;
        if (max == Double.MIN_VALUE)
            max = DEFAULT_MAX_RANDOM_WEIGHT;
        return max;
    }

    public double getMinRandomWeightBound(double min)
    {

        if (min == Double.MAX_VALUE)
            min = minWeight;

        if (min == Double.MAX_VALUE)
            min = DEFAULT_MIN_RANDOM_WEIGHT;

        return min;
    }

    public double getMaxRandomWeightBound(double max)
    {

        if (max == Double.MIN_VALUE)
            max = maxWeight;

        if (max == Double.MIN_VALUE)
            max = DEFAULT_MAX_RANDOM_WEIGHT;
        return max;
    }

    public double perturbationFraction = 2.0;
    boolean usedDefaultWeights = false;
    int dcount = 0;
    public Link perturbWeights()
    {
        initializeWeights(false);
        usedDefaultWeights = false;
        dcount = 0;
        weights.mapD(new MatrixMapper() {
            @Override
            public double map(double c, int i, int j)
            {
                double lowerBound = getMinRandomWeightBound(minWeightDelta.value(i, j));
                double upperBound = getMaxRandomWeightBound(maxWeightDelta.value(i, j));

                if (lowerBound == upperBound)
                {
                    lowerBound = DEFAULT_MIN_RANDOM_WEIGHT;
                    upperBound = DEFAULT_MAX_RANDOM_WEIGHT;
                    usedDefaultWeights = true;
                    dcount++;
                }
                double width =  perturbationFraction * (upperBound - lowerBound)*Math.random();
                return lowerBound - (upperBound - lowerBound)*perturbationFraction/2 + width;
            }
        });
        if (usedDefaultWeights)
            System.out.println("Used default weights: " + dcount);
        return this;

    }



    public Link feedforward()
    {
        /** This is redundant so long as all input activations to output gates are committed before
         * the Cell output is fed forward
        if (sourceLayer.getLayerType() == NetConfiguration.LayerType.CELLOUTPUT)
        {
            ((CellOutputLayer)sourceLayer).updateCellOutput();
        }
         */
        targetLayer.addNetinput(getForwardActivations());
        return this;
    }

    public Vector getForwardActivations()
    {
        return weights.multiply(sourceLayer.getActivation());
    }


    protected void updateElligibilityTrace()
    {
        switch (targetLayer.getLayerType())
        {
            case OUTPUT:
            case OUTPUTGATE:
            case HIDDEN:
                elligibilityTrace.mapD(new MatrixMapper() {
                    @Override
                    public double map(double c, int i, int j)
                    {
                        return sourceLayer.getActivation().value(j);
                    }
                });
                break;
            case CELLINPUT:
            {
                final Vector inputGateActivation = ((CellInputLayer) targetLayer).getMemoryCell().getInputGate().getActivation();
                final Vector forgetActivation = ((CellInputLayer) targetLayer).getMemoryCell().getForgetGate().getActivation();
                elligibilityTrace.mapD(new MatrixMapper() {
                    @Override
                    public double map(double c, int i, int j)
                    {
                        return c* forgetActivation.value(i) + inputGateActivation.value(i) * sourceLayer.getActivation().value(j);
                    }
                });
                break;
            }
            case INPUTGATE:
            {
                // Using cell input activation as originally proposed by Gers, Schraudolph, et al
                // Can also use the identity squashing function to make consistent with Monner and Reggia
                final Vector cellInputActivation = ((InputGateLayer) targetLayer).getMemoryCell().getCellInput().getActivation();
                final Vector forgetActivation = ((InputGateLayer) targetLayer).getMemoryCell().getForgetGate().getActivation();
                elligibilityTrace.mapD(new MatrixMapper() {
                    @Override
                    public double map(double c, int i, int j)
                    {
                        return c*forgetActivation.value(i) + cellInputActivation.value(i)*targetLayer.getDerivativeOfNetInput(i)*sourceLayer.getActivation().value(j);
                    }
                });
                break;
            }
            case FORGETGATE:
                final Vector state = ((ForgetGateLayer)targetLayer).getMemoryCell().getCellState();

                elligibilityTrace.mapD(new MatrixMapper() {
                    @Override
                    public double map(double c, int i, int j)
                    {
                        return c*targetLayer.getActivation().value(i) + state.value(i)*targetLayer.getDerivativeOfNetInput(i)*sourceLayer.getActivation().value(j);
                    }
                });
        }
    }

    public void calculatePartialGradient()
    {
        updateElligibilityTrace();
        final Vector targerErrorResponsiblity = targetLayer.getErrorResponsibility();
        calculatedGradient.mapD(new MatrixMapper() {
            @Override
            public double map(double c, int i, int j)
            {
                double negDerivative = elligibilityTrace.value(i, j)*targerErrorResponsiblity.value(i);
                return c + negDerivative;
            }
        });
    }

    public abstract void updateWeights();

    public abstract void resetWeightHistory();

    public abstract void initializeWeights(boolean resetStats);
    public abstract void initializeWeights(WeightMatrix matrix);
}
