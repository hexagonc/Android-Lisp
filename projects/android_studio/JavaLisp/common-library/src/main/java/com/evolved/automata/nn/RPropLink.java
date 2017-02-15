package com.evolved.automata.nn;



/**
 * Created by Evolved8 on 12/8/16.
 */
public class RPropLink extends Link {
    final double n_plus;
    final double n_minus;

    final double deltaMax;
    final double deltaMin;
    final double deltaInit;
    WeightMatrix weightDeltaMatrix;
    WeightMatrix prevCalculatedGradient;



    public RPropLink(NodeGroup source, NodeGroup target, WeightMatrix weights, double n_plus, double n_minus, double deltaMax, double deltaMin, double deltaInit)
    {
        super( source,  target,  weights);
        this.n_plus = n_plus;
        this.n_minus = n_minus;

        this.deltaInit = deltaInit;
        this.deltaMax = deltaMax;
        this.deltaMin = deltaMin;
        prevCalculatedGradient = new WeightMatrix(targetLayer.getDimen(), sourceLayer.getDimen());
        weightDeltaMatrix = new WeightMatrix(targetLayer.getDimen(), sourceLayer.getDimen());
        weightDeltaMatrix.setValue(deltaInit);
    }



    @Override
    public void updateWeights()
    {
        // calculated gradient is the negative of the actual gradient,
        // since we are doing gradient descent instead of gradient ascent,
        // hence -∂E/∂wij

        weightDeltaMatrix.mapD(new MatrixMapper() {
            @Override
            public double map(double delta, int i, int j)
            {
                if (prevCalculatedGradient.value(i, j) * calculatedGradient.value(i,j) > 0)
                {
                    return Math.min(delta * n_plus, deltaMax);
                }
                else if (prevCalculatedGradient.value(i, j) * calculatedGradient.value(i,j) < 0)
                {
                    calculatedGradient.setValue(0, i,j);
                    return Math.max(delta*n_minus, deltaMin);
                }
                else
                    return delta;
            }
        });

        // TODO: Combine this with above eventually
        prevCalculatedGradient.mapD(new MatrixMapper() {
            @Override
            public double map(double c, int i, int j)
            {
                return calculatedGradient.value(i, j);
            }
        });


        weights.mapD(new MatrixMapper() {
            @Override
            public double map(double old_weight, int i, int j)
            {
                double newWeight = old_weight - sign(calculatedGradient.value(i,j)) * weightDeltaMatrix.value(i, j);
                if (updateListener != null && allowLocalPerturbationOfWeightsP)
                {
                    updateListener.onUpdate(old_weight, newWeight, i, j);
                }
                return newWeight;
            }
        });
        calculatedGradient.setValue(0);
    }

    @Override
    public void calculatePartialGradient()
    {
        updateElligibilityTrace();
        final Vector targerErrorResponsiblity = targetLayer.getErrorResponsibility();
        calculatedGradient.mapD(new MatrixMapper() {
            @Override
            public double map(double c, int i, int j)
            {
                double negDerivative = elligibilityTrace.value(i, j) * targerErrorResponsiblity.value(i);
                return c - negDerivative;
            }
        });
    }

    @Override
    public void resetWeightHistory()
    {
        elligibilityTrace.setValue(0);

    }

    private double sign(double value)
    {
        if (value > 0)
            return 1;
        else if (value == 0)
            return 0;
        else
            return -1;
    }

    public void initializeWeights(boolean resetStats)
    {
        randomizeWeights(DEFAULT_MIN_RANDOM_WEIGHT, DEFAULT_MAX_RANDOM_WEIGHT, resetStats);
        resetWeightHistory();
        calculatedGradient.setValue(0);
        prevCalculatedGradient.setValue(0);
        weightDeltaMatrix.setValue(deltaInit);
    }
    public void initializeWeights(WeightMatrix matrix)
    {
        initializeWeights(true);
        setWeights(matrix);

    }
}
