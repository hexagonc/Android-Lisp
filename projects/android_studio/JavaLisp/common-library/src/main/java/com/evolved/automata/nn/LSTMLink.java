package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public class LSTMLink extends Link {



    double learningRate;
    double momentum = 0;
    WeightMatrix prevWeightDelta;

    public LSTMLink(NodeGroup source, NodeGroup target, WeightMatrix weights, double learningRate, double momentum)
    {
        super( source,  target,  weights);
        this.learningRate = learningRate;
        prevWeightDelta = new WeightMatrix(targetLayer.getDimen(), sourceLayer.getDimen());
        this.momentum = momentum;
    }


    @Override
    public void updateWeights()
    {
        final WeightMatrix temp = calculatedGradient.multiply(learningRate);
        weights.mapD(new MatrixMapper() {
            @Override
            public double map(double c, int i, int j)
            {
                double value = c + temp.value(i,j);
                if (momentum > 0)
                    value += prevWeightDelta.value(i, j)* momentum;

                if (updateListener != null && allowLocalPerturbationOfWeightsP)
                {
                    updateListener.onUpdate(c, value, i, j);
                }
                return value;
            }
        });


        prevWeightDelta = temp;
        //resetWeightHistory();
    }

    @Override
    public void resetWeightHistory()
    {
        calculatedGradient.setValue(0);
        elligibilityTrace.setValue(0);
    }

    public void initializeWeights(boolean resetStats)
    {
        randomizeWeights(DEFAULT_MIN_RANDOM_WEIGHT, DEFAULT_MAX_RANDOM_WEIGHT, resetStats);
        prevWeightDelta.setValue(0);
        resetWeightHistory();
    }
    public void initializeWeights(WeightMatrix matrix)
    {
        initializeWeights(true);
        setWeights(matrix);

    }
}
