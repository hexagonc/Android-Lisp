package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 1/8/17.
 */
public interface SequencePredictor {

    public void clearAllSLSTMs();

    public SequenceLSTM[] getMembers();
    public Vector[] observePredictNext(Vector input, boolean learnP);

    public Vector[] observePredictNext();

    public Vector getBestPrediction(Vector input, boolean learnP);

    public Vector getBestPrediction();

    public void clearAllPredictions();

    public void setMaxLearningError(double max);

    public void setMaxLearningSteps(int steps);

    public Vector[] getPreviousPrediction();

    public Vector getBestPrediction(Vector[] pool);

    public void resetPredictions();

    public void setAllowMidSequenceInitialPrediction(boolean enable);

    public String getDataView();

    public String serializedForm();

    public void loadData(String serializedData);

    public SequencePredictor setPredictionEvaluator(PredictionComparator comp);

    public SequencePredictor setCustomPredictionAggregator(SequencePredictor.BestPredictionAggregator predictionAggregator);

    public interface PredictionComparator
    {
        double weighPrediction(Vector actual, Vector predicted, int predictorIndex);
        double getResetThresholdWeight();
    }

    public interface BestPredictionAggregator
    {
        Vector aggregateResult(Vector[] lastPrediction, int[] netMatchCounts, SequenceLSTM[] lstms);
    }

}
