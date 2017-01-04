package com.evolved.automata.nn;

import java.util.ArrayList;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 12/26/16.
 */
public class SLSTMSet {


    public interface StatePredictionTester
    {
        public boolean areMatched(Vector actualState, Vector predictedState);
    }

    public interface OutputAggregator
    {
        Vector aggregateResult(Vector[] lastPrediction, Vector hierarchicalAggregateResult);
    }

    public interface InputPropertyDetector
    {
        public boolean hasProperty(Vector input, SLSTMSet slstmSet);
    }

    public enum PatternRecognitionPolicy
    {
        CONTIUOUS, AT_PATTERN_BOUNDARY, IN_PATTERN_INTERIOR_WHEN_POSSIBLE
    }


    public enum TempSLSTMFlushPolicy
    {
        ON_DEMAND, IMMEDIATELY, NEVER
    }
    Vector previousObservationOutput = null;
    Vector previousInput = null;
    Vector[] predictedOutput = null;
    SequenceLSTM[] group;
    double[] matchHistory;
    int[] failureCount;
    int[] patternLength;

    boolean onlyPropateCompletionsToHigherLevel = true;
    TempSLSTMFlushPolicy flushPolicy = TempSLSTMFlushPolicy.IMMEDIATELY;



    int _ABSOLUTE_MAX_LSTM = 1000;
    int minIndex = 0;
    int maxLSTM;
    int lstmTempIndex = 0;

    SLSTMSet higherOrderPatterns;
    SLSTMSet childPatterns = null;
    int _HIGHER_ORDER_PATTERN_BUFFER_SIZE = 30;
    int _DEFAULT_HIGHER_ORDER_SLSTMSET_MEMORY_CELL_SIZE = 10;
    boolean previousMatchesP = false;
    double maxError = 0.1;
    int maxSteps = 300;
    int maxConsecutiveFailures = 2;


    StatePredictionTester predictionMatcher;
    OutputAggregator resultAggregator;
    VectorMapper observationOutputMapper;
    InputPropertyDetector commitInputDetector;
    InputPropertyDetector rollbackInputDetector;

    SequenceLSTM temp;

    LinkedList<SequenceLSTM> tempBuffer;
    String savedWeights;
    String savedStates;

    int inputNodeCount;
    int stateNodeCount;

    int markedMinIndex;
    int markedlstmTempIndex;
    PatternRecognitionPolicy recognitionPolicy = PatternRecognitionPolicy.AT_PATTERN_BOUNDARY;


    public SLSTMSet(int inputNodeCount, int stateNodeCount, int bufferSize)
    {
        this.inputNodeCount = inputNodeCount;
        this.stateNodeCount = stateNodeCount;
        maxLSTM = bufferSize;
        lstmTempIndex = 0;
        group = new SequenceLSTM[_ABSOLUTE_MAX_LSTM];
        matchHistory = new double[_ABSOLUTE_MAX_LSTM];
        failureCount = new int[_ABSOLUTE_MAX_LSTM];
        patternLength = new int[_ABSOLUTE_MAX_LSTM];
        tempBuffer = new LinkedList<SequenceLSTM>();

        higherOrderPatterns = null;
        predictionMatcher = new StatePredictionTester()
        {
            public boolean areMatched(Vector actualState, Vector predictedState)
            {
                return actualState.equals(predictedState);
            }
        };

        observationOutputMapper = new VectorMapper() {
            @Override
            public double map(double v, int i)
            {
                if (v > 0)
                    return 1;
                else
                    return 0;
            }
        };

        resultAggregator = getAveragingOutputAggregator();
        markIndexState(false);
    }

    public SLSTMSet addHierarchicalLayer(OutputAggregator aggregator, SLSTMSet higherLayer)
    {
        higherLayer.childPatterns = this;
        resultAggregator = aggregator;
        higherOrderPatterns = higherLayer;
        return higherOrderPatterns;
    }

    public SLSTMSet setPredictionAggregator(OutputAggregator aggregator)
    {
        resultAggregator = aggregator;
        return this;
    }



    public SLSTMSet addHierarchicalLayer(OutputAggregator aggregator)
    {

        resultAggregator = aggregator;
        higherOrderPatterns = new SLSTMSet(maxLSTM, _DEFAULT_HIGHER_ORDER_SLSTMSET_MEMORY_CELL_SIZE, _HIGHER_ORDER_PATTERN_BUFFER_SIZE);
        higherOrderPatterns.childPatterns = this;
        return higherOrderPatterns;
    }

    public SLSTMSet addHierarchicalLayer(SLSTMSet higherLayer)
    {
        higherLayer.childPatterns = this;
        higherOrderPatterns = higherLayer;
        return higherOrderPatterns;
    }

    public SLSTMSet addHierarchicalLayer()
    {
        higherOrderPatterns = new SLSTMSet(maxLSTM, _DEFAULT_HIGHER_ORDER_SLSTMSET_MEMORY_CELL_SIZE, _HIGHER_ORDER_PATTERN_BUFFER_SIZE);
        higherOrderPatterns.childPatterns = this;
        return higherOrderPatterns;
    }


    public SLSTMSet clearAllSLSTMs()
    {
        minIndex = lstmTempIndex = 0;
        markIndexState(false);
        temp = null;
        previousMatchesP = false;
        predictedOutput = null;
        return this;
    }

    public SLSTMSet setMaxLearningError(double error)
    {
        this.maxError = error;
        return this;
    }

    public SLSTMSet setMaxLearningSteps(int steps)
    {
        this.maxSteps = steps;
        return this;
    }

    public int getNumPatterns()
    {
        if (minIndex <= lstmTempIndex)
            return lstmTempIndex - minIndex;
        else
            return lstmTempIndex + (_ABSOLUTE_MAX_LSTM - minIndex);
    }

    public void markIndexState(boolean saveTemp)
    {
        if (saveTemp && temp!=null && temp.getSequenceLength() > 1)
        {
            addSLSTM(temp);
            temp = null;
        }
        markedMinIndex = minIndex;
        markedlstmTempIndex = lstmTempIndex;
        if (higherOrderPatterns != null)
            higherOrderPatterns.markIndexState(saveTemp);
    }

    public void restoreState(boolean clearTemp)
    {
        minIndex = markedMinIndex;
        lstmTempIndex = markedlstmTempIndex;

        if (clearTemp)
        {
            temp = null;
            tempBuffer.clear();
        }

        if (higherOrderPatterns != null)
            higherOrderPatterns.restoreState(clearTemp);
    }

    public void restoreState()
    {
        restoreState(true);

    }

    public SLSTMSet setStatePredictionTester(StatePredictionTester tester)
    {
        predictionMatcher = tester;
        return this;
    }

    public SLSTMSet setMembers(SequenceLSTM[] inputs)
    {
        clearAllSLSTMs();
        for (SequenceLSTM s: inputs)
        {
            addSLSTM(s);
        }
        return this;
    }


    public SequenceLSTM[] getMembers()
    {
        SequenceLSTM[] out =new SequenceLSTM[getNumPatterns()];
        for (int k = 0;k<getNumPatterns();k++)
        {
            out[k] = group[relativeToAbsoluteIndex(k)];
        }

        return out;
    }


    public SLSTMSet addSLSTM(SequenceLSTM lstm)
    {
        group[lstmTempIndex] = lstm;
        patternLength[lstmTempIndex] = lstm.getSequenceLength();

        if (getNumPatterns() > maxLSTM)
        {
            minIndex = (minIndex + 1) % _ABSOLUTE_MAX_LSTM;
        }
        lstmTempIndex = (lstmTempIndex + 1) % _ABSOLUTE_MAX_LSTM;

        return this;
    }

    public Vector getLastPatternCompletion()
    {
        double[] completion = new double[maxLSTM];
        int i;
        for (int k = 0;k < getNumPatterns();k++)
        {
            i = relativeToAbsoluteIndex(k);
            if (patternLength[i] > 0)
            {
                completion[k] = matchHistory[i]/patternLength[i];
            }
        }

        return new Vector(completion);
    }

    public Vector[] getLastPredictedOutput()
    {
        return predictedOutput;
    }

    public Vector getAggregatePrediction()
    {
        if (predictedOutput == null)
            return null;

        Vector higherResult = null;
        if (higherOrderPatterns != null)
            higherResult = higherOrderPatterns.getAggregatePrediction();

        return resultAggregator.aggregateResult(predictedOutput, higherResult);
    }

    protected int relativeToAbsoluteIndex(int relative)
    {
        return (relative + minIndex) % _ABSOLUTE_MAX_LSTM;
    }

    public SLSTMSet setPatternRecognitionPolicy(PatternRecognitionPolicy policy)
    {
        recognitionPolicy = policy;
        return this;
    }

    public PatternRecognitionPolicy getPatternRecognitionPolicy()
    {

        return recognitionPolicy;
    }

    public Vector observe(Vector newInput, boolean addIfNoPresent)
    {
        Vector[] nextPredictedOutput = new Vector[maxLSTM];
        boolean anyCompletionsP = false;
        previousInput = newInput;

        int score = 0;
        int i;
        int numPatterns = getNumPatterns();
        double[] historySlice = new double[maxLSTM];
        if (previousMatchesP || recognitionPolicy == PatternRecognitionPolicy.CONTIUOUS)
        {
            for (int k = 0;k < numPatterns;k++)
            {
                i = relativeToAbsoluteIndex(k);

                if (predictedOutput!=null && predictedOutput[k] != null && predictionMatcher.areMatched(newInput, predictedOutput[k]))
                {
                    matchHistory[i]++;
                    failureCount[i] = 0;
                }
                else
                {
                    failureCount[i] = failureCount[i] + 1;
                    matchHistory[i] = Math.max(0, matchHistory[i] - 1);
                    if (maxConsecutiveFailures < failureCount[i])
                    {
                        matchHistory[i] = 0;
                        failureCount[i] = 0;
                    }
                }

                if (matchHistory[i] > 0 || recognitionPolicy == PatternRecognitionPolicy.CONTIUOUS || recognitionPolicy == PatternRecognitionPolicy.IN_PATTERN_INTERIOR_WHEN_POSSIBLE)
                {
                    group[i].viewSequenceOutput(new Vector[]{newInput}, true);
                    nextPredictedOutput[k] = group[i].getOutputValues();
                }

                score+= matchHistory[i];
                historySlice[k] = matchHistory[i];
                anyCompletionsP = anyCompletionsP || (((double)matchHistory[i])/patternLength[i])  == 1;
            }
        }

        if (score == 0 || !previousMatchesP || recognitionPolicy == PatternRecognitionPolicy.CONTIUOUS)
        {
            for (int k = 0;k < numPatterns;k++)
            {
                i = relativeToAbsoluteIndex(k);

                if (matchHistory[i] != 0) // only true if recognitionPolicy == PatternRecognitionPolicy.CONTIUOUS
                {
                    continue;
                }
                if (predictionMatcher.areMatched(newInput, group[i].getInitialInputActivation()))
                {
                    group[i].viewSequenceOutput(new Vector[]{newInput}, false);
                    nextPredictedOutput[k] = group[i].getOutputValues();
                    matchHistory[i] = 1;
                }
                else
                {
                    matchHistory[i] = 0;
                }
                score+= matchHistory[i];
                historySlice[k] = matchHistory[i];
            }
        }

        if (score == 0 && addIfNoPresent)
        {
            if (temp == null)
            {
                temp = NNTools.getStandardSequenceLSTM(inputNodeCount, stateNodeCount, null);
            }
        }

        if (temp != null && addIfNoPresent)
        {
            double[] result = temp.add(newInput, maxSteps, maxError);
            if (result[0] <= maxError)
            {
                saveTempState();
            }
            else
            {
                restoreTempLSTMState();

                if (score == 0)
                {

                    switch (flushPolicy)
                    {
                        case IMMEDIATELY:
                            patternLength[lstmTempIndex] = temp.getSequenceLength();
                            group[lstmTempIndex] = temp;
                            lstmTempIndex++;
                            tempBuffer.clear();
                            temp = NNTools.getStandardSequenceLSTM(inputNodeCount, stateNodeCount, null);
                            temp.add(newInput, maxSteps, maxError);
                            break;
                        case ON_DEMAND:
                            tempBuffer.add(temp);
                            temp = NNTools.getStandardSequenceLSTM(inputNodeCount, stateNodeCount, null);
                            temp.add(newInput, maxSteps, maxError);
                            break;

                    }

                }
                else
                {
                    boolean savedValue = false;
                    while (!savedValue && temp.getSequenceLength()>1)
                    {
                        temp.removeFirst();
                        result = temp.add(newInput, maxSteps, maxError);
                        savedValue = result[0] <= maxError;
                    }
                }

            }
        }
        previousMatchesP = score > 0;
        if (!previousMatchesP)
        {
            predictedOutput = null;
        }
        else
        {
            predictedOutput = nextPredictedOutput;
        }

        previousObservationOutput = new Vector(historySlice).mapD(observationOutputMapper);

        if (higherOrderPatterns != null && (!onlyPropateCompletionsToHigherLevel || anyCompletionsP ))
            higherOrderPatterns.observe(previousObservationOutput, addIfNoPresent);

        return previousObservationOutput;
    }

    public void flushTempBuffer(boolean withoutAddingP)
    {
        if (!withoutAddingP)
        {
            for (SequenceLSTM slstm : tempBuffer)
            {
                addSLSTM(slstm);
            }
        }
        tempBuffer.clear();
    }

    public SLSTMSet setTempBufferFlushPolicy(TempSLSTMFlushPolicy policy)
    {
        flushPolicy = policy;
        if (policy == TempSLSTMFlushPolicy.IMMEDIATELY && tempBuffer.size()> 0)
        {
            flushTempBuffer(false);
        }
        return this;
    }

    public void setDefaltCommitInputDetector()
    {
        setAutoCommitInputDetector(getDefaultCommitDetector());

    }

    public void setPropagateAllObservationsToHigherLevel(boolean flag)
    {
        onlyPropateCompletionsToHigherLevel = !flag;
    }

    public void setDefaltRollbackInputDetector()
    {
        setAutoRollbackInputDetector(getDefaultRollbackDetector());
    }

    public void setAutoCommitInputDetector(InputPropertyDetector inputDetector)
    {
        commitInputDetector = inputDetector;
    }

    public void setAutoRollbackInputDetector(InputPropertyDetector inputDetector)
    {
        rollbackInputDetector  = inputDetector;
    }

    InputPropertyDetector getDefaultCommitDetector()
    {
        return new InputPropertyDetector()
        {

            @Override
            public boolean hasProperty(Vector input, SLSTMSet slstmSet)
            {
                SLSTMSet child = slstmSet.childPatterns;
                if (child != null)
                {
                    Vector completions = child.getLastPatternCompletion();
                    if (completions != null)
                    {
                        double[] raw = completions.raw();

                        for (int i = 0;i < raw.length;i++)
                        {
                            if (raw[i] == 1.0)
                                return true;
                        }
                    }

                }
                return false;
            }
        };
    }


    InputPropertyDetector getDefaultRollbackDetector()
    {
        return new InputPropertyDetector()
        {

            @Override
            public boolean hasProperty(Vector input, SLSTMSet slstmSet)
            {
                SLSTMSet child = slstmSet.childPatterns;
                if (child != null)
                {
                    Vector completions = child.getLastPatternCompletion();
                    if (completions != null)
                    {
                        double[] raw = completions.raw();

                        for (int i = 0;i < raw.length;i++)
                        {
                            if (raw[i] == 1.0)
                                return true;
                        }
                    }

                }
                return false;
            }
        };
    }

    public OutputAggregator getAveragingOutputAggregator()
    {

        return new OutputAggregator()
        {

            double weightSum = 0;

            @Override
            public Vector aggregateResult(final Vector[] lastPrediction, final Vector hierarchicalAggregateResult)
            {

                final boolean hasHigherResult =  hierarchicalAggregateResult != null;

                if (lastPrediction == null && hasHigherResult)
                {
                    ArrayList<Vector> choiceList = new ArrayList<Vector>();
                    for (int k = 0;k < hierarchicalAggregateResult.dimen();k++)
                    {
                        if (hierarchicalAggregateResult.value(k) > 0)
                        {
                            choiceList.add(group[relativeToAbsoluteIndex(k)].getInitialInputActivation());
                        }

                    }

                    if (choiceList.size() > 0)
                    {
                        return choiceList.get((int)(choiceList.size()*Math.random()));
                    }
                    else
                        return null;
                }


                if (hasHigherResult)
                {
                    weightSum = hierarchicalAggregateResult.value(0);
                }
                else
                    weightSum = 1;

                Vector average = lastPrediction[0].add(0).multiplyD(weightSum);

                double weight;
                for (int k = 1;k < lastPrediction.length;k++)
                {
                    if (lastPrediction[k] == null)
                        continue;
                    weight = 1;
                    if (hasHigherResult)
                        weight = hierarchicalAggregateResult.value(k);
                    average.addD(lastPrediction[k]).multiplyD(weight);
                    weightSum+=weight;
                }

                return average.mapD(new VectorMapper() {
                    @Override
                    public double map(double v, int i)
                    {
                        return   v/weightSum;
                    }
                });
            }
        };
    }



    //TODO: instead of serializing the weight and node state, just save the raw data to speed
    //      this up.  Make sure you profile this on Android
    private void saveTempState()
    {
        savedWeights = temp.serializeLinkWeights();
        savedStates = temp.serializeNetworkActivationState();
    }

    private void restoreTempLSTMState()
    {
        temp.decodeSerializedLinksToLinkBuffer(savedWeights);
        temp.loadbufferedLinkWeights();
        temp.loadSerializedNetworkActivationState(savedStates);
        temp.removeLast();
    }


}
