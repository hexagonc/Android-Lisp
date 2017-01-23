package com.evolved.automata.nn;

import com.evolved.automata.AITools;
import com.evolved.automata.WeightedValue;

import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.PriorityQueue;

/**
 * Created by Evolved8 on 1/14/17.
 */
public class LinkedLSTMSet implements SequencePredictor{

    public interface OutputAggregator
    {
        Vector aggregateResult(Vector[] lastPrediction);
    }


    public enum PredictionAggregateMethod
    {
        STOCHASTIC_MAX, SIMPLE_MAX, CUSTOM

    }

    public enum MatchCountResetPolicy
    {
        ALWAYS_ZERO_ON_FAILURE, USE_PADDING_ON_TRANSITION, ALWAYS_USE_PADDING
    }


    VectorViewer dataViewer = null;
    boolean allowParentSupportOfChildSegmentP = true;
    boolean allowFailingLSTMsToPredictP = false;
    double initialMatchCountLengthFraction = 0.25;
    int maxFailureCount = 2;
    int initialMatchCount = 3;
    RingBufferManager bufferManager;
    int[] failureCounts = null;
    int[] matchCounts = null;
    SequenceLSTM[] segments = null;
    Integer currentSegmentId = null;
    Integer parentSegmentId = null;
    int MAX_LSTM = 100;
    Vector[] currentPredictions;
    Vector[] previousPredictions;
    HashMap<Integer, HashMap<Integer, Integer>> childMap;
    MatchCountResetPolicy matchCountResetPolicy = MatchCountResetPolicy.ALWAYS_ZERO_ON_FAILURE;
    boolean forceMidSequencePredictionP = false;
    double maxError = 0.1;
    int maxSteps = 200;
    SequenceLSTM currentLSTM = null;
    int _inputNodeCount;
    int _memoryCellCount;
    SequenceLSTM.LSTMNetworkBuilder builder;

    boolean allowIgnoreNextInput = false;
    PredictionAggregateMethod predictionAggregateMethod = null;
    OutputAggregator predictionAggregator;

    String LSTM_ELEMENT_DELIMITER = ",";
    String LSTM_DELIMITER = ":";

    int initialLearningMatchThreshold = 4;
    int maxPreviousMatchCount= 0;
    Integer previousRecycledSlot = null;
    int[] previousFinishedSegments = null;

    public static class Builder
    {
        MatchCountResetPolicy matchCountResetPolicy = MatchCountResetPolicy.ALWAYS_ZERO_ON_FAILURE;
        int numMemoryCellNodes;
        int numInputOutputNodes;
        double maxError = 0.1;
        int maxNumLearningStepsPerItem = 200;
        int numLSTM = 100;
        int initialMatchCount = 3;
        double initialMatchFraction = 0.25;
        int maxConsecutiveFailures = 2;
        PredictionAggregateMethod predictionAggregateMethod = null;
        OutputAggregator predictionAggregator;
        boolean allowFailingLSTMsToPredictP = false;
        boolean allowParentSupportOfChildSegmentP = true;
        VectorViewer dataViewer = null;
        int initialLearningMatchThreshold = 4;

        public Builder()
        {
            setSimpleMaxPredictionAggregator();

        }

        public Builder setNewPatternResetMatchCount(int resetMatchCount)
        {
            initialLearningMatchThreshold = resetMatchCount;
            return this;
        }

        public Builder setCustomVectorViewer(VectorViewer dataViewer)
        {
            this.dataViewer = dataViewer;
            return this;
        }

        public Builder setParentSegmentChildSupportPolicy(boolean allowParentSupport)
        {
            allowParentSupportOfChildSegmentP = allowParentSupport;
            return this;
        }
        public Builder setAllowFailingLSTMsToPredict()
        {
            allowFailingLSTMsToPredictP = true;
            return this;
        }

        public Builder setPreventFailingLSTMsToPredict()
        {
            allowFailingLSTMsToPredictP = false;
            return this;
        }

        public Builder setMatchCountResetPolicy(MatchCountResetPolicy matchCountResetPolicy)
        {
            this.matchCountResetPolicy = matchCountResetPolicy;
            return this;
        }



        public Builder setStochasticMaxPredictionAggregator()
        {
            predictionAggregateMethod = PredictionAggregateMethod.STOCHASTIC_MAX;
            return this;
        }

        public Builder setSimpleMaxPredictionAggregator()
        {
            predictionAggregateMethod = PredictionAggregateMethod.SIMPLE_MAX;
            return this;
        }

        public Builder setCustomPredictionAggregator(OutputAggregator predictionAggregator)
        {
            predictionAggregateMethod = PredictionAggregateMethod.CUSTOM;
            this.predictionAggregator = predictionAggregator;
            return this;
        }


        public Builder setNumInputNodes(int nodes)
        {
            numInputOutputNodes = nodes;
            return this;
        }

        public Builder setNumMemoryCellNodes(int nodes)
        {
            numMemoryCellNodes = nodes;
            return this;
        }

        public Builder setMaxError(double error)
        {
            maxError = error;
            return this;
        }

        public Builder setMaxLearningSteps(int steps)
        {
            maxNumLearningStepsPerItem = steps;
            return this;
        }

        public Builder setLSTMBufferSize(int size)
        {
            numLSTM = size;
            return this;
        }

        public Builder setTransitionMatchCount(int base)
        {
            initialMatchCount = base;
            return this;
        }

        public Builder setTransitionMatchFraction(double startFraction)
        {
            initialMatchFraction = startFraction;
            return this;
        }

        public Builder setMaxConsecutiveFailures(int threshold)
        {
            maxConsecutiveFailures = threshold;
            return this;
        }


        public LinkedLSTMSet build()
        {
            LinkedLSTMSet out = new LinkedLSTMSet(numInputOutputNodes, numMemoryCellNodes, numLSTM);
            out.maxFailureCount = maxConsecutiveFailures;
            out.initialMatchCount = initialMatchCount;
            out.maxSteps = maxNumLearningStepsPerItem;
            out.maxError = maxError;
            out.initialMatchCountLengthFraction = initialMatchFraction;
            out.maxFailureCount = maxConsecutiveFailures;

            out.predictionAggregateMethod = predictionAggregateMethod;
            switch (predictionAggregateMethod)
            {
                case CUSTOM:
                    out.predictionAggregator = predictionAggregator;
                    break;
                case SIMPLE_MAX:
                    out.predictionAggregator = out.getDeterministicMaxAggregator();
                    break;
                case STOCHASTIC_MAX:
                    out.predictionAggregator = out.getStochasticMaxAggregator();
                    break;
            }
            out.matchCountResetPolicy = matchCountResetPolicy;
            out.allowFailingLSTMsToPredictP = allowFailingLSTMsToPredictP;
            out.allowParentSupportOfChildSegmentP =  allowParentSupportOfChildSegmentP;
            out.dataViewer = dataViewer;
            out.initialLearningMatchThreshold = initialLearningMatchThreshold;
            return out;
        }

    }



    private LinkedLSTMSet(int inputNodeCount, int stateNodeCount, int maxBuffer)
    {

        childMap = new HashMap<Integer, HashMap<Integer, Integer>>();
        bufferManager = new RingBufferManager(maxBuffer);
        _inputNodeCount = inputNodeCount;
        _memoryCellCount = stateNodeCount;
        MAX_LSTM =maxBuffer;
        matchCounts = new int[MAX_LSTM];
        segments = new SequenceLSTM[MAX_LSTM];
        failureCounts = new int[MAX_LSTM];
        builder = NNTools.getStandardSequenceLSTMBuilder(inputNodeCount, stateNodeCount, null);
        builder.setEndCapPolicy(SequenceLSTM.EndCapPolicy.BOTH_ENDS);
        builder.setCapDisplayPolicy(SequenceLSTM.EndCapDisplayPolicy.HIDE);

    }

    private void incrementParentChildRelationShip(Integer parent, Integer child)
    {
        HashMap<Integer, Integer> children = childMap.get(parent);
        Integer previous;
        if (children == null)
        {
            children = new HashMap<Integer, Integer>();

            childMap.put(parent, children);
        }

        previous = children.get(child);
        if (previous == null)
        {
            previous = Integer.valueOf(0);
        }

        children.put(child, Integer.valueOf(previous + 1));
    }

    private Integer[] getSortedChildren(Integer parent)
    {
        final HashMap<Integer, Integer> children = childMap.get(parent);
        Integer previous;
        if (children == null)
        {
            return new Integer[0];
        }
        int L = children.size();
        Integer[] ranked = new Integer[L];

        PriorityQueue<Integer> rank = new PriorityQueue<Integer>(children.size(), new Comparator<Integer>() {
            @Override
            public int compare(Integer lvalue, Integer rvalue)
            {
                Integer lcount = children.get(lvalue);
                Integer rcount = children.get(rvalue);
                return -1* lcount.compareTo(rcount);
            }
        });


        for (Integer child: children.keySet())
        {
            rank.add(child);
        }

        for (int i = 0;i < L;i ++)
        {
            ranked[i] = rank.poll();
        }

        return ranked;
    }




    protected int getPredictionResetMatchCount(int lstmIndex)
    {
        if (segments[lstmIndex] == null)
        {
            return 0;
        }

        switch (matchCountResetPolicy)
        {
            case ALWAYS_USE_PADDING:
                return Math.min((int)(segments[lstmIndex].getSequenceLength()*initialMatchCountLengthFraction), initialMatchCount );
            case ALWAYS_ZERO_ON_FAILURE:
            case USE_PADDING_ON_TRANSITION:
                return 0;
        }
        return 0;
    }

    protected int getInitialMatchCount(int lstmIndex)
    {
        return 0;
    }

    protected int getLSTMTransitionMatchCount(int lstmIndex)
    {
        if (segments[lstmIndex] == null)
        {
            return 0;
        }

        switch (matchCountResetPolicy)
        {
            case USE_PADDING_ON_TRANSITION:
            case ALWAYS_USE_PADDING:
                return Math.min((int)(segments[lstmIndex].getSequenceLength()*initialMatchCountLengthFraction), initialMatchCount );
            case ALWAYS_ZERO_ON_FAILURE:
                return 0;
        }
        return 0;
    }

    private double getParentChildTransitionCount(Integer parent)
    {
        int count = 0;
        HashMap<Integer, Integer> children = childMap.get(parent);
        if (children != null)
        {
            for (Integer childKey:children.keySet())
            {
                count+=children.get(childKey);
            }
        }
        return count;
    }

    private void updateCurrentSegment()
    {

        if (bufferManager.getNumberOfUnclaimedSlots() == 0)
        {
            currentSegmentId = bufferManager.getOldestClaimedSlot(); // focuses attention on oldest slot
            if (previousRecycledSlot != currentSegmentId)
            {
                if (previousRecycledSlot != null)
                {
                    matchCounts[previousRecycledSlot] = getInitialMatchCount(previousRecycledSlot);
                }
                segments[currentSegmentId].clear();
                previousRecycledSlot = currentSegmentId;
            }

        }

        if (currentSegmentId == null) // creating lstm for the first time for this slot
        {
            currentSegmentId = bufferManager.tryClaimBestSlot(); // creates a new slot
            if (currentSegmentId.intValue() != -1)
                segments[currentSegmentId] = currentLSTM = builder.build();
            else
                currentSegmentId = null;
        }
    }

    public OutputAggregator getStochasticMaxAggregator()
    {
        return new OutputAggregator() {
            @Override
            public Vector aggregateResult(Vector[] lastPrediction)
            {
                if (lastPrediction == null)
                    return null;

                List<WeightedValue<Vector>> predictionList = new LinkedList<WeightedValue<Vector>>();
                for (int i = 0;i < MAX_LSTM;i++)
                {
                    if (matchCounts[i]>0)
                        predictionList.add(new WeightedValue<Vector>(lastPrediction[i], matchCounts[i]));
                }

                if (predictionList.size()>0)
                {
                    return AITools.ChooseWeightedRandomFair(predictionList).GetValue();
                }

                return null;
            }
        };

    }

    public OutputAggregator getDeterministicMaxAggregator()
    {
        return new OutputAggregator() {
            @Override
            public Vector aggregateResult(Vector[] lastPrediction)
            {
                if (lastPrediction == null)
                    return null;

                int maxCount = Integer.MIN_VALUE;
                Vector maxVector = null;
                int L = MAX_LSTM;
                for (int i = 0;i < L;i++)
                {
                    if (lastPrediction[i] == null)
                        continue;
                    if (matchCounts[i]>0 && ((matchCounts[i] - failureCounts[i]) > maxCount))
                    {

                        maxCount = matchCounts[i] - failureCounts[i];
                        maxVector = lastPrediction[i];
                    }
                }


                return maxVector;
            }
        };

    }

    public LinkedLSTMSet ignoreNextInput()
    {
        allowIgnoreNextInput = true;
        return this;
    }

    @Override
    public String getDataView()
    {
        return viewListStructure(false, true);
    }

    @Override
    public String serializedForm()
    {
        return null;
    }

    @Override
    public void loadData(String serializedData)
    {

    }

    @Override
    public SequencePredictor setPredictionEvaluator(PredictionComparator comp)
    {
        return null;
    }

    public String viewListStructure(boolean columnP, boolean onlyDefinedP)
    {

        StringBuilder out = new StringBuilder();
        String separator = null;
        if (columnP)
            separator = "\n";
        else
            separator = ",";

        String svalue;

        for (int i = 0;i<MAX_LSTM;i++)
        {
            if (segments[i] != null)
            {
                if (matchCounts[i] > 0) // calling toString on a SequenceLSTM modifies its state
                    svalue = "busy";
                else
                    svalue = segments[i].toString(dataViewer, LSTM_ELEMENT_DELIMITER);
            }
            else
                svalue = "null";

            if (columnP)
            {
                out.append(svalue).append(separator);
            }
            else
            {
                if (i > 0)
                    out.append(separator);
                out.append(svalue);
            }

        }

        return out.toString();

    }

    public static LinkedLSTMSet.Builder getBuider()
    {
        return new Builder();
    }

    public void setAllowMidSequenceInitialPrediction(boolean enable)
    {
        forceMidSequencePredictionP = enable;
    }


    @Override
    public Vector[] observePredictNext(Vector input, boolean learnP)
    {
        double[] learningResult = null;

        LinkedList<Integer> endOfSegmentProcessQueue = new LinkedList<Integer>();

        boolean wasPreviouslyMatching = maxPreviousMatchCount > initialLearningMatchThreshold;
        if (learnP)
        {

            if (!wasPreviouslyMatching)
            {

                updateCurrentSegment();

                if (currentSegmentId != null)
                {
                    learningResult = segments[currentSegmentId].add(input, maxSteps, maxError);
                    matchCounts[currentSegmentId] = -1;
                    if (learningResult[0] > maxError)
                    {
                        if (parentSegmentId != null)
                        {
                            incrementParentChildRelationShip(parentSegmentId, currentSegmentId);

                        }

                        matchCounts[currentSegmentId] = getInitialMatchCount(currentSegmentId);
                        bufferManager.refreshClaim(currentSegmentId);
                        parentSegmentId = currentSegmentId;
                        // Finished with previous segment
                        currentSegmentId = null;
                        updateCurrentSegment();
                        if (currentSegmentId != null)
                        {
                            segments[currentSegmentId].add(input, maxSteps, maxError);
                            matchCounts[currentSegmentId] = -1;
                        }

                    }
                }

            }

        }



        int numMappedLSTM = bufferManager.getNumberOfClaimedSlots();

        SequenceLSTM slstm = null;
        if (previousPredictions == null)
            previousPredictions = new Vector[MAX_LSTM];


        Vector initialPrediction = null;
        boolean predictionsP = false;
        boolean noPriorPredictions = currentPredictions == null;
        maxPreviousMatchCount= Integer.MIN_VALUE;
        for (int i = 0;i < numMappedLSTM;i++)
        {
            maxPreviousMatchCount = Math.max(matchCounts[i], maxPreviousMatchCount);
            if (matchCounts[i] < 0)
                continue;
            slstm = segments[i];

            if (!noPriorPredictions && currentPredictions[i] != null)
            {
                previousPredictions[i] = currentPredictions[i];
                if (currentPredictions[i].equals(input))
                {
                    currentPredictions[i] = slstm.viewSequenceOutput(new Vector[]{input}, true)[0];
                    matchCounts[i]++;

                    predictionsP = true;
                }
                else if (allowIgnoreNextInput)
                {
                    currentPredictions[i] = slstm.viewSequenceOutput(new Vector[]{currentPredictions[i]}, true)[0];
                    predictionsP = true;
                }
                else
                {
                    matchCounts[i]= Math.max(0, matchCounts[i] - 1);
                    failureCounts[i]++;
                    if (matchCounts[i] > 0 && failureCounts[i]<=maxFailureCount)
                    {
                        currentPredictions[i] = slstm.viewSequenceOutput(new Vector[]{input}, true)[0];
                        predictionsP = true;
                    }
                    else
                    {
                        matchCounts[i] = 0;
                        failureCounts[i] = 0;
                        currentPredictions[i] = null;


                    }
                }

                if (slstm.atListEnd())
                {
                    endOfSegmentProcessQueue.add(Integer.valueOf(i));
                }

            }
            else if (allowFailingLSTMsToPredictP || noPriorPredictions)
            {
                if (currentPredictions == null)
                    currentPredictions = new Vector[MAX_LSTM];
                matchCounts[i] = getPredictionResetMatchCount(i);
                slstm.resetToStartOfSequence();
                initialPrediction = slstm.getOutputValuesExternal();
                if (initialPrediction.equals(input))
                {
                    matchCounts[i] = matchCounts[i] + 1;
                    currentPredictions[i] = slstm.viewSequenceOutput(new Vector[]{input}, true)[0];
                    if (!slstm.atListEnd())
                        predictionsP = true;
                    failureCounts[i] = 0;
                }
                else if (allowIgnoreNextInput)
                {
                    currentPredictions[i] = slstm.viewSequenceOutput(new Vector[]{currentPredictions[i]}, true)[0];
                    if (!slstm.atListEnd())
                        predictionsP = true;
                }
                else
                {
                    failureCounts[i]++;
                    matchCounts[i]= Math.max(0, matchCounts[i] - 1);
                    if (matchCounts[i] > 0 && failureCounts[i]<=maxFailureCount)
                    {
                        currentPredictions[i] = slstm.viewSequenceOutput(new Vector[]{input}, true)[0];
                        if (!slstm.atListEnd())
                            predictionsP = true;
                    }
                    else
                    {
                        currentPredictions[i] = null;
                        failureCounts[i] = 0;
                        matchCounts[i] = 0;
                    }
                }

                if (slstm.atListEnd())
                {
                    endOfSegmentProcessQueue.add(Integer.valueOf(i));
                }
            }


        }

        double addFraction, total;
        previousFinishedSegments = new int[endOfSegmentProcessQueue.size()];
        int k = 0;
        for (Integer id: endOfSegmentProcessQueue)
        {
            previousFinishedSegments[k++] = id;
            HashMap<Integer, Integer> children = childMap.get(id);
            bufferManager.refreshClaim(id);
            if (children != null)
            {
                total = getParentChildTransitionCount(id);
                for (Integer childKey:children.keySet())
                {
                    addFraction = children.get(childKey).doubleValue()/total;
                    if (matchCounts[childKey.intValue()] != -1)
                    {
                        // You need to restart predicting with the child from the beginning, even if it already
                        // succeeded or failed in tests above
                        slstm = segments[childKey.intValue()];
                        slstm.resetToStartOfSequence();
                        initialPrediction = slstm.getOutputValuesExternal();
                        if (allowParentSupportOfChildSegmentP)
                            matchCounts[childKey.intValue()] = (int)(addFraction * matchCounts[id]) + getLSTMTransitionMatchCount(childKey.intValue());
                        else
                            matchCounts[childKey.intValue()] = getLSTMTransitionMatchCount(childKey.intValue());
                        currentPredictions[childKey.intValue()] = initialPrediction;
                        predictionsP = true;
                    }
                    else
                    {

                        currentPredictions[childKey.intValue()] = null;
                    }

                }

            }
            matchCounts[id] = 0;
            parentSegmentId = id;
            if (currentPredictions[id] != null)
                currentPredictions[id] = null;
        }

        if (!predictionsP)
            currentPredictions = null;

        if (currentPredictions != null)
        {
            boolean empty = true;
            for (int i = 0;i < MAX_LSTM;i++)
            {
                if (currentPredictions[i] != null)
                {
                    empty = false;
                    break;
                }
            }
            if (empty)
                currentPredictions = null;

        }

        allowIgnoreNextInput = false;
        return currentPredictions;
    }

    public int[] getMatchCounts()
    {
        return matchCounts;
    }

    public int[] getFinishingSegments()
    {
        return previousFinishedSegments;
    }

    public LinkedLSTMSet nudgePrediction(int segmentId, int enhancementAmount)
    {
        if (currentPredictions[segmentId] != null)
        {
            matchCounts[segmentId] = enhancementAmount;
            if (enhancementAmount == 0)
                currentPredictions[segmentId] = null;
        }
        return this;
    }


    public HashMap<Integer, HashMap<Integer, Integer>> getChildMap()
    {
        return childMap;
    }



    @Override
    public void resetPredictions()
    {
        previousPredictions = null;

        for (int i = 0;i < MAX_LSTM;i++)
        {
            matchCounts[i] = 0;
            if (segments[i] != null)
                segments[i].resetToStartOfSequence();
        }
        currentPredictions = null;

    }

    @Override
    public Vector[] observePredictNext()
    {
        if (currentPredictions != null)
            return observePredictNext(getBestPrediction(), false);
        else
            return null;
    }

    @Override
    public Vector getBestPrediction(Vector input, boolean learnP)
    {

        return getBestPrediction(observePredictNext(input, learnP));
    }

    @Override
    public Vector getBestPrediction()
    {
        return getBestPrediction(currentPredictions);
    }

    @Override
    public void clearAllPredictions()
    {
        previousPredictions = null;
        currentPredictions = null;
        for (int i = 0;i<MAX_LSTM;i++)
        {
            matchCounts[i] = 0;
            if (segments[i] != null)
                segments[i].resetToStartOfSequence();
        }
    }

    @Override
    public Vector[] getPreviousPrediction()
    {
        return previousPredictions;
    }

    @Override
    public Vector getBestPrediction(Vector[] pool)
    {
        return predictionAggregator.aggregateResult(pool);
    }
}
