package com.evolved.automata.nn;

import com.evolved.automata.AITools;
import com.evolved.automata.ArrayMapper;
import com.evolved.automata.IndexedValueMapper;
import com.evolved.automata.WeightedValue;
import com.evolved.automata.parser.general.Sequence;

import org.apache.commons.lang3.StringUtils;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Created by Evolved8 on 1/9/17.
 */
public class LinkedLSTM implements SequencePredictor{


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

    boolean forceMidSequencePredictionP = false;
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
    HashMap<Integer, Integer> childMap;
    MatchCountResetPolicy matchCountResetPolicy = MatchCountResetPolicy.ALWAYS_ZERO_ON_FAILURE;

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

    String DATA_SEPARATOR = "(+)";


    int initialLearningMatchThreshold = 4;
    int maxPreviousMatchCount= 0;
    double predictionAcceptThreshold = 0.5;


    SequencePredictor.PredictionComparator predictionAssessor;

    SequencePredictor.PredictionComparator getDefaultAssessor()
    {
        return new SequencePredictor.PredictionComparator(){

            @Override
            public double weighPrediction(Vector actual, Vector predicted, int predictorIndex)
            {
                if (actual.equals(predicted))
                    return 1;
                else
                    return 0;

            }

            public double getResetThresholdWeight()
            {
                return predictionAcceptThreshold;
            }
        };
    }

    public SequencePredictor setPredictionEvaluator(SequencePredictor.PredictionComparator evaluator)
    {
        predictionAssessor = evaluator;
        return this;
    }

    public String serializedForm()
    {
        StringBuilder s = new StringBuilder();
        // Add the LSTM data

        String segmentData = NNTools.getSerializedSequenceData(segments);
        s.append(segmentData);

        s.append(DATA_SEPARATOR);
        // Add the BufferManager data

        String bufferData = bufferManager.getSerializedForm();
        s.append(bufferData);
        // Add the current predictions

        String predictions = NNTools.vArrayToString(currentPredictions);
        s.append(DATA_SEPARATOR);
        s.append(predictions);
        // Add the current match counts

        String serializedMatches = (new Vector(matchCounts)).serialize();
        s.append(DATA_SEPARATOR);
        s.append(serializedMatches);
        // Serialized failure counts

        String serializedFailures = (new Vector(failureCounts)).serialize();
        s.append(DATA_SEPARATOR);
        s.append(serializedFailures);
        // Add the childmap data


        Vector[] vdata = new Vector[childMap.size()];
        int i = 0;
        for (Integer key:childMap.keySet())
        {
            vdata[i] = new Vector(new int[]{key.intValue(), childMap.get(key).intValue()});
            i++;
        }


        String serializedChildMap = NNTools.vArrayToString(vdata);
        s.append(DATA_SEPARATOR);
        s.append(serializedChildMap);
        s.append(DATA_SEPARATOR);
        if (currentSegmentId != null)
            s.append(currentSegmentId);
        s.append(DATA_SEPARATOR);
        s.append(maxPreviousMatchCount);
        s.append(DATA_SEPARATOR);
        if (parentSegmentId != null)
            s.append(parentSegmentId);
        s.append(DATA_SEPARATOR);
        if (previousRecycledSlot != null)
            s.append(previousRecycledSlot);
        return s.toString();
    }


    public void loadData(String serializedData)
    {
        String[] parts = StringUtils.splitByWholeSeparatorPreserveAllTokens(serializedData, DATA_SEPARATOR);
        String segmentData = parts[0];
        SequenceLSTM.LSTMNetworkBuilder builder = segmentBuilder();
        segments = NNTools.deSerializeData(builder, segmentData);
        String bufferData = parts[1];
        bufferManager = RingBufferManager.fromSerializedForm(bufferData);
        String predictions = parts[2];
        currentPredictions = NNTools.stringToVArray(predictions);

        String serializedMatches = parts[3];
        matchCounts = NNTools.getVectorDataAsInt(Vector.fromSerialized(serializedMatches));
        String serializedFailures = parts[4];
        failureCounts = NNTools.getVectorDataAsInt(Vector.fromSerialized(serializedFailures));

        String serializedChildMap = parts[5];
        Vector[] arrayData = NNTools.stringToVArray(serializedChildMap);

        childMap.clear();
        AITools.map(arrayData, new ArrayMapper<Vector>() {
            @Override
            public Vector map(Vector input, int index)
            {
                if (input != null)
                {
                    int[] raw = NNTools.getVectorDataAsInt(input);
                    childMap.put(raw[0], raw[1]);
                }
                return null;
            }
        });

        String currentLSTMIndex = parts[6];
        if (currentLSTMIndex != null && currentLSTMIndex.length()>0)
            currentSegmentId = Integer.parseInt(currentLSTMIndex);
        else
            currentSegmentId = null;

        if (currentSegmentId != null)
            currentLSTM = segments[currentSegmentId.intValue()];
        else
            currentLSTM = null;

        String preMatchCounts = parts[7];
        maxPreviousMatchCount = Integer.parseInt(preMatchCounts);
        String parentId = parts[8];
        if (parentId != null && parentId.length()>0)
            parentSegmentId = Integer.parseInt(parentId);
        else
            parentSegmentId = null;
        String previousRecycled = parts[9];
        if (previousRecycled != null && previousRecycled.length()>0)
            previousRecycledSlot = Integer.parseInt(previousRecycled);
        else
            previousRecycledSlot = null;
    }



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


        public LinkedLSTM build()
        {
            LinkedLSTM out = new LinkedLSTM(numInputOutputNodes, numMemoryCellNodes, numLSTM);
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



    private LinkedLSTM(int inputNodeCount, int stateNodeCount, int maxBuffer)
    {

        childMap = new HashMap<Integer, Integer>();
        bufferManager = new RingBufferManager(maxBuffer);
        _inputNodeCount = inputNodeCount;
        _memoryCellCount = stateNodeCount;
        MAX_LSTM =maxBuffer;
        matchCounts = new int[MAX_LSTM];
        segments = new SequenceLSTM[MAX_LSTM];
        failureCounts = new int[MAX_LSTM];
        builder = segmentBuilder();
        builder.setEndCapPolicy(SequenceLSTM.EndCapPolicy.BOTH_ENDS);
        builder.setCapDisplayPolicy(SequenceLSTM.EndCapDisplayPolicy.HIDE);
        predictionAssessor = getDefaultAssessor();
    }



    SequenceLSTM.LSTMNetworkBuilder segmentBuilder()
    {
        SequenceLSTM.LSTMNetworkBuilder sbuilder = NNTools.getStandardSequenceLSTMBuilder(_inputNodeCount, _memoryCellCount, null);
        sbuilder.setEndCapPolicy(SequenceLSTM.EndCapPolicy.BOTH_ENDS);
        sbuilder.setCapDisplayPolicy(SequenceLSTM.EndCapDisplayPolicy.HIDE);
        return sbuilder;
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

    public LinkedLSTM ignoreNextInput()
    {
        allowIgnoreNextInput = true;
        return this;
    }

    public boolean joinableP(LinkedLSTM outer)
    {
        return outer!=null && _inputNodeCount == outer._inputNodeCount && _memoryCellCount == outer._memoryCellCount;
    }

    public LinkedLSTM join(LinkedLSTM outer)
    {
        if (!joinableP(outer))
            throw new IllegalArgumentException("Can only join LinkedLSTM of similar structure");
        SequenceLSTM[] newSegments = new SequenceLSTM[MAX_LSTM + outer.MAX_LSTM];
        int[] newMatchCount = new int[MAX_LSTM + outer.MAX_LSTM];
        int[] newFailureCount = new int[MAX_LSTM + outer.MAX_LSTM];
        Vector[] newCurrentPredictions = null;

        if (currentPredictions != null || outer.currentPredictions!=null)
            newCurrentPredictions = new Vector[MAX_LSTM + outer.MAX_LSTM];

        RingBufferManager newManager = new RingBufferManager(MAX_LSTM + outer.MAX_LSTM);
        int i = 0;
        for (;i < bufferManager.getNumberOfClaimedSlots();i++)
        {
            newSegments[i] = segments[i];
            newMatchCount[i] = matchCounts[i];
            newFailureCount[i] = failureCounts[i];
            if (currentPredictions != null)
                newCurrentPredictions[i] = currentPredictions[i];
        }
        newManager.addAllClaims(bufferManager.getAllClaims());
        int baseOffset = 0;
        for (int j = 0;j<outer.bufferManager.getNumberOfClaimedSlots();j++, i++)
        {
            if (j == 0)
            {
                baseOffset = i;
                if (currentSegmentId != null)
                {
                    childMap.put(currentSegmentId, Integer.valueOf(i));
                    if (parentSegmentId != null && !childMap.containsKey(parentSegmentId))
                    {
                        childMap.put(parentSegmentId, currentSegmentId);
                    }
                }
            }
            newSegments[i] = outer.segments[j];
            newMatchCount[i] = outer.matchCounts[j];
            newFailureCount[i] = outer.failureCounts[j];
            if (outer.currentPredictions != null)
                newCurrentPredictions[i] = outer.currentPredictions[j];


            if (outer.currentSegmentId != null && outer.currentSegmentId.intValue() == j)
            {
                currentSegmentId = Integer.valueOf(i);
                currentLSTM = outer.segments[j];
            }

            if (outer.parentSegmentId != null && outer.parentSegmentId.intValue() == j)
            {
                parentSegmentId = Integer.valueOf(i);
            }

            if (outer.previousRecycledSlot != null && outer.previousRecycledSlot.intValue() == j)
            {
                previousRecycledSlot = Integer.valueOf(i);
            }

            Integer oldChild = outer.childMap.get(Integer.valueOf(j));
            if (oldChild != null)
            {
                childMap.put(Integer.valueOf(i), oldChild + baseOffset);
            }

        }
        newManager.addAllClaims(outer.bufferManager.getAllClaims());

        bufferManager = newManager;
        segments = newSegments;
        currentPredictions = newCurrentPredictions;
        matchCounts = newMatchCount;
        failureCounts = newFailureCount;
        MAX_LSTM += outer.MAX_LSTM;
        maxPreviousMatchCount = outer.maxPreviousMatchCount;
        return this;
    }

    @Override
    public String getDataView()
    {
        return viewListStructure(false, true);
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

    public static LinkedLSTM.Builder getLinkedLSTMBuider()
    {
        return new Builder();
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

    public int[] getMatchCounts()
    {
        return matchCounts;
    }

    Integer previousRecycledSlot = null;

    private boolean testPrediction(Vector actual, Vector predicted, int index)
    {
        return predictionAssessor.weighPrediction(actual, predicted, index) > predictionAcceptThreshold;
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
                bufferManager.refreshClaim(currentSegmentId);
                previousRecycledSlot = currentSegmentId;
            }

        }
        else
            previousRecycledSlot = null;

        if (currentSegmentId == null) // creating lstm for the first time for this slot
        {
            currentSegmentId = bufferManager.tryClaimBestSlot(); // creates a new slot
            if (currentSegmentId.intValue() != -1)
                segments[currentSegmentId] = currentLSTM = builder.build();
            else
                currentSegmentId = null;
        }
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
                if (currentSegmentId == null)
                    updateCurrentSegment();

                if (currentSegmentId != null)
                {
                    learningResult = segments[currentSegmentId].add(input, maxSteps, maxError);
                    matchCounts[currentSegmentId] = -1;
                    if (learningResult[0] > maxError)
                    {

                        if (parentSegmentId != null)
                        {
                            childMap.put(parentSegmentId, currentSegmentId);
                        }

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
                if (testPrediction(input, currentPredictions[i], i))
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

                        if (forceMidSequencePredictionP)
                        {
                            slstm.viewSequenceOutput(new Vector[]{input}, true);
                        }

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
                if (!forceMidSequencePredictionP)
                    slstm.resetToStartOfSequence();
                initialPrediction = slstm.getOutputValuesExternal();
                if (testPrediction(input, initialPrediction, i))
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
                        if (forceMidSequencePredictionP)
                        {
                            slstm.viewSequenceOutput(new Vector[]{input}, true);
                        }
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

        for (Integer id: endOfSegmentProcessQueue)
        {
            Integer childKey = childMap.get(id);
            //bufferManager.refreshClaim(id);
            if (childKey != null)
            {
                if (matchCounts[childKey.intValue()] != -1)
                {
                    // You need to restart predicting with the child from the beginning, even if it already
                    // succeeded or failed in tests above
                    slstm = segments[childKey.intValue()];
                    slstm.resetToStartOfSequence();
                    initialPrediction = slstm.getOutputValuesExternal();
                    if (allowParentSupportOfChildSegmentP)
                        matchCounts[childKey.intValue()] = matchCounts[id] + getLSTMTransitionMatchCount(childKey.intValue());
                    else
                        matchCounts[childKey.intValue()] = getLSTMTransitionMatchCount(childKey.intValue());
                    currentPredictions[childKey.intValue()] = initialPrediction;
                    predictionsP = true;
                }
                else
                {

                    currentPredictions[childKey.intValue()] = null;
                }
                if (learnP)
                    matchCounts[id] = -1;
                else
                    matchCounts[id] = 0;
            }
            else
            { // AT end of linked LSTM
                matchCounts[id] = 0;

            }
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

    public void setAllowMidSequenceInitialPrediction(boolean enable)
    {
        forceMidSequencePredictionP = enable;
    }

    public Vector[] getSequence()
    {
        Integer oldest = bufferManager.getOldestClaimedSlot();
        Integer nextId = oldest;
        LinkedList<Vector> out = new LinkedList<Vector>();
        int i = 0;
        int max = bufferManager.getNumberOfClaimedSlots();
        while (nextId != null && i < max)
        {
            i++;
            for (Vector v:segments[nextId.intValue()].getCurrentSequence())
            {
                out.add(v);
            }
            nextId = childMap.get(nextId);
        }
        if (currentSegmentId != null && !childMap.containsKey(currentSegmentId) && currentSegmentId != oldest)
        {
            for (Vector v:segments[currentSegmentId.intValue()].getCurrentSequence())
            {
                out.add(v);
            }
        }
        return out.toArray(new Vector[0]);
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
