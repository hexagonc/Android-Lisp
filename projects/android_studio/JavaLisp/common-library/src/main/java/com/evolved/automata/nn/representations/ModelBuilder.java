package com.evolved.automata.nn.representations;

import com.evolved.automata.nn.FastLSTMNetwork;
import com.evolved.automata.nn.LSTMNetwork;
import com.evolved.automata.nn.NNTools;
import com.evolved.automata.nn.RingBufferManager;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import javax.tools.Tool;

/**
 * Created by Evolved8 on 2/27/17.
 */

public class ModelBuilder {

    // tunable parameters

    int __MAX_FAILURE_COUNT = 2;

    boolean __ONLY_RESET_PREDICTOR_METADATA_P = false;
    float __COMPLETION_THRESHOLD = 0.75F;
    // recognition considered to be occuring if matchCount is the max of these two parameters
    float __RECOGNITION_THRESHOLD_FRACTION = 0.25F; // fraction of Predictor length
    float __RECOGNITION_THRESHOLD_MATCHCOUNT = 2;

    float __SOME_PREDICTOR_THRESHOLD_FRACTION = 0.1F;
    float __SOME_PREDICTOR_MINIMUM_VALUE = 2;

    float __MANY_PREDICTOR_THRESHOLD_FRACTION = 0.5F;
    float __MANY_PREDICTOR_MINIMUM_VALUE = 5;

    float __MINIMUM_FEATURE_LENGTH = 1;

    int __CURRENT_FEATURE_TRAINING_STEP_LIMIT = 80;

    int __PREDICTOR_SUMMARY_HISTORY_LENGTH = 4;

    int __VERY_FAMILIAR_RECOGNITION_THRESHOLD = 1;

    boolean __COMPARE_LOGICAL_PREDICTOR_STATES_P = true;
    boolean __USE_STRICT_FAILURE_ACCOUNTING_P = true;

    HashMap<PREDICTOR_MATCH_STATE, PREDICTOR_MATCH_STATE> __EQUIVALENCY_MAP = new HashMap<PREDICTOR_MATCH_STATE, PREDICTOR_MATCH_STATE>()
    {
        {
            put(PREDICTOR_MATCH_STATE.STARTING_TO_MATCH_FROM_BEGINNING, PREDICTOR_MATCH_STATE.NOT_MATCHING);
            put(PREDICTOR_MATCH_STATE.STARTING_TO_MATCH_FROM_MIDDLE, PREDICTOR_MATCH_STATE.NOT_MATCHING);
        }
    };



    public interface FastLSTMNetworkViewer {
        String toString(float[] network);
    }


    public enum PREDICTOR_CURRENT_FEATURE_RECOGNITION_STATE
    {
        UNFAMILIAR, // no matching Predictors
        VERY_FAMILIAR_TO_ONE, // single matching Predictor whose matchCount greater than feature length
        VERY_FAMILIAR_TO_SOME, // single matching Predictors whose matchCount greater than feature length
        VERY_FAMILIAR_TO_MANY, // many matching Predictor whose matchCount greater than feature length
        STARTING_TO_BE_RECOGNIZED_BY_ONE, // One Predictor matches but the matchCount is less than recognition threshold and is less than feature length
        STARTING_TO_BE_RECOGNIZED_BY_SOME,
        STARTING_TO_BE_RECOGNIZED_BY_MANY,
        BEING_RECOGNIZED_BY_ONE,  // One Predictor matches and matchCount greater than recognition threshold but matchCount less than feature length
        BEING_RECOGNIZED_BY_SOME,
        BEING_RECOGNIZED_BY_MANY,
        FULLY_MATCHED_BY_ONE, // One Predictor matches and matchCount equals feature length
        FULLY_MATCHED_BY_SOME,
        FULLY_MATCHED_BY_MANY
    }

    public enum PREDICTOR_MATCH_STATE
    {
        NOT_MATCHING, // matchCount is 0
        STARTING_TO_MATCH_FROM_BEGINNING, // start Predictor has 1 <= matchCount < recognition threshold
        STARTING_TO_MATCH_FROM_MIDDLE,   // mid sequence Predictor has 1 <= matchCount <  recognition Threshold
        MATCHING_FROM_BEGINNING,    // start Predictor has matchCount >= recognition threshold
        MATCHING_FROM_MIDDLE,  // mid sequence Predictor has matchCount >= recognition threshold
        MATCHED_TO_END; // matchCount == Predictor length and Predictor at end of sequence

        public float[] toArray()
        {
            float[] out = new float[values().length];
            out[ordinal()] = 1;
            return out;
        }

        @Override
        public String toString()
        {
            return name();
        }
    }

    public enum CURRENT_FEATURE_RECOGNITION_STATE
    {
        UNFAMILIAR, // no matching Predictors
        VERY_FAMILIAR, // multiple matching Predictors with matchCount > feature length
        STARTING_TO_BE_RECOGNIZED, // some Predictors with matchCount < featureLength and less than recognition threshold
        BEING_RECOGNIZED, // some Predictors with matchCount < featureLength but greater than or equal to recognition threshold
        FULLY_MATCHED // some Predictor fully matches current feature
    }


    public class Predictor
    {
        float[] sequenceStartPredictor;
        float[] midSequencePredictor;

        int maxMatchLength;
        int length;
        float[] initialState;
        float[] finalState;
        float[] stateVector;
        int stateWidth;
        LinkedList<PREDICTOR_MATCH_STATE> simpleStateHistory;

        PREDICTOR_MATCH_STATE lastSummary;


        public Predictor(FastLSTMNetwork network, float[] initialState, float[] finalState)
        {
            stateWidth = PREDICTOR_MATCH_STATE.values().length;
            this.initialState = initialState;
            this.finalState = finalState;
            maxMatchLength = 0;
            sequenceStartPredictor = network.getCopyofData();
            midSequencePredictor = network.getCopyofData();
            length = Tools.getCappedLSTMLength(sequenceStartPredictor);
            stateVector = new float[stateWidth];
            stateVector[PREDICTOR_MATCH_STATE.NOT_MATCHING.ordinal()] = 1;
            simpleStateHistory = new LinkedList<PREDICTOR_MATCH_STATE>();
            lastSummary = PREDICTOR_MATCH_STATE.NOT_MATCHING;
        }

        public int getMaxMatchLength()
        {
            return maxMatchLength;
        }

        public int getLength()
        {
            return length;
        }


        private void setStateFlag(float[] vector, PREDICTOR_MATCH_STATE state)
        {
            vector[state.ordinal()] = 1;
        }

        private void clearStateFlag(float[] vector, PREDICTOR_MATCH_STATE state)
        {
            vector[state.ordinal()] = 0;
        }

        private boolean isStateFlagSet(float[] vector, PREDICTOR_MATCH_STATE state)
        {
            return vector[state.ordinal()] == 1;
        }

        private boolean isStateFlagSet(PREDICTOR_MATCH_STATE state)
        {
            return isStateFlagSet(stateVector, state);
        }

        public void reset()
        {
            Tools.resetCappedLSTM(sequenceStartPredictor, initialState);
            Tools.resetCappedLSTM(midSequencePredictor, initialState);
            simpleStateHistory.clear();
        }

        public float[] getState()
        {
            return stateVector;
        }

        public float[] getFeatureData()
        {

            return sequenceStartPredictor;
        }

        public PREDICTOR_MATCH_STATE getStateSummary()
        {
            return getStateSummary(stateVector);
        }

        public PREDICTOR_MATCH_STATE getStateSummary(float[] state)
        {
            if (isStateFlagSet(state, PREDICTOR_MATCH_STATE.NOT_MATCHING))
                return PREDICTOR_MATCH_STATE.NOT_MATCHING;
            else if (isStateFlagSet(state, PREDICTOR_MATCH_STATE.STARTING_TO_MATCH_FROM_BEGINNING))
                return PREDICTOR_MATCH_STATE.STARTING_TO_MATCH_FROM_BEGINNING;
            else if (isStateFlagSet(state, PREDICTOR_MATCH_STATE.MATCHING_FROM_BEGINNING))
                return PREDICTOR_MATCH_STATE.MATCHING_FROM_BEGINNING;
            else if (isStateFlagSet(state, PREDICTOR_MATCH_STATE.MATCHED_TO_END))
            {
                return PREDICTOR_MATCH_STATE.MATCHED_TO_END;
            }
            else if (isStateFlagSet(state, PREDICTOR_MATCH_STATE.MATCHING_FROM_MIDDLE))
                return PREDICTOR_MATCH_STATE.MATCHING_FROM_MIDDLE;
            else if (isStateFlagSet(state, PREDICTOR_MATCH_STATE.MATCHING_FROM_BEGINNING))
                return PREDICTOR_MATCH_STATE.MATCHING_FROM_BEGINNING;
            else
            // Excluding case of starting to match from middle to avoid creating noise
                return PREDICTOR_MATCH_STATE.NOT_MATCHING;
        }

        List<PREDICTOR_MATCH_STATE> getStateFullDescription()
        {
            List<PREDICTOR_MATCH_STATE> list = new LinkedList<PREDICTOR_MATCH_STATE>();
            for (int i = 0; i < stateVector.length;i++)
            {
                if (stateVector[i] == 1)
                {
                    list.add(PREDICTOR_MATCH_STATE.values()[i]);
                }
            }
            return list;
        }


        public int getRecognitionThreshold()
        {
            return ModelBuilder.this.getRecognitionThreshold(length);
        }

        public boolean isAtBeginning(int matchCount)
        {
            return getRecognitionThreshold() > matchCount;

        }

        public boolean isMatching(int matchCount)
        {
            return getRecognitionThreshold() <= matchCount;
        }

        public boolean isCompleteEnough(int matchCount)
        {
            int recognitionThreshold = getRecognitionThreshold();
            return matchCount >= NNTools.getMinThreshold(recognitionThreshold, __COMPLETION_THRESHOLD, length);
        }


        public float[] observePredict(float[] lstmInput, float[] wrappedMask)
        {
            float[] newStateVector  = new float[stateWidth];

            float[] midSequencePrediction = FastLSTMNetwork.getOutputActivation(midSequencePredictor);
            float[] startSequencePrediction = FastLSTMNetwork.getOutputActivation(sequenceStartPredictor);

            int midSequenceMatchCount = Tools.getMatchCount(midSequencePredictor);
            int startSequenceMatchCount = Tools.getMatchCount(sequenceStartPredictor);

            int startSequenceFailureCount = Tools.getFailureCount(sequenceStartPredictor);
            int midSequencePredictorFailureCount = Tools.getFailureCount(midSequencePredictor);

            boolean matchedStartPredictor = Tools.maskedRoundedVerifyResult(startSequencePrediction, lstmInput, wrappedMask);
            boolean matchedMidPredictor = Tools.maskedRoundedVerifyResult(midSequencePrediction, lstmInput, wrappedMask);

            FastLSTMNetwork.forwardPass(midSequencePredictor, lstmInput);
            FastLSTMNetwork.forwardPass(sequenceStartPredictor, lstmInput);

            boolean midSequencePredictorAtFinalState = Tools.isAtFinalState(midSequencePredictor, finalState);
            boolean startOfSequencePredictorAtFinalState = Tools.isAtFinalState(sequenceStartPredictor, finalState);

            maxMatchLength = 0;

            if (startOfSequencePredictorAtFinalState && isCompleteEnough(startSequenceMatchCount))
            {
                setStateFlag(newStateVector, PREDICTOR_MATCH_STATE.MATCHED_TO_END);
                Tools.resetCappedLSTM(sequenceStartPredictor, initialState);
                Tools.resetCappedLSTM(midSequencePredictor, initialState, true);
                maxMatchLength = Math.max(midSequenceMatchCount, startSequenceMatchCount);
            }
            else if (midSequencePredictorAtFinalState && isMatching(midSequenceMatchCount))
            {
                Tools.resetCappedLSTM(midSequencePredictor, initialState);
                Tools.resetCappedLSTM(sequenceStartPredictor, initialState);
                setStateFlag(newStateVector, PREDICTOR_MATCH_STATE.MATCHED_TO_END);
                maxMatchLength = Math.max(midSequenceMatchCount, startSequenceMatchCount);
            }
            else
            {

                if (!matchedStartPredictor)
                {
                    Tools.incrementFailureCount(sequenceStartPredictor);
                    startSequenceFailureCount++;

                    if (startSequenceMatchCount >= startSequenceFailureCount && (!__USE_STRICT_FAILURE_ACCOUNTING_P || __USE_STRICT_FAILURE_ACCOUNTING_P && startSequenceFailureCount <= __MAX_FAILURE_COUNT))
                    {
                        Tools.decrementMatchCount(sequenceStartPredictor);
                        startSequenceMatchCount = Math.max(0, startSequenceMatchCount - 1);
                        matchedStartPredictor = true;
                    }
                }
                else
                {
                    Tools.resetFailureCount(sequenceStartPredictor);

                }


                if (matchedStartPredictor)
                {
                    Tools.incrementMatchCount(sequenceStartPredictor);
                    startSequenceMatchCount++;
                    if (isAtBeginning(startSequenceMatchCount))
                    {
                        setStateFlag(newStateVector, PREDICTOR_MATCH_STATE.STARTING_TO_MATCH_FROM_BEGINNING);
                    }
                    else
                    {
                        setStateFlag(newStateVector, PREDICTOR_MATCH_STATE.MATCHING_FROM_BEGINNING);
                    }
                }
                else
                {
                    Tools.resetCappedLSTM(sequenceStartPredictor, initialState);
                    startSequenceMatchCount = 0;
                }


                if (!matchedMidPredictor)
                {
                    Tools.incrementFailureCount(midSequencePredictor);

                    midSequencePredictorFailureCount++;

                    if (midSequenceMatchCount >= midSequencePredictorFailureCount && (!__USE_STRICT_FAILURE_ACCOUNTING_P || __USE_STRICT_FAILURE_ACCOUNTING_P && midSequencePredictorFailureCount <= __MAX_FAILURE_COUNT))
                    {
                        matchedMidPredictor = true;
                        Tools.decrementMatchCount(midSequencePredictor);
                        midSequenceMatchCount = Math.max(0, midSequenceMatchCount - 1);
                    }

                }
                else
                {
                    Tools.resetFailureCount(midSequencePredictor);
                }



                if (matchedMidPredictor)
                {
                    Tools.incrementMatchCount(midSequencePredictor);
                    midSequenceMatchCount++;
                    if (isAtBeginning(midSequenceMatchCount))
                    {
                        setStateFlag(newStateVector, PREDICTOR_MATCH_STATE.STARTING_TO_MATCH_FROM_MIDDLE);
                    }
                    else
                    {
                        setStateFlag(newStateVector, PREDICTOR_MATCH_STATE.MATCHING_FROM_MIDDLE);
                    }
                }
                else
                {
                    Tools.resetCappedLSTM(midSequencePredictor, initialState, true);
                    midSequenceMatchCount = 0;
                }

                if (!matchedMidPredictor && !matchedStartPredictor)
                {
                    setStateFlag(newStateVector, PREDICTOR_MATCH_STATE.NOT_MATCHING);

                }
                else
                    maxMatchLength = Math.max(midSequenceMatchCount, startSequenceMatchCount);

            }

            PREDICTOR_MATCH_STATE currentSummary = getStateSummary(newStateVector);
            if (simpleStateHistory.size() < __PREDICTOR_SUMMARY_HISTORY_LENGTH)
            {
                simpleStateHistory.add(currentSummary);
            }
            else if (__PREDICTOR_SUMMARY_HISTORY_LENGTH > 0)
            {
                simpleStateHistory.removeFirst();
                simpleStateHistory.add(currentSummary);

            }

            boolean exactSameState = Arrays.equals(newStateVector, stateVector);

            PREDICTOR_MATCH_STATE mappedSummary = __EQUIVALENCY_MAP.get(currentSummary);

            if (mappedSummary != null)
                currentSummary = mappedSummary;

            boolean exactSameLogicalState = currentSummary == lastSummary;

            lastSummary = currentSummary;

            boolean sameState;
            if (__COMPARE_LOGICAL_PREDICTOR_STATES_P)
                sameState = exactSameLogicalState;
            else
                sameState = exactSameState;

            if (!sameState)
            {
                return stateVector = newStateVector;
            }
            else
                return null;

        }

        public float[] getMidSequencePrediction()
        {
            return NNTools.unwrapVector(NNTools.roundToInt(FastLSTMNetwork.getOutputActivation(midSequencePredictor)));
        }


        public float[] getStartSequencePrediction()
        {
            return NNTools.unwrapVector(NNTools.roundToInt(FastLSTMNetwork.getOutputActivation(sequenceStartPredictor)));
        }

        public float[] getBestPrediction()
        {
            int midSequenceMatchCount = Tools.getMatchCount(midSequencePredictor);
            int startSequenceMatchCount = Tools.getMatchCount(sequenceStartPredictor);

            if (startSequenceMatchCount >= midSequenceMatchCount)
                return getStartSequencePrediction();
            else
                return getMidSequencePrediction();
        }

    }

    FastLSTMNetworkViewer viewer = null;



    int capacity;
    ModelBuilder nextLevel;

    int dimension;
    Predictor[] _predictors;

    int initialNumCellStates = 13;
    float[] initialState;
    float[] finalState;
    boolean stateIsNewP = false;

    float resetThresholdFraction = 0.0001F;

    FastLSTMNetwork currentFeatureLSTM;

    LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.RPROP;

    RingBufferManager manager;
    boolean extrapModeP = false;
    LinkedList<CURRENT_FEATURE_RECOGNITION_STATE> currentFeatureState;

    PREDICTOR_MATCH_STATE[] hiearchicalState;

    int lastSelectedPredictor = -1;

    public ModelBuilder(int inputDimension, int capacity, int limit)
    {
        dimension = inputDimension+2;
        initialState = new float[dimension];
        finalState = new float[dimension];

        initialState[0] = 1;
        finalState[dimension-1]=1;

        this.capacity = capacity;
         _predictors = new Predictor[capacity];

        __CURRENT_FEATURE_TRAINING_STEP_LIMIT = limit;

        manager = new RingBufferManager(capacity);



        currentFeatureState = new LinkedList<CURRENT_FEATURE_RECOGNITION_STATE>();
        currentFeatureLSTM = getNetwork();
    }

    FastLSTMNetwork getNetwork()
    {
        FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(dimension, dimension, initialNumCellStates);
        FastLSTMNetwork network = builder.build();
        int steps = Tools.createCappedLSTM(network.getActualData(), initialState, finalState);
        Tools.setId(network.getActualData(), manager.getNumberOfClaimedSlots());
        return network;
    }



    String viewPredictor(int i)
    {
        if (i < manager.getNumberOfClaimedSlots())
        {
            String stateName = _predictors[i].getStateSummary().name();
            if (viewer == null)
                return stateName;
            else
                return "<" + stateName + "> " + viewer.toString(_predictors[i].getFeatureData());
        }
        else
            return "";
    }


    public String viewAllPredictors()
    {
        StringBuilder builder = new StringBuilder();
        String value;
        String delimiter = "\n";
        for (int i = 0; i < manager.getNumberOfClaimedSlots(); i++)
        {
            value = viewPredictor(i);
            if (i > 0)
            {
                builder.append(delimiter);
            }
            builder.append(value);
        }
        return builder.toString();
    }


    public boolean previousStateWasNewP()
    {
        return  stateIsNewP;
    }


    public void reset()
    {
        for (int i = 0; i < manager.getNumberOfClaimedSlots();i ++)
        {
            _predictors[i].reset();
        }
    }

    public void finalize(boolean recursive)
    {
        reset();

        int featureLength = Tools.getCappedLSTMLength(currentFeatureLSTM.getActualData());
        int dataLength = currentFeatureState.size();
        if (featureLength > 0)
        {
            CURRENT_FEATURE_RECOGNITION_STATE recognitionState = currentFeatureState.getLast();
            if (recognitionState == CURRENT_FEATURE_RECOGNITION_STATE.UNFAMILIAR)
            {
                saveCurrentFeature();
            }
            else
            {
                currentFeatureState.clear();
            }
            currentFeatureLSTM = getNetwork();

            if (currentFeatureLSTM == null)
            {
                throw new RuntimeException("Failure to create capped lstm");
            }
        }
        if (nextLevel != null && recursive)
            nextLevel.finalize(recursive);
    }

    private void saveCurrentFeature()
    {
        int index = getNextPredictorIndex();
        if (_predictors[index] != null)
        {
            // TODO: write code to ignore this index for a while
        }

        currentFeatureState.clear();
        Tools.setId(currentFeatureLSTM.getActualData(), index);
        _predictors[index] = getPredictor(currentFeatureLSTM);

        Tools.resetCappedLSTM(currentFeatureLSTM.getActualData(), initialState);
    }

    public boolean learnNextFeatureState(float[] wrappedState)
    {

        int loopCount = 0;
        double learningError;
        do
        {
            loopCount++;
            learningError  = Tools.appendVectorToSequence(currentFeatureLSTM.getActualData(), initialState, finalState, wrappedState, __CURRENT_FEATURE_TRAINING_STEP_LIMIT, resetThresholdFraction, true, updateType);
        }while (learningError > __CURRENT_FEATURE_TRAINING_STEP_LIMIT && Tools.getCappedLSTMLength(currentFeatureLSTM.getActualData()) < __MINIMUM_FEATURE_LENGTH);
        return learningError > __CURRENT_FEATURE_TRAINING_STEP_LIMIT;
    }

    public float[] getFeatureInitialState()
    {
        return initialState;
    }

    public float[] getFeatureFinalState()
    {
        return finalState;
    }



    Predictor getPredictor(FastLSTMNetwork network)
    {
        return new Predictor(network, initialState, finalState);
    }


    public int getRecognitionThreshold(float predictorLength)
    {
        return (int)NNTools.getMinThreshold(__RECOGNITION_THRESHOLD_MATCHCOUNT, __RECOGNITION_THRESHOLD_FRACTION, predictorLength);

    }

    FastLSTMNetwork.LSTMNetworkBuilder getStandardBuilder(int inputNodeCode, int outputNodeCode, int numMemoryCellStates)
    {
        FastLSTMNetwork.LSTMNetworkBuilder builder = FastLSTMNetwork.getFastBuilder();

        builder.setInputNodeCount(inputNodeCode, FastLSTMNetwork.MSE_ERROR_FUNCTION_ID, FastLSTMNetwork.SIGMOID_ACTIVATION_ID);
        builder.setOutputNodeCount(outputNodeCode);
        builder.addMemoryCell("M", numMemoryCellStates);
        HashMap<String, ArrayList<String>> connectivityMap = NNTools.getStandardLinkConnectivityMap("M");
        for (String sourceNode:connectivityMap.keySet())
        {
            builder.addNodeConnections(sourceNode, NNTools.arrayListToArray(connectivityMap.get(sourceNode)));
        }

        builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrder("M"));
        builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrder("M"));
        builder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.INITIAL_DELTA, 0.012);
        builder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MAX_DELTA, 50);
        builder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MIN_DELTA, 0);
        builder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MAX, 1.2);
        builder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MIN, 0.5);
        builder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.CONVERGENCE_THRESHOLD, 0.0001);
        builder.setWeightUpdateType(LSTMNetwork.WeightUpdateType.RPROP);


        return builder;
    }

    public PREDICTOR_MATCH_STATE[] getBaseState()
    {
        PREDICTOR_MATCH_STATE[] nextState = new PREDICTOR_MATCH_STATE[capacity];
        for (int i = 0; i < nextState.length;i ++)
        {
            nextState[i] = PREDICTOR_MATCH_STATE.NOT_MATCHING;
        }
        return nextState;
    }

    public void setLSTMViewer(FastLSTMNetworkViewer viewer)
    {
        this.viewer = viewer;
    }

    private int getNextPredictorIndex()
    {
        int index = manager.tryClaimBestSlot();
        return index;
    }


    public PREDICTOR_MATCH_STATE[] observePredict(float[] input, float[] mask)
    {
        int featureLength = Tools.getCappedLSTMLength(currentFeatureLSTM.getActualData());
        float[] lstmInput = NNTools.wrapVector(input);
        float[] wrappedMask = null;
        if (mask != null)
            wrappedMask = NNTools.wrapVector(mask);

        LinkedList<PREDICTOR_MATCH_STATE> outputState = new LinkedList<PREDICTOR_MATCH_STATE>();
        PREDICTOR_MATCH_STATE[] nextState = getBaseState();

        PREDICTOR_MATCH_STATE predictorState;
        stateIsNewP = false;

        float[] fullState;
        int maxMatchLength = 0;

        boolean matchingPredictorsP = false;
        int[] matchingStateCounts = new int[CURRENT_FEATURE_RECOGNITION_STATE.values().length];
        int matchLength, predictorLength, totalPredictors = manager.getNumberOfClaimedSlots();
        CURRENT_FEATURE_RECOGNITION_STATE currentFeatureRecognitionSummary = CURRENT_FEATURE_RECOGNITION_STATE.UNFAMILIAR;
        for (int i = 0; i < totalPredictors; i++)
        {
            fullState = _predictors[i].observePredict(lstmInput, wrappedMask);
            predictorState = _predictors[i].getStateSummary();

            if (fullState != null)
            {
                stateIsNewP = true;
            }

            matchLength = _predictors[i].getMaxMatchLength();
            predictorLength = _predictors[i].getLength();

            maxMatchLength = Math.max(maxMatchLength, matchLength);
            nextState[i] = predictorState;
            outputState.add(predictorState);

            if (predictorState != PREDICTOR_MATCH_STATE.NOT_MATCHING)
            {
                matchingPredictorsP = true;
                if (matchLength >= featureLength)
                    matchingStateCounts[CURRENT_FEATURE_RECOGNITION_STATE.VERY_FAMILIAR.ordinal()]++;
                else if (matchLength < featureLength &&  matchLength < getRecognitionThreshold(predictorLength))
                {
                    matchingStateCounts[CURRENT_FEATURE_RECOGNITION_STATE.STARTING_TO_BE_RECOGNIZED.ordinal()]++;
                }
                else if (matchLength < featureLength && matchLength >= getRecognitionThreshold(predictorLength))
                {
                    matchingStateCounts[CURRENT_FEATURE_RECOGNITION_STATE.BEING_RECOGNIZED.ordinal()]++;
                }
                else if (matchLength == featureLength)
                {
                    matchingStateCounts[CURRENT_FEATURE_RECOGNITION_STATE.FULLY_MATCHED.ordinal()]++;
                }

            }
            else
                matchingStateCounts[CURRENT_FEATURE_RECOGNITION_STATE.UNFAMILIAR.ordinal()]++;

        }

        if (!matchingPredictorsP)
            lastSelectedPredictor = -1;

        hiearchicalState = nextState;

        if (totalPredictors > 0)
        {
            if (matchingStateCounts[CURRENT_FEATURE_RECOGNITION_STATE.VERY_FAMILIAR.ordinal()] >= __VERY_FAMILIAR_RECOGNITION_THRESHOLD)
            {
                currentFeatureRecognitionSummary = CURRENT_FEATURE_RECOGNITION_STATE.VERY_FAMILIAR;
            }
            else if (matchingStateCounts[CURRENT_FEATURE_RECOGNITION_STATE.FULLY_MATCHED.ordinal()] >= 1)
            {
                currentFeatureRecognitionSummary = CURRENT_FEATURE_RECOGNITION_STATE.FULLY_MATCHED;
            }
            else if (matchingStateCounts[CURRENT_FEATURE_RECOGNITION_STATE.BEING_RECOGNIZED.ordinal()] >= 1)
            {
                currentFeatureRecognitionSummary = CURRENT_FEATURE_RECOGNITION_STATE.BEING_RECOGNIZED;
            }
            else
            {
                int predictorsStarting = matchingStateCounts[CURRENT_FEATURE_RECOGNITION_STATE.STARTING_TO_BE_RECOGNIZED.ordinal()];

                int threshold = (int)NNTools.getMinThreshold( __SOME_PREDICTOR_MINIMUM_VALUE, __SOME_PREDICTOR_THRESHOLD_FRACTION, totalPredictors);

                if (predictorsStarting >= threshold)
                {
                    currentFeatureRecognitionSummary = CURRENT_FEATURE_RECOGNITION_STATE.STARTING_TO_BE_RECOGNIZED;
                }
            }
        }




        currentFeatureState.add(currentFeatureRecognitionSummary);

        // Processing next proposed feature

        boolean endOfFeatureP = learnNextFeatureState(lstmInput);

        if (endOfFeatureP)
        {

            if (Tools.DEBUG)
            {
                String featureDescription = currentFeatureState.toString();
                if (viewer!=null)
                    featureDescription = featureDescription + " (" + viewer.toString(currentFeatureLSTM.getActualData()) + ")";

                System.out.println("Completed feature " + featureDescription);
            }
            switch (currentFeatureRecognitionSummary)
            {
                case VERY_FAMILIAR:
                case FULLY_MATCHED:
                case STARTING_TO_BE_RECOGNIZED:
                case BEING_RECOGNIZED:
                {
                    currentFeatureState.clear();
                    // discard this feature since it is redundant
                    currentFeatureLSTM = getNetwork();

                    if (currentFeatureLSTM == null)
                    {
                        throw new RuntimeException("Failure to create capped lstm");
                    }
                    if (!matchingPredictorsP)
                        learnNextFeatureState(lstmInput);
                    if (Tools.DEBUG)
                        System.out.println("feature discarded");
                }
                break;
                case UNFAMILIAR:
                {
                    // there is new information so save it
                    // really, however, we probably only want to save all of it,
                    // maybe only the parts that are unfamiliar based on the history
                    // in currentFeatureState
                    saveCurrentFeature();
                    currentFeatureLSTM = getNetwork();

                    if (currentFeatureLSTM == null)
                    {
                        throw new RuntimeException("Failure to create capped lstm");
                    }
                    if (!matchingPredictorsP)
                        learnNextFeatureState(lstmInput);
                    if (Tools.DEBUG)
                        System.out.println("feature is saved");
                }
                break;
            }


        }


        if (stateIsNewP)
        {
            float[][] predictorStatesVectors = new float[capacity][];
            for (int i = 0; i < nextState.length; i++)
            {
                predictorStatesVectors[i] = nextState[i].toArray();
            }

            float[] hierarchicalStateVector = NNTools.flattenArray(predictorStatesVectors);

            if (nextLevel != null)
            {
                PREDICTOR_MATCH_STATE[] stateGroups = nextLevel.observePredict(hierarchicalStateVector, null);

                // TODO: Filter nextState based on stateGroups
                float[] higherOrderPrediction = nextLevel.getPrediction(stateGroups);
                PREDICTOR_MATCH_STATE[] updatedNextState = getLogicalStateFromHierarchicalVectorState(higherOrderPrediction);

                nextState = hiearchicalState = updatedNextState;
            }

            //PREDICTOR_MATCH_STATE[] updatedNextState = getLogicalStateFromHierarchicalVectorState(hierarchicalStateVector);
            //boolean matched = Arrays.equals(updatedNextState, nextState);


        }

        return nextState;

    }

    public float[] getPrediction()
    {
        return getPrediction(hiearchicalState);
    }

    public float[] getPrediction(PREDICTOR_MATCH_STATE[] higherOrderState)
    {
        float[] scoredAverage = new float[higherOrderState.length];
        for (int i = 0;i < manager.getNumberOfClaimedSlots();i++)
        {
            scoredAverage[i] = 1;
            switch (higherOrderState[i])
            {
                case STARTING_TO_MATCH_FROM_BEGINNING:
                case STARTING_TO_MATCH_FROM_MIDDLE:
                case MATCHING_FROM_BEGINNING:
                case MATCHING_FROM_MIDDLE:
                case MATCHED_TO_END:
                    if (i == lastSelectedPredictor)
                    {

                        return _predictors[i].getBestPrediction();
                    }
                    else
                    {
                        scoredAverage[i] += _predictors[i].getMaxMatchLength();
                    }
                    break;
                default:
            }
        }

        lastSelectedPredictor = FastLSTMNetwork.sampleVectorIndexProportionally(scoredAverage, 0, higherOrderState.length, 1, false);

        return _predictors[lastSelectedPredictor].getBestPrediction();
    }

    PREDICTOR_MATCH_STATE[] getLogicalStateFromHierarchicalVectorState(float[] vector)
    {
        PREDICTOR_MATCH_STATE[] out = new PREDICTOR_MATCH_STATE[capacity];
        for (int i = 0; i < out.length;i++)
            out[i] = PREDICTOR_MATCH_STATE.NOT_MATCHING;

        int predictorStateWidth = PREDICTOR_MATCH_STATE.values().length;
        int subStateOffsetIndex = 0, subStateBaseIndex = 0;
        float[] subState = null;
        int predictorIndex = -1;
        for (int i = 0; i < vector.length;i++)
        {

            if (i % predictorStateWidth == 0)
            {
                subStateBaseIndex = i;
                subState = new float[predictorStateWidth];
                predictorIndex++;
            }

            subStateOffsetIndex = i - subStateBaseIndex;

            if (subStateOffsetIndex < predictorStateWidth)
            {
                subState[subStateOffsetIndex] = vector[i];

                if (vector[i] == 1)
                {
                    out[predictorIndex] = PREDICTOR_MATCH_STATE.values()[subStateOffsetIndex];
                }

                if (subStateOffsetIndex == predictorStateWidth - 1)
                {
                    // Do something with the whole subState vector
                }
            }


        }

        return out;

    }

}
