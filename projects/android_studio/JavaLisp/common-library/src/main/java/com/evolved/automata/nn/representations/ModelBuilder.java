package com.evolved.automata.nn.representations;

import com.evolved.automata.nn.FastLSTMNetwork;
import com.evolved.automata.nn.LSTMNetwork;
import com.evolved.automata.nn.NNTools;
import com.evolved.automata.nn.RingBufferManager;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;

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
            //put(PREDICTOR_MATCH_STATE.STARTING_TO_MATCH_FROM_BEGINNING, PREDICTOR_MATCH_STATE.NOT_MATCHING);
            put(PREDICTOR_MATCH_STATE.STARTING_TO_MATCH_FROM_MIDDLE, PREDICTOR_MATCH_STATE.NOT_MATCHING);
        }
    };


    HashSet<PREDICTOR_MATCH_STATE> __START_BLOCKING_SET = new HashSet<PREDICTOR_MATCH_STATE>()
    {
        {
            add(PREDICTOR_MATCH_STATE.MATCHING_FROM_BEGINNING);
            add(PREDICTOR_MATCH_STATE.MATCHING_FROM_MIDDLE);

        }
    };


    HashSet<PREDICTOR_MATCH_STATE> __START_SET = new HashSet<PREDICTOR_MATCH_STATE>()
    {
        {
            add(PREDICTOR_MATCH_STATE.STARTING_TO_MATCH_FROM_BEGINNING);
            add(PREDICTOR_MATCH_STATE.STARTING_TO_MATCH_FROM_MIDDLE);

        }
    };


    public interface FastLSTMNetworkViewer {
        String toString(float[] network);
    }

    public interface VectorInputViewer {
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

    public enum FEATURE_SAVE_OPTIONS
    {
        CONSIDER_LAST,
        SELECT_RANDOM,
        SELECT_MOST
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
            stateVector =  new float[stateWidth];
            setStateFlag(stateVector, PREDICTOR_MATCH_STATE.NOT_MATCHING);

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
            PREDICTOR_MATCH_STATE out = getStateSummaryInternal(state);
            PREDICTOR_MATCH_STATE corrected = __EQUIVALENCY_MAP.get(out);
            if (corrected == null)
                return out;
            else
                return corrected;
        }
        public PREDICTOR_MATCH_STATE getStateSummaryInternal(float[] state)
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

            if (startOfSequencePredictorAtFinalState && matchedStartPredictor && isCompleteEnough(startSequenceMatchCount+1))
            {
                setStateFlag(newStateVector, PREDICTOR_MATCH_STATE.MATCHED_TO_END);
                Tools.resetCappedLSTM(sequenceStartPredictor, initialState);
                Tools.resetCappedLSTM(midSequencePredictor, initialState, true);
                maxMatchLength = Math.max(midSequenceMatchCount, startSequenceMatchCount);
            }
            else if (midSequencePredictorAtFinalState && matchedMidPredictor && isMatching(midSequenceMatchCount + 1))
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

    FastLSTMNetworkViewer lstmViewer = null;
    VectorInputViewer inputViewer = null;
    FEATURE_SAVE_OPTIONS saveMode = FEATURE_SAVE_OPTIONS.SELECT_MOST;

    int capacity;
    ModelBuilder nextLevel;
    ModelBuilder prevLevel;
    boolean allowHigherLevelP = false;
    boolean allowLearningP = true;
    boolean hasNewPredictedStateP = false;



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

    PREDICTOR_MATCH_STATE[] currentState, nextPredictedState = null;

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
        currentState = new PREDICTOR_MATCH_STATE[capacity];

        resetState(currentState);
        currentFeatureState = new LinkedList<CURRENT_FEATURE_RECOGNITION_STATE>();
        currentFeatureLSTM = getNetwork();
        lstmViewer = getLowerLevelStateTransitionLSTMViewer(initialState, finalState);
        inputViewer = getLowerOrderStateInputVectorViewer();
    }

    private void resetState(PREDICTOR_MATCH_STATE[] state)
    {
        for (int i = manager.getNumberOfClaimedSlots();i < capacity;i++)
        {
            state[i] = PREDICTOR_MATCH_STATE.NOT_MATCHING;
        }
    }

    FastLSTMNetwork getNetwork()
    {
        FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(dimension, dimension, initialNumCellStates);
        FastLSTMNetwork network = builder.build();
        int steps = Tools.createCappedLSTM(network.getActualData(), initialState, finalState);
        Tools.setId(network.getActualData(), manager.getNumberOfClaimedSlots());
        return network;
    }


    ModelBuilder.FastLSTMNetworkViewer getLowerLevelStateTransitionLSTMViewer(final float[] initialValue, final float[] finalValue)
    {
        return new ModelBuilder.FastLSTMNetworkViewer() {
            @Override
            public String toString(float[] networkSpec)
            {
                float[] copy =  Arrays.copyOf(networkSpec, networkSpec.length);
                FastLSTMNetwork.resetNetworkToInitialState(copy);
                FastLSTMNetwork.forwardPass(copy, initialValue);
                float[] output = NNTools.roundToInt(FastLSTMNetwork.getOutputActivation(copy));
                StringBuilder builder = new StringBuilder("{");
                boolean second = false;
                while (!Arrays.equals(output, finalValue))
                {
                    if (second)
                        builder.append(", ");
                    second = true;
                    PREDICTOR_MATCH_STATE[] logical = getLogicalStateFromPredictionVectorState(NNTools.unwrapVector(output));

                    builder.append(Arrays.toString(logical));
                    FastLSTMNetwork.forwardPass(copy, output);
                    output = NNTools.roundToInt(FastLSTMNetwork.getOutputActivation(copy));
                }
                return builder.append("}").toString();
            }
        };
    }

    VectorInputViewer getRawDataViewer(final float[] data)
    {
        return new VectorInputViewer()
        {

            @Override
            public String toString(float[] network)
            {
                return Arrays.toString(data);
            }
        };
    }

    VectorInputViewer getLowerOrderStateInputVectorViewer()
    {
        return new VectorInputViewer()
        {

            @Override
            public String toString(float[] lowerOrderState)
            {
                PREDICTOR_MATCH_STATE[] logical = getLogicalStateFromPredictionVectorState(lowerOrderState);
                return Arrays.toString(logical);
            }
        };
    }

    public void setInputVectorViewer(VectorInputViewer viewer)
    {
        inputViewer = viewer;
    }

    public void setAllowLearning(boolean allowP)
    {
        allowLearningP = allowP;
    }

    public void setAllowHigherlevel(boolean allowP)
    {
        allowHigherLevelP = allowP;
    }


    public String viewPredictor(int i)
    {
        if (i < manager.getNumberOfClaimedSlots())
        {
            String stateName = _predictors[i].getStateSummary().name();
            if (lstmViewer == null)
                return stateName;
            else
                return "<" + stateName + "> " + lstmViewer.toString(_predictors[i].getFeatureData());
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
        lastSelectedPredictor = -1;
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
        this.lstmViewer = viewer;
    }

    private int getNextPredictorIndex()
    {
        int index = manager.tryClaimBestSlot();
        return index;
    }



    public PREDICTOR_MATCH_STATE[] observePredict(float[] input, float[] mask)
    {
        hasNewPredictedStateP = false;
        resetState(currentState);
        if (Tools.DEBUG && prevLevel != null)
        {

            System.out.println("% Processing input: " + inputViewer.toString(input));
        }
        int featureLength = Tools.getCappedLSTMLength(currentFeatureLSTM.getActualData());
        float[] lstmInput = NNTools.wrapVector(input);
        float[] wrappedMask = null;
        if (mask != null)
            wrappedMask = NNTools.wrapVector(mask);

        LinkedList<PREDICTOR_MATCH_STATE> outputState = new LinkedList<PREDICTOR_MATCH_STATE>();

        PREDICTOR_MATCH_STATE predictorState;
        stateIsNewP = false;

        float[] fullState;
        int maxMatchLength = 0;

        boolean matchingPredictorsP = false;
        int[] matchingStateCounts = new int[CURRENT_FEATURE_RECOGNITION_STATE.values().length];
        int matchLength, predictorLength, totalPredictors = manager.getNumberOfClaimedSlots();
        CURRENT_FEATURE_RECOGNITION_STATE currentFeatureRecognitionSummary = CURRENT_FEATURE_RECOGNITION_STATE.UNFAMILIAR;
        HashSet<Integer> maxStartPredictors;

        boolean allowStartState = true;

        int maxStartMatchLength = 0;
        HashMap<Integer, PREDICTOR_MATCH_STATE> startValues = new HashMap<Integer, PREDICTOR_MATCH_STATE>();


        for (int i = 0; i < totalPredictors; i++)
        {
            fullState = _predictors[i].observePredict(lstmInput, wrappedMask);
            predictorState = _predictors[i].getStateSummary();

            if (i == lastSelectedPredictor && (predictorState == PREDICTOR_MATCH_STATE.MATCHED_TO_END || predictorState == PREDICTOR_MATCH_STATE.NOT_MATCHING))
            {
                lastSelectedPredictor = -1;
            }

            if (__START_BLOCKING_SET.contains(predictorState))
            {
                allowStartState = false;
            }



            matchLength = _predictors[i].getMaxMatchLength();
            predictorLength = _predictors[i].getLength();

            if (__START_SET.contains(predictorState))
            {
                startValues.put(Integer.valueOf(i), predictorState);
                if (startValues.size() == 0 || maxStartMatchLength < matchLength)
                    maxStartMatchLength = matchLength;
            }
            else
            {
                if (currentState[i] != predictorState)
                {
                    stateIsNewP = true;
                    currentState[i] = predictorState;
                }
            }


            maxMatchLength = Math.max(maxMatchLength, matchLength);

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

        if (startValues.size() > 0)
        {
            for (Integer predictorIndex: startValues.keySet())
            {
                int i = predictorIndex.intValue();
                int matchCount =  _predictors[i].getMaxMatchLength();
                if (matchCount != maxStartMatchLength)
                {
                    currentState[i] = PREDICTOR_MATCH_STATE.NOT_MATCHING;
                }
                else if (currentState[i] != startValues.get(i))
                {
                    currentState[i] = startValues.get(i);
                    stateIsNewP = true;
                }
            }
        }

        currentFeatureState.add(currentFeatureRecognitionSummary);

        // Processing next proposed feature

        boolean endOfFeatureP = learnNextFeatureState(lstmInput);

        if (endOfFeatureP )
        {
            CURRENT_FEATURE_RECOGNITION_STATE representativeSummary = CURRENT_FEATURE_RECOGNITION_STATE.UNFAMILIAR;
            switch (saveMode)
            {
                case SELECT_MOST:
                {
                    int maxScore = 0;
                    CURRENT_FEATURE_RECOGNITION_STATE maxstate = representativeSummary;
                    int[] counts = new int[CURRENT_FEATURE_RECOGNITION_STATE.values().length];

                    for (CURRENT_FEATURE_RECOGNITION_STATE state: currentFeatureState)
                    {
                        counts[state.ordinal()]++;
                        if (counts[state.ordinal()] > maxScore)
                        {
                            maxstate = state;
                            maxScore = counts[state.ordinal()];
                        }
                    }
                    representativeSummary = maxstate;
                }
                break;
                case SELECT_RANDOM:
                    representativeSummary  = currentFeatureState.get((int)(FastLSTMNetwork.randomLCG()*(1 + featureLength)));
                    break;
                case CONSIDER_LAST:
                    representativeSummary = currentFeatureState.getLast();
                    break;
            }


            if (Tools.DEBUG && prevLevel == null)
            {
                String featureDescription = currentFeatureState.toString();
                featureDescription = featureDescription + " (" + lstmViewer.toString(currentFeatureLSTM.getActualData()) + ")";

                System.out.println("% Completed feature " + featureDescription);
                System.out.println("Using representative feature: " + representativeSummary);
            }
            if (allowLearningP)
            {
                switch (representativeSummary)
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
                        {
                            learnNextFeatureState(lstmInput);
                            currentFeatureState.add(currentFeatureRecognitionSummary);
                        }
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
                        {
                            learnNextFeatureState(lstmInput);
                            currentFeatureState.add(currentFeatureRecognitionSummary);
                        }
                        if (Tools.DEBUG)
                            System.out.println("feature is saved");
                    }
                    break;
                }
            }
            else
            {
                currentFeatureState.clear();
                currentFeatureLSTM = getNetwork();

                if (currentFeatureLSTM == null)
                {
                    throw new RuntimeException("Failure to create capped lstm");
                }
                if (!matchingPredictorsP)
                {
                    learnNextFeatureState(lstmInput);
                    currentFeatureState.add(currentFeatureRecognitionSummary);
                }
            }



        }



        if (stateIsNewP && hasPredictionsP(currentState) && allowHigherLevelP)
        {

            float[] currentStateVector = getStateVector(currentState);

            if (nextLevel != null)
            {
                PREDICTOR_MATCH_STATE[] higherOrderState = nextLevel.observePredict(currentStateVector, null);

                if (hasPredictionsP(higherOrderState))
                {
                    if (Tools.DEBUG)
                    {
                        System.out.println("~<o>~<o>~<o>~<o>~<o>~<o>~<o>~<o>~<o>~");
                        System.out.println("Processing predictions of: " + nextLevel.toString());
                    }

                    // TODO: Filter nextState based on stateGroups
                    float[] nextPredictedState = nextLevel.getPrediction(higherOrderState);
                    if (nextPredictedState != null)
                    {
                        if (Tools.DEBUG)
                            System.out.println("-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-");

                        this.nextPredictedState = getLogicalStateFromHierarchicalVectorState(nextPredictedState);
                        hasNewPredictedStateP =  true;
                        if (lastSelectedPredictor!= -1 && higherOrderState[lastSelectedPredictor] == PREDICTOR_MATCH_STATE.NOT_MATCHING)
                            lastSelectedPredictor = -1;
                        if (Tools.DEBUG)
                            System.out.println("% Predicting next match state and setting as current state: " + Arrays.toString(this.nextPredictedState));

                    }
                    else
                    {
                        if (Tools.DEBUG)
                            System.out.println("No predictions from " + nextLevel.toString());
                    }
                    if (Tools.DEBUG)
                        System.out.println("~<o>~<o>~<o>~<o>~<o>~<o>~<o>~<o>~<o>~");
                }

            }


        }

        if (!hasNewPredictedStateP)
            nextPredictedState = currentState;
        return currentState;

    }

    public boolean hasPredictionsP(PREDICTOR_MATCH_STATE[] state)
    {
        for (int i = 0;i < state.length;i++)
        {
            switch (state[i])
            {
                case STARTING_TO_MATCH_FROM_BEGINNING:
                case STARTING_TO_MATCH_FROM_MIDDLE:
                case MATCHING_FROM_BEGINNING:
                case MATCHING_FROM_MIDDLE:
                case MATCHED_TO_END:
                    return true;

            }

        }

        return false;
    }


    public void setFeatureSaveMode(FEATURE_SAVE_OPTIONS saveMode)
    {
        this.saveMode = saveMode;
    }
    public ModelBuilder addHigherOrderLevel(int capacity)
    {
        int nextLevelDataWidth = this.capacity*PREDICTOR_MATCH_STATE.values().length;
        ModelBuilder builder = new ModelBuilder(nextLevelDataWidth, capacity, __CURRENT_FEATURE_TRAINING_STEP_LIMIT);
        builder.prevLevel = this;
        return nextLevel = builder;
    }

    public float[] getPrediction()
    {
        return getPrediction(nextPredictedState);
    }

    public int getSelectedPredictor()
    {
        return lastSelectedPredictor;
    }

    public PREDICTOR_MATCH_STATE[] getCurrentState()
    {
        return currentState;
    }

    public PREDICTOR_MATCH_STATE[] getNextPredictedState()
    {
        return nextPredictedState;
    }

    public boolean hasNewPredictedStateP()
    {
        return hasNewPredictedStateP;
    }

    public void setState(PREDICTOR_MATCH_STATE[] higherOrderState)
    {
        nextPredictedState = currentState = higherOrderState;
    }



    public float[] getStateVector(PREDICTOR_MATCH_STATE[] state)
    {
        float[][] predictorStatesVectors = new float[capacity][];
        for (int i = 0; i < state.length; i++)
        {
            predictorStatesVectors[i] = state[i].toArray();
        }

        return NNTools.flattenArray(predictorStatesVectors);
    }

    public float[] getPrediction(PREDICTOR_MATCH_STATE[] higherOrderState)
    {
        float[] scoredAverage = new float[higherOrderState.length];
        float maxScore = 0, matchLength = 0;
        HashSet<Integer> maxSet = new HashSet<Integer>();


        for (int i = 0;i < manager.getNumberOfClaimedSlots();i++)
        {
            scoredAverage[i] = 1;
            switch (higherOrderState[i])
            {
                case MATCHED_TO_END:
                    if (currentState[i] == higherOrderState[i])
                        break;
                case STARTING_TO_MATCH_FROM_BEGINNING:
                case STARTING_TO_MATCH_FROM_MIDDLE:
                case MATCHING_FROM_BEGINNING:
                case MATCHING_FROM_MIDDLE:
                    if (-1 != lastSelectedPredictor && higherOrderState[lastSelectedPredictor] != PREDICTOR_MATCH_STATE.NOT_MATCHING)
                    {
                        return _predictors[lastSelectedPredictor].getBestPrediction();
                    }
                    else
                    {
                        matchLength = _predictors[i].getMaxMatchLength();
                        if (maxScore == matchLength)
                        {
                            maxSet.add(Integer.valueOf(i));
                        }
                        else if (matchLength > maxScore)
                        {
                            maxSet = new HashSet<Integer>();
                            maxSet.add(Integer.valueOf(i));
                            maxScore = matchLength;
                        }

                        scoredAverage[i] +=  matchLength;
                    }
                    break;
                default:
            }
        }

        if (maxSet.size() > 0)
        {
            for (int i = 0; i < scoredAverage.length;i++)
            {
                if (!maxSet.contains(Integer.valueOf(i)))
                {
                    scoredAverage[i] = 0;
                }
            }
        }


        lastSelectedPredictor = FastLSTMNetwork.sampleVectorIndexProportionally(scoredAverage, 0, higherOrderState.length, 1, false);

        if (lastSelectedPredictor < manager.getNumberOfClaimedSlots())
        {

            return _predictors[lastSelectedPredictor].getBestPrediction();
        }
        else
            return null;
    }



    PREDICTOR_MATCH_STATE[] getLogicalStateFromPredictionVectorState(float[] vector)
    {


        int predictorStateWidth = PREDICTOR_MATCH_STATE.values().length;
        PREDICTOR_MATCH_STATE[] out = new PREDICTOR_MATCH_STATE[vector.length/predictorStateWidth];
        for (int i = 0; i < out.length;i++)
            out[i] = PREDICTOR_MATCH_STATE.NOT_MATCHING;

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

    public ArrayList<ModelBuilder> getHigherLayers()
    {
        ArrayList<ModelBuilder> levels = new ArrayList<ModelBuilder>();
        if (nextLevel != null)
        {
            levels.add(nextLevel);
            levels.addAll(nextLevel.getHigherLayers());
        }
        return levels;
    }

}
