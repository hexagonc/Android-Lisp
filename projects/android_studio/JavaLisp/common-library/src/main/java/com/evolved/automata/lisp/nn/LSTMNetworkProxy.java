package com.evolved.automata.lisp.nn;

import com.evolved.automata.lisp.FloatValue;
import com.evolved.automata.nn.FastLSTMNetwork;
import com.evolved.automata.nn.LSTMNetwork;
import com.evolved.automata.nn.NNTools;
import com.evolved.automata.nn.OutputLayer;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import static com.evolved.automata.nn.FastLSTMNetwork.roundToInt;

/**
 * Created by Evolved8 on 12/25/17.
 */

public class LSTMNetworkProxy {

    public enum NETWORK_TOPOLOGY {
        ORIGINAL(0), PEEPHOLES_ONLY(1), FORGET_GATES_ONLY(2);
        int value;

        NETWORK_TOPOLOGY(int v)
        {
            value = v;
        }

        public int getValue()
        {
            return value;
        }
    }

    float[] mNetwork;

    public static final char DELIMITER = ':';

    public static class NodeState {
        final float[] _nodeActivations;
        final String _serialized;

        private NodeState(float[] network)
        {
            _nodeActivations = network;
            _serialized = serializeFloats(network);
        }

        public float[] getState()
        {
            return _nodeActivations;
        }

        public String serialize()
        {
            return _serialized;
        }

        public static NodeState createNodeState(float[] d)
        {
            return new NodeState(d);
        }

        public static NodeState createNodeState(String stringSerialized)
        {
            return new NodeState(deserializeFloatString(stringSerialized));
        }

        public boolean compare(NodeState rstate, double fractionalError)
        {
            if (rstate._nodeActivations.length != _nodeActivations.length)
                return false;
            for (int i = 0;i < _nodeActivations.length;i++)
            {
                if (Math.abs((rstate._nodeActivations[i] - _nodeActivations[i])/_nodeActivations[i]) > fractionalError)
                    return false;
            }
            return true;
        }

        public float[] getRawActivations(){
            return _nodeActivations;
        }


    }

    protected LSTMNetworkProxy(float[] d)
    {
        mNetwork = d;
    }


    // .oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.
    //      Constructors


    public static LSTMNetworkProxy make(float[] d)
    {
        return new LSTMNetworkProxy(d);
    }
    /**
     *
     * @param numInputOutputNodes
     * @param numMemoryCellStates
     * @return
     */
    public static LSTMNetworkProxy makeStandardStateSequenceNetwork(int numInputOutputNodes, int numMemoryCellStates, int flags)
    {
        return new LSTMNetworkProxy(getStateSequenceNetwork(numInputOutputNodes, numMemoryCellStates, flags));
    }

    public static LSTMNetworkProxy makeStandardBinarySequenceNetwork(int numInputOutputNodes, int numMemoryCellStates, int flags)
    {
        return new LSTMNetworkProxy(getStandardSequenceNetwork(numInputOutputNodes, numMemoryCellStates, flags));
    }

    public static LSTMNetworkProxy makeStandardBinarySequenceClassifierNetwork(int numInput, int numOutputNodes, int numMemoryCellStates, int flags)
    {
        return new LSTMNetworkProxy(getStandardSequenceClassifierNetwork(numInput, numOutputNodes, numMemoryCellStates, flags));
    }


    public static LSTMNetworkProxy duplicate(LSTMNetworkProxy proxy)
    {
        float[] next = Arrays.copyOf(proxy.mNetwork, proxy.mNetwork.length);

        return new LSTMNetworkProxy(next);
    }

    // .oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.


    // <(--~o~--)> <(--~o~--)> <(--~o~--)> <(--~o~--)> <(--~o~--)> <(--~o~--)>
    //                      Standard


    public LSTMNetworkProxy updateWeights(LSTMNetwork.WeightUpdateType updateType)
    {
        FastLSTMNetwork.updateWeightsFromErrors(mNetwork, updateType);
        return this;
    }

    public float[] executeForwardPass(float[] input)
    {
        FastLSTMNetwork.forwardPass(mNetwork, input);
        return FastLSTMNetwork.getOutputActivation(mNetwork);
    }

    /**
     *
     * @param expectedOutput
     * @param roundNetworkOutput
     * @param useRoundedExpectedOutput
     * @param calculateErrorsP
     * @return
     *
     * Modifies expectedOutput if useRoundedExpectedOutput is true
     */
    public float getOutputError(float[] expectedOutput, boolean roundNetworkOutput, boolean useRoundedExpectedOutput, boolean calculateErrorsP)
    {

        if (useRoundedExpectedOutput)
        {
            for (int i = 0;i < expectedOutput.length;i++)
            {
                expectedOutput[i] = roundToInt(expectedOutput[i]);
            }
        }

        if (calculateErrorsP)
            FastLSTMNetwork.updateForwardPassErrors(mNetwork, expectedOutput, calculateErrorsP);

        if (roundNetworkOutput)
            return FastLSTMNetwork.getRoundedOutputError(mNetwork, expectedOutput);
        else
            return FastLSTMNetwork.getOutputError(mNetwork, expectedOutput);
    }

    public NodeState getCurrentNodeState()
    {
        return NodeState.createNodeState(FastLSTMNetwork.getNodeStateSnapshot(mNetwork));
    }

    public float[] getOutputVaues()
    {
        return FastLSTMNetwork.getOutputActivation(mNetwork);
    }

    public LSTMNetworkProxy setNodeState(NodeState state)
    {
        float[] data = state.getState();
        FastLSTMNetwork.loadNodeStateFromSnapshot(mNetwork, data);
        return this;
    }

    public LSTMNetworkProxy setOutputErrorMask(float[] mask)
    {
        FastLSTMNetwork.setOutputErrorMask(mNetwork, mask);
        return this;
    }

    public LSTMNetworkProxy clearOutputErrorMask()
    {

        FastLSTMNetwork.clearOutputErrorMask(mNetwork);
        return this;
    }

    public LSTMNetworkProxy resetNetworkToInitialState()
    {

        FastLSTMNetwork.resetNetworkToInitialState(mNetwork);
        return this;
    }


    public LSTMNetworkProxy randomizeNetworkWeights()
    {
        FastLSTMNetwork.initializeAllWeights(mNetwork);
        return this;
    }


    public LSTMNetworkProxy randomizeNetworkWeights(float fraction)
    {
        FastLSTMNetwork.perturbWeights(mNetwork, fraction);
        return this;
    }


    public LSTMNetworkProxy randomizeNetworkWeights(float fraction, float setValue)
    {
        FastLSTMNetwork.perturbWeights(mNetwork, fraction, setValue);
        return this;
    }


    public String serialize()
    {
        return serializeFloats(mNetwork);
    }



    // <(--~o~--)> <(--~o~--)> <(--~o~--)> <(--~o~--)> <(--~o~--)> <(--~o~--)>


    static float[] getTallyVector(int number, int width, float[] outOfRangeVector)
    {
        if (number > width || number < 0)
            return outOfRangeVector;
        float[] out = new float[width];
        for (int i = 0; i < width;i++)
        {
            if (i < number)
                out[i] = 1.0F;
            else
                out[i] = 0.0F;
        }
        return out;
    }

    static int getTallyVectorInteger(float[] vector, int invalidResult)
    {
        int number = 0;
        boolean finishedP = false;
        for (int i = 0; i < vector.length;i++)
        {
            if (finishedP)
            {
                if (vector[i] > 0.5)
                    return invalidResult;
            }
            else
            {
                if (vector[i] > 0.5)
                    number = i + 1;
                else
                    finishedP = true;
            }

        }
        return number;
    }

    static String getStringFromOneHotVector(float[] vectorChar, String[] alphabet)
    {

        for (int i = 0;i < vectorChar.length;i++)
        {
            if (FastLSTMNetwork.roundToInt(vectorChar[i]) == 1)
            {
                return alphabet[i];
            }
        }
        return null;
    }

    static int getIntegerFromOneHotVector(float[] vectorChar)
    {

        int numOnes = 0, value = -1;
        for (int i = 0;i < vectorChar.length;i++)
        {
            if (FastLSTMNetwork.roundToInt(vectorChar[i]) == 1)
            {
                value = i;
                numOnes++;
            }
        }
        if (numOnes > 1)
            return -1;
        else
            return value;
    }

    static float[][] getOneHotStringRep(String s, String[] alphabet)
    {
        float[][] out = new float[s.length()][];
        HashMap<String, Integer> alphabetMap = new HashMap<String, Integer>();
        String c;
        int i = 0;
        for ( ;i < alphabet.length;i++)
        {
            alphabetMap.put(alphabet[i], i);
        }

        for (i = 0;i < s.length();i++)
        {
            out[i] = getOneHotArray(alphabetMap.get(s.substring(i, i+1)), alphabet.length);
        }
        return out;
    }


    static float[] getOneHotArray(int hot, int alphabetSize)
    {
        float[] out = new float[alphabetSize];
        out[hot] = 1;
        return out;
    }

    static String getStringFromOneHotVectorRep(float[][] vectorString, String[] alphabet)
    {
        StringBuilder sbuilder = new StringBuilder();
        float[] vectorChar;
        outer: for (int c = 0;c<vectorString.length;c++)
        {
            vectorChar = vectorString[c];
            for (int i = 0;i < vectorChar.length;i++)
            {
                if (roundToInt(vectorChar[i]) == 1)
                {
                    sbuilder.append(alphabet[i]);
                    continue outer;
                }
            }

        }
        return sbuilder.toString();
    }

    static String serializeFloats(float[] data)
    {
        StringBuilder bulder = new StringBuilder();
        for (float d:data)
        {
            if (bulder.length() > 0)
                bulder.append(DELIMITER);
            bulder.append(d);
        }
        return bulder.toString();
    }

    static float[] deserializeFloatString(String stringSerialized)
    {
        String[] parts = StringUtils.split(stringSerialized,DELIMITER);
        float[] out = new float[parts.length];
        for (int i = 0; i < out.length;i++)
        {
            out[i] = Float.parseFloat(parts[i]);
        }
        return out;
    }

    // Create template network strings

    private static float[] getStateSequenceNetwork(int inputOutputWidth, int numMemoryCellStates, int flags)
    {
        FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(inputOutputWidth, inputOutputWidth, numMemoryCellStates, flags);
        builder.setInputNodeCount(inputOutputWidth, FastLSTMNetwork.CROSS_ENTROPY_ERROR_ID, FastLSTMNetwork.SOFTMAX_ACTIVATION_ID);
        FastLSTMNetwork network = builder.build();
        return network.getActualData();
    }

    private static float[] getStandardSequenceNetwork(int numInputNodes, int memoryCellCount, int flags)
    {
        return getStandardBuilder(numInputNodes, numInputNodes, memoryCellCount, flags).build().getActualData();
    }

    private static float[] getStandardSequenceClassifierNetwork(int numInputNodes, int numOutputNodes, int memoryCellCount, int flags)
    {
        FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(numInputNodes, numOutputNodes, memoryCellCount, flags);
        builder.setInputNodeCount(numInputNodes, FastLSTMNetwork.CROSS_ENTROPY_ERROR_ID, FastLSTMNetwork.SOFTMAX_ACTIVATION_ID);
        builder.setOutputNodeCount(numOutputNodes);
        return builder.build().getActualData();
    }


    // Tools

    static FastLSTMNetwork.LSTMNetworkBuilder getStandardBuilder(int inputNodeCode, int outputNodeCode, int numMemoryCellStates, int flags)
    {
        FastLSTMNetwork.LSTMNetworkBuilder builder = FastLSTMNetwork.getFastBuilder();

        builder.setInputNodeCount(inputNodeCode, FastLSTMNetwork.MSE_ERROR_FUNCTION_ID, FastLSTMNetwork.SIGMOID_ACTIVATION_ID);
        builder.setOutputNodeCount(outputNodeCode);
        builder.addMemoryCell("M", numMemoryCellStates);
        HashMap<String, ArrayList<String>> connectivityMap = null;

        switch (flags)
        {
            case 0: // standard
            {
                connectivityMap = NNTools.getStandardLinkConnectivityMap("M");
                for (String sourceNode:connectivityMap.keySet())
                {
                    builder.addNodeConnections(sourceNode, NNTools.arrayListToArray(connectivityMap.get(sourceNode)));
                }

                builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrder("M"));
                builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrder("M"));
            }
            break;
            case 1: // peepholes
            {
                connectivityMap = NNTools.getStandardLinkConnectivityMapWithPeepholes("M");
                for (String sourceNode:connectivityMap.keySet())
                {
                    builder.addNodeConnections(sourceNode, NNTools.arrayListToArray(connectivityMap.get(sourceNode)));
                }

                builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrderWithPeepholes("M"));
                builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrderWithPeepholes("M"));
            }
            break;
            case 2: // forget gates
            {
                connectivityMap = NNTools.getStandardLinkConnectivityMapWithForgetGates("M");
                for (String sourceNode:connectivityMap.keySet())
                {
                    builder.addNodeConnections(sourceNode, NNTools.arrayListToArray(connectivityMap.get(sourceNode)));
                }

                builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrderWithForgetGates("M"));
                builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrderWithForgetGates("M"));
            }
            break;
            case 3: // peepholes and forget gates
            {
                connectivityMap = NNTools.getStandardLinkConnectivityMapWithPeepholesAndForgetGates("M");
                for (String sourceNode:connectivityMap.keySet())
                {
                    builder.addNodeConnections(sourceNode, NNTools.arrayListToArray(connectivityMap.get(sourceNode)));
                }

                builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrderWithPeepholesAndForgetGates("M"));
                builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrderWithPeepholesAndForgetGates("M"));
            }
            break;

        }



        builder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.INITIAL_DELTA, 0.012);
        builder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MAX_DELTA, 50);
        builder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MIN_DELTA, 0);
        builder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MAX, 1.2);
        builder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MIN, 0.5);
        builder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.CONVERGENCE_THRESHOLD, 0.0001);
        builder.setWeightUpdateType(LSTMNetwork.WeightUpdateType.RPROP);

        return builder;
    }
}
