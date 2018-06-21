package com.evolved.automata.nn;

import com.evolved.automata.AITools;

import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 1/4/17.
 */
public class HashtableLSTM extends LSTMNetwork {

    public static class LSTMNetworkBuilder extends LSTMNetwork.LSTMNetworkBuilder{
        int endDelimiterValue = -1;
        int keyBitWidth = 4;
        int inputOutputNodeCount = 0;
        public LSTMNetworkBuilder()
        {
            super();
        }

        /**
         * This defines an input layer using the identity activation function
         * and an output layer of the same node count using the least squares
         * error function and the sigmoid activation function
         * @param count
         * @return
         */
        public LSTMNetworkBuilder setInputNodeCount(int count)
        {
            inputOutputNodeCount = count;

            return this;
        }

        public LSTMNetworkBuilder setEndDelimiterKey(int delimiterKey)
        {
            endDelimiterValue = delimiterKey;
            return this;
        }

        public LSTMNetworkBuilder setKeyBitWidth(int width)
        {
            keyBitWidth = width;
            return this;
        }

        public HashtableLSTM build()
        {
            HashtableLSTM lstm = new HashtableLSTM();

            if (connectivityMap == null || connectivityMap.size() == 0)
            {
                throw new IllegalStateException("Can't build an LSTMNetwork without node graph");
            }
            lstm.connectivityMap = connectivityMap;
            if (feedforwardLinkOrder == null || feedforwardLinkOrder.length == 0)
            {
                throw new IllegalStateException("Can't build an LSTMNetwork without specifying order to update links in forward pass");
            }

            lstm.feedforwardLinkOrder = feedforwardLinkOrder;


            if (inputOutputNodeCount == 0 )
            {
                throw new IllegalStateException("Can't build an LSTMNetwork without an input/output node count");
            }



            inputLayer = new InputLayer(inputOutputNodeCount + keyBitWidth, new IdentityActivation());
            outputLayer = new OutputLayer(inputOutputNodeCount + keyBitWidth, new SigmoidActivation(), new LeastSquaresError());



            lstm.inputLayer = inputLayer;
            lstm.outputLayer = outputLayer;
            lstm.memoryCellSpecList = memoryCellSpecList;
            lstm.updateType = updateType;
            if (weightUpdateLinkOrder == null || weightUpdateLinkOrder.length == 0)
            {
                throw new IllegalStateException("Can't build an LSTMNetwork without specifying order to update weights in backward pass");
            }

            lstm.weightUpdateLinkOrder = weightUpdateLinkOrder;

            if (initialNodeMap != null)
                lstm.nodeMap = initialNodeMap;

            if (linkWeightData != null)
            {
                lstm.decodeSerializedLinksToLinkBuffer(linkWeightData);
            }

            if (weightParameters == null || weightParameters.size() == 0)
            {
                throw new IllegalStateException("Can't build an LSTMNetwork without specifying weight update parameters.  These must be consistent with the weight update type, (defaults to standard LSTM backpropagation method)");
            }
            lstm.weightParameters = weightParameters;
            lstm.biasSpecMap = biasSpecMap;
            lstm.keyBitWidth = keyBitWidth;
            lstm.exposedInputLayerCount = inputOutputNodeCount;
            if (endDelimiterValue != -1)
                lstm.END_DELIMITER_VALUE = endDelimiterValue;

            lstm.initialize();

            return lstm;
        }


    }

    int keyBitWidth = 4;
    int keyWidth;
    Vector END_DELIMITER;
    int exposedInputLayerCount;
    int END_DELIMITER_VALUE = -1;
    HashSet<Integer> keys;
    private HashtableLSTM()
    {
        super();

    }

    public static HashtableLSTM.LSTMNetworkBuilder getHashtableLSTMBuilder()
    {
        return new HashtableLSTM.LSTMNetworkBuilder();
    }

    public Integer[] getKeys()
    {
        return keys.toArray(new Integer[0]);
    }


    public int size()
    {
        return keys.size();
    }

    protected void initialize()
    {
        super.initialize();
        keyWidth = (int)Math.pow(2, keyBitWidth);
        keys = new HashSet<Integer>();
        if (END_DELIMITER_VALUE == -1)
            END_DELIMITER_VALUE = keyWidth-1;

        END_DELIMITER = NNTools.padEnd(NNTools.intToStandardBinaryVectorLE(END_DELIMITER_VALUE, keyBitWidth), exposedInputLayerCount, 0);
    }

    public boolean atEndOfListP()
    {
        return END_DELIMITER.equals(getOutputValues().map(roundingMapper));
    }

    public Vector resetToStartOfSequence(int key)
    {
        initializeNodeState();
        clearWeightHistory();
        Vector seed = getInternalKeyRepresentation(key);

        executeForwardPass(seed);
        if (roundOutput)
            return getExternalInputRepresentation(getOutputValues().map(roundingMapper));
        else
            return getExternalInputRepresentation(getOutputValues());
    }


    public Vector getExternalInputRepresentation(Vector internalStateRepresentation)
    {
        return NNTools.getTrimmedVector(internalStateRepresentation, keyBitWidth);
    }

    private Vector getInternalInputRepresentation(Vector input)
    {
        return NNTools.padEnd(input, exposedInputLayerCount, 0);
    }


    private Vector[] getInternalInputRepresentation(final Vector[] input)
    {
        return AITools.mapValues(input, new VectorValueMapper<Vector>() {
            @Override
            public Vector map(Vector input, int index)
            {
                return getInternalInputRepresentation(input);
            }
        });
    }

    private Vector getInternalKeyRepresentation(Vector baseKey)
    {
        return NNTools.padEnd(baseKey, exposedInputLayerCount, 0);
    }

    private Vector getInternalKeyRepresentation(int baseKey)
    {
        return NNTools.padEnd(NNTools.intToStandardBinaryVectorLE(baseKey, keyBitWidth), exposedInputLayerCount, 0);
    }

    public Vector[] get(int key, final boolean includeEnds)
    {
        if (!keys.contains(Integer.valueOf(key)))
            return null;

        Vector seed = getInternalKeyRepresentation(key);
        return AITools.mapValues(getSubSequence(seed, END_DELIMITER, includeEnds), new VectorValueMapper<Vector>() {
            @Override
            public Vector map(Vector input, int index)
            {
                if (!includeEnds)
                    return NNTools.getTrimmedVector(input, keyBitWidth);
                else
                    return input;
            }
        });
    }

    public Vector[] get(int key)
    {
        return get(key, false);

    }

    public Vector[] getSubSequence( Vector startVector, Vector stopVector, boolean includeEndsP)
    {
        Vector nextOutput = null;
        initializeNodeState();
        clearWeightHistory();

        LinkedList<Vector> buffer = new LinkedList<Vector>();

        if (includeEndsP)
            buffer.add(startVector);

        executeForwardPass(startVector);
        if (roundOutput)
            nextOutput = getOutputValues().mapD(roundingMapper);
        else
            nextOutput = getOutputValues();

        int limit = 40;
        int i = 0;
        while (!nextOutput.equals(stopVector))
        {
            i++;

            if (i > limit)
            {
                System.out.println("********* bad **********");
                break;
            }
            buffer.add(nextOutput);
            executeForwardPass(nextOutput);
            nextOutput = getOutputValues();
            if (roundOutput)
                nextOutput.mapD(roundingMapper);
        }
        if (includeEndsP)
            buffer.add(stopVector);
        return buffer.toArray(new Vector[0]);
    }

    public Vector getNextExpectedOutput(boolean externalP)
    {

        if (externalP)
        {
            if (roundOutput)
                return getExternalInputRepresentation(getOutputValues()).mapD(roundingMapper);
            else
                return getExternalInputRepresentation(getOutputValues());
        }
        else
        {
            if (roundOutput)
                return getOutputValues().mapD(roundingMapper);
            else
                return getOutputValues();
        }
    }

    public int getEndDelimiter()
    {
        return END_DELIMITER_VALUE;
    }


    public double[] add(final Vector value, int key,  int maxSteps, double maxAcceptableError)
    {
        HashMap<String, Vector> activationMap = getNetworkActivationSnapshot();
        saveAllLinkWeights();

        Vector keyVector = NNTools.intToStandardBinaryVectorLE(key, keyBitWidth);
        Vector actualValue = NNTools.joinVectors(keyVector, value);
        keyVector = NNTools.padEnd(keyVector, exposedInputLayerCount, 0);
        //Vector actualValue = NNTools.padStart(value, keyBitWidth, 0);

        Vector[] input, segment;
        ArrayList<Pair<Vector, Vector>> trainingSpec = new ArrayList<Pair<Vector, Vector>>();
        boolean containsKey = false;
        for (Integer existingKey:keys)
        {
            if (existingKey.intValue() == key)
            {
                containsKey = true;
                continue;
            }

            if (trainingSpec.size() == 0)
                trainingSpec.add(null);
            segment = get(existingKey.intValue(), true);

            trainingSpec.addAll(getSequenceTrainingSpec(segment));

        }

        if (containsKey)
        {
            Vector nextOutput = null;
            initializeNodeState();
            clearWeightHistory();

            LinkedList<Vector> buffer = new LinkedList<Vector>();

            buffer.add(keyVector);

            executeForwardPass(keyVector);
            if (roundOutput)
                nextOutput = getOutputValues().mapD(roundingMapper);
            else
                nextOutput = getOutputValues();

            while (!nextOutput.equals(END_DELIMITER))
            {
                buffer.add(nextOutput);
                executeForwardPass(nextOutput);
                nextOutput = getOutputValues();
                if (roundOutput)
                    nextOutput.mapD(roundingMapper);
            }
            buffer.add(actualValue);
            buffer.add(END_DELIMITER);
            if (trainingSpec.size()>0)
                trainingSpec.add(null);
            trainingSpec.addAll(getSequenceTrainingSpec(buffer.toArray(new Vector[0])));

        }
        else
        {
            keys.add(key);
            if (trainingSpec.size()>0)
                trainingSpec.add(null);
            trainingSpec.addAll(getSequenceTrainingSpec(new Vector[]{keyVector, actualValue, END_DELIMITER}));
        }

        double[] out = learnInputOutputPairs(trainingSpec, maxSteps, maxAcceptableError);
        if (out[0] > maxAcceptableError)
        {
            setNetworkActivation(activationMap);
            loadbufferedLinkWeights();
            if (!containsKey)
                keys.remove(key);
        }
        return out;

    }

    public HashtableLSTM deleteKey(int key)
    {
        keys.remove(key);
        return this;
    }

    /**
     * This automatically adds the padding bits if there are endcaps
     * @param drivingSequence
     * @param retainPreviousState - flag indicating whether to reinitialize the activation of all nodes
     *                            (which will be the activations defined in initialNodeActivationMap if not
     *                            null) before driving the network with drivingSequence
     * @return
     */
    @Override
    public Vector[] viewSequenceOutput(Vector[] drivingSequence, boolean retainPreviousState)
    {
        Vector[] out = super.viewSequenceOutput(getInternalInputRepresentation(drivingSequence), retainPreviousState);
        return AITools.mapValues(out, new VectorValueMapper<Vector>() {
            @Override
            public Vector map(Vector input, int index)
            {
                return NNTools.getTrimmedVector(input, 2);
            }
        });
    }

    public Vector extrapNext(Vector drivingInput)
    {
        Vector[] out = viewSequenceOutput(new Vector[]{drivingInput}, true);
        return out[0];
    }


}
