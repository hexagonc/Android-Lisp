package com.evolved.automata.nn;

import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by Evolved8 on 12/24/16.
 */
public class SequenceLSTM extends LSTMNetwork {

    public static boolean DEBUG = true;
    public static class LSTMNetworkBuilder extends LSTMNetwork.LSTMNetworkBuilder{




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
            inputLayer = new InputLayer(count, new IdentityActivation());
            outputLayer = new OutputLayer(count, new SigmoidActivation(), new LeastSquaresError());
            return this;
        }


        public LSTMNetwork build()
        {
            SequenceLSTM lstm = new SequenceLSTM();

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

            if (inputLayer == null )
            {
                throw new IllegalStateException("Can't build an LSTMNetwork without an input layer");
            }
            lstm.inputLayer = inputLayer;
            if (outputLayer == null )
            {
                throw new IllegalStateException("Can't build an LSTMNetwork without an output layer");
            }

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
            lstm.initialize();
            lstm.weightParameters = weightParameters;

            return lstm;
        }


    }

    Vector initialInputActivation = null;
    int sequenceLength = 0;



    private SequenceLSTM()
    {
        super();
    }

    public SequenceLSTM clear()
    {
        sequenceLength = 0;
        return this;
    }

    public int getSequenceLength()
    {
        return sequenceLength;
    }


    // TODO: decide if need to return the prior last value or allow this to return void
    public void removeLast()
    {

        sequenceLength = Math.max(0, sequenceLength - 1);

    }

    /**
     * Use caution!!!
     * ONLY use this after restoring previous weights
     */
    public void undoRemoveLast()
    {

        sequenceLength++;

    }

    public Vector removeFirst()
    {
        Vector old = initialInputActivation;
        if (sequenceLength>0)
        {
            sequenceLength--;
            if (sequenceLength > 0)
            {
                initialInputActivation = viewSequenceOutput(new Vector[]{initialInputActivation}, false)[0];
            }
        }
        return old;
    }

    public double[] add(Vector value, int maxSteps, double maxAcceptableError)
    {
        if (sequenceLength == 0)
        {
            initialInputActivation = value;
            sequenceLength++;
            return new double[]{0, 0, 0};
        }

        Vector[] input = null;

        if (sequenceLength > 1)
        {
            input = extrapolate(new Vector[]{initialInputActivation}, sequenceLength + 1, false);
            for (int i = sequenceLength;i>0;i--)
            {
                input[i] = input[i-1];
            }
            input[0] = initialInputActivation;
            input[sequenceLength] = value;
        }
        else
        {
            input = new Vector[]{initialInputActivation, value};
        }
        sequenceLength++;

        return learnSequence(input, maxSteps, maxAcceptableError);
    }

    public Vector[] getCurrentSequence()
    {
        Vector[] out = new Vector[sequenceLength];
        if (sequenceLength == 1)
        {
            out[0] = initialInputActivation;
        }
        else if (sequenceLength > 1)
        {
            out = extrapolate(new Vector[]{initialInputActivation}, sequenceLength, false);
            for (int i = sequenceLength-1;i>0;i--)
            {
                out[i] = out[i-1];
            }
            out[0] = initialInputActivation;
        }

        return out;
    }

    public Vector getInitialInputActivation()
    {
        return initialInputActivation;
    }

    public static SequenceLSTM.LSTMNetworkBuilder getSequenceBuilder()
    {
        return new LSTMNetworkBuilder();
    }

}
