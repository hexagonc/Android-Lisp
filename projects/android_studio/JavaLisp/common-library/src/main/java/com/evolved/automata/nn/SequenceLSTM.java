package com.evolved.automata.nn;

import com.evolved.automata.AITools;
import com.evolved.automata.IndexedValueMapper;

import java.util.HashMap;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 12/24/16.
 */
public class SequenceLSTM extends LSTMNetwork {

    public enum EndCapPolicy
    {
        BOTH_ENDS,
        AT_START, // Not used and only partially implemented
        AT_END,  // Not used and only partially implemented
        NONE
    }


    public static boolean DEBUG = true;
    public static class LSTMNetworkBuilder extends LSTMNetwork.LSTMNetworkBuilder{

        EndCapPolicy endCapPolicy = EndCapPolicy.NONE;
        int inputOutputNodeCount = 0;
        EndCapDisplayPolicy capDisplay = EndCapDisplayPolicy.HIDE;
        public LSTMNetworkBuilder()
        {
            super();
        }


        public LSTMNetworkBuilder setCapDisplayPolicy(EndCapDisplayPolicy policy)
        {
            capDisplay = policy;
            return this;
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

        public LSTMNetworkBuilder setEndCapPolicy(EndCapPolicy endCapPolicy)
        {
            this.endCapPolicy = endCapPolicy;

            return this;
        }

        public SequenceLSTM build()
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


            if (inputOutputNodeCount == 0 )
            {
                throw new IllegalStateException("Can't build an LSTMNetwork without an input/output node count");
            }

            int offset = 0;
            switch (this.endCapPolicy)
            {
                case BOTH_ENDS:
                    offset=2;
                    break;
                case AT_START:
                case AT_END:
                    offset = 1;
                    break;
                case NONE:

            }
            inputLayer = new InputLayer(inputOutputNodeCount + offset, new IdentityActivation());
            outputLayer = new OutputLayer(inputOutputNodeCount + offset, new SigmoidActivation(), new LeastSquaresError());



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
            lstm.initialize();
            lstm.weightParameters = weightParameters;

            int length = inputLayer.getDimen();
            Vector startCap = null;
            Vector endCap = null;

            switch (this.endCapPolicy)
            {
                case BOTH_ENDS:
                    startCap = new Vector(length);
                    startCap.setValue(1, 0);
                    endCap = new Vector(length);
                    endCap.setValue(1, 1);
                    break;
                case AT_START:
                    startCap = new Vector(length);
                    startCap.setValue(1, 0);
                    break;
                case AT_END:
                    endCap = new Vector(length);
                    endCap.setValue(1, 0);
                    break;
                case NONE:

            }
            lstm._endCap = endCap;
            lstm._startCap = startCap;
            lstm._endCapPolicy = endCapPolicy;
            lstm._capDisplayPolicy = capDisplay;
            return lstm;
        }


    }

    Vector initialInputActivation = null;
    int sequenceLength = 0;
    EndCapPolicy _endCapPolicy = EndCapPolicy.NONE;
    Vector _startCap;
    Vector _endCap;

    String DEFAULT_VECTOR_STRING_DELIMITER = ",";
    VectorViewer defaultVectorViewer = new VectorViewer() {
        @Override
        public String toString(Vector v)
        {
            return v.toString();
        }
    };

    public enum EndCapDisplayPolicy
    {
        HIDE, EXPOSE
    }

    EndCapDisplayPolicy _capDisplayPolicy = EndCapDisplayPolicy.HIDE;

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
        switch (_capDisplayPolicy)
        {
            case EXPOSE:
                return sequenceLength;
            case HIDE:
                switch (_endCapPolicy)
                {
                    case BOTH_ENDS:
                        return sequenceLength - 2;
                    case NONE:
                        return sequenceLength;
                    case AT_START:
                    case AT_END:
                        return sequenceLength - 1;
                }

        }

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

    public Vector[] getAugmentedVector(Vector[] input)
    {
        return AITools.mapValues(input, new IndexedValueMapper<Vector, Vector>()
        {

            @Override
            public Vector map(final Vector input, int index)
            {
                Vector actualValue = new Vector(input.dimen() + 2);
                actualValue.mapD(new VectorMapper() {
                    @Override
                    public double map(double v, int i)
                    {
                        if (i < 2)
                            return 0;
                        else
                            return input.value(i - 2);
                    }
                });
                return actualValue;
            }

            @Override
            public Vector[] getEmptyOutput()
            {
                return new Vector[0];
            }
        });


    }

    /**
     * This automatically adds the padding bits if there are endcaps
     * @param drivingSequence
     * @param retainPreviousState - flag indicating whether to reinitialize the activation of all nodes
     *                            (which will be the activations defined in initialNodeActivationMap if not
     *                            null) before driving the network with drivingSequence
     * @return
     */
    public Vector[] viewSequenceOutput(Vector[] drivingSequence, boolean retainPreviousState)
    {
        if (_endCapPolicy == EndCapPolicy.BOTH_ENDS)
        {
            Vector[] out = super.viewSequenceOutput(getAugmentedVector(drivingSequence), retainPreviousState);
            return AITools.mapValues(out, new VectorValueMapper<Vector>() {
                @Override
                public Vector map(Vector input, int index)
                {
                    return NNTools.getTrimmedVector(input, 2);
                }
            });
        }
        else
        {
            return super.viewSequenceOutput(drivingSequence, retainPreviousState);

        }
    }

    public String toString(final VectorViewer viewer, final String delimiter)
    {

        if (sequenceLength > 0)
        {

            final StringBuilder builder = new StringBuilder("{");
            AITools.mapValues(getCurrentSequence(), new IndexedValueMapper<Vector, String>() {
                @Override
                public String map(Vector input, int index)
                {
                    if (index > 0)
                        builder.append(delimiter);
                    builder.append(viewer.toString(input));
                    return null;
                }

                @Override
                public String[] getEmptyOutput()
                {
                    return new String[0];
                }
            });
            return builder.append("}").toString();

        }
        else
        {
            return "{}";
        }
    }

    public String toString(final VectorViewer viewer)
    {
       return toString(viewer, DEFAULT_VECTOR_STRING_DELIMITER);
    }

    public String toString()
    {
        return toString(defaultVectorViewer, DEFAULT_VECTOR_STRING_DELIMITER);
    }


    public double[] add(final Vector value, int maxSteps, double maxAcceptableError)
    {
        HashMap<String, Vector> activationMap;
        double[] out = null;
        switch (_endCapPolicy)
        {
            case NONE:
            {
                if (sequenceLength == 0)
                {
                    initialInputActivation = value;
                    sequenceLength++;
                    return new double[]{0, 0, 0};
                }

                Vector[] input = null;
                activationMap = getNetworkActivationSnapshot();
                saveAllLinkWeights();
                if (sequenceLength > 1)
                {
                    input = extrapolate(new Vector[]{initialInputActivation}, sequenceLength + 1, false);
                    for (int i = sequenceLength; i > 0; i--)
                    {
                        input[i] = input[i - 1];
                    }
                    input[0] = initialInputActivation;
                    input[sequenceLength] = value;
                } else
                {
                    input = new Vector[]{initialInputActivation, value};
                }


                out = learnSequence(input, maxSteps, maxAcceptableError);
                if (out[0] <= maxAcceptableError)
                {
                    sequenceLength = sequenceLength + 1;
                }
                else
                {
                    setNetworkActivation(activationMap);
                    loadbufferedLinkWeights();
                }
                break;
            }
            case BOTH_ENDS:
            {
                Vector actualValue = new Vector(value.dimen() + 2);
                actualValue.mapD(new VectorMapper() {
                    @Override
                    public double map(double v, int i)
                    {
                        if (i < 2)
                            return 0;
                        else
                            return value.value(i - 2);
                    }
                });
                if (sequenceLength == 0)
                {
                    out = learnSequence(new Vector[]{_startCap, actualValue, _endCap}, maxSteps, maxAcceptableError);
                    if (out[0] <= maxAcceptableError)
                    {
                        sequenceLength = 3;
                        initialInputActivation = value;
                    }

                    return out;
                }
                activationMap = getNetworkActivationSnapshot();

                saveAllLinkWeights();
                Vector nextOutput = null;
                initializeNodeState();
                clearWeightHistory();

                Vector[] input = new Vector[sequenceLength + 1];
                int i = 0;
                input[i] = _startCap;
                executeForwardPass(input[i]);
                if (roundOutput)
                    nextOutput = getOutputValues().mapD(roundingMapper);
                else
                    nextOutput = getOutputValues();
                while (!nextOutput.equals(_endCap))
                {
                    i++;
                    input[i] = nextOutput;
                    executeForwardPass(input[i]);
                    nextOutput = getOutputValues();
                    if (roundOutput)
                        nextOutput.mapD(roundingMapper);

                }
                i++;
                input[i] = actualValue;
                input[i + 1] = _endCap;
                out = learnSequence(input, maxSteps, maxAcceptableError);
                if (out[0] <= maxAcceptableError)
                {
                    sequenceLength = sequenceLength + 1;
                }
                else
                {
                    setNetworkActivation(activationMap);
                    loadbufferedLinkWeights();
                }

                break;
            }

        }

        return out;


    }

    public SequenceLSTM setCapDisplayPolicy(EndCapDisplayPolicy policy)
    {
        _capDisplayPolicy = policy;
        return this;
    }


    public Vector[] getCurrentSequence()
    {
        HashMap<String, Vector> v = getNetworkActivationSnapshot();
        try
        {
            return getCurrentSequence(false);
        }
        finally
        {
            setNetworkActivation(v);
        }

    }

    void setSequenceLength(int l)
    {
        switch (_capDisplayPolicy)
        {
            case EXPOSE:
                sequenceLength = l;
                break;
            case HIDE:
                switch (_endCapPolicy)
                {
                    case BOTH_ENDS:
                        sequenceLength = l + 2;
                    break;
                    case NONE:
                        sequenceLength = l;
                        break;
                    case AT_START:
                    case AT_END:
                        sequenceLength = l + 1;
                        break;
                }

        }

    }


    public Vector[] getCurrentSequence(boolean hideCaps)
    {
        Vector[] out = new Vector[0];
        if (_endCapPolicy == EndCapPolicy.NONE )
        {
            out = new Vector[sequenceLength];
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
        }
        else
        {
            if (sequenceLength == 0 )
                return out;
            initializeNodeState();
            clearWeightHistory();
            boolean addCaps = _capDisplayPolicy == EndCapDisplayPolicy.EXPOSE;

            Vector nextValue = null;
            int i = 0;
            if (addCaps && !hideCaps)
            {
                Vector[] output = new Vector[sequenceLength];
                executeForwardPass(_startCap);
                output[i] = _startCap;
                if (roundOutput)
                    nextValue = getOutputValues().mapD(roundingMapper);
                else
                    nextValue = getOutputValues();
                while (!nextValue.equals(_endCap))
                {
                    i++;
                    output[i] = nextValue;
                    executeForwardPass(nextValue);
                    nextValue = getOutputValues();
                    if (roundOutput)
                        nextValue.mapD(roundingMapper);

                }
                output[i+1] = _endCap;
                return output;
            }
            else
            {
                int offset  = 2; // vary this by end cap policy eventually
                Vector[] output = new Vector[sequenceLength - offset];
                executeForwardPass(_startCap);
                if (roundOutput)
                    nextValue = getOutputValues().mapD(roundingMapper);
                else
                    nextValue = getOutputValues();
                while (!nextValue.equals(_endCap))
                {

                    output[i] = NNTools.getTrimmedVector(nextValue, 2);
                    executeForwardPass(nextValue);
                    if (roundOutput)
                        nextValue = getOutputValues().mapD(roundingMapper);
                    else
                        nextValue = getOutputValues();
                    i++;
                }
                return output;
            }


        }
        return out;
    }

    @Deprecated
    public Vector getInitialInputActivation()
    {
        return initialInputActivation;
    }

    public boolean atListEnd()
    {
        return super.getOutputValues().map(roundingMapper).equals(_endCap);
    }


    public boolean isEndCap(Vector v)
    {
        return _endCap != null && _endCap.equals(v);
    }

    /**
     * This is basically putting the LSTM in iterator mode.  The Vector returned is the first
     * vector in the list.  If you call executeForwardPass(...) with this Vector as an input,
     * the output values will be the next Vector in the list
     * @return
     */
    public Vector resetToStartOfSequence()
    {
        initializeNodeState();
        clearWeightHistory();

        if (_endCapPolicy == EndCapPolicy.BOTH_ENDS)
        {
            executeForwardPass(_startCap);
            if (_capDisplayPolicy == EndCapDisplayPolicy.HIDE)
                return getOutputValuesExternal();
            else
                return super.getOutputValues();
        }
        return initialInputActivation;
    }

    public static SequenceLSTM.LSTMNetworkBuilder getSequenceBuilder()
    {
        return new LSTMNetworkBuilder();
    }


    public Vector getOutputValuesExternal()
    {
        if (_capDisplayPolicy == EndCapDisplayPolicy.HIDE)
        {
            if (roundOutput)
                return NNTools.getTrimmedVector(getOutputValues().map(roundingMapper), 2);
            else
                return NNTools.getTrimmedVector(getOutputValues(), 2);
        }
        else
        {
            if (roundOutput)
                return super.getOutputValues().map(roundingMapper);
            else
                return super.getOutputValues();
        }

    }

}
