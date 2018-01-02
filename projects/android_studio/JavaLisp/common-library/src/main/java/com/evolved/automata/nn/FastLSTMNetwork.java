package com.evolved.automata.nn;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 2/10/17.
 */

public class FastLSTMNetwork extends LSTMNetwork{


    public static class TrainingSpec
    {
        public float[] input;
        public float[] expectedOutput;
        public float[] errorMask;
        public boolean skipMinAcceptabledErrorCheckP;
        public boolean resetNetworkStateP;
        public boolean useAverageErrorP;

        public int recordLength = 0;

        public float[] record;

        public float[] toArray()
        {
            int inputWidth = input.length;
            int outputWidth = expectedOutput.length;
            int inputOffsetIndex = 0;
            int outputOffsetIndex = inputOffsetIndex + inputWidth;
            int errorMaskOffsetIndex = outputOffsetIndex + outputWidth;
            int skipMinAcceptabledErrorCheckIndex = errorMaskOffsetIndex + outputWidth;
            int resetNetworkStateIndex = skipMinAcceptabledErrorCheckIndex + 1;
            int useAverageErrorIndex = resetNetworkStateIndex + 1;
            int hasErrorMaskIndex = useAverageErrorIndex + 1;
            int recordLength = hasErrorMaskIndex + 1;

            this.record = new float[recordLength];
            this.recordLength = recordLength;

            for (int i = 0; i < inputWidth;i++)
            {
                record[inputOffsetIndex + i] = input[i];
            }

            for (int i = 0; i < outputWidth;i++)
            {
                record[outputOffsetIndex + i] = expectedOutput[i];
            }

            if (errorMask != null)
            {
                for (int i = 0; i < outputWidth;i++)
                {
                    record[errorMaskOffsetIndex + i] = errorMask[i];
                }
                record[hasErrorMaskIndex] = 1;
            }
            else
                record[hasErrorMaskIndex] = 0;

            record[skipMinAcceptabledErrorCheckIndex] = (skipMinAcceptabledErrorCheckP)?1:0;
            record[resetNetworkStateIndex] = (resetNetworkStateP)?1:0;
            record[useAverageErrorIndex] = (useAverageErrorP)?1:0;
            return record;
        }


    }


    public static float[] getTrainingSpecBuffer(LinkedList<TrainingSpec> spec)
    {
        int totalLength = 0;
        float[] output;
        for (TrainingSpec tspec:spec)
        {
            tspec.toArray();
            totalLength+=tspec.recordLength;
        }
        output = new float[totalLength];
        int j = 0;
        for (TrainingSpec tspec:spec)
        {
            for (int i = 0; i < tspec.recordLength;i++, j++)
            {
                output[j] = tspec.record[i];
            }
        }
        return output;
    }


    public static TrainingSpec trainingSpec(float[] input, float[] expectedOutput, float[] errorMask, boolean skipMinErrorCheck, boolean resetNetworkStateP)
    {
        TrainingSpec spec = new TrainingSpec();
        spec.errorMask = errorMask;
        spec.input = input;
        spec.expectedOutput = expectedOutput;
        spec.skipMinAcceptabledErrorCheckP = skipMinErrorCheck;
        spec.resetNetworkStateP = resetNetworkStateP;
        spec.useAverageErrorP =  false;

        return spec;
    }


    public static class LSTMNetworkBuilder {

        HashMap<String, NodeGroup> initialNodeMap = null;
        OutputLayer outputLayer;
        InputLayer inputLayer;
        ArrayList<Pair<String, Integer>> memoryCellSpecList;
        String[] feedforwardLinkOrder;
        String[] weightUpdateLinkOrder;
        HashMap<String, ArrayList<String>> connectivityMap;
        LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.DEFAULT;
        HashMap<LSTMNetwork.WeightUpdateParameters, Double> weightParameters;
        String linkWeightData;
        HashMap<String, Double> biasSpecMap;
        int outputErrorFunctionId = MSE_ERROR_FUNCTION_ID;
        int outputActivationFunctionId = SIGMOID_ACTIVATION_ID;




        public LSTMNetworkBuilder()
        {
            memoryCellSpecList = new ArrayList<Pair<String, Integer>>();
            connectivityMap = new HashMap<String, ArrayList<String>>();
            weightParameters = new HashMap<LSTMNetwork.WeightUpdateParameters, Double>();
            biasSpecMap = new HashMap<String, Double> ();
        }

        public FastLSTMNetwork.LSTMNetworkBuilder setLinkData(String data)
        {
            linkWeightData = data;
            return this;
        }

        public FastLSTMNetwork.LSTMNetworkBuilder addWeightParameter(LSTMNetwork.WeightUpdateParameters param, double value)
        {
            weightParameters.put(param, value);
            return this;
        }
        public FastLSTMNetwork.LSTMNetworkBuilder setInputLayer(InputLayer input)
        {
            inputLayer = input;
            return this;
        }

        public FastLSTMNetwork.LSTMNetworkBuilder setLinkWeightSerializedData(String data)
        {
            linkWeightData = data;
            return this;
        }

        public FastLSTMNetwork.LSTMNetworkBuilder setOutputLayer(OutputLayer output)
        {
            outputLayer = output;
            return this;
        }

        /**
         * This implicitly defines an input layer using the identity squashing function
         * @param count
         * @return
         */
        public FastLSTMNetwork.LSTMNetworkBuilder setInputNodeCount(int count, int errorFunctionId, int outputActivationId)
        {
            inputLayer = new InputLayer(count, new IdentityActivation());
            this.outputActivationFunctionId = outputActivationId;
            this.outputErrorFunctionId = errorFunctionId;
            return this;
        }

        /**
         * This implicitly defines an output layer using the least squares error function
         * and a sigmoid activation
         * @param count
         * @return
         */
        public FastLSTMNetwork.LSTMNetworkBuilder setOutputNodeCount(int count)
        {
            outputLayer = new OutputLayer(count, new SigmoidActivation(), new LeastSquaresError());
            return this;
        }

        public FastLSTMNetwork.LSTMNetworkBuilder addFeedForwardLinkOrder(String[] linkOrder)
        {
            if (linkOrder == null || linkOrder.length < 1)
                throw new IllegalArgumentException("Link order cannot be null or empty");

            feedforwardLinkOrder = linkOrder;
            return this;
        }

        public FastLSTMNetwork.LSTMNetworkBuilder addWeightUpdateOrder(String[] updateOrderSpec)
        {
            if (updateOrderSpec == null || updateOrderSpec.length < 1)
                throw new IllegalArgumentException("Update order cannot be null or empty");

            weightUpdateLinkOrder = updateOrderSpec;
            return this;
        }

        public FastLSTMNetwork.LSTMNetworkBuilder addMemoryCell(String name, int stateSize)
        {
            memoryCellSpecList.add(Pair.of(name, stateSize));
            return this;
        }

        public FastLSTMNetwork.LSTMNetworkBuilder addNodeConnections(String sourceName, String[] targerNodeNames)
        {
            ArrayList<String> targets = new ArrayList<String>();
            for (String s: targerNodeNames)
            {
                targets.add(s);
            }

            connectivityMap.put(sourceName, targets);
            return this;
        }

        public FastLSTMNetwork.LSTMNetworkBuilder setWeightUpdateType(LSTMNetwork.WeightUpdateType updateType)
        {
            this.updateType = updateType;
            return this;
        }

        public FastLSTMNetwork.LSTMNetworkBuilder setNodeMap(HashMap<String, NodeGroup> initialNodeMap)
        {
            this.initialNodeMap = initialNodeMap;
            return this;
        }

        public FastLSTMNetwork.LSTMNetworkBuilder setBiasLinkWeight(String linkSpecKey, double biasWeight)
        {
            biasSpecMap.put(linkSpecKey, biasWeight);
            return this;
        }

        public FastLSTMNetwork build()
        {
            FastLSTMNetwork lstm = new FastLSTMNetwork();

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
            lstm.outputErrorFunctionId = outputErrorFunctionId;
            lstm.outputActivationFunctionId = outputActivationFunctionId;
            lstm.initialize();
            lstm.weightParameters = weightParameters;

            return lstm;
        }


    }



    // ---------------------------------------------------------------
    // Port these interfaces to c/c++ function pointer typedefs
    // ---------------------------------------------------------------


    public interface UnaryOperator
    {
        public float operation(float value);
    }


    public interface UnaryVectorOperator
    {
        /**
         *
         * @param data
         * @param vectorSpecificationIndex index in the data where the vector is specified.  This index
         *                                 should point to the vector data itself
         * @param i the index in the vector to apply the operator to
         * @return
         */
        public float operate(float[] data, int vectorSpecificationIndex, int length, int i);
    }




    public interface OutputErrorFunction
    {
        double error(float[] networkData, float[] expectedOutput);
        double errorPartialDerivative(float[] networkData, float[] expectedOutput, int i);
    }

    // ---------------------------------------------------------------
    // Port these instances to c/c++ function definitions
    // ---------------------------------------------------------------

    public static final OutputErrorFunction MeanSquaredError = new OutputErrorFunction()
    {

        @Override
        public double error(float[] networkData, float[] expectedOutput)
        {
            int errorMaskIndex =  getOutputLayerMaskIndex(networkData);
            int width = getLayerWidth(networkData, OUTPUT_LAYER_ID);
            int output_layer_activation_idx = getLayerActivationIndex(networkData, OUTPUT_LAYER_ID);
            double mse = 0, error;
            double output_activation, expected_activation;
            float mask;
            float weightSum = 0;
            for (int i = 0;i < width;i++)
            {
                output_activation = networkData[output_layer_activation_idx + i];
                expected_activation = expectedOutput[i];
                error = (expected_activation - output_activation);
                mask = getVectorValue(networkData, errorMaskIndex, i);
                mse += error*error*mask;
                weightSum+=mask;
            }
            return mse/weightSum;
        }

        @Override
        public double errorPartialDerivative(float[] networkData, float[] expectedOutput, int i)
        {
            int errorMaskIndex =  getOutputLayerMaskIndex(networkData);
            int width = getLayerWidth(networkData, OUTPUT_LAYER_ID);
            int output_layer_activation_idx = getLayerActivationIndex(networkData, OUTPUT_LAYER_ID);
            double output_activation = networkData[output_layer_activation_idx + i];
            float mask = getVectorValue(networkData, errorMaskIndex, i);
            return 2*(expectedOutput[i] - output_activation)* mask/width;
        }
    };


    public static final OutputErrorFunction CrossEntropyError = new OutputErrorFunction()
    {

        @Override
        public double error(float[] networkData, float[] expectedOutput)
        {
            int outputLayerActivationFunctionId = getLayerActivationFunctionId(networkData, OUTPUT_LAYER_ID);
            switch (outputLayerActivationFunctionId)
            {
                case SIGMOID_ACTIVATION_ID:
                    // TODO: implement stable version for sigmoid
                case SOFTMAX_ACTIVATION_ID:
                    // TODO: implement stable version for softmax
                case RECTILINEAR_ACTIVATION_ID:
                    // TODO: implement stable version for ReLU
                default:
                {
                    int width = getLayerWidth(networkData, OUTPUT_LAYER_ID);
                    double error = 0, output, expected, errorMask;
                    int activationIndex = getLayerActivationIndex(networkData, OUTPUT_LAYER_ID);
                    int errorMaskIndex =  getOutputLayerMaskIndex(networkData);
                    float nodeError;
                    double minY = 0.001;
                    // Normally we would use ∑t_i *Ln(y_i)
                    for (int i = 0; i < width; i++)
                    {
                        nodeError = 0;
                        errorMask = getVectorValue(networkData, errorMaskIndex, i);

                        expected = expectedOutput[i];
                        if (errorMask != 0 && expected != 0)
                        {

                            output = getVectorValue(networkData, activationIndex, i);
                            nodeError = (float)(expected * Math.log(Math.max(minY, output)/expected));

                        }

                        error +=nodeError*errorMask;

                    }
                    if (error != error)
                    {
                        throw new RuntimeException("Invalid data");
                    }
                    return -1*error;
                }

            }

        }

        @Override
        public double errorPartialDerivative(float[] networkData, float[] expectedOutput, int i)
        {
            int errorMaskIndex =  getOutputLayerMaskIndex(networkData);
            int activationIndex = getLayerActivationIndex(networkData, OUTPUT_LAYER_ID);
            return (expectedOutput[i] - getVectorValue(networkData, activationIndex, i))*getVectorValue(networkData, errorMaskIndex, i);
        }
    };


    public static final UnaryOperator SigmoidOperator = new UnaryOperator() {
        @Override
        public float operation(float value)
        {

            return (float)(1/(1 + Math.exp(-1*value)));
        }
    };



    public static final UnaryOperator SigmoidDerivativeOperator = new UnaryOperator() {
        @Override
        public float operation(float value)
        {
            double sig = (1/(1 + Math.exp(-1*value)));
            return (float)(sig * (1 - sig));
        }
    };


    public static final UnaryOperator HypertangentOperator = new UnaryOperator() {
        @Override
        public float operation(float value)
        {

            return (float)Math.tanh(value);
        }
    };


    public static final UnaryOperator HypertangentDerivativeOperator = new UnaryOperator() {
        @Override
        public float operation(float value)
        {
            double cosh = Math.cosh(value);
            return (float)(1/cosh/cosh);
        }
    };


    public static final UnaryOperator IdentityOperator = new UnaryOperator() {
        @Override
        public float operation(float value)
        {

            return value;
        }
    };



    public static final UnaryOperator IdentityDerivativeOperator = new UnaryOperator() {
        @Override
        public float operation(float value)
        {

            return 1;
        }
    };

    public static final UnaryOperator RectilinearOperator = new UnaryOperator() {
        @Override
        public float operation(float value)
        {
            if (value < 0)
                return 0;
            return value;
        }
    };

    public static final UnaryOperator RectilinearDerivativeOperator = new UnaryOperator() {
        @Override
        public float operation(float value)
        {
            if (value < 0)
                return 0;
            return 1;
        }
    };


    public static final UnaryVectorOperator SoftmaxVectorOperator = new UnaryVectorOperator() {
        @Override
        public float operate(float[] data, int vectorSpecificationIndex, int length, int i)
        {

            double sum = 0, maxValue = Float.MIN_VALUE, iValue=0, value, vectorValue;
            // calculate
            for (int j = 0; j < length;j++ )
            {
                vectorValue = data[vectorSpecificationIndex + j];
                maxValue = Math.max(maxValue, vectorValue);
            }

            for (int j = 0; j < length;j++ )
            {
                vectorValue = data[vectorSpecificationIndex + j] - maxValue;
                value = Math.exp(vectorValue );
                sum +=value;
                if (j == i)
                {
                    iValue = value;
                }
            }



            if (sum == 0)
            { // this probably means numerical underflow somewhere.  Will have to treat all possibilities as the same
                return 1/length;
            }
            float out = (float)(iValue/sum);
            if (out != out)
            {
                throw new RuntimeException("NaN exception");
            }

            return out;
        }
    };

    public static final UnaryVectorOperator SoftmaxVectorDerivativeOperator = new UnaryVectorOperator() {
        @Override
        public float operate(float[] data, int vectorSpecificationIndex, int length, int i)
        {
            float s_i = SoftmaxVectorOperator.operate(data, vectorSpecificationIndex, length, i);
            return s_i * (1 - s_i);
        }
    };


    public static final UnaryVectorOperator SigmoidVectorOperator = new UnaryVectorOperator() {
        @Override
        public float operate(float[] data, int vectorSpecificationIndex, int length, int i)
        {
            float vectorValue = data[vectorSpecificationIndex + i];

            return SigmoidOperator.operation(vectorValue);
        }
    };

    public static final UnaryVectorOperator SigmoidVectorDerivativeOperator = new UnaryVectorOperator() {
        @Override
        public float operate(float[] data, int vectorSpecificationIndex, int length, int i)
        {
            float vectorValue = data[vectorSpecificationIndex  + i];
            return SigmoidDerivativeOperator.operation(vectorValue);
        }
    };

    public static final UnaryVectorOperator HypertangentVectorOperator = new UnaryVectorOperator() {
        @Override
        public float operate(float[] data, int vectorSpecificationIndex, int length,  int i)
        {
            float vectorValue = data[vectorSpecificationIndex + i];

            return HypertangentOperator.operation(vectorValue);
        }
    };

    public static final UnaryVectorOperator HypertangentVectorDerivativeOperator = new UnaryVectorOperator() {
        @Override
        public float operate(float[] data, int vectorSpecificationIndex, int length, int i)
        {
            float vectorValue = data[vectorSpecificationIndex + i];
            return HypertangentDerivativeOperator.operation(vectorValue);
        }
    };

    public static final UnaryVectorOperator RectilinearVectorOperator = new UnaryVectorOperator() {
        @Override
        public float operate(float[] data, int vectorSpecificationIndex, int length, int i)
        {
            float vectorValue = data[vectorSpecificationIndex + i];

            return RectilinearOperator.operation(vectorValue);
        }
    };

    public static final UnaryVectorOperator RectilinearVectorDerivativeOperator = new UnaryVectorOperator() {
        @Override
        public float operate(float[] data, int vectorSpecificationIndex, int length, int i)
        {
            float vectorValue = data[vectorSpecificationIndex + i];
            return RectilinearDerivativeOperator.operation(vectorValue);
        }
    };

    public static final UnaryVectorOperator IdentityVectorOperator = new UnaryVectorOperator() {
        @Override
        public float operate(float[] data, int vectorSpecificationIndex, int length, int i)
        {
            float vectorValue = data[vectorSpecificationIndex + i];

            return IdentityOperator.operation(vectorValue);
        }
    };

    public static final UnaryVectorOperator IdentityVectorDerivativeOperator = new UnaryVectorOperator() {
        @Override
        public float operate(float[] data, int vectorSpecificationIndex, int length, int i)
        {
            float vectorValue = data[vectorSpecificationIndex + i];
            return IdentityDerivativeOperator.operation(vectorValue);
        }
    };


    float[] _networkData;

    static final float CURRENT_VERSION = 1;
    static final float MIN_INITIAL_WEIGHT = -1;
    static final float MAX_INITIAL_WEIGHT = 1;
    static final float N_PLUS = 1.2F;
    static final float N_MINUS = 0.5F;
    static final float MIN_WEIGHT_DELTA = 0;
    static final float MAX_WEIGHT_DELTA = 50;
    static final float INITIAL_WEIGHT_DELTA = 0.0125F;
    static final float LEARNING_RATE= 0.001F;
    // not currently used
    static final float MOMENTUM_FRACTION =  0.1F;
    // ******************************************************
    // START: Port these constants to c/c++ as static variables/defines
    // ******************************************************

    static final int ERROR_STATUS_FAILURE = 1;
    static final int ERROR_STATUS_SUCESS = 0;

    static final int LINK_DATA_SOURCE_LAYER_ID_IDX = 0;
    static final int LINK_DATA_TARGET_LAYER_ID_IDX = 1;
    static final int LINK_DATA_WEIGHT_MATRIX_IDX = 2;

    static final int NUM_LAYERS = 8;
    static final int NUM_CUSTOM_DATA_REGISTERS = 10;

    static final int VERSION_IDX = 0;
    static final int NETWORK_LENGTH_IDX = VERSION_IDX + 1;
    static final int CUSTOM_VARIABLE_DATA_IDX = NETWORK_LENGTH_IDX + 1;
    static final int CUSTOM_DATA_1_IDX = CUSTOM_VARIABLE_DATA_IDX + 1;
    static final int FORWARD_PASS_LINK_ORDER_DATA_IDX = CUSTOM_DATA_1_IDX + NUM_CUSTOM_DATA_REGISTERS;
    static final int BACKWARD_PASS_LINK_ORDER_DATA_IDX = FORWARD_PASS_LINK_ORDER_DATA_IDX + 1;

    static final int MIN_INITIAL_WEIGHT_IDX = BACKWARD_PASS_LINK_ORDER_DATA_IDX + 1;
    static final int MAX_INITIAL_WEIGHT_IDX = MIN_INITIAL_WEIGHT_IDX + 1;
    static final int N_PLUS_IDX = MAX_INITIAL_WEIGHT_IDX + 1;
    static final int N_MINUS_IDX = N_PLUS_IDX + 1;
    static final int MIN_WEIGHT_DELTA_IDX = N_MINUS_IDX + 1;
    static final int MAX_WEIGHT_DELTA_IDX = MIN_WEIGHT_DELTA_IDX + 1;
    static final int INITIAL_WEIGHT_DELTA_IDX = MAX_WEIGHT_DELTA_IDX + 1;
    static final int LEARNING_RATE_IDX= INITIAL_WEIGHT_DELTA_IDX + 1;
    static final int MOMENTUM_FRACTION_IDX =  LEARNING_RATE_IDX + 1;
    



    static final int OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX = MOMENTUM_FRACTION_IDX + 1;

    static final int LAYER_ACTIVATION_FUNCTION_ID_IDX = OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX + 1;
    // Start of data map indexes
    /**
     * Location of the mapping between linkIds and link data indices
     */
    static final int LINK_ID_TO_DATA_IDX_IDX = LAYER_ACTIVATION_FUNCTION_ID_IDX + NUM_LAYERS;

    /**
     * Defines the connectivity for each layer in the canonical layer order,
     * which is I, O, CI, CO, F, IG, OG, P
     * Forward link spec from layer i to layer j can be found as
     * NETWORK_GRAPH_BASE_IDX + 2*i.  A link spec consists of first the number
     * of links and second, an array of the layerids themselves
     */
    static final int NETWORK_GRAPH_BASE_IDX = LINK_ID_TO_DATA_IDX_IDX + NUM_LAYERS * NUM_LAYERS;
    static final int INPUT_NODE_COUNT_IDX = NETWORK_GRAPH_BASE_IDX + (1 + NUM_LAYERS)* NUM_LAYERS; // some wasted space
    static final int OUTPUT_NODE_COUNT_IDX = INPUT_NODE_COUNT_IDX + 1;
    static final int NUM_CELL_STATES_IDX = OUTPUT_NODE_COUNT_IDX + 1;
    static final int LAYER_DIMEN_MAP_IDX = NUM_CELL_STATES_IDX + 1;
    static final int LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX = LAYER_DIMEN_MAP_IDX + NUM_LAYERS;
    static final int LAYER_STATE_BASE_IDX = LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + NUM_LAYERS;


    static final int INPUT_LAYER_ID = 0;
    static final int OUTPUT_LAYER_ID = 1;
    static final int CELL_INPUT_LAYER_ID = 2;
    static final int CELL_OUTPUT_LAYER_ID = 3;
    static final int FORGET_LAYER_ID = 4;
    static final int INPUT_GATE_LAYER_ID = 5;
    static final int OUTPUT_GATE_LAYER_ID = 6;
    static final int PEEPHOLE_LAYER_ID = 7;


    // Activation function ids
    public static final int SIGMOID_ACTIVATION_ID = 0;
    public static final int HYPERTANGENT_ACTIVATION_ID = 1;
    public static final int RECTILINEAR_ACTIVATION_ID = 2;
    public static final int IDENTITY_ACTIVATION_ID = 3;
    public static final int SOFTMAX_ACTIVATION_ID = 4;

    static final UnaryVectorOperator[] ACTIVATION_FUNCTION_MAP = {SigmoidVectorOperator, HypertangentVectorOperator, RectilinearVectorOperator, IdentityVectorOperator, SoftmaxVectorOperator};
    static final UnaryVectorOperator[] ACTIVATION_FUNCTION_DERIVATIVE_MAP = {SigmoidVectorDerivativeOperator, HypertangentVectorDerivativeOperator, RectilinearVectorDerivativeOperator, IdentityVectorDerivativeOperator, SoftmaxVectorDerivativeOperator};



    static final int[] LAYER_ID_ACTIVATION_FUNCTION_MAP = {IDENTITY_ACTIVATION_ID, SIGMOID_ACTIVATION_ID, SIGMOID_ACTIVATION_ID, SIGMOID_ACTIVATION_ID, SIGMOID_ACTIVATION_ID, SIGMOID_ACTIVATION_ID, SIGMOID_ACTIVATION_ID, HYPERTANGENT_ACTIVATION_ID};


    public static final int MSE_ERROR_FUNCTION_ID = 0;
    public static final int CROSS_ENTROPY_ERROR_ID = 1;

    static final OutputErrorFunction[] ERROR_FUNCTION_MAP = {MeanSquaredError, CrossEntropyError};
    // Default layer activation definition

    // ******************************************************
    // END: Constants ported to c/c++ as static variables/defines
    // ******************************************************




    int outputErrorFunctionId = 0;
    int outputActivationFunctionId = 0;

    public final HashMap<String, Integer> layerNameShortToLayerIdMap = new HashMap<String, Integer>(){
        {
            put("I", Integer.valueOf(INPUT_LAYER_ID));
            put("O", Integer.valueOf(OUTPUT_LAYER_ID));
            put("CI", Integer.valueOf(CELL_INPUT_LAYER_ID));
            put("CO", Integer.valueOf(CELL_OUTPUT_LAYER_ID));
            put("FG", Integer.valueOf(FORGET_LAYER_ID));
            put("IG", Integer.valueOf(INPUT_GATE_LAYER_ID));
            put("OG", Integer.valueOf(OUTPUT_GATE_LAYER_ID));
            put("P", Integer.valueOf(PEEPHOLE_LAYER_ID));
        }
    } ;



    protected FastLSTMNetwork()
    {
        super();
    }




    protected void setRawData()
    {
        HashMap<Integer, Float> network = new HashMap<Integer, Float>();

        network.put(VERSION_IDX, CURRENT_VERSION);
        network.put(NETWORK_LENGTH_IDX, 0F);
        // Set default learning and weight parameters
        // some of these parameters can be overriden in the builder, some must be manuall overriden
        network.put(N_MINUS_IDX, N_MINUS);
        network.put(N_PLUS_IDX, N_PLUS);
        network.put(LEARNING_RATE_IDX, LEARNING_RATE);
        network.put(MIN_WEIGHT_DELTA_IDX, MIN_WEIGHT_DELTA);
        network.put(MAX_WEIGHT_DELTA_IDX, MAX_WEIGHT_DELTA);
        network.put(INITIAL_WEIGHT_DELTA_IDX, INITIAL_WEIGHT_DELTA);

        network.put(MIN_INITIAL_WEIGHT_IDX, MIN_INITIAL_WEIGHT);
        network.put(MAX_INITIAL_WEIGHT_IDX, MAX_INITIAL_WEIGHT);


        int numInputNodes = inputLayer.getDimen();
        int numOutputNodes = outputLayer.getDimen();
        int numMemoryCellStates = 0;


        // Use default layer activation functions for now
        // TODO: make this setable from the builder, especially will want to be able to change the output activation function
        // Update: 2/15/2017 - currently allowing the builder to set the output error function and output activation
        for (int i = 0; i< LAYER_ID_ACTIVATION_FUNCTION_MAP.length;i++)
        {
            network.put(LAYER_ACTIVATION_FUNCTION_ID_IDX + i, (float)LAYER_ID_ACTIVATION_FUNCTION_MAP[i]);
        }

        network.put(LAYER_ACTIVATION_FUNCTION_ID_IDX + OUTPUT_LAYER_ID, (float)outputActivationFunctionId);
        network.put(OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX, (float)outputErrorFunctionId);




        // Only works when there is one memory-cell
        for (Pair<String, Integer> cellSpec: memoryCellSpecList)
        {
            int size = cellSpec.getRight();
            numMemoryCellStates+=size;
        }

        HashMap<Integer, LinkedList<Integer>> graphMap = new HashMap<Integer, LinkedList<Integer>>();
        LinkedList<Integer> connections = null;
        Integer sourceId, targetId;
        int layerIdIdx = 0;

        // Build the nodegroup graph



        for (String sourceNodeName: connectivityMap.keySet())
        {
            ArrayList<String> targetNodeNames = connectivityMap.get(sourceNodeName);
            sourceId = getLayerIdFromGroupName(sourceNodeName);
            connections = graphMap.get(sourceId);
            layerIdIdx = 0;
            network.put(Integer.valueOf(NETWORK_GRAPH_BASE_IDX + sourceId*(NUM_LAYERS + 1)), (float)targetNodeNames.size());


            if (connections == null)
            {
                connections = new LinkedList<Integer>();
                graphMap.put(sourceId, connections);
            }

            for (String targetNodeName:targetNodeNames)
            {
                targetId = getLayerIdFromGroupName(targetNodeName);
                connections.add(targetId);
                //network.put(Integer.valueOf(NETWORK_GRAPH_BASE_IDX + sourceId*(NUM_LAYERS + 1) + 1 + layerIdIdx), (float)targetId);

                layerIdIdx++;
            }


        }


        // Defining the network layer sizes array

        network.put(INPUT_NODE_COUNT_IDX, (float)numInputNodes);
        network.put(OUTPUT_NODE_COUNT_IDX, (float)numOutputNodes);
        network.put(NUM_CELL_STATES_IDX, (float)numMemoryCellStates);



        HashMap<Integer, Integer> layerNodeCountMap = new HashMap<Integer, Integer>();
        layerNodeCountMap.put(Integer.valueOf(INPUT_LAYER_ID), numInputNodes);
        layerNodeCountMap.put(Integer.valueOf(OUTPUT_LAYER_ID), numOutputNodes);
        layerNodeCountMap.put(Integer.valueOf(CELL_INPUT_LAYER_ID), numMemoryCellStates);
        layerNodeCountMap.put(Integer.valueOf(CELL_OUTPUT_LAYER_ID), numMemoryCellStates);
        layerNodeCountMap.put(Integer.valueOf(FORGET_LAYER_ID), numMemoryCellStates);
        layerNodeCountMap.put(Integer.valueOf(INPUT_GATE_LAYER_ID), numMemoryCellStates);
        layerNodeCountMap.put(Integer.valueOf(OUTPUT_GATE_LAYER_ID), numMemoryCellStates);
        layerNodeCountMap.put(Integer.valueOf(PEEPHOLE_LAYER_ID), numMemoryCellStates);
        // layer dimen map

        layerIdIdx = 0;
        network.put(LAYER_DIMEN_MAP_IDX + layerIdIdx++, (float)numInputNodes);
        network.put(LAYER_DIMEN_MAP_IDX + layerIdIdx++, (float)numOutputNodes);
        for (int z = 2; z < NUM_LAYERS; z++)
        {
            network.put(LAYER_DIMEN_MAP_IDX + z, (float)numMemoryCellStates);
        }



        // Layer node state data
        int I_length = Math.max(1, numInputNodes);
        int O_length = Math.max(1, numOutputNodes);
        int P_length = Math.max(1, numMemoryCellStates);

        layerIdIdx = 0;
        int input_layer_net_input_idx = LAYER_STATE_BASE_IDX;
        int input_layer_activation_idx = input_layer_net_input_idx +  I_length;
        int input_layer_partial_net_input_idx =  input_layer_activation_idx + I_length;
        int input_layer_error_responsibility_idx = input_layer_partial_net_input_idx + I_length;
        network.put(LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerIdIdx++ , (float)input_layer_net_input_idx);


        int output_layer_net_input_idx = input_layer_error_responsibility_idx + I_length;
        int output_layer_activation_idx = output_layer_net_input_idx + O_length;
        int output_layer_partial_net_input_idx =  output_layer_activation_idx + O_length;
        int output_layer_error_responsibility_idx = output_layer_partial_net_input_idx + O_length;
        int output_layer_error_mask_idx = output_layer_error_responsibility_idx + O_length;
        network.put(LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerIdIdx++ , (float)output_layer_net_input_idx);
        for (int i=0;i < O_length;i++)
            network.put(output_layer_error_mask_idx + i, 1F);

        int cell_input_net_input_idx = output_layer_error_mask_idx + O_length;
        int cell_input_activation_idx = cell_input_net_input_idx + P_length;
        int cell_input_partial_net_input_idx =  cell_input_activation_idx + P_length;
        int cell_input_error_responsibility_idx = cell_input_partial_net_input_idx + P_length;
        network.put(LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerIdIdx++ , (float)cell_input_net_input_idx);


        int cell_output_net_input_idx = cell_input_error_responsibility_idx + P_length;
        int cell_output_activation_idx = cell_output_net_input_idx + P_length;
        int cell_output_partial_net_input_idx =  cell_output_activation_idx + P_length;
        int cell_output_error_responsibility_idx = cell_output_partial_net_input_idx + P_length;
        network.put(LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerIdIdx++ , (float)cell_output_net_input_idx);


        int forget_gate_net_input_idx = cell_output_error_responsibility_idx + P_length;
        int forget_gate_activation_idx = forget_gate_net_input_idx + P_length;
        int forget_gate_partial_net_input_idx =  forget_gate_activation_idx + P_length;
        int forget_gate_error_responsibility_idx = forget_gate_partial_net_input_idx + P_length;
        network.put(LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerIdIdx++ , (float)forget_gate_net_input_idx);


        int input_gate_net_input_idx = forget_gate_error_responsibility_idx + P_length;
        int input_gate_activation_idx = input_gate_net_input_idx  + P_length;
        int input_gate_partial_net_input_idx =  input_gate_activation_idx + P_length;
        int input_gate_error_responsibility_idx = input_gate_partial_net_input_idx + P_length;
        network.put(LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerIdIdx++ , (float)input_gate_net_input_idx);


        int output_gate_net_input_idx = input_gate_error_responsibility_idx + P_length;
        int output_gate_activation_idx = output_gate_net_input_idx + P_length;
        int output_gate_partial_net_input_idx =  output_gate_activation_idx + P_length;
        int output_gate_error_responsibility_idx = output_gate_partial_net_input_idx + P_length;
        network.put(LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerIdIdx++ , (float)output_gate_net_input_idx);


        int memory_cell_activation_idx = output_gate_error_responsibility_idx + P_length;
        int memory_cell_net_input_idx = memory_cell_activation_idx + P_length;
        network.put(LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerIdIdx++ , (float)memory_cell_activation_idx);



        // Define the layer data idx map
        HashMap<Integer, Integer> layerNodeDataIndexMap = new HashMap<Integer, Integer>();
        for (int i = 0;i < NUM_LAYERS;i++)
        {
            layerNodeDataIndexMap.put(Integer.valueOf(i), network.get(LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + i).intValue());
        }



        // ********************************************
        // START: Parameters to be passed into main neural network functions
        // ********************************************

        // feedforward and backpropagation data
        int backprop_link_count_idx;
        int feedforward_link_count_idx;
        feedforward_link_count_idx = memory_cell_net_input_idx + P_length; // ** pass to function **
        network.put(feedforward_link_count_idx, (float)feedforwardLinkOrder.length);
        network.put(FORWARD_PASS_LINK_ORDER_DATA_IDX, (float)feedforward_link_count_idx);


        int feedforward_link_data_idx = feedforward_link_count_idx + 1;
        int forwardOrderIndex = feedforward_link_data_idx;
        for (String linkSpec:feedforwardLinkOrder)
        {
            Pair<Integer, Integer> link = getSourceTargerLayerIdsFromLinkSpec(linkSpec);

            if (link.getLeft()>=0)
            {
                network.put(forwardOrderIndex, (float)getLinkId(link));

            }
            else
                network.put(forwardOrderIndex, (float) (-1* link.getRight()) ); // a negative sign indicates the layer id to commit the partial net inputs

            forwardOrderIndex++;
        }

        int feedforward_data_width = Math.max(1, feedforwardLinkOrder.length);
        backprop_link_count_idx = feedforward_link_data_idx + feedforward_data_width; // *** pass to function **
        int backprop_link_data_idx = backprop_link_count_idx + 1;
        network.put(backprop_link_count_idx, (float)weightUpdateLinkOrder.length);
        network.put(BACKWARD_PASS_LINK_ORDER_DATA_IDX, (float)backprop_link_count_idx);

        int updateOrderIndex = backprop_link_data_idx;
        for (String linkSpec:weightUpdateLinkOrder)
        {
            Pair<Integer, Integer> link = getSourceTargerLayerIdsFromLinkSpec(linkSpec);


            network.put(updateOrderIndex, (float)getLinkId(link));
            updateOrderIndex++;
        }

        int weightUpdateDataWidth = Math.max(1, weightUpdateLinkOrder.length);

        // set the link data
        int nextIndex = backprop_link_data_idx + weightUpdateDataWidth;
        HashMap<Integer, Integer> linkIndexToArrayIndexMap = new HashMap<Integer, Integer>();
        for(int sourceLayerId = 0; sourceLayerId < NUM_LAYERS; sourceLayerId++)
        {
            connections = graphMap.get(Integer.valueOf(sourceLayerId));
            if (connections != null)
            {

                for (Integer targetLayerId: connections)
                {
                    nextIndex = packLinkData(network, linkIndexToArrayIndexMap, layerNodeCountMap, nextIndex, sourceLayerId, targetLayerId);
                }

            }
        }

        // write the graph to weight map

        int graphData_idx = nextIndex;
        int dataLength = graphData_idx;
        network.put(CUSTOM_VARIABLE_DATA_IDX, Float.valueOf(nextIndex));

        for (int linkId = 0; linkId < (NUM_LAYERS * NUM_LAYERS); linkId++)
        {
            if (linkIndexToArrayIndexMap.containsKey(Integer.valueOf(linkId)))
            {
                network.put(LINK_ID_TO_DATA_IDX_IDX + linkId, (float)linkIndexToArrayIndexMap.get(Integer.valueOf(linkId)));

            }
            else
            {
                network.put(LINK_ID_TO_DATA_IDX_IDX + linkId, (float)-1);

            }

        }

        _networkData = new float[dataLength];
        for (Integer index:network.keySet())
        {
            _networkData[index.intValue()] = network.get(index).floatValue();
        }
        _networkData[NETWORK_LENGTH_IDX] = dataLength;
    }

    protected void initialize()
    {
        super.initialize();
        setRawData();
    }

    private int getLayerIdFromGroup(NodeGroup group)
    {
        if (group instanceof MemoryCellNodeLayer)
        {
            String[] parts = StringUtils.splitByWholeSeparatorPreserveAllTokens(((MemoryCellNodeLayer)group).getCellKey("M"), MemoryCell.NODE_GROUP_SEPARATOR);
            return layerNameShortToLayerIdMap.get(parts[1]);
        }
        else if (group instanceof  InputLayer)
        {
            return layerNameShortToLayerIdMap.get("I");
        }
        else if (group instanceof  OutputLayer)
        {
            return layerNameShortToLayerIdMap.get("O");
        }
        else
            return -1;
    }

    private int getLayerIdFromGroupName(String name)
    {
        if (name.indexOf(MemoryCell.NODE_GROUP_SEPARATOR) > -1)
        {
            String[] parts = StringUtils.splitByWholeSeparatorPreserveAllTokens(name, MemoryCell.NODE_GROUP_SEPARATOR);
            return layerNameShortToLayerIdMap.get(parts[1]);
        }
        else
        {
            return layerNameShortToLayerIdMap.get(name);
        }

    }



    private int packLinkData(HashMap<Integer, Float> network, HashMap<Integer, Integer> linkIndexToArrayIndexMap , HashMap<Integer, Integer> layerNodeCountMap, int offset, int sourceLayerId, int targetLayerId)
    {
        linkIndexToArrayIndexMap.put(getLinkId(sourceLayerId, targetLayerId), offset);
        int matrixWidth = layerNodeCountMap.get(sourceLayerId) * layerNodeCountMap.get(targetLayerId);
        int source_nodestate_idx = offset + LINK_DATA_SOURCE_LAYER_ID_IDX;
        int target_nodestate_idex = offset + LINK_DATA_TARGET_LAYER_ID_IDX;
        network.put(source_nodestate_idx, (float)sourceLayerId);
        network.put(target_nodestate_idex, (float)targetLayerId);
        //_networkData[source_nodestate_idx] = layerNodeDataIndexMap.get(sourceLayerId);
        //_networkData[target_nodestate_idex] = layerNodeDataIndexMap.get(targetLayerId);
        int weightMatrix_idx = offset + LINK_DATA_WEIGHT_MATRIX_IDX;
        int weightMatrix_delta_idx = weightMatrix_idx + matrixWidth;
        int eligibility_trace_matrix_idx = weightMatrix_delta_idx  + matrixWidth;
        int calculatedGradientMatrix_idx = eligibility_trace_matrix_idx  + matrixWidth;
        int prevCalculatedGradientMatrix_idx = calculatedGradientMatrix_idx + matrixWidth;
        return prevCalculatedGradientMatrix_idx + matrixWidth;
    }


    Pair<Integer, Integer> getSourceTargerLayerIdsFromLinkSpec(String spec)
    {
        String[] parts = StringUtils.splitByWholeSeparatorPreserveAllTokens(spec, ":");
        Integer targetLayerId = getLayerIdFromGroupName(parts[1]);
        Integer sourceLayerId = -1;
        if (!"*".equals(parts[0]))
        {
            sourceLayerId = getLayerIdFromGroupName(parts[0]);
        }
        return Pair.of(sourceLayerId, targetLayerId);
    }


    // ported to c
    private int getLinkId(Pair<Integer, Integer> linkPair)
    {
        return getLinkId(linkPair.getLeft(), linkPair.getRight());
    }



    // --<(==)>-- --<(==)>-- --<(==)>-- --<(==)>-- --<(==)>-- --<(==)>-- --<(==)>--
    //                  Main Neural Network Methods
    // --<(==)>-- --<(==)>-- --<(==)>-- --<(==)>-- --<(==)>-- --<(==)>-- --<(==)>--


    // ported to c
    private static int getLinkId(int sourceId, int targetId)
    {
        return sourceId* NUM_LAYERS + targetId;
    }


    // ported to c
    static int getLayerActivationFunctionId(float[] networkSpec, int layerId)
    {
        return (int)networkSpec[LAYER_ACTIVATION_FUNCTION_ID_IDX + layerId];
    }

    // ported to c
    static UnaryVectorOperator getLayerActivationFunction(float[] networkSpec, int layerId)
    {

        return  ACTIVATION_FUNCTION_MAP[getLayerActivationFunctionId(networkSpec, layerId)];
    }

    // ported to c
    static UnaryVectorOperator getLayerActivationFunctionDerivative(float[] networkSpec, int layerId)
    {

        return  ACTIVATION_FUNCTION_DERIVATIVE_MAP[getLayerActivationFunctionId(networkSpec, layerId)];
    }


    static int getLayerTargetLinksIndex(int layerId)
    {
        return NETWORK_GRAPH_BASE_IDX + (NUM_LAYERS + 1)*layerId;
    }

    // ported to c
    static  int getLayerActivationIndex(float[] networkSpec, int layerId)
    {
        int layer_width = getLayerWidth(networkSpec, layerId);
        int layer_state_idx = getLayerStateIndex(networkSpec, layerId);
        if (layerId == PEEPHOLE_LAYER_ID)
            return layer_state_idx;
        else
            return layer_state_idx + layer_width;
    }

    // ported to c
    static int getLayerNetInputIndex(float[] networkSpec, int layerId)
    {
        int layer_width = getLayerWidth(networkSpec, layerId);
        int layer_state_idx = getLayerStateIndex(networkSpec, layerId);
        if (layerId == PEEPHOLE_LAYER_ID)
            return layer_state_idx + layer_width;
        else
            return layer_state_idx;
    }

    // ported to c
    static int getLayerPartialNetInputIndex(float[] networkSpec, int layerId)
    {
        int layer_width = getLayerWidth(networkSpec, layerId);
        int layer_state_idx = getLayerStateIndex(networkSpec, layerId);
        if (layerId == PEEPHOLE_LAYER_ID)
            return -1;
        else
            return layer_state_idx + 2 * layer_width;
    }

    // ported to c
    static int getLayerErrorResponsibilityIndex(float[] networkSpec, int layerId)
    {
        int layer_width = getLayerWidth(networkSpec, layerId);
        int layer_state_idx = getLayerStateIndex(networkSpec, layerId);
        if (layerId == PEEPHOLE_LAYER_ID)
            return -1;
        else
            return layer_state_idx + 3 * layer_width;
    }

    // ported to c
    static int getOutputLayerMaskIndex(float[] networkSpec)
    {
        int layer_width = getLayerWidth(networkSpec, OUTPUT_LAYER_ID);
        int layer_state_idx = getLayerStateIndex(networkSpec, OUTPUT_LAYER_ID);
        return layer_state_idx + 4 * layer_width;
    }


    // ported to c
    static int getLinkSourceLayerIdIdx(int data_idx)
    {
        return data_idx + LINK_DATA_SOURCE_LAYER_ID_IDX;
    }

    // ported to c
    static int getLinkTargetLayerIdIdx(int data_idx)
    {
        return data_idx + LINK_DATA_TARGET_LAYER_ID_IDX;
    }

    static boolean isLayerConnectedToOutputLayer(float[] networkSpec, int layerId)
    {

        return networkSpec[LINK_ID_TO_DATA_IDX_IDX + getLinkId(layerId, OUTPUT_LAYER_ID)] > -1;
    }

    static boolean isLinkConnectedToOutputLayer(float[] networkSpec, int linkId)
    {

        return linkId % NUM_LAYERS == OUTPUT_LAYER_ID;
    }

    // ported to c
    static int getLinkWeightIndex(int rows, int cols, int data_idx)
    {
        return data_idx + LINK_DATA_WEIGHT_MATRIX_IDX;
    }

    // ported to c
    static int getLinkWeightDeltaIndex(int rows, int cols, int data_idx)
    {
        return data_idx + LINK_DATA_WEIGHT_MATRIX_IDX + rows*cols;
    }

    // ported to c
    static int getLinkElligibilityTraceIndex(int rows, int cols, int data_idx)
    {
        return data_idx + LINK_DATA_WEIGHT_MATRIX_IDX + 2 * rows*cols;
    }

    // ported to c
    static int getLinkCalculatedGradientIndex(int rows, int cols, int data_idx)
    {
        return data_idx + LINK_DATA_WEIGHT_MATRIX_IDX + 3 * rows*cols;
    }

    // ported to c
    static int getLinkPrevCalculatedGradientIndex(int rows, int cols, int data_idx)
    {
        return data_idx + LINK_DATA_WEIGHT_MATRIX_IDX + 4 * rows*cols;
    }

    // ported to c *
    static int getLinkSourceLayerId(float[] networkSpec, int linkId)
    {
        int linkDataIdx = getLinkDataIndex(networkSpec, linkId);
        return (int)networkSpec[linkDataIdx + LINK_DATA_SOURCE_LAYER_ID_IDX];
    }

    // ported to c
    static int getLinkSourceLayerWidth(float[] networkSpec, int linkId)
    {
        int linkDataIdx = getLinkDataIndex(networkSpec, linkId);
        int layerId = (int)networkSpec[linkDataIdx + LINK_DATA_SOURCE_LAYER_ID_IDX];
        return getLayerWidth(networkSpec, layerId);
    }


    // ported to c
    static int getLinkTargetLayerId(float[] networkSpec, int linkId)
    {
        int linkDataIdx = getLinkDataIndex(networkSpec, linkId);
        return (int)networkSpec[linkDataIdx + LINK_DATA_TARGET_LAYER_ID_IDX];
    }

    // ported to c
    static int getLinkTargetLayerWidth(float[] networkSpec, int linkId)
    {
        int linkDataIdx = getLinkDataIndex(networkSpec, linkId);
        int layerId = (int)networkSpec[linkDataIdx + LINK_DATA_TARGET_LAYER_ID_IDX];
        return getLayerWidth(networkSpec, layerId);
    }


    // ported to c
    static int getLayerWidth(float[] networkSpec, int layerId)
    {
        return (int)networkSpec[LAYER_DIMEN_MAP_IDX + layerId];
    }

    // ported to c
    static int getLayerStateIndex(float[] networkSpec, int layerId)
    {
        return (int)networkSpec[LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerId];
    }

    // ported to c
    static int getLinkDataIndex(float[] networkSpec, int linkId)
    {
        if (linkId >= 0)
        {
            return (int)networkSpec[LINK_ID_TO_DATA_IDX_IDX + linkId];
        }
        else // Throw exception?
            return -1;

    }



    // adapted from Park and Miller as per Numerical Recipes in C for portability
    static long currentSeed = 1;

    // Park and Miller constants
    static final long IA = 16807;
    static final long IM = 2147483647L;
    static final double AM = 1.0/IM;
    static final long IQ = 127773;
    static final long IR = 2836;
    static final long MASK = 123459876;

    // Java derivative
    static final long LCG_M = 1L << 48;
    static final long LCG_A = 25214903917L;
    static final long LCG_C = 11L;
    static final long LCG_DIVIDER = 1L << 32;


    public enum RNG_ALGORITHM
    {
        JAVA_DERIVATIVE, PARK_AND_MILLER, MERSENNE_TWISTER, PLATFORM_DEFAULT
    }


    static RNG_ALGORITHM RNG_METHOD = RNG_ALGORITHM.JAVA_DERIVATIVE;

    public static void setRNDAlgorithm(RNG_ALGORITHM alg)
    {
        RNG_METHOD = alg;
    }


    public static double javaRNG()
    {
        long nextSeed = ((LCG_A*currentSeed + LCG_C) % LCG_M);
        setSeed(nextSeed);
        long ranLong = nextSeed >> 17;
        // have to add 0.5 since nextSeed can overflow to negative numbers and randLong will be between -2^31 to 2^31 - 1
        return  0.5 + 1.0*ranLong/LCG_DIVIDER;
    }


    public static double parkMillerRNG()
    {
        double ans;
        currentSeed^=MASK;
        long k = currentSeed/IQ;
        currentSeed = IA * (currentSeed - k * IQ) - IR * k;
        if (currentSeed < 0)
            currentSeed+=IM;
        ans = AM * currentSeed;
        currentSeed^=MASK;

        return ans;
    }

    public static double randomLCG()
    {
        switch (RNG_METHOD)
        {
            case PARK_AND_MILLER:
            {
                return parkMillerRNG();
            }
            case JAVA_DERIVATIVE:
            {
                return javaRNG();
            }
            case PLATFORM_DEFAULT:
            {
                return Math.random();
            }
            case MERSENNE_TWISTER:
            default:
                throw new RuntimeException("Unsupported random number generator algorithm");
        }

    }




    // ported to c
    public static void setSeed(long seed)
    {

        currentSeed =  seed;
    }


    // TODO modify this to use v1 and v2 instead of just v2
    // ported to c
    public static double randomGaussian()
    {
        float fac, rsq, v1, v2;

        do
        {
            v1 = (float)(2*randomLCG() - 1);
            v2 = (float)(2*randomLCG() - 1);
            rsq = v1*v1 + v2*v2;

        }while (rsq >=1 || rsq == 0);
        fac = (float)Math.sqrt(-2*Math.log(rsq)/rsq);
        return v2*fac;
    }

    // ported to c
    public static int probabilisticSample(float[] weights, float weightSum, boolean failureOnAllZeroP)
    {
        float cutPoint = (float)randomLCG()*weightSum;

        for (int i = 0; i < weights.length;i++)
        {
            if (weights[i] > cutPoint)
                return i;
            else
            {
                cutPoint -= weights[i];

            }
        }
        if (failureOnAllZeroP)
            return -1;
        else
            return (int)(randomLCG()*weights.length);
    }


    // ported to c
    /**
     * Negative values will be treated as having 0 weight
     * @param networkSpec
     * @param vectorOffset
     * @param length
     * @param minValue
     * @param failureOnAllZeroP
     * @return
     */
    public static int sampleVectorIndexProportionally(float[] networkSpec, int vectorOffset, int length, float minValue, boolean failureOnAllZeroP)
    {
        float weightSum = 0;
        for (int i = 0; i < length;i++)
        {
            if (networkSpec[vectorOffset + i] >= minValue)
                weightSum+=networkSpec[vectorOffset + i];

        }

        float cutPoint = (float)randomLCG()*weightSum;

        for (int i=0;i < length;i++)
        {
            if (networkSpec[vectorOffset + i] < minValue)
                continue;
            if (networkSpec[vectorOffset + i] > cutPoint)
            {
                return i;
            }
            else
            {
                cutPoint-=networkSpec[vectorOffset + i];
            }
        }


        if (failureOnAllZeroP)
            return -1;
        else
            return (int)(randomLCG()*length);
    }



    // ported to c
    // + reviewed +
    /**
     * Make sure this is called AFTER the output gate activation and memory cell state have been
     * updated for this forward pass
     * @param networkSpec
     */
    static void updateCellOutputState(float[] networkSpec)
    {
        // calculate cell activation, then update the celloutput activation, multiplying by the input gate
        int outputGateActivationIdx = getLayerActivationIndex(networkSpec, OUTPUT_GATE_LAYER_ID);

        int peepholeActivationIdx = getLayerActivationIndex(networkSpec, PEEPHOLE_LAYER_ID);
        int cellOutputWidth = getLayerWidth(networkSpec, CELL_OUTPUT_LAYER_ID);
        int cellOutputActivationIdx = getLayerActivationIndex(networkSpec, CELL_OUTPUT_LAYER_ID);


        for (int i = 0;i < cellOutputWidth;i++)
        {
            networkSpec[cellOutputActivationIdx + i] = networkSpec[outputGateActivationIdx + i]*networkSpec[peepholeActivationIdx + i];
        }
    }

    // ported to c
    // + reviewed +
    /**
     * This should be called after the cell input activation and input gate activation have been
     * calculated.  Ensure that your feedforward order respects this!
     * @param networkSpec
     */
    static void updateMemoryCellState(float[] networkSpec)
    {

        int peephole_net_input_idx = getLayerNetInputIndex(networkSpec, PEEPHOLE_LAYER_ID);
        int forget_gate_activation_idx = getLayerActivationIndex(networkSpec, FORGET_LAYER_ID);
        int cell_input_activation_idx = getLayerActivationIndex(networkSpec, CELL_INPUT_LAYER_ID);
        int input_gate_activation_idx = getLayerActivationIndex(networkSpec, INPUT_GATE_LAYER_ID);
        int peephole_layer_width = getLayerWidth(networkSpec, PEEPHOLE_LAYER_ID);
        int peepholeActivationIdx = getLayerActivationIndex(networkSpec, PEEPHOLE_LAYER_ID);
        UnaryVectorOperator activation = getLayerActivationFunction(networkSpec, PEEPHOLE_LAYER_ID);
        for (int i = 0;i < peephole_layer_width;i++)
        {

            networkSpec[peephole_net_input_idx + i] =
                    networkSpec[peephole_net_input_idx + i] * networkSpec[forget_gate_activation_idx + i] +
                    networkSpec[cell_input_activation_idx + i]*networkSpec[input_gate_activation_idx + i];

            // precalculating the cell state (peephole net activation)
            networkSpec[peepholeActivationIdx + i] = activation.operate (networkSpec, peephole_net_input_idx, peephole_layer_width, i);
        }
    }



    // ported to c
    // + reviewed +
    static void feedforward(float[] networkSpec, int linkId)
    {
        int sourceLayerId = getLinkSourceLayerId(networkSpec, linkId);
        int targetLayerId = getLinkTargetLayerId(networkSpec, linkId);



        int link_data_idx = getLinkDataIndex(networkSpec, linkId);

        int sourceLayerWidth = getLayerWidth(networkSpec, sourceLayerId);
        int targetLayerWidth = getLayerWidth(networkSpec, targetLayerId);
        int linkWeightMatrixIdx = getLinkWeightIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);
        int sourceNodeStateActivation_idx = getLayerActivationIndex(networkSpec, sourceLayerId);
        int targetPartialNetInput_idx = getLayerPartialNetInputIndex(networkSpec, targetLayerId);


        //System.out.println(String.format("Feedforward: sourceLayerId - [%1$s] targetLayerId - [%2$s] link_data_idx - [%3$s] sourceLayerWidth - [%4$s], targetLayerWidth - [%5$s], linkWeightMatrixIdx - [%6$s], sourceNodeStateActivation_idx - [%7$s], targetPartialNetInput_idx - [%8$s]", sourceLayerId, targetLayerId, link_data_idx, sourceLayerWidth, targetLayerWidth, linkWeightMatrixIdx, sourceNodeStateActivation_idx, targetPartialNetInput_idx));
        matrixMultiplyByVector(targetLayerWidth, sourceLayerWidth, networkSpec, linkWeightMatrixIdx, sourceNodeStateActivation_idx, targetPartialNetInput_idx, true);


    }

    


    // .:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:.
    //          Backward pass functions
    // .:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:.



    // ported to c
    // + provisionally reviewed +
    static float getOutputLinkErrorPartialDerivative(float[] networkSpec, int sourceNodeIndex)
    {

        return networkSpec[getLayerErrorResponsibilityIndex(networkSpec, CELL_OUTPUT_LAYER_ID) + sourceNodeIndex];
    }

    // ported to c
    // + reviewed +
    static void setErrorResponsibility(float[] networkSpec, int layerId, float[] targetOutput)
    {
        int layer_width = getLayerWidth(networkSpec, layerId);

        int layer_error_responsibility_idx = getLayerErrorResponsibilityIndex(networkSpec, layerId);
        int memory_cell_state_idx = getLayerNetInputIndex(networkSpec, PEEPHOLE_LAYER_ID);
        int memory_cell_state_squashed_idx = getLayerActivationFunctionId(networkSpec, PEEPHOLE_LAYER_ID);
        int net_input_idx = getLayerNetInputIndex(networkSpec, layerId);
        UnaryVectorOperator activationSquashedDerivative = getLayerActivationFunctionDerivative(networkSpec, layerId);

        float outputError;
        switch (layerId)
        {
            case OUTPUT_LAYER_ID: //
                OutputErrorFunction errorFunction = ERROR_FUNCTION_MAP[(int)networkSpec[OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX]];

                UnaryVectorOperator outputActivationPrime = getLayerActivationFunctionDerivative(networkSpec, OUTPUT_LAYER_ID);

                for (int i = 0;i < layer_width;i++)
                {
                    networkSpec[layer_error_responsibility_idx + i] =
                            (float)(errorFunction.errorPartialDerivative(networkSpec, targetOutput, i)*outputActivationPrime.operate(networkSpec, net_input_idx, layer_width, i));
                }
                setErrorResponsibility(networkSpec, CELL_OUTPUT_LAYER_ID, targetOutput);
                break;
            case INPUT_GATE_LAYER_ID: //
            case CELL_INPUT_LAYER_ID:
            case FORGET_LAYER_ID:
                int output_gate_activation_idx = getLayerActivationIndex(networkSpec, OUTPUT_GATE_LAYER_ID);
                UnaryVectorOperator memory_cell_squashed_derivative = getLayerActivationFunctionDerivative(networkSpec, PEEPHOLE_LAYER_ID);
                int memory_cell_width = getLayerWidth(networkSpec, PEEPHOLE_LAYER_ID);
                for (int i=0;i<layer_width;i++)
                {
                    outputError = getOutputLinkErrorPartialDerivative(networkSpec, i);
                    networkSpec[layer_error_responsibility_idx + i] =
                            networkSpec[output_gate_activation_idx + i]*memory_cell_squashed_derivative.operate(networkSpec, memory_cell_state_idx, memory_cell_width, i)*outputError;
                }
                break;
            case OUTPUT_GATE_LAYER_ID: //
                for (int i=0;i<layer_width;i++)
                {
                    outputError = getOutputLinkErrorPartialDerivative(networkSpec, i);
                    networkSpec[layer_error_responsibility_idx + i] = activationSquashedDerivative.operate(networkSpec, net_input_idx, layer_width,i) *
                            networkSpec[memory_cell_state_squashed_idx + i]*outputError;
                }
                break;
            case CELL_OUTPUT_LAYER_ID: //
                int output_layer_error_responsibility_idx = getLayerErrorResponsibilityIndex(networkSpec, OUTPUT_LAYER_ID);
                int output_layer_width = getLayerWidth(networkSpec, OUTPUT_LAYER_ID);
                int linkId = getLinkId(CELL_OUTPUT_LAYER_ID, OUTPUT_LAYER_ID);
                int output_link_data_idx = getLinkDataIndex(networkSpec, linkId);

                int output_link_weight_idx = getLinkWeightIndex(output_layer_width, layer_width, output_link_data_idx);
                float error = 0;
                int output_layer_mask_id = getOutputLayerMaskIndex(networkSpec);
                for (int j = 0; j < layer_width;j++)
                {
                    error = 0;
                    for (int k = 0; k < output_layer_width;k++)
                    {
                        error += networkSpec[output_layer_mask_id + k]*getMatrixValue(layer_width, networkSpec, output_link_weight_idx, k, j)*networkSpec[output_layer_error_responsibility_idx + k];
                    }
                    networkSpec[layer_error_responsibility_idx + j] = error;
                }
                break;
            default:

        }
    }

    // ported to c
    // + reviewed +
    static void updateElligibilityTrace(float[] networkSpec, int linkId)
    {
        int link_data_idx = getLinkDataIndex(networkSpec, linkId);
        int source_layer_id = (int)networkSpec[getLinkSourceLayerIdIdx(link_data_idx)];
        int target_layer_id = (int)networkSpec[getLinkTargetLayerIdIdx(link_data_idx)];

        int sourceLayerWidth = getLayerWidth(networkSpec, source_layer_id);
        int targetLayerWidth = getLayerWidth(networkSpec, target_layer_id);
        int source_activation_idx = getLayerActivationIndex(networkSpec, source_layer_id);
        int eligibilityTrace_idx = getLinkElligibilityTraceIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);
        int forget_gate_activation_idx = getLayerActivationIndex(networkSpec, FORGET_LAYER_ID);
        int target_net_input_idx = getLayerNetInputIndex(networkSpec, target_layer_id);
        float sourceActivation;
        float previousTraceCellValue, newTraceValue;

        int peephole_net_input_idx;

        switch (target_layer_id)
        {
            case OUTPUT_LAYER_ID:
            case OUTPUT_GATE_LAYER_ID: // + reviewed +
                for (int i = 0; i < targetLayerWidth; i++)
                {
                    for (int j = 0; j < sourceLayerWidth;j++)
                    {
                        sourceActivation = networkSpec[source_activation_idx + j];
                        setMatrixValue(sourceLayerWidth, networkSpec, eligibilityTrace_idx, i, j, sourceActivation);
                    }
                }

                break;
            case CELL_INPUT_LAYER_ID: // + reviewed +
                int input_gate_activation_idx = getLayerActivationIndex(networkSpec, INPUT_GATE_LAYER_ID);
                UnaryVectorOperator squashedCellInputDerivative = getLayerActivationFunctionDerivative(networkSpec, CELL_INPUT_LAYER_ID);
                for (int i = 0; i < targetLayerWidth; i++)
                {
                    for (int j = 0; j < sourceLayerWidth;j++)
                    {
                        sourceActivation = networkSpec[source_activation_idx + j];
                        // can set the squashing function of the cell input to the identity function to simplify targetNetInputSquashed to 1
                        float targetNetInputSquashed = squashedCellInputDerivative.operate(networkSpec, target_net_input_idx, targetLayerWidth, i);

                        previousTraceCellValue = getMatrixValue(sourceLayerWidth, networkSpec, eligibilityTrace_idx, i, j);

                        newTraceValue = previousTraceCellValue * getVectorValue(networkSpec, forget_gate_activation_idx, i) +
                                getVectorValue(networkSpec, input_gate_activation_idx, i)*targetNetInputSquashed*sourceActivation;

                        setMatrixValue(sourceLayerWidth, networkSpec, eligibilityTrace_idx, i, j, newTraceValue);
                    }
                }
                break;
            case INPUT_GATE_LAYER_ID: // + reviewed +
                UnaryVectorOperator targetActivationDerivative = getLayerActivationFunctionDerivative(networkSpec, target_layer_id);
                int cell_input_activation_idx = getLayerActivationIndex(networkSpec, CELL_INPUT_LAYER_ID);
                for (int i = 0; i < targetLayerWidth; i++)
                {
                    for (int j = 0; j < sourceLayerWidth;j++)
                    {
                        sourceActivation = networkSpec[source_activation_idx + j];

                        previousTraceCellValue = getMatrixValue(sourceLayerWidth, networkSpec, eligibilityTrace_idx, i, j);

                        newTraceValue = previousTraceCellValue * getVectorValue(networkSpec, forget_gate_activation_idx, i) +
                                getVectorValue(networkSpec, cell_input_activation_idx, i) * targetActivationDerivative.operate(networkSpec, target_net_input_idx, targetLayerWidth, i)*sourceActivation;

                        setMatrixValue(sourceLayerWidth, networkSpec, eligibilityTrace_idx, i, j, newTraceValue);
                    }
                }
                break;
            case FORGET_LAYER_ID: // + reviewed +
                peephole_net_input_idx = getLayerNetInputIndex(networkSpec, PEEPHOLE_LAYER_ID);

                UnaryVectorOperator forgetGetSquashedDerivative = getLayerActivationFunctionDerivative(networkSpec, FORGET_LAYER_ID);
                float forgetSquashedPrime;
                for (int i = 0; i < targetLayerWidth; i++)
                {
                    for (int j = 0; j < sourceLayerWidth;j++)
                    {
                        sourceActivation = networkSpec[source_activation_idx + j];

                        forgetSquashedPrime = forgetGetSquashedDerivative.operate(networkSpec, target_net_input_idx, targetLayerWidth, i);

                        previousTraceCellValue = getMatrixValue(sourceLayerWidth, networkSpec, eligibilityTrace_idx, i, j);

                        newTraceValue = previousTraceCellValue * getVectorValue(networkSpec, forget_gate_activation_idx, i) +
                                getVectorValue(networkSpec, peephole_net_input_idx, i)*forgetSquashedPrime*sourceActivation;

                        setMatrixValue(sourceLayerWidth, networkSpec, eligibilityTrace_idx, i, j, newTraceValue);
                    }
                }
                break;

        }
    }

    // + reviewed +
    static void updatePartialGradient(float[] networkSpec, int linkId, float[] targetOutput)
    {

        updateElligibilityTrace(networkSpec, linkId);

        int link_data_idx = getLinkDataIndex(networkSpec, linkId);
        int source_layer_id = (int)networkSpec[getLinkSourceLayerIdIdx(link_data_idx)];
        int target_layer_id = (int)networkSpec[getLinkTargetLayerIdIdx(link_data_idx)];
        int sourceLayerWidth = getLayerWidth(networkSpec, source_layer_id);
        int targetLayerWidth = getLayerWidth(networkSpec, target_layer_id);

        int calculated_gradient_idx = getLinkCalculatedGradientIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);
        int target_error_responsibility_idx = getLayerErrorResponsibilityIndex(networkSpec, target_layer_id);
        int eligibility_trace_idx = getLinkElligibilityTraceIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);
        float negativeDerivative;
        float priorCalculatedGradient;

        setErrorResponsibility(networkSpec, target_layer_id, targetOutput);

        for (int i = 0; i < targetLayerWidth;i++)
        {
            for (int j = 0; j < sourceLayerWidth;j++)
            {
                negativeDerivative = getMatrixValue(sourceLayerWidth, networkSpec, eligibility_trace_idx, i, j)*getVectorValue(networkSpec, target_error_responsibility_idx, i);
                priorCalculatedGradient = getMatrixValue(sourceLayerWidth, networkSpec, calculated_gradient_idx, i, j);
                setMatrixValue(sourceLayerWidth, networkSpec, calculated_gradient_idx, i, j, priorCalculatedGradient - negativeDerivative);
            }
        }
    }


    static float min(float x, float y)
    {
        return Math.min(x, y);
    }

    static float max(float x, float y)
    {
        return Math.max(x, y);
    }

    // ported to c
    static float sign(float value)
    {
        if (value > 0)
            return 1;
        else if (value == 0)
            return 0;
        else
            return -1;
    }

    // ported to c
    static void updateWeights(float[] networkSpec, int linkId,  LSTMNetwork.WeightUpdateType updateType)
    {
        float n_minus = getRROPN_Minus(networkSpec);
        float n_plus = getRROPN_Plus(networkSpec);
        float deltaMax = getRPROPMaxWeightUpdateDelta(networkSpec);
        float deltaMin = getRPROPMinWeightUpdateDelta(networkSpec);
        switch (updateType)
        {
            case RPROP:
            {
                int link_data_idx = getLinkDataIndex(networkSpec, linkId);
                int source_layer_id = (int)networkSpec[getLinkSourceLayerIdIdx(link_data_idx)];
                int target_layer_id = (int)networkSpec[getLinkTargetLayerIdIdx(link_data_idx)];
                int sourceLayerWidth = getLayerWidth(networkSpec, source_layer_id);
                int targetLayerWidth = getLayerWidth(networkSpec, target_layer_id);

                int calculated_gradient_idx = getLinkCalculatedGradientIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);
                int weight_matrix_idx = getLinkWeightIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);
                int weight_delta_matrix_idx = getLinkWeightDeltaIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);
                int prevCalculated_gradient_idx = getLinkPrevCalculatedGradientIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);

                float prevDelta, prevWeight, newWeight, prevCalculatedGradient, calculatedGradient, value, newDelta;
                for (int i = 0; i < targetLayerWidth;i++)
                {
                    for (int j = 0; j < sourceLayerWidth;j++)
                    {
                        prevDelta = getMatrixValue(sourceLayerWidth, networkSpec, weight_delta_matrix_idx, i, j);
                        prevCalculatedGradient = getMatrixValue(sourceLayerWidth, networkSpec, prevCalculated_gradient_idx, i, j);
                        calculatedGradient = getMatrixValue(sourceLayerWidth, networkSpec, calculated_gradient_idx, i, j);

                        if (prevCalculatedGradient*calculatedGradient > 0)
                            value = min(prevDelta*n_plus, deltaMax);
                        else if (prevCalculatedGradient*calculatedGradient < 0)
                        {
                            setMatrixValue(sourceLayerWidth, networkSpec, calculated_gradient_idx, i, j, 0);
                            value = max(prevDelta*n_minus, deltaMin);
                        }
                        else
                            value = prevDelta;

                        setMatrixValue(sourceLayerWidth, networkSpec, weight_delta_matrix_idx, i, j, value);
                        setMatrixValue(sourceLayerWidth, networkSpec, prevCalculated_gradient_idx, i, j, calculatedGradient);
                    }
                }


                for (int i = 0; i < targetLayerWidth;i++)
                {
                    for (int j = 0; j < sourceLayerWidth;j++)
                    {
                        prevWeight = getMatrixValue(sourceLayerWidth, networkSpec, weight_matrix_idx, i, j);
                        calculatedGradient = getMatrixValue(sourceLayerWidth, networkSpec, calculated_gradient_idx, i, j);
                        newDelta = getMatrixValue(sourceLayerWidth, networkSpec, weight_delta_matrix_idx, i, j);
                        newWeight = prevWeight - sign(calculatedGradient)*newDelta;

                        setMatrixValue(sourceLayerWidth, networkSpec, weight_matrix_idx, i, j, newWeight);
                        setMatrixValue(sourceLayerWidth, networkSpec, calculated_gradient_idx, i, j, 0); // clear calculated gradient
                    }
                }
            }
            break;
            case DEFAULT:
            {
                int link_data_idx = getLinkDataIndex(networkSpec, linkId);
                int source_layer_id = (int)networkSpec[getLinkSourceLayerIdIdx(link_data_idx)];
                int target_layer_id = (int)networkSpec[getLinkTargetLayerIdIdx(link_data_idx)];
                int sourceLayerWidth = getLayerWidth(networkSpec, source_layer_id);
                int targetLayerWidth = getLayerWidth(networkSpec, target_layer_id);

                int calculated_gradient_idx = getLinkCalculatedGradientIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);
                int weight_matrix_idx = getLinkWeightIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);

                float learningRate = getLearningRate(networkSpec);
                for (int i = 0; i < targetLayerWidth;i++)
                {
                    for (int j = 0;j<sourceLayerWidth;j++)
                    {
                        float gradient = getMatrixValue(sourceLayerWidth, networkSpec, calculated_gradient_idx, i, j);
                        float prevWeight = getMatrixValue(sourceLayerWidth, networkSpec, weight_matrix_idx, i, j);
                        // doing gradient descent so we move in the direction of - ∂E/∂W_i_j
                        setMatrixValue(sourceLayerWidth, networkSpec, weight_matrix_idx, i, j, prevWeight - learningRate*gradient);
                        setMatrixValue(sourceLayerWidth, networkSpec, calculated_gradient_idx, i, j, 0); // clear calculated gradient
                    }
                }

            }

        }

    }


    // ported to c
    // + reviewed +
    static void pushNetInput(float[] networkSpec, int layerId)
    {
        int layer_activation_idx = getLayerActivationIndex(networkSpec, layerId);
        int layer_partial_net_idx = getLayerPartialNetInputIndex(networkSpec, layerId);
        int layer_net_input_idx = getLayerNetInputIndex(networkSpec, layerId);

        int layerWidth = getLayerWidth(networkSpec, layerId);

        //System.out.println(String.format("PushNetInput: layerId - [%5$s] layer_activation_idx - [%1$s] layer_partial_net_idx - [%2$s] layer_net_input_idx - [%3$s] layerWidth - [%4$s]", layer_activation_idx, layer_partial_net_idx, layer_net_input_idx, layerWidth, layerId));

        UnaryVectorOperator activationFunction = getLayerActivationFunction(networkSpec, layerId);
        switch (layerId)
        {
            case PEEPHOLE_LAYER_ID: // do nothing, peephole can't be the target of external links
                break;
            case OUTPUT_GATE_LAYER_ID:
                vectorCopy(layerWidth, networkSpec, layer_partial_net_idx, layer_net_input_idx);
                vectorElementwiseSet(layerWidth, networkSpec, layer_partial_net_idx, 0);
                vectorElementwiseMap(layerWidth, networkSpec, layer_net_input_idx, layer_activation_idx, activationFunction);
                updateCellOutputState(networkSpec);
                break;
            case INPUT_GATE_LAYER_ID:
                vectorCopy(layerWidth, networkSpec, layer_partial_net_idx, layer_net_input_idx);
                vectorElementwiseSet(layerWidth, networkSpec, layer_partial_net_idx, 0);
                vectorElementwiseMap(layerWidth, networkSpec, layer_net_input_idx, layer_activation_idx, activationFunction);
                updateMemoryCellState(networkSpec);
                break;
            default:
                vectorCopy(layerWidth, networkSpec, layer_partial_net_idx, layer_net_input_idx);
                vectorElementwiseSet(layerWidth, networkSpec, layer_partial_net_idx, 0);
                vectorElementwiseMap(layerWidth, networkSpec, layer_net_input_idx, layer_activation_idx, activationFunction);
        }
    }





    // ported to c
    // + reviewed +  - Reviewed for correctness, needs to be reviewed for optimization
    static void resetLayerNodeState(float[] networkSpec, int layerId)
    {
        int layer_activation_idx = getLayerActivationIndex(networkSpec, layerId);
        int layer_partial_net_idx = getLayerPartialNetInputIndex(networkSpec, layerId);

        int layerWidth = getLayerWidth(networkSpec, layerId);

        switch (layerId)
        {
            // initialize all gates as open initially.
            case INPUT_GATE_LAYER_ID:
            case FORGET_LAYER_ID:
            case OUTPUT_GATE_LAYER_ID:
                vectorElementwiseSet(layerWidth, networkSpec, layer_partial_net_idx, 0);
                vectorElementwiseSet(layerWidth, networkSpec, layer_activation_idx, 1);
                break;
            case CELL_OUTPUT_LAYER_ID:
            case CELL_INPUT_LAYER_ID:
                vectorElementwiseSet(layerWidth, networkSpec, layer_partial_net_idx, 0);
                pushNetInput(networkSpec, layerId);
                break;
            case PEEPHOLE_LAYER_ID:
                int net_input_idx = getLayerNetInputIndex(networkSpec, layerId);
                vectorElementwiseSet(layerWidth, networkSpec, net_input_idx, 0);
                UnaryVectorOperator function = getLayerActivationFunction(networkSpec, layerId);
                vectorElementwiseMap(layerWidth, networkSpec, net_input_idx, layer_activation_idx, function);
                break;
        }
    }

    // ported to c
    // + reviewed +  - Reviewed for correctness.  Definitely need to refactor so that peephole is only reset if it is
    //                 an explicit link source
    static void resetMemoryCellState(float[] networkSpec)
    {
        int[] layerIds = {CELL_INPUT_LAYER_ID, CELL_OUTPUT_LAYER_ID, PEEPHOLE_LAYER_ID, INPUT_GATE_LAYER_ID, OUTPUT_GATE_LAYER_ID};
        for (int i = 0;i < layerIds.length;i++)
        {
            resetLayerNodeState(networkSpec, layerIds[i]);
        }

    }


    // ported to c
    // + reviewed +
    static void clearLinkWeightHistory(float[] networkSpec, int linkId)
    {
        int linkDataIdx = getLinkDataIndex(networkSpec, linkId);
        int sourceLayerWidth = getLinkSourceLayerWidth(networkSpec, linkId);
        int targetLayerWidth = getLinkTargetLayerWidth(networkSpec, linkId);
        int elligibilityTraceIdx = getLinkElligibilityTraceIndex(targetLayerWidth, sourceLayerWidth, linkDataIdx);

        matrixElementwiseSet(targetLayerWidth, sourceLayerWidth, networkSpec, elligibilityTraceIdx, 0);
        // why not reset prevCalculated gradient and previous weight delta?
        // answer: because this happens when you update the weights in updateWeightsFromErrors

    }


    // ported to c
    // + reviewed +
    static void initializeLink(float[] networkSpec, int linkId, float initialDelta, float minWeight, float maxWeight)
    {
        int linkDataIdx = getLinkDataIndex(networkSpec, linkId);
        int sourceLayerWidth = getLinkSourceLayerWidth(networkSpec, linkId);
        int targetLayerWidth = getLinkTargetLayerWidth(networkSpec, linkId);

        int weight_idx = getLinkWeightIndex(targetLayerWidth, sourceLayerWidth, linkDataIdx);
        int weight_delta_idx = getLinkWeightDeltaIndex(targetLayerWidth, sourceLayerWidth, linkDataIdx);
        int calculated_gradient_idx = getLinkCalculatedGradientIndex(targetLayerWidth, sourceLayerWidth, linkDataIdx);
        int prev_calculated_gradient_idx = getLinkPrevCalculatedGradientIndex(targetLayerWidth, sourceLayerWidth, linkDataIdx);
        int elligibilityTraceIdx = getLinkElligibilityTraceIndex(targetLayerWidth, sourceLayerWidth, linkDataIdx);


        matrixElementwiseRandomize(targetLayerWidth, sourceLayerWidth, networkSpec, weight_idx, minWeight, maxWeight);
        matrixElementwiseSet(targetLayerWidth, sourceLayerWidth, networkSpec, elligibilityTraceIdx, 0);

        matrixElementwiseSet(targetLayerWidth, sourceLayerWidth, networkSpec, calculated_gradient_idx, 0);
        matrixElementwiseSet(targetLayerWidth, sourceLayerWidth, networkSpec, prev_calculated_gradient_idx, 0);
        matrixElementwiseSet(targetLayerWidth, sourceLayerWidth, networkSpec, weight_delta_idx, initialDelta);
    }

    static void initializeLink(float[] networkSpec, int linkId, float initialDelta, float minWeight, float maxWeight, float fraction)
    {
        int linkDataIdx = getLinkDataIndex(networkSpec, linkId);
        int sourceLayerWidth = getLinkSourceLayerWidth(networkSpec, linkId);
        int targetLayerWidth = getLinkTargetLayerWidth(networkSpec, linkId);

        int weight_idx = getLinkWeightIndex(targetLayerWidth, sourceLayerWidth, linkDataIdx);
        int weight_delta_idx = getLinkWeightDeltaIndex(targetLayerWidth, sourceLayerWidth, linkDataIdx);
        int calculated_gradient_idx = getLinkCalculatedGradientIndex(targetLayerWidth, sourceLayerWidth, linkDataIdx);
        int prev_calculated_gradient_idx = getLinkPrevCalculatedGradientIndex(targetLayerWidth, sourceLayerWidth, linkDataIdx);
        int elligibilityTraceIdx = getLinkElligibilityTraceIndex(targetLayerWidth, sourceLayerWidth, linkDataIdx);


        matrixRandomizeFraction(targetLayerWidth, sourceLayerWidth, networkSpec, weight_idx, minWeight, maxWeight, fraction);
        matrixElementwiseSet(targetLayerWidth, sourceLayerWidth, networkSpec, elligibilityTraceIdx, 0);

        matrixElementwiseSet(targetLayerWidth, sourceLayerWidth, networkSpec, calculated_gradient_idx, 0);
        matrixElementwiseSet(targetLayerWidth, sourceLayerWidth, networkSpec, prev_calculated_gradient_idx, 0);
        matrixElementwiseSet(targetLayerWidth, sourceLayerWidth, networkSpec, weight_delta_idx, initialDelta);
    }




    // ported to c
    /**
     * Reset the network node state such as when you are learning a pattern sequence from the
     * default context
     * @param networkSpec
     */
    static void initializeNodeState(float[] networkSpec)
    {
        resetMemoryCellState(networkSpec);
    }

    // ported to c
    /**
     * Call this at the beginning of learning a new pattern sequence batch
     * @param networkSpec

     */
    static void resetWeightHistory(float[] networkSpec)
    {
        int backprop_link_count_idx = getBackwardPassLinkIndex(networkSpec);
        int linkIdIdx = 0;
        int linkId;

        int numBackwardPassLinks = (int)networkSpec[backprop_link_count_idx];
        int update_gradient_link_order = backprop_link_count_idx + 1;
        for (linkIdIdx = 0; linkIdIdx < numBackwardPassLinks; linkIdIdx++ )
        {
            linkId = (int)networkSpec[update_gradient_link_order + linkIdIdx];
            clearLinkWeightHistory(networkSpec, linkId);
        }
    }


    // o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o
    //              Primary Public Helper Utility functions
    // o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o

    // ported to c
    public static int getNumCustomData(float[] networkSpec)
    {
        return NUM_CUSTOM_DATA_REGISTERS;
    }

    // ported to c
    public static int setCustomData(float[] networkSpec, float data, int index)
    {
        if (index >= getNumCustomData(networkSpec) || index < 0)
            return ERROR_STATUS_FAILURE;
        networkSpec[CUSTOM_DATA_1_IDX + index] = data;
        return ERROR_STATUS_SUCESS;
    }


    // ported to c
    public static float getCustomData(float[] networkSpec, int index, float errorValue)
    {
        if (index >= getNumCustomData(networkSpec) || index < 0)
            return errorValue;
        return networkSpec[CUSTOM_DATA_1_IDX + index];
    }

    // ported to c
    public static int getCustomVariableDataIndex(float[] networkSpec)
    {
        return (int)networkSpec[CUSTOM_VARIABLE_DATA_IDX];
    }

    // ported to c
    public static void setOutputErrorMask(float[] rawData, float[] mask)
    {
        for (int i = 0;i < mask.length;i++)
        {
            rawData[getOutputLayerMaskIndex(rawData) + i] = mask[i];
        }
    }

    // ported to c
    public static void clearOutputErrorMask(float[] rawData)
    {
        int width = getLayerWidth(rawData, OUTPUT_LAYER_ID);
        for (int i = 0;i < width;i++)
        {
            rawData[getOutputLayerMaskIndex(rawData) + i] = 1;
        }
    }

    public static float[] getNodeStateSnapshot(float[] networkSpec)
    {
        int feedforware_link_count_index = getForwardPassLinkIndex(networkSpec);
        int I_length = Math.max(1, (int)networkSpec[INPUT_NODE_COUNT_IDX]);
        int O_length = Math.max(1, (int)networkSpec[OUTPUT_NODE_COUNT_IDX]);
        int P_length = Math.max(1, (int)networkSpec[NUM_CELL_STATES_IDX]);


        int node_state_length = feedforware_link_count_index - LAYER_STATE_BASE_IDX;
        assert node_state_length == I_length*4 + O_length*4 + 22*P_length;

        float[] nodeStateSnapshot = new float[node_state_length];
        for (int i = 0;i<node_state_length;i++)
        {
            nodeStateSnapshot[i] = networkSpec[LAYER_STATE_BASE_IDX + i];
        }
        return nodeStateSnapshot;
    }


    public static void loadNodeStateFromSnapshot(float[] networkSpec,  float[] nodeStateSnapshot)
    {
        int feedforward_link_count_index = getForwardPassLinkIndex(networkSpec);
        int I_length = Math.max(1, (int)networkSpec[INPUT_NODE_COUNT_IDX]);
        int O_length = Math.max(1, (int)networkSpec[OUTPUT_NODE_COUNT_IDX]);
        int P_length = Math.max(1, (int)networkSpec[NUM_CELL_STATES_IDX]);


        int node_state_length = feedforward_link_count_index - LAYER_STATE_BASE_IDX;
        assert node_state_length == I_length*4 + O_length*5 + 22*P_length;
        for (int i = 0;i<node_state_length;i++)
        {
            networkSpec[LAYER_STATE_BASE_IDX + i] = nodeStateSnapshot[i];
        }
    }

    // ported to c
    /**
     * Copies the node state from one networkSpec to another that are the same node dimension
     * @param sourceNetworkSpec
     * @param targetNetworkSpec
     */
    public static void loadNodeStateFromOtherNetwork(float[] sourceNetworkSpec, float[] targetNetworkSpec)
    {
        int feedforward_link_count_index = getForwardPassLinkIndex(sourceNetworkSpec);
        for (int i = LAYER_STATE_BASE_IDX;i<feedforward_link_count_index;i++)
        {
            targetNetworkSpec[i] = sourceNetworkSpec[i];
        }
    }

    // ported to c
    // + reviewed +
    public static float getLearningRate(float[] networkSpec)
    {
        return networkSpec[LEARNING_RATE_IDX];
    }


    // ported to c
    public static void setLearningRate(float[] networkSpec, float learningRate)
    {
        networkSpec[LEARNING_RATE_IDX] = learningRate;
    }


    // ported to c
    public static float getRROPN_Plus(float[] networkSpec)
    {
        return networkSpec[N_PLUS_IDX];
    }

    // ported to c
    public static void setRROPN_Plux(float[] networkSpec, float n_plus)
    {
        networkSpec[N_PLUS_IDX] = n_plus;
    }

    // ported to c
    public static float getRROPN_Minus(float[] networkSpec)
    {
        return networkSpec[N_MINUS_IDX];
    }

    // ported to c
    public static void setRROPN_Minus(float[] networkSpec, float n_minus)
    {
        networkSpec[N_MINUS_IDX] = n_minus;
    }

    // ported to c
    public static float getRPROPMinWeightUpdateDelta(float[] networkSpec)
    {
        return networkSpec[MIN_WEIGHT_DELTA_IDX];
    }

    // ported to c
    public static void setRPROPMinWeightUpdateDelta(float[] networkSpec, float delta)
    {
        networkSpec[MIN_WEIGHT_DELTA_IDX] = delta;
    }

    // ported to c
    public static float getRPROPMaxWeightUpdateDelta(float[] networkSpec)
    {
        return networkSpec[MAX_WEIGHT_DELTA_IDX];
    }

    // ported to c
    public static void setRPROPMaxWeightUpdateDelta(float[] networkSpec, float delta)
    {
        networkSpec[MAX_WEIGHT_DELTA_IDX] = delta;
    }

    // ported to c
    public static float getRPROPInitialWeightDelta(float[] networkSpec)
    {
        return networkSpec[INITIAL_WEIGHT_DELTA_IDX];
    }

    // ported to c
    public static void setRPROPInitialWeightDelta(float[] networkSpec, float delta)
    {
        networkSpec[INITIAL_WEIGHT_DELTA_IDX] = delta;
    }

    // ported to c
    public static float getMinInitialWeight(float[] networkSpec)
    {
        return networkSpec[MIN_INITIAL_WEIGHT_IDX];
    }

    // ported to c
    public static void setMinInitialWeight(float[] networkSpec, float weight)
    {
        networkSpec[MIN_INITIAL_WEIGHT_IDX] = weight;
    }

    // ported to c
    public static float getMaxInitialWeight(float[] networkSpec)
    {
        return networkSpec[MAX_INITIAL_WEIGHT_IDX];
    }

    // ported to c
    public static void setMaxInitialWeight(float[] networkSpec, float weight)
    {
        networkSpec[MAX_INITIAL_WEIGHT_IDX] = weight;
    }

    // ported to c
    public static int getForwardPassLinkIndex(float[] networkSpec)
    {
        return (int)networkSpec[FORWARD_PASS_LINK_ORDER_DATA_IDX];
    }

    // ported to c
    public static int getBackwardPassLinkIndex(float[] networkSpec)
    {
        return (int)networkSpec[BACKWARD_PASS_LINK_ORDER_DATA_IDX];
    }

    // *o o* *o o* *o o* *o o* *o o* *o o* *o o* *o o* *o o* *o o*
    //   BEGIN
    //   Main Interface Functions
    // *o o* *o o* *o o* *o o* *o o* *o o* *o o* *o o* *o o* *o o*


    public float[] getCopyofData()
    {
        return Arrays.copyOf(_networkData, _networkData.length);
    }

    public float[] getActualData()
    {
        return _networkData;
    }




    // ported to c
    // + reviewed +
    /**
     * Call this after setting the initial input activation
     * @param networkSpec

     */
    public static void forwardPass(float[] networkSpec, float[] inputActivation)
    {
        int feedforward_link_count_idx = getForwardPassLinkIndex(networkSpec);
        setInputActivation(networkSpec, inputActivation);
        int linkIdIdx = 0;
        int linkId;

        int numFeedforwardLinks = (int)networkSpec[feedforward_link_count_idx];
        int feedforward_order_link_idx = feedforward_link_count_idx + 1;
        for (linkIdIdx = 0; linkIdIdx < numFeedforwardLinks; linkIdIdx++ )
        {
            linkId = (int)networkSpec[feedforward_order_link_idx + linkIdIdx];
            //System.out.println(String.format("Forward pass LinkId: %1$s", linkId));
            if (linkId > 0)
            // techically, 0 is a valid linkId but since linkId of 0 is a recurrent link from the input layer to itself,
            // you would never call feedforward on it
            {

                feedforward(networkSpec, linkId);
            }
            else
            {
                // In this case, -linkId is the target layer id whose node state should be finalized (move partial
                // net input to the net input and calculate the overall node activation
                //System.out.println(String.format("Pushing net input pass LinkId: %1$s", linkId));
                pushNetInput(networkSpec, (-1 * linkId));
            }
        }
    }

    // ported to c
    /**
     * Call this after setting the initial input activation
     * @param networkSpec

     */
    public static void forwardPass(float[] networkSpec, float[] inputActivation, boolean applyErrorMaskToInput)
    {
        int feedforward_link_count_idx = getForwardPassLinkIndex(networkSpec);

        if (applyErrorMaskToInput)
        {
            int maskIndex = getOutputLayerMaskIndex(networkSpec);
            for (int i = 0; i < inputActivation.length;i++)
            {
                float mask = networkSpec[maskIndex + i];
                inputActivation[i]*=mask;
            }
        }
        setInputActivation(networkSpec, inputActivation);

        int linkIdIdx = 0;
        int linkId;

        int numFeedforwardLinks = (int)networkSpec[feedforward_link_count_idx];
        int feedforward_order_link_idx = feedforward_link_count_idx + 1;
        for (linkIdIdx = 0; linkIdIdx < numFeedforwardLinks; linkIdIdx++ )
        {
            linkId = (int)networkSpec[feedforward_order_link_idx + linkIdIdx];
            if (linkId > 0)
            // techically, 0 is a valid linkId but since linkId of 0 is a recurrent link from the input layer to itself,
            // you would never call feedforward on it
            {

                feedforward(networkSpec, linkId);
            }
            else
            {
                // In this case, -linkId is the target layer id whose node state should be finalized (move partial
                // net input to the net input and calculate the overall node activation
                pushNetInput(networkSpec, (-1 * linkId));
            }
        }
    }


    // ported to c
    // + reviewed +
    public static float[] getOutputActivation(float[] networkSpec)
    {
        int layerWidth = getLayerWidth(networkSpec, OUTPUT_LAYER_ID);
        int output_activation_idx = getLayerActivationIndex(networkSpec, OUTPUT_LAYER_ID);
        float[] output = new float[layerWidth];

        for (int i =0;i < layerWidth;i++)
            output[i] = networkSpec[output_activation_idx + i];
        return output;
    }

    // ported to c
    // + reviewed +
    public static void setInputActivation(float[] networkSpec, float[] data)
    {
        int layerWidth = getLayerWidth(networkSpec, INPUT_LAYER_ID);
        int input_activation_idx = getLayerActivationIndex(networkSpec, INPUT_LAYER_ID);
        for (int i =0;i < layerWidth;i++)
            networkSpec[input_activation_idx + i] = data[i];
    }

    // ported to c
    // + reviewed +
    /**
     * Use this to calculate the errors after each forward pass.  This should be called eventually after
     * EACH forward pass or else the error responsibility vectors will be wrong
     * @param networkSpec
     * @param targetOutput
     * @return
     */
    public static float updateForwardPassErrors(float[] networkSpec, float[] targetOutput)
    {
        return updateForwardPassErrors(networkSpec, targetOutput, true);
    }


    /**
     * Use this to calculate the errors after each forward pass.  This should be called eventually after
     * EACH forward pass or else the error responsibility vectors will be wrong
     * @param networkSpec
     * @param targetOutput
     * @return
     */
    public static float updateForwardPassErrors(float[] networkSpec, float[] targetOutput, boolean updateErrorsP)
    {
        if (updateErrorsP)
        {
            int backprop_link_count_idx = getBackwardPassLinkIndex(networkSpec);
            int linkIdIdx = 0;
            int linkId;

            int numBackwardPassLinks = (int)networkSpec[backprop_link_count_idx];
            int update_gradient_link_order = backprop_link_count_idx + 1;
            for (linkIdIdx = 0; linkIdIdx < numBackwardPassLinks; linkIdIdx++ )
            {
                linkId = (int)networkSpec[update_gradient_link_order + linkIdIdx];
                updatePartialGradient(networkSpec, linkId, targetOutput);
            }
        }

        OutputErrorFunction outError = ERROR_FUNCTION_MAP[(int)networkSpec[OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX]];
        return (float)outError.error(networkSpec, targetOutput);
    }


    // ported to c
    public static float getOutputError(float[] networkSpec, float[] targetOutput)
    {
        OutputErrorFunction outError = ERROR_FUNCTION_MAP[(int)networkSpec[OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX]];
        return (float)outError.error(networkSpec, targetOutput);
    }

    // ported to c
    public static float getRoundedOutputError(float[] networkSpec, float[] targetOutput)
    {
        int idx = getLayerActivationIndex(networkSpec, OUTPUT_LAYER_ID);
        int width = getLayerWidth(networkSpec, OUTPUT_LAYER_ID);
        float prior;
        for (int i = idx; i < width + idx; i++)
        {
            prior = networkSpec[i];
            networkSpec[i] = roundToInt(prior);
        }
        OutputErrorFunction outError = ERROR_FUNCTION_MAP[(int)networkSpec[OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX]];
        return (float)outError.error(networkSpec, targetOutput);
    }

    // ported to c
    /**
     * Use this function to update the weights after calculating the gradients along each step of the sequence
     * This can be called after a batch of learning is complete.  Between each sequence, you should reset the
     * network state via resetNetworkToInitialState
     * @param networkSpec

     */
    public static void updateWeightsFromErrors(float[] networkSpec,  LSTMNetwork.WeightUpdateType updateType)
    {
        int backprop_link_count_idx = getBackwardPassLinkIndex(networkSpec);
        int linkIdIdx = 0;
        int linkId;

        int numBackwardPassLinks = (int)networkSpec[backprop_link_count_idx];
        int update_gradient_link_order = backprop_link_count_idx + 1;
        for (linkIdIdx = 0; linkIdIdx < numBackwardPassLinks; linkIdIdx++ )
        {
            linkId = (int)networkSpec[update_gradient_link_order + linkIdIdx];
            updateWeights(networkSpec, linkId, updateType);
        }

    }

    public static void perturbWeights(float[] networkSpec, float randomizeFraction)
    {
        int backprop_link_count_idx = getBackwardPassLinkIndex(networkSpec);
        float initialDelta  = getRPROPInitialWeightDelta(networkSpec);
        float minWeight = getMinInitialWeight(networkSpec);
        float maxWeight = getMaxInitialWeight(networkSpec);
        int linkIdIdx = 0;
        int linkId;
        int numBackwardPassLinks = (int)networkSpec[backprop_link_count_idx];
        int update_gradient_link_order = backprop_link_count_idx + 1;
        for (linkIdIdx = 0; linkIdIdx < numBackwardPassLinks; linkIdIdx++ )
        {
            linkId = (int)networkSpec[update_gradient_link_order + linkIdIdx];

            initializeLink(networkSpec, linkId, initialDelta, minWeight, maxWeight, randomizeFraction);
        }
    }


    public static void perturbWeights(float[] networkSpec, float randomizeFraction, float value)
    {
        int backprop_link_count_idx = getBackwardPassLinkIndex(networkSpec);
        float initialDelta  = getRPROPInitialWeightDelta(networkSpec);
        int linkIdIdx = 0;
        int linkId;
        int numBackwardPassLinks = (int)networkSpec[backprop_link_count_idx];
        int update_gradient_link_order = backprop_link_count_idx + 1;
        for (linkIdIdx = 0; linkIdIdx < numBackwardPassLinks; linkIdIdx++ )
        {
            linkId = (int)networkSpec[update_gradient_link_order + linkIdIdx];

            initializeLink(networkSpec, linkId, initialDelta, value, value, randomizeFraction);
        }
    }


    // ported to c
    /**
     * Call this after creating a new network or when rerolling all weights
     * Basically, this, together with resetNetworkToInitialState creates a brand new random LSTM network
     * that is ready to be trained with new training sequences.  After this, you should call any of
     * setMaxInitialWeight
     * setMinInitialWeight
     * setRPROPInitialWeightDelta
     * setRPROPMaxWeightUpdateDelta
     * setRPROPMinWeightUpdateDelta
     * setRROPN_Minus
     * setRROPN_Plus
     * setLearningRate
     *
     * to reconfigure the network from its initial configuration from the builder
     * @param networkSpec

     */
    public static void initializeAllWeights(float[] networkSpec)
    {
        int backprop_link_count_idx = getBackwardPassLinkIndex(networkSpec);
        float initialDelta  = getRPROPInitialWeightDelta(networkSpec);
        float minWeight = getMinInitialWeight(networkSpec);
        float maxWeight = getMaxInitialWeight(networkSpec);
        int linkIdIdx = 0;
        int linkId;
        int numBackwardPassLinks = (int)networkSpec[backprop_link_count_idx];
        int update_gradient_link_order = backprop_link_count_idx + 1;
        for (linkIdIdx = 0; linkIdIdx < numBackwardPassLinks; linkIdIdx++ )
        {
            linkId = (int)networkSpec[update_gradient_link_order + linkIdIdx];
            initializeLink(networkSpec, linkId, initialDelta, minWeight, maxWeight);
        }
    }

    // ported to c
    /**
     * Call this at the beginning of the training input sequence
     * @param networkSpec
     */
    public static void resetNetworkToInitialState(float[] networkSpec)
    {
        FastLSTMNetwork.resetWeightHistory(networkSpec);
        FastLSTMNetwork.initializeNodeState(networkSpec);
    }



    public static FastLSTMNetwork.LSTMNetworkBuilder getFastBuilder()
    {
        return new FastLSTMNetwork.LSTMNetworkBuilder();
    }


    // *o o* *o o* *o o* *o o* *o o* *o o* *o o* *o o* *o o* *o o*
    //   END
    //   Main Interface Functions
    // *o o* *o o* *o o* *o o* *o o* *o o* *o o* *o o* *o o* *o o*


    // ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
    //      Functions to be ported to c/c++
    // ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

    // ported to c
    static void matrixMultiplyByVector(int rows, int cols, float[] all_data, int transform_matrix_id, int source_vector_idx, int target_vector_id, boolean incrementTargetVectorP)
    {
        float value;
        float inputValue;
        float rowValue;
        for (int i = 0; i < rows;i++)
        {
            value = 0;
            for (int j = 0;j < cols;j++)
            {
                inputValue = all_data[source_vector_idx + j];
                rowValue = all_data[transform_matrix_id + getMatrixCellIndex(cols, i, j)];
                value +=rowValue*inputValue;
            }


            if (incrementTargetVectorP)
                all_data[target_vector_id + i] += value;
            else
                all_data[target_vector_id + i] = value;
        }
    }

    // ported to c
    static int getMatrixCellIndex(int cols, int i, int j)
    {
        return cols*i + j;
    }

    // ported to c
    static void setMatrixValue(int cols, float[] all_data, int maxtrix_offset_index, int i, int j, float value)
    {
        int offsetIndex = getMatrixCellIndex(cols, i, j);
        all_data[maxtrix_offset_index + offsetIndex] = value;
    }

    // ported to c
    static void matrixElementwiseSet(int rows, int cols, float[] all_data, int matrix_offset_index, float value)
    {
        for (int i = 0;i < rows*cols;i++)
        {
            all_data[matrix_offset_index + i] = value;
        }
    }

    // ported to c
    static void matrixElementwiseRandomize(int rows, int cols, float[] all_data, int matrix_offset_index, float minValue, float maxValue)
    {
        double ran = 0;
        for (int i = 0;i < rows*cols;i++)
        {
            ran = randomLCG();

            all_data[matrix_offset_index + i] = (minValue + (maxValue - minValue)* (float)ran);
        }
    }

    static void matrixRandomizeFraction(int rows, int cols, float[] all_data, int matrix_offset_index, float minValue, float maxValue, float fraction)
    {
        double ran = 0;

        int randomCount = (int)(rows*cols*fraction);

        int j, i;
        int[] selectionMap = new int[rows*cols];
        for (j = 0; j < rows*cols;j++)
            selectionMap[j] = j;


        for (j = 0; j < randomCount;j++)
        {
            ran = randomLCG();
            int selectedIndex = (int)((rows*cols - j)*ran) + j;
            int effectiveIndex =  selectionMap[selectedIndex];
            int temp = selectionMap[j];
            selectionMap[j] = effectiveIndex;
            selectionMap[selectedIndex] = temp;
            all_data[matrix_offset_index + effectiveIndex] = (minValue + (maxValue - minValue)* (float)ran);

        }

    }


    // ported to c
    static void vectorElementwiseSet(int length, float[] all_data, int vector_idx_offset, float value)
    {
        for (int k = 0;k < length;k++ )
        {
            all_data[vector_idx_offset + k] = value;
        }
    }

    // ported to c
    static void vectorElementwiseMap(int length, float[] all_data, int source_idx, int target_idx, UnaryVectorOperator operation)
    {
        for (int i = 0; i < length;i++)
        {

            all_data[target_idx + i] = operation.operate(all_data, source_idx, length, i);
        }
    }


    // ported to c
    static void vectorCopy(int length, float[] all_data, int source_vector_idx_offset, int target_vector_idx_offset)
    {
        for (int k = 0;k < length;k++ )
        {
            all_data[target_vector_idx_offset + k] = all_data[source_vector_idx_offset + k];
        }
    }

    // ported to c
    static float getMatrixValue(int cols, float[] all_data, int matrix_idx, int i, int j)
    {
        return all_data[matrix_idx + getMatrixCellIndex(cols, i, j)];
    }

    // ported to c
    static float getVectorValue(float[] all_data, int vector_idx, int i)
    {
        return all_data[vector_idx + i];
    }

    // ported to c
    public static int  roundToInt(float v)
    {
        if (v < 0.5)
            return 0;
        else
            return 1;
    }



    // ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
    //                  Java Interface
    // All static methods above this can be ported to c/c++
    // Everything below this can be ported to c++ or some other object-oriented
    // language
    // ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

    public float learnMap(TrainingSpec spec, float[] inputMatchingMask)
    {
        float[] input = spec.input;
        float[] expectedOutput = spec.expectedOutput;
        float[] mask = spec.errorMask;

        if (mask != null)
        {
            setOutputErrorMask (_networkData, mask);

        }
        else
        {
            //clearOutputErrorMask(_networkData);
            for (int i = 0;i < mask.length;i++)
            {
                _networkData[getOutputLayerMaskIndex(_networkData) + i] = mask[i];
            }
        }

        forwardPass(_networkData, input);
        if (inputMatchingMask != null)
        {
            for (int i = 0;i < inputMatchingMask.length;i++)
            {
                if (inputMatchingMask[i] == 1)
                {
                    expectedOutput[i] = _networkData[getOutputLayerMaskIndex(_networkData) + i];
                }

            }
        }

        return updateForwardPassErrors(_networkData, expectedOutput);
    }

    public static float learnMap(float[] networkSpec, TrainingSpec spec, float[] inputMatchingMask)
    {
        float[] input = spec.input;
        float[] expectedOutput = spec.expectedOutput;
        float[] mask = spec.errorMask;
        if (spec.resetNetworkStateP)
            FastLSTMNetwork.resetNetworkToInitialState(networkSpec);

        if (mask != null)
        {
            setOutputErrorMask (networkSpec, mask);

        }
        else
        {

            // TODO: Decide if we want to explicitly clear this or leave it as it was
            //clearOutputErrorMask(networkSpec);
        }

        forwardPass(networkSpec, input);
        if (inputMatchingMask != null)
        {
            for (int i = 0;i < inputMatchingMask.length;i++)
            {
                if (inputMatchingMask[i] == 1)
                {
                    expectedOutput[i] = networkSpec[getOutputLayerMaskIndex(networkSpec) + i];
                }

            }
        }

        return updateForwardPassErrors(networkSpec, expectedOutput);
    }

    public static float[] learnMapWithDetails(float[] networkSpec, TrainingSpec spec, float[] inputMatchingMask)
    {
        float[] input = spec.input;
        float[] expectedOutput = spec.expectedOutput;
        float[] mask = spec.errorMask;
        float[] out = new float[expectedOutput.length + 1];
        if (spec.resetNetworkStateP)
            FastLSTMNetwork.resetNetworkToInitialState(networkSpec);

        if (mask != null)
        {
            setOutputErrorMask (networkSpec, mask);

        }
        else
        {

            // TODO: Decide if we want to explicitly clear this or leave it as it was
            //clearOutputErrorMask(networkSpec);
        }

        forwardPass(networkSpec, input);
        if (inputMatchingMask != null)
        {
            for (int i = 0;i < inputMatchingMask.length;i++)
            {
                if (inputMatchingMask[i] == 1)
                {
                    expectedOutput[i] = networkSpec[getOutputLayerMaskIndex(networkSpec) + i];
                }

            }
        }
        float error = updateForwardPassErrors(networkSpec, expectedOutput);
        float[] output = FastLSTMNetwork.getOutputActivation(networkSpec);
        for (int i = 0;i < output.length;i ++)
        {
            if (i == 0)
                out[i] = error;
            out[i + 1] = output[i];
        }
        return out;
    }


    public static float[] addNoise(float[] input, float width, boolean gaussian)
    {
        float[] out = new float[input.length];
        for (int i = 0; i < input.length;i++)
        {
            float random = 1;
            if (gaussian)
                random = (float)randomGaussian();
            else
            {
                random = (float)randomLCG() - 0.5F;
            }
            out[i] = Math.min(1, Math.max(0, input[i] + width*random));
        }
        return out;
    }





}
