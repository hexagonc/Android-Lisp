package com.evolved.automata.nn;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 2/10/17.
 */

public class FastLSTMNetwork extends LSTMNetwork{

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
        public FastLSTMNetwork.LSTMNetworkBuilder setInputNodeCount(int count)
        {
            inputLayer = new InputLayer(count, new IdentityActivation());
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
            lstm.initialize();
            lstm.weightParameters = weightParameters;

            return lstm;
        }


    }



    // ---------------------------------------------------------------
    // Port these interfaces to c/c++ function pointer typedefs
    // ---------------------------------------------------------------


    public static interface UnaryOperation
    {
        public float operation(float value);
    }


    public static interface OutputErrorFunction
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

            int width = getLayerWidth(networkData, OUTPUT_LAYER_ID);
            int output_layer_activation_idx = getLayerActivationIndex(networkData, OUTPUT_LAYER_ID);
            double mse = 0, error;
            double output_activation, expected_activation;
            for (int i = 0;i < width;i++)
            {
                output_activation = networkData[output_layer_activation_idx + i];
                expected_activation = expectedOutput[i];
                error = (expected_activation - output_activation);
                mse += error*error;
            }
            return mse/width;
        }

        @Override
        public double errorPartialDerivative(float[] networkData, float[] expectedOutput, int i)
        {
            int width = getLayerWidth(networkData, OUTPUT_LAYER_ID);
            int output_layer_activation_idx = getLayerActivationIndex(networkData, OUTPUT_LAYER_ID);
            double output_activation = networkData[output_layer_activation_idx + i];
            return 2*(expectedOutput[i] - output_activation)/width;
        }
    };



    public static final UnaryOperation SigmoidOperator = new UnaryOperation() {
        @Override
        public float operation(float value)
        {

            return (float)(1/(1 + Math.exp(-1*value)));
        }
    };



    public static final UnaryOperation SigmoidDerivativeOperator = new UnaryOperation() {
        @Override
        public float operation(float value)
        {
            double sig = (1/(1 + Math.exp(-1*value)));
            return (float)(sig * (1 - sig));
        }
    };


    public static final UnaryOperation HypertangentOperator = new UnaryOperation() {
        @Override
        public float operation(float value)
        {

            return (float)Math.tanh(value);
        }
    };


    public static final UnaryOperation HypertangentDerivativeOperator = new UnaryOperation() {
        @Override
        public float operation(float value)
        {
            double cosh = Math.cosh(value);
            return (float)(1/cosh/cosh);
        }
    };


    public static final UnaryOperation IdentityOperator = new UnaryOperation() {
        @Override
        public float operation(float value)
        {

            return value;
        }
    };



    public static final UnaryOperation IdentityDerivativeOperator = new UnaryOperation() {
        @Override
        public float operation(float value)
        {

            return 1;
        }
    };

    public static final UnaryOperation RectilinearOperator = new UnaryOperation() {
        @Override
        public float operation(float value)
        {
            if (value < 0)
                return 0;
            return value;
        }
    };

    public static final UnaryOperation RectilinearDerivativeOperator = new UnaryOperation() {
        @Override
        public float operation(float value)
        {
            if (value < 0)
                return 0;
            return 1;
        }
    };



    float[] _networkData;

    // ******************************************************
    // START: Port these constants to c/c++ as static variables/defines
    // ******************************************************


    static final int LINK_DATA_SOURCE_LAYER_ID_IDX = 0;
    static final int LINK_DATA_TARGET_LAYER_ID_IDX = 1;
    static final int LINK_DATA_WEIGHT_MATRIX_IDX = 2;

    static final int NUM_LAYERS = 8;


    // Start of data map indexes
    static final int OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX = 0;
    static final int LAYER_ACTIVATION_FUNCTION_ID_IDX = OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX + 1;
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
    static final int SIGMOID_ACTIVATION_ID = 0;
    static final int HYPERTANGENT_ACTIVATION_ID = 1;
    static final int RECTILINEAR_ACTIVATION_ID = 2;
    static final int IDENTITY_ACTIVATION_ID = 3;
    static final int SOFTMAX_ACTIVATION_ID = 4;

    static final UnaryOperation[] ACTIVATION_FUNCTION_MAP = {SigmoidOperator, HypertangentOperator, RectilinearOperator, IdentityOperator};
    static final UnaryOperation[] ACTIVATION_FUNCTION_DERIVATIVE_MAP = {SigmoidDerivativeOperator, HypertangentDerivativeOperator, RectilinearDerivativeOperator, IdentityDerivativeOperator};

    static final OutputErrorFunction[] ERROR_FUNCTION_MAP = {MeanSquaredError};
    // Default layer activation definition

    static final int[] LAYER_ID_ACTIVATION_FUNCTION_MAP = {IDENTITY_ACTIVATION_ID, SIGMOID_ACTIVATION_ID, SIGMOID_ACTIVATION_ID, SIGMOID_ACTIVATION_ID, SIGMOID_ACTIVATION_ID, SIGMOID_ACTIVATION_ID, SIGMOID_ACTIVATION_ID, HYPERTANGENT_ACTIVATION_ID};


    static final int MSE_ERROR_FUNCTION_ID = 0;
    static final int CROSS_ENTROPY_ERROR_ID = 1;

    // ******************************************************
    // END: Constants ported to c/c++ as static variables/defines
    // ******************************************************


    // Create a static parameter for this in c/c++

    static final OutputErrorFunction defaultOutputErrorFunction = MeanSquaredError;
    // Parameters to be passed to all raw neural network functions

    int backprop_link_count_idx;
    int feedforward_link_count_idx;
    int connection_data_idx; // not used

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
        int numInputNodes = inputLayer.getDimen();
        int numOutputNodes = outputLayer.getDimen();
        int numMemoryCellStates = 0;


        // Use default layer activation functions for now
        // TODO: make this setable from the builder, especially will want to be able to change the output activation function
        for (int i = 0; i< LAYER_ID_ACTIVATION_FUNCTION_MAP.length;i++)
        {
            network.put(LAYER_ACTIVATION_FUNCTION_ID_IDX + i, (float)LAYER_ID_ACTIVATION_FUNCTION_MAP[i]);
        }

        // TODO: make this depend on the actual builder configuration
        network.put(OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX, (float)MSE_ERROR_FUNCTION_ID);


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
                network.put(Integer.valueOf(NETWORK_GRAPH_BASE_IDX + sourceId*(NUM_LAYERS + 1) + 1 + layerIdIdx), (float)targetId);

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
        network.put(LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerIdIdx++ , (float)output_layer_net_input_idx);


        int cell_input_net_input_idx = output_layer_error_responsibility_idx + O_length;
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
        feedforward_link_count_idx = memory_cell_net_input_idx + P_length; // ** pass to function **
        network.put(feedforward_link_count_idx, (float)feedforwardLinkOrder.length);



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


        int updateOrderIndex = backprop_link_data_idx;
        for (String linkSpec:weightUpdateLinkOrder)
        {
            Pair<Integer, Integer> link = getSourceTargerLayerIdsFromLinkSpec(linkSpec);


            network.put(updateOrderIndex, (float)getLinkId(link));
            updateOrderIndex++;
        }

        int weightUpdateDataWidth = Math.max(1, weightUpdateLinkOrder.length);
        connection_data_idx = backprop_link_data_idx + weightUpdateDataWidth; // *** pass to function ***

        // set the link data
        int nextIndex = connection_data_idx;
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



    private int getLinkId(Pair<Integer, Integer> linkPair)
    {
        return getLinkId(linkPair.getLeft(), linkPair.getRight());
    }








    // --<(==)>-- --<(==)>-- --<(==)>-- --<(==)>-- --<(==)>-- --<(==)>-- --<(==)>--
    //                  Main Neural Network Methods
    // --<(==)>-- --<(==)>-- --<(==)>-- --<(==)>-- --<(==)>-- --<(==)>-- --<(==)>--

    private static int getLinkId(int sourceId, int targetId)
    {
        return sourceId* NUM_LAYERS + targetId;
    }


    static int getLayerActivationFunctionId(float[] networkSpec, int layerId)
    {
        return (int)networkSpec[LAYER_ACTIVATION_FUNCTION_ID_IDX + layerId];
    }
    static UnaryOperation getLayerActivationFunction(float[] networkSpec, int layerId)
    {

        return  ACTIVATION_FUNCTION_MAP[getLayerActivationFunctionId(networkSpec, layerId)];
    }

    static UnaryOperation getLayerActivationFunctionDerivative(float[] networkSpec, int layerId)
    {

        return  ACTIVATION_FUNCTION_DERIVATIVE_MAP[getLayerActivationFunctionId(networkSpec, layerId)];
    }


    static int getLayerTargetLinksIndex(int layerId)
    {
        return NETWORK_GRAPH_BASE_IDX + (NUM_LAYERS + 1)*layerId;
    }

    static  int getLayerActivationIndex(float[] networkSpec, int layerId)
    {
        int layer_width = getLayerWidth(networkSpec, layerId);
        int layer_state_idx = getLayerStateIndex(networkSpec, layerId);
        if (layerId == PEEPHOLE_LAYER_ID)
            return layer_state_idx;
        else
            return layer_state_idx + layer_width;
    }

    static int getLayerNetInputIndex(float[] networkSpec, int layerId)
    {
        int layer_width = getLayerWidth(networkSpec, layerId);
        int layer_state_idx = getLayerStateIndex(networkSpec, layerId);
        if (layerId == PEEPHOLE_LAYER_ID)
            return layer_state_idx + layer_width;
        else
            return layer_state_idx;
    }

    static int getLayerPartialNetInputIndex(float[] networkSpec, int layerId)
    {
        int layer_width = getLayerWidth(networkSpec, layerId);
        int layer_state_idx = getLayerStateIndex(networkSpec, layerId);
        if (layerId == PEEPHOLE_LAYER_ID)
            return -1;
        else
            return layer_state_idx + 2 * layer_width;
    }

    static int getLayerErrorResponsibilityIndex(float[] networkSpec, int layerId)
    {
        int layer_width = getLayerWidth(networkSpec, layerId);
        int layer_state_idx = getLayerStateIndex(networkSpec, layerId);
        if (layerId == PEEPHOLE_LAYER_ID)
            return -1;
        else
            return layer_state_idx + 3 * layer_width;
    }

    static int getLinkSourceLayerIdIdx(int data_idx)
    {
        return data_idx + LINK_DATA_SOURCE_LAYER_ID_IDX;
    }

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

    static int getLinkWeightIndex(int rows, int cols, int data_idx)
    {
        return data_idx + LINK_DATA_WEIGHT_MATRIX_IDX;
    }

    static int getLinkWeightDeltaIndex(int rows, int cols, int data_idx)
    {
        return data_idx + LINK_DATA_WEIGHT_MATRIX_IDX + rows*cols;
    }

    static int getLinkElligibilityTraceIndex(int rows, int cols, int data_idx)
    {
        return data_idx + LINK_DATA_WEIGHT_MATRIX_IDX + 2 * rows*cols;
    }

    static int getLinkCalculatedGradientIndex(int rows, int cols, int data_idx)
    {
        return data_idx + LINK_DATA_WEIGHT_MATRIX_IDX + 3 * rows*cols;
    }

    static int getLinkPrevCalculatedGradientIndex(int rows, int cols, int data_idx)
    {
        return data_idx + LINK_DATA_WEIGHT_MATRIX_IDX + 4 * rows*cols;
    }

    static int getLinkSourceLayerId(float[] networkSpec, int linkId)
    {
        int linkDataIdx = getLinkDataIndex(networkSpec, linkId);
        return (int)networkSpec[linkDataIdx + LINK_DATA_SOURCE_LAYER_ID_IDX];
    }

    static int getLinkSourceLayerWidth(float[] networkSpec, int linkId)
    {
        int linkDataIdx = getLinkDataIndex(networkSpec, linkId);
        int layerId = (int)networkSpec[linkDataIdx + LINK_DATA_SOURCE_LAYER_ID_IDX];
        return getLayerWidth(networkSpec, layerId);
    }


    static int getLinkTargetLayerId(float[] networkSpec, int linkId)
    {
        int linkDataIdx = getLinkDataIndex(networkSpec, linkId);
        return (int)networkSpec[linkDataIdx + LINK_DATA_TARGET_LAYER_ID_IDX];
    }

    static int getLinkTargetLayerWidth(float[] networkSpec, int linkId)
    {
        int linkDataIdx = getLinkDataIndex(networkSpec, linkId);
        int layerId = (int)networkSpec[linkDataIdx + LINK_DATA_TARGET_LAYER_ID_IDX];
        return getLayerWidth(networkSpec, layerId);
    }


    static int getLayerWidth(float[] networkSpec, int layerId)
    {
        return (int)networkSpec[LAYER_DIMEN_MAP_IDX + layerId];
    }

    static int getLayerStateIndex(float[] networkSpec, int layerId)
    {
        return (int)networkSpec[LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerId];
    }

    static int getLinkDataIndex(float[] networkSpec, int linkId)
    {
        if (linkId >= 0)
        {
            return (int)networkSpec[LINK_ID_TO_DATA_IDX_IDX + linkId];
        }
        else // Throw exception?
            return -1;

    }


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
        UnaryOperation activation = getLayerActivationFunction(networkSpec, PEEPHOLE_LAYER_ID);
        for (int i = 0;i < peephole_layer_width;i++)
        {

            networkSpec[peephole_net_input_idx + i] =
                    networkSpec[peephole_net_input_idx + i] * networkSpec[forget_gate_activation_idx + i] +
                    networkSpec[cell_input_activation_idx + i]*networkSpec[input_gate_activation_idx + i];

            // precalculating the cell state (peephole net activation)
            networkSpec[peepholeActivationIdx + i] = activation.operation(networkSpec[peephole_net_input_idx + i]);
        }
    }



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

        matrixMultiplyByVector(targetLayerWidth, sourceLayerWidth, networkSpec, linkWeightMatrixIdx, sourceNodeStateActivation_idx, targetPartialNetInput_idx, true);


    }

    


    // .:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:.
    //          Backward pass functions
    // .:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:..:8:.


    // + provisionally reviewed +
    static float getOutputLinkErrorPartialDerivative(float[] networkSpec, int sourceNodeIndex)
    {

        return networkSpec[getLayerErrorResponsibilityIndex(networkSpec, CELL_OUTPUT_LAYER_ID) + sourceNodeIndex];
    }

    // + reviewed +
    static void setErrorResponsibility(float[] networkSpec, int layerId, float[] targetOutput)
    {
        int layer_width = getLayerWidth(networkSpec, layerId);

        int layer_error_responsibility_idx = getLayerErrorResponsibilityIndex(networkSpec, layerId);
        int memory_cell_state_idx = getLayerNetInputIndex(networkSpec, PEEPHOLE_LAYER_ID);
        int memory_cell_state_squashed_idx = getLayerActivationFunctionId(networkSpec, PEEPHOLE_LAYER_ID);
        int net_input_idx = getLayerNetInputIndex(networkSpec, layerId);
        UnaryOperation activationSquashedDerivative = getLayerActivationFunctionDerivative(networkSpec, layerId);

        float outputError;
        switch (layerId)
        {
            case OUTPUT_LAYER_ID: // + reviewed +
                OutputErrorFunction errorFunction = ERROR_FUNCTION_MAP[(int)networkSpec[OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX]];
                int output_net_input_idx = getLayerNetInputIndex(networkSpec, OUTPUT_LAYER_ID);
                UnaryOperation outputActivationPrime = getLayerActivationFunctionDerivative(networkSpec, OUTPUT_LAYER_ID);
                float net;
                for (int i = 0;i < layer_width;i++)
                {
                    net = networkSpec[output_net_input_idx + i];
                    networkSpec[layer_error_responsibility_idx + i] =
                            (float)(errorFunction.errorPartialDerivative(networkSpec, targetOutput, i)*outputActivationPrime.operation(net));
                }
                setErrorResponsibility(networkSpec, CELL_OUTPUT_LAYER_ID, targetOutput);
                break;
            case INPUT_GATE_LAYER_ID: // + reviewed +
            case CELL_INPUT_LAYER_ID:
            case FORGET_LAYER_ID:
                int output_gate_activation_idx = getLayerActivationIndex(networkSpec, OUTPUT_GATE_LAYER_ID);
                UnaryOperation memory_cell_squashed_derivative = getLayerActivationFunctionDerivative(networkSpec, PEEPHOLE_LAYER_ID);

                for (int i=0;i<layer_width;i++)
                {
                    outputError = getOutputLinkErrorPartialDerivative(networkSpec, i);
                    networkSpec[layer_error_responsibility_idx + i] =
                            networkSpec[output_gate_activation_idx + i]*memory_cell_squashed_derivative.operation(networkSpec[memory_cell_state_idx + i])*outputError;
                }
                break;
            case OUTPUT_GATE_LAYER_ID: // + reviewed +
                for (int i=0;i<layer_width;i++)
                {
                    outputError = getOutputLinkErrorPartialDerivative(networkSpec, i);
                    networkSpec[layer_error_responsibility_idx + i] = activationSquashedDerivative.operation(networkSpec[net_input_idx + i]) *
                            networkSpec[memory_cell_state_squashed_idx + i]*outputError;
                }
                break;
            case CELL_OUTPUT_LAYER_ID: // + reviewed +
                int output_layer_error_responsibility_idx = getLayerErrorResponsibilityIndex(networkSpec, OUTPUT_LAYER_ID);
                int output_layer_width = getLayerWidth(networkSpec, OUTPUT_LAYER_ID);
                int linkId = getLinkId(CELL_OUTPUT_LAYER_ID, OUTPUT_LAYER_ID);
                int output_link_data_idx = getLinkDataIndex(networkSpec, linkId);

                int output_link_weight_idx = getLinkWeightIndex(output_layer_width, layer_width, output_link_data_idx);
                float error = 0;
                for (int j = 0; j < layer_width;j++)
                {
                    error = 0;
                    for (int k = 0; k < output_layer_width;k++)
                    {
                        error += getMatrixValue(layer_width, networkSpec, output_link_weight_idx, k, j)*networkSpec[output_layer_error_responsibility_idx + k];
                    }
                    networkSpec[layer_error_responsibility_idx + j] = error;
                }
                break;
            default:

        }
    }

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

        float target_net_input;
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
                UnaryOperation squashedCellInputDerivative = getLayerActivationFunctionDerivative(networkSpec, CELL_INPUT_LAYER_ID);
                for (int i = 0; i < targetLayerWidth; i++)
                {
                    for (int j = 0; j < sourceLayerWidth;j++)
                    {
                        sourceActivation = networkSpec[source_activation_idx + j];
                        // can set the squashing function of the cell input to the identity function to simplify targetNetInputSquashed to 1
                        float targetNetInputSquashed = squashedCellInputDerivative.operation(networkSpec[target_net_input_idx + i]);

                        previousTraceCellValue = getMatrixValue(sourceLayerWidth, networkSpec, eligibilityTrace_idx, i, j);

                        newTraceValue = previousTraceCellValue * getVectorValue(networkSpec, forget_gate_activation_idx, i) +
                                getVectorValue(networkSpec, input_gate_activation_idx, i)*targetNetInputSquashed*sourceActivation;

                        setMatrixValue(sourceLayerWidth, networkSpec, eligibilityTrace_idx, i, j, newTraceValue);
                    }
                }
                break;
            case INPUT_GATE_LAYER_ID: // + reviewed +
                UnaryOperation targetActivationDerivative = getLayerActivationFunctionDerivative(networkSpec, target_layer_id);
                int cell_input_activation_idx = getLayerActivationIndex(networkSpec, CELL_INPUT_LAYER_ID);
                for (int i = 0; i < targetLayerWidth; i++)
                {
                    for (int j = 0; j < sourceLayerWidth;j++)
                    {
                        sourceActivation = networkSpec[source_activation_idx + j];

                        previousTraceCellValue = getMatrixValue(sourceLayerWidth, networkSpec, eligibilityTrace_idx, i, j);

                        target_net_input = getVectorValue(networkSpec, target_net_input_idx, i);


                        newTraceValue = previousTraceCellValue * getVectorValue(networkSpec, forget_gate_activation_idx, i) +
                                getVectorValue(networkSpec, cell_input_activation_idx, i) * targetActivationDerivative.operation(target_net_input)*sourceActivation;

                        setMatrixValue(sourceLayerWidth, networkSpec, eligibilityTrace_idx, i, j, newTraceValue);
                    }
                }
                break;
            case FORGET_LAYER_ID: // + reviewed +
                peephole_net_input_idx = getLayerNetInputIndex(networkSpec, PEEPHOLE_LAYER_ID);

                UnaryOperation forgetGetSquashedDerivative = getLayerActivationFunctionDerivative(networkSpec, FORGET_LAYER_ID);
                float forgetSquashedPrime;
                for (int i = 0; i < targetLayerWidth; i++)
                {
                    for (int j = 0; j < sourceLayerWidth;j++)
                    {
                        sourceActivation = networkSpec[source_activation_idx + j];

                        forgetSquashedPrime = forgetGetSquashedDerivative.operation(networkSpec[target_net_input_idx + i]);

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

    static float sign(float value)
    {
        if (value > 0)
            return 1;
        else if (value == 0)
            return 0;
        else
            return -1;
    }

    static void updateWeights(float[] networkSpec, int linkId, float n_minus, float n_plus, float deltaMax, float deltaMin, LSTMNetwork.WeightUpdateType updateType)
    {

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

                float learningRate = 0.00001F;
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


    // + reviewed +
    static void pushNetInput(float[] networkSpec, int layerId)
    {
        int layer_activation_idx = getLayerActivationIndex(networkSpec, layerId);
        int layer_partial_net_idx = getLayerPartialNetInputIndex(networkSpec, layerId);
        int layer_net_input_idx = getLayerNetInputIndex(networkSpec, layerId);

        int layerWidth = getLayerWidth(networkSpec, layerId);
        UnaryOperation activationFunction = getLayerActivationFunction(networkSpec, layerId);
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
                UnaryOperation function = getLayerActivationFunction(networkSpec, layerId);
                vectorElementwiseMap(layerWidth, networkSpec, net_input_idx, layer_activation_idx, function);
                break;
        }
    }

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

    // o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o
    //              Primary Public Helper Utility functions
    // o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o


    public static float[] getNodeStateSnapshot(float[] networkSpec, int feedforware_link_count_index)
    {
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


    public static void loadNodeStateFromSnapshot(float[] networkSpec, int feedforware_link_count_index, float[] nodeStateSnapshot)
    {
        int I_length = Math.max(1, (int)networkSpec[INPUT_NODE_COUNT_IDX]);
        int O_length = Math.max(1, (int)networkSpec[OUTPUT_NODE_COUNT_IDX]);
        int P_length = Math.max(1, (int)networkSpec[NUM_CELL_STATES_IDX]);


        int node_state_length = feedforware_link_count_index - LAYER_STATE_BASE_IDX;
        assert node_state_length == I_length*4 + O_length*4 + 22*P_length;
        for (int i = 0;i<node_state_length;i++)
        {
            networkSpec[LAYER_STATE_BASE_IDX + i] = nodeStateSnapshot[i];
        }
    }

    /**
     * Copies the node state from one networkSpec to another that are the same node dimension
     * @param sourceNetworkSpec
     * @param targetNetworkSpec
     * @param feedforward_link_count_index
     */
    public static void loadNodeStateFromOtherNetwork(float[] sourceNetworkSpec, float[] targetNetworkSpec, int feedforward_link_count_index)
    {
        for (int i = LAYER_STATE_BASE_IDX;i<feedforward_link_count_index;i++)
        {
            targetNetworkSpec[i] = sourceNetworkSpec[i];
        }
    }

    // + reviewed +
    /**
     * Reset the network node state such as when you are learning a pattern sequence from the
     * default context
     * @param networkSpec
     */
    public static void initializeNodeState(float[] networkSpec)
    {
        resetMemoryCellState(networkSpec);
    }

    // + reviewed +
    /**
     * Call this after creating a new network or when rerolling all weights
     * Basically, this, together with initializeNodeState creates a brand new random LSTM network
     * @param networkSpec
     * @param backprop_link_count_idx
     * @param initialDelta
     * @param minWeight
     * @param maxWeight
     */
    public static void initializeAllWeights(float[] networkSpec, int backprop_link_count_idx, float initialDelta, float minWeight, float maxWeight)
    {
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


    // *o o* *o o* *o o* *o o* *o o* *o o* *o o* *o o* *o o* *o o*
    //   BEGIN
    //   Main Interface Functions
    // *o o* *o o* *o o* *o o* *o o* *o o* *o o* *o o* *o o* *o o*




    // + reviewed +
    /**
     * Call this after setting the initial input activation
     * @param networkSpec
     * @param feedforward_link_count_idx
     */
    public static void forwardPass(float[] networkSpec, int feedforward_link_count_idx, float[] inputActivation)
    {
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

    // + reviewed +
    public static void setInputActivation(float[] networkSpec, float[] data)
    {
        int layerWidth = getLayerWidth(networkSpec, INPUT_LAYER_ID);
        int input_activation_idx = getLayerActivationIndex(networkSpec, INPUT_LAYER_ID);
        for (int i =0;i < layerWidth;i++)
            networkSpec[input_activation_idx + i] = data[i];
    }

    // + reviewed +
    /**
     * Use this to calculate the errors
     * @param networkSpec
     * @param backprop_link_count_idx
     * @param targetOutput
     * @return
     */
    public static float updateForwardPassErrors(float[] networkSpec, int backprop_link_count_idx, float[] targetOutput)
    {
        int linkIdIdx = 0;
        int linkId;

        int numBackwardPassLinks = (int)networkSpec[backprop_link_count_idx];
        int update_gradient_link_order = backprop_link_count_idx + 1;
        for (linkIdIdx = 0; linkIdIdx < numBackwardPassLinks; linkIdIdx++ )
        {
            linkId = (int)networkSpec[update_gradient_link_order + linkIdIdx];
            updatePartialGradient(networkSpec, linkId, targetOutput);
        }

        OutputErrorFunction outError = ERROR_FUNCTION_MAP[(int)networkSpec[OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX]];
        return (float)outError.error(networkSpec, targetOutput);
    }

    /**
     * Use this function to update the weights after calculating the gradients along each step of the sequence
     * @param networkSpec
     * @param backprop_link_count_idx
     * @param n_minus
     * @param n_plus
     * @param deltaMax
     * @param deltaMin
     */
    public static void updateWeightsFromErrors(float[] networkSpec, int backprop_link_count_idx, float n_minus, float n_plus, float deltaMax, float deltaMin, LSTMNetwork.WeightUpdateType updateType)
    {
        int linkIdIdx = 0;
        int linkId;

        int numBackwardPassLinks = (int)networkSpec[backprop_link_count_idx];
        int update_gradient_link_order = backprop_link_count_idx + 1;
        for (linkIdIdx = 0; linkIdIdx < numBackwardPassLinks; linkIdIdx++ )
        {
            linkId = (int)networkSpec[update_gradient_link_order + linkIdIdx];
            updateWeights(networkSpec, linkId, n_minus, n_plus, deltaMax, deltaMin, updateType);
        }

    }


    /**
     * Call this at the beginning of learning a new pattern sequence batch
     * @param networkSpec
     * @param backprop_link_count_idx
     */
    public static void resetWeightHistory(float[] networkSpec, int backprop_link_count_idx)
    {
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

    static int getMatrixCellIndex(int cols, int i, int j)
    {
        return cols*i + j;
    }

    static void setMatrixValue(int cols, float[] all_data, int maxtrix_offset_index, int i, int j, float value)
    {
        int offsetIndex = getMatrixCellIndex(cols, i, j);
        all_data[maxtrix_offset_index + offsetIndex] = value;
    }

    static void matrixElementwiseSet(int rows, int cols, float[] all_data, int matrix_offset_index, float value)
    {
        for (int i = 0;i < rows*cols;i++)
        {
            all_data[matrix_offset_index + i] = value;
        }
    }

    static void matrixElementwiseRandomize(int rows, int cols, float[] all_data, int matrix_offset_index, float minValue, float maxValue)
    {
        for (int i = 0;i < rows*cols;i++)
        {
            all_data[matrix_offset_index + i] = (float)(minValue + (maxValue - minValue)*Math.random());
        }
    }


    static void vectorElementwiseSet(int length, float[] all_data, int vector_idx_offset, float value)
    {
        for (int k = 0;k < length;k++ )
        {
            all_data[vector_idx_offset + k] = value;
        }
    }

    static void vectorElementwiseMap(int length, float[] all_data, int source_idx, int target_idx, UnaryOperation operation)
    {
        float value;
        for (int i = 0; i < length;i++)
        {
            value = all_data[source_idx + i];
            all_data[target_idx + i] = operation.operation(value);
        }
    }


    static void vectorCopy(int length, float[] all_data, int source_vector_idx_offset, int target_vector_idx_offset)
    {
        for (int k = 0;k < length;k++ )
        {
            all_data[target_vector_idx_offset + k] = all_data[source_vector_idx_offset + k];
        }
    }

    static float getMatrixValue(int cols, float[] all_data, int matrix_idx, int i, int j)
    {
        return all_data[matrix_idx + getMatrixCellIndex(cols, i, j)];
    }

    static float getVectorValue(float[] all_data, int vector_idx, int i)
    {
        return all_data[vector_idx + i];
    }



    /*

    static void vectorElementwiseRandomize(int length, float[] all_data, int vector_idx_offset, float minValue, float maxValue)
    {
        for (int k = 0;k < length;k++ )
        {
            all_data[vector_idx_offset + k] = (float)(minValue + (maxValue - minValue)*Math.random());
        }
    }

    static void setVectorValue(float[] all_data, int vector_offset_index, int i, float value)
    {

        all_data[vector_offset_index + i] = value;
    }

    static void matrixAddition(int rows, int cols, float[] all_data, int left_idx, int right_idx, int target_idx)
    {
        for (int i = 0;i < rows*cols;i++)
        {
            all_data[target_idx + i] = all_data[left_idx + i] + all_data[right_idx + i];
        }
    }


    static void matrixMap(int rows, int cols, float[] all_data, int source_idx, int target_idx, MatrixMap mapper)
    {
        float sourceCellValue;
        for (int i=0;i < rows;i++)
        {
            for (int j = 0;j < cols;j++)
            {
                sourceCellValue = all_data[source_idx + getMatrixCellIndex(cols, i, j)];
                mapper.map(all_data, target_idx, i, j, sourceCellValue);
            }
        }
    }

    static void vectorMap(int length, float[] all_data, int source_idx, int target_idx, VectorMap mapper)
    {
        float sourceVectorValue;
        for (int i = 0;i < length;i++)
        {
            sourceVectorValue = all_data[source_idx + i];
            mapper.map(all_data, target_idx, i, sourceVectorValue);
        }
    }

    static void matrixMultiplyByVector(int rows, int cols, float[] all_data, int transform_matrix_id, int source_vector_idx, int target_vector_id)
    {
        matrixMultiplyByVector(rows, cols, all_data, transform_matrix_id, source_vector_idx, target_vector_id, false);
    }

    static void matrixElementwiseMap(int rows, int cols, float[] all_data, int source_idx, int target_idx, UnaryOperation operation)
    {
        float value;
        for (int i = 0; i < rows;i++)
        {
            for (int j = 0; j < cols; j++)
            {
                value = all_data[source_idx + getMatrixCellIndex(cols, i, j)];
                all_data[target_idx + getMatrixCellIndex(cols, i, j)] = operation.operation(value);
            }
        }
    }
    */





}
