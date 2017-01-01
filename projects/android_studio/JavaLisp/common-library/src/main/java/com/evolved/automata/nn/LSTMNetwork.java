package com.evolved.automata.nn;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;


/**
 * Created by Evolved8 on 12/9/16.
 */
public class LSTMNetwork {


    public static boolean DEBUG = true;
    public static class LSTMNetworkBuilder {

        HashMap<String, NodeGroup> initialNodeMap = null;
        OutputLayer outputLayer;
        InputLayer inputLayer;
        ArrayList<Pair<String, Integer>> memoryCellSpecList;
        String[] feedforwardLinkOrder;
        String[] weightUpdateLinkOrder;
        HashMap<String, ArrayList<String>> connectivityMap;
        WeightUpdateType updateType = WeightUpdateType.DEFAULT;
        HashMap<WeightUpdateParameters, Double> weightParameters;
        String linkWeightData;
        HashMap<String, Double> biasSpecMap;


        public LSTMNetworkBuilder()
        {
            memoryCellSpecList = new ArrayList<Pair<String, Integer>>();
            connectivityMap = new HashMap<String, ArrayList<String>>();
            weightParameters = new HashMap<WeightUpdateParameters, Double>();
            biasSpecMap = new HashMap<String, Double> ();
        }

        public LSTMNetworkBuilder setLinkData(String data)
        {
            linkWeightData = data;
            return this;
        }

        public LSTMNetworkBuilder addWeightParameter(WeightUpdateParameters param, double value)
        {
            weightParameters.put(param, value);
            return this;
        }
        public LSTMNetworkBuilder setInputLayer(InputLayer input)
        {
            inputLayer = input;
            return this;
        }

        public LSTMNetworkBuilder setLinkWeightSerializedData(String data)
        {
            linkWeightData = data;
            return this;
        }

        public LSTMNetworkBuilder setOutputLayer(OutputLayer output)
        {
            outputLayer = output;
            return this;
        }

        /**
         * This implicitly defines an input layer using the identity squashing function
         * @param count
         * @return
         */
        public LSTMNetworkBuilder setInputNodeCount(int count)
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
        public LSTMNetworkBuilder setOutputNodeCount(int count)
        {
            outputLayer = new OutputLayer(count, new SigmoidActivation(), new LeastSquaresError());
            return this;
        }

        public LSTMNetworkBuilder addFeedForwardLinkOrder(String[] linkOrder)
        {
            if (linkOrder == null || linkOrder.length < 1)
                throw new IllegalArgumentException("Link order cannot be null or empty");

            feedforwardLinkOrder = linkOrder;
            return this;
        }

        public LSTMNetworkBuilder addWeightUpdateOrder(String[] updateOrderSpec)
        {
            if (updateOrderSpec == null || updateOrderSpec.length < 1)
                throw new IllegalArgumentException("Update order cannot be null or empty");

            weightUpdateLinkOrder = updateOrderSpec;
            return this;
        }

        public LSTMNetworkBuilder addMemoryCell(String name, int stateSize)
        {
            memoryCellSpecList.add(Pair.of(name, stateSize));
            return this;
        }

        public LSTMNetworkBuilder addNodeConnections(String sourceName, String[] targerNodeNames)
        {
            ArrayList<String> targets = new ArrayList<String>();
            for (String s: targerNodeNames)
            {
                targets.add(s);
            }

            connectivityMap.put(sourceName, targets);
            return this;
        }

        public LSTMNetworkBuilder setWeightUpdateType(WeightUpdateType updateType)
        {
            this.updateType = updateType;
            return this;
        }

        public LSTMNetworkBuilder setNodeMap(HashMap<String, NodeGroup> initialNodeMap)
        {
            this.initialNodeMap = initialNodeMap;
            return this;
        }

        public LSTMNetworkBuilder setBiasLinkWeight(String linkSpecKey, double biasWeight)
        {
            biasSpecMap.put(linkSpecKey, biasWeight);
            return this;
        }

        public LSTMNetwork build()
        {
            LSTMNetwork lstm = new LSTMNetwork();

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

    OutputLayer outputLayer;
    InputLayer inputLayer;
    ArrayList<Pair<String, Integer>> memoryCellSpecList;
    String[] feedforwardLinkOrder;
    String[] weightUpdateLinkOrder;
    HashMap<String, ArrayList<String>> connectivityMap;
    WeightUpdateType updateType;
    HashMap<WeightUpdateParameters, Double> weightParameters;
    HashMap<String, NodeGroup> nodeMap;
    HashMap<String, Link> linkMap;
    HashMap<String, MemoryCell> memoryCells;
    HashMap<String, WeightMatrix> bufferedLinkWeightMap;
    HashMap<String, Double> biasSpecMap;
    NodeGroup bias;
    public static final String LINK_NODE_SEPARATOR = ":";
    double convergenceThresholdFraction = 0.001;
    boolean allowRerollingWeights = true;
    HashMap<String, Vector> initialNodeActivationMap = null;
    Link.RANDOMIZATION_SCHEME weightRerollScheme = Link.RANDOMIZATION_SCHEME.UNIFORM_RANGE;
    private static String SERIALIZED_WEIGHT_RECORD_DELIMITER = "+";
    private static String SERIALIZED_LINK_RECORD_DELIMITER = "|";

    private static String SERIALIZED_STATE_RECORD_DELIMITER = "+";
    private static String SERIALIZED_STATE_NODE_DATA_DELIMITER = "|";
    private boolean useZeroSuffixForSequenceLearningP = false;

    final VectorMapper roundingMapper = new VectorMapper() {
        @Override
        public double map(double v, int i)
        {
            if (v < 0.5)
                return 0;
            else
                return 1;
        }
    };

    boolean roundOutput = true;

    public enum WeightUpdateType
    {
        RPROP, DEFAULT
    }

    public enum WeightUpdateParameters
    {
        LEARNING_RATE,
        MOMENTUM,
        MIN_DELTA,
        MAX_DELTA,
        INITIAL_DELTA,
        N_MAX,
        N_MIN,
        CONVERGENCE_THRESHOLD,
        MAX_POSSIBLE_ERROR
    }

    protected LSTMNetwork()
    {
        nodeMap = new HashMap<String, NodeGroup>();
        linkMap = new HashMap<String, Link>();
        memoryCells = new HashMap<String, MemoryCell>();
        bias = new NodeGroup(1, new IdentityActivation(), new Vector(new double[]{1}));
        bufferedLinkWeightMap = new HashMap<String, WeightMatrix>();
        biasSpecMap = new HashMap<String, Double> ();
    }

    protected void initialize()
    {
        nodeMap.put("I", inputLayer);
        nodeMap.put("O", outputLayer);
        nodeMap.put("B", bias);

        for (Pair<String, Integer> cellSpec: memoryCellSpecList)
        {
            String name = cellSpec.getKey();
            int size = cellSpec.getRight();

            MemoryCell cell = new MemoryCell(size);
            memoryCells.put(name, cell);

            nodeMap.put(cell.getCellInput().getCellKey(name), cell.getCellInput());
            nodeMap.put(cell.getCellOutput().getCellKey(name), cell.getCellOutput());
            nodeMap.put(cell.getForgetGate().getCellKey(name), cell.getForgetGate());
            nodeMap.put(cell.getInputGate().getCellKey(name), cell.getInputGate());
            nodeMap.put(cell.getOutputGate().getCellKey(name), cell.getOutputGate());
            nodeMap.put(name + MemoryCell.NODE_GROUP_SEPARATOR + "P", cell.getPeepHole());
        }

        for (String sourceNodeName: connectivityMap.keySet())
        {
            ArrayList<String> targetNodeNames = connectivityMap.get(sourceNodeName);
            for (String targetNodeName:targetNodeNames)
            {
                String linkKey = getNodeLinkKey(sourceNodeName, targetNodeName);
                switch (updateType)
                {
                    case DEFAULT:
                    {
                        LSTMLink link = new LSTMLink(
                                nodeMap.get(sourceNodeName),
                                nodeMap.get(targetNodeName),
                                getWeights(sourceNodeName, targetNodeName),
                                weightParameters.get(WeightUpdateParameters.LEARNING_RATE),
                                weightParameters.get(WeightUpdateParameters.MOMENTUM));
                        linkMap.put(linkKey, link);
                        break;
                    }
                    case RPROP:
                    {
                        RPropLink link = new RPropLink(
                                nodeMap.get(sourceNodeName),
                                nodeMap.get(targetNodeName),
                                getWeights(sourceNodeName, targetNodeName),
                                weightParameters.get(WeightUpdateParameters.N_MAX),
                                weightParameters.get(WeightUpdateParameters.N_MIN),
                                weightParameters.get(WeightUpdateParameters.MAX_DELTA),
                                weightParameters.get(WeightUpdateParameters.MIN_DELTA),
                                weightParameters.get(WeightUpdateParameters.INITIAL_DELTA));
                        linkMap.put(linkKey, link);
                        break;
                    }
                }
            }

        }

        clearAllMemoryCells();

    }

    public static LSTMNetworkBuilder getBuilder()
    {
        return new LSTMNetworkBuilder();
    }

    void executeForwardPass(Vector input)
    {

        ((InputLayer)nodeMap.get("I")).setActivation(input);
        String commitNodeName;
        for (String linkSpec:feedforwardLinkOrder)
        {
            commitNodeName = getCommitLink(linkSpec);
            if (commitNodeName != null)
            {
                nodeMap.get(commitNodeName).pushNetInputs();
            }
            else
            {
                linkMap.get(linkSpec).feedforward();
            }

        }

    }

    double executeBackwardTrainingPass(Vector targetOutput)
    {
        OutputLayer output = (OutputLayer)nodeMap.get("O");
        output.setTargetOutput(targetOutput);
        for (String linkSpec:weightUpdateLinkOrder)
        {
            linkMap.get(linkSpec).calculatePartialGradient();
        }

        return output.getError();
    }

    void comitAllWeightUpdates()
    {
        for (String linkSpec:weightUpdateLinkOrder)
        {
            linkMap.get(linkSpec).updateWeights();
        }
    }


    ArrayList<Pair<Vector, Vector>> getSequenceTrainingSpec(Vector[] trainingList, Vector lastValue)
    {
        ArrayList<Pair<Vector, Vector>> trainingSpec = new ArrayList<Pair<Vector, Vector>>();

        Vector v = null;
        int i;
        for (i = 1;i<trainingList.length;i++)
        {
            v = trainingList[i];
            trainingSpec.add(Pair.of(trainingList[i - 1], trainingList[i]));
        }
        trainingSpec.add(Pair.of(v, lastValue));

        return trainingSpec;
    }

    ArrayList<Pair<Vector, Vector>> getSequenceTrainingSpec(Vector[] trainingList)
    {
        ArrayList<Pair<Vector, Vector>> trainingSpec = new ArrayList<Pair<Vector, Vector>>();

        Vector v = null;
        int i;
        for (i = 1;i<trainingList.length;i++)
        {
            v = trainingList[i];
            trainingSpec.add(Pair.of(trainingList[i - 1], v));
        }

        return trainingSpec;
    }

    ArrayList<Pair<Vector, Vector>> getSequenceClassTrainingSpec(Vector[] trainingList, int classId, int numClasses)
    {
        ArrayList<Pair<Vector, Vector>> trainingSpec = new ArrayList<Pair<Vector, Vector>>();

        Vector v = null;
        int i;

        double[] classLabel;

        for (i = 0;i<trainingList.length;i++)
        {
            v = trainingList[i];
            classLabel = new double[numClasses];
            classLabel[classId - 1] = 1;
            trainingSpec.add(Pair.of(v, new Vector(classLabel)));
        }

        return trainingSpec;
    }

    /**
     * Specify whether to round the activations of the output layer to 0 or 1.  This makes sense if the
     * output activations represent binary options, but less so if they are probability distributions (such as
     * if the output layer has a softmax activation function).  When true, the output layer activation will be
     * rounded after the errors from the unrounded output has flowed back.
     * @param enable
     */
    public void setRoundOutput(boolean enable)
    {
        roundOutput = enable;
    }

    /**
     * Specify whether to reroll the weights of all links of the network if the network detects that it is
     * failing to learn a pattern.  Set this to true when teaching the network the first pattern that will be
     * stored.  Set this to false in order to learn new patterns while retaining the previously learned patterns
     * @param enable
     */
    public void setAllowRerollingWeights(boolean enable)
    {
        allowRerollingWeights = enable;
    }

    /**
     * Main method for learning a pattern sequence.  This is a helper method for when there are the same
     * number of output nodes as input nodes and the structure of the network and purpose is to predict the
     * next input given a previous sequence of inputs.
     *
     * The trainList is learned from the context of either a newly initialized LSTM network or the
     * activations in initialNodeActivationMap
     * @param trainingList a sequence of training output activations.
     * @param maxSteps
     * @param acceptableError the maximum acceptable training error for all components of the training sequence
     * @return
     */
    public double[] learnSequence(Vector[] trainingList, double maxSteps, double acceptableError)
    {
        ArrayList<Pair<Vector, Vector>> trainingSpec = null;
        if (useZeroSuffixForSequenceLearningP)
            trainingSpec = getSequenceTrainingSpec(trainingList, new Vector(trainingList[0].dimen()));
        else
            trainingSpec = getSequenceTrainingSpec(trainingList);

        return learnInputOutputPairs(trainingSpec, maxSteps, acceptableError);

    }


    /**
     * General method for teaching the LSTM to learn a sequence of input-output pairs.  The sequence of
     * input-output pairs are learned from the context of either a newly created LSTM or the activations
     * defined in initialNodeActivationMap
     * @param trainingSpec a sequence of training output activations.
     * @param maxSteps
     * @param acceptableError the maximum acceptable training error for all components of the training sequence
     * @return
     */
    public double[] learnInputOutputPairs(ArrayList<Pair<Vector, Vector>> trainingSpec, double maxSteps, double acceptableError)
    {
        double[] errorList = new double[trainingSpec.size() + 2];


        int c, pc = 0;
        Pair<Vector, Vector> data;
        Vector trainingInput, expectedOutput;
        double convergenceThreshold = convergenceThresholdFraction, effectiveThreshold, minEffectiveThresholdFraction = 0.2, worstError = 0;
        boolean stretchThresholdP = false;
        if (weightParameters.get(WeightUpdateParameters.CONVERGENCE_THRESHOLD) != null)
        {
            convergenceThreshold = weightParameters.get(WeightUpdateParameters.CONVERGENCE_THRESHOLD);
        }


        if (weightParameters.get(WeightUpdateParameters.MAX_POSSIBLE_ERROR) != null)
        {
            stretchThresholdP = true;
            worstError = weightParameters.get(WeightUpdateParameters.MAX_POSSIBLE_ERROR);
        }

        double error = 0, prev = 0;
        for (c = 0;c < maxSteps;c++)
        {
            initializeNodeState();
            clearWeightHistory();
            error = -1;
            for (int t = 0;t < trainingSpec.size();t++)
            {
                data = trainingSpec.get(t);

                if (data == null) // temporary hack to allow multiple patterns
                // null data value is a delimiter between separate patterns that
                // require the lstm state be reset
                {
                    initializeNodeState();
                    clearWeightHistory();
                    continue;
                }
                trainingInput = data.getLeft();
                expectedOutput = data.getRight();
                executeForwardPass(trainingInput);

                errorList[2 + t] = executeBackwardTrainingPass(expectedOutput);
                error = Math.max(error, errorList[2 + t]);
            }

            if (error < acceptableError)
            {
                errorList[0] = error;
                errorList[1] = c;
                return errorList;
            }


            if (stretchThresholdP)
                effectiveThreshold = Math.min(minEffectiveThresholdFraction * convergenceThreshold, convergenceThreshold * (error - acceptableError)/worstError);
            else
                effectiveThreshold = convergenceThreshold;


            if (allowRerollingWeights && prev > 0 && Math.abs(prev - error)/prev < effectiveThreshold)
            {
                if (DEBUG)
                    System.out.println("Rerolling all weights after failure at: [" + (c - pc) + "] " + error);
                rerollAllWeights();
                pc = c;
                prev = 0;
            }
            else
            {

                comitAllWeightUpdates();
                prev = error;
            }

        }

        // TODO: Eventually replace this with LSTMLearningResult or something like it
        errorList[0] = error;
        errorList[1] = maxSteps;
        return errorList;
    }


    /**
     * Learn to associate sequences of inputs with categories.  This method will associate the input sequences
     * in trainingSpec to categories that are defined in a one-hot vector.  This implies that the number of nodes
     * in the output layer needs to be equal to the maximum number of possible categories
     * @param trainingSpec array of sequences of input patterns
     * @param classIdList the category to be assigned to each sequence in trainingSpec.  Category numbers start with
     *                    1 (as opposed to 0) and the maximum category in this list must be less than the number of
     *                    possible categories, maxNumClasses
     * @param maxNumClasses Maximum possible category for any pattern.  This should be equal to the number of nodes
     *                      in the output layer
     * @param maxSteps Maximum number of learning steps
     * @param acceptableError the maximum acceptable training error for all components of the training sequence
     * @return
     */
    public double[] learnPatternClass(Vector[][] trainingSpec, int[] classIdList, int maxNumClasses, double maxSteps, double acceptableError)
    {
        ArrayList<Pair<Vector, Vector>> trainingList = new ArrayList<Pair<Vector, Vector>>();
        ArrayList<Pair<Vector, Vector>> trainingPatternSequence = null;

        for (int i = 0;i<classIdList.length;i++)
        {
            if (i > 0)
                trainingList.add(null);
            trainingPatternSequence  = getSequenceClassTrainingSpec(trainingSpec[i], classIdList[i], maxNumClasses);
            trainingList.addAll(trainingPatternSequence);
        }

        return learnInputOutputPairs(trainingList, maxSteps, acceptableError);
    }


    /**
     * Generates the sequence of outputs for a corresponding input sequence.
     * @param initialSequence - this is an initial driving sequence, can be empty
     * @param extrapSteps - the number of steps to extrapolate
     * @param retainPreviousState - a flag indicating whether to initialize the activation of all
     *                            nodes of the network before extrapolating or to extrapolate from
     *                            the current state of the network
     * @return
     */
    public Vector[] extrapolate(Vector[] initialSequence, int extrapSteps, boolean retainPreviousState)
    {
        if (!retainPreviousState)
        {
            initializeNodeState();
            clearWeightHistory();
        }

        Vector[] extrapolated = new Vector[extrapSteps];
        Vector output;
        int k = 0;
        for (Vector driving: initialSequence)
        {
            executeForwardPass(driving);

            if (roundOutput)
            {
                output = getOutputValues();
                output.mapD(roundingMapper);
            }

        }

        if (extrapSteps < 1)
            return new Vector[0];

        extrapolated[k] = getOutputValues();

        for (k = 1;k<extrapSteps;k++)
        {
            executeForwardPass(getOutputValues());
            if (roundOutput)
            {
                output = getOutputValues();
                output.mapD(roundingMapper);
            }
            extrapolated[k] = getOutputValues();

        }
        return extrapolated;

    }

    /**
     * Given a sequence of input vectors, this method returns the corresponding sequence of outputs.
     * @param drivingSequence
     * @param retainPreviousState - flag indicating whether to reinitialize the activation of all nodes
     *                            (which will be the activations defined in initialNodeActivationMap if not
     *                            null) before driving the network with drivingSequence
     * @return
     */
    public Vector[] viewSequenceOutput(Vector[] drivingSequence, boolean retainPreviousState)
    {
        if (!retainPreviousState)
        {
            initializeNodeState();
            clearWeightHistory();
        }

        Vector[] extrapolated = new Vector[drivingSequence.length];
        Vector output;
        int k = 0;
        for (Vector driving: drivingSequence)
        {
            executeForwardPass(driving);
            output = getOutputValues();
            if (roundOutput)
            {

                output.mapD(roundingMapper);
            }

            extrapolated[k] = output;
            k++;
        }

        return extrapolated;

    }




    public Vector getOutputValues()
    {
        return nodeMap.get("O").getActivation();
    }

    // TODO: Think carefully if want to keep distinction between buffered link weights and active weights, the current distinction seems error-prone
    /**
     * Get the buffer of link weights.  Since
     * @return
     */
    public HashMap<String, WeightMatrix> getLinkWeightMap(boolean saveToBufferFirstP)
    {
        if (saveToBufferFirstP)
            saveAllLinkWeights();
        return bufferedLinkWeightMap;
    }

    /**
     * Get the buffer of link weights
     * @return
     */
    public LSTMNetwork setBufferedLinkWeightMap(HashMap<String, WeightMatrix> map)
    {
        bufferedLinkWeightMap = map;
        return this;
    }


    /**
     * Use this to extract the network activation state, mostly so that you can reuse it in the future
     * with setInitialNodeActivation or setNetworkActivation
     * @return
     */
    public HashMap<String, Vector> getNetworkActivationSnapshot()
    {
        HashMap<String, Vector> stateMap = new HashMap<String, Vector>();
        Vector copy;
        for (String nodename: nodeMap.keySet())
        {
            copy = nodeMap.get(nodename).getActivation().multiply(1);
            stateMap.put(nodename, copy);
        }
        return stateMap;
    }

    /**
     * Set the current network activation state.  This method is usually used to rollback the state
     * of the LSTMNetwork to some prior state, usually in conjunction with setBufferedLinkWeightMap
     * @param activationMap this should usually be the value returned by getNetworkActivationSnapshot
     * @return
     */
    public LSTMNetwork setNetworkActivation(HashMap<String, Vector> activationMap)
    {
        HashMap<String, Vector> stateMap = new HashMap<String, Vector>();

        for (String nodename: activationMap.keySet())
        {
            final Vector newActivation = activationMap.get(nodename);
            nodeMap.get(nodename).getActivation().mapD(new VectorMapper() {
                @Override
                public double map(double v, int i)
                {
                    return newActivation.value(i);
                }
            }) ;

        }
        return this;
    }


    /**
     * This overrides the initial node activation for all nodes of the network. When learning a new sequence pattern, such as
     * during learnSequence, this defines the activations that the network will be reset to at the beginning of the
     * pattern sequence
     * @param activationMap
     * @return
     */
    public LSTMNetwork setInitialNodeActivation(HashMap<String, Vector> activationMap)
    {
        initialNodeActivationMap = activationMap;
        return this;
    }

    /**
     * Instructs the network to use the default node initial activations when learning a sequence.
     * This removes the default activations that were set in the call to setInitialNodeActivation
     * @return
     */
    public LSTMNetwork useDefaultInitialNodeActivation()
    {
        initialNodeActivationMap = null;
        return this;
    }

    /**
     * Sets the initialization activation of the network to the current activation.

     * @return
     */
    public LSTMNetwork setInitialNodeActivationAsCurrent()
    {
        HashMap<String, Vector> activationMap = new HashMap<String, Vector>();
        for (String nodeKey:nodeMap.keySet())
        {
            activationMap.put(nodeKey,
                    nodeMap.get(nodeKey).getActivation().multiply(1));
        }
        initialNodeActivationMap = activationMap;
        return this;
    }


    /**
     * Gets a string serialization of this network.  This can be used as an input to decodeSerializedLinksToLinkBuffer and,
     * together with
     * @return
     */
    public String serializeLinkWeights()
    {
        saveAllLinkWeights();
        StringBuilder serialized = new StringBuilder();
        for (String key: bufferedLinkWeightMap.keySet())
        {
            if (serialized.length()>0)
                serialized.append(SERIALIZED_WEIGHT_RECORD_DELIMITER);
            serialized.append(key).append(SERIALIZED_LINK_RECORD_DELIMITER).append(bufferedLinkWeightMap.get(key).serialize());
        }

        return serialized.toString();
    }

    /**
     * Deserialization is currently a two step process.  First, use this method to decode a string serialization of
     * the weights of the links to bufferedLinkWeightMap, then use loadbufferedLinkWeights to update the
     * weights of each link in linkMap to the values defined in that buffer
     * @param serializedLinks
     */
    public void decodeSerializedLinksToLinkBuffer(String serializedLinks)
    {
        String[] weights = StringUtils.split(serializedLinks, SERIALIZED_WEIGHT_RECORD_DELIMITER);
        for (String weightSpec:weights)
        {
            String[] linkSpec = StringUtils.split(weightSpec, SERIALIZED_LINK_RECORD_DELIMITER);
            String linkKey = linkSpec[0];
            String weightData  = linkSpec[1];
            bufferedLinkWeightMap.put(linkKey, WeightMatrix.deserialize(weightData));
        }
    }

    /**
     * Sets the weights of all Links in accordance to the current Link weight buffer
     */
    public void loadbufferedLinkWeights()
    {
        for (String key: bufferedLinkWeightMap.keySet())
        {
            WeightMatrix old = bufferedLinkWeightMap.get(key);
            WeightMatrix newWeights = new WeightMatrix(old.rows(), old.cols());
            newWeights.addD(old);
            linkMap.get(key).setWeights(newWeights);
        }
    }


    /**
     * Get a string with the serialization node activation for all nodes is this network.  You can reconstruct the
     * network node state from this serialized form using the companion method, loadSerializedNetworkActivationState
     * @return
     */
    public String serializeNetworkActivationState()
    {
        StringBuilder sbuilder = new StringBuilder();
        NodeGroup group;
        for (String nodeKey:nodeMap.keySet())
        {
            group = nodeMap.get(nodeKey);
            if (sbuilder.length()>0)
            {
                sbuilder.append(SERIALIZED_STATE_RECORD_DELIMITER);
            }

            sbuilder.append(nodeKey).append(SERIALIZED_STATE_NODE_DATA_DELIMITER).append(group.getActivation().serialize());

        }

        return sbuilder.toString();
    }

    /**
     * Sets the activation of all nodes in accordance to the a serialization string, probably having
     * been generated from a call to serializeNetworkActivationState
     * @param state
     * @return
     */
    public LSTMNetwork loadSerializedNetworkActivationState(String state)
    {
        clearAllMemoryCells();
        String[] serializedKeyValues = StringUtils.split(state, SERIALIZED_STATE_RECORD_DELIMITER);
        String keyValue;
        String[] pairs;

        String nodeName, serializedActivationData;
        NodeGroup ngroup;
        for (int i = 0;i<serializedKeyValues.length;i++)
        {
            keyValue = serializedKeyValues[i];
            pairs = StringUtils.split(keyValue, SERIALIZED_STATE_NODE_DATA_DELIMITER);
            nodeName = pairs[0];
            serializedActivationData = pairs[1];
            ngroup = nodeMap.get(nodeName);
            ngroup.setActivation(Vector.fromSerialized(serializedActivationData));

        }
        return this;
    }

    /**
     * Gets the weights linking node soourceName to targetName
     * @param sourceName
     * @param targetName
     * @return
     */
    public WeightMatrix getWeights(String sourceName, String targetName)
    {
        String linkKey = getNodeLinkKey(sourceName, targetName);
        if (bufferedLinkWeightMap.containsKey(linkKey))
        {
            return bufferedLinkWeightMap.get(linkKey);
        }
        WeightMatrix matrix = null;
        int targetDimen = nodeMap.get(targetName).getDimen();
        if (biasSpecMap.containsKey(linkKey))
        {
            final double bias = biasSpecMap.get(linkKey);
            matrix = new WeightMatrix(targetDimen, 1);
            return matrix.mapD(new MatrixMapper() {
                @Override
                public double map(double c, int i, int j)
                {
                    return bias;
                }
            });
        }
        else
        {
            int sourceDimen = nodeMap.get(sourceName).getDimen();
            matrix = new WeightMatrix(targetDimen, sourceDimen);
            return matrix.mapD(new MatrixMapper() {
                @Override
                public double map(double c, int i, int j)
                {
                    return -1 + 2*Math.random();
                }
            });
        }

    }

    /**
     * Not using this since Link.RANDOMIZATION_SCHEME.PERTRUBATION doesn't work quite right yet
     * @param scheme
     * @return
     */
    LSTMNetwork setWeightRollStrategy(Link.RANDOMIZATION_SCHEME  scheme)
    {

        weightRerollScheme = scheme;
        return this;
    }



    private static String getCommitLink(String spec)
    {
        String[] parts = StringUtils.split(spec, LINK_NODE_SEPARATOR);
        if (parts[0].equals("*"))
            return parts[1];
        else
            return null;
    }

    void clearAllMemoryCells()
    {
        for (Map.Entry<String, MemoryCell> entry: memoryCells.entrySet())
        {
            entry.getValue().reset();
        }
    }

    void clearWeightHistory()
    {
        for (String key:linkMap.keySet())
        {
            linkMap.get(key).resetWeightHistory();

        }
    }

    private void saveAllLinkWeights()
    {

        for (String key:linkMap.keySet())
        {
            WeightMatrix old = linkMap.get(key).getWeights();
            WeightMatrix newWeights = new WeightMatrix(old.rows(), old.cols());
            newWeights.addD(old);
            bufferedLinkWeightMap.put(key, newWeights);
        }

    }

    private LSTMNetwork initializeNodeState()
    {
        clearAllMemoryCells();
        if (initialNodeActivationMap != null)
        {
            for (String nodeName: initialNodeActivationMap.keySet())
            {
                NodeGroup group = nodeMap.get(nodeName);
                group.setActivation(initialNodeActivationMap.get(nodeName));
            }
        }
        return this;
    }






    public static String getNodeLinkKey(String sourceName, String targetName)
    {
        return sourceName + LINK_NODE_SEPARATOR + targetName;

    }



    public void rerollAllWeights()
    {
        Link l;
        for (String name: weightUpdateLinkOrder)
        {
            l = linkMap.get(name);
            switch (weightRerollScheme)
            {
                case UNIFORM_RANGE:
                    l.initializeWeights(true);
                    break;
                case PERTRUBATION:
                    l.perturbWeights();
                    break;
            }

        }
    }

}
