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



    public static final class LSTMNetworkBuilder {

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
                lstm.decodeSerializedLinks(linkWeightData);
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
    HashMap<String, WeightMatrix> linkData;
    HashMap<String, Double> biasSpecMap;
    NodeGroup bias;
    public static final String LINK_NODE_SEPARATOR = ":";
    double convergenceThresholdFraction = 0.001;
    double maxPossibleError = 0.5; // for a sum squared error output error function
    boolean stretchThresholdP = false;
    Link.RANDOMIZATION_SCHEME weightRerollScheme = Link.RANDOMIZATION_SCHEME.UNIFORM_RANGE;
    private static String SERIALIZED_WEIGHT_RECORD_DELIMITER = "+";
    private static String SERIALIZED_LINK_RECORD_DELIMITER = "|";

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

    private LSTMNetwork()
    {
        nodeMap = new HashMap<String, NodeGroup>();
        linkMap = new HashMap<String, Link>();
        memoryCells = new HashMap<String, MemoryCell>();
        bias = new NodeGroup(1, new IdentityActivation(), new Vector(new double[]{1}));
        linkData = new HashMap<String, WeightMatrix>();
        biasSpecMap = new HashMap<String, Double> ();
    }

    private void initialize()
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

    public void setRoundOutput(boolean enable)
    {
        roundOutput = enable;
    }

    public double[] learnSequence(Vector[] trainingList, double maxSteps, double acceptableError)
    {
        ArrayList<Pair<Vector, Vector>> trainingSpec = getSequenceTrainingSpec(trainingList, new Vector(trainingList[0].dimen()));

        return learnInputOutputPairs(trainingSpec, maxSteps, acceptableError);

    }


    public double[] learnInputOutputPairs(ArrayList<Pair<Vector, Vector>> trainingSpec, double maxSteps, double acceptableError)
    {
        double[] errorList = new double[trainingSpec.size() + 2];


        int c, pc = 0;
        Pair<Vector, Vector> data;
        Vector trainingInput, expectedOutput;
        double convergenceThreshold = convergenceThresholdFraction, effectiveThreshold, minEffectiveThresholdFraction = 0.2, worstError = maxPossibleError;

        if (weightParameters.get(WeightUpdateParameters.CONVERGENCE_THRESHOLD) != null)
        {
            convergenceThreshold = weightParameters.get(WeightUpdateParameters.CONVERGENCE_THRESHOLD);
        }


        if (weightParameters.get(WeightUpdateParameters.MAX_POSSIBLE_ERROR) != null)
        {
            worstError = weightParameters.get(WeightUpdateParameters.MAX_POSSIBLE_ERROR);
        }

        double error = 0, prev = 0;
        for (c = 0;c < maxSteps;c++)
        {

            error = -1;
            for (int t = 0;t < trainingSpec.size();t++)
            {
                data = trainingSpec.get(t);

                if (data == null) // temporary hack to allow multiple patterns
                // null data value is a delimiter between separate patterns that
                // require the lstm state be reset
                {
                    clearAllMemoryCells();
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

            if (prev > 0 && Math.abs(prev - error)/prev < effectiveThreshold)
            {
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

        errorList[0] = error;
        errorList[1] = maxSteps;
        return errorList;
    }


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

    public LSTMNetwork setWeightRollStrategy(Link.RANDOMIZATION_SCHEME scheme)
    {
        weightRerollScheme = scheme;
        return this;
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

    public Vector[] extrapolate(Vector[] initialSequence, int extrapSteps, boolean retainPreviousState)
    {
        if (!retainPreviousState)
        {
            clearAllMemoryCells();
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

    public Vector[] viewOutput(Vector[] initialSequence, boolean retainPreviousState)
    {
        if (!retainPreviousState)
        {
            clearAllMemoryCells();
            clearWeightHistory();
        }

        Vector[] extrapolated = new Vector[initialSequence.length];
        Vector output;
        int k = 0;
        for (Vector driving: initialSequence)
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


    private static String getCommitLink(String spec)
    {
        String[] parts = StringUtils.split(spec, LINK_NODE_SEPARATOR);
        if (parts[0].equals("*"))
            return parts[1];
        else
            return null;
    }

    public void clearAllMemoryCells()
    {
        for (Map.Entry<String, MemoryCell> entry: memoryCells.entrySet())
        {
            entry.getValue().reset();
        }
    }

    public void clearWeightHistory()
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
            linkData.put(key, newWeights);
        }

    }

    public Vector getOutputValues()
    {
        return nodeMap.get("O").getActivation();
    }

    public void loadLinkData()
    {
        for (String key:linkMap.keySet())
        {
            WeightMatrix old = linkData.get(key);
            WeightMatrix newWeights = new WeightMatrix(old.rows(), old.cols());
            newWeights.addD(old);
            linkMap.get(key).setWeights(newWeights);
        }
    }

    public HashMap<String, WeightMatrix> getLinkData()
    {
        return linkData;
    }



    public String serializeLinkData()
    {
        saveAllLinkWeights();
        StringBuilder serialized = new StringBuilder();
        for (String key:linkData.keySet())
        {
            if (serialized.length()>0)
                serialized.append(SERIALIZED_WEIGHT_RECORD_DELIMITER);
            serialized.append(key).append(SERIALIZED_LINK_RECORD_DELIMITER).append(linkData.get(key).serialize());
        }

        return serialized.toString();
    }

    public void decodeSerializedLinks(String serializedLinks)
    {
        String[] weights = StringUtils.split(serializedLinks, SERIALIZED_WEIGHT_RECORD_DELIMITER);
        for (String weightSpec:weights)
        {
            String[] linkSpec = StringUtils.split(weightSpec, SERIALIZED_LINK_RECORD_DELIMITER);
            String linkKey = linkSpec[0];
            String weightData  = linkSpec[1];
            linkData.put(linkKey, WeightMatrix.deserialize(weightData));
        }
    }



    public static String getNodeLinkKey(String sourceName, String targetName)
    {
        return sourceName + LINK_NODE_SEPARATOR + targetName;

    }

    public WeightMatrix getWeights(String sourceName, String targetName)
    {
        String linkKey = getNodeLinkKey(sourceName, targetName);
        if (linkData.containsKey(linkKey))
        {
            return linkData.get(linkKey);
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


}
