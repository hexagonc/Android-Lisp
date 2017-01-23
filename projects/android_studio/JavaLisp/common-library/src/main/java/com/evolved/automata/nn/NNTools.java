package com.evolved.automata.nn;

import com.evolved.automata.AITools;
import com.evolved.automata.ArrayMapper;
import com.evolved.automata.IndexedValueMapper;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

/**
 * Created by Evolved8 on 12/16/16.
 */
public class NNTools {

    public static final String LSTM_DATA_SEPARATOR = "(*)";
    public static final String LSTM_WEIGHT_NODE_SEPARATOR = "(<>)";
    public static final String _VDELIMITER = "~.~";
    public static String toString(List<Vector> vlist)
    {
        return vArrayToString(vlist.toArray(new Vector[0]));
    }

    public static String vArrayToString(Vector[] vlist)
    {
        if (vlist == null)
            return "";
        StringBuilder s = new StringBuilder();
        for (Vector v:vlist)
        {
            if (s.length() > 0)
            {
                s.append(_VDELIMITER);
            }
            if (v!=null)
                s.append(v.serialize());
        }
        return s.toString();
    }

    public static Vector[] stringToVArray(String serialized)
    {
        if (serialized == null)
            return null;

        if (serialized.length() == 0)
            return new Vector[0];
        final String[] parts = StringUtils.splitByWholeSeparatorPreserveAllTokens(serialized, _VDELIMITER);
        return AITools.mapValues(parts, new IndexedValueMapper<String, Vector>() {
            @Override
            public Vector map(String input, int index)
            {
                if (input == null || input.length() == 0)
                    return null;
                else
                    return Vector.fromSerialized(input);
            }

            @Override
            public Vector[] getEmptyOutput()
            {
                return new Vector[0];
            }
        });
    }

    public static SequenceLSTM[] deSerializeData(final SequenceLSTM.LSTMNetworkBuilder builder,    String data)
    {

        return AITools.mapValues(StringUtils.splitByWholeSeparatorPreserveAllTokens(data, LSTM_DATA_SEPARATOR), new IndexedValueMapper<String, SequenceLSTM>() {
            @Override
            public SequenceLSTM map(String input, int index)
            {
                if (input != null && input.length()>0)
                {
                    SequenceLSTM lstm = builder.build();
                    String[] parts = StringUtils.splitByWholeSeparatorPreserveAllTokens(input, LSTM_WEIGHT_NODE_SEPARATOR);
                    String weightData = parts[0];
                    String activations  =parts[1];
                    String size = parts[2];


                    lstm.loadSerializedNetworkActivationState(activations);
                    lstm.decodeSerializedLinksToLinkBuffer(weightData);
                    lstm.loadbufferedLinkWeights();
                    lstm.setSequenceLength(Integer.parseInt(size));
                    return lstm;
                }
                else
                    return null;
            }

            @Override
            public SequenceLSTM[] getEmptyOutput()
            {
                return new SequenceLSTM[0];
            }
        });
    }

    public static String getSerializedSequenceData(SequenceLSTM[] slstms)
    {
        final StringBuilder s = new StringBuilder();

        AITools.map(slstms, new ArrayMapper<SequenceLSTM>() {
            @Override
            public SequenceLSTM map(SequenceLSTM input, int index)
            {
                if (index > 0)
                    s.append(LSTM_DATA_SEPARATOR);

                if (input != null)
                {
                    s.append(input.serializeLinkWeights());
                    s.append(LSTM_WEIGHT_NODE_SEPARATOR);
                    s.append(input.serializeNetworkActivationState());
                    s.append(LSTM_WEIGHT_NODE_SEPARATOR);
                    s.append(input.getSequenceLength());
                }


                return null;
            }
        });

        return s.toString();

    }


    private static final String[] toArraySampleValue = new String[0];

    /**
     * Returns the feedforward order when updating the links of a simple LSTM network with
     * one memory cell, peepholes and a forget gate.  This link order assumes
     * nearly complete connectivity of the gates.  I.e., all gates receive recurrent connections
     * to other gates as as well as from previous output.  Bias nodes can be prefixed to this list
     * @param memoryCellName
     * @return
     */
    public static String[] getStandardSingleCellFeedforwardOrderWithPeepholesAndForgetGates(String memoryCellName)
    {
        String[] feedforwardOrder = new String[]{
                "M-FG:M-OG",
                "M-FG:M-IG",
                "I:M-FG",
                "M-OG:M-FG",
                "M-CO:M-FG",
                "M-P:M-FG",
                "M-IG:M-FG",
                "*:M-FG",
                "M-CO:M-IG",
                "M-CO:M-OG",
                "M-OG:M-IG",
                "M-CO:M-CI",
                "M-OG:M-CI",
                "M-IG:M-OG",
                "M-IG:M-CI",
                "M-P:M-IG",
                "I:M-CI",
                "*:M-CI",
                "I:M-IG",
                "*:M-IG",
                "M-P:M-OG",
                "I:M-OG",
                "*:M-OG", "M-CO:O", "*:O"};
        if ("M".equals(memoryCellName))
            return feedforwardOrder;

        String[] updated = new String[feedforwardOrder.length];

        String replacementString = memoryCellName + "-";
        for (int i = 0;i<feedforwardOrder.length;i++)
        {
            updated[i] = StringUtils.replace(feedforwardOrder[i], "M-", replacementString);
        }
        return updated;
    }

    /**
     * Returns the feedforward order when updating the links of a simple LSTM network with
     * one memory cell, peepholes and an inactive forget gate.  This link order assumes
     * nearly complete connectivity of the gates.  I.e., all gates receive recurrent connections
     * to other gates as as well as from previous output.  Bias nodes can be prefixed to this list
     * @param memoryCellName
     * @return
     */
    public static String[] getStandardSingleCellFeedforwardOrderWithForgetGates(String memoryCellName)
    {
        String[] feedforwardOrder = new String[]{ "M-FG:M-OG", "M-FG:M-IG", "I:M-FG", "M-OG:M-FG", "M-CO:M-FG",  "M-IG:M-FG",  "*:M-FG", "M-CO:M-IG", "M-CO:M-OG", "M-OG:M-IG", "M-CO:M-CI", "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-CI", "*:M-CI", "I:M-IG", "*:M-IG",  "I:M-OG",  "*:M-OG", "M-CO:O", "*:O"};

        if ("M".equals(memoryCellName))
            return feedforwardOrder;

        String[] updated = new String[feedforwardOrder.length];

        String replacementString = memoryCellName + "-";
        for (int i = 0;i<feedforwardOrder.length;i++)
        {
            updated[i] = StringUtils.replace(feedforwardOrder[i], "M-", replacementString);
        }
        return updated;
    }

    /**
     * Returns the feedforward order when updating the links of a simple LSTM network with
     * one memory cell, peepholes and an inactive forget gate.  This link order assumes
     * nearly complete connectivity of the gates.  I.e., all gates receive recurrent connections
     * to other gates as as well as from previous output.  Bias nodes can be prefixed to this list
     * @param memoryCellName
     * @return
     */
    public static String[] getStandardSingleCellFeedforwardOrderWithPeepholes(String memoryCellName)
    {
        String[] feedforwardOrder = new String[]{"M-CO:M-IG", "M-CO:M-OG", "M-OG:M-IG", "M-CO:M-CI", "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-CI", "*:M-CI", "I:M-IG", "M-P:M-IG", "*:M-IG",  "I:M-OG", "M-P:M-OG",  "*:M-OG", "M-CO:O", "*:O"};
        if ("M".equals(memoryCellName))
            return feedforwardOrder;

        String[] updated = new String[feedforwardOrder.length];

        String replacementString = memoryCellName + "-";
        for (int i = 0;i<feedforwardOrder.length;i++)
        {
            updated[i] = StringUtils.replace(feedforwardOrder[i], "M-", replacementString);
        }
        return updated;
    }


    /**
     * Returns the feedforward order when updating the links of a simple LSTM network with
     * one memory cell, no peepholes and an inactive forget gate.  This link order assumes
     * nearly complete connectivity of the gates.  I.e., all gates receive recurrent connections
     * to other gates as as well as from previous output.  Bias nodes can be prefixed to this list
     * @param memoryCellName
     * @return
     */
    public static String[] getStandardSingleCellFeedforwardOrder(String memoryCellName)
    {
        String[] feedforwardOrder = new String[]{"M-CO:M-IG", "M-CO:M-OG", "M-OG:M-IG", "M-CO:M-CI", "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-CI", "*:M-CI", "I:M-IG", "*:M-IG",  "I:M-OG",  "*:M-OG", "M-CO:O", "*:O"};
        if ("M".equals(memoryCellName))
            return feedforwardOrder;

        String[] updated = new String[feedforwardOrder.length];

        String replacementString = memoryCellName + "-";
        for (int i = 0;i<feedforwardOrder.length;i++)
        {
            updated[i] = StringUtils.replace(feedforwardOrder[i], "M-", replacementString);
        }
        return updated;
    }

    public static LSTMNetwork.LSTMNetworkBuilder addStandardSimpleLSTMFeedforwardOrder(LSTMNetwork.LSTMNetworkBuilder builder)
    {
        return builder.addFeedForwardLinkOrder(getStandardSingleCellFeedforwardOrder("M"));
    }

    /**
     * Returns the order to update the weights of links assuming a single memory cell, no
     * peepholes and no (or disabled) forget gates.  Assumes nearly complete connectivity of the
     * hidden layer, i.e., all gates receive recurrent connections
     * to other gates as as well as from previous output.
     * @param memoryCellName
     * @return
     */
    public static String[] getStandardSingleCellWeightUpdateOrder(String memoryCellName)
    {
        String[] linkUpdateOrder= new String[]{"M-CO:O", "M-CO:M-IG", "M-CO:M-OG", "M-CO:M-CI", "M-OG:M-IG",  "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-IG", "I:M-OG", "I:M-CI"};
        if ("M".equals(memoryCellName))
            return linkUpdateOrder;

        String[] updated = new String[linkUpdateOrder.length];

        String replacementString = memoryCellName + "-";
        for (int i = 0;i<linkUpdateOrder.length;i++)
        {
            updated[i] = StringUtils.replace(linkUpdateOrder[i], "M-", replacementString);
        }
        return updated;
    }

    /**
     * Returns the order to update the weights of links assuming a single memory cell,
     * peepholes and no (or disabled) forget gates.  Assumes nearly complete connectivity of the
     * hidden layer, i.e., all gates receive recurrent connections
     * to other gates as as well as from previous output.
     * @param memoryCellName
     * @return
     */
    public static String[] getStandardSingleCellWeightUpdateOrderWithPeepholes(String memoryCellName)
    {
        String[] linkUpdateOrder= new String[]{"M-CO:O", "M-P:M-OG", "M-P:M-IG", "M-CO:M-IG", "M-CO:M-OG", "M-CO:M-CI", "M-OG:M-IG",  "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-IG", "I:M-OG", "I:M-CI"};
        if ("M".equals(memoryCellName))
            return linkUpdateOrder;

        String[] updated = new String[linkUpdateOrder.length];

        String replacementString = memoryCellName + "-";
        for (int i = 0;i<linkUpdateOrder.length;i++)
        {
            updated[i] = StringUtils.replace(linkUpdateOrder[i], "M-", replacementString);
        }
        return updated;
    }

    /**
     * Returns the order to update the weights of links assuming a single memory cell,
     * peepholes and forget gates.  Assumes nearly complete connectivity of the
     * hidden layer, i.e., all gates receive recurrent connections
     * to other gates as as well as from previous output.
     * @param memoryCellName
     * @return
     */
    public static String[] getStandardSingleCellWeightUpdateOrderWithPeepholesAndForgetGates(String memoryCellName)
    {
        String[] linkUpdateOrder= new String[]{"M-CO:O", "M-OG:M-FG", "M-IG:M-FG", "M-CO:M-FG", "M-FG:M-OG", "M-FG:M-IG", "I:M-FG",  "M-CO:M-IG", "M-CO:M-OG", "M-CO:M-CI", "M-OG:M-IG",  "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "M-P:M-FG", "M-P:M-IG", "M-P:M-OG", "I:M-IG", "I:M-OG", "I:M-CI"};
        if ("M".equals(memoryCellName))
            return linkUpdateOrder;

        String[] updated = new String[linkUpdateOrder.length];

        String replacementString = memoryCellName + "-";
        for (int i = 0;i<linkUpdateOrder.length;i++)
        {
            updated[i] = StringUtils.replace(linkUpdateOrder[i], "M-", replacementString);
        }
        return updated;
    }

    /**
     * Returns the order to update the weights of links assuming a single memory cell,
     * and forget gates.  Assumes nearly complete connectivity of the
     * hidden layer, i.e., all gates receive recurrent connections
     * to other gates as as well as from previous output.
     * @param memoryCellName
     * @return
     */
    public static String[] getStandardSingleCellWeightUpdateOrderWithForgetGates(String memoryCellName)
    {
        String[] linkUpdateOrder= new String[]{"M-CO:O", "M-OG:M-FG", "M-IG:M-FG", "M-CO:M-FG", "M-FG:M-OG", "M-FG:M-IG", "I:M-FG",  "M-CO:M-IG", "M-CO:M-OG", "M-CO:M-CI", "M-OG:M-IG",  "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-IG", "I:M-OG", "I:M-CI"};
        if ("M".equals(memoryCellName))
            return linkUpdateOrder;

        String[] updated = new String[linkUpdateOrder.length];

        String replacementString = memoryCellName + "-";
        for (int i = 0;i<linkUpdateOrder.length;i++)
        {
            updated[i] = StringUtils.replace(linkUpdateOrder[i], "M-", replacementString);
        }
        return updated;
    }


    public static LSTMNetwork.LSTMNetworkBuilder addStandardSimpleLSTMWeightUpdateOrder(LSTMNetwork.LSTMNetworkBuilder builder)
    {
        return builder.addWeightUpdateOrder(getStandardSingleCellWeightUpdateOrder("M"));
    }

    public static HashMap<String, ArrayList<String>> getStandardLinkConnectivityMap(String memoryCellName)
    {
        final String replacementString = memoryCellName + "-";
        HashMap<String, ArrayList<String>> out = new HashMap<String, ArrayList<String>>();
        ArrayList<String> component;
        // Add input node connections

        out.put("I", arrayListToArray(AITools.mapD(new String[]{"M-CI", "M-IG", "M-OG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));

        // add connections from output gate
        out.put("M-OG", arrayListToArray(AITools.mapD(new String[]{"M-CI", "M-IG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));

        // add connections from input gate
        out.put("M-IG", arrayListToArray(AITools.mapD(new String[]{"M-CI", "M-OG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));

        // add connections from cell output gate
        out.put("M-CO", arrayListToArray(AITools.mapD(new String[]{"O", "M-CI", "M-IG", "M-OG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));

        return out;

    }


    public static HashMap<String, ArrayList<String>> getStandardLinkConnectivityMapWithPeepholes(String memoryCellName)
    {
        final String replacementString = memoryCellName + "-";
        HashMap<String, ArrayList<String>> out = new HashMap<String, ArrayList<String>>();
        ArrayList<String> component;
        // Add input node connections

        out.put("I", arrayListToArray(AITools.mapD(new String[]{"M-CI", "M-IG", "M-OG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));

        // add connections from output gate
        out.put("M-OG", arrayListToArray(AITools.mapD(new String[]{"M-CI", "M-IG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));

        // add connections from input gate
        out.put("M-IG", arrayListToArray(AITools.mapD(new String[]{"M-CI", "M-OG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));

        // add connections from cell output gate
        out.put("M-CO", arrayListToArray(AITools.mapD(new String[]{"O", "M-CI", "M-IG", "M-OG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));

        // add connections from Peepholes
        out.put("M-P", arrayListToArray(AITools.mapD(new String[]{"M-IG", "M-OG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));


        return out;

    }

    public static HashMap<String, ArrayList<String>> getStandardLinkConnectivityMapWithPeepholesAndForgetGates(String memoryCellName)
    {
        final String replacementString = memoryCellName + "-";
        HashMap<String, ArrayList<String>> out = new HashMap<String, ArrayList<String>>();
        ArrayList<String> component;
        // Add input node connections

        out.put("I", arrayListToArray(AITools.mapD(new String[]{"M-CI", "M-IG", "M-OG", "M-FG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));

        // add connections from output gate
        out.put("M-FG", arrayListToArray(AITools.mapD(new String[]{"M-OG", "M-IG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));


        // add connections from output gate
        out.put("M-OG", arrayListToArray(AITools.mapD(new String[]{"M-CI", "M-IG", "M-FG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));

        // add connections from input gate
        out.put("M-IG", arrayListToArray(AITools.mapD(new String[]{"M-CI", "M-OG", "M-FG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));

        // add connections from cell output gate
        out.put("M-CO", arrayListToArray(AITools.mapD(new String[]{"O", "M-CI", "M-IG", "M-OG", "M-FG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));

        // add connections from Peepholes
        out.put("M-P", arrayListToArray(AITools.mapD(new String[]{"M-OG", "M-FG", "M-IG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));


        return out;

    }

    public static HashMap<String, ArrayList<String>> getStandardLinkConnectivityMapWithForgetGates(String memoryCellName)
    {
        final String replacementString = memoryCellName + "-";
        HashMap<String, ArrayList<String>> out = new HashMap<String, ArrayList<String>>();
        ArrayList<String> component;
        // Add input node connections

        out.put("I", arrayListToArray(AITools.mapD(new String[]{"M-CI", "M-IG", "M-OG", "M-FG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));

        // add connections from output gate
        out.put("M-FG", arrayListToArray(AITools.mapD(new String[]{"M-OG", "M-IG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));


        // add connections from output gate
        out.put("M-OG", arrayListToArray(AITools.mapD(new String[]{"M-CI", "M-IG", "M-FG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));

        // add connections from input gate
        out.put("M-IG", arrayListToArray(AITools.mapD(new String[]{"M-CI", "M-OG", "M-FG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));

        // add connections from cell output gate
        out.put("M-CO", arrayListToArray(AITools.mapD(new String[]{"O", "M-CI", "M-IG", "M-OG", "M-FG"}, new ArrayMapper<String>() {
            @Override
            public String map(String input, int index)
            {
                return StringUtils.replace(input, "M-", replacementString);
            }
        })));



        return out;

    }


    /**
     * Configures an LSTMNetworkBuilder with a given node connectivity graph, represented
     * as a hashtable, returning the same builder
     * @param builder
     * @param connectivityMap
     * @return
     */
    public static LSTMNetwork.LSTMNetworkBuilder addNodeConnectivity(LSTMNetwork.LSTMNetworkBuilder builder, HashMap<String, ArrayList<String>> connectivityMap)
    {
        for (String sourceNode:connectivityMap.keySet())
        {
            builder.addNodeConnections(sourceNode, arrayListToArray(connectivityMap.get(sourceNode)));
        }
        return builder;
    }

    /**
     * Adds the connectivity graph of a standard simple LSTM network.
     * @param builder
     * @return
     */
    public static LSTMNetwork.LSTMNetworkBuilder addStandardNodeConnections(LSTMNetwork.LSTMNetworkBuilder builder)
    {
       return addNodeConnectivity(builder, getStandardLinkConnectivityMap("M"));
    }


    public static LSTMNetwork.LSTMNetworkBuilder addStandardRPROPWeightUpdatePolicies(LSTMNetwork.LSTMNetworkBuilder lstmBuilder)
    {
        lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.INITIAL_DELTA, 0.012);
        lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MAX_DELTA, 50);
        lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MIN_DELTA, 0);
        lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MAX, 1.2);
        lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MIN, 0.5);
        lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.CONVERGENCE_THRESHOLD, 0.0001);
        lstmBuilder.setWeightUpdateType(LSTMNetwork.WeightUpdateType.RPROP);
        return lstmBuilder;
    }

    public static LSTMNetwork.LSTMNetworkBuilder addStandardGradientDescentWeightUpdatePolicies(LSTMNetwork.LSTMNetworkBuilder lstmBuilder)
    {
        lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.LEARNING_RATE, 0.5);
        lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MOMENTUM, 0.25);
        lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.CONVERGENCE_THRESHOLD, 0.0001);
        lstmBuilder.setWeightUpdateType(LSTMNetwork.WeightUpdateType.DEFAULT);
        return lstmBuilder;
    }

    /**
     * Returns a standard LSTM network suitable for most experimental and playful purposes.
     * This network contains a simple memory cell.  There will be nearly complete connectivity
     * between the gates.  The output error function is least squares.  The node activation
     * function is sigmoid for all but the input layer and cell output which uses the hypertangent
     * activation function.  None of peepholes, forget gates, or bias nodes are included in this
     * network.  This is a very basic starter network that supports sequence prediction and
     * classification.
     * @param numInputNodes
     * @param numMemoryCellStates
     * @param numOutputNodes
     * @return
     */
    public static LSTMNetwork getStandardLSTM(int numInputNodes, int numMemoryCellStates, int numOutputNodes)
    {
        return getStandardLSTM(numInputNodes, numMemoryCellStates, numOutputNodes, null);
    }


    /**
     * Returns a standard LSTM network suitable for most experimental and playful purposes.
     * This network contains a simple memory cell.  There will be nearly complete connectivity
     * between the gates.  The output error function is least squares.  The node activation
     * function is sigmoid for all but the input layer and cell output which uses the hypertangent
     * activation function.  None of peepholes, forget gates, or bias nodes are included in this
     * network.  This is a very basic starter network that supports sequence prediction and
     * classification.
     *
     * This version takes the string serialized weights to initialize the network with as an argument
     * @param numInputNodes
     * @param numMemoryCellStates
     * @param numOutputNodes
     * @param storedWeights
     * @return
     */
    public static LSTMNetwork getStandardLSTM(int numInputNodes, int numMemoryCellStates, int numOutputNodes, String storedWeights)
    {
        LSTMNetwork.LSTMNetworkBuilder lstmBuilder = LSTMNetwork.getBuilder();
        lstmBuilder.setInputNodeCount(numInputNodes).setOutputNodeCount(numOutputNodes).addMemoryCell("M", numMemoryCellStates);
        if (storedWeights !=null && storedWeights.length()>0)
        {
            lstmBuilder.setLinkWeightSerializedData(storedWeights);
        }
        addStandardNodeConnections(
                addStandardRPROPWeightUpdatePolicies(
                        addStandardSimpleLSTMWeightUpdateOrder(
                                addStandardSimpleLSTMFeedforwardOrder(lstmBuilder))));


        return lstmBuilder.build();
    }

    /**
     * Returns a standard LSTM network suitable for most experimental and playful purposes.
     * This network contains a simple memory cell.  There will be nearly complete connectivity
     * between the gates.  The output error function is least squares.  The node activation
     * function is sigmoid for all but the input layer and cell output which uses the hypertangent
     * activation function.  None of peepholes, forget gates, or bias nodes are included in this
     * network.  This is a very basic starter network that supports sequence prediction and
     * classification.  This LSTM will use the standard gradient descent weight update method
     *
     * This version takes the string serialized weights to initialize the network with as an argument
     * @param numInputNodes
     * @param numMemoryCellStates
     * @param numOutputNodes
     * @param storedWeights
     * @return
     */
    public static LSTMNetwork getStandardGDLSTM(int numInputNodes, int numMemoryCellStates, int numOutputNodes, String storedWeights)
    {
        LSTMNetwork.LSTMNetworkBuilder lstmBuilder = LSTMNetwork.getBuilder();
        lstmBuilder.setInputNodeCount(numInputNodes).setOutputNodeCount(numOutputNodes).addMemoryCell("M", numMemoryCellStates);
        if (storedWeights !=null && storedWeights.length()>0)
        {
            lstmBuilder.setLinkWeightSerializedData(storedWeights);
        }
        addStandardNodeConnections(
                addStandardGradientDescentWeightUpdatePolicies(
                        addStandardSimpleLSTMWeightUpdateOrder(
                                addStandardSimpleLSTMFeedforwardOrder(lstmBuilder))));


        return lstmBuilder.build();
    }

    /**
     * Returns a standard LSTM network suitable for most experimental and playful purposes.
     * This network contains a simple memory cell.  There will be nearly complete connectivity
     * between the gates.  The output error function is least squares.  The node activation
     * function is sigmoid for all but the input layer and cell output which uses the hypertangent
     * activation function.  None of peepholes, forget gates, or bias nodes are included in this
     * network.  This is a very basic starter network that supports sequence prediction and
     * classification.  This LSTM will use the standard gradient descent weight update method
     *
     * This version takes the string serialized weights to initialize the network with as an argument
     * @param numInputNodes
     * @param numMemoryCellStates
     * @param storedWeights
     * @return
     */
    public static SequenceLSTM getStandardSequenceLSTM(int numInputNodes, int numMemoryCellStates, String storedWeights)
    {
        SequenceLSTM.LSTMNetworkBuilder lstmBuilder = SequenceLSTM.getSequenceBuilder();
        lstmBuilder.setInputNodeCount(numInputNodes).addMemoryCell("M", numMemoryCellStates);
        if (storedWeights !=null && storedWeights.length()>0)
        {
            lstmBuilder.setLinkWeightSerializedData(storedWeights);
        }
        addStandardNodeConnections(
                addStandardRPROPWeightUpdatePolicies(
                        addStandardSimpleLSTMWeightUpdateOrder(
                                addStandardSimpleLSTMFeedforwardOrder(lstmBuilder))));


        return (SequenceLSTM)lstmBuilder.build();
    }


    /**
     * Returns a standard LSTM network suitable for most experimental and playful purposes.
     * This network contains a simple memory cell.  There will be nearly complete connectivity
     * between the gates.  The output error function is least squares.  The node activation
     * function is sigmoid for all but the input layer and cell output which uses the hypertangent
     * activation function.  None of peepholes, forget gates, or bias nodes are included in this
     * network.  This is a very basic starter network that supports sequence prediction and
     * classification.  This LSTM will use the standard gradient descent weight update method
     *
     * This version takes the string serialized weights to initialize the network with as an argument
     * @param numInputNodes
     * @param numMemoryCellStates
     * @param storedWeights
     * @return
     */
    public static SequenceLSTM.LSTMNetworkBuilder getStandardSequenceLSTMBuilder(int numInputNodes, int numMemoryCellStates, String storedWeights)
    {
        SequenceLSTM.LSTMNetworkBuilder lstmBuilder = SequenceLSTM.getSequenceBuilder();
        lstmBuilder.setInputNodeCount(numInputNodes).addMemoryCell("M", numMemoryCellStates);
        if (storedWeights !=null && storedWeights.length()>0)
        {
            lstmBuilder.setLinkWeightSerializedData(storedWeights);
        }
        addStandardNodeConnections(
                addStandardRPROPWeightUpdatePolicies(
                        addStandardSimpleLSTMWeightUpdateOrder(
                                addStandardSimpleLSTMFeedforwardOrder(lstmBuilder))));


        return lstmBuilder;
    }

    /**
     * Returns a standard LSTM network suitable for classification.
     * This network contains a simple memory cell.  There will be nearly complete connectivity
     * between the gates.  The output error function is cross entropy.  The output activation
     * function is softmax.  None of peepholes, forget gates, or bias nodes are included in this
     * network.

     * @param numInputNodes
     * @param numMemoryCellStates
     * @param numOutputNodes The output layer uses the softmax activation function and the
     *                       cross entropy error function.  The number of output nodes is
     *                       interpreted as the number of categories.  Training outputs
     *                       are expected to be one-hot category vectors.
     * @return
     */
    public static LSTMNetwork getStandardClassificationLSTM(int numInputNodes, int numMemoryCellStates, int numOutputNodes)
    {
        return getStandardLSTM(numInputNodes, numMemoryCellStates, numOutputNodes, null);
    }


    /**
     * Returns a standard LSTM network suitable for classification.
     * This network contains a simple memory cell.  There will be nearly complete connectivity
     * between the gates.  The output error function is cross entropy.  The output activation
     * function is softmax.  None of peepholes, forget gates, or bias nodes are included in this
     * network.
     *
     * This version takes the string serialized weights to initialize the network with as an argument
     * @param numInputNodes
     * @param numMemoryCellStates
     * @param numOutputNodes The output layer uses the softmax activation function and the
     *                       cross entropy error function.  The number of output nodes is
     *                       interpreted as the number of categories.  Training outputs
     *                       are expected to be one-hot category vectors.
     * @param storedWeights
     * @return
     */
    public static LSTMNetwork getStandardClassificationLSTM(int numInputNodes, int numMemoryCellStates, int numOutputNodes, String storedWeights)
    {
        LSTMNetwork.LSTMNetworkBuilder lstmBuilder = LSTMNetwork.getBuilder();
        lstmBuilder.setInputNodeCount(numInputNodes).addMemoryCell("M", numMemoryCellStates);

        OutputLayer output = new OutputLayer(numOutputNodes, new SoftmaxActivation(), new CrossEntropyError());
        lstmBuilder.setOutputLayer(output);
        if (storedWeights !=null && storedWeights.length()>0)
        {
            lstmBuilder.setLinkWeightSerializedData(storedWeights);
        }
        addStandardNodeConnections(
                addStandardRPROPWeightUpdatePolicies(
                        addStandardSimpleLSTMWeightUpdateOrder(
                                addStandardSimpleLSTMFeedforwardOrder(lstmBuilder))));


        return lstmBuilder.build();
    }


    public static ArrayList<Double> stageDiscretize(double value, double range, int bits )
    {
        return stageDiscretizeAroundPivot(value, 0, range, bits);
    }

    public static ArrayList<Double> stageDiscretizeAroundPivot(double value, double min, double max, int steps)
    {
        double pivot = (max + min)/2;
        boolean leq_pivot = value <= pivot;
        double range = max - min;
        boolean closer_than_quarter_range = (Math.abs(value - pivot) < range/4);
        ArrayList<Double> base = new ArrayList<Double>();
        if (leq_pivot)
        {
            base.add(0.0);
        }
        else
            base.add(1.0);

        if (closer_than_quarter_range)
        {
            base.add(1.0);
        }
        else
            base.add(0.0);

        if (steps > 1)
        {
            base.addAll(stageDiscretizeAroundPivot(
                    value,
                    (leq_pivot)?((closer_than_quarter_range)?(pivot - range/4):min):( (closer_than_quarter_range)?pivot:pivot + range/4),
                    (leq_pivot)?((closer_than_quarter_range)?pivot:pivot - range/4):( (closer_than_quarter_range)?pivot + range/4:max),
                    steps - 1));
            return base;
        }
        else
            return base;

    }

    public static double  averagedStageContinuize(double range, double[] discretized)
    {
        ArrayList<Double> in = new ArrayList<Double>();
        for (double d:discretized)
        {
            in.add(d);
        }
        return getAverage(stageContinuize(0, range, in));
    }

    public static Pair<Double, Double>  stageContinuize(double range, double[] discretized)
    {
        ArrayList<Double> in = new ArrayList<Double>();
        for (double d:discretized)
        {
            in.add(d);
        }
        return stageContinuize(0, range, in);
    }


    public static Pair<Double, Double>  stageContinuize(double range, ArrayList<Double> discretized)
    {
        return stageContinuize(0, range, discretized);
    }

    public static Pair<Double, Double> stageContinuize(double min, double max, ArrayList<Double> discretized)
    {
        boolean greater_than_midpoint = false;
        double width = max - min;;
        double v_i;
        int i = 0;
        boolean flag;
        double tmin;
        for (i = 0;i< discretized.size();i++)
        {
            v_i = discretized.get(i);
            flag = v_i > 0.5;
            if ( i % 2 == 0)
            {
                greater_than_midpoint = flag;
                width = max - min;
            }
            else if (greater_than_midpoint)
            {
                if (flag)
                {
                    tmin = (max - width/2);
                    max = min + width/2 + width/4;
                    min = tmin;
                }
                else
                {
                    min = max - width/4;
                }
            }
            else
            {
                if (flag)
                {
                    tmin = (min + width/4);
                    max = min + width/2;
                    min = tmin;
                }
                else
                {
                    max = min + width/4;
                }
            }

        }

        return Pair.of(min, max);
    }


    private static String[] arrayListToArray(ArrayList<String> input)
    {
        return input.toArray(toArraySampleValue);
    }

    private static ArrayList<String> arrayListToArray(String[] input)
    {
        ArrayList<String> out = new ArrayList<String>();
        for (String s:input)
        {
            out.add(s);
        }
        return out;
    }

    private static double[] conv(Double[] d)
    {
        double[] o = new double[d.length];
        for (int i = 0;i<o.length;i++)
            o[i] = d[i];
        return o;
    }


    static Vector[]  getVector(double[] data)
    {
        Vector[] out = new Vector[data.length];
        for (int i = 0;i < data.length; i++)
        {
            out[i] = new Vector(new double[]{data[i]});
        }
        return out;
    }

    static Vector getVector(ArrayList<Double> data)
    {
        double[] out = new double[data.size()];
        for (int i = 0;i < data.size(); i++)
        {
            out[i] = data.get(i);
        }
        return new Vector(out);
    }

    public static int[] getVectorDataAsInt(Vector vector)
    {
        double[] v = vector.raw();
        int[] o = new int[v.length];
        for (int i = 0;i < v.length;i++)
            o[i] = (int)v[i];
        return o;
    }

    static Vector[] getVector(double[][] data)
    {
        Vector[] out = new Vector[data.length];
        for (int i = 0;i < data.length; i++)
        {
            out[i] = new Vector(data[i]);
        }
        return out;
    }

    public static Vector padEnd(final Vector input, int padding, final int value)
    {
        Vector actualValue = new Vector(input.dimen() + padding);
        final int width = input.dimen();
        actualValue.mapD(new VectorMapper() {
            @Override
            public double map(double v, int i)
            {
                if (i < width)
                    return input.value(i);
                else
                    return value;
            }
        });
        return actualValue;


    }


    public static Vector padStart(final Vector input, final int padding, final int value)
    {
        Vector actualValue = new Vector(input.dimen() + padding);
        actualValue.mapD(new VectorMapper() {
            @Override
            public double map(double v, int i)
            {
                if (i < padding)
                    return value;
                else
                    return input.value(i - padding);
            }
        });
        return actualValue;


    }

    public static Vector[] padStart(Vector[] input, final int padding, final int value)
    {
        return AITools.mapValues(input, new IndexedValueMapper<Vector, Vector>()
        {

            @Override
            public Vector map(final Vector input, int index)
            {
                Vector actualValue = new Vector(input.dimen() + padding);
                actualValue.mapD(new VectorMapper() {
                    @Override
                    public double map(double v, int i)
                    {
                        if (i < padding)
                            return value;
                        else
                            return input.value(i - padding);
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
     * Non-destructive
     * @param v
     * @param startAmount
     * @return
     */
    public static Vector getTrimmedVector(Vector v, int startAmount)
    {
        return new Vector(Arrays.copyOfRange(v.raw(), startAmount, v.dimen()));
    }

    /**
     * Convert to standard binary little-endian
     * @param integer
     * @param numBits
     * @return
     */
    public static Vector intToStandardBinaryVectorLE(int integer, int numBits)
    {
        Vector out = new Vector(numBits);
        for (int i = 0;i<numBits;i++)
        {
            if (integer % 2 == 0)
            {
                out.setValue(0, i);
                integer = integer/2;
            }
            else
            {
                out.setValue(1, i);
                integer = (integer - 1)/2;
            }

        }
        return out;
    }

    /**
     * Produces a [2 * numStages] binary vector representation of num.  Num should be between
     * 0 and range.  This allows us to approximate any positive integer by a binary vector of
     * fixed number number of bits.
     * @param num
     * @param range - max value
     * @param numStages
     * @return
     */
    Vector discretizeInStages(int num, int range, int numStages)
    {
        return NNTools.getVector(NNTools.stageDiscretize(num, range, numStages));
    }


    public static Vector headVector(final Vector vec, final int dimen)
    {
        return (new Vector(dimen)).mapD(new VectorMapper() {
            @Override
            public double map(double v, int i)
            {

                return vec.value(i);
            }
        });
    }

    public static Vector tailVector(final Vector vec, final int start)
    {
        return (new Vector(vec.dimen() - start)).mapD(new VectorMapper() {
            @Override
            public double map(double v, int i)
            {

                return vec.value(i + start);
            }
        });
    }

    public static int binaryVectorToInteger(Vector vec)
    {
        int out = 0, l = vec.dimen();
        for (int i=0;i<l;i++)
        {
            out+=vec.value(i)*Math.pow(2, i);
        }
        return out;
    }

    public static Vector joinVectors(final Vector firstSegment, final Vector secondSegment)
    {
        final int fLength = firstSegment.dimen();
        final int sLength = secondSegment.dimen();

        int tLength = fLength + sLength;
        return new Vector(tLength).mapD(new VectorMapper() {
            @Override
            public double map(double v, int i)
            {
                if (i >= fLength)
                    return secondSegment.value(i - fLength);
                else
                    return firstSegment.value(i);
            }
        });
    }

    public static Vector addNoiseToVector(Vector v, int numBitsToggle)
    {
        Vector out = new Vector(v.raw());
        double[] raw = out.raw();
        int l = raw.length;
        int flipIndex = 0;
        int[] bitList = AITools.getRandomSubset(l, numBitsToggle);


        for (int i = 0; i < bitList.length;i++)
        {
            flipIndex = bitList[i];
            out.setValue(((out.value(flipIndex) + 1) % 2), flipIndex );

        }

        return out;
    }

    public static Vector swapD(Vector v, int i, int j)
    {
        double t = v.value(i);
        v.setValue(v.value(j), i);
        v.setValue(t, j);
        return v;
    }

    public static double getNumericValueOutput(Vector output, final int range)
    {
        return getAverage(NNTools.stageContinuize(range, output.raw()));

    }

    public static Double[] getNumericValueOutput(Vector[] output, final int range)
    {
        return getNumericValueOutput(output, range, null);
    }


    public static Double[] getNumericValueOutput(Vector[] output, final int range, final Double nullValue)
    {
        return AITools.mapValues(output, new IndexedValueMapper<Vector, Double>() {
            @Override
            public Double map(Vector input, int index)
            {
                if (input == null)
                    return nullValue;
                ArrayList<Double> v = new ArrayList<Double>();
                for (double d : input.raw())
                {
                    v.add(d);
                }

                Pair<Double, Double> out = NNTools.stageContinuize(range, v);
                return (out.getLeft() + out.getRight()) / 2;
            }

            @Override
            public Double[] getEmptyOutput()
            {
                return new Double[0];
            }
        });
    }


    public static Double[] getNumericSequenceOutput(SequenceLSTM lstm, final int range)
    {
        Vector[] output = lstm.getCurrentSequence();
        return AITools.mapValues(output, new IndexedValueMapper<Vector, Double>() {
            @Override
            public Double map(Vector input, int index)
            {
                ArrayList<Double> v = new ArrayList<Double>();
                for (double d : input.raw())
                {
                    v.add(d);
                }

                Pair<Double, Double> out = NNTools.stageContinuize(range, v);
                return (out.getLeft() + out.getRight()) / 2;
            }

            @Override
            public Double[] getEmptyOutput()
            {
                return new Double[0];
            }
        });
    }

    static double getAverage(Pair<Double, Double> p)
    {
        return p.getLeft()/2 + p.getRight()/2;
    }
}
