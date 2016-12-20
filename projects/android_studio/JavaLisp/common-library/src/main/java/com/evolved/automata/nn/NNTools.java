package com.evolved.automata.nn;

import com.evolved.automata.AITools;
import com.evolved.automata.ArrayMapper;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by Evolved8 on 12/16/16.
 */
public class NNTools {
    private static final String[] toArraySampleValue = new String[0];

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



}
