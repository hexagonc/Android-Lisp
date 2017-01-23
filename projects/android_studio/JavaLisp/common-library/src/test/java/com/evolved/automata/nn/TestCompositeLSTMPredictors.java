package com.evolved.automata.nn;

import com.evolved.automata.AITools;
import com.evolved.automata.IndexedValueMapper;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 1/5/17.
 */
public class TestCompositeLSTMPredictors {


    @Test
    public void testHashtableLSTMRecognition()
    {

        String errorMessage = "failure saving patterns";
        HashtableLSTM.LSTMNetworkBuilder builder = HashtableLSTM.getHashtableLSTMBuilder();
        builder.setInputNodeCount(8).addMemoryCell("M", 15);
        builder.setKeyBitWidth(4);

        NNTools.addStandardRPROPWeightUpdatePolicies(builder);
        builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrder("M"));
        builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrder("M"));
        NNTools.addNodeConnectivity(builder, NNTools.getStandardLinkConnectivityMap("M"));


        int numErrorBits = 2, numErrorVectors = 2;

        testLSTM("TEsting hastable lstm", builder);



    }

    private void testLSTM(String message, HashtableLSTM.LSTMNetworkBuilder builder)
    {
        System.out.println("****************************************");
        System.out.println(message);
        System.out.println("****************************************");
        double[] inputs = {59, 55, 117, 255, 255, 42, 84, 43, 255, 45, 39, 42, 255, 55, 57, 255,
                43, 42, 109, 70, 42, 115, 43, 43, 255, 255, 44, 46, 255, 255, 85, 255, 43, 44, 87, 255, 88, 120, 44,
                43, 98, 84, 85, 255, 45, 43, 255, 66, 48, 255, 255, 66, 43, 255, 48, 255, 255, 66, 44, 255, 70, 255, 255,
                66, 45, 46, 255, 255, 255, 255, 57, 48,59, 55, 117, 255, 255, 42, 84, 43, 255, 45, 39, 42, 255, 55, 57, 255,
                43, 42, 109, 70, 42, 115, 43, 43, 39, 93, 255, 41, 50, 54, 40, 71, 255, 41, 51, 255, 39, 69, 255, 42,
                50, 50, 39, 70, 255, 43, 50, 51};
        boolean success = false;

        SequenceLSTM.LSTMNetworkBuilder compBuilder = SequenceLSTM.getSequenceBuilder();
        compBuilder.setEndCapPolicy(SequenceLSTM.EndCapPolicy.BOTH_ENDS);
        compBuilder.setInputNodeCount(8).addMemoryCell("M", 15);
        compBuilder.setCapDisplayPolicy(SequenceLSTM.EndCapDisplayPolicy.HIDE);

        NNTools.addStandardRPROPWeightUpdatePolicies(compBuilder);
        compBuilder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrderWithForgetGates("M"));
        compBuilder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrderWithForgetGates("M"));
        NNTools.addNodeConnectivity(compBuilder, NNTools.getStandardLinkConnectivityMapWithForgetGates("M"));

        SequenceLSTM current = compBuilder.build();
        double[] learningResult=null;
        int thresholdCount = 200;
        double errorThreshold = 0.1;
        double value;
        Vector[] saved;
        int averageSteps = 0;
        try
        {
            LinkedList<SequenceLSTM> output = new LinkedList<>();
            LinkedList<Integer> lengths = new LinkedList<>();
            LinkedList sequences = new LinkedList();

            for (int i = 0;i<inputs.length;i++)
            {

                value = inputs[i];
                learningResult = current.add(NNTools.getVector(NNTools.stageDiscretize(value, 255, 4)), thresholdCount, errorThreshold);

                if (learningResult[0] > errorThreshold)
                {
                    saved = current.getCurrentSequence();
                    sequences.add(saved);
                    lengths.add(saved.length);
                    System.out.println("After [" + (int)learningResult[1] + "] steps, saved ("+ saved.length + ")" + Arrays.toString(getNumericValueOutput(saved, 255)));
                    output.add(current);
                    current = compBuilder.build();
                }

            }



            if (current.getSequenceLength()>0)
            {
                saved = current.getCurrentSequence();
                sequences.add(saved);
                lengths.add(saved.length);
                System.out.println("After [" + (int)learningResult[1] + "] steps, saved ("+ saved.length + ")" + Arrays.toString(getNumericValueOutput(saved, 255)));
                output.add(current);
            }
            System.out.println("Partitioned original input into " + sequences.size() + " parts ");

            HashtableLSTM hlstm = builder.build();

            Vector[][] items = new Vector[sequences.size()][];

            int[] samples = AITools.getRandomSubset(sequences.size(), 3);
            int[] keys = new int[]{1,2,4};

            for (int k = 0;k<sequences.size();k++)
            {
                items[k]= (Vector[])sequences.get(k);
            }

            Vector[] currentItem, out;

            int key;
            errorThreshold = 0.1;
            for (int j=0;j<3;j++)
            {
                key = keys[j];
                currentItem = items[samples[j]];
                if (currentItem == null)
                    continue;
                System.out.println("Trying to map " + Arrays.toString(getNumericValueOutput(currentItem, 255)) + " to key " + key );
                for (int i = 0;i < currentItem.length;i++)
                {
                    System.out.println("Trying to save: " + NNTools.getNumericValueOutput(currentItem[i], 255));
                    learningResult = hlstm.add(currentItem[i], key, 2*thresholdCount, errorThreshold);
                    if (learningResult[0] <= errorThreshold)
                        System.out.println("After [" + (int)learningResult[1] + "] steps, added " + NNTools.getNumericValueOutput(currentItem[i], 255) );
                    else
                    {
                        out = hlstm.get(key);
                        if (out!=null)
                            System.out.println("Finishing saving data as: " + Arrays.toString(getNumericValueOutput(out, 255)));
                        else
                            System.out.println("Failed to save any data to key " + key);
                        break;
                    }
                }
            }

            System.out.println("Added " + hlstm.size() + " items");

            if (hlstm.size()>0)
            {
                for (Integer Key: hlstm.getKeys())
                {
                    currentItem = hlstm.get(Key.intValue());
                    if (currentItem != null)
                    {
                        System.out.println("Found key "+ Key + " with value: " + Arrays.toString(getNumericValueOutput(currentItem, 255)));
                    }
                }
            }




            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();;
        }
    }

    private static Double[] getNumericValueOutput(Vector[] output, final int range)
    {

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

}
