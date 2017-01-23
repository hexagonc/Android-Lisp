package com.evolved.automata.nn;

import com.evolved.automata.AITools;
import com.evolved.automata.IndexedValueMapper;
import com.evolved.automata.parser.general.Sequence;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.Ignore;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;

import static org.junit.Assert.assertTrue;




/**
 * Created by Evolved8 on 12/25/16.
 */
public class SequencePredictorLSTMTester {



    @Test
    public void testVariousArchLongVeryNoisySequenceRecognition()
    {


        String errorMessage = "failure saving patterns";
        SequenceLSTM.LSTMNetworkBuilder builder = SequenceLSTM.getSequenceBuilder();
        builder.setEndCapPolicy(SequenceLSTM.EndCapPolicy.BOTH_ENDS);
        builder.setInputNodeCount(8).addMemoryCell("M", 15);
        builder.setCapDisplayPolicy(SequenceLSTM.EndCapDisplayPolicy.HIDE);

        NNTools.addStandardRPROPWeightUpdatePolicies(builder);
        builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrderWithForgetGates("M"));
        builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrderWithForgetGates("M"));
        NNTools.addNodeConnectivity(builder, NNTools.getStandardLinkConnectivityMapWithForgetGates("M"));


        int numErrorBits = 2, numErrorVectors = 2;

        testNoiseToleranceLSTM(String.format("Testing with Forget Gates and %1$s error bits and %2$s error vector ", numErrorBits, numErrorVectors),  builder, numErrorBits, numErrorVectors);
        builder = NNTools.getStandardSequenceLSTMBuilder(8, 15, null);

        testNoiseToleranceLSTM(String.format("Testing with Generic LSTM and %1$s error bits and %2$s error vector ", numErrorBits, numErrorVectors), builder, numErrorBits, numErrorVectors);

        builder = SequenceLSTM.getSequenceBuilder();
        builder.setEndCapPolicy(SequenceLSTM.EndCapPolicy.BOTH_ENDS);
        builder.setInputNodeCount(8).addMemoryCell("M", 15);
        builder.setCapDisplayPolicy(SequenceLSTM.EndCapDisplayPolicy.HIDE);
        NNTools.addStandardRPROPWeightUpdatePolicies(builder);
        builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrderWithPeepholes("M"));
        builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrderWithPeepholes("M"));
        NNTools.addNodeConnectivity(builder, NNTools.getStandardLinkConnectivityMapWithPeepholes("M"));

        testNoiseToleranceLSTM(String.format("Testing with Peepholes and %1$s error bits and %2$s error vector ", numErrorBits, numErrorVectors), builder, numErrorBits, numErrorVectors);


        builder = SequenceLSTM.getSequenceBuilder();
        builder.setEndCapPolicy(SequenceLSTM.EndCapPolicy.BOTH_ENDS);
        builder.setInputNodeCount(8).addMemoryCell("M", 15);
        builder.setCapDisplayPolicy(SequenceLSTM.EndCapDisplayPolicy.HIDE);
        NNTools.addStandardRPROPWeightUpdatePolicies(builder);
        builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrderWithPeepholesAndForgetGates("M"));
        builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrderWithPeepholesAndForgetGates("M"));
        NNTools.addNodeConnectivity(builder, NNTools.getStandardLinkConnectivityMapWithPeepholesAndForgetGates("M"));

        testNoiseToleranceLSTM(String.format("Testing with Peepholes and Forget gates and %1$s error bits and %2$s error vector ", numErrorBits, numErrorVectors), builder, numErrorBits, numErrorVectors);



    }

    @Test
    public void testVariousArchLongNoisySequenceRecognition()
    {


        String errorMessage = "failure saving patterns";
        SequenceLSTM.LSTMNetworkBuilder builder = SequenceLSTM.getSequenceBuilder();
        builder.setEndCapPolicy(SequenceLSTM.EndCapPolicy.BOTH_ENDS);
        builder.setInputNodeCount(8).addMemoryCell("M", 15);
        builder.setCapDisplayPolicy(SequenceLSTM.EndCapDisplayPolicy.HIDE);

        NNTools.addStandardRPROPWeightUpdatePolicies(builder);
        builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrderWithForgetGates("M"));
        builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrderWithForgetGates("M"));
        NNTools.addNodeConnectivity(builder, NNTools.getStandardLinkConnectivityMapWithForgetGates("M"));


        int numErrorBits = 1, numErrorVectors = 1;
        numErrorBits = 1;
        numErrorVectors = 1;
        testNoiseToleranceLSTM(String.format("Testing with Forget Gates and %1$s error bits and %2$s error vector ", numErrorBits, numErrorVectors), builder, numErrorBits, numErrorVectors);
        builder = NNTools.getStandardSequenceLSTMBuilder(8, 15, null);
        numErrorBits = 1;
        numErrorVectors = 1;
        testNoiseToleranceLSTM(String.format("Testing with Generic LSTM and %1$s error bits and %2$s error vector ", numErrorBits, numErrorVectors), builder, numErrorBits, numErrorVectors);

        builder = SequenceLSTM.getSequenceBuilder();
        builder.setEndCapPolicy(SequenceLSTM.EndCapPolicy.BOTH_ENDS);
        builder.setInputNodeCount(8).addMemoryCell("M", 15);
        builder.setCapDisplayPolicy(SequenceLSTM.EndCapDisplayPolicy.HIDE);
        NNTools.addStandardRPROPWeightUpdatePolicies(builder);
        builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrderWithPeepholes("M"));
        builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrderWithPeepholes("M"));
        NNTools.addNodeConnectivity(builder, NNTools.getStandardLinkConnectivityMapWithPeepholes("M"));

        testNoiseToleranceLSTM(String.format("Testing with Peepholes and %1$s error bits and %2$s error vector ", numErrorBits, numErrorVectors), builder, numErrorBits, numErrorVectors);


        builder = SequenceLSTM.getSequenceBuilder();
        builder.setEndCapPolicy(SequenceLSTM.EndCapPolicy.BOTH_ENDS);
        builder.setInputNodeCount(8).addMemoryCell("M", 15);
        builder.setCapDisplayPolicy(SequenceLSTM.EndCapDisplayPolicy.HIDE);
        NNTools.addStandardRPROPWeightUpdatePolicies(builder);
        builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrderWithPeepholesAndForgetGates("M"));
        builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrderWithPeepholesAndForgetGates("M"));
        NNTools.addNodeConnectivity(builder, NNTools.getStandardLinkConnectivityMapWithPeepholesAndForgetGates("M"));

        testNoiseToleranceLSTM(String.format("Testing with Peepholes and Forget gates and %1$s error bits and %2$s error vector ", numErrorBits, numErrorVectors), builder, numErrorBits, numErrorVectors);



    }

    @Test
    public void testSystematicErrorSequenceRecognition()
    {


        String errorMessage = "failure saving patterns";
        SequenceLSTM.LSTMNetworkBuilder builder = SequenceLSTM.getSequenceBuilder();
        builder.setEndCapPolicy(SequenceLSTM.EndCapPolicy.BOTH_ENDS);
        builder.setInputNodeCount(8).addMemoryCell("M", 15);
        builder.setCapDisplayPolicy(SequenceLSTM.EndCapDisplayPolicy.HIDE);

        NNTools.addStandardRPROPWeightUpdatePolicies(builder);
        builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrderWithForgetGates("M"));
        builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrderWithForgetGates("M"));
        NNTools.addNodeConnectivity(builder, NNTools.getStandardLinkConnectivityMapWithForgetGates("M"));


        int numErrorBits = 1, numErrorVectors = 1;
        numErrorBits = 1;
        numErrorVectors = 1;
        testToleranceToSystematicErrorLSTM(String.format("Testing with Forget Gates and %1$s systematic error bits across all vectors ", numErrorBits), builder, numErrorBits, true);

        /*
        builder = NNTools.getStandardSequenceLSTMBuilder(8, 15, null);
        numErrorBits = 1;
        numErrorVectors = 1;
        testNoiseToleranceLSTM(String.format("Testing with Generic LSTM and %1$s error bits and %2$s error vector ", numErrorBits, numErrorVectors), builder, numErrorBits, numErrorVectors);

        builder = SequenceLSTM.getSequenceBuilder();
        builder.setEndCapPolicy(SequenceLSTM.EndCapPolicy.BOTH_ENDS);
        builder.setInputNodeCount(8).addMemoryCell("M", 15);
        builder.setCapDisplayPolicy(SequenceLSTM.EndCapDisplayPolicy.HIDE);
        NNTools.addStandardRPROPWeightUpdatePolicies(builder);
        builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrderWithPeepholes("M"));
        builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrderWithPeepholes("M"));
        NNTools.addNodeConnectivity(builder, NNTools.getStandardLinkConnectivityMapWithPeepholes("M"));

        testNoiseToleranceLSTM(String.format("Testing with Peepholes and %1$s error bits and %2$s error vector ", numErrorBits, numErrorVectors), builder, numErrorBits, numErrorVectors);


        builder = SequenceLSTM.getSequenceBuilder();
        builder.setEndCapPolicy(SequenceLSTM.EndCapPolicy.BOTH_ENDS);
        builder.setInputNodeCount(8).addMemoryCell("M", 15);
        builder.setCapDisplayPolicy(SequenceLSTM.EndCapDisplayPolicy.HIDE);
        NNTools.addStandardRPROPWeightUpdatePolicies(builder);
        builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrderWithPeepholesAndForgetGates("M"));
        builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrderWithPeepholesAndForgetGates("M"));
        NNTools.addNodeConnectivity(builder, NNTools.getStandardLinkConnectivityMapWithPeepholesAndForgetGates("M"));

        testNoiseToleranceLSTM(String.format("Testing with Peepholes and Forget gates and %1$s error bits and %2$s error vector ", numErrorBits, numErrorVectors), builder, numErrorBits, numErrorVectors);
        */


    }


    @Test
    public void testVariousArchLongSequenceRecognitionAndStorage()
    {


        String errorMessage = "failure saving patterns";
        SequenceLSTM.LSTMNetworkBuilder builder = SequenceLSTM.getSequenceBuilder();
        builder.setEndCapPolicy(SequenceLSTM.EndCapPolicy.BOTH_ENDS);
        builder.setInputNodeCount(8).addMemoryCell("M", 15);
        builder.setCapDisplayPolicy(SequenceLSTM.EndCapDisplayPolicy.HIDE);

        NNTools.addStandardRPROPWeightUpdatePolicies(builder);
        builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrderWithForgetGates("M"));
        builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrderWithForgetGates("M"));
        NNTools.addNodeConnectivity(builder, NNTools.getStandardLinkConnectivityMapWithForgetGates("M"));



        testLSTM("Testing with Forget Gates", builder);
        builder = NNTools.getStandardSequenceLSTMBuilder(8, 15, null);
        testLSTM("Testing with Generic", builder);

        builder = SequenceLSTM.getSequenceBuilder();
        builder.setEndCapPolicy(SequenceLSTM.EndCapPolicy.BOTH_ENDS);
        builder.setInputNodeCount(8).addMemoryCell("M", 15);
        builder.setCapDisplayPolicy(SequenceLSTM.EndCapDisplayPolicy.HIDE);
        NNTools.addStandardRPROPWeightUpdatePolicies(builder);
        builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrderWithPeepholes("M"));
        builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrderWithPeepholes("M"));
        NNTools.addNodeConnectivity(builder, NNTools.getStandardLinkConnectivityMapWithPeepholes("M"));

        testLSTM("Testing with Peepholes", builder);


        builder = SequenceLSTM.getSequenceBuilder();
        builder.setEndCapPolicy(SequenceLSTM.EndCapPolicy.BOTH_ENDS);
        builder.setInputNodeCount(8).addMemoryCell("M", 15);
        builder.setCapDisplayPolicy(SequenceLSTM.EndCapDisplayPolicy.HIDE);
        NNTools.addStandardRPROPWeightUpdatePolicies(builder);
        builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrderWithPeepholesAndForgetGates("M"));
        builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrderWithPeepholesAndForgetGates("M"));
        NNTools.addNodeConnectivity(builder, NNTools.getStandardLinkConnectivityMapWithPeepholesAndForgetGates("M"));

        testLSTM("Testing with Peepholes and Forget gates", builder);



    }



    private void testLSTM(String message, SequenceLSTM.LSTMNetworkBuilder builder)
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

        SequenceLSTM current = builder.build();
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

            for (int i = 0;i<inputs.length;i++)
            {

                value = inputs[i];
                learningResult = current.add(NNTools.getVector(NNTools.stageDiscretize(value, 255, 4)), thresholdCount, errorThreshold);

                if (learningResult[0] > errorThreshold)
                {
                    saved = current.getCurrentSequence();
                    lengths.add(saved.length);
                    System.out.println("After [" + (int)learningResult[1] + "] steps, saved ("+ saved.length + ")" + Arrays.toString(NNTools.getNumericValueOutput(saved, 255)));
                    output.add(current);
                    current = builder.build();
                }

            }

            if (current.getSequenceLength()>0)
            {
                saved = current.getCurrentSequence();
                lengths.add(saved.length);
                System.out.println("After [" + (int)learningResult[1] + "] steps, saved ("+ saved.length + ")" + Arrays.toString(NNTools.getNumericValueOutput(saved, 255)));
                output.add(current);
            }

            System.out.println("Partitioned original input into " + output.size() + " parts of length: " + lengths);
            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();;
        }
    }

    private void testToleranceToSystematicErrorLSTM(String message, SequenceLSTM.LSTMNetworkBuilder builder ,int bitsToFlip, final boolean forceTrue)
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

        SequenceLSTM current = builder.build();
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

            for (int i = 0;i<inputs.length;i++)
            {

                value = inputs[i];
                learningResult = current.add(NNTools.getVector(NNTools.stageDiscretize(value, 255, 4)), thresholdCount, errorThreshold);

                if (learningResult[0] > errorThreshold)
                {
                    saved = current.getCurrentSequence();
                    lengths.add(saved.length);
                    System.out.println("After [" + (int)learningResult[1] + "] steps, saved ("+ saved.length + ")" + Arrays.toString(NNTools.getNumericValueOutput(saved, 255)));
                    output.add(current);
                    current = builder.build();
                }

            }

            if (current.getSequenceLength()>0)
            {
                saved = current.getCurrentSequence();
                lengths.add(saved.length);
                System.out.println("After [" + (int)learningResult[1] + "] steps, saved ("+ saved.length + ")" + Arrays.toString(NNTools.getNumericValueOutput(saved, 255)));
                output.add(current);
            }

            System.out.println("Partitioned original input into " + output.size() + " parts of length: " + lengths);
            success = true;

            Vector[] originalPattern;
            Vector[] noisyVersion;
            SequenceLSTM stm;
            Double[] converted;
            Vector extrapolated;
            final int[] bits = AITools.getRandomSubset(8, bitsToFlip);
            for (int i =0;i<output.size();i++)
            {
                stm = output.get(i);

                originalPattern = stm.getCurrentSequence();
                String internal = Arrays.toString(originalPattern);
                //System.out.println("Original pattern: " + internal);
                converted = NNTools.getNumericValueOutput(originalPattern, 255);



                System.out.println("Adding " + bitsToFlip + " bits of noise to " + Arrays.toString(converted) + " at positions " + Arrays.toString(bits));
                originalPattern = AITools.mapValues(originalPattern, new VectorValueMapper<Vector>() {

                    @Override
                    public Vector map(Vector input, int index)
                    {
                        for (int i = 0; i < bits.length; i++)
                        {
                            if (forceTrue)
                                input.setValue(1, bits[i]);
                            else
                                input.setValue(0, bits[i]);

                        }
                        return input;
                    }
                });


                internal = Arrays.toString(originalPattern);
                System.out.println("Mutated driving pattern: " + internal);
                converted = NNTools.getNumericValueOutput(originalPattern, 255);
                System.out.println("Noisy driving input is: " + Arrays.toString(converted));
                stm.resetToStartOfSequence();
                noisyVersion = stm.viewSequenceOutput(originalPattern, true);
                converted = NNTools.getNumericValueOutput(noisyVersion, 255);
                System.out.println("Noisy extrapolation is: " + Arrays.toString(converted));
                internal = Arrays.toString(originalPattern);
                System.out.println("Raw output: " + Arrays.toString(noisyVersion));
            }



        }
        catch (Exception e)
        {
            e.printStackTrace();;
        }
    }



    private void testNoiseToleranceLSTM(String message, SequenceLSTM.LSTMNetworkBuilder builder ,int bitsToFlip, int numSamplesToAddNoise)
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

        SequenceLSTM current = builder.build();
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

            for (int i = 0;i<inputs.length;i++)
            {

                value = inputs[i];
                learningResult = current.add(NNTools.getVector(NNTools.stageDiscretize(value, 255, 4)), thresholdCount, errorThreshold);

                if (learningResult[0] > errorThreshold)
                {
                    saved = current.getCurrentSequence();
                    lengths.add(saved.length);
                    System.out.println("After [" + (int)learningResult[1] + "] steps, saved ("+ saved.length + ")" + Arrays.toString(NNTools.getNumericValueOutput(saved, 255)));
                    output.add(current);
                    current = builder.build();
                }

            }

            if (current.getSequenceLength()>0)
            {
                saved = current.getCurrentSequence();
                lengths.add(saved.length);
                System.out.println("After [" + (int)learningResult[1] + "] steps, saved ("+ saved.length + ")" + Arrays.toString(NNTools.getNumericValueOutput(saved, 255)));
                output.add(current);
            }

            System.out.println("Partitioned original input into " + output.size() + " parts of length: " + lengths);
            success = true;

            Vector[] originalPattern;
            Vector[] noisyVersion;
            SequenceLSTM stm;
            Double[] converted;
            int[] noiseIndices;
            Vector extrapolated;
            for (int i =0;i<output.size();i++)
            {
                stm = output.get(i);

                originalPattern = stm.getCurrentSequence();
                String internal = Arrays.toString(originalPattern);
                //System.out.println("Original pattern: " + internal);
                converted = NNTools.getNumericValueOutput(originalPattern, 255);
                noiseIndices = AITools.getRandomSubset(originalPattern.length, numSamplesToAddNoise);
                System.out.println("Adding " + bitsToFlip + " bits of noise to " + Arrays.toString(converted) + " at positions " + Arrays.toString(noiseIndices));
                for (int j = 0;j<numSamplesToAddNoise;j++)
                {
                    originalPattern[noiseIndices[j]] = NNTools.addNoiseToVector(originalPattern[noiseIndices[j]], bitsToFlip);
                }
                internal = Arrays.toString(originalPattern);
                System.out.println("Mutated driving pattern: " + internal);
                converted = NNTools.getNumericValueOutput(originalPattern, 255);
                System.out.println("Noisy driving input is: " + Arrays.toString(converted));
                stm.resetToStartOfSequence();
                noisyVersion = stm.viewSequenceOutput(originalPattern, true);
                converted = NNTools.getNumericValueOutput(noisyVersion, 255);
                System.out.println("Noisy extrapolation is: " + Arrays.toString(converted));
                internal = Arrays.toString(originalPattern);
                System.out.println("Raw output: " + Arrays.toString(noisyVersion));
            }



        }
        catch (Exception e)
        {
            e.printStackTrace();;
        }
    }


    @Test
    public void testLongSequenceRecognitionAndStorage()
    {
        double[] inputs = {59, 55, 117, 255, 255, 42, 84, 43, 255, 45, 39, 42, 255, 55, 57, 255,
                43, 42, 109, 70, 42, 115, 43, 43, 255, 255, 44, 46, 255, 255, 85, 255, 43, 44, 87, 255, 88, 120, 44,
        43, 98, 84, 85, 255, 45, 43, 255, 66, 48, 255, 255, 66, 43, 255, 48, 255, 255, 66, 44, 255, 70, 255, 255,
        66, 45, 46, 255, 255, 255, 255, 57, 48,59, 55, 117, 255, 255, 42, 84, 43, 255, 45, 39, 42, 255, 55, 57, 255,
        43, 42, 109, 70, 42, 115, 43, 43, 39, 93, 255, 41, 50, 54, 40, 71, 255, 41, 51, 255, 39, 69, 255, 42,
        50, 50, 39, 70, 255, 43, 50, 51};
        boolean success = false;
        double[] result;
        int totalSteps = 0;
        int failureCount = 0;
        double errorThreshold = 0.1;
        String errorMessage = "failure saving patterns";
        int lstmBufferLength = 20;
        int numL = 1;
        SequenceLSTM[] lstmBuffer = new SequenceLSTM[lstmBufferLength];
        int i = 0;
        double value;

        int thresholdCount = 400;
        String lastWeights = null, lastState = null;

        ArrayList<Vector> distortedPattern = new ArrayList<Vector>();

        Vector[] pattern;
        long start, elapsed;

        int allSteps = 0;
        try
        {
            lstmBuffer[i] = NNTools.getStandardSequenceLSTM(8, 10, null);
            for (int j = 0;j<inputs.length;j++)
            {
                value = inputs[j];


                start = System.currentTimeMillis();
                result = lstmBuffer[i].add(NNTools.getVector(NNTools.stageDiscretize(value, 255, 4)), thresholdCount, errorThreshold);
                totalSteps+=result[1];
                System.out.println("Trying to save " + value + " Output: " + Arrays.toString(result));
                if (result[0] <= errorThreshold)
                {
                    allSteps+=result[1];
                    lastWeights = lstmBuffer[i].serializeLinkWeights();
                    lastState = lstmBuffer[i].serializeNetworkActivationState();
                    elapsed = System.currentTimeMillis() - start;
                    System.out.println("Saved: " + value + " in " + (int) result[1] + " steps and [" + elapsed + "] ms");
                    System.out.println("Used "+ totalSteps + " steps for current subsequence.  Stored result is: " + Arrays.toString( getNumericSequenceOutput(lstmBuffer[i], 255)));
                }
                else
                {

                    pattern = lstmBuffer[i].getCurrentSequence();
                    for (Vector vi:pattern)
                    {
                        distortedPattern.add(vi);
                    }

                    start = System.currentTimeMillis();
                    lstmBuffer[i].decodeSerializedLinksToLinkBuffer(lastWeights);
                    lstmBuffer[i].loadbufferedLinkWeights();


                    elapsed = System.currentTimeMillis() - start;
                    System.out.println("Finished segment " + i + " after " + elapsed + " ms, allocating new lstm " + (i + 1));
                    System.out.println("Saving distorted test pattern: " + Arrays.toString(NNTools.getNumericValueOutput(pattern, 255)));


                    if (i >= lstmBufferLength)
                        System.out.println("Rolling over slstm buffer");
                    i = (i + 1) % lstmBufferLength;
                    lstmBuffer[i] = NNTools.getStandardSequenceLSTM(8, 10, null);
                    lstmBuffer[i].loadSerializedNetworkActivationState(lastState);
                    lstmBuffer[i].setInitialNodeActivationAsCurrent();
                    totalSteps = 0;
                    j--;
                }

            }


            errorMessage = "failure reconstructing pattern";
            ArrayList<Double> outputList = new ArrayList<Double>();
            Vector[] v;
            Double[] s;
            for (int k = 0;k<=i;k++)
            {
                v = lstmBuffer[k].getCurrentSequence();
                s = NNTools.getNumericValueOutput(v, 255);
                for (Double ss: s)
                    outputList.add(ss);
            }

            System.out.println("Reconstructed: " + outputList + " after " + allSteps+ " steps");

            errorMessage = "failure with original pattern recognition";

            SequenceLSTM hierarchicalPatternBuilder = NNTools.getStandardSequenceLSTM(lstmBufferLength, 10, null);

            int[] filter=new int[lstmBufferLength];


            System.out.println("Trying to recognize sequence: " + Arrays.toString(inputs));
            Vector higherPattern, inputVector, prev = new Vector(lstmBufferLength);
            for (i = 0;i < inputs.length;i++)
            {
                inputVector = NNTools.getVector(NNTools.stageDiscretize(inputs[i], 255, 4));
                System.out.println("Trying to recognize: " + inputs[i]);

                higherPattern = getMatchingLSTMs(lstmBuffer, inputVector, prev.raw(), filter);
                if (!higherPattern.equals(prev))
                {
                    System.out.println("Adding new hierachical pattern " + higherPattern);
                    result = hierarchicalPatternBuilder.add(higherPattern, 600, 0.1);
                    if (result[0]>0.1)
                        System.out.println("<><><><<>< Failed to store hierarchical pattern");
                }
                prev = higherPattern;
                System.out.println("Recognized higher order pattern is: " + higherPattern);

            }

            System.out.println("Hierarchical patterns are: " + Arrays.toString(hierarchicalPatternBuilder.getCurrentSequence()));

            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();;
        }

        assertTrue(errorMessage, success);
    }


    @Test
    public void testMiddleSequenceRecognition()
    {
        double[] inputs = {59, 55, 117, 255, 255, 42, 84, 43, 255, 45, 39, 42, 255, 55, 57, 255,
                43, 42, 109, 70, 42, 115, 43, 43};
        boolean success = false;
        double[] result;
        int totalSteps = 0;
        int failureCount = 0;
        double errorThreshold = 0.1;
        String errorMessage = "failure saving patterns";
        int lstmBufferLength = 20;
        int numL = 1;
        SequenceLSTM[] lstmBuffer = new SequenceLSTM[lstmBufferLength];
        int i = 0;
        double value;

        int thresholdCount = 400;
        String lastWeights = null, lastState = null;

        ArrayList<Vector> distortedPattern = new ArrayList<Vector>();

        Vector[] pattern;
        long start, elapsed;
        SequenceLSTM hierarchicalPattern = NNTools.getStandardSequenceLSTM(lstmBufferLength, 10, null);
        int allSteps = 0;
        try
        {
            lstmBuffer[i] = NNTools.getStandardSequenceLSTM(8, 10, null);
            for (int j = 0;j<inputs.length;j++)
            {
                value = inputs[j];


                start = System.currentTimeMillis();
                result = lstmBuffer[i].add(NNTools.getVector(NNTools.stageDiscretize(value, 255, 4)), thresholdCount, errorThreshold);
                totalSteps+=result[1];
                System.out.println("Trying to save " + value + " Output: " + Arrays.toString(result));
                if (result[0] <= errorThreshold)
                {
                    allSteps+=result[1];
                    lastWeights = lstmBuffer[i].serializeLinkWeights();
                    lastState = lstmBuffer[i].serializeNetworkActivationState();
                    elapsed = System.currentTimeMillis() - start;
                    System.out.println("Saved: " + value + " in " + (int) result[1] + " steps and [" + elapsed + "] ms");
                    System.out.println("Used "+ totalSteps + " steps for current subsequence.  Stored result is: " + Arrays.toString( getNumericSequenceOutput(lstmBuffer[i], 255)));
                }
                else
                {

                    pattern = lstmBuffer[i].getCurrentSequence();
                    for (Vector vi:pattern)
                    {
                        distortedPattern.add(vi);
                    }

                    start = System.currentTimeMillis();
                    lstmBuffer[i].decodeSerializedLinksToLinkBuffer(lastWeights);
                    lstmBuffer[i].loadbufferedLinkWeights();


                    elapsed = System.currentTimeMillis() - start;
                    System.out.println("Finished segment " + i + " after " + elapsed + " ms, allocating new lstm " + (i + 1));
                    System.out.println("Saving distorted test pattern: " + Arrays.toString(NNTools.getNumericValueOutput(pattern, 255)));


                    if (i >= lstmBufferLength)
                        System.out.println("Rolling over slstm buffer");
                    i = (i + 1) % lstmBufferLength;
                    lstmBuffer[i] = NNTools.getStandardSequenceLSTM(8, 10, null);
                    lstmBuffer[i].loadSerializedNetworkActivationState(lastState);
                    lstmBuffer[i].setInitialNodeActivationAsCurrent();
                    totalSteps = 0;
                    j--;
                }

            }


            errorMessage = "failure reconstructing pattern";
            ArrayList<Double> outputList = new ArrayList<Double>();
            Vector[] v;
            Double[] s;
            for (int k = 0;k<=i;k++)
            {
                v = lstmBuffer[k].getCurrentSequence();
                s = NNTools.getNumericValueOutput(v, 255);
                for (Double ss: s)
                    outputList.add(ss);
            }

            System.out.println("Reconstructed: " + outputList + " after " + allSteps+ " steps");

            errorMessage = "failure with original pattern recognition";

            int sampleCount = 9;
            int[] filter=null;


            for (i = 0;i < sampleCount;i++)
            {
                int offset = (int)(Math.random() * inputs.length);

                System.out.println("Trying to recognize sequence: " + Arrays.toString(Arrays.copyOfRange(inputs, offset, inputs.length)));
                filter = null;
                for (int k = offset;k<inputs.length;k++)
                {
                    filter = recognizingDiscretizedNumericPatterns(lstmBuffer, NNTools.getVector(NNTools.stageDiscretize(inputs[k], 255, 4)), filter);
                }

                String name = Arrays.toString(Arrays.copyOfRange(inputs, offset, inputs.length));
                for (int f = 0;f < filter.length;f++)
                {
                    if (filter[f]>0 && 1.0*filter[f]/(inputs.length - offset) > 0.5)
                    {
                        System.out.println("**** Pattern " + name + " recognized by lstm " + f);
                    }
                }
            }

            errorMessage = "failure with distorted pattern recognition";

            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();;
        }

        assertTrue(errorMessage, success);
    }




    @Test
    public void testMiddleSequenceConstruction()
    {
        double[] inputs = {59, 55, 117, 255, 255, 42, 84, 43, 255, 59, 55, 117, 255, 255, 42, 84, 43, 255, 59, 55, 117, 255, 255, 42, 84, 43, 255,};
        boolean success = false;
        double[] result;
        int totalSteps = 0;
        int failureCount = 0;
        double errorThreshold = 0.1;
        String errorMessage = "failure saving patterns";
        int lstmBufferLength = 20;
        int numL = 1;
        SequenceLSTM[] lstmBuffer = new SequenceLSTM[lstmBufferLength];
        int i = 0;
        double value;

        int thresholdCount = 400;
        String lastWeights = null;
        ArrayList distortedPattern = new ArrayList();
        Vector[] pattern;
        long start, elapsed;
        SequenceLSTM hierarchicalPattern = NNTools.getStandardSequenceLSTM(lstmBufferLength, 10, null);
        int allSteps = 0;
        try
        {
            lstmBuffer[i] = NNTools.getStandardSequenceLSTM(8, 10, null);
            for (int j = 0;j<inputs.length;j++)
            {
                value = inputs[j];


                start = System.currentTimeMillis();
                result = lstmBuffer[i].add(NNTools.getVector(NNTools.stageDiscretize(value, 255, 4)), thresholdCount, errorThreshold);
                totalSteps+=result[1];
                System.out.println("Trying to save " + value + " Output: " + Arrays.toString(result));
                if (result[0] <= errorThreshold)
                {
                    allSteps+=totalSteps;
                    lastWeights = lstmBuffer[i].serializeLinkWeights();
                    elapsed = System.currentTimeMillis() - start;
                    System.out.println("Saved: " + value + " in " + (int) result[1] + " steps and [" + elapsed + "] ms");
                    System.out.println("Used "+ totalSteps + " steps for current subsequence.  Stored result is: " + Arrays.toString( getNumericSequenceOutput(lstmBuffer[i], 255)));
                }
                else
                {

                    pattern = lstmBuffer[i].getCurrentSequence();
                    distortedPattern.add(pattern);

                    start = System.currentTimeMillis();
                    lstmBuffer[i].decodeSerializedLinksToLinkBuffer(lastWeights);
                    lstmBuffer[i].loadbufferedLinkWeights();
                    elapsed = System.currentTimeMillis() - start;
                    System.out.println("Finished segment " + i + " after " + elapsed + " ms, allocating new lstm " + (i + 1));
                    System.out.println("Saving distorted test pattern: " + Arrays.toString(NNTools.getNumericValueOutput(pattern, 255)));


                    if (i >= lstmBufferLength)
                        System.out.println("Rolling over slstm buffer");
                    i = (i + 1) % lstmBufferLength;
                    lstmBuffer[i] = NNTools.getStandardSequenceLSTM(8, 10, null);
                    totalSteps = 0;
                    j--;
                }

            }


            errorMessage = "failure reconstructing pattern";
            ArrayList<Double> outputList = new ArrayList<Double>();
            Vector[] v;
            Double[] s;
            for (int k = 0;k<=i;k++)
            {
                v = lstmBuffer[k].getCurrentSequence();
                s = NNTools.getNumericValueOutput(v, 255);
                for (Double ss: s)
                    outputList.add(ss);
            }

            System.out.println("Reconstructed: " + outputList + " after " + allSteps+ " steps");

            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();;
        }

        assertTrue(errorMessage, success);
    }


    @Test
    public void testPeriodicSequenceConstructionCase1()
    {
        SequenceLSTM lstm = NNTools.getStandardSequenceLSTM(8, 10, null);
        double[] inputs = {59, 55, 117, 255, 255, 42, 84, 43, 255, 59, 55, 117, 255, 255, 42, 84, 43, 255, 59, 55, 117, 255, 255, 42, 84, 43, 255,};
        double[] result;
        int totalSteps = 0;
        int failureCount = 0;
        for (double value:inputs)
        {
            result = lstm.add(NNTools.getVector(NNTools.stageDiscretize(value, 255, 4)), (failureCount + 1)*1000, 0.1);
            totalSteps+=result[1];
            System.out.println("Output: " + Arrays.toString(result));
            if (result[0] <= 0.1)
            {
                System.out.println("Saved: " + value + " in " + (int) result[1] + " steps");
                System.out.println("Used "+ totalSteps + " steps for current subsequence.  Stored result is: " + Arrays.toString( getNumericSequenceOutput(lstm, 255)));
            }
            else
            {
                failureCount+=3;
                System.out.println("Failed to save " + value);
                System.out.println("Actual output now is: " + Arrays.toString( getNumericSequenceOutput(lstm, 255)));
            }

        }

        System.out.println("Used "+ totalSteps + " total steps.  Final output is: " + Arrays.toString( getNumericSequenceOutput(lstm, 255)));
    }


    @Test
    public void testSequenceConstructionCase1()
    {
        SequenceLSTM lstm = NNTools.getStandardSequenceLSTM(8, 10, null);
        double[] inputs = {59, 55, 117, 255, 255, 42, 84, 43, 255, 45, 39, 42, 255, 55, 57, 255,
                43, 42, 109, 70, 42, 115, 43, 43};
        double[] result;
        int totalSteps = 0;
        int failureCount = 0;
        for (double value:inputs)
        {
            result = lstm.add(NNTools.getVector(NNTools.stageDiscretize(value, 255, 4)), (failureCount + 1)*1000, 0.1);
            totalSteps+=result[1];
            System.out.println("Output: " + Arrays.toString(result));
            if (result[0] <= 0.1)
            {
                System.out.println("Saved: " + value + " in " + (int) result[1] + " steps");
                System.out.println("Used "+ totalSteps + " steps for current subsequence.  Stored result is: " + Arrays.toString( getNumericSequenceOutput(lstm, 255)));
            }
            else
            {
                failureCount+=3;
                System.out.println("Failed to save " + value);
                System.out.println("Actual output now is: " + Arrays.toString( getNumericSequenceOutput(lstm, 255)));
            }

        }

        System.out.println("Used "+ totalSteps + " total steps.  Final output is: " + Arrays.toString( getNumericSequenceOutput(lstm, 255)));
    }

    @Test
    public void testSequenceConstructionCase2()
    {
        SequenceLSTM lstm = NNTools.getStandardSequenceLSTM(8, 10, null);
        double[] inputs = {39, 93, 255, 41, 50, 54, 40, 71, 255, 41, 51, 255, 39, 69, 255, 42,
                50, 50, 39, 70, 255, 43, 50, 51};
        double[] result;
        int totalSteps = 0;

        for (double value:inputs)
        {
            result = lstm.add(NNTools.getVector(NNTools.stageDiscretize(value, 255, 4)), 1000, 0.1);
            totalSteps+=result[1];
            System.out.println("Output: " + Arrays.toString(result));
            if (result[0] <= 0.1)
            {
                System.out.println("Saved: " + value + " in " + (int) result[1] + " steps");
                System.out.println("Used "+ totalSteps + " steps for current subsequence.  Stored result is: " + Arrays.toString( getNumericSequenceOutput(lstm, 255)));
            }
            else
            {
                System.out.println("Failed to save " + value);
                System.out.println("Actual output now is: " + Arrays.toString( getNumericSequenceOutput(lstm, 255)));
            }

        }

        System.out.println("Used "+ totalSteps + " total steps.  Final output is: " + Arrays.toString( getNumericSequenceOutput(lstm, 255)));
    }

    private static Double[] getNumericSequenceOutput(SequenceLSTM lstm, final int range)
    {
        Vector[] output = lstm.getCurrentSequence();
        return AITools.mapValues(output, new IndexedValueMapper<Vector, Double>() {
            @Override
            public Double map(Vector input, int index)
            {
                ArrayList<Double> v = new ArrayList<Double>();
                for (double d:input.raw())
                {
                    v.add(d);
                }

                Pair<Double, Double> out = NNTools.stageContinuize(range, v);
                return (out.getLeft() + out.getRight())/2;
            }

            @Override
            public Double[] getEmptyOutput()
            {
                return new Double[0];
            }
        });
    }




    public static int[] recognizingDiscretizedNumericPatterns(SequenceLSTM[] total, Vector value, int[] filter)
    {
        int i = 0, L = total.length;
        double error = 0.1;
        if (filter == null)
        {
            System.out.println("Seeding all with: " + NNTools.averagedStageContinuize(255, value.raw()));
            filter = new int[L];
            for (i = 0;i< L;i++)
            {
                filter[i] = 1;

                if (total[i] != null)
                    total[i].viewSequenceOutput(new Vector[]{value}, false);
            }
        }
        else
        {
            boolean any = false;
            Vector predictedVector;
            double target = NNTools.averagedStageContinuize(255, value.raw());
            System.out.println("Recognizing: " + target);
            for (i = 0;i<L;i++)
            {
                if (total[i] == null)
                    continue;

                predictedVector = total[i].getOutputValues();
                double predictedNumeric = NNTools.averagedStageContinuize(255, predictedVector.raw());
                System.out.println("LSTM " + i + " predicted " + predictedNumeric);
                if (Math.abs(predictedNumeric - target)/target < error)
                {
                    any = true;
                    filter[i]++;
                    System.out.println("Recognized pattern with LSTM " + i);
                    total[i].viewSequenceOutput(new Vector[]{value}, true);
                }
                else
                {
                    total[i].viewSequenceOutput(new Vector[]{value}, true);
                }


            }

            if (!any)
                System.out.println("Failed to recognize pattern");
        }
        return filter;
    }

    public static Vector getMatchingLSTMs(SequenceLSTM[] total, Vector value, double[] prev, int[] filter)
    {
        int i = 0, L = total.length;
        double error = 0.05;
        double[] output = new double[L];

        boolean any = false;
        Vector predictedVector;
        double target = NNTools.averagedStageContinuize(255, value.raw());
        int conscCount = 4;
        boolean wasFirst = false;
        wasFirst = true;
        for (i = 0;i<L;i++)
        {
            if (filter[i] > 0)
            {
                wasFirst = false;
                break;
            }
        }



        while (!any)
        {
            for (i = 0;i<L;i++)
            {

                if (total[i] == null)
                    break;

                if (wasFirst)
                {
                    total[i].viewSequenceOutput(new Vector[]{value}, false);
                    predictedVector = total[i].getInitialInputActivation();
                }
                else if (filter[i]>0)
                {
                    predictedVector = total[i].getOutputValues().add(0); // creates a copy
                    total[i].viewSequenceOutput(new Vector[]{value}, true);
                }
                else
                {
                    continue;
                }
                double predictedNumeric = NNTools.averagedStageContinuize(255, predictedVector.raw());
                System.out.println("LSTM " + i + " predicted " + predictedNumeric);
                if (Math.abs(predictedNumeric - target)/target < error)
                {
                    any = true;
                    filter[i]++;
                    if (filter[i] >= conscCount || wasFirst)
                        output[i] = 1;
                    else
                        output[i] = prev[i];

                }
                else
                {
                    output[i] = 0;
                    filter[i] = 0;
                }


            }
            if (!any && wasFirst)
                break;
            else if (!any)
                wasFirst = true;
        }



        return new Vector(output);
    }

}
