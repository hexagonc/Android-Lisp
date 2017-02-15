package com.evolved.automata.nn;

import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import static org.junit.Assert.assertTrue;
/**
 * Created by Evolved8 on 2/13/17.
 */



public class FastLSTMTester extends BaseLSTMTester {
    @Test
    public void testInitialization()
    {
        int inputNodeCode = 20;
        int outputNodeCode = 20;
        int numMemoryCellStates = 17;

        String errorMessage = "Failed to create FastLSTM";
        try
        {
            FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);

            FastLSTMNetwork network = builder.build();
            float[] networkSpec = network._networkData;

            Assert.assertTrue(errorMessage, true);
        }
        catch (Exception e)
        {
            Assert.assertTrue(errorMessage, false);
        }

    }

    FastLSTMNetwork.LSTMNetworkBuilder getStandardBuilder(int inputNodeCode, int outputNodeCode, int numMemoryCellStates)
    {
        FastLSTMNetwork.LSTMNetworkBuilder builder = FastLSTMNetwork.getFastBuilder();

        builder.setInputNodeCount(inputNodeCode);
        builder.setOutputNodeCount(outputNodeCode);
        builder.addMemoryCell("M", numMemoryCellStates);
        HashMap<String, ArrayList<String>> connectivityMap = NNTools.getStandardLinkConnectivityMap("M");
        for (String sourceNode:connectivityMap.keySet())
        {
            builder.addNodeConnections(sourceNode, NNTools.arrayListToArray(connectivityMap.get(sourceNode)));
        }

        builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrder("M"));
        builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrder("M"));
        builder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.INITIAL_DELTA, 0.012);
        builder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MAX_DELTA, 50);
        builder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MIN_DELTA, 0);
        builder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MAX, 1.2);
        builder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MIN, 0.5);
        builder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.CONVERGENCE_THRESHOLD, 0.0001);
        builder.setWeightUpdateType(LSTMNetwork.WeightUpdateType.RPROP);
        return builder;
    }


    @Test
    public void testFeedForward()
    {
        String errorMessage = "Failed to create FastLSTM";
        int inputNodeCode = 20;
        int outputNodeCode = 20;
        int numMemoryCellStates = 17;
        try
        {
            FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);

            FastLSTMNetwork network = builder.build();
            float[] networkSpec = network._networkData;
            int feedforward_link_count_idx = network.feedforward_link_count_idx;
            int weight_update_link_count_idx = network.backprop_link_count_idx;


            double[] objOrientedInput = new double[inputNodeCode];
            float[] testSuccinctInput = new float[inputNodeCode];
            double v;
            for (int i=0; i<inputNodeCode;i++)
            {
                v = Math.random();
                if (v < 0.5)
                    objOrientedInput[i] = 0;
                else
                    objOrientedInput[i] = 1;

                testSuccinctInput[i] = (float)objOrientedInput[i];
            }

            Vector testInput = new Vector(objOrientedInput);
            errorMessage = "Failure to execute legacy feedforward";

            long start = System.currentTimeMillis();
            network.executeForwardPass(testInput);
            long legacyTime = System.currentTimeMillis() - start;
            double[] output = network.getOutputValues().raw();
            Assert.assertTrue(errorMessage, output!=null);

            errorMessage = "Failure to initialize succinct network";

            FastLSTMNetwork.initializeNodeState(networkSpec);
            FastLSTMNetwork.initializeAllWeights(networkSpec, weight_update_link_count_idx, 0.012F, -1, 1);

            errorMessage = "Failure to execute succinct feedforward";

            start = System.currentTimeMillis();
            FastLSTMNetwork.forwardPass(networkSpec, feedforward_link_count_idx, testSuccinctInput);
            long nativeTime = System.currentTimeMillis() - start;
            float[] outputNative = FastLSTMNetwork.getOutputActivation(networkSpec);
            System.out.println("Legacy: " + Arrays.toString(output) + " after " + legacyTime + ", Succinct: " + Arrays.toString(outputNative) + " after " + nativeTime);

            Assert.assertTrue(errorMessage, true);
        }
        catch (Exception e)
        {
            Assert.assertTrue(e + "\n" + errorMessage, false);
        }
    }



    @Test
    public void testWeightUpdates()
    {
        String errorMessage = "Failed to create FastLSTM";
        int inputNodeCode = 20;
        int outputNodeCode = 20;
        int numMemoryCellStates = 10;



        try
        {
            FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);

            FastLSTMNetwork network = builder.build();
            float[] networkSpec = network._networkData;
            int feedforward_link_count_idx = network.feedforward_link_count_idx;
            int weight_update_link_count_idx = network.backprop_link_count_idx;



            float[] initialInput = new float[inputNodeCode], output = new float[outputNodeCode], expected = new float[outputNodeCode];
            initialInput[0] = 1;
            expected[inputNodeCode-1] = 1;


            long start = System.currentTimeMillis();



            FastLSTMNetwork.initializeAllWeights(networkSpec, weight_update_link_count_idx, 0.0012F, -3, 3);

            errorMessage = "Failure to execute learning passes";

            start = System.currentTimeMillis();

            FastLSTMNetwork.resetWeightHistory(networkSpec, weight_update_link_count_idx);
            int numSteps = 100;
            float error = 0, firstError=0, minError = Float.MAX_VALUE;
            for (int i = 0;i < numSteps;i++)
            {

                FastLSTMNetwork.initializeNodeState(networkSpec);
                FastLSTMNetwork.forwardPass(networkSpec, feedforward_link_count_idx, initialInput);
                error = FastLSTMNetwork.updateForwardPassErrors(networkSpec, weight_update_link_count_idx, expected);
                System.out.println("i: " + i + ") Computed Error: [" + error + "], min Error: [" + minError+"]");
                if (i == 0)
                    firstError = error;
                FastLSTMNetwork.updateWeightsFromErrors(networkSpec, weight_update_link_count_idx, 0.5F, 1.2F, 50, 0, LSTMNetwork.WeightUpdateType.RPROP);
                minError = Math.min(minError, error);
            }

            long nativeTime = System.currentTimeMillis() - start;
            float[] outputNative = FastLSTMNetwork.getOutputActivation(networkSpec);
            System.out.println(" Takes (" + nativeTime + ") to map: " + Arrays.toString(initialInput) + "\n to : " + Arrays.toString(outputNative));
            errorMessage = "Failed to learn pattern";
            Assert.assertTrue(errorMessage, minError < firstError);
        }
        catch (Exception e)
        {
            Assert.assertTrue(e + "\n" + errorMessage, false);
        }
    }


    @Test
    public void testNodeStateInitialization()
    {
        String errorMessage = "Failed to create FastLSTM";
        int inputNodeCode = 1;
        int outputNodeCode = 1;
        int numMemoryCellStates = 7;



        try
        {
            FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);

            FastLSTMNetwork network = builder.build();
            float[] networkSpec = network._networkData;
            int feedforward_link_count_idx = network.feedforward_link_count_idx;
            int weight_update_link_count_idx = network.backprop_link_count_idx;


            float[] sequence = {1,0,1,1,0,1,1,1,0,0,1,0,1};

            errorMessage = "Failed to get node state snapshot";
            FastLSTMNetwork.initializeAllWeights(networkSpec, weight_update_link_count_idx, 0.012F, -1, 1);
            FastLSTMNetwork.resetWeightHistory(networkSpec, weight_update_link_count_idx);
            FastLSTMNetwork.initializeNodeState(networkSpec);

            float[] nextInput;

            //float[] initialNodeState = FastLSTMNetwork.getNodeStateSnapshot(networkSpec, feedforward_link_count_idx);

            errorMessage = "Failed to match node state snapshot";
            float[] firstOutput = null, output;
            for (int j = 0; j < sequence.length;j++)
            {
                nextInput = new float[] {sequence[j]};
                FastLSTMNetwork.forwardPass(networkSpec, feedforward_link_count_idx, nextInput);
                if (j == 0)
                    firstOutput = FastLSTMNetwork.getOutputActivation(networkSpec);
            }

            FastLSTMNetwork.resetWeightHistory(networkSpec, weight_update_link_count_idx);
            FastLSTMNetwork.initializeNodeState(networkSpec);
            nextInput = new float[] {sequence[0]};
            FastLSTMNetwork.forwardPass(networkSpec, feedforward_link_count_idx, nextInput);
            output = FastLSTMNetwork.getOutputActivation(networkSpec);

            //float[] newNodeState = FastLSTMNetwork.getNodeStateSnapshot(networkSpec, feedforward_link_count_idx);

            Assert.assertTrue(errorMessage, Arrays.equals(output, firstOutput));

        }
        catch (Exception e)
        {
            Assert.assertTrue(e + "\n" + errorMessage, false);
        }
    }



    @Test
    public void testLearningSequence()
    {
        String errorMessage = "Failed to create FastLSTM";
        int inputNodeCode = 1;
        int outputNodeCode = 1;
        int numMemoryCellStates = 7;



        try
        {
            FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);

            FastLSTMNetwork network = builder.build();
            float[] networkSpec = network._networkData;
            int feedforward_link_count_idx = network.feedforward_link_count_idx;
            int weight_update_link_count_idx = network.backprop_link_count_idx;


            int j;
            float[] sequence = {1,0,1,1,0,1,1,1,0,0,1,0,1};
            float[] expectedOutput = new float[sequence.length - 1], stepErrors = new float[sequence.length - 1];

            for (j = 0;j < sequence.length-1;j++)
                expectedOutput[j] = sequence[j+1];


            float errorTolerance;

            FastLSTMNetwork.initializeAllWeights(networkSpec, weight_update_link_count_idx, 0.012F, -1, 1);

            errorMessage = "Failure to execute learning passes";



            int numSteps = 1000;
            float error = 0, firstError=0, maxError = Float.MIN_VALUE, stepError;
            float[] nextInput, expectedNext, predictedNextValue;
            for (int i = 0;i < numSteps;i++)
            {

                FastLSTMNetwork.resetWeightHistory(networkSpec, weight_update_link_count_idx);
                FastLSTMNetwork.initializeNodeState(networkSpec);
                maxError = Float.MIN_VALUE;

                for (j = 0; j < sequence.length - 1;j++)
                {
                    nextInput = new float[] {sequence[j]};
                    expectedNext = new float[] {expectedOutput[j]};
                    FastLSTMNetwork.forwardPass(networkSpec, feedforward_link_count_idx, nextInput);
                    predictedNextValue = FastLSTMNetwork.getOutputActivation(networkSpec);
                    stepError = FastLSTMNetwork.updateForwardPassErrors(networkSpec, weight_update_link_count_idx, expectedNext);
                    System.out.println("input: " + nextInput[0] + " predicted next input: " + predictedNextValue[0] + " expected next: " + expectedNext[0] + " Error: " + stepError);

                    stepErrors[j] = stepError;
                    maxError = Math.max(maxError, stepError);
                }
                if (i == 0)
                    firstError = maxError;
                FastLSTMNetwork.updateWeightsFromErrors(networkSpec, weight_update_link_count_idx, 0.5F, 1.2F, 50, 0, LSTMNetwork.WeightUpdateType.DEFAULT);
                System.out.println(".*.*.*.*.* Finished Round: " + i + " max step error: " + maxError);
            }
            errorMessage = "Failed to improve upon initial error";
            Assert.assertTrue(errorMessage, maxError < firstError);

            int seedLength = 1;

            errorMessage = "Failed to extrapolate from seed = " + seedLength;

            FastLSTMNetwork.resetWeightHistory(networkSpec, weight_update_link_count_idx);
            FastLSTMNetwork.initializeNodeState(networkSpec);
            float[] extrapolated = new float[sequence.length - 1];
            for (j = 0; j < sequence.length - 1;j++)
            {
                if (j < seedLength)
                {
                    nextInput = new float[] {sequence[j]};
                }
                else
                {
                    nextInput = FastLSTMNetwork.getOutputActivation(networkSpec);
                }
                FastLSTMNetwork.forwardPass(networkSpec, feedforward_link_count_idx, nextInput);
                extrapolated[j] = nextInput[0];

            }

            System.out.println("extrapolated: " + Arrays.toString(extrapolated));
        }
        catch (Exception e)
        {
            Assert.assertTrue(e + "\n" + errorMessage, false);
        }
    }


    @Test
    public void testLearningSequenceWithResets()
    {
        String errorMessage = "Failed to create FastLSTM";
        int inputNodeCode = 1;
        int outputNodeCode = 1;
        int numMemoryCellStates = 7;



        try
        {
            FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);

            FastLSTMNetwork network = builder.build();
            float[] networkSpec = network._networkData;
            int feedforward_link_count_idx = network.feedforward_link_count_idx;
            int weight_update_link_count_idx = network.backprop_link_count_idx;

            float[] minErrorSequence = null;
            int j;
            float[] sequence = {0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0};
            float[] expectedOutput = new float[sequence.length - 1], stepErrors = new float[sequence.length - 1];

            for (j = 0;j < sequence.length-1;j++)
                expectedOutput[j] = sequence[j+1];



            double conv=0;
            FastLSTMNetwork.initializeAllWeights(networkSpec, weight_update_link_count_idx, 0.012F, -1, 1);

            errorMessage = "Failure to execute learning passes";


            LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.RPROP;
            int numSteps = 50000;
            float prevError = 0, firstError=0, maxError = Float.MIN_VALUE, stepError;
            float[] nextInput, expectedNext, predictedNextValue;
            double errorThreshold = 0.05, resetThreshold = 0.0001;
            float minMaxError = Float.MAX_VALUE;
            float[] predicted = new float[expectedOutput.length];
            for (int i = 0;i < numSteps;i++)
            {

                FastLSTMNetwork.resetWeightHistory(networkSpec, weight_update_link_count_idx);
                FastLSTMNetwork.initializeNodeState(networkSpec);
                maxError = Float.MIN_VALUE;

                for (j = 0; j < sequence.length - 1;j++)
                {
                    nextInput = new float[] {sequence[j]};
                    expectedNext = new float[] {expectedOutput[j]};
                    FastLSTMNetwork.forwardPass(networkSpec, feedforward_link_count_idx, nextInput);
                    predictedNextValue = FastLSTMNetwork.getOutputActivation(networkSpec);
                    stepError = FastLSTMNetwork.updateForwardPassErrors(networkSpec, weight_update_link_count_idx, expectedNext);
                    predicted[j] = predictedNextValue[0];
                    stepErrors[j] = stepError;
                    maxError = Math.max(maxError, stepError);
                }
                if (i == 0)
                {
                    firstError = maxError;
                    prevError = maxError;
                    conv = maxError;
                }
                else
                {
                    conv = Math.abs(maxError - prevError)/maxError;
                    prevError = maxError;
                    if (conv < resetThreshold)
                    {
                        System.out.println("Reseting weights: " + i);
                        FastLSTMNetwork.initializeAllWeights(networkSpec, weight_update_link_count_idx, 0.012F, -1, 1);
                    }
                    else if (maxError < errorThreshold)
                    {
                        minErrorSequence = Arrays.copyOf(networkSpec, networkSpec.length);
                        System.out.println("Found solution at: " + i + " out: " + Arrays.toString(predicted));
                        break;
                    }

                }

                if (maxError < minMaxError)
                {
                    minMaxError = maxError;
                    minErrorSequence = Arrays.copyOf(networkSpec, networkSpec.length);
                    System.out.println("Found new min error! " + minMaxError);
                }

                FastLSTMNetwork.updateWeightsFromErrors(networkSpec, weight_update_link_count_idx, 0.5F, 1.2F, 50, 0, updateType);
                System.out.println(".*.*.*.*.* Finished Round: " + i + " max step error: " + maxError + " conv error: " + conv);

            }
            errorMessage = "Failed to improve upon initial error";
            Assert.assertTrue(errorMessage, maxError < firstError);

            int seedLength = 1;

            errorMessage = "Failed to extrapolate from seed = " + seedLength;

            FastLSTMNetwork.resetWeightHistory(minErrorSequence, weight_update_link_count_idx);
            FastLSTMNetwork.initializeNodeState(minErrorSequence);
            float[] extrapolated = new float[sequence.length - 1];
            for (j = 0; j < sequence.length - 1;j++)
            {
                if (j < seedLength)
                {
                    nextInput = new float[] {sequence[j]};
                }
                else
                {
                    nextInput = FastLSTMNetwork.getOutputActivation(minErrorSequence);
                }
                FastLSTMNetwork.forwardPass(minErrorSequence, feedforward_link_count_idx, nextInput);
                extrapolated[j] = nextInput[0];

            }

            System.out.println("best extrapolation: " + Arrays.toString(extrapolated));
        }
        catch (Exception e)
        {
            Assert.assertTrue(e + "\n" + errorMessage, false);
        }
    }

}
