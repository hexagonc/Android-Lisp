package com.evolved.automata.nn;

import org.junit.Ignore;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;

import static org.junit.Assert.assertTrue;

/**
 * Created by Evolved8 on 12/10/16.
 */
public class LSTMTester {



    @Test
    public void testLSTMSerialization()
    {
        boolean success = false;
        String failureMessage = "";
        failureMessage = "Failed to create lstm";
        try
        {
            LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.RPROP;

            int memoryCellStateSize = 10;
            String[] feedforwardOrder = new String[]{"M-CO:M-IG", "M-CO:M-OG", "M-OG:M-IG", "M-CO:M-CI", "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-CI", "*:M-CI", "I:M-IG", "*:M-IG",  "I:M-OG",  "*:M-OG", "M-CO:O", "*:O"};
            String[] linkUpdateOrder= new String[]{"M-CO:O", "M-CO:M-IG", "M-CO:M-OG", "M-CO:M-CI", "M-OG:M-IG",  "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-IG", "I:M-OG", "I:M-CI"};

            LSTMNetwork.LSTMNetworkBuilder lstmBuilder =  LSTMNetwork.getBuilder();

            lstmBuilder.setInputNodeCount(1).setOutputNodeCount(1).addMemoryCell("M", memoryCellStateSize);
            lstmBuilder.addNodeConnections("I", new String[]{"M-CI", "M-IG", "M-OG"});
            lstmBuilder.addNodeConnections("M-OG", new String[]{"M-CI", "M-IG"});
            lstmBuilder.addNodeConnections("M-CO", new String[]{"O", "M-CI", "M-IG", "M-OG"});
            lstmBuilder.addNodeConnections("M-IG", new String[]{"M-CI", "M-OG"});
            lstmBuilder.addFeedForwardLinkOrder(feedforwardOrder);
            lstmBuilder.addWeightUpdateOrder(linkUpdateOrder);


            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.INITIAL_DELTA, 0.012);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MAX_DELTA, 50);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MIN_DELTA, 0);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MAX, 1.2);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MIN, 0.5);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.CONVERGENCE_THRESHOLD, 0.0001);
            lstmBuilder.setWeightUpdateType(updateType);


            LSTMNetwork lstm = lstmBuilder.build();
            failureMessage = "Failed to learn sequence";

            double[] testInput = new double[]{0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0};

            int maxSteps = 120000;
            double errorThreshold = 0.10;
            long start = System.currentTimeMillis();

            Vector[] input = getVector(testInput);
            double[] errors = lstm.learnSequence(input, maxSteps, errorThreshold);
            long end = System.currentTimeMillis() - start;
            System.out.println("Finished: (" + end + ") ms out: " + Arrays.toString(errors));
            failureMessage = "Failed to extrapolate sequence";


            double[] seed = new double[]{0};
            Vector[] seedV = getVector(seed);
            Vector[] out = lstm.extrapolate(seedV, 10, false);

            String extrapolatedValueString = Arrays.toString(out);
            System.out.println("Extrapolated: " + extrapolatedValueString);
            failureMessage = "Failed to serialize LSTM";

            // Now create new lstm
            String serializedNet = lstm.serializeLinkData();

            assertTrue(failureMessage, serializedNet !=null && serializedNet.length()>0);
            failureMessage = "Failed to create next lstm";

            lstmBuilder.setLinkData(serializedNet);
            LSTMNetwork lstmClone = lstmBuilder.build();

            failureMessage = "Failed to extrapolate result from clone";

            out = lstmClone.extrapolate(seedV, 10, false);

            String extrapolatedCloneValueString = Arrays.toString(out);
            System.out.println("Extrapolated clone: " + extrapolatedCloneValueString);
            assertTrue(failureMessage, extrapolatedCloneValueString.equals(extrapolatedValueString));


            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();

        }
        assertTrue(failureMessage, success);

    }


    @Test
    public void testSingleDimensionalNonPeepholeSequenceLearning()
    {
        boolean success = false;
        String failureMessage = "";
        failureMessage = "Failed to create lstm";
        try
        {
            LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.DEFAULT;
            // or LSTMNetwork.WeightUpdateType.DEFAULT
            //  LSTMNetwork.WeightUpdateType.RPROP;
            int memoryCellStateSize = 10;
            String[] feedforwardOrder = new String[]{"M-CO:M-IG", "M-CO:M-OG", "M-OG:M-IG", "M-CO:M-CI", "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-CI", "*:M-CI", "I:M-IG", "*:M-IG",  "I:M-OG",  "*:M-OG", "M-CO:O", "*:O"};
            String[] linkUpdateOrder= new String[]{"M-CO:O", "M-CO:M-IG", "M-CO:M-OG", "M-CO:M-CI", "M-OG:M-IG",  "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-IG", "I:M-OG", "I:M-CI"};

            LSTMNetwork.LSTMNetworkBuilder lstmBuilder =  LSTMNetwork.getBuilder();

            lstmBuilder.setInputNodeCount(1).setOutputNodeCount(1).addMemoryCell("M", memoryCellStateSize);
            lstmBuilder.addNodeConnections("I", new String[]{"M-CI", "M-IG", "M-OG"});
            lstmBuilder.addNodeConnections("M-OG", new String[]{"M-CI", "M-IG"});
            lstmBuilder.addNodeConnections("M-CO", new String[]{"O", "M-CI", "M-IG", "M-OG"});
            lstmBuilder.addNodeConnections("M-IG", new String[]{"M-CI", "M-OG"});
            lstmBuilder.addFeedForwardLinkOrder(feedforwardOrder);
            lstmBuilder.addWeightUpdateOrder(linkUpdateOrder);


            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.INITIAL_DELTA, 0.012);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MAX_DELTA, 50);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MIN_DELTA, 0);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MAX, 1.2);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MIN, 0.5);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.CONVERGENCE_THRESHOLD, 0.0001);
            lstmBuilder.setWeightUpdateType(updateType);

            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.LEARNING_RATE, 1.2);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MOMENTUM, 0.25);


            LSTMNetwork lstm = lstmBuilder.build();
            failureMessage = "Failed to learn sequence";

            double[] testInput = new double[]{0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0};

            int maxSteps = 120000;
            double errorThreshold = 0.13;
            long start = System.currentTimeMillis();

            Vector[] input = getVector(testInput);
            double[] errors = lstm.learnSequence(input, maxSteps, errorThreshold);
            long end = System.currentTimeMillis() - start;
            System.out.println("Finished: (" + end + ") ms out: " + Arrays.toString(errors));
            failureMessage = "Failed to extrapolate sequence";


            double[] seed = new double[]{0};
            Vector[] seedV = getVector(seed);
            Vector[] out = lstm.extrapolate(seedV, 10, false);

            System.out.println("Extrapolated: " + Arrays.toString(out));
            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();

        }
        assertTrue(failureMessage, success);

    }

    @Ignore
    @Test
    public void testSingleDimensionalNonPeepholeSequenceLearningWithWeightPerturbation()
    {
        boolean success = false;
        String failureMessage = "";
        failureMessage = "Failed to create lstm";
        try
        {
            LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.DEFAULT;
            // or LSTMNetwork.WeightUpdateType.DEFAULT
            //  LSTMNetwork.WeightUpdateType.RPROP;
            int memoryCellStateSize = 10;
            String[] feedforwardOrder = new String[]{"M-CO:M-IG", "M-CO:M-OG", "M-OG:M-IG", "M-CO:M-CI", "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-CI", "*:M-CI", "I:M-IG", "*:M-IG",  "I:M-OG",  "*:M-OG", "M-CO:O", "*:O"};
            String[] linkUpdateOrder= new String[]{"M-CO:O", "M-CO:M-IG", "M-CO:M-OG", "M-CO:M-CI", "M-OG:M-IG",  "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-IG", "I:M-OG", "I:M-CI"};

            LSTMNetwork.LSTMNetworkBuilder lstmBuilder =  LSTMNetwork.getBuilder();

            lstmBuilder.setInputNodeCount(1).setOutputNodeCount(1).addMemoryCell("M", memoryCellStateSize);
            lstmBuilder.addNodeConnections("I", new String[]{"M-CI", "M-IG", "M-OG"});
            lstmBuilder.addNodeConnections("M-OG", new String[]{"M-CI", "M-IG"});
            lstmBuilder.addNodeConnections("M-CO", new String[]{"O", "M-CI", "M-IG", "M-OG"});
            lstmBuilder.addNodeConnections("M-IG", new String[]{"M-CI", "M-OG"});
            lstmBuilder.addFeedForwardLinkOrder(feedforwardOrder);
            lstmBuilder.addWeightUpdateOrder(linkUpdateOrder);


            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.INITIAL_DELTA, 0.012);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MAX_DELTA, 50);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MIN_DELTA, 0);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MAX, 1.2);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MIN, 0.5);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.CONVERGENCE_THRESHOLD, 0.0001);
            lstmBuilder.setWeightUpdateType(updateType);

            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.LEARNING_RATE, 0.5);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MOMENTUM, 0.25);


            LSTMNetwork lstm = lstmBuilder.build();
            lstm.setWeightRollStrategy(Link.RANDOMIZATION_SCHEME.PERTRUBATION);
            failureMessage = "Failed to learn sequence";

            double[] testInput = new double[]{0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0};

            int maxSteps = 60000;
            double errorThreshold = 0.1;
            long start = System.currentTimeMillis();

            Vector[] input = getVector(testInput);
            double[] errors = lstm.learnSequence(input, maxSteps, errorThreshold);
            long end = System.currentTimeMillis() - start;
            System.out.println("Finished: (" + end + ") ms out: " + Arrays.toString(errors));
            failureMessage = "Failed to extrapolate sequence";


            double[] seed = new double[]{0};
            Vector[] seedV = getVector(seed);
            Vector[] out = lstm.extrapolate(seedV, 10, false);

            System.out.println("Extrapolated: " + Arrays.toString(out));
            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();

        }
        assertTrue(failureMessage, success);

    }


    @Test
    public void testSingleDimensionalWithPeepholesSequenceLearning()
    {
        boolean success = false;
        String failureMessage = "";
        failureMessage = "Failed to create lstm";
        try
        {
            LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.DEFAULT;
            // or LSTMNetwork.WeightUpdateType.DEFAULT
            //  LSTMNetwork.WeightUpdateType.RPROP;
            int memoryCellStateSize = 10;
            String[] feedforwardOrder = new String[]{"M-CO:M-IG", "M-CO:M-OG", "M-OG:M-IG", "M-CO:M-CI", "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-CI", "*:M-CI", "I:M-IG", "M-P:M-IG", "*:M-IG",  "I:M-OG", "M-P:M-OG",  "*:M-OG", "M-CO:O", "*:O"};
            String[] linkUpdateOrder= new String[]{"M-CO:O", "M-P:M-OG", "M-P:M-IG", "M-CO:M-IG", "M-CO:M-OG", "M-CO:M-CI", "M-OG:M-IG",  "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-IG", "I:M-OG", "I:M-CI"};

            LSTMNetwork.LSTMNetworkBuilder lstmBuilder =  LSTMNetwork.getBuilder();

            lstmBuilder.setInputNodeCount(1).setOutputNodeCount(1).addMemoryCell("M", memoryCellStateSize);
            lstmBuilder.addNodeConnections("I", new String[]{"M-CI", "M-IG", "M-OG"});
            lstmBuilder.addNodeConnections("M-OG", new String[]{"M-CI", "M-IG"});
            lstmBuilder.addNodeConnections("M-CO", new String[]{"O", "M-CI", "M-IG", "M-OG"});
            lstmBuilder.addNodeConnections("M-P", new String[]{"M-IG", "M-OG"});
            lstmBuilder.addNodeConnections("M-IG", new String[]{"M-CI", "M-OG"});
            lstmBuilder.addFeedForwardLinkOrder(feedforwardOrder);
            lstmBuilder.addWeightUpdateOrder(linkUpdateOrder);


            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.INITIAL_DELTA, 0.012);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MAX_DELTA, 50);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MIN_DELTA, 0);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MAX, 1.2);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MIN, 0.5);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.CONVERGENCE_THRESHOLD, 0.001);
            lstmBuilder.setWeightUpdateType(updateType);

            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.LEARNING_RATE, 1.0);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MOMENTUM, 0.25);


            LSTMNetwork lstm = lstmBuilder.build();
            failureMessage = "Failed to learn sequence";

            double[] testInput = new double[]{0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0};

            int maxSteps = 60000;
            double errorThreshold = 0.1;
            long start = System.currentTimeMillis();

            Vector[] input = getVector(testInput);
            double[] errors = lstm.learnSequence(input, maxSteps, errorThreshold);
            long end = System.currentTimeMillis() - start;
            System.out.println("Finished: (" + end + ") ms out: " + Arrays.toString(errors));
            failureMessage = "Failed to extrapolate sequence";


            double[] seed = new double[]{0};
            Vector[] seedV = getVector(seed);
            Vector[] out = lstm.extrapolate(seedV, 10, false);

            System.out.println("Extrapolated: " + Arrays.toString(out));
            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();

        }
        assertTrue(failureMessage, success);

    }


    @Test
    public void testSingleDimensionalWithForgetNonPeepholeSequenceLearning()
    {
        boolean success = false;
        String failureMessage = "";
        failureMessage = "Failed to create lstm";
        try
        {
            LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.RPROP;
            // or LSTMNetwork.WeightUpdateType.DEFAULT
            //  LSTMNetwork.WeightUpdateType.RPROP;
            int memoryCellStateSize = 10;
            String[] feedforwardOrder = new String[]{ "M-FG:M-OG", "M-FG:M-IG", "I:M-FG", "M-OG:M-FG", "M-CO:M-FG",  "M-IG:M-FG",  "*:M-FG", "M-CO:M-IG", "M-CO:M-OG", "M-OG:M-IG", "M-CO:M-CI", "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-CI", "*:M-CI", "I:M-IG", "*:M-IG",  "I:M-OG",  "*:M-OG", "M-CO:O", "*:O"};
            String[] linkUpdateOrder= new String[]{"M-CO:O", "M-OG:M-FG", "M-IG:M-FG", "M-CO:M-FG", "M-FG:M-OG", "M-FG:M-IG", "I:M-FG",  "M-CO:M-IG", "M-CO:M-OG", "M-CO:M-CI", "M-OG:M-IG",  "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-IG", "I:M-OG", "I:M-CI"};

            LSTMNetwork.LSTMNetworkBuilder lstmBuilder =  LSTMNetwork.getBuilder();

            lstmBuilder.setInputNodeCount(1).setOutputNodeCount(1).addMemoryCell("M", memoryCellStateSize);
            lstmBuilder.addNodeConnections("I", new String[]{"M-CI", "M-IG", "M-OG", "M-FG"});
            lstmBuilder.addNodeConnections("M-FG", new String[]{"M-OG", "M-IG"});
            lstmBuilder.addNodeConnections("M-OG", new String[]{"M-CI", "M-IG", "M-FG"});
            lstmBuilder.addNodeConnections("M-CO", new String[]{"O", "M-CI", "M-IG", "M-OG", "M-FG"});
            lstmBuilder.addNodeConnections("M-IG", new String[]{"M-CI", "M-OG", "M-FG"});
            lstmBuilder.addFeedForwardLinkOrder(feedforwardOrder);
            lstmBuilder.addWeightUpdateOrder(linkUpdateOrder);


            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.INITIAL_DELTA, 0.012);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MAX_DELTA, 50);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MIN_DELTA, 0);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MAX, 1.2);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MIN, 0.5);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.CONVERGENCE_THRESHOLD, 0.001);
            lstmBuilder.setWeightUpdateType(updateType);

            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.LEARNING_RATE, 0.5);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MOMENTUM, 0.25);


            LSTMNetwork lstm = lstmBuilder.build();
            failureMessage = "Failed to learn sequence";

            double[] testInput = new double[]{0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0};

            int maxSteps = 60000;
            double errorThreshold = 0.1;
            long start = System.currentTimeMillis();

            Vector[] input = getVector(testInput);
            double[] errors = lstm.learnSequence(input, maxSteps, errorThreshold);
            long end = System.currentTimeMillis() - start;
            System.out.println("Finished: (" + end + ") ms out: " + Arrays.toString(errors));
            failureMessage = "Failed to extrapolate sequence";


            double[] seed = new double[]{0};
            Vector[] seedV = getVector(seed);
            Vector[] out = lstm.extrapolate(seedV, 10, false);

            System.out.println("Extrapolated: " + Arrays.toString(out));
            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();

        }
        assertTrue(failureMessage, success);

    }



    @Test
    public void testSingleDimensionalWithForgetAndPeepholesSequenceLearning()
    {
        boolean success = false;
        String failureMessage = "";
        failureMessage = "Failed to create lstm";
        try
        {
            LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.RPROP;
            // or LSTMNetwork.WeightUpdateType.DEFAULT
            //  LSTMNetwork.WeightUpdateType.RPROP;
            int memoryCellStateSize = 10;
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
            String[] linkUpdateOrder= new String[]{"M-CO:O", "M-OG:M-FG", "M-IG:M-FG", "M-CO:M-FG", "M-FG:M-OG", "M-FG:M-IG", "I:M-FG",  "M-CO:M-IG", "M-CO:M-OG", "M-CO:M-CI", "M-OG:M-IG",  "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "M-P:M-FG", "M-P:M-IG", "M-P:M-OG", "I:M-IG", "I:M-OG", "I:M-CI"};

            LSTMNetwork.LSTMNetworkBuilder lstmBuilder =  LSTMNetwork.getBuilder();

            lstmBuilder.setInputNodeCount(1).setOutputNodeCount(1).addMemoryCell("M", memoryCellStateSize);
            lstmBuilder.addNodeConnections("I", new String[]{"M-CI", "M-IG", "M-OG", "M-FG"});
            lstmBuilder.addNodeConnections("M-FG", new String[]{"M-OG", "M-IG"});
            lstmBuilder.addNodeConnections("M-OG", new String[]{"M-CI", "M-IG", "M-FG"});
            lstmBuilder.addNodeConnections("M-CO", new String[]{"O", "M-CI", "M-IG", "M-OG", "M-FG"});
            lstmBuilder.addNodeConnections("M-IG", new String[]{"M-CI", "M-OG", "M-FG"});
            lstmBuilder.addNodeConnections("M-P", new String[]{ "M-OG", "M-FG", "M-IG"});

            lstmBuilder.addFeedForwardLinkOrder(feedforwardOrder);
            lstmBuilder.addWeightUpdateOrder(linkUpdateOrder);


            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.INITIAL_DELTA, 0.012);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MAX_DELTA, 50);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MIN_DELTA, 0);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MAX, 1.2);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MIN, 0.5);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.CONVERGENCE_THRESHOLD, 0.001);
            lstmBuilder.setWeightUpdateType(updateType);

            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.LEARNING_RATE, 0.5);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MOMENTUM, 0.25);


            LSTMNetwork lstm = lstmBuilder.build();
            failureMessage = "Failed to learn sequence";

            double[] testInput = new double[]{0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0};

            int maxSteps = 60000;
            double errorThreshold = 0.1;
            long start = System.currentTimeMillis();

            Vector[] input = getVector(testInput);
            double[] errors = lstm.learnSequence(input, maxSteps, errorThreshold);
            long end = System.currentTimeMillis() - start;
            System.out.println("Finished: (" + end + ") ms out: " + Arrays.toString(errors));
            failureMessage = "Failed to extrapolate sequence";


            double[] seed = new double[]{0};
            Vector[] seedV = getVector(seed);
            Vector[] out = lstm.extrapolate(seedV, 10, false);

            System.out.println("Extrapolated: " + Arrays.toString(out));
            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();

        }
        assertTrue(failureMessage, success);

    }

    @Test
    public void testMultiDimensionalNonPeepholeSequenceLearning()
    {
        boolean success = false;
        String failureMessage = "";
        failureMessage = "Failed to create lstm";
        try
        {

            double[][] sequence = new double[][]{{0,0,0,1,1,1,0,1},{0,0,0,1,1,0,1,1},{0,0,0,1,1,0,0,0},{0,0,1,1,0,1,1,1},{0,0,0,1,1,0,1,1},{0,0,0,1,1,0,0,0},{0,0,0,1,1,0,1,1},{0,0,1,1,0,0,1,0},{0,0,1,1,1,0,1,1},{0,0,1,1,0,1,1,1},{0,0,1,1,0,0,0,1},{0,0,0,1,1,1,0,1}};
            LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.RPROP;
            // or LSTMNetwork.WeightUpdateType.DEFAULT
            //  LSTMNetwork.WeightUpdateType.RPROP;
            int memoryCellStateSize = 10;
            String[] feedforwardOrder = new String[]{"M-CO:M-IG", "M-CO:M-OG", "M-OG:M-IG", "M-CO:M-CI", "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-CI", "*:M-CI", "I:M-IG", "*:M-IG",  "I:M-OG",  "*:M-OG", "M-CO:O", "*:O"};
            String[] linkUpdateOrder= new String[]{"M-CO:O", "M-CO:M-IG", "M-CO:M-OG", "M-CO:M-CI", "M-OG:M-IG",  "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-IG", "I:M-OG", "I:M-CI"};

            LSTMNetwork.LSTMNetworkBuilder lstmBuilder =  LSTMNetwork.getBuilder();

            int size = sequence[0].length;
            lstmBuilder.setInputNodeCount(size).setOutputNodeCount(size).addMemoryCell("M", memoryCellStateSize);
            lstmBuilder.addNodeConnections("I", new String[]{"M-CI", "M-IG", "M-OG"});
            lstmBuilder.addNodeConnections("M-OG", new String[]{"M-CI", "M-IG"});
            lstmBuilder.addNodeConnections("M-CO", new String[]{"O", "M-CI", "M-IG", "M-OG"});
            lstmBuilder.addNodeConnections("M-IG", new String[]{"M-CI", "M-OG"});
            lstmBuilder.addFeedForwardLinkOrder(feedforwardOrder);
            lstmBuilder.addWeightUpdateOrder(linkUpdateOrder);


            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.INITIAL_DELTA, 0.012);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MAX_DELTA, 50);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MIN_DELTA, 0);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MAX, 1.2);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MIN, 0.5);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.CONVERGENCE_THRESHOLD, 0.001);
            lstmBuilder.setWeightUpdateType(updateType);

            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.LEARNING_RATE, 0.5);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MOMENTUM, 0.25);


            LSTMNetwork lstm = lstmBuilder.build();
            failureMessage = "Failed to learn sequence";


            int maxSteps = 60000;
            double errorThreshold = 0.1;
            long start = System.currentTimeMillis();

            Vector[] input = getVector(sequence);
            double[] errors = lstm.learnSequence(input, maxSteps, errorThreshold);
            long end = System.currentTimeMillis() - start;
            System.out.println("Finished: (" + end + ") ms out: " + Arrays.toString(errors));
            failureMessage = "Failed to extrapolate sequence";


            double[][] seed = new double[][]{{0,0,0,1,1,1,0,1}};
            Vector[] seedV = getVector(seed);
            Vector[] out = lstm.extrapolate(seedV, 10, false);

            System.out.println("Extrapolated: " + Arrays.toString(out));
            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();

        }
        assertTrue(failureMessage, success);

    }

    @Test
    public void testSimilarMultiDimensionalNonPeepholeSequenceLearning()
    {
        boolean success = false;
        String failureMessage = "";
        failureMessage = "Failed to create lstm";
        try
        {

            double[][] sequence = new double[][]{{0,0,0,1,1,1,0,1},{0,0,0,1,1,0,1,1},{0,0,0,1,1,0,0,0},{0,0,1,1,0,1,1,1},{0,0,0,1,1,0,1,1},{0,0,0,1,1,0,0,0},{0,0,0,1,1,0,1,1},{0,0,1,1,0,0,1,0},{0,0,1,1,1,0,1,1},{0,0,1,1,0,1,1,1},{0,0,1,1,0,0,0,1},{0,0,0,1,1,1,0,1}};
            LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.RPROP;
            // or LSTMNetwork.WeightUpdateType.DEFAULT
            //  LSTMNetwork.WeightUpdateType.RPROP;
            int memoryCellStateSize = 10;
            String[] feedforwardOrder = new String[]{"M-CO:M-IG", "M-CO:M-OG", "M-OG:M-IG", "M-CO:M-CI", "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-CI", "*:M-CI", "I:M-IG", "*:M-IG",  "I:M-OG",  "*:M-OG", "M-CO:O", "*:O"};
            String[] linkUpdateOrder= new String[]{"M-CO:O", "M-CO:M-IG", "M-CO:M-OG", "M-CO:M-CI", "M-OG:M-IG",  "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-IG", "I:M-OG", "I:M-CI"};

            LSTMNetwork.LSTMNetworkBuilder lstmBuilder =  LSTMNetwork.getBuilder();

            int size = sequence[0].length;
            lstmBuilder.setInputNodeCount(size).setOutputNodeCount(size).addMemoryCell("M", memoryCellStateSize);
            lstmBuilder.addNodeConnections("I", new String[]{"M-CI", "M-IG", "M-OG"});
            lstmBuilder.addNodeConnections("M-OG", new String[]{"M-CI", "M-IG"});
            lstmBuilder.addNodeConnections("M-CO", new String[]{"O", "M-CI", "M-IG", "M-OG"});
            lstmBuilder.addNodeConnections("M-IG", new String[]{"M-CI", "M-OG"});
            lstmBuilder.addFeedForwardLinkOrder(feedforwardOrder);
            lstmBuilder.addWeightUpdateOrder(linkUpdateOrder);


            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.INITIAL_DELTA, 0.012);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MAX_DELTA, 50);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MIN_DELTA, 0);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MAX, 1.2);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MIN, 0.5);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.CONVERGENCE_THRESHOLD, 0.001);
            lstmBuilder.setWeightUpdateType(updateType);

            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.LEARNING_RATE, 0.5);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MOMENTUM, 0.25);


            LSTMNetwork lstm = lstmBuilder.build();
            failureMessage = "Failed to learn sequence";


            int maxSteps = 60000;
            double errorThreshold = 0.1;
            long start = System.currentTimeMillis();

            Vector[] input = getVector(sequence);
            double[] errors = lstm.learnSequence(input, maxSteps, errorThreshold);
            long end = System.currentTimeMillis() - start;
            System.out.println("Finished: (" + end + ") ms out: " + Arrays.toString(errors));
            failureMessage = "Failed to extrapolate sequence";


            double[][] seed = new double[][]{{0, 0, 0, 1, 0, 1, 0, 0}};
            Vector[] seedV = getVector(seed);
            Vector[] out = lstm.extrapolate(seedV, 10, false);

            System.out.println("Extrapolated: " + Arrays.toString(out));


            //
            double[][] shiftedSequence = new double[][]{{0,0,1,1,0,1,1,1},{0,0,0,1,1,0,1,1},{0,0,0,1,1,0,0,0}};
            seedV = getVector(shiftedSequence);

            int driveCount = 5;

            for (int i = 0;i<driveCount;i++)
            {
                if (i == 0)
                    lstm.extrapolate(seedV, 0, false);
                else
                    lstm.extrapolate(seedV, 0, true);
            }

            out = lstm.extrapolate(seedV, 10, true);
            System.out.println("Extrapolated after reseeding with (15, 12, 11)" + Arrays.toString(out));
            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();

        }
        assertTrue(failureMessage, success);

    }



    @Test
    public void testSimilarMultiDimensionalNonPeepholeSequenceRecognition()
    {
        boolean success = false;
        String failureMessage = "";
        failureMessage = "Failed to create lstm";
        try
        {
            int discretizationRange = 100;
            int discretizationSteps = 4;

            double[] oddSequence = new double[]{1,3,5,7};
            double[] evenSequence = new double[]{2, 4, 6, 8};

            double[][] sequenceSet = new double[][]{oddSequence, evenSequence};

            LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.RPROP;

            int memoryCellStateSize = 10;
            String[] feedforwardOrder = new String[]{"M-CO:M-IG", "M-CO:M-OG", "M-OG:M-IG", "M-CO:M-CI", "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-CI", "*:M-CI", "I:M-IG", "*:M-IG",  "I:M-OG",  "*:M-OG", "M-CO:O", "*:O"};
            String[] linkUpdateOrder= new String[]{"M-CO:O", "M-CO:M-IG", "M-CO:M-OG", "M-CO:M-CI", "M-OG:M-IG",  "M-OG:M-CI", "M-IG:M-OG", "M-IG:M-CI", "I:M-IG", "I:M-OG", "I:M-CI"};

            LSTMNetwork.LSTMNetworkBuilder lstmBuilder =  LSTMNetwork.getBuilder();


            lstmBuilder.setInputNodeCount(discretizationSteps*2).setOutputNodeCount(sequenceSet.length).addMemoryCell("M", memoryCellStateSize);
            lstmBuilder.addNodeConnections("I", new String[]{"M-CI", "M-IG", "M-OG"});
            lstmBuilder.addNodeConnections("M-OG", new String[]{"M-CI", "M-IG"});
            lstmBuilder.addNodeConnections("M-CO", new String[]{"O", "M-CI", "M-IG", "M-OG"});
            lstmBuilder.addNodeConnections("M-IG", new String[]{"M-CI", "M-OG"});
            lstmBuilder.addFeedForwardLinkOrder(feedforwardOrder);
            lstmBuilder.addWeightUpdateOrder(linkUpdateOrder);


            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.INITIAL_DELTA, 0.012);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MAX_DELTA, 50);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.MIN_DELTA, 0);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MAX, 1.2);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.N_MIN, 0.5);
            lstmBuilder.addWeightParameter(LSTMNetwork.WeightUpdateParameters.CONVERGENCE_THRESHOLD, 0.001);
            lstmBuilder.setWeightUpdateType(updateType);


            LSTMNetwork lstm = lstmBuilder.build();
            failureMessage = "Failed to learn classes";

            int maxSteps = 60000;
            double errorThreshold = 0.1;

            double[] errors = null;

            long start, end;

            int patternCount = sequenceSet.length;
            int patternIndex;
            double[] pattern;
            ArrayList<Double> inputRaw;


            Vector[][] trainingInput = new Vector[patternCount][];
            Vector[]  patternInput = null;
            int[] classIds = new int[patternCount];

            for (int i = 0;i<classIds.length;i++)
            {
                classIds[i] = (i + 1);
            }

            for (patternIndex = 0;patternIndex< patternCount;patternIndex++)
            {
                pattern = sequenceSet[patternIndex];
                patternInput = new Vector[pattern.length];


                for (int j = 0;j < pattern.length;j++)
                {
                    inputRaw = NNTools.stageDiscretize(pattern[j], discretizationRange, discretizationSteps);
                    patternInput[j] = getVector(inputRaw);
                }
                trainingInput[patternIndex] = patternInput;
                classIds[patternIndex] = classIds[patternIndex];

            }

            start = System.currentTimeMillis();
            errors = lstm.learnPatternClass(trainingInput, classIds, patternCount, maxSteps, errorThreshold);
            end = System.currentTimeMillis() - start;
            System.out.println("Finished: (" + end + ") ms out: " + Arrays.toString(errors));



            failureMessage = "Failed to learn patterns";

            double[] sampleSequence = oddSequence;

            Vector[] identifiedPattern = new Vector[sampleSequence.length];
            Vector[] samplePattern = new Vector[sampleSequence.length];

            for (int j = 0;j < samplePattern.length;j++)
            {
                inputRaw = NNTools.stageDiscretize(sampleSequence[j], discretizationRange, discretizationSteps);
                samplePattern[j] = getVector(inputRaw);
            }

            identifiedPattern = lstm.viewOutput(samplePattern,false );

            System.out.println("Identified classes: " + Arrays.toString(identifiedPattern));
            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();

        }
        assertTrue(failureMessage, success);

    }


    Vector[] getVector(double[] data)
    {
        Vector[] out = new Vector[data.length];
        for (int i = 0;i < data.length; i++)
        {
            out[i] = new Vector(new double[]{data[i]});
        }
        return out;
    }

    Vector getVector(ArrayList<Double> data)
    {
        double[] out = new double[data.size()];
        for (int i = 0;i < data.size(); i++)
        {
            out[i] = data.get(i);
        }
        return new Vector(out);
    }


    Vector[] getVector(double[][] data)
    {
        Vector[] out = new Vector[data.length];
        for (int i = 0;i < data.length; i++)
        {
            out[i] = new Vector(data[i]);
        }
        return out;
    }






}
