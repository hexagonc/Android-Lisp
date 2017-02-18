package com.evolved.automata.nn;

import com.evolved.automata.nn.grammar.GrammarStateMachine;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

import static org.junit.Assert.assertTrue;
/**
 * Created by Evolved8 on 2/13/17.
 */



public class FastLSTMTester extends BaseLSTMTester {



    @Test
    public void testStochasticStateMachine()
    {
        int maxLength = 20;
        String error = "Failed to create Reber grammar";
        try
        {
            GrammarStateMachine reber = makeReberGrammar();
            error = "Failed to construct grammar";
            int numSamples = 10;
            String n;
            for (int i = 0;i<numSamples;i++)
            {
                StringBuilder sample = new StringBuilder();
                int j = 0;

                while ((n = reber.next())!= null && j < maxLength)
                {
                    j++;
                    sample.append(n);

                }
                reber.reset();
                System.out.println(sample.toString());
            }
        }
        catch (Exception e)
        {
            Assert.assertTrue(e.getMessage(), false);
        }
    }

    @Test
    public void testStochasticEmbeddedStateMachine()
    {

        String error = "Failed to create Embedded Reber grammar";
        try
        {
            GrammarStateMachine embedded = makeEmbeddedReber();
            error = "Failed to construct grammar";
            int numSamples = 10;
            String n;
            for (int i = 0;i<numSamples;i++)
            {
                StringBuilder sample = new StringBuilder();

                while ((n = embedded.next())!=null)
                {
                    sample.append(n);
                }
                embedded.reset();
                System.out.println(sample.toString());
            }
        }
        catch (Exception e)
        {
            Assert.assertTrue(e.getMessage(), false);
        }
    }



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

        builder.setInputNodeCount(inputNodeCode, FastLSTMNetwork.MSE_ERROR_FUNCTION_ID, FastLSTMNetwork.SIGMOID_ACTIVATION_ID);
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
            FastLSTMNetwork.initializeAllWeights(networkSpec);

            errorMessage = "Failure to execute succinct feedforward";

            start = System.currentTimeMillis();
            FastLSTMNetwork.forwardPass(networkSpec, testSuccinctInput);
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
    public void testConstructTestTrainingSets()
    {
        String error = "Failure to create test sets";
        try
        {
            int totalTries = 100;
            int stepTries = 20;
            int traingSetSize = 256;
            int testSetSize = 256;

            FastLSTMNetwork.setSeed(20);

            GrammarStateMachine machine = makeEmbeddedReber();
            Pair<HashSet<String>, HashSet<String>> trainingPair = getTrainingTestGrammarSets(machine, traingSetSize, testSetSize, totalTries, stepTries);
            Assert.assertTrue(error, trainingPair != null);

            HashSet<String> trainingSet = trainingPair.getLeft();
            HashSet<String> testSet = trainingPair.getRight();

            error = "Failed to create requested training set size";

            Assert.assertTrue(error, trainingSet!=null && trainingSet.size() == traingSetSize);

            error = "Failed to create requested test set size";

            Assert.assertTrue(error, testSet!=null && testSet.size() == testSetSize);

            error = "Failed to create disjoint data sets";


            for (String key:trainingSet)
            {
                Assert.assertTrue(error, !testSet.contains(key));
            }

        }
        catch (Exception e)
        {
            Assert.assertTrue(error + ": " + e.toString(), false);
        }
    }




    @Test
    public void testSoftmaxAndCrossEntropy()
    {
        String error = "Failure to create test Reber string";

        try
        {
            // +++++++++++++++++++++++++++++++++++++++++++++
            // Construct grammar
            // +++++++++++++++++++++++++++++++++++++++++++++
            FastLSTMNetwork.setSeed(20);
            GrammarStateMachine machine = makeReberGrammar();
            String[] alphabet = ReberGrammar.ALPHABET;
            String testString = getGrammarString(machine);
            // ______________________________________________

            FastLSTMNetwork.setSeed(System.currentTimeMillis());


            // +++++++++++++++++++++++++++++++++++++++++++++
            // Converting test string to vector form
            float[][] dataInput =  getOneHotStringRep(testString, alphabet);
            // ______________________________________________



            System.out.println("Testing with: " + testString);
            error = "Failed to create proper vector string rep";
            Assert.assertTrue(error, testString.equals(getStringFromOneHotVectorRep(dataInput, alphabet)));

            ArrayList<Pair<Vector, Vector>> trainingPairs = getSequenceTrainingPairs(dataInput);


            // +++++++++++++++++++++++++++++++++++++++++++++
            //          Constructing Reber Grammar Character generation Neural network with Softmax and Cross Entropy
            // +++++++++++++++++++++++++++++++++++++++++++++
            int inputNodeCode = 7;
            int outputNodeCode = 7;
            int numMemoryCellStates = 10;
            LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.RPROP;

            FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);
            builder.setInputNodeCount(inputNodeCode, FastLSTMNetwork.CROSS_ENTROPY_ERROR_ID, FastLSTMNetwork.SOFTMAX_ACTIVATION_ID);
            FastLSTMNetwork network = builder.build();

            FastLSTMNetwork.setSeed(173545135270257L);
            float[] networkSpec = network._networkData;
            
            // ______________________________________________


            // +++++++++++++++++++++++++++++++++++++++++++++
            //          Set error parameters
            // +++++++++++++++++++++++++++++++++++++++++++++
            float maxAcceptableError = 0.1F, maxSegmentError, convergenceFraction, resetThresholdFraction = 0.0001F, prevError =0 , segmentError;
            float minTrainingError = Float.MAX_VALUE;
            float trainingPassError = 0;
            int maxLearningPasses = 10000;
            boolean afterWeightInitialization = true;
            float[] bestNetworkSoFar = networkSpec;
            int lastResetTrainingPassIndex = 0;
            // ______________________________________________

            // +++++++++++++++++++++++++++++++++++++++++++++
            //      Standard Training pass parameters
            // +++++++++++++++++++++++++++++++++++++++++++++
            Pair<Vector, Vector> trainingPair;
            float[] trainingInput, expectedOutput, actualOutput;
            long prevGeneratingSeed = FastLSTMNetwork.currentSeed, currentGeneratingSeed = prevGeneratingSeed;

            error = "Failed at learning";

            // +++++++++++++++++++++++++++++++++++++++++++++
            // Starting learning process
            // +++++++++++++++++++++++++++++++++++++++++++++

            System.out.println("Generating network from seed: " + currentGeneratingSeed);
            FastLSTMNetwork.initializeAllWeights(networkSpec);
            for (int trainingPass = 0; trainingPass < maxLearningPasses; trainingPass++)
            {
                FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
                maxSegmentError = Float.MIN_VALUE;
                for (int i = 0;i < trainingPairs.size();i++)
                {
                    trainingPair = trainingPairs.get(i);
                    trainingInput = NNTools.getVectorDataAsFloat(trainingPair.getLeft());
                    expectedOutput = NNTools.getVectorDataAsFloat(trainingPair.getRight());

                    FastLSTMNetwork.forwardPass(networkSpec, trainingInput);
                    actualOutput = FastLSTMNetwork.getOutputActivation(networkSpec);
                    segmentError = FastLSTMNetwork.updateForwardPassErrors(networkSpec, expectedOutput);

                    maxSegmentError = Math.max(maxSegmentError, segmentError);
                }

                trainingPassError = maxSegmentError;
                if (trainingPassError <= maxAcceptableError)
                {
                    segmentError = trainingPassError;
                    System.out.println("Learned pattern with seed: " + currentGeneratingSeed + " and error: " + segmentError);
                    break;
                }

                if (trainingPassError < minTrainingError)
                {

                    System.out.println("" + trainingPass + ") Found new lowest error! " + trainingPassError + " using seed: " + currentGeneratingSeed);
                    bestNetworkSoFar = Arrays.copyOf(networkSpec, networkSpec.length);

                    minTrainingError = trainingPassError;
                }

                // Update the weights from the calculated errors
                FastLSTMNetwork.updateWeightsFromErrors(networkSpec, updateType);
                if (afterWeightInitialization)
                {
                    afterWeightInitialization = false;
                }
                else
                {
                    convergenceFraction = Math.abs(prevError - trainingPassError)/trainingPassError;
                    if (convergenceFraction < resetThresholdFraction)
                    {
                        System.out.println("Resetting weights after " + (trainingPass - lastResetTrainingPassIndex) + " failing steps");

                        currentGeneratingSeed = FastLSTMNetwork.currentSeed;
                        FastLSTMNetwork.initializeAllWeights(networkSpec);
                        afterWeightInitialization = true;
                    }
                }
                prevError = trainingPassError;
            }

            if (trainingPassError > maxAcceptableError)
            {
                networkSpec = bestNetworkSoFar;
            }

            System.out.println("Testing extrapolation");
            // ++++++++++++++++++++++++++++++++++++
            //  Extrapolation
            // ++++++++++++++++++++++++++++++++++++


            FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
            int extrapLength = dataInput.length;
            float[][] extrapolatedOutput = new float[extrapLength][];

            // initialize the extrapolation input to the initial value from the original input
            // from this point forward, the network must predict the remaining inputs and the extrapInput
            // will be set to the network's own generated output
            float[] extrapInput = dataInput[0];

            for (int i = 0; i < extrapLength - 1;i++)
            {
                extrapolatedOutput[i] = extrapInput;
                FastLSTMNetwork.forwardPass(networkSpec, extrapInput);
                extrapInput = FastLSTMNetwork.getOutputActivation(networkSpec);
            }
            extrapolatedOutput[extrapLength - 1] = extrapInput;

            // +++++++++++++++++++
            // Test the Extrapolated output string
            String outputStringForm = getStringFromOneHotVectorRep(extrapolatedOutput, alphabet);
            Assert.assertTrue("Failed to learn pattern: '" + outputStringForm + "'!='" + testString + "'", outputStringForm.equals(testString));
        }
        catch (Exception e)
        {
            Assert.assertTrue(error + ": " + e.toString(), false);
        }
    }

    /*
    @Test
    public void testTeachingReberGrammar()
    {

        String error = "Failure to create test Reber sets";
        try
        {
            int totalTries = 100;
            int stepTries = 20;
            int traingSetSize = 256;
            int testSetSize = 256;

            FastLSTMNetwork.setSeed(20);

            //GrammarStateMachine machine = makeEmbeddedReber();
            GrammarStateMachine machine = makeReberGrammar();
            String[] alphabet = ReberGrammar.ALPHABET;

            Pair<HashSet<String>, HashSet<String>> trainingPair = getTrainingTestGrammarSets(machine, traingSetSize, testSetSize, totalTries, stepTries);

            Assert.assertTrue(error, trainingPair != null);
            HashSet<String> trainingSet = trainingPair.getLeft();
            HashSet<String> testSet = trainingPair.getRight();


            int inputNodeCode = 7;
            int outputNodeCode = 7;
            int numMemoryCellStates = 20;


            FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);
            builder.setInputNodeCount(inputNodeCode, FastLSTMNetwork.CROSS_ENTROPY_ERROR_ID, FastLSTMNetwork.SOFTMAX_ACTIVATION_ID);
            FastLSTMNetwork network = builder.build();

            float[] networkData = network._networkData;

            FastLSTMNetwork.setSeed(25);

            // *************************************
            // Training round
            // *************************************

            int maxTrainingIterations = 40000; // !
            int traininStep = 0;

            for (;traininStep < maxTrainingIterations; traininStep++)
            {
                for (String trainingExample: trainingSet)
                {
                    float[][] vectorTrainingForm = getOneHotStringRep(trainingExample, alphabet);
                    FastLSTMNetwork.in
                }


            }






        }
        catch (Exception e)
        {
            Assert.assertTrue(error + ": " + e.toString(), false);
        }
    }


    @Test
    public void testTeachingEmbeddedReberGrammar()
    {

        String error = "Failure to create test sets";
        try
        {
            int totalTries = 100;
            int stepTries = 20;
            int traingSetSize = 256;
            int testSetSize = 256;

            FastLSTMNetwork.setSeed(20);

            GrammarStateMachine machine = makeEmbeddedReber();
            //GrammarStateMachine machine = makeReberGrammar();
            Pair<HashSet<String>, HashSet<String>> trainingPair = getTrainingTestGrammarSets(machine, traingSetSize, testSetSize, totalTries, stepTries);


            Assert.assertTrue(error, trainingPair != null);

            error = "Failed to train network";
            HashSet<String> trainingSet = trainingPair.getLeft();
            HashSet<String> testSet = trainingPair.getRight();


            int inputNodeCode = 7;
            int outputNodeCode = 7;
            int numMemoryCellStates = 20;


            FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);
            builder.setInputNodeCount(inputNodeCode, FastLSTMNetwork.CROSS_ENTROPY_ERROR_ID, FastLSTMNetwork.SOFTMAX_ACTIVATION_ID);
            FastLSTMNetwork network = builder.build();

            float[] networkData = network._networkData;

            FastLSTMNetwork.setSeed(25);







        }
        catch (Exception e)
        {
            Assert.assertTrue(error + ": " + e.toString(), false);
        }
    }

*/
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

            float[] initialInput = new float[inputNodeCode], output = new float[outputNodeCode], expected = new float[outputNodeCode];
            initialInput[0] = 1;
            expected[inputNodeCode-1] = 1;


            long start = System.currentTimeMillis();



            FastLSTMNetwork.initializeAllWeights(networkSpec);

            errorMessage = "Failure to execute learning passes";

            start = System.currentTimeMillis();

            FastLSTMNetwork.resetWeightHistory(networkSpec);
            int numSteps = 100;
            float error = 0, firstError=0, minError = Float.MAX_VALUE;
            for (int i = 0;i < numSteps;i++)
            {

                FastLSTMNetwork.initializeNodeState(networkSpec);
                FastLSTMNetwork.forwardPass(networkSpec, initialInput);
                error = FastLSTMNetwork.updateForwardPassErrors(networkSpec, expected);
                System.out.println("i: " + i + ") Computed Error: [" + error + "], min Error: [" + minError+"]");
                if (i == 0)
                    firstError = error;
                FastLSTMNetwork.updateWeightsFromErrors(networkSpec, LSTMNetwork.WeightUpdateType.RPROP);
                minError = Math.min(minError, error);
            }

            long nativeTime = System.currentTimeMillis() - start;
            float[] outputNative = FastLSTMNetwork.getOutputActivation(networkSpec);
            System.out.println(" Takes (" + nativeTime + ") to map: " + Arrays.toString(initialInput) + "\n to : " + Arrays.toString(outputNative));
            errorMessage = "Failed to learn pattern";
            Assert.assertTrue(errorMessage, minError < 0.1);
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

            float[] sequence = {1,0,1,1,0,1,1,1,0,0,1,0,1};

            errorMessage = "Failed to get node state snapshot";
            FastLSTMNetwork.initializeAllWeights(networkSpec);
            FastLSTMNetwork.resetWeightHistory(networkSpec);
            FastLSTMNetwork.initializeNodeState(networkSpec);

            float[] nextInput;

            //float[] initialNodeState = FastLSTMNetwork.getNodeStateSnapshot(networkSpec, feedforward_link_count_idx);

            errorMessage = "Failed to match node state snapshot";
            float[] firstOutput = null, output;
            for (int j = 0; j < sequence.length;j++)
            {
                nextInput = new float[] {sequence[j]};
                FastLSTMNetwork.forwardPass(networkSpec, nextInput);
                if (j == 0)
                    firstOutput = FastLSTMNetwork.getOutputActivation(networkSpec);
            }

            FastLSTMNetwork.resetWeightHistory(networkSpec);
            FastLSTMNetwork.initializeNodeState(networkSpec);
            nextInput = new float[] {sequence[0]};
            FastLSTMNetwork.forwardPass(networkSpec, nextInput);
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


            int j;
            float[] sequence = {1,0,1,1,0,1,1,1,0,0,1,0,1};
            float[] expectedOutput = new float[sequence.length - 1], stepErrors = new float[sequence.length - 1];

            for (j = 0;j < sequence.length-1;j++)
                expectedOutput[j] = sequence[j+1];


            float errorTolerance;

            FastLSTMNetwork.initializeAllWeights(networkSpec);

            errorMessage = "Failure to execute learning passes";



            int numSteps = 1000;
            float error = 0, firstError=0, maxError = Float.MIN_VALUE, stepError;
            float[] nextInput, expectedNext, predictedNextValue;
            for (int i = 0;i < numSteps;i++)
            {

                FastLSTMNetwork.resetWeightHistory(networkSpec);
                FastLSTMNetwork.initializeNodeState(networkSpec);
                maxError = Float.MIN_VALUE;

                for (j = 0; j < sequence.length - 1;j++)
                {
                    nextInput = new float[] {sequence[j]};
                    expectedNext = new float[] {expectedOutput[j]};
                    FastLSTMNetwork.forwardPass(networkSpec, nextInput);
                    predictedNextValue = FastLSTMNetwork.getOutputActivation(networkSpec);
                    stepError = FastLSTMNetwork.updateForwardPassErrors(networkSpec, expectedNext);
                    System.out.println("input: " + nextInput[0] + " predicted next input: " + predictedNextValue[0] + " expected next: " + expectedNext[0] + " Error: " + stepError);

                    stepErrors[j] = stepError;
                    maxError = Math.max(maxError, stepError);
                }
                if (i == 0)
                    firstError = maxError;
                FastLSTMNetwork.updateWeightsFromErrors(networkSpec,  LSTMNetwork.WeightUpdateType.DEFAULT);
                System.out.println(".*.*.*.*.* Finished Round: " + i + " max step error: " + maxError);
            }
            errorMessage = "Failed to improve upon initial error";
            Assert.assertTrue(errorMessage, maxError < firstError);

            int seedLength = 1;

            errorMessage = "Failed to extrapolate from seed = " + seedLength;

            FastLSTMNetwork.resetWeightHistory(networkSpec);
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
                FastLSTMNetwork.forwardPass(networkSpec, nextInput);
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
    public void testRandomNumberGenerator()
    {
        long seed = 789656;
        //
        FastLSTMNetwork.setSeed(seed);
        int iterations = 1000;
        System.out.println("Generating random numbers with seed: " + seed);


        double prev = seed;
        double average = 0;
        for (int i = 0;i < iterations;i++)
        {
            double ran = FastLSTMNetwork.randomLCG();
            System.out.println("Random number is: " + ran);
            Assert.assertTrue("Numbers aren't random enough! Current value equals previous: " + ran, ran != prev);
            prev = ran;
            Assert.assertTrue("Random numbers out of range: " + ran, ran < 1 && ran >=0);
            average = (average*i + ran)/(i + 1);
        }

        // TODO: make this test more rigorous

        double expectedAverage = 0.5;
        double error = 0.01;
        Assert.assertTrue("Random numbers are probably not uniformly distributed from 0 to 1.  Average after " + iterations + " steps is: " + average, Math.abs(expectedAverage - average)<error);

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


            float[] minErrorSequence = null;
            int j;
            float[] sequence = {0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0};
            float[] expectedOutput = new float[sequence.length - 1], stepErrors = new float[sequence.length - 1];

            for (j = 0;j < sequence.length-1;j++)
                expectedOutput[j] = sequence[j+1];



            double conv=0;
            FastLSTMNetwork.setSeed(10);
            FastLSTMNetwork.initializeAllWeights(networkSpec);

            errorMessage = "Failure to execute learning passes";


            LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.RPROP;
            int numSteps = 50000;
            float prevError = 0, firstError=0, maxError = Float.MIN_VALUE, stepError;
            float[] nextInput, expectedNext, predictedNextValue;
            double errorThreshold = 0.1, resetThreshold = 0.0001;
            float minMaxError = Float.MAX_VALUE;
            float[] predicted = new float[expectedOutput.length];

            boolean firstAfterReset = true;
            int batchIndex = 0;
            boolean roundExtrapolated = true;
            for (int i = 0;i < numSteps;i++)
            {

                FastLSTMNetwork.resetNetworkToInitialState(networkSpec);

                maxError = Float.MIN_VALUE;

                for (j = 0; j < sequence.length - 1;j++)
                {
                    nextInput = new float[] {sequence[j]};
                    expectedNext = new float[] {expectedOutput[j]};
                    FastLSTMNetwork.forwardPass(networkSpec, nextInput);
                    predictedNextValue = FastLSTMNetwork.getOutputActivation(networkSpec);
                    stepError = FastLSTMNetwork.updateForwardPassErrors(networkSpec, expectedNext);
                    predicted[j] = predictedNextValue[0];
                    stepErrors[j] = stepError;
                    maxError = Math.max(maxError, stepError);
                }
                if (firstAfterReset)
                {
                    batchIndex = i;
                    firstAfterReset = false;
                    prevError = maxError;

                }
                else
                {
                    if (maxError < errorThreshold)
                    {
                        minErrorSequence = Arrays.copyOf(networkSpec, networkSpec.length);
                        System.out.println("Found solution at: " + i + " out: " + Arrays.toString(predicted));
                        break;
                    }
                    conv = Math.abs(maxError - prevError)/maxError;
                    prevError = maxError;
                    if (conv < resetThreshold)
                    {
                        System.out.println("Reseting weights after batch size: " + (i - batchIndex));
                        FastLSTMNetwork.initializeAllWeights(networkSpec);
                        firstAfterReset = true;
                    }


                }

                if (maxError < minMaxError)
                {
                    minMaxError = maxError;
                    minErrorSequence = Arrays.copyOf(networkSpec, networkSpec.length);
                    System.out.println("Found new min error! " + minMaxError);
                }

                FastLSTMNetwork.updateWeightsFromErrors(networkSpec, updateType);
                //System.out.println(".*.*.*.*.* Finished Round: " + i + " max step error: " + maxError + " conv error: " + conv);

            }
            errorMessage = "Failed to learn pattern";
            Assert.assertTrue(errorMessage, maxError <= errorThreshold);

            int seedLength = 1;

            errorMessage = "Failed to extrapolate from seed = " + seedLength;

            FastLSTMNetwork.resetNetworkToInitialState(minErrorSequence);

            // extrapolation
            float[] extrapolated = new float[sequence.length];
            float[] prediction = null;
            for (j = 0; j < sequence.length - 1;j++)
            {


                if (j < seedLength || prediction == null)
                {
                    nextInput = new float[] {sequence[j]};
                    extrapolated[j] = nextInput[0];
                    FastLSTMNetwork.forwardPass(minErrorSequence, nextInput);
                    prediction = FastLSTMNetwork.getOutputActivation(minErrorSequence);
                }
                else
                {
                    if (roundExtrapolated)
                        nextInput = new float[]{(float)network.roundingMapper.map(prediction[0], 0)};
                    else
                        nextInput = prediction;
                    extrapolated[j] = nextInput[0];
                    FastLSTMNetwork.forwardPass(minErrorSequence, nextInput);
                    prediction = FastLSTMNetwork.getOutputActivation(minErrorSequence);
                }

            }
            if (prediction != null)
            {
                if (roundExtrapolated)
                    extrapolated[sequence.length - 1] = (float)network.roundingMapper.map(prediction[0], 0);
                else
                    extrapolated[sequence.length - 1] = prediction[0];
            }


            System.out.println("Expected output: " + Arrays.toString(sequence));
            System.out.println("best extrapolation: " + Arrays.toString(extrapolated));
        }
        catch (Exception e)
        {
            Assert.assertTrue(e + "\n" + errorMessage, false);
        }
    }





}
