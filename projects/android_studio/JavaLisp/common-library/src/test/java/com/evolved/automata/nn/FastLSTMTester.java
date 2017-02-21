package com.evolved.automata.nn;

import com.evolved.automata.nn.grammar.GrammarStateMachine;
import com.evolved.automata.nn.grammar.MatchStatus;
import com.sun.org.apache.xerces.internal.impl.xpath.regex.Match;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;

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




    @Test
    public void testProbabilisticSampling()
    {


        float[] classes = new float[]{12, 23, 56, 7};
        float[] adjustedClasses = new float[]{12, 23, 56, 7};
        int numTrials = 10000;
        float currentWeightSum = 0, adjustedWeightSum;
        float minWeightFraction = 0.1F;
        int numClasses = classes.length;
        boolean failureOnAllZeroP = true;

        float[] expectedFraction = new float[classes.length];
        for (int k = 0;k < numClasses;k++)
        {
            currentWeightSum +=classes[k];
        }

        adjustedWeightSum = currentWeightSum;
        for (int k = 0;k < numClasses;k++)
        {

            if (classes[k]/currentWeightSum < minWeightFraction)
            {
                adjustedClasses[k] = 0;
                adjustedWeightSum-=classes[k];
            }
        }


        for (int k = 0;k < numClasses;k++)
        {
            if (adjustedWeightSum == 0)
            {
                if (failureOnAllZeroP)
                    expectedFraction[k] = 0;
                else
                    expectedFraction[k] = 1F/numClasses;
            }
            else
                expectedFraction[k] = classes[k]/adjustedWeightSum;
        }

        float[] sampleCounts = new float[numClasses];

        for (int i = 0; i < numTrials;i++)
        {
            int sample = FastLSTMNetwork.probabilisticSample(adjustedClasses,adjustedWeightSum, failureOnAllZeroP);

            sampleCounts[sample]++;

        }

        System.out.println("Generated sample counts, " + Arrays.toString(sampleCounts));
        float error = 0.01F;

        for (int k = 0;k < numClasses;k++)
        {
            float sampleProportion = sampleCounts[k]/numTrials;
            float expectedProportion = expectedFraction[k];

            if (expectedProportion < minWeightFraction)
                Assert.assertTrue("Failed to exclude classes with insufficient weight.  Sample count is " + sampleCounts[k] + " but expected proportion, " + expectedProportion + " < min weight fraction " + minWeightFraction, sampleCounts[k] == 0);
            else
            {
                Assert.assertTrue("Found wrong fraction of class: " + k + ".  Should be about " + expectedProportion + " but is " + sampleProportion, Math.abs(sampleProportion - expectedProportion) <= error);
            }
        }
    }

    @Test
    public void testProbabilisticVectorSampling()
    {

        float[] vector = new float[]{12, 23, 56, 7};

        int numTrials = 10000;
        float currentWeightSum = 0;
        float minValue = 9F;
        int numClasses = vector.length;
        boolean failureOnAllZeroP = true;

        float[] expectedFraction = new float[vector.length];
        for (int k = 0;k < numClasses;k++)
        {
            if (vector[k] >= minValue)
                currentWeightSum +=vector[k];
        }

        for (int k = 0;k < numClasses;k++)
        {
            if (vector[k] >= minValue)
                expectedFraction[k] = vector[k]/currentWeightSum;
            else
                expectedFraction[k] = 0;
        }

        float[] sampleCounts = new float[numClasses];

        for (int i = 0; i < numTrials;i++)
        {
            int sample = FastLSTMNetwork.sampleVectorIndexProportionally(vector, 0, vector.length, minValue, failureOnAllZeroP);

            sampleCounts[sample]++;

        }

        System.out.println("Generated sample counts, " + Arrays.toString(sampleCounts));
        float error = 0.01F;

        for (int k = 0;k < numClasses;k++)
        {
            float sampleProportion = sampleCounts[k]/numTrials;
            float expectedProportion = expectedFraction[k];

            Assert.assertTrue("Found wrong fraction of class: " + k + ".  Should be about " + expectedProportion + " but is " + sampleProportion, Math.abs(sampleProportion - expectedProportion) <= error);
        }
    }



    @Test
    public void testReberGrammarMatching()
    {
        try
        {
            GrammarStateMachine machine = makeReberGrammar();

            int count = 0, numSamples = 100;

            ReberGrammarMatcher reberMatcher = new ReberGrammarMatcher();

            StringBuilder builder = null;
            String nextChar;
            MatchStatus status, expectedStatus;
            boolean success = false;

            for (count = 0; count < numSamples;count++)
            {
                reberMatcher.reset();
                machine.reset();
                builder = new StringBuilder();
                while ((nextChar = machine.next())!=null)
                {
                    builder.append(nextChar);
                    status = reberMatcher.match(nextChar);

                    success = nextChar.equals("E") && MatchStatus.FINISHED == status || MatchStatus.SUCCESS == status;
                    Assert.assertTrue("Failed to match next character of grammar", success);
                }

                System.out.println("Successfully matched " + builder);
            }

            boolean validGrammar = false, cont = true;
            float invalidGrammarProbability = 0.25F;
            String token;
            for (count = 0; count < numSamples;count++)
            {
                reberMatcher.reset();
                builder = new StringBuilder();

                ReberGrammar currentValue = ReberGrammar.B, lastValue = ReberGrammar.E;
                cont = true;
                while (cont)
                {
                    validGrammar = FastLSTMNetwork.randomLCG() > invalidGrammarProbability;

                    if (validGrammar)
                    {
                        if (currentValue != lastValue)
                            expectedStatus = MatchStatus.SUCCESS;
                        else
                        {
                            expectedStatus = MatchStatus.FINISHED;
                            cont = false;
                        }
                        token = currentValue.getToken();

                    }
                    else
                    {
                        cont = false;
                        expectedStatus = MatchStatus.FAILURE;
                        token = "*"; // invalid grammar element

                    }
                    builder.append(token);
                    status = reberMatcher.match(token);
                    Assert.assertTrue("Failed to match '" + token + " to expected status.  Expected " +  expectedStatus.name() + " found: " + status.name() , status == expectedStatus);

                    if (cont)
                    {
                        int[] transitions = currentValue.getTransitions();
                        int index = transitions[(int)(transitions.length*FastLSTMNetwork.randomLCG())];
                        currentValue = ReberGrammar.from(index);
                    }
                    System.out.println("Successfully matched or failed to match " + builder.toString());
                }

            }


        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue("Failed due exception", false);
        }


    }

    @Test
    public void testTeachingReberGrammar()
    {

        String error = "Failure to create test Reber sets";
        try
        {
            int totalTries = 100;
            int stepTries = 20;
            int traingSetSize = 50;
            int testSetSize = 50;

            FastLSTMNetwork.setSeed(2053116556161410L);


            GrammarStateMachine machine = makeReberGrammar();
            String[] alphabet = ReberGrammar.ALPHABET;

            Pair<HashSet<String>, HashSet<String>> trainingPair = getTrainingTestGrammarSets(machine, traingSetSize, testSetSize, totalTries, stepTries);

            Assert.assertTrue(error, trainingPair != null);
            HashSet<String> trainingSet = trainingPair.getLeft();
            HashSet<String> testSet = trainingPair.getRight();


            int inputNodeCode = 7;
            int outputNodeCode = 7;
            int numMemoryCellStates = 17;


            FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);
            builder.setInputNodeCount(inputNodeCode, FastLSTMNetwork.CROSS_ENTROPY_ERROR_ID, FastLSTMNetwork.SOFTMAX_ACTIVATION_ID);
            FastLSTMNetwork network = builder.build();

            float[] networkSpec = network._networkData;

            //FastLSTMNetwork.setSeed(25);
            FastLSTMNetwork.setSeed(1487434922904L);

            // *************************************
            // Overall training parameters
            // *************************************

            int maxTrainingIterations = 20000; // !
            int trainingStep = 0;
            float maxAverageSequenceError = 1.0F;
            float learningRate = 0.1F; // overriding default learning rate
            FastLSTMNetwork.setLearningRate(networkSpec, learningRate);
            LinkedList<Float> errorHistory = new LinkedList<Float>();
            float progressFraction = 0.05F;
            int progressIndicatorSteps = (int)(progressFraction*maxTrainingIterations);


            float averageOverallSegmentError = 0, prevAverageOverallSegmentError = 0;
            long totalCount = 0;
            float convergenceFraction, resetThresholdFraction = 0.0001F, prevError = 0;
            boolean afterWeightInitialization = true;
            boolean sequencesLessThanErrorThresholdP = true;
            int lastResetIndex = 0;
            //LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.DEFAULT;
            LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.RPROP;
            float minOverallAverageError = Float.MAX_VALUE;
            float[] bestShorttermNetwork = networkSpec;
            long currentGeneratingSeed = FastLSTMNetwork.currentSeed;

            // *************************************
            // Sequence Parameters
            // *************************************

            float averageSegmentError = 0;
            ArrayList<Pair<Vector, Vector>> inputOutputPairs;
            float[][] vectorizedReberTrainingSequence;
            Pair<Vector, Vector> inputOutputPair;

            // *************************************
            // Segment Parameters
            // *************************************
            float segmentError;
            float[] expectedOutput, trainingInput;

            // ______________________________________________



            error = "Failed at learning";

            // +++++++++++++++++++++++++++++++++++++++++++++
            // Starting learning process
            // +++++++++++++++++++++++++++++++++++++++++++++

            System.out.println("Generating network from seed: " + currentGeneratingSeed);
            FastLSTMNetwork.initializeAllWeights(networkSpec);

            for (trainingStep = 0; trainingStep < maxTrainingIterations; trainingStep++)
            {
                sequencesLessThanErrorThresholdP = true;
                totalCount = 0;
                averageOverallSegmentError = 0;

                for (String trainingReberString:trainingSet)
                {
                    FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
                    //System.out.println("Trying to learn: " + trainingReberString);
                    vectorizedReberTrainingSequence = getOneHotStringRep(trainingReberString, alphabet);
                    inputOutputPairs = getSequenceTrainingPairs(vectorizedReberTrainingSequence);

                    averageSegmentError = 0;
                    for (int i = 0;i < inputOutputPairs.size();i++)
                    {
                        inputOutputPair = inputOutputPairs.get(i);
                        trainingInput = NNTools.getVectorDataAsFloat(inputOutputPair.getLeft());
                        expectedOutput = NNTools.getVectorDataAsFloat(inputOutputPair.getRight());

                        FastLSTMNetwork.forwardPass(networkSpec, trainingInput);
                        FastLSTMNetwork.getOutputActivation(networkSpec);
                        segmentError = FastLSTMNetwork.updateForwardPassErrors(networkSpec, expectedOutput);

                        averageSegmentError = (i * averageSegmentError + segmentError)/(i + 1);
                        averageOverallSegmentError = (totalCount * averageOverallSegmentError + segmentError)/(totalCount + 1);
                        totalCount++;
                    }

                    sequencesLessThanErrorThresholdP = sequencesLessThanErrorThresholdP && (averageSegmentError <= maxAverageSequenceError);
                }

                if (sequencesLessThanErrorThresholdP)
                {
                    System.out.println("Successfully learned training set!  Average segment error: " + averageOverallSegmentError + " and seed: " + currentGeneratingSeed);
                    break;
                }

                if (averageOverallSegmentError < minOverallAverageError)
                {
                    minOverallAverageError = averageOverallSegmentError;
                    System.out.println("Found new best overall error! " + minOverallAverageError + " using seed: " + currentGeneratingSeed);
                    bestShorttermNetwork = Arrays.copyOf(networkSpec, networkSpec.length);
                }

                if (afterWeightInitialization)
                {
                    afterWeightInitialization = false;
                    prevAverageOverallSegmentError = averageOverallSegmentError;
                    FastLSTMNetwork.updateWeightsFromErrors(networkSpec, updateType);
                }
                else
                {
                    convergenceFraction = Math.abs(averageOverallSegmentError - prevAverageOverallSegmentError)/averageOverallSegmentError;
                    System.out.println(trainingStep + ") average total error: " + averageOverallSegmentError + " convergence: " + convergenceFraction);
                    int resetInterval = (trainingStep - lastResetIndex);
                    boolean insufficientProgress = (progressIndicatorSteps < resetInterval) && averageOverallSegmentError >= minOverallAverageError;
                    if (insufficientProgress  || convergenceFraction < resetThresholdFraction || averageOverallSegmentError != averageOverallSegmentError)
                    {
                        if (insufficientProgress)
                        {
                            System.out.print("Insufficient progress after " + progressIndicatorSteps + " steps.  Current average error: " + averageOverallSegmentError + " best average error: " + minOverallAverageError);
                        }
                        System.out.println("Resetting weights after " + (trainingStep - lastResetIndex) + " failing steps. " + averageOverallSegmentError);
                        lastResetIndex = trainingStep;
                        currentGeneratingSeed = FastLSTMNetwork.currentSeed;
                        FastLSTMNetwork.initializeAllWeights(networkSpec);
                        afterWeightInitialization = true;
                        prevAverageOverallSegmentError = 0;
                    }
                    else
                    {
                        // Update the weights from the calculated errors
                        FastLSTMNetwork.updateWeightsFromErrors(networkSpec, updateType);
                        prevAverageOverallSegmentError = averageOverallSegmentError;
                    }
                }

            }

            if (!sequencesLessThanErrorThresholdP)
            {
                networkSpec = bestShorttermNetwork;
                System.out.println("Failed to learn training set but testing with best short-term network");
            }

            // ******************************************
            // Testing
            // See if LSTM can generate consistent
            // ******************************************
            float[][] vectorizedReberTestSequence;
            float[] predictedNext;

            float maxAcceptabledPredictionDiff = 0.05F;
            float[] currentVectorizedCharacter, nextVectorizedCharacter;
            float maxPredictedValue;
            int actualNextCharIndex;
            int predictedNextCharIndex;

            int steps = 0, totalSteps = 0;
            float averageDifference = 0, totalAverage = 0;
            for (String testReberString:testSet)
            {
                steps = 0;
                System.out.println("Testing against: " + testReberString);
                FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
                vectorizedReberTestSequence = getOneHotStringRep(testReberString, alphabet);

                int extrapLength = vectorizedReberTestSequence.length;

                for (int i = 0;i < extrapLength - 1;i++)
                {
                    System.out.println("History: " + testReberString.substring(0, i+1));
                    currentVectorizedCharacter = vectorizedReberTestSequence[i];
                    nextVectorizedCharacter = vectorizedReberTestSequence[i + 1];
                    FastLSTMNetwork.forwardPass(networkSpec, currentVectorizedCharacter);
                    predictedNext = FastLSTMNetwork.getOutputActivation(networkSpec);
                    maxPredictedValue = Float.MIN_VALUE;
                    actualNextCharIndex = 0;
                    predictedNextCharIndex = 0;
                    for (int j = 0;j<predictedNext.length;j++)
                    {
                        if (nextVectorizedCharacter[j] == 1)
                            actualNextCharIndex = j;
                        if (predictedNext[j] > maxPredictedValue)
                        {
                            maxPredictedValue = predictedNext[j];
                            predictedNextCharIndex = j;
                        }

                    }

                    float predictionDifference = Math.abs(maxPredictedValue - predictedNext[actualNextCharIndex]);

                    averageDifference = (averageDifference*steps + predictionDifference)/(steps + 1);
                    totalAverage = (totalAverage * totalSteps + totalAverage)/(totalSteps + 1);
                    System.out.println("Prediction difference: " + predictionDifference + " distrib: " + Arrays.toString(predictedNext));
                    //
                    steps++;
                    totalSteps++;
                }


                System.out.println("Finished trying to predict [" + testReberString + "] with average difference of: " + averageDifference);



            }
            Assert.assertTrue("Failed to generalize to test set, average prediction error: " + totalAverage + " max allowed: " + maxAcceptabledPredictionDiff, totalAverage < maxAcceptabledPredictionDiff);
            System.out.println("Finished all patterns.  Total average difference: " + totalAverage);





        }
        catch (Exception e)
        {
            Assert.assertTrue(error + ": " + e.toString(), false);
        }
    }


    @Test
    public void testTeachingEmbeddedReberGrammar()
    {

        String error = "Failure to create test Reber sets";
        try
        {
            int totalTries = 100;
            int stepTries = 20;
            int traingSetSize = 256;
            int testSetSize = 256;

            FastLSTMNetwork.setSeed(25);


            GrammarStateMachine machine = makeEmbeddedReber();
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

            float[] networkSpec = network._networkData;


            // good seeds
            // 23578608053233
            // -154733259603855
            //FastLSTMNetwork.setSeed(L);
            FastLSTMNetwork.setSeed(System.currentTimeMillis());

            // *************************************
            // Overall training parameters
            // *************************************

            int maxTrainingIterations = 20000; // !
            int trainingStep = 0;
            float maxAverageSequenceError = 0.7F;
            float learningRate = 0.1F; // overriding default learning rate
            FastLSTMNetwork.setLearningRate(networkSpec, learningRate);
            LinkedList<Float> errorHistory = new LinkedList<Float>();
            float progressFraction = 0.05F;
            int progressIndicatorSteps = (int)(progressFraction*maxTrainingIterations);


            float averageOverallSegmentError = 0, prevAverageOverallSegmentError = 0;
            long totalCount = 0;
            float convergenceFraction, resetThresholdFraction = 0.0001F, prevError = 0;
            boolean afterWeightInitialization = true;
            boolean sequencesLessThanErrorThresholdP = true;
            int lastResetIndex = 0;
            //LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.DEFAULT;
            LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.RPROP;
            float minOverallAverageError = Float.MAX_VALUE;
            float[] bestShorttermNetwork = networkSpec;
            long currentGeneratingSeed = FastLSTMNetwork.currentSeed;

            // *************************************
            // Sequence Parameters
            // *************************************

            float averageSegmentError = 0;
            ArrayList<Pair<Vector, Vector>> inputOutputPairs;
            float[][] vectorizedReberTrainingSequence;
            Pair<Vector, Vector> inputOutputPair;

            // *************************************
            // Segment Parameters
            // *************************************
            float segmentError;
            float[] expectedOutput, trainingInput;

            // ______________________________________________



            error = "Failed at learning";

            // +++++++++++++++++++++++++++++++++++++++++++++
            // Starting learning process
            // +++++++++++++++++++++++++++++++++++++++++++++

            System.out.println("Generating network from seed: " + currentGeneratingSeed);
            FastLSTMNetwork.initializeAllWeights(networkSpec);

            for (trainingStep = 0; trainingStep < maxTrainingIterations; trainingStep++)
            {
                sequencesLessThanErrorThresholdP = true;
                totalCount = 0;
                averageOverallSegmentError = 0;

                for (String trainingReberString:trainingSet)
                {
                    FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
                    //System.out.println("Trying to learn: " + trainingReberString);
                    vectorizedReberTrainingSequence = getOneHotStringRep(trainingReberString, alphabet);
                    inputOutputPairs = getSequenceTrainingPairs(vectorizedReberTrainingSequence);

                    averageSegmentError = 0;
                    for (int i = 0;i < inputOutputPairs.size();i++)
                    {
                        inputOutputPair = inputOutputPairs.get(i);
                        trainingInput = NNTools.getVectorDataAsFloat(inputOutputPair.getLeft());
                        expectedOutput = NNTools.getVectorDataAsFloat(inputOutputPair.getRight());

                        FastLSTMNetwork.forwardPass(networkSpec, trainingInput);
                        FastLSTMNetwork.getOutputActivation(networkSpec);
                        segmentError = FastLSTMNetwork.updateForwardPassErrors(networkSpec, expectedOutput);

                        averageSegmentError = (i * averageSegmentError + segmentError)/(i + 1);
                        averageOverallSegmentError = (totalCount * averageOverallSegmentError + segmentError)/(totalCount + 1);
                        totalCount++;
                    }

                    sequencesLessThanErrorThresholdP = sequencesLessThanErrorThresholdP && (averageSegmentError <= maxAverageSequenceError);
                }

                //if (averageOverallSegmentError < maxAverageSequenceError)
                if (sequencesLessThanErrorThresholdP)
                {
                    System.out.println("Successfully learned training set!  Average segment error: " + averageOverallSegmentError + " and seed: " + currentGeneratingSeed);
                    break;
                }

                if (averageOverallSegmentError < minOverallAverageError)
                {
                    minOverallAverageError = averageOverallSegmentError;
                    System.out.println("Found new best overall error! " + minOverallAverageError + " using seed: " + currentGeneratingSeed);
                    bestShorttermNetwork = Arrays.copyOf(networkSpec, networkSpec.length);
                }

                if (afterWeightInitialization)
                {
                    afterWeightInitialization = false;
                    prevAverageOverallSegmentError = averageOverallSegmentError;
                    FastLSTMNetwork.updateWeightsFromErrors(networkSpec, updateType);
                }
                else
                {
                    convergenceFraction = Math.abs(averageOverallSegmentError - prevAverageOverallSegmentError)/averageOverallSegmentError;
                    System.out.println(trainingStep + ") average total error: " + averageOverallSegmentError + " convergence: " + convergenceFraction);
                    int resetInterval = (trainingStep - lastResetIndex);
                    boolean insufficientProgress = (progressIndicatorSteps < resetInterval) && averageOverallSegmentError >= minOverallAverageError;
                    if (insufficientProgress  || convergenceFraction < resetThresholdFraction || averageOverallSegmentError != averageOverallSegmentError)
                    {
                        if (insufficientProgress)
                        {
                            System.out.print("Insufficient progress after " + progressIndicatorSteps + " steps.  Current average error: " + averageOverallSegmentError + " best average error: " + minOverallAverageError);
                        }
                        System.out.println("Resetting weights after " + (trainingStep - lastResetIndex) + " failing steps. " + averageOverallSegmentError);
                        lastResetIndex = trainingStep;
                        currentGeneratingSeed = FastLSTMNetwork.currentSeed;
                        FastLSTMNetwork.initializeAllWeights(networkSpec);
                        afterWeightInitialization = true;
                        prevAverageOverallSegmentError = 0;
                    }
                    else
                    {
                        // Update the weights from the calculated errors
                        FastLSTMNetwork.updateWeightsFromErrors(networkSpec, updateType);
                        prevAverageOverallSegmentError = averageOverallSegmentError;
                    }
                }

            }

            if (!sequencesLessThanErrorThresholdP)
            {
                networkSpec = bestShorttermNetwork;
                System.out.println("Failed to learn training set but testing with best short-term network");
            }

            // ******************************************
            // Testing
            // See if LSTM can generate consistent
            // ******************************************
            float[][] vectorizedReberTestSequence;
            float[] predictedNext;

            float maxAcceptabledPredictionDiff = 0.05F;
            float[] currentVectorizedCharacter, nextVectorizedCharacter;
            float maxPredictedValue;
            int actualNextCharIndex;
            int predictedNextCharIndex;

            int steps = 0, totalSteps = 0;
            float averageDifference = 0, totalAverage = 0;
            for (String testReberString:testSet)
            {
                steps = 0;
                System.out.println("Testing against: " + testReberString);
                FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
                vectorizedReberTestSequence = getOneHotStringRep(testReberString, alphabet);

                int extrapLength = vectorizedReberTestSequence.length;

                for (int i = 0;i < extrapLength - 1;i++)
                {
                    System.out.println("History: " + testReberString.substring(0, i+1));
                    currentVectorizedCharacter = vectorizedReberTestSequence[i];
                    nextVectorizedCharacter = vectorizedReberTestSequence[i + 1];
                    FastLSTMNetwork.forwardPass(networkSpec, currentVectorizedCharacter);
                    predictedNext = FastLSTMNetwork.getOutputActivation(networkSpec);
                    maxPredictedValue = Float.MIN_VALUE;
                    actualNextCharIndex = 0;
                    predictedNextCharIndex = 0;
                    for (int j = 0;j<predictedNext.length;j++)
                    {
                        if (nextVectorizedCharacter[j] == 1)
                            actualNextCharIndex = j;
                        if (predictedNext[j] > maxPredictedValue)
                        {
                            maxPredictedValue = predictedNext[j];
                            predictedNextCharIndex = j;
                        }

                    }

                    float predictionDifference = Math.abs(maxPredictedValue - predictedNext[actualNextCharIndex]);

                    averageDifference = (averageDifference*steps + predictionDifference)/(steps + 1);
                    totalAverage = (totalAverage * totalSteps + totalAverage)/(totalSteps + 1);
                    System.out.println("Prediction difference: " + predictionDifference + " distrib: " + Arrays.toString(predictedNext));

                    //  second to last character is dependent in the second character.
                    // This must be predicted exactly in order to learning to be considered a success
                    if (extrapLength - 3 == i)
                    {

                        Assert.assertTrue("Failed to learn essential feature of embedded Reber grammar.  Should have predicted: " + testReberString.substring(i+1, i+2) + " instead predicted: " + Arrays.toString(predictedNext) + " with error: " + predictionDifference, predictionDifference == 0);
                    }
                    steps++;
                    totalSteps++;
                }


                System.out.println("Finished trying to predict [" + testReberString + "] with average difference of: " + averageDifference);

            }
            Assert.assertTrue("Failed to generalize to embedded Reber test set, average prediction error: " + totalAverage + " max allowed: " + maxAcceptabledPredictionDiff, totalAverage < maxAcceptabledPredictionDiff);
            System.out.println("Finished all patterns.  Total average difference: " + totalAverage);





        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(error + ": " + e.toString(), false);
        }
    }





    @Test
    public void testErrorMasks()
    {

        String errorMessage = "failed to create noisy input";
        try
        {
            float[][] testInput = new float[][]{{1, 0}, {0, 1}, {1, 0} , {0, 1}, {1, 0}, {0, 1}, {1, 0}, {0, 1}, {1, 0}, {0, 1}, {1, 0}};

            int inputWidth = testInput[0].length;
            int noiseWidth = 4;
            FastLSTMNetwork.setSeed(10);

            float[] errorMask = createPrefixErrorMask(inputWidth, noiseWidth);
            float[][] noisyComplexInput = addNoiseBits(testInput, 4, true, true);

            int totalInputWidth = inputWidth + noiseWidth;
            int inputNodeCode = totalInputWidth;
            int outputNodeCode = totalInputWidth;
            int numMemoryCellStates = 10;


            FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);
            FastLSTMNetwork network = builder.build();

            float[] networkSpec = network._networkData;

            errorMessage = "Failed to learn original pattern";

            // *****************************************
            // Standard Explicit learning strategy
            // *****************************************
            float maxAcceptableError = 0.1F, maxSegmentError, convergenceFraction, resetThresholdFraction = 0.0001F, prevError =0 , segmentError;
            LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.RPROP;
            float trainingPassError = 0;
            int initialLearningPassCount = 0;
            int simplifiedLearningPassCount = 0;
            boolean afterWeightInitialization = true;
            float[] complexTrainedNetwork = networkSpec;
            int lastResetTrainingPassIndex = 0;
            ArrayList<Pair<Vector, Vector>> trainingPairs = getSequenceTrainingPairs(noisyComplexInput);
            // ______________________________________________

            // +++++++++++++++++++++++++++++++++++++++++++++
            //      Standard Training pass parameters
            // +++++++++++++++++++++++++++++++++++++++++++++
            Pair<Vector, Vector> trainingPair;
            float[] trainingInput, expectedOutput, actualOutput;

            errorMessage = "Failed to have easier time learning simplified pattern";

            // +++++++++++++++++++++++++++++++++++++++++++++
            // Starting learning process
            // +++++++++++++++++++++++++++++++++++++++++++++
            FastLSTMNetwork.setSeed(System.currentTimeMillis());
            float averageSimplifiedLearningSteps = 0;
            int numStages = 100;
            outer: for (int testStage = 0; testStage < numStages;testStage++)
            {
                switch (testStage + 1)
                {
                    case 1: // learn the complex pattern
                    {
                        System.out.println("Trying to learning initial, complex pattern");
                        trainingPairs = getSequenceTrainingPairs(noisyComplexInput);
                        FastLSTMNetwork.initializeAllWeights(networkSpec);
                        afterWeightInitialization = true;
                        int maxInitialLearningPasses = 20000;
                        for (initialLearningPassCount = 0; initialLearningPassCount < maxInitialLearningPasses; initialLearningPassCount++)
                        {
                            FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
                            maxSegmentError = Float.MIN_VALUE;
                            for (int i = 0;i < trainingPairs.size();i++)
                            {
                                trainingPair = trainingPairs.get(i);
                                trainingInput = NNTools.getVectorDataAsFloat(trainingPair.getLeft());
                                expectedOutput = NNTools.getVectorDataAsFloat(trainingPair.getRight());

                                FastLSTMNetwork.forwardPass(networkSpec, trainingInput);
                                FastLSTMNetwork.getOutputActivation(networkSpec);
                                segmentError = FastLSTMNetwork.updateForwardPassErrors(networkSpec, expectedOutput);

                                maxSegmentError = Math.max(maxSegmentError, segmentError);
                            }

                            trainingPassError = maxSegmentError;
                            if (trainingPassError <= maxAcceptableError)
                            {

                                System.out.println("Learned noisy pattern after " + initialLearningPassCount + " steps and error: " + trainingPassError);
                                complexTrainedNetwork = Arrays.copyOf(networkSpec, networkSpec.length);
                                continue outer;
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
                                    System.out.println(" ******** Resetting weights after " + (initialLearningPassCount - lastResetTrainingPassIndex) + " failing steps");
                                    lastResetTrainingPassIndex = initialLearningPassCount;
                                    FastLSTMNetwork.initializeAllWeights(networkSpec);
                                    afterWeightInitialization = true;
                                }
                            }
                            prevError = trainingPassError;
                        }
                        System.out.println("Failed to learn noisy pattern after " + initialLearningPassCount + " steps and error: " + trainingPassError);
                    }
                    break;
                    default:
                    {

                        float[][] ignorableNoisyComplexInput;
                        if (testStage == numStages - 1)
                        {
                            // For the last stage retrain the network on the initial input as the
                            // complex testing stage, but ignoring noise bits
                            ignorableNoisyComplexInput = noisyComplexInput;
                            System.out.println("Trying to relearn original input while ignoring noise bits");
                        }
                        else
                        {
                            System.out.println("Trying to learning simplified pattern");
                            ignorableNoisyComplexInput = addNoiseBits(testInput, 4, true, true);
                        }
                        trainingPairs = getSequenceTrainingPairs(ignorableNoisyComplexInput);
                        FastLSTMNetwork.setOutputErrorMask(networkSpec, errorMask);
                        FastLSTMNetwork.setSeed(System.currentTimeMillis());
                        FastLSTMNetwork.initializeAllWeights(networkSpec);
                        afterWeightInitialization = true;
                        int maxMaskedLearningPasses = 1000;
                        for (simplifiedLearningPassCount = 0; simplifiedLearningPassCount < maxMaskedLearningPasses; simplifiedLearningPassCount++)
                        {
                            FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
                            maxSegmentError = Float.MIN_VALUE;
                            for (int i = 0;i < trainingPairs.size();i++)
                            {
                                trainingPair = trainingPairs.get(i);
                                trainingInput = NNTools.getVectorDataAsFloat(trainingPair.getLeft());
                                expectedOutput = NNTools.getVectorDataAsFloat(trainingPair.getRight());

                                FastLSTMNetwork.forwardPass(networkSpec, trainingInput);
                                FastLSTMNetwork.getOutputActivation(networkSpec);
                                segmentError = FastLSTMNetwork.updateForwardPassErrors(networkSpec, expectedOutput);

                                maxSegmentError = Math.max(maxSegmentError, segmentError);
                            }

                            trainingPassError = maxSegmentError;
                            if (trainingPassError <= maxAcceptableError)
                            {

                                System.out.println("Learned simplified pattern after " + simplifiedLearningPassCount + " steps and error: " + trainingPassError);

                                averageSimplifiedLearningSteps = (averageSimplifiedLearningSteps * (testStage - 1) + simplifiedLearningPassCount)/ testStage;

                                if (testStage == numStages - 1)
                                {
                                    Assert.assertTrue("Failed to learn complex input in fewer than original steps even while ignoring noise bits", simplifiedLearningPassCount < initialLearningPassCount);
                                }
                                continue outer;
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
                                    System.out.println("<><><><><><><><> Resetting weights after " + (simplifiedLearningPassCount - lastResetTrainingPassIndex) + " failing steps");
                                    lastResetTrainingPassIndex = simplifiedLearningPassCount;
                                    FastLSTMNetwork.initializeAllWeights(networkSpec);
                                    afterWeightInitialization = true;
                                }
                            }
                            prevError = trainingPassError;
                        }
                        errorMessage = "Failed to learn simplified pattern";
                        Assert.assertTrue(errorMessage, false);


                    }
                    break;
                }
            }

            errorMessage = "Simplified pattern fails to make learning easier.";
            System.out.println("Learned simplified pattern after an average of " + averageSimplifiedLearningSteps + " steps vs " + initialLearningPassCount + " steps for the initial complex pattern");
            Assert.assertTrue(errorMessage, averageSimplifiedLearningSteps < initialLearningPassCount);
            FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
            // Extrapolation

            int extrapLength = noisyComplexInput.length;

            float[] extrapInput = noisyComplexInput[0];
            errorMessage = "Failed to learn to generalize from simplified pattern";
            System.out.println("Extrapolating from: " + Arrays.toString(extrapInput));
            boolean driveOutput = true;
            boolean roundOutput = false;
            for (int i = 0; i < extrapLength - 1;i++)
            {
                if (roundOutput)
                {
                    extrapInput = roundToInt(extrapInput);
                }
                FastLSTMNetwork.forwardPass(networkSpec, extrapInput);
                extrapInput = FastLSTMNetwork.getOutputActivation(networkSpec);
                System.out.println("Predicted: " + Arrays.toString(extrapInput));
                FastLSTMNetwork.OutputErrorFunction outError = FastLSTMNetwork.ERROR_FUNCTION_MAP[(int)networkSpec[FastLSTMNetwork.OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX]];
                double error = outError.error(networkSpec, noisyComplexInput[i + 1]);
                if (driveOutput)
                    extrapInput = noisyComplexInput[i + 1];


                Assert.assertTrue(errorMessage, maxAcceptableError>= error);
            }



        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testIgnoreCompression()
    {

        String errorMessage = "failed to create noisy input";
        try
        {
            float[][] testInput = new float[][]{{1, 0}, {0, 1}, {1, 0} , {0, 1}, {1, 0}, {0, 1}, {1, 0}, {0, 1}, {1, 0}, {0, 1}, {1, 0}};

            int inputWidth = testInput[0].length;
            int noiseWidth = 4;
            FastLSTMNetwork.setSeed(10);

            float[] errorMask = createPrefixErrorMask(inputWidth, noiseWidth);
            float[][] noisyComplexInput = addNoiseBits(testInput, 4, true, true);

            int totalInputWidth = inputWidth + noiseWidth;
            int inputNodeCode = totalInputWidth;
            int outputNodeCode = totalInputWidth;
            int numMemoryCellStates = 10;


            FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);
            FastLSTMNetwork network = builder.build();

            float[] networkSpec = network._networkData;

            errorMessage = "Failed to learn original pattern";

            // *****************************************
            // Standard Explicit learning strategy
            // *****************************************
            long seed = System.currentTimeMillis();

            int numLearningSteps = 300;
            float maxAcceptableError = 0.1F;

            final int FINDING_MINIMUM_INITIAL_COMPRESSION = 0;
            final int FINDING_MINIMUM_SIMPLIFIED_NETWORK= 1;
            final int END_STATE = 2;
            int testStage = 0;
            int currentNumMemoryCells = numMemoryCellStates;
            int smallestNoisyMemoryCells =currentNumMemoryCells, smallestSimplifiedMemoryCells = currentNumMemoryCells ;
            float[] smallestSimplifiedData = null, smallestNoisyData;
            Pair<Float, Integer> result = null;
            FastLSTMNetwork.useEmbeddedRNG = false;
            outer: while (testStage != END_STATE)
            {
                switch (testStage)
                {
                    case FINDING_MINIMUM_INITIAL_COMPRESSION: // learn the complex pattern
                    {
                        seed = System.currentTimeMillis();
                        System.out.println("Learning complex pattern with initial seed: "+ seed);
                        result = teachSequence(networkSpec, noisyComplexInput, maxAcceptableError, numLearningSteps);
                        if (result.getRight() < numLearningSteps)
                        {
                            System.out.println("Memorized noisy pattern in " + result.getRight() + " steps and error: " + result.getLeft());
                            smallestNoisyData = Arrays.copyOf(networkSpec, networkSpec.length);
                            smallestNoisyMemoryCells = currentNumMemoryCells;
                            currentNumMemoryCells--;
                            builder = getStandardBuilder(inputNodeCode, outputNodeCode, currentNumMemoryCells);
                            networkSpec = builder.build()._networkData;

                        }
                        else
                        {
                            System.out.println("Found the solution");
                            testStage = FINDING_MINIMUM_SIMPLIFIED_NETWORK;
                            currentNumMemoryCells = numMemoryCellStates;
                            builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);
                            networkSpec = builder.build()._networkData;
                            FastLSTMNetwork.setOutputErrorMask(networkSpec, errorMask);
                        }
                    }
                    break;
                    case FINDING_MINIMUM_SIMPLIFIED_NETWORK:
                    {
                        seed = System.currentTimeMillis();
                        System.out.println("Learning simplified pattern with initial seed: "+ seed);
                        result = teachSequence(networkSpec, noisyComplexInput, maxAcceptableError, numLearningSteps);
                        if (result.getRight() < numLearningSteps)
                        {
                            System.out.println("Memorized simplified pattern in " + result.getRight() + " steps and error: " + result.getLeft());
                            smallestSimplifiedData = Arrays.copyOf(networkSpec, networkSpec.length);
                            smallestSimplifiedMemoryCells = currentNumMemoryCells;
                            currentNumMemoryCells--;
                            builder = getStandardBuilder(inputNodeCode, outputNodeCode, currentNumMemoryCells);
                            networkSpec = builder.build()._networkData;
                            FastLSTMNetwork.setOutputErrorMask(networkSpec, errorMask);

                        }
                        else
                        {
                            System.out.println("Memorized simplified pattern in a minimum of : " + smallestSimplifiedMemoryCells + " memory cells");
                            Assert.assertTrue("Simplifying network failed to allow compression", smallestSimplifiedMemoryCells < smallestNoisyMemoryCells);
                            testStage = END_STATE;
                        }

                    }
                    break;
                    case END_STATE:

                }
            }

            int extrapLength = noisyComplexInput.length;

            networkSpec = smallestSimplifiedData;
            FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
            float[] extrapInput = noisyComplexInput[0];
            errorMessage = "Compressing simplified pattern doesn't recreate pattern";
            System.out.println("Extrapolating from: " + Arrays.toString(extrapInput));
            boolean driveOutput = true;
            boolean roundOutput = false;
            for (int i = 0; i < extrapLength - 1;i++)
            {
                if (roundOutput)
                {
                    extrapInput = roundToInt(extrapInput);
                }
                FastLSTMNetwork.forwardPass(networkSpec, extrapInput);
                extrapInput = FastLSTMNetwork.getOutputActivation(networkSpec);
                System.out.println("Predicted: " + Arrays.toString(extrapInput));
                FastLSTMNetwork.OutputErrorFunction outError = FastLSTMNetwork.ERROR_FUNCTION_MAP[(int)networkSpec[FastLSTMNetwork.OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX]];
                double error = outError.error(networkSpec, noisyComplexInput[i + 1]);
                if (driveOutput)
                    extrapInput = noisyComplexInput[i + 1];


                Assert.assertTrue(errorMessage, maxAcceptableError>= error);
            }



        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testCompactionGeneralization()
    {

        String errorMessage = "failed to create noisy input";
        try
        {
            FastLSTMNetwork.useEmbeddedRNG = false;
            float[][] testInput = new float[][]{{1, 0}, {0, 1}, {1, 0} , {0, 1}, {1, 0}, {0, 1}, {1, 0}, {0, 1}, {1, 0}, {0, 1}, {1, 0}};

            int inputWidth = testInput[0].length;
            int noiseWidth = 4;
            FastLSTMNetwork.setSeed(System.currentTimeMillis());

            float[] errorMask = createPrefixErrorMask(inputWidth, noiseWidth);
            float[] noiseMask = NNTools.toggleBits(errorMask);
            float[][] noisyComplexInput = null;

            int totalInputWidth = inputWidth + noiseWidth;
            int inputNodeCode = totalInputWidth;
            int outputNodeCode = totalInputWidth;
            int numMemoryCellStates = 10;


            FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);
            FastLSTMNetwork network = builder.build();

            float[] networkSpec = network._networkData;

            errorMessage = "Failed to learn original pattern";

            // *****************************************
            // Standard Explicit learning strategy
            // *****************************************
            long seed = System.currentTimeMillis();

            int numLearningSteps = 300;
            float maxAcceptableError = 0.1F;

            final int FINDING_MINIMUM_INITIAL_COMPRESSION = 0;
            final int FINDING_MINIMUM_SIMPLIFIED_NETWORK= 1;
            final int EXTRAPOLATING = 2;

            final int END_STATE = 3;
            int testStage = 0;
            int numExtrapolations = 1000;
            float extrapCount = 0;
            boolean firstSimplified=true, firstNoisy = true;
            int currentNumMemoryCells = numMemoryCellStates;
            int smallestNoisyMemoryCells =currentNumMemoryCells, smallestSimplifiedMemoryCells = currentNumMemoryCells ;
            float[] smallestSimplifiedData = null, smallestNoisyData=null, largestSimplifiedData=null, largestNoisyData = null;
            Pair<Float, Integer> result = null;
            String[] dataList = new String[]{"Original Noisy Network", "Compacted Noisy Network", "Original Simplified Network", "Compacted and simplified network"};

            float[][] testNetworks = null;

            float[] totalAverageError = new float[dataList.length];
            float[] averageNetworkSize = new float[4];
            float[] averageNumMemoryCells = new float[4];


            for (extrapCount = 0; extrapCount < numExtrapolations;extrapCount++)
            {
                firstNoisy = true;
                firstSimplified = true;
                noisyComplexInput = addNoiseBits(testInput, 4, true, true);
                testStage = FINDING_MINIMUM_INITIAL_COMPRESSION;
                currentNumMemoryCells = numMemoryCellStates;
                builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);
                networkSpec = builder.build()._networkData;

                while (testStage != END_STATE)
                {
                    switch (testStage)
                    {

                        case FINDING_MINIMUM_INITIAL_COMPRESSION: // learn the complex pattern
                        {
                            seed = System.currentTimeMillis();
                            System.out.println("Learning complex pattern with num memory cells: " + currentNumMemoryCells);
                            if (currentNumMemoryCells < 1)
                            {
                                smallestNoisyData = Arrays.copyOf(networkSpec, networkSpec.length);
                                System.out.println("Saved smaller noisy LSTM with " + smallestNoisyMemoryCells + " memory cells");
                                testStage = FINDING_MINIMUM_SIMPLIFIED_NETWORK;

                                currentNumMemoryCells = numMemoryCellStates;
                                break;
                            }
                            result = teachNoisySequence(networkSpec, noisyComplexInput, maxAcceptableError, numLearningSteps, noiseMask);
                            if (result.getRight() < numLearningSteps)
                            {
                                if (firstNoisy)
                                {
                                    firstNoisy = false;
                                    largestNoisyData = Arrays.copyOf(networkSpec, networkSpec.length);
                                    int width = FastLSTMNetwork.getLayerWidth(largestNoisyData, FastLSTMNetwork.PEEPHOLE_LAYER_ID);
                                    System.out.println("Saving large noisy LSTM " + largestNoisyData + "with " + numMemoryCellStates + " memory cells");

                                }
                                System.out.println("Memorized noisy pattern in " + result.getRight() + " steps and error: " + result.getLeft());
                                smallestNoisyData = Arrays.copyOf(networkSpec, networkSpec.length);
                                smallestNoisyMemoryCells = currentNumMemoryCells;
                                currentNumMemoryCells--;
                                builder = getStandardBuilder(inputNodeCode, outputNodeCode, currentNumMemoryCells);
                                networkSpec = builder.build()._networkData;

                            } else
                            {
                                smallestNoisyData = Arrays.copyOf(networkSpec, networkSpec.length);
                                System.out.println("Saved smaller noisy LSTM with " + smallestNoisyMemoryCells + " memory cells");
                                testStage = FINDING_MINIMUM_SIMPLIFIED_NETWORK;

                                currentNumMemoryCells = numMemoryCellStates;

                                builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);
                                networkSpec = builder.build()._networkData;
                                FastLSTMNetwork.setOutputErrorMask(networkSpec, errorMask);
                            }
                        }
                        break;
                        case FINDING_MINIMUM_SIMPLIFIED_NETWORK:
                        {

                            System.out.println("Learning simplified pattern with num memory cells: " + currentNumMemoryCells);
                            //result = teachNoisySequence(networkSpec, noisyComplexInput, maxAcceptableError, numLearningSteps, noiseMask);
                            result = teachSequence(networkSpec, noisyComplexInput, maxAcceptableError, numLearningSteps);


                            if (result.getRight() < numLearningSteps)
                            {
                                if (firstSimplified)
                                {
                                    firstSimplified = false;
                                    largestSimplifiedData = Arrays.copyOf(networkSpec, networkSpec.length);
                                    System.out.println("Saving large simplified LSTM with " + numMemoryCellStates + " memory cells");
                                }
                                System.out.println("Memorized simplified pattern in " + result.getRight() + " steps and error: " + result.getLeft());
                                smallestSimplifiedData = Arrays.copyOf(networkSpec, networkSpec.length);
                                smallestSimplifiedMemoryCells = currentNumMemoryCells;
                                currentNumMemoryCells--;
                                builder = getStandardBuilder(inputNodeCode, outputNodeCode, currentNumMemoryCells);
                                networkSpec = builder.build()._networkData;
                                FastLSTMNetwork.setOutputErrorMask(networkSpec, errorMask);

                            } else
                            {
                                smallestSimplifiedData = Arrays.copyOf(networkSpec, networkSpec.length);
                                System.out.println("Saved small simplified LSTM with " + smallestSimplifiedMemoryCells + " memory cells");
                                //System.out.println("Memorized simplified pattern in a minimum of : " + smallestSimplifiedMemoryCells + " memory cells");
                                //Assert.assertTrue("Simplifying network failed to allow compression", smallestSimplifiedMemoryCells < smallestNoisyMemoryCells);
                                testStage = EXTRAPOLATING;

                            }

                        }
                        break;
                        case EXTRAPOLATING:
                        {
                            testNetworks = new float[][]{largestNoisyData, smallestNoisyData, largestSimplifiedData, smallestSimplifiedData};

                            System.out.println(Arrays.toString(testNetworks));
                            for (int i = 0;i < testNetworks.length;i++)
                            {
                                FastLSTMNetwork.resetNetworkToInitialState(testNetworks[i]);
                            }
                            float[][] noisyTestInput = addNoiseBits(testInput, 4, true, true);
                            int extrapLength = noisyTestInput.length;
                            networkSpec = smallestSimplifiedData;
                            float[] extrapInput = noisyTestInput[0];
                            System.out.println("*********************************");
                            System.out.println(extrapCount + ") Test noisy input: ");
                            for (int i = 0; i < extrapLength; i++)
                            {

                                System.out.println(Arrays.toString(noisyTestInput[i]));
                            }
                            System.out.println();
                            System.out.println("*********************************");

                            float[] averageError = new float[testNetworks.length];
                            String label;
                            String[] errorList = new String[testNetworks.length];

                            for (int i = 0; i < extrapLength - 1; i++)
                            {

                                System.out.println("Current test noisy input: " + Arrays.toString(noisyTestInput[i]) + " expected output: " + Arrays.toString(noisyTestInput[i + 1]));
                                int networkIndex = 0;
                                for (; networkIndex < testNetworks.length; networkIndex++)
                                {
                                    FastLSTMNetwork.forwardPass(testNetworks[networkIndex], noisyTestInput[i]);
                                    extrapInput = FastLSTMNetwork.getOutputActivation(testNetworks[networkIndex]);
                                    FastLSTMNetwork.OutputErrorFunction outError = FastLSTMNetwork.ERROR_FUNCTION_MAP[(int) testNetworks[networkIndex][FastLSTMNetwork.OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX]];
                                    double error = outError.error(testNetworks[networkIndex], noisyTestInput[i + 1]);
                                    averageError[networkIndex] = (float) ((averageError[networkIndex] * i + error) / (i + 1));
                                    errorList[networkIndex] = dataList[networkIndex] + " predicted: " + Arrays.toString(extrapInput) +  " with error:  " + error;

                                }

                                System.out.println(Arrays.toString(errorList));


                                //Assert.assertTrue(errorMessage, maxAcceptableError>= error);
                            }
                            for (int i = 0; i < testNetworks.length; i++)
                            {

                                totalAverageError[i] = totalAverageError[i] * extrapCount/(extrapCount + 1)  + averageError[i] / (extrapCount + 1);
                                averageNetworkSize[i] = averageNetworkSize[i] * extrapCount/(extrapCount + 1) + (1.0F*testNetworks[i].length)/(extrapCount + 1) ;
                                float memory = FastLSTMNetwork.getLayerWidth(testNetworks[i], FastLSTMNetwork.PEEPHOLE_LAYER_ID);
                                if (i == 0)
                                {
                                    System.out.println("<> " + memory + " " + testNetworks[i]);
                                }
                                averageNumMemoryCells[i] = averageNumMemoryCells[i] * extrapCount/(extrapCount + 1) + memory / (extrapCount + 1);
                            }
                            System.out.println(extrapCount + ": " + Arrays.toString(averageNumMemoryCells));
                            testStage = END_STATE;
                        }
                        break;
                        case END_STATE:

                            break;

                    }
                }
            }

            System.out.println("Average overall errors: ");
            for (int i = 0; i < dataList.length;i++)
            {
                int networkSize = (int)averageNetworkSize[i];
                float memoryCellsToTenths = ((int)(averageNumMemoryCells[i] * 10))/10F;
                System.out.println(dataList[i] + "(" + networkSize + ", " + memoryCellsToTenths+ ") - " + totalAverageError[i]);
            }


        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
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
    public void testGaussianRandomNumberGenerator()
    {
        long seed = System.currentTimeMillis();
        //
        FastLSTMNetwork.setSeed(seed);
        int iterations = 10000;
        System.out.println("Generating random numbers with seed: " + seed);


        double prev = seed;
        double average = 0;
        for (int i = 0;i < iterations;i++)
        {
            double ran = FastLSTMNetwork.randomGaussian();
            System.out.println("Random number is: " + ran);
            Assert.assertTrue("Numbers aren't random enough! Current value equals previous: " + ran, ran != prev);
            prev = ran;
            //Assert.assertTrue("Random numbers out of range: " + ran, ran > -1 && ran < 1);
            average = (average*i + ran)/(i + 1);
        }

        // TODO: make this test more rigorous

        double expectedAverage = 0;
        double error = 0.05;
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

    static float[] createPrefixErrorMask(int allowedPrefixBits, int ignoredSuffixBits)
    {
        float[] o = new float[allowedPrefixBits + ignoredSuffixBits];
        for (int i=0; i < o.length;i++)
        {
            if (i < allowedPrefixBits)
            {
                o[i] = 1;
            }
            else
                o[i] = 0;
        }
        return o;
    }


    float[] addNoiseBits(float[] base, int noiseWidth, boolean noiseSuffixP, boolean roundNoiseP)
    {

        int oldWidth = base.length;
        int newWidth = oldWidth + noiseWidth;
        float[] vec = new float[newWidth];
        for (int j = 0;j < newWidth;j++)
        {
            if (noiseSuffixP)
            {
                if (j < oldWidth)
                {
                    vec[j] = base[j];
                }
                else
                {
                    if (roundNoiseP)
                        vec[j] = NNTools.roundToInt (FastLSTMNetwork.randomLCG());
                    else
                        vec[j] = (float) FastLSTMNetwork.randomLCG();
                }
            }
            else
            {
                if (j >= noiseWidth)
                {
                    vec[j] = base[j - noiseWidth];
                }
                else
                {
                    if (roundNoiseP)
                        vec[j] = NNTools.roundToInt (FastLSTMNetwork.randomLCG());
                    else
                        vec[j] = (float) FastLSTMNetwork.randomLCG();
                }
            }
        }
        return vec;
    }

    float[][] addNoiseBits(float[][] base, int noiseWidth, boolean noiseSuffixP, boolean roundNoiseP)
    {
        float[][] out = new float[base.length][];
        for (int i = 0;i < out.length;i++)
        {
            out[i] = addNoiseBits(base[i], noiseWidth, noiseSuffixP, roundNoiseP);
        }
        return out;
    }


    Pair<Float, Integer> teachSequence(float[] networkSpec, float[][] inputSequence, float maxAcceptableError, int maxSteps)
    {
        int steps = 0;
        LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.RPROP;
        ArrayList<Pair<Vector, Vector>> trainingPairs = getSequenceTrainingPairs(inputSequence);
        Pair<Vector, Vector> trainingPair;
        float[] trainingInput, expectedOutput;

        FastLSTMNetwork.initializeAllWeights(networkSpec);
        boolean afterWeightInitialization = true;
        float maxSegmentError = 0, segmentError, convergenceFraction = 0, resetThresholdFraction = 0.0001F, prevError = 0;
        for (steps = 0; steps < maxSteps; steps++)
        {
            FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
            maxSegmentError = Float.MIN_VALUE;
            for (int i = 0;i < trainingPairs.size();i++)
            {
                trainingPair = trainingPairs.get(i);
                trainingInput = NNTools.getVectorDataAsFloat(trainingPair.getLeft());
                expectedOutput = NNTools.getVectorDataAsFloat(trainingPair.getRight());

                FastLSTMNetwork.forwardPass(networkSpec, trainingInput, true);
                FastLSTMNetwork.getOutputActivation(networkSpec);
                segmentError = FastLSTMNetwork.updateForwardPassErrors(networkSpec, expectedOutput);

                maxSegmentError = Math.max(maxSegmentError, segmentError);
            }


            if (maxSegmentError <= maxAcceptableError)
            {

                 return Pair.of(Float.valueOf(maxSegmentError), steps);
            }

            // Update the weights from the calculated errors
            FastLSTMNetwork.updateWeightsFromErrors(networkSpec, updateType);
            if (afterWeightInitialization)
            {
                afterWeightInitialization = false;
            }
            else
            {
                convergenceFraction = Math.abs(prevError - maxSegmentError)/maxSegmentError;
                if (convergenceFraction < resetThresholdFraction)
                {


                    FastLSTMNetwork.initializeAllWeights(networkSpec);
                    afterWeightInitialization = true;
                }
            }
            prevError = maxSegmentError;
        }
        return Pair.of(Float.valueOf(maxSegmentError), steps);

    }


    Pair<Float, Integer> teachNoisySequence(float[] networkSpec, float[][] inputSequence, float maxAcceptableError, int maxSteps, float[] inputNoiseMask)
    {
        int steps = 0;
        LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.RPROP;
        ArrayList<Pair<Vector, Vector>> trainingPairs = getSequenceTrainingPairs(inputSequence);
        Pair<Vector, Vector> trainingPair;
        float[] trainingInput, expectedOutput;

        FastLSTMNetwork.initializeAllWeights(networkSpec);
        boolean afterWeightInitialization = true;
        float maxSegmentError = 0, segmentError, convergenceFraction = 0, resetThresholdFraction = 0.0001F, prevError = 0;
        for (steps = 0; steps < maxSteps; steps++)
        {
            FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
            maxSegmentError = Float.MIN_VALUE;
            for (int i = 0;i < trainingPairs.size();i++)
            {
                trainingPair = trainingPairs.get(i);
                trainingInput = NNTools.getVectorDataAsFloat(trainingPair.getLeft());
                if (inputNoiseMask != null)
                {
                    for (int k = 0;k<inputNoiseMask.length;k++)
                    {
                        if (inputNoiseMask[k] == 1)
                        {
                            trainingInput[k] = roundToInt(FastLSTMNetwork.randomLCG());
                        }
                    }
                }
                expectedOutput = NNTools.getVectorDataAsFloat(trainingPair.getRight());

                FastLSTMNetwork.forwardPass(networkSpec, trainingInput, true);
                FastLSTMNetwork.getOutputActivation(networkSpec);
                segmentError = FastLSTMNetwork.updateForwardPassErrors(networkSpec, expectedOutput);

                maxSegmentError = Math.max(maxSegmentError, segmentError);
            }


            if (maxSegmentError <= maxAcceptableError)
            {

                return Pair.of(Float.valueOf(maxSegmentError), steps);
            }

            // Update the weights from the calculated errors
            FastLSTMNetwork.updateWeightsFromErrors(networkSpec, updateType);
            if (afterWeightInitialization)
            {
                afterWeightInitialization = false;
            }
            else
            {
                convergenceFraction = Math.abs(prevError - maxSegmentError)/maxSegmentError;
                if (convergenceFraction < resetThresholdFraction)
                {


                    FastLSTMNetwork.initializeAllWeights(networkSpec);
                    afterWeightInitialization = true;
                }
            }
            prevError = maxSegmentError;
        }
        return Pair.of(Float.valueOf(maxSegmentError), steps);

    }



}
