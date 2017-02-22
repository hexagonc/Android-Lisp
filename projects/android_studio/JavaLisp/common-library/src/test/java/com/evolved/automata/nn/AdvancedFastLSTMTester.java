package com.evolved.automata.nn;

import com.evolved.automata.nn.BaseLSTMTester;
import com.evolved.automata.nn.FastLSTMNetwork;
import com.evolved.automata.nn.LSTMNetwork;
import com.evolved.automata.nn.NNTools;
import com.evolved.automata.nn.Vector;
import com.evolved.automata.nn.grammar.GrammarStateMachine;
import com.evolved.automata.nn.grammar.MatchStatus;

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
 * Created by Evolved8 on 2/21/17.
 */

public class AdvancedFastLSTMTester extends BaseLSTMTester {


    // TODO: finish this after testing incremental storage
    @Test
    public void testRecoverReberGrammar()
    {
        String errorMessage = "failed to learn reber grammar";
        try
        {
            // =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+=
            //              Building the Network
            // =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+=

            int inputNodeCode = 7;
            int outputNodeCode = 7;
            int numMemoryCellStates = 17;


            FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);
            builder.setInputNodeCount(inputNodeCode, FastLSTMNetwork.CROSS_ENTROPY_ERROR_ID, FastLSTMNetwork.SOFTMAX_ACTIVATION_ID);
            FastLSTMNetwork network = builder.build();

            float[] networkSpec = network._networkData;

            boolean learnedReberGrammar = learnReberGrammar(networkSpec);

            Assert.assertTrue(errorMessage, learnedReberGrammar);

            // -=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=
            //              Attempting to Recover Pattern
            // -=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=

            String[] alphabet = ReberGrammar.ALPHABET;
            float[] currentInput = getOneHotStringRep("B", alphabet)[0], extrudedOutput;
            float[] extrudeChar;
            ReberGrammarMatcher reberMatcher = new ReberGrammarMatcher();

            boolean testPredictionsP = false;
            MatchStatus status;
            int testCount = 0, numExtrusions = 20;
            String extrudedString = "", inputString;
            StringBuilder sbuilder = new StringBuilder();
            int activationIndex = FastLSTMNetwork.getLayerActivationIndex(networkSpec, FastLSTMNetwork.OUTPUT_LAYER_ID);
            for (testCount = 0; testCount < numExtrusions; testCount++)
            {
                reberMatcher.reset();
                sbuilder = new StringBuilder();
                FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
                currentInput = getOneHotStringRep("B", alphabet)[0];
                status = reberMatcher.match("B");
                sbuilder.append("B");
                int maxLength = 20, extrapCount = 0;
                do
                {
                    inputString = getStringFromOneHotVector(currentInput, alphabet);
                    System.out.println("Input: " + inputString);
                    FastLSTMNetwork.forwardPass(networkSpec, currentInput);
                    extrudedOutput =  FastLSTMNetwork.getOutputActivation(networkSpec);
                    System.out.println("Raw extrapolated output: " + Arrays.toString(extrudedOutput));
                    int selectedCharIndex = FastLSTMNetwork.sampleVectorIndexProportionally(networkSpec, activationIndex, outputNodeCode, 0.15F, false);
                    extrudeChar = new float[outputNodeCode];
                    extrudeChar[selectedCharIndex] = 1;
                    extrudedString = getStringFromOneHotVector(extrudeChar, alphabet);
                    status = reberMatcher.match(extrudedString);
                    System.out.println("Constructed output: " + extrudedString + ".  Match status: " + status.name());
                    if (testPredictionsP)
                    {
                        Assert.assertTrue("Constructed invalid string: " + extrudedString, status == MatchStatus.FINISHED || status == MatchStatus.SUCCESS);
                    }
                    sbuilder.append(extrudedString);
                    currentInput = extrudeChar;
                    extrapCount++;
                } while (!"E".equals(extrudedString) && extrapCount < maxLength);
                System.out.println("Constructed: " + sbuilder);

            }


        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testLearnReberGrammarIncrementally()
    {
        String errorMessage = "failed to learn reber grammar";
        try
        {
            // =========================================
            // Construct the training and test sets
            // =========================================


            int totalTries = 100;
            int stepTries = 20;
            int traingSetSize = 50;
            int testSetSize = 50;

            FastLSTMNetwork.setSeed(2053116556161410L);


            GrammarStateMachine machine = makeReberGrammar();
            String[] alphabet = ReberGrammar.ALPHABET;

            Pair<HashSet<String>, HashSet<String>> trainingPair = getTrainingTestGrammarSets(machine, traingSetSize, testSetSize, totalTries, stepTries);


            HashSet<String> trainingSet = trainingPair.getLeft();
            HashSet<String> testSet = trainingPair.getRight();

            // =========================================
            // Construct the neural network
            // =========================================


            int inputNodeCode = 7;
            int outputNodeCode = 7;
            int numMemoryCellStates = 17;


            FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);
            builder.setInputNodeCount(inputNodeCode, FastLSTMNetwork.CROSS_ENTROPY_ERROR_ID, FastLSTMNetwork.SOFTMAX_ACTIVATION_ID);
            FastLSTMNetwork network = builder.build();

            float[] networkSpec = network._networkData;


            // =========================================
            // Learn each pattern in the training set
            // =========================================
            float[][] vectorizedReberTrainingSequence;
            boolean firstPattern = true;
            float error;
            float resetThresholdFraction = 0.0001F, maxAverageSequenceError = 0.1F;
            int maxStepsPerSequence = 300;
            int maxLearningStages = 1;
            LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.RPROP;
            boolean success = false;
            errorMessage = "Failed to save all training patterns in the LSTM";
            outer: for (int learningStage = 0; learningStage < maxLearningStages; learningStage++)
            {
                FastLSTMNetwork.initializeAllWeights(networkSpec);
                firstPattern = true;
                for (String trainingString:trainingSet)
                {
                    System.out.println("Trying to add Reber string: " + trainingString);
                    vectorizedReberTrainingSequence = getOneHotStringRep(trainingString, alphabet);

                    float[] before = Arrays.copyOf(networkSpec, networkSpec.length);
                    if (firstPattern)
                    {
                        error = (float)learnSimpleSequence(networkSpec, vectorizedReberTrainingSequence, maxAverageSequenceError, maxStepsPerSequence, resetThresholdFraction, false, updateType, true);
                    }
                    else
                    {
                        error = (float)addSimpleSequence(networkSpec, vectorizedReberTrainingSequence, maxAverageSequenceError, maxStepsPerSequence, resetThresholdFraction, false, updateType, true);

                    }
                    firstPattern = false;
                    if (error < 0 || error > maxAverageSequenceError)
                    {
                        Assert.assertTrue("Arrays should be equal!", Arrays.equals(before, networkSpec));
                        System.out.println("Failed to  add pattern: " + trainingString);
                        continue;
                    }

                    System.out.println("Successfully added pattern: " + trainingString);

                }
            }

            //Assert.assertTrue(errorMessage, success);

            // ---------------------------------------------
            //              Testing against test set
            // ---------------------------------------------

            boolean useTestSet = false;

            if (useTestSet)
            {
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
            else
            {

                float[] currentInput = getOneHotStringRep("B", alphabet)[0], extrudedOutput;
                float[] extrudeChar;
                ReberGrammarMatcher reberMatcher = new ReberGrammarMatcher();

                boolean testPredictionsP = false;
                MatchStatus status;
                int testCount = 0, numExtrusions = 20;
                String extrudedString = "", inputString;
                StringBuilder sbuilder = new StringBuilder();
                int activationIndex = FastLSTMNetwork.getLayerActivationIndex(networkSpec, FastLSTMNetwork.OUTPUT_LAYER_ID);
                for (testCount = 0; testCount < numExtrusions; testCount++)
                {
                    reberMatcher.reset();
                    sbuilder = new StringBuilder();
                    FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
                    currentInput = getOneHotStringRep("B", alphabet)[0];
                    status = reberMatcher.match("B");
                    sbuilder.append("B");
                    int maxLength = 20, extrapCount = 0;
                    do
                    {
                        inputString = getStringFromOneHotVector(currentInput, alphabet);
                        System.out.println("Input: " + inputString);
                        FastLSTMNetwork.forwardPass(networkSpec, currentInput);
                        extrudedOutput =  FastLSTMNetwork.getOutputActivation(networkSpec);
                        System.out.println("Raw extrapolated output: " + Arrays.toString(extrudedOutput));
                        int selectedCharIndex = FastLSTMNetwork.sampleVectorIndexProportionally(networkSpec, activationIndex, outputNodeCode, 0.15F, false);
                        extrudeChar = new float[outputNodeCode];
                        extrudeChar[selectedCharIndex] = 1;
                        extrudedString = getStringFromOneHotVector(extrudeChar, alphabet);
                        status = reberMatcher.match(extrudedString);
                        System.out.println("Constructed output: " + extrudedString + ".  Match status: " + status.name());
                        if (testPredictionsP)
                        {
                            Assert.assertTrue("Constructed invalid string: " + extrudedString, status == MatchStatus.FINISHED || status == MatchStatus.SUCCESS);
                        }
                        sbuilder.append(extrudedString);
                        currentInput = extrudeChar;
                        extrapCount++;
                    } while (!"E".equals(extrudedString) && extrapCount < maxLength);
                    System.out.println("Constructed: " + sbuilder);

                }
            }


        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }




    // ------------------------------------------------
    //          Helper Functions
    // ------------------------------------------------

    /**
     *
     * @param networkSpec
     * @param inputSequence
     * @param maxAcceptabledError
     * @param maxSteps
     * @return
     */
    double learnSimpleSequence(float[] networkSpec, float[][] inputSequence, float maxAcceptabledError, int maxSteps, float convergenceThreshold, boolean allowWeightResetsP, FastLSTMNetwork.WeightUpdateType updateType, boolean rollbackFailureP)
    {
        LinkedList<FastLSTMNetwork.TrainingSpec> trainingSpec = new LinkedList<FastLSTMNetwork.TrainingSpec>();

        FastLSTMNetwork.TrainingSpec spec;

        for (int i = 0; i < inputSequence.length - 1;i++)
        {
            spec = FastLSTMNetwork.trainingSpec(inputSequence[i], inputSequence[i + 1], null, false, false);
            trainingSpec.add(spec);
        }

        return learnTrainingSpec(networkSpec, trainingSpec, maxAcceptabledError, maxSteps, convergenceThreshold, allowWeightResetsP, updateType);

    }


    /**
     *
     * @param networkSpec
     * @param inputSequence
     * @param maxAcceptabledError
     * @param maxSteps
     * @return if rollbackFailureP is true and function failed to learn the sequence within the allotted stesps, then return the unchanged network
     *         and the negative of the max error
     *
     *         if rollbackFailureP is false and there is a failure then the return value is the maxError as a positive number
     */
    double addSimpleSequence(float[] networkSpec, float[][] inputSequence, float maxAcceptabledError, int maxSteps, float convergenceThreshold, boolean allowWeightResetsP, FastLSTMNetwork.WeightUpdateType updateType, boolean rollbackFailureP)
    {
        LinkedList<FastLSTMNetwork.TrainingSpec> trainingSpec = new LinkedList<FastLSTMNetwork.TrainingSpec>();
        float[] testNetwork = Arrays.copyOf(networkSpec, networkSpec.length);

        FastLSTMNetwork.resetNetworkToInitialState(testNetwork);
        FastLSTMNetwork.TrainingSpec spec;
        boolean recognizeInputP = false;
        float error;
        for (int i = 0; i < inputSequence.length - 1;i++)
        {
            FastLSTMNetwork.forwardPass(testNetwork, inputSequence[i]);
            error = FastLSTMNetwork.updateForwardPassErrors(testNetwork, inputSequence[i + 1]);

            if (error <= maxAcceptabledError)
            {
                spec = FastLSTMNetwork.trainingSpec(inputSequence[i], inputSequence[i + 1], null, false, false);
                recognizeInputP =  true;
            }
            else
            {
                // passing recognizeInputP to skipMinErrorCheck so that the network isn't expected to be able to
                // predict the transition from inputSequence[i] to inputSequence[i + 1] because the transition
                // is non-deterministic
                spec = FastLSTMNetwork.trainingSpec(inputSequence[i], inputSequence[i + 1], null, recognizeInputP, false);
                recognizeInputP = false;
            }

            trainingSpec.add(spec);

        }


        float learningError;
        if (rollbackFailureP)
        {
            learningError = learnTrainingSpec(testNetwork, trainingSpec, maxAcceptabledError, maxSteps, convergenceThreshold, allowWeightResetsP, updateType);

            if (learningError <= maxAcceptabledError)
            {
                for (int i = 0; i < testNetwork.length;i++)
                    networkSpec[i] = testNetwork[i];
                return learningError;
            }
            return -learningError;
        }
        else
        {
            return learnTrainingSpec(networkSpec, trainingSpec, maxAcceptabledError, maxSteps, convergenceThreshold, allowWeightResetsP, updateType);
        }

    }

    float learnTrainingSpec(float[] networkSpec, LinkedList<FastLSTMNetwork.TrainingSpec> trainingSpec, float maxAcceptabledError, int maxSteps, float convergenceThreshold, boolean allowWeightResetsP, FastLSTMNetwork.WeightUpdateType updateType)
    {
        float maxError = Float.MIN_VALUE, error=0, prevError = 0;
        boolean afterWeightInitialization = true;

        for (int i = 0; i < maxSteps; i++)
        {
            FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
            maxError = Float.MIN_VALUE;

            for (FastLSTMNetwork.TrainingSpec spec:trainingSpec)
            {
                if (spec.resetNetworkStateP)
                    FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
                error = FastLSTMNetwork.learnMap(networkSpec, spec, null);
                if (!spec.skipMinAcceptabledErrorCheckP)
                {
                    maxError = Math.max(maxError, error);
                }
            }

            if (maxError < maxAcceptabledError)
                return error;

            if (afterWeightInitialization)
            {
                afterWeightInitialization = false;
                prevError = maxError;
                FastLSTMNetwork.updateWeightsFromErrors(networkSpec, updateType);
            }
            else
            {
                float convergenceFraction = (Math.abs(maxError - prevError)/maxError);
                if (convergenceFraction < convergenceThreshold && allowWeightResetsP)
                {
                    FastLSTMNetwork.initializeAllWeights(networkSpec);
                    afterWeightInitialization = true;

                }
                FastLSTMNetwork.updateWeightsFromErrors(networkSpec, updateType);
                prevError = maxError;
            }
        }

        return maxError;
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

    boolean learnReberGrammar(float[] networkSpec)
    {
        boolean success = false;

        int totalTries = 100;
        int stepTries = 20;
        int traingSetSize = 50;
        int testSetSize = 50;

        FastLSTMNetwork.setSeed(2053116556161410L);


        GrammarStateMachine machine = makeReberGrammar();
        String[] alphabet = ReberGrammar.ALPHABET;

        Pair<HashSet<String>, HashSet<String>> trainingPair = getTrainingTestGrammarSets(machine, traingSetSize, testSetSize, totalTries, stepTries);


        HashSet<String> trainingSet = trainingPair.getLeft();
        HashSet<String> testSet = trainingPair.getRight();


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
            return false;
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


        }

        if (totalAverage < maxAcceptabledPredictionDiff)
        {
            return true;
        }

        return false;
    }


}
