package com.evolved.automata.nn;

import com.evolved.automata.nn.BaseLSTMTester;
import com.evolved.automata.nn.FastLSTMNetwork;
import com.evolved.automata.nn.LSTMNetwork;
import com.evolved.automata.nn.NNTools;
import com.evolved.automata.nn.Vector;
import com.evolved.automata.nn.grammar.GrammarStateMachine;
import com.evolved.automata.nn.grammar.MatchStatus;
import com.evolved.automata.nn.representations.ModelBuilder;
import com.evolved.automata.nn.representations.Tools;
import com.sun.org.apache.xpath.internal.operations.Mod;

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
    static final int MATCH_COUNT = 0;
    static final int FAILURE_COUNT = 1;
    static final int LENGTH = 2;
    static final int WEIGHT = 3;
    static final int ID = 4;

    @Test
    public void testAddNoise()
    {
        try
        {
            float[] baseInput = {0.2F, 0.5F, 0.25F, 1, 0.75F, 0.5F, 0.5F, 0.1F};


            System.out.println("Base input is: " + Arrays.toString(baseInput));
            float[] noisy;
            boolean gaussian;
            float width;
            for (int i = 0;i < 20;i++)
            {
                gaussian = FastLSTMNetwork.randomLCG() < 0.5;
                width = (float) FastLSTMNetwork.randomLCG();
                noisy = FastLSTMNetwork.addNoise(baseInput, width, gaussian);
                System.out.print("Added ");
                if (gaussian)
                    System.out.print("gaussian");
                else
                    System.out.print("uniform");
                System.out.println(" noise yielding: " + Arrays.toString(noisy));
                System.out.println("Rounded: " + Arrays.toString(NNTools.roundToInt(noisy)));
            }




        }
        catch (Exception e)
        {
            e.printStackTrace();;
            Assert.assertTrue("failure", false);
        }
    }



    public void testConvertTrainingSpec()
    {
        try
        {


        }
        catch (Exception e)
        {
            e.printStackTrace();

        }
    }



    @Test
    public void testPatternExtrude()
    {

        String errorMessage = null;
        try
        {


            int[][] testPatterns = new int[][]{
                    {128, 10, 20, 30, 40, 50, 250},
                    {128, 10, 20, 30, 20, 10, 250},
                    {128, 100, 10, 30, 250}
            };

            int bitWidth = 3;
            int range = 256;
            float[][][] testInput = getDiscretizedSequence(testPatterns, bitWidth, range);

            float[] stopValue = stageDiscretize(250, range, bitWidth);
            // =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+=
            //              Building the Network
            // =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+=

            int inputNodeCode = 2*bitWidth;
            int outputNodeCode = 2*bitWidth;
            int numMemoryCellStates = 17;


            FastLSTMNetwork.LSTMNetworkBuilder builder = getStandardBuilder(inputNodeCode, outputNodeCode, numMemoryCellStates);
            FastLSTMNetwork network = builder.build();

            long seed = System.currentTimeMillis();
            FastLSTMNetwork.setSeed(seed);
            System.out.println("Using seed: " + seed);
            float[] networkSpec = network._networkData;

            float resetThresholdFraction = 0.0001F, maxAverageSequenceError = 0.1F, error = 0;
            int maxSteps = 300;
            int maxLearningStages = 1;
            LSTMNetwork.WeightUpdateType updateType = LSTMNetwork.WeightUpdateType.RPROP;
            boolean success = false;
            errorMessage = "Failed to save all training patterns in the LSTM";

            int stage = 0, numStages = 30;

            System.out.println("Attempting to Learn patterns: ");
            for (int k = 0;k < testInput.length;k++)
            {
                for (int j = 0;j < testInput[k].length;j++)
                {
                    if (j > 0)
                        System.out.print(", ");
                    System.out.print(NNTools.stageContinuize(range, testInput[k][j]));
                }
                System.out.println();
            }


            for (stage = 0;stage < numStages; stage++)
            {
                System.out.println("Trying to learn all patterns. . .");
                long start = System.currentTimeMillis();
                FastLSTMNetwork.initializeAllWeights(networkSpec);
                error = learnMultiSequence(networkSpec, testInput, maxAverageSequenceError, maxSteps,resetThresholdFraction, true, updateType, false);
                double stop = (System.currentTimeMillis() - start)/1000.0;
                if (error <= maxAverageSequenceError)
                {
                    System.out.println("Learned the pattern after " + stop + " seconds");
                    break;
                }
                else
                {
                    System.out.println("Failed to learn pattern after " + stop + " seconds");
                }
            }

            double value;
            float noiseWidth = 0.5F;
            float[] noisyInput;
            boolean roundNoiseP = true;
            StringBuilder sbuilder = new StringBuilder();
            if (error <= maxAverageSequenceError)
            {
                float[] initialValue = testInput[0][0];


                float[] currentInput = initialValue;
                for (int i = 0; i < 20;i++)
                {
                    sbuilder = new StringBuilder();
                    FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
                    currentInput = initialValue;
                    boolean second = false;
                    do {
                        value = NNTools.stageContinuize(range, currentInput);
                        System.out.println("Original value is: " + Arrays.toString(currentInput) + " representing " + value);
                        if (second)
                        {

                            noisyInput = FastLSTMNetwork.addNoise(currentInput, noiseWidth, false);
                            if (roundNoiseP)
                                currentInput = NNTools.roundToInt(noisyInput);
                            else
                                currentInput = noisyInput;
                            value = NNTools.stageContinuize(range, currentInput);
                            System.out.println("Noisy input is: "+  Arrays.toString(currentInput) + " representing " + value);
                        }
                        second = true;
                        sbuilder.append(value);
                        FastLSTMNetwork.forwardPass(networkSpec, currentInput);
                        float[] out = FastLSTMNetwork.getOutputActivation(networkSpec);
                        currentInput = out;
                    }while (!Arrays.equals(currentInput, stopValue));
                    System.out.println("Extrapolated " + sbuilder);


                }
            }


        }
        catch (Exception e)
        {
            e.printStackTrace();;
            Assert.assertTrue(errorMessage, false);
        }
    }


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


    @Test
    public void testHierarchicalRepresentationHighLevelExtrapolation()
    {
        String errorMessage = "Failed to create base training level";
        try
        {
            int[] baseInput = new int[]{10, 20, 24, 30, 20, 250, 100, 50, 45, 40, 35, 30, 30, 30, 25, 20, 15, 230, 200, 180, 160, 150, +145, 5, 10, 15, 20, 100, 110, 120, 130, 130, 130, 120, 110, 100, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175, 40, +40, 40, 35, 30, 20, 15, 15, +15, 10, 90, 95, 90, 95, 90, 95, 90, 75, 65, 75, 65, 75, 65, 75, 65};

            float[] nextInput;

            int bitWidth = 3;
            int range = 256;


            // =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+=
            //              Build the TrainingLevel
            // =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+=



            int inputNodeCode = 2*bitWidth;
            int capacity = 15;
            int featureTrainingSteps = 100;
            float[] input;
            //long seed = System.currentTimeMillis();
            long seed = 1488293344623L;
            FastLSTMNetwork.setSeed(seed);
            System.out.println("Using seed: " + seed);

            ModelBuilder base = new ModelBuilder(inputNodeCode, capacity, featureTrainingSteps);
            base.setLSTMViewer(getNumericLSTMNetworkViewer(range, base.getFeatureInitialState(), base.getFeatureFinalState()));
            base.setInputVectorViewer(getNumericInputVectorViewer(range));
            ModelBuilder nextLevel = base.addHigherOrderLevel(10);
            boolean success = false;
            errorMessage = "Failed to learn features";

            if (Tools.DEBUG)
                System.out.println("Generating base feature model of " + Arrays.toString(baseInput));
            base.setAllowLearning(true);
            base.setAllowHigherlevel(false);
            for (int i = 0; i < baseInput.length;i++)
            {
                System.out.println("" + i + ") Trying to learn: " + baseInput[i]);
                input = stageDiscretize(baseInput[i], range, bitWidth);
                base.observePredict(input, null);
            }
            base.finalize(true);
            base.setAllowLearning(false);
            base.setAllowHigherlevel(true);

            String features = base.viewAllPredictors();
            errorMessage = "Failed to construct predictors";
            Assert.assertTrue(errorMessage, features.length() > 10);
            System.out.println("Partitioned into initial features: " + features);



            errorMessage = "Failed to match patterns, building hierarchical representations";

            System.out.println("+++++ Reprocessing initial features into hierarchical model ++++++");
            float[] firstValue = null;
            ModelBuilder.PREDICTOR_MATCH_STATE[] modelState = null, prevModel = null;
            for (int i = 0; i < baseInput.length;i++)
            {
                System.out.println("**********************");
                System.out.println( "("+ i + ") Reprocessing: " + baseInput[i]);
                input = stageDiscretize(baseInput[i], range, bitWidth);
                if (i == 0)
                    firstValue = input;
                modelState = base.observePredict(input, null);
                features = base.viewAllPredictors();
                if (base.previousStateWasNewP())
                    System.out.println("\n" + i + ") Features to be passed to higher level: \n" + features);
                System.out.println("<><><><><><><><><><><><><><><>\n");
            }

            base.finalize(true);

            System.out.println("\n**********************");
            System.out.println("Processing higher order models state: \n" + nextLevel.viewAllPredictors());



            // --+o- -o+-- --+o- -o+-- --+o- -o+-- --+o- -o+-- --+o- -o+--
            //                  Extrapolation
            // --+o- -o+-- --+o- -o+-- --+o- -o+-- --+o- -o+-- --+o- -o+--

            base.setAllowLearning(false);
            ModelBuilder.PREDICTOR_MATCH_STATE[] baseState = new ModelBuilder.PREDICTOR_MATCH_STATE[capacity], currenState;
            for (int i = 0; i < baseState.length;i++)
                baseState[i] = ModelBuilder.PREDICTOR_MATCH_STATE.NOT_MATCHING;

            int selectedFeature = 2;

            baseState[selectedFeature] = ModelBuilder.PREDICTOR_MATCH_STATE.MATCHING_FROM_BEGINNING;


            base.setState(baseState);
            int extrapCount = 0, maxExtrap = 50;
            System.out.println("Extrapolating from: " + base.viewPredictor(selectedFeature));

            System.out.println("total features: " + base.viewAllPredictors());

            StringBuilder outputBuilder = new StringBuilder("{");
            double logicalFeature;
            int pointer = base.getSelectedPredictor();
            input = base.getPrediction();
            while (input!=null && extrapCount < maxExtrap)
            {
                pointer = base.getSelectedPredictor();
                extrapCount++;
                logicalFeature = NNTools.averagedStageContinuize(range, input);
                System.out.println("(" + extrapCount + ") generated: " + logicalFeature);
                outputBuilder.append(logicalFeature + " ");
                base.observePredict(input, null);
                currenState = base.getCurrentState();
                for (int k = 0; k < currenState.length;k++)
                {

                    if (currenState[k] ==ModelBuilder.PREDICTOR_MATCH_STATE.MATCHED_TO_END)
                    {

                        System.out.println("" +  k + " finished.  Selected feature is: " + pointer);
                    }
                }
                input = base.getPrediction();

            }
            outputBuilder.append("}");

            System.out.println("<><><><><><<> Extrapolated: \n" + outputBuilder);

        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }



    @Test
    public void testHierarchicalRepresentationExtrapolation()
    {
        String errorMessage = "Failed to create base training level";
        try
        {
            int[] baseInput = new int[]{10, 20, 24, 30, 20, 250, 100, 50, 45, 40, 35, 30, 30, 30, 25, 20, 15, 230, 200, 180, 160, 150, +145, 5, 10, 15, 20, 100, 110, 120, 130, 130, 130, 120, 110, 100, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175, 40, +40, 40, 35, 30, 20, 15, 15, +15, 10, 90, 95, 90, 95, 90, 95, 90, 75, 65, 75, 65, 75, 65, 75, 65};

            float[] nextInput;

            int bitWidth = 3;
            int range = 256;


            // =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+=
            //              Build the TrainingLevel
            // =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+=



            int inputNodeCode = 2*bitWidth;
            int capacity = 15;
            int featureTrainingSteps = 100;
            float[] input;
            //long seed = System.currentTimeMillis();
            long seed = 1488293344623L;
            FastLSTMNetwork.setSeed(seed);
            System.out.println("Using seed: " + seed);

            ModelBuilder base = new ModelBuilder(inputNodeCode, capacity, featureTrainingSteps);
            base.setLSTMViewer(getNumericLSTMNetworkViewer(range, base.getFeatureInitialState(), base.getFeatureFinalState()));
            base.setInputVectorViewer(getNumericInputVectorViewer(range));
            ModelBuilder nextLevel = base.addHigherOrderLevel(10);
            boolean success = false;
            errorMessage = "Failed to learn features";

            if (Tools.DEBUG)
                System.out.println("Generating hierarchical model of " + Arrays.toString(baseInput));

            for (int i = 0; i < baseInput.length;i++)
            {
                System.out.println("" + i + ") Trying to learn: " + baseInput[i]);
                input = stageDiscretize(baseInput[i], range, bitWidth);
                base.observePredict(input, null);
            }
            base.finalize(true);
            base.setAllowHigherlevel(false);

            String features = base.viewAllPredictors();
            errorMessage = "Failed to construct predictors";
            Assert.assertTrue(errorMessage, features.length() > 10);
            System.out.println("Partitioned into initial features: " + features);



            errorMessage = "Failed to match patterns, building hierarchical representations";

            System.out.println("+++++ Reprocessing initial features into hierarchical model ++++++");
            float[] firstValue = null;
            ModelBuilder.PREDICTOR_MATCH_STATE[] modelState = null, prevModel = null;
            for (int i = 0; i < baseInput.length;i++)
            {
                System.out.println("**********************");
                System.out.println( "("+ i + ") Reprocessing: " + baseInput[i]);
                input = stageDiscretize(baseInput[i], range, bitWidth);
                if (i == 0)
                    firstValue = input;
                modelState = base.observePredict(input, null);
                features = base.viewAllPredictors();
                if (base.previousStateWasNewP())
                    System.out.println("\n" + i + ") Features to be passed to higher level: \n" + features);
                System.out.println("<><><><><><><><><><><><><><><>\n");
            }

            base.finalize(true);

            System.out.println("\n**********************");
            System.out.println("Processing higher order models state: \n" + nextLevel.viewAllPredictors());

            // Third pass

            for (int i = 0; i < baseInput.length;i++)
            {
                System.out.println("._--._--._--._--._--._--._--._--._--._--");
                System.out.println( "("+ i + ") Higher processing: " + baseInput[i]);
                input = stageDiscretize(baseInput[i], range, bitWidth);
                if (i == 0)
                    firstValue = input;
                modelState = base.observePredict(input, null);
                features = base.viewAllPredictors();
                if (base.previousStateWasNewP())
                    System.out.println("\n" + i + ") Features to be passed to higher level: \n" + features);
                System.out.println("._--._--._--._--._--._--._--._--._--._--");
            }

            base.finalize(true);


            // --+o- -o+-- --+o- -o+-- --+o- -o+-- --+o- -o+-- --+o- -o+--
            //                  Extrapolation
            // --+o- -o+-- --+o- -o+-- --+o- -o+-- --+o- -o+-- --+o- -o+--

            base.setAllowLearning(false);
            ModelBuilder.PREDICTOR_MATCH_STATE[] baseState = new ModelBuilder.PREDICTOR_MATCH_STATE[capacity], currenState;
            for (int i = 0; i < baseState.length;i++)
                baseState[i] = ModelBuilder.PREDICTOR_MATCH_STATE.NOT_MATCHING;

            int selectedFeature = 1;

            baseState[selectedFeature] = ModelBuilder.PREDICTOR_MATCH_STATE.MATCHING_FROM_BEGINNING;


            base.setState(baseState);
            int extrapCount = 0, maxExtrap = 100;
            System.out.println("Extrapolating from: " + base.viewPredictor(selectedFeature));

            System.out.println("total features: " + base.viewAllPredictors());

            StringBuilder outputBuilder = new StringBuilder("{");
            double logicalFeature;
            int pointer = base.getSelectedPredictor();
            while ((input = base.getPrediction())!=null && extrapCount < maxExtrap)
            {
                pointer = base.getSelectedPredictor();
                extrapCount++;
                logicalFeature = NNTools.averagedStageContinuize(range, input);
                System.out.println("(" + extrapCount + ") generated: " + logicalFeature);
                outputBuilder.append(logicalFeature + " ");
                base.observePredict(input, null);
                currenState = base.getCurrentState();
                for (int k = 0; k < currenState.length;k++)
                {

                    if (currenState[k] ==ModelBuilder.PREDICTOR_MATCH_STATE.MATCHED_TO_END)
                    {

                        System.out.println("" +  k + " finished.  Selected feature is: " + pointer);
                    }
                }

            }
            outputBuilder.append("}");

            System.out.println("<><><><><><<> Extrapolated: \n" + outputBuilder);

        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }



    @Test
    public void testBaseHierarchicalRepresentation()
    {
        String errorMessage = "Failed to create base training level";
        try
        {
            int[] baseInput = new int[]{10, 20, 24, 30, 20, 250, 100, 50, 45, 40, 35, 30, 30, 30, 25, 20, 15, 230, 200, 180, 160, 150, +145, 5, 10, 15, 20, 100, 110, 120, 130, 130, 130, 120, 110, 100, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175, 40, +40, 40, 35, 30, 20, 15, 15, +15, 10, 90, 95, 90, 95, 90, 95, 90, 75, 65, 75, 65, 75, 65, 75, 65};

            float[] nextInput;

            int bitWidth = 3;
            int range = 256;


            // =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+=
            //              Build the TrainingLevel
            // =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+= =+=



            int inputNodeCode = 2*bitWidth;
            int capacity = 15;
            int featureTrainingSteps = 100;

            //long seed = System.currentTimeMillis();
            long seed = 1488293344623L;
            FastLSTMNetwork.setSeed(seed);
            System.out.println("Using seed: " + seed);

            ModelBuilder base = new ModelBuilder(inputNodeCode, capacity, featureTrainingSteps);
            base.setLSTMViewer(getNumericLSTMNetworkViewer(range, base.getFeatureInitialState(), base.getFeatureFinalState()));
            boolean success = false;
            errorMessage = "Failed to learn features";

            for (int i = 0; i < baseInput.length;i++)
            {
                System.out.println("" + i + ") Trying to learn: " + baseInput[i]);
                float[] input = stageDiscretize(baseInput[i], range, bitWidth);
                base.observePredict(input, null);
            }
            base.finalize(true);

            String features = base.viewAllPredictors();
            errorMessage = "Failed to construct predictors";
            Assert.assertTrue(errorMessage, features.length() > 10);
            System.out.println("Learned patterns: " + features);

            // TODO: add code to detect non-overlapping features

        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    ModelBuilder.VectorInputViewer getNumericInputVectorViewer(final int range)
    {
        return new ModelBuilder.VectorInputViewer()
        {

            @Override
            public String toString(float[] network)
            {
                double out = NNTools.averagedStageContinuize(range, network);
                return ""+ out;
            }
        };
    }

    ModelBuilder.FastLSTMNetworkViewer getNumericLSTMNetworkViewer(final int range, final float[] initialValue, final float[] finalValue)
    {
        return new ModelBuilder.FastLSTMNetworkViewer() {
            @Override
            public String toString(float[] networkSpec)
            {
                float[] copy =  Arrays.copyOf(networkSpec, networkSpec.length);
                FastLSTMNetwork.resetNetworkToInitialState(copy);
                FastLSTMNetwork.forwardPass(copy, initialValue);
                float[] output = NNTools.roundToInt(FastLSTMNetwork.getOutputActivation(copy));
                StringBuilder builder = new StringBuilder("{");
                boolean second = false;
                while (!Arrays.equals(output, finalValue))
                {
                    if (second)
                        builder.append(", ");
                    second = true;
                    builder.append(NNTools.averagedStageContinuize(range, NNTools.unwrapVector(output)));
                    FastLSTMNetwork.forwardPass(copy, output);
                    output = NNTools.roundToInt(FastLSTMNetwork.getOutputActivation(copy));
                }
                return builder.append("}").toString();
            }
        };
    }

    // ------------------------------------------------
    //          Helper Functions
    // ------------------------------------------------




    /**
     *
     * @param networkSpec
     * @param startInput
     * @param stopInput
     * @return
     */
    static double createCapptedLSTM(float[] networkSpec, float[] startInput, float[] stopInput)
    {
        LinkedList<FastLSTMNetwork.TrainingSpec> trainingSpec = new LinkedList<FastLSTMNetwork.TrainingSpec>();

        trainingSpec.add(FastLSTMNetwork.trainingSpec(startInput, stopInput, null, false, false));

        float maxAcceptabledError = 0.1F;
        FastLSTMNetwork.initializeAllWeights(networkSpec);
        float error = learnTrainingSpec(networkSpec, trainingSpec, maxAcceptabledError, 20, 0.001F, true, LSTMNetwork.WeightUpdateType.RPROP);
        FastLSTMNetwork.setCustomData(networkSpec, 0, MATCH_COUNT);
        FastLSTMNetwork.setCustomData(networkSpec, 0, FAILURE_COUNT);
        FastLSTMNetwork.setCustomData(networkSpec, 0, WEIGHT);
        return error;
    }


    /**
     *
     * @param cappedNetworkSpec
     * @param startInput
     * @param stopInput
     * @param newInput
     * @param maxAcceptabledError
     * @param maxSteps
     * @param convergenceThreshold
     * @param allowWeightResetsP
     * @param updateType
     * @return
     */
    static double appendVectorToSequence(float[] cappedNetworkSpec, float[] startInput, float[] stopInput, float[] newInput, float maxAcceptabledError, int maxSteps, float convergenceThreshold, boolean allowWeightResetsP, FastLSTMNetwork.WeightUpdateType updateType)
    {
        float[] output;
        float[] copy = Arrays.copyOf(cappedNetworkSpec, cappedNetworkSpec.length);

        LinkedList<FastLSTMNetwork.TrainingSpec> trainingSpec = new LinkedList<FastLSTMNetwork.TrainingSpec>();

        FastLSTMNetwork.TrainingSpec spec;
        int dataId = (int)AdvancedFastLSTMTester.getId(copy);
        float[] input = startInput;
        FastLSTMNetwork.resetNetworkToInitialState(copy);
        FastLSTMNetwork.forwardPass(copy, input);
        output = NNTools.roundToInt(FastLSTMNetwork.getOutputActivation(copy));
        int priorLength = getCappedLSTMLength(cappedNetworkSpec);

        int dataWidth = startInput.length;

        while (!Arrays.equals(output, stopInput))
        {
            spec = FastLSTMNetwork.trainingSpec(input, output, null, false, false);
            trainingSpec.add(spec);
            input = output;
            FastLSTMNetwork.forwardPass(copy, input);
            output = NNTools.roundToInt(FastLSTMNetwork.getOutputActivation(copy));
        }
        spec = FastLSTMNetwork.trainingSpec(input, newInput, null, false, false);
        trainingSpec.add(spec);
        spec = FastLSTMNetwork.trainingSpec(newInput, stopInput, null, false, false);
        trainingSpec.add(spec);
        FastLSTMNetwork.initializeAllWeights(copy);

        float error = learnTrainingSpec(copy, trainingSpec, maxAcceptabledError, maxSteps, convergenceThreshold, allowWeightResetsP, updateType);

        if (error == 0)
        {
            for (int i = 0; i < cappedNetworkSpec.length;i++)
            {
                cappedNetworkSpec[i] = copy[i];
            }

            incrementLength(cappedNetworkSpec);
        }

        return error;
    }

    static void resetCappedLSTM(float[] networkSpec, float[] initialState)
    {
        resetCappedLSTM(networkSpec, initialState, false);

    }

    static void resetCappedLSTM(float[] networkSpec, float[] initialState, boolean onlyMetaDataP)
    {

        if (!onlyMetaDataP)
        {
            FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
            FastLSTMNetwork.forwardPass(networkSpec, initialState);
        }
        FastLSTMNetwork.setCustomData(networkSpec, 0, MATCH_COUNT);
        FastLSTMNetwork.setCustomData(networkSpec, 0, FAILURE_COUNT);
        FastLSTMNetwork.setCustomData(networkSpec, 0, WEIGHT);

    }


    static boolean isAtFinalState(float[] networkSpec, float[] finalState)
    {
        float[] out = NNTools.roundToInt(FastLSTMNetwork.getOutputActivation(networkSpec));
        return Arrays.equals(out, finalState);
    }

    static void incrementLength(float[] networkSpec)
    {
        setCappedLSTMLength(networkSpec, 1 + getCappedLSTMLength(networkSpec));
    }

    static void setCappedLSTMLength(float[] networkSpec, int i)
    {
        FastLSTMNetwork.setCustomData(networkSpec, i, LENGTH);
    }


    static int getCappedLSTMLength(float[] networkSpec)
    {
        return (int)FastLSTMNetwork.getCustomData(networkSpec, LENGTH, -1);
    }


    static void setCappedLSTMWeight(float[] networkSpec, float weight)
    {
        FastLSTMNetwork.setCustomData(networkSpec, weight, WEIGHT);
    }


    static float getCappedLSTMWeight(float[] networkSpec)
    {
        return FastLSTMNetwork.getCustomData(networkSpec, WEIGHT, -1);
    }


    static int getMatchCount(float[] networkSpec)
    {
        return (int)FastLSTMNetwork.getCustomData(networkSpec, MATCH_COUNT, -1);
    }

    static void incrementMatchCount(float[] networkSpec)
    {
        int prior = (int)FastLSTMNetwork.getCustomData(networkSpec, MATCH_COUNT, -1);
        FastLSTMNetwork.setCustomData(networkSpec, prior + 1, MATCH_COUNT);

    }

    static void incrementFailureCount(float[] networkSpec)
    {
        int prior = (int)FastLSTMNetwork.getCustomData(networkSpec, FAILURE_COUNT, -1);
        FastLSTMNetwork.setCustomData(networkSpec, prior + 1, FAILURE_COUNT);
    }

    static int getFailureCount(float[] networkSpec)
    {
        return (int)FastLSTMNetwork.getCustomData(networkSpec, FAILURE_COUNT, -1);
    }

    static void setId(float[] networkSpec, float id)
    {
        FastLSTMNetwork.setCustomData(networkSpec, id, ID);
    }

    static float getId(float[] networkSpec)
    {
        return FastLSTMNetwork.getCustomData(networkSpec, ID, -1) ;
    }

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



    float learnMultiSequence(float[] networkSpec, float[][][] inputSequence, float maxAcceptabledError, int maxSteps, float convergenceThreshold, boolean allowWeightResetsP, FastLSTMNetwork.WeightUpdateType updateType, boolean rollbackFailureP)
    {
        float[] testNetwork = Arrays.copyOf(networkSpec, networkSpec.length);

        LinkedList<FastLSTMNetwork.TrainingSpec> trainingSpec = new LinkedList<FastLSTMNetwork.TrainingSpec>();

        FastLSTMNetwork.TrainingSpec spec;
        boolean recognizeInputP = false;
        float error;

        for (int j = 0; j < inputSequence.length;j++)
        {
            FastLSTMNetwork.resetNetworkToInitialState(testNetwork);
            for (int i = 0; i < inputSequence.length - 1;i++)
            {
                FastLSTMNetwork.forwardPass(testNetwork, inputSequence[j][i]);
                error = FastLSTMNetwork.getOutputError(testNetwork, inputSequence[j][i + 1]);

                if (error <= maxAcceptabledError)
                {
                    spec = FastLSTMNetwork.trainingSpec(inputSequence[j][i], inputSequence[j][i + 1], null, false, i == 0);
                    recognizeInputP =  true;
                }
                else
                {
                    // passing recognizeInputP to skipMinErrorCheck so that the network isn't expected to be able to
                    // predict the transition from inputSequence[i] to inputSequence[i + 1] because the transition
                    // is non-deterministic
                    spec = FastLSTMNetwork.trainingSpec(inputSequence[j][i], inputSequence[j][i + 1], null, recognizeInputP, i == 0);
                    recognizeInputP = false;
                }

                trainingSpec.add(spec);
            }
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
    float addSimpleSequence(float[] networkSpec, float[][] inputSequence, float maxAcceptabledError, int maxSteps, float convergenceThreshold, boolean allowWeightResetsP, FastLSTMNetwork.WeightUpdateType updateType, boolean rollbackFailureP)
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


    static boolean maskedRoundedVerifyResult(float[] value, float[] expectedValue, float[] errorMask)
    {
        for (int i = 0;i < value.length;i++)
        {
            if (((errorMask != null && errorMask[i] == 1) || errorMask == null) && NNTools.roundToInt(expectedValue[i]) != NNTools.roundToInt(value[i]))
                return false;
        }
        return true;
    }


    static boolean verifyTrainingResult(float[][] trainingResults, LinkedList<FastLSTMNetwork.TrainingSpec> trainingSpec)
    {

        int i = 0, j;
        float[] expected;
        for (FastLSTMNetwork.TrainingSpec spec: trainingSpec)
        {
            expected = spec.expectedOutput;
            for (j = 1;j < trainingResults[i].length;j++)
            {
                if (expected[j-1] != NNTools.roundToInt(trainingResults[i][j]))
                    return false;
            }
            i++;
        }
        return true;
    }

    static float  learnTrainingSpec(float[] networkSpec, LinkedList<FastLSTMNetwork.TrainingSpec> trainingSpec, float maxAcceptabledError, int maxSteps, float convergenceThreshold, boolean allowWeightResetsP, FastLSTMNetwork.WeightUpdateType updateType)
    {
        float maxError = Float.MIN_VALUE, error=0, prevError = 0, averageError = 0;
        boolean afterWeightInitialization = true, returnAverage = false;
        float compError=0;
        float[][] errors = new float[trainingSpec.size()][];
        float[] fullResult;
        int j;
        int i = 0;
        for (i=0; i < maxSteps; i++)
        {
            FastLSTMNetwork.resetNetworkToInitialState(networkSpec);
            maxError = Float.MIN_VALUE;
            j = 0;
            for (FastLSTMNetwork.TrainingSpec spec:trainingSpec)
            {
                returnAverage =spec.useAverageErrorP;
                if (spec.resetNetworkStateP)
                    FastLSTMNetwork.resetNetworkToInitialState(networkSpec);


                fullResult = FastLSTMNetwork.learnMapWithDetails(networkSpec, spec, null);
                error = fullResult[0];
                averageError = averageError*i/(i+1F)+error/(i + 1F);
                if (!spec.skipMinAcceptabledErrorCheckP)
                {
                    maxError = Math.max(maxError, error);
                }

                if (returnAverage)
                    compError = averageError;
                else
                    compError = maxError;

                errors[j++] = fullResult;
            }



            boolean verify =  verifyTrainingResult(errors, trainingSpec);
            if (verify)
            {
                System.out.println("Verified with erro: " + compError);
                return 0;
            }


            if (afterWeightInitialization)
            {
                afterWeightInitialization = false;
                prevError = compError;
                FastLSTMNetwork.updateWeightsFromErrors(networkSpec, updateType);
            }
            else
            {
                float convergenceFraction = (Math.abs(compError - prevError)/compError);
                if (convergenceFraction < convergenceThreshold && allowWeightResetsP)
                {
                    FastLSTMNetwork.initializeAllWeights(networkSpec);
                    afterWeightInitialization = true;

                }
                FastLSTMNetwork.updateWeightsFromErrors(networkSpec, updateType);
                prevError = compError;
            }
        }

        return maxError;
    }



    static FastLSTMNetwork.LSTMNetworkBuilder getStandardBuilder(int inputNodeCode, int outputNodeCode, int numMemoryCellStates)
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

    float[][][] getDiscretizedSequence(int[][] patterns, int bitWidth, int range)
    {
        float[][][] out = new float[patterns.length][][];
        for (int pcount = 0;pcount < patterns.length;pcount++)
        {
            float[][] pattern = new float[patterns[pcount].length][];
            for (int i = 0; i < patterns[pcount].length;i++)
            {
                ArrayList<Double> discretized = NNTools.stageDiscretize(patterns[pcount][i], range, bitWidth);
                pattern[i] = new float[discretized.size()];
                for (int j = 0; j < discretized.size();j++)
                {
                    pattern[i][j] = discretized.get(j).floatValue();
                }
            }
            out[pcount] = pattern;
        }

        return out;
    }





}
