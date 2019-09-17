package com.evolved.automata.nn;

import com.evolved.automata.ConcurrentGenerator;
import com.evolved.automata.InterruptibleResultProducer;
import com.evolved.automata.lisp.ListValue;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.StringHashtableValue;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.lisp.nn.LSTMNetworkProxy;
import com.evolved.automata.nn.util.FeatureModel;
import com.evolved.automata.nn.util.Group;
import com.evolved.automata.nn.util.IncrementalUpdateSpec;
import com.evolved.automata.nn.util.LearningConfiguration;
import com.evolved.automata.nn.util.WorldModel;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Random;

import static com.evolved.automata.nn.FastLSTMNetwork.roundToInt;

/**
 * Created by Evolved8 on 5/28/18.
 */

public class LSTMGroupTests {

    class SimpleAllocator {

        int _maxFeatures;
        int _allocationIndex = 0;

        public SimpleAllocator(int max){
            _maxFeatures = max;
        }
        public FeatureModel getFeature(){
            _allocationIndex++;
            FeatureModel model = null;
            if (_allocationIndex <= _maxFeatures){
                model = new FeatureModel(10, 30, WorldModel.getFeatureLearningConfiguration());
            }
            return model;
        }
    }


    final int[] baseTestInput = new int[]{5, 5, 5, 4, 4, 3, 3, 2, 2, 1, 2, 2, 3, 6, 10, 10, 5, 3};
    final int baseTestRadiix = 10;

    FeatureModel[] buffer = null;

    @Test
    public void testLearningSequence(){

        String errorMessage = "failed to create network";
        try
        {
            int[] rawInput = new int[]{5, 5, 5, 5, 4, 4, 3, 3, 2, 2, 1, 2, 2, 3, 6, 10, 10, 5, 3};
            int inputNodeCode = 10;
            int outputNodeCode = 10;
            int numMemoryCellStates = 30;
            int flags = 0;

            LSTMNetworkProxy networkProxy = LSTMNetworkProxy.makeStandardBinarySequenceNetwork(inputNodeCode, numMemoryCellStates, flags);

            LearningConfiguration context = getSimpleLearningConfiguration();

            int subSequence = 5;

            ArrayList<Vector> inputVector = convert(rawInput, inputNodeCode, subSequence);

            errorMessage = "Failed to get input output spec";
            ArrayList<Pair<Vector, Vector>> inputOutputSpec =  getInputOututSpec(inputVector);

            errorMessage = "Failed to learn sequence";
            IncrementalUpdateSpec result = simpleLearnSequence(networkProxy, inputOutputSpec, context);
            ArrayList<Vector> extrapolated = null;
            if (result.successCriteriaSatisfied()) {
                errorMessage = "Failed to extrapolate output";
                extrapolated = result.extrapolateRange();
                int[] extrapolatedValue = vectorsToInts(extrapolated);
                System.out.println("Successfully extrapolated: " + Arrays.toString(extrapolatedValue));
            }
            else {
                System.out.println("Failed to extrapolate");
            }

        }
        catch (Exception e){
            Assert.assertTrue(errorMessage, false);
        }
    }



    @Test
    public void testLearningRepetitiveSequence(){

        String errorMessage = "failed to create network";
        try
        {
            int[] rawInput = new int[]{5, 5, 5, 5, 4, 4, 3, 3, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 6,6,6, 10, 10, 5, 3};
            int inputNodeCode = 10;
            int outputNodeCode = 10;
            int numMemoryCellStates = 30;
            int flags = 0;
            long start = System.currentTimeMillis();
            LSTMNetworkProxy networkProxy = LSTMNetworkProxy.makeStandardBinarySequenceNetwork(inputNodeCode, numMemoryCellStates, flags);

            LearningConfiguration context = getSimpleLearningConfiguration();

            int subSequence = rawInput.length;

            ArrayList<Vector> inputVector = convert(rawInput, inputNodeCode, subSequence);

            errorMessage = "Failed to get input output spec";
            ArrayList<Pair<Vector, Vector>> inputOutputSpec =  getInputOututSpec(inputVector);

            start = System.currentTimeMillis();
            errorMessage = "Failed to learn sequence";
            IncrementalUpdateSpec result = simpleLearnSequence(networkProxy, inputOutputSpec, context);
            long duration = (System.currentTimeMillis() - start);
            ArrayList<Vector> extrapolated = null;
            if (result.successCriteriaSatisfied()) {
                errorMessage = "Failed to extrapolate output";
                extrapolated = result.extrapolateRange();
                int[] extrapolatedValue = vectorsToInts(extrapolated);
                System.out.println("After: (" + duration + ") Successfully extrapolated: " + Arrays.toString(extrapolatedValue));
            }
            else {
                System.out.println("|" + subSequence +  "| After: (" + duration + ") Failed to extrapolate");
            }

        }
        catch (Exception e){
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testStagedSequence(){

        String errorMessage = "failed to create network";
        try
        {
            int[] rawInput = new int[]{5, 5, 5, 5, 4, 4, 3, 3, 2, 2, 1, 2, 2, 3, 6, 10, 10, 5, 3};
            int inputNodeCode = 10;
            int outputNodeCode = 10;
            int numMemoryCellStates = 30;
            int flags = 0;

            LSTMNetworkProxy networkProxy = LSTMNetworkProxy.makeStandardBinarySequenceNetwork(inputNodeCode, numMemoryCellStates, flags);

            LearningConfiguration context = getSimpleLearningConfiguration();

            int maxLength = 7;

            for (int subSequence = 2; subSequence < maxLength; subSequence++){
                ArrayList<Vector> inputVector = convert(rawInput, inputNodeCode, subSequence);
                int[] original = vectorsToInts(inputVector);
                System.out.println("Trying to learn: " + Arrays.toString(original));
                errorMessage = "Failed to get input output spec";
                ArrayList<Pair<Vector, Vector>> inputOutputSpec =  getInputOututSpec(inputVector);

                errorMessage = "Failed to learn sequence";
                IncrementalUpdateSpec result = simpleLearnSequence(networkProxy, inputOutputSpec, context);
                ArrayList<Vector> extrapolated = null;
                if (result.successCriteriaSatisfied()) {
                    errorMessage = "Failed to extrapolate output";
                    extrapolated = result.extrapolateRange(true);
                    int[] extrapolatedValues= vectorsToInts(extrapolated);

                    errorMessage = "Failed to extrapolate correct output length: expected " + original.length +" but found: " + extrapolatedValues.length;
                    Assert.assertTrue(errorMessage, extrapolatedValues.length == original.length);
                    for (int j = 0;j<extrapolatedValues.length;j++){
                        errorMessage = "Failed to learn: x[" + j +"] = " + original[j];
                        Assert.assertTrue(errorMessage, original[j] == extrapolatedValues[j]);
                    }
                    System.out.println("Successfully extrapolated: " + Arrays.toString(extrapolatedValues));
                }
                else {
                    System.out.println("Failed to extrapolate: " + Arrays.toString(original));
                }

            }
        }
        catch (Exception e){
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testIncrementalLearning(){
        String errorMessage = "failed to create network";
        try
        {
            errorMessage = "Failed to create feature model";
            int[] rawInput = new int[]{5, 5, 5, 4, 4, 3, 3, 2, 2, 1, 2, 2, 3, 6, 10, 10, 5, 3};
            int inputNodeCode = 10;
            int numMemoryCellStates = 30;

            int bufferSize = 10;

            buffer = new FeatureModel[bufferSize];


            FeatureModel model = new FeatureModel(inputNodeCode, numMemoryCellStates, getSimpleLearningConfiguration().setMaxIterations(2000).setDebugLevel(0));

            for (int i = 0; i < rawInput.length;i++){
                ArrayList<Vector> inputVector = convert(rawInput, inputNodeCode, i+1);
                int[] original = vectorsToInts(inputVector);
                System.out.println("Original: " + Arrays.toString(original));

                Vector input = intToTallyVector(rawInput[i], 10);
                errorMessage = "Failed to process: " + rawInput[i];
                model.processNextInput(input);
                ArrayList<Vector> extrapolated = model.extrapFeature();
                int[] output = vectorsToInts(extrapolated);
                System.out.println("(" + i + ") Partial: " + Arrays.toString(output));

                if (model.isComplete()){
                    System.out.println("Finished.  Validation skipped.  Couldn't learn: " + Arrays.toString(original));
                    break;
                }
                else {
                    errorMessage = "Failed to extrapolate correct length: expected " + original.length + " but found: " + output.length;
                    Assert.assertTrue(errorMessage, output.length == original.length);
                    for (int j = 0; j<original.length;j++){
                        errorMessage = "Failed to learn value: (" + j + ") " + original[j] + " found: " + output[j];
                        Assert.assertTrue(errorMessage, output[j] == original[j]);
                    }
                }
            }

            errorMessage = "failed to extrapolate feature";
            ArrayList<Vector> extrapolated = model.extrapFeature();
            int[] output = vectorsToInts(extrapolated);
            System.out.println("Final output: " + Arrays.toString(output));

        }
        catch (Exception e){
            Assert.assertTrue(errorMessage, false);
        }
    }


    private ArrayList<Vector> trainFeature(FeatureModel model, ArrayList<Vector> base, int minRequiredLength){
        model.forceInitialState();
        LearningConfiguration originalConfig = model.getConfiguration();
        LearningConfiguration unlimitedConfig = originalConfig.getUnlimitedConfiguration();
        model.setConfiguration(unlimitedConfig);

        for (Vector input:base) {
            System.out.println("Learning: " + tallyVectorToInteger(input.rawFloat()));
            if (!model.isComplete()){
                if (model.getFeatureLength() > minRequiredLength){
                    System.out.println("Switching to constrained learning");
                    model.setConfiguration(originalConfig);
                }
                model.processNextInput(input);
            }
            else
                break;
        }
        return model.extrapFeature();
    }

    private int[] getRandomTestInput(int size, Integer radiix, Long seed){
        if (radiix == null){
            return baseTestInput;
        }
        int[] out = new int[size];
        if (seed != 0)
            FastLSTMNetwork.setSeed(seed);
        for (int i = 0; i < size;i++){
            double ran;
            if (seed != 0 )
                ran = FastLSTMNetwork.randomLCG();
            else
                ran = Math.random();
            out[i] = (int)(ran*radiix);
        }
        return out;
    }

    @Test
    public void testExtrapolation(){
        String errorMessage = "";

        try
        {
            errorMessage = "Failed to create feature model";

            long seed = System.currentTimeMillis();
            int radiix = 10;
            int size = 25;
            int prefixSize = 3;
            System.out.println("trying prefix extrapolation with seed [" + seed + "] and radiix " + radiix);

            int[] rawInput = getRandomTestInput(size,radiix,  seed);

            System.out.println("Trying to learn: " + Arrays.toString(rawInput));
            int inputNodeCode = 10;
            int numMemoryCellStates = 30;

            FeatureModel model = new FeatureModel(inputNodeCode, numMemoryCellStates, WorldModel.getFeatureLearningConfiguration().setDebugLevel(0));

            errorMessage = "Failed to learn any of " + Arrays.toString(rawInput);

            ArrayList<Vector> learned = trainFeature(model, convert(rawInput, 10), 2*prefixSize);

            int[] output = vectorsToInts(learned);
            System.out.println("Feature consists of: " + Arrays.toString(output));

            if (output.length > 2 * prefixSize){
                int[] drivingInput = getSuffix(output, prefixSize);
                System.out.println("Trying to continue pattern: " + Arrays.toString(drivingInput));
                errorMessage = "Failed to reset recognition";
                model.resetRecognition();
                int driveIndex = 0;
                for (int i = 0; i < drivingInput.length;i++){
                    errorMessage = "Failed to extrapolate from " + drivingInput[i];
                    Vector input = intToTallyVector(drivingInput[i], radiix);

                    Vector predictedOutput = model.getPredictedOutput();
                    if (driveIndex > prefixSize){
                        System.out.println("Expect match with: [" + drivingInput[i] +" -> " + tallyVectorToInteger(predictedOutput.rawFloat()) + "] " + model.getDistanceToFinalState());
                    }
                    else {
                        System.out.println("Driving model with: [" + drivingInput[i] + " -> " + tallyVectorToInteger(predictedOutput.rawFloat()) + "] "+ model.getDistanceToFinalState());
                    }

                    FeatureModel.STATE out = model.processNextInput(input);

                    errorMessage = "Failed to start recognizing pattern after  " + driveIndex + " steps.";
                    //Assert.assertTrue(errorMessage, (driveIndex < prefixSize) || out == FeatureModel.STATE.MATCHING);
                    driveIndex++;
                }
            }
            else {
                System.out.println("Insufficient learning");
            }

        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testFeatureSerialization(){
        String errorMessage = "";

        try
        {
            errorMessage = "Failed to create feature model";

            boolean useCustomSeed = true;
            long priorSeed = 1530494192910L;
            long seed = System.currentTimeMillis();

            if (useCustomSeed)
                seed = priorSeed;

            int radiix = 10;
            int size = 25;
            int prefixSize = 3;
            System.out.println("trying prefix extrapolation with seed [" + seed + "] and radiix " + radiix);

            int[] rawInput = getRandomTestInput(size,radiix,  seed);

            System.out.println("Trying to learn: " + Arrays.toString(rawInput));
            int inputNodeCode = 10;
            int numMemoryCellStates = 30;

            FeatureModel model = new FeatureModel(inputNodeCode, numMemoryCellStates, WorldModel.getFeatureLearningConfiguration().setDebugLevel(0));

            errorMessage = "Failed to learn any of " + Arrays.toString(rawInput);

            ArrayList<Vector> learned = trainFeature(model, convert(rawInput, 10), 2*prefixSize);

            int[] output = vectorsToInts(learned);
            System.out.println("Feature consists of: " + Arrays.toString(output));

            if (output.length > 2 * prefixSize){
                errorMessage = "Failed to serialize feature model";

                byte[] serialized = model.serializeBytes();

                Assert.assertTrue(errorMessage, serialized != null && serialized.length>0);

                errorMessage = "Failed to deserialize feature model";
                FeatureModel deserialized = FeatureModel.deserializeBytes(serialized);

                Assert.assertTrue(errorMessage, deserialized != null);

                errorMessage = "failed to extrapolate deserialized data";
                ArrayList<Vector> data = deserialized.extrapFeature();

                errorMessage = "Failed to match new vs old extrapolated value size";
                Assert.assertTrue(errorMessage, data != null && data.size() == learned.size());

                int[] items = vectorsToInts(data);

                for (int i = 0;i<items.length;i++){
                    errorMessage = "Failed to match extrapolated item values: i = " + i + " expectd " + output[i] + " but found " + items[i];
                    Assert.assertTrue(errorMessage, items[i] == output[i]);
                }

                System.out.println("Output: " + Arrays.toString(items));

            }
            else {
                System.out.println("Insufficient learning");
            }

        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }



    @Test
    public void testLimitedFeatures(){

        String errorMessage = "failed to create FeatureModel";
        try
        {
            errorMessage = "Failed to create feature model";
            int[] rawInput = new int[]{5, 5, 5, 4, 4, 3, 3, 2, 2, 1, 2, 2, 3, 6, 10, 10, 5, 3};
            int inputNodeCode = 10;
            int numMemoryCellStates = 30;
            FeatureModel model = new FeatureModel(inputNodeCode, numMemoryCellStates, WorldModel.getFeatureLearningConfiguration().getUnlimitedConfiguration().setDebugLevel(0).addAllowedDebugTag("FINISHED"));
            int bufferSize = 5;

            for (int i = 0; i < rawInput.length;i++){
                Vector input = intToTallyVector(rawInput[i], inputNodeCode);
                if (model.getFeatureLength() == bufferSize){
                    System.out.println("Shifing");
                    errorMessage = "Failed to shift model forward";
                    model.shiftForward();
                }

                errorMessage = "Failed to process: " + rawInput[i];
                model.processNextInput(input);
                errorMessage = "Failed to extrapolate model";
                ArrayList<Vector> out = model.extrapFeature();
                System.out.println("Learned: " + Arrays.toString(vectorsToInts(out)));
            }


        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testLearningLongSequences(){
        String errorMessage = "Failed to create simple allocator";
        try
        {

            int featureBufferSize = 4;
            int minimumBufferOverlap = 3;
            int maxBufferSize = 20;
            SimpleAllocator allocator = new SimpleAllocator(maxBufferSize);

            long seed = System.currentTimeMillis();
            int radiix = 10;
            int size = 25;
            System.out.println("trying prefix extrapolation with seed [" + seed + "] and radiix " + radiix);

            int[] rawInput = getRandomTestInput(size,radiix,  seed);

            LearningConfiguration baseConfig = WorldModel.getFeatureLearningConfiguration().setDebugLevel(0).addAllowedDebugTag("FINISHED");
            LearningConfiguration currentUnlimitedConfig =baseConfig.getUnlimitedConfiguration();
            LearningConfiguration bufferUnlimitedConfig = baseConfig.getUnlimitedConfiguration();

            // Initialization of the current and buffer models
            FeatureModel currentModel = allocator.getFeature();
            currentModel.setConfiguration(currentUnlimitedConfig);

            FeatureModel bufferModel = allocator.getFeature();
            bufferModel.setConfiguration(bufferUnlimitedConfig);

            ArrayList<FeatureModel> completedFeatures = new ArrayList<FeatureModel>();

            for (int i = 0; i < rawInput.length; i++){
                Vector input = intToTallyVector(rawInput[i], radiix);
                System.out.println("learning: " + rawInput[i]);

                if (currentModel == null){
                    System.out.println("Finished due to feature allocator exhaustion");
                    break;
                }

                if (currentModel.getFeatureLength() >= featureBufferSize + minimumBufferOverlap){
                    System.out.println("Extending current model: " + currentModel.getFeatureLength());
                    currentModel.setConfiguration(baseConfig );
                }
                else {
                    currentModel.setConfiguration(currentUnlimitedConfig);
                }

                if (bufferModel != null &&  bufferModel.getFeatureLength() == featureBufferSize){
                    bufferModel.shiftForward();
                }

                currentModel.processNextInput(input);
                if (bufferModel != null) {
                    bufferModel.processNextInput(input);
                }

                if (currentModel.isComplete()){
                    completedFeatures.add(currentModel);
                    //FeatureModel temp = bufferModel;
                    currentModel = bufferModel;

                    bufferModel = allocator.getFeature();
                    bufferModel.setConfiguration(baseConfig.getUnlimitedConfiguration());
                }
            }

            if (currentModel != null){
                completedFeatures.add(currentModel);
            }


            System.out.println("Results of: " + Arrays.toString(rawInput));
            for (FeatureModel learnedModel:completedFeatures){
                System.out.println("" + Arrays.toString(vectorsToInts(learnedModel.extrapFeature())));
            }

        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testLearningLongSequencesInGroups(){
        String errorMessage = "Failed to create Initial input sequence";
        try
        {

            System.out.println(String.format("%1$.3f, %1$.7f  %2$02d, %3$02d", 2.3455, 1, 22));
            int maxAllocation = 20;
            int initialGroupWeight = 1;
            int featureBufferSize = 4;
            int minimumBufferOverlap = 3;

            int size = 25;
            long seed = System.currentTimeMillis();
            int radiix = 10;

            //int[] rawInput = getRandomTestInput(size,radiix,  seed);
            int[] rawInput = new int[]{5, 5, 5, 4, 4, 3, 3, 2, 2, 1, 2, 2, 3, 6, 10, 10, 5, 3};
            System.out.println("trying prefix extrapolation with seed [" + seed + "] and radiix " + radiix + " resulting in: " + Arrays.toString(rawInput));
            errorMessage = "Failed to create World Model";
            // using default LearningConfiguration

            // Step (1) - Create WorldModel

            LearningConfiguration base = WorldModel.getFeatureLearningConfiguration();
            base.setInputValidator((float[] input) -> (tallyVectorToInteger(input) != null));

            WorldModel world = new WorldModel(maxAllocation, base);

            errorMessage = "Failed to create new group type";

            // Step (2) - Create the Group type if not using the predefined BASIC (world.BASIC)
            WorldModel.GroupType DEFAULT_TYPE = world.createGroupType("DEFAULT", radiix, 30, initialGroupWeight, featureBufferSize, minimumBufferOverlap, WorldModel.getFeatureLearningConfiguration());

            Group DEFAULT_GROUP = world.addGroup("DEFAULT", DEFAULT_TYPE);
            DEFAULT_GROUP.setMode(Group.MODE.MIXED);

            int j = 0;
            for (int i = 0; i < rawInput.length; i++){
                Vector input = intToTallyVector(rawInput[i], radiix);

                errorMessage = "Failed to process input " + rawInput[i];
                System.out.println("learned: " + rawInput[i]);

                // Step (3) process all group types that you want
                ArrayList<WorldModel.GroupSpecification> results = world.processNextInput(input, DEFAULT_TYPE);

                errorMessage = "Failed to obtain correct result size.  Expected 1 but found " + results.size();
                Assert.assertTrue(errorMessage, results.size() == 1);
                WorldModel.GroupSpecification result = results.get(0);
                j = 0;
                errorMessage = "Failed to get all features";
                ArrayList<FeatureModel> existingModels = result.getGroup().getAllFeatures();

                for (FeatureModel feature: existingModels){

                    System.out.println("" + j + ") " + feature.toString());
                    j++;
                }
            }

            errorMessage = "Failed to assert boundary";

            world.assertBoundary(DEFAULT_TYPE);
            DEFAULT_GROUP.setMode(Group.MODE.EXTRAPOLATION);

            errorMessage = "Failed to obtain all features";
            // Step (4) view all features, ordered

            System.out.println("(-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-)");
            System.out.println("Finished learning: " + Arrays.toString(rawInput));
            System.out.println("Feature learned data:");
            ArrayList<FeatureModel> existingModels = DEFAULT_GROUP.getAllFeatures();

            j = 0;
            for (FeatureModel feature: existingModels){

                if (feature.isComplete()){
                    System.out.println("" + j + ") " + Arrays.toString(vectorsToInts(feature.extrapFeature())));
                }
                else {
                    System.out.println("" + j + ") " + feature);
                }

                j++;
            }

            world.assertBoundary(DEFAULT_TYPE);
            errorMessage = "Failed to get sorted features";

            existingModels = DEFAULT_GROUP.getOrderedFeatures();
            System.out.println("_.<>._.<>._.<>._.<>._.<>._.<>._.<>._.<>._.<>._");
            String featurestring = null;
            for (int i = 0; i < rawInput.length; i++){
                Vector input = intToTallyVector(rawInput[i], radiix);

                errorMessage = "Failed to extrapolate input " + rawInput[i];
                System.out.println("************************");
                System.out.println("Trying to predict: " + rawInput[i]);
                j = 0;

                for (FeatureModel feature: existingModels){
                    featurestring = "(" + feature.getMetaData() + ") " + feature.toString();
                    if (feature.isComplete()){
                        featurestring+= " -> " + tallyVectorToInteger(NNTools.roundToInt(feature.getPredictedOutput().rawFloat()));
                    }
                    System.out.println(featurestring);
                    j++;
                }

                world.processNextInput(input, DEFAULT_GROUP);
                existingModels = DEFAULT_GROUP.getOrderedFeatures();
            }

            System.out.println("overall preference: " + Arrays.toString(DEFAULT_GROUP.getPreference()));

        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testLearningLongPeriodicSequencesUntilExhaustion(){
        String errorMessage = "Failed to create Initial input sequence";
        try
        {

            System.out.println(String.format("%1$.3f, %1$.7f  %2$02d, %3$02d", 2.3455, 1, 22));
            int maxAllocation = 20;
            int initialGroupWeight = 1;
            int featureBufferSize = 4;
            int minimumBufferOverlap = 3;

            long baseSeed = 1528643029020L;

            boolean useSeed = true;
            int size = 25;
            long seed = System.currentTimeMillis();
            if (useSeed)
                seed = baseSeed;

            int radiix = 10;
            int numCycles = 18;

            boolean useDefaultPattern = false;
            int[] rawInput = null;
            if (useDefaultPattern)
            {
                rawInput = new int[]{5, 5, 5, 4, 4, 3, 3, 2, 2, 1, 2, 2, 3, 6, 10, 10, 5, 3};
                System.out.println("Using default input: " + Arrays.toString(rawInput));
            }
            else
            {
                rawInput = getRandomTestInput(size, radiix, seed);
                System.out.println("trying extrapolation with seed [" + seed + "] and radiix " + radiix + " resulting in: " + Arrays.toString(rawInput));
            }

            errorMessage = "Failed to create World Model";
            // using default LearningConfiguration

            // Step (1) - Create WorldModel

            LearningConfiguration base = WorldModel.getFeatureLearningConfiguration();
            base.setInputValidator((float[] input) -> (tallyVectorToInteger(input) != null));

            WorldModel world = new WorldModel(maxAllocation, base);

            errorMessage = "Failed to create new group type";

            // Step (2) - Create the Group type if not using the predefined BASIC (world.BASIC)
            WorldModel.GroupType DEFAULT_TYPE = world.createGroupType("DEFAULT", radiix, 30, initialGroupWeight, featureBufferSize, minimumBufferOverlap, base);

            Group DEFAULT_GROUP = world.addGroup("DEFAULT", DEFAULT_TYPE);

            DEFAULT_GROUP.setMemoryManagement(false);
            DEFAULT_GROUP.setMode(Group.MODE.MIXED);

            int j = 0;

            int testIndex = 0;
            outer: for (int repCount = 0; repCount < numCycles; repCount++){
                System.out.println("<o><o><o><o><o> Starting Step (" + (1 + repCount) + ") <o><o><o><o><o><o><o>");

                for (int i = 0; i < rawInput.length; i++){
                    testIndex++;
                    Vector input = intToTallyVector(rawInput[i], radiix);

                    errorMessage = "Failed to process input " + rawInput[i];
                    System.out.println("Test (" + testIndex + "): Trying to learn: " + rawInput[i]);

                    // Step (3) process all group types that you want
                    ArrayList<WorldModel.GroupSpecification> results = world.processNextInput(input, DEFAULT_TYPE);

                    errorMessage = "Failed to obtain correct result size.  Expected 1 but found " + results.size();
                    Assert.assertTrue(errorMessage, results.size() == 1);
                    WorldModel.GroupSpecification result = results.get(0);
                    j = 0;
                    errorMessage = "Failed to get all features";
                    ArrayList<FeatureModel> existingModels = result.getGroup().getAllFeatures();

                    for (FeatureModel feature: existingModels){

                        System.out.println("" + j + ") " + feature.toString());
                        j++;
                    }

                    if (result.getGroup().getMode() == Group.MODE.EXTRAPOLATION){
                        System.out.println("......................................");
                        System.out.println("Memory allocation exhausted, finished");
                        System.out.println("......................................");
                        break outer;
                    }
                }

            }

            errorMessage = "Failed to assert boundary";

            world.assertBoundary(DEFAULT_TYPE);
            DEFAULT_GROUP.setMode(Group.MODE.EXTRAPOLATION);

            errorMessage = "Failed to obtain all features";
            // Step (4) view all features

            System.out.println("(-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-)");
            System.out.println("Finished learning: " + Arrays.toString(rawInput));
            System.out.println("Feature learned data:");
            ArrayList<FeatureModel> existingModels = DEFAULT_GROUP.getAllFeatures();

            errorMessage = "Failed to allocate expected size.  Expected " + maxAllocation + " but found: " + existingModels.size();
            Assert.assertTrue(errorMessage, maxAllocation == existingModels.size());

            j = 0;
            for (FeatureModel feature: existingModels){

                if (feature.isComplete()){
                    System.out.println("" + j + ") " + Arrays.toString(vectorsToInts(feature.extrapFeature())));
                }
                else {
                    System.out.println("" + j + ") " + feature);
                }

                j++;
            }

            /*
            world.assertBoundary(DEFAULT_TYPE);
            errorMessage = "Failed to get sorted features";

            existingModels = DEFAULT_GROUP.getOrderedFeatures();
            System.out.println("_.<>._.<>._.<>._.<>._.<>._.<>._.<>._.<>._.<>._");
            String featurestring = null;
            for (int i = 0; i < rawInput.length; i++){
                Vector input = intToTallyVector(rawInput[i], radiix);

                errorMessage = "Failed to extrapolate input " + rawInput[i];
                System.out.println("************************");
                System.out.println("Trying to predict: " + rawInput[i]);
                j = 0;

                for (FeatureModel feature: existingModels){
                    featurestring = "(" + feature.getMetaData() + ") " + feature.toString();
                    if (feature.isComplete()){
                        featurestring+= " -> " + tallyVectorToInteger(NNTools.roundToInt(feature.getPredictedOutput().rawFloat()));
                    }
                    System.out.println(featurestring);
                    j++;
                }

                world.processNextInput(input, DEFAULT_GROUP);
                existingModels = DEFAULT_GROUP.getOrderedFeatures();
            }

            System.out.println("overall preference: " + Arrays.toString(DEFAULT_GROUP.getPreference()));
            */
        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testLearningLongPeriodicSequencesWithDreaming(){
        String errorMessage = "Failed to create Initial input sequence";
        try
        {
            FeatureModel.THREAD_COUNT = 2;
            System.out.println(String.format("%1$.3f, %1$.7f  %2$02d, %3$02d", 2.3455, 1, 22));
            int maxAllocation = 20;
            int initialGroupWeight = 1;
            int featureBufferSize = 4;
            int minimumBufferOverlap = 3;

            long baseSeed = 1528643029020L;

            boolean useSeed = true;
            int size = 25;
            long seed = System.currentTimeMillis();
            if (useSeed)
                seed = baseSeed;

            int radiix = 10;
            int numCycles = 30;

            boolean useDefaultPattern = false;
            int[] rawInput = null;
            if (useDefaultPattern)
            {
                rawInput = new int[]{5, 5, 5, 4, 4, 3, 3, 2, 2, 1, 2, 2, 3, 6, 10, 10, 5, 3};
                System.out.println("Using default input: " + Arrays.toString(rawInput));
            }
            else
            {
                rawInput = getRandomTestInput(size, radiix, seed);
                System.out.println("trying extrapolation with seed [" + seed + "] and radiix " + radiix + " resulting in: " + Arrays.toString(rawInput));
            }

            errorMessage = "Failed to create World Model";
            // using default LearningConfiguration

            // Step (1) - Create WorldModel

            LearningConfiguration base = WorldModel.getFeatureLearningConfiguration();
            base.setInputValidator((float[] input) -> (tallyVectorToInteger(input) != null));
            base.setInputToStringConverter((float[] input) -> ("" + tallyVectorToInteger(NNTools.roundToInt(input))));
            WorldModel world = new WorldModel(maxAllocation, base);

            errorMessage = "Failed to create new group type";

            // Step (2) - Create the Group type if not using the predefined BASIC (world.BASIC)
            WorldModel.GroupType DEFAULT_TYPE = world.createGroupType("DEFAULT", radiix, 30, initialGroupWeight, featureBufferSize, minimumBufferOverlap, base);

            Group DEFAULT_GROUP = world.addGroup("DEFAULT", DEFAULT_TYPE);

            DEFAULT_GROUP.setDebugEnabled(false);

            DEFAULT_GROUP.setMemoryManagement(true);
            DEFAULT_GROUP.setMode(Group.MODE.MIXED);

            int j = 0;

            int testIndex = 0;
            outer: for (int repCount = 0; repCount < numCycles; repCount++){
                System.out.println("<o><o><o><o><o> Starting Step (" + (1 + repCount) + ") <o><o><o><o><o><o><o>");

                for (int i = 0; i < rawInput.length; i++){
                    testIndex++;
                    Vector input = intToTallyVector(rawInput[i], radiix);

                    errorMessage = "Failed to process input " + rawInput[i];
                    System.out.println("Test (" + testIndex + "): Trying to learn: " + rawInput[i]);

                    // Step (3) process all group types that you want
                    ArrayList<WorldModel.GroupSpecification> results = world.processNextInput(input, DEFAULT_TYPE);

                    errorMessage = "Failed to obtain correct result size.  Expected 1 but found " + results.size();
                    Assert.assertTrue(errorMessage, results.size() == 1);
                    WorldModel.GroupSpecification result = results.get(0);
                    j = 0;
                    errorMessage = "Failed to get all features";
                    ArrayList<FeatureModel> existingModels = result.getGroup().getAllFeatures();

                    for (FeatureModel feature: existingModels){

                        System.out.println("" + j + ") " + feature.toString());
                        j++;
                    }

                    if (result.getGroup().getMode() == Group.MODE.EXTRAPOLATION){
                        System.out.println("......................................");
                        System.out.println("Memory allocation exhausted, finished");
                        System.out.println("......................................");
                        break outer;
                    }
                }

            }

            System.out.println("(-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-)");
            ArrayList<FeatureModel> existingModels = DEFAULT_GROUP.getAllFeatures();

            j = 0;
            for (FeatureModel feature: existingModels){
                Group.FeatureMetaData data = (Group.FeatureMetaData)feature.getMetaData();
                if (feature.isComplete()){
                    System.out.println("" + data.getAllocationIndex() + ") " + Arrays.toString(vectorsToInts(feature.extrapFeature())));
                }
                else {
                    System.out.println("" + data.getAllocationIndex() + ") " + feature);
                }

                j++;
            }


            errorMessage = "Failed to sleep";

            Group.DreamSpec dreamedResults = DEFAULT_GROUP.sleep();
            HashMap<Integer, Group.FeatureValueMetadata> prefs = DEFAULT_GROUP.getPreferenceMap();

            System.out.println("[oOo] [oOo] [oOo] [oOo] [oOo] [oOo] [oOo] [oOo] [oOo] [oOo] [oOo]");
            System.out.println("Final preferences");
            System.out.println("Data: " + Arrays.toString(rawInput));
            int i = 0;
            for (Integer index:DEFAULT_GROUP.getPreference()){
                FeatureModel model = DEFAULT_GROUP.getFeature(index);
                System.out.println("(" + i + ") *" + prefs.get(index) + "* " + model.toString() + " " + model.displayExtrapolatedFeature());
                i++;
            }

            System.out.println("Dream sequence: " + dreamedResults.getDreamedValue());
        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testLearningLongPeriodicSequencesWithDreamingAndClustering(){
        String errorMessage = "Failed to create Initial input sequence";
        try
        {

            System.out.println(String.format("%1$.3f, %1$.7f  %2$02d, %3$02d", 2.3455, 1, 22));
            int maxAllocation = 30;
            int initialGroupWeight = 1;
            int featureBufferSize = 4;
            int minimumBufferOverlap = 3;

            long baseSeed = 1528684442328L;

            boolean useSeed = true;
            int size = 25;
            long seed = System.currentTimeMillis();
            if (useSeed)
                seed = baseSeed;

            int radiix = 10;
            int numCycles = 70;

            int numClusters = 2;

            int[][] rawInput = new int[numClusters][size];

            for (int i = 0; i < numClusters;i++){
                rawInput[i] = getRandomTestInput(size, radiix, seed);
                seed++;
            }

            errorMessage = "Failed to create World Model";
            // using default LearningConfiguration

            // Step (1) - Create WorldModel

            LearningConfiguration base = WorldModel.getFeatureLearningConfiguration();
            base.setInputValidator((float[] input) -> (tallyVectorToInteger(input) != null));
            base.setInputToStringConverter((float[] input) -> ("" + tallyVectorToInteger(NNTools.roundToInt(input))));
            WorldModel world = new WorldModel(maxAllocation, base);

            errorMessage = "Failed to create new group type";

            // Step (2) - Create the Group type if not using the predefined BASIC (world.BASIC)
            WorldModel.GroupType DEFAULT_TYPE = world.createGroupType("DEFAULT", radiix, 30, initialGroupWeight, featureBufferSize, minimumBufferOverlap, base);

            Group DEFAULT_GROUP = world.addGroup("DEFAULT", DEFAULT_TYPE);

            DEFAULT_GROUP.setDebugEnabled(false);

            DEFAULT_GROUP.setMode(Group.MODE.MIXED);

            int j = 0;

            int testIndex = 0;

            outer: for (j = 0;j<numCycles;j++){
                if (j > 50){
                    DEFAULT_GROUP.setMemoryManagement(false);
                }
                for (int clusterIndex = numClusters-1; clusterIndex >=0; clusterIndex--){
                    System.out.println("<o><o><o><o><o><o><o><o><o><o><o><o>");
                    System.out.println("Learning cluster (" + (1 + clusterIndex) + "): " + Arrays.toString(rawInput[clusterIndex]));

                    for (int k = 0;k < 2;k++){
                        for (int i = 0; i < rawInput[clusterIndex].length; i++){
                            testIndex++;
                            Vector input = intToTallyVector(rawInput[clusterIndex][i], radiix);

                            errorMessage = "Failed to process input " + rawInput[clusterIndex][i];
                            System.out.println("Test (" + testIndex + "): Trying to learn: " + rawInput[clusterIndex][i]);

                            // Step (3) process all group types that you want
                            ArrayList<WorldModel.GroupSpecification> results = world.processNextInput(input, DEFAULT_TYPE);

                            errorMessage = "Failed to obtain correct result size.  Expected 1 but found " + results.size();
                            Assert.assertTrue(errorMessage, results.size() == 1);
                            WorldModel.GroupSpecification result = results.get(0);
                            int jj = 0;
                            errorMessage = "Failed to get all features";
                            ArrayList<FeatureModel> existingModels = result.getGroup().getAllFeatures();

                            for (FeatureModel feature: existingModels){

                                System.out.println("" + jj + ") " + feature.toString());
                                jj++;
                            }

                            if (result.getGroup().getMode() == Group.MODE.EXTRAPOLATION){
                                System.out.println("......................................");
                                System.out.println("Memory allocation exhausted, finished");
                                System.out.println("......................................");
                                break outer;
                            }
                        }
                    }

                    DEFAULT_GROUP.resetAll();
                }
            }


            System.out.println("(-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-)");
            ArrayList<FeatureModel> existingModels = DEFAULT_GROUP.getAllFeatures();

            j = 0;
            for (FeatureModel feature: existingModels){
                Group.FeatureMetaData data = (Group.FeatureMetaData)feature.getMetaData();
                if (feature.isComplete()){
                    System.out.println("" + data.getAllocationIndex() + ") " + Arrays.toString(vectorsToInts(feature.extrapFeature())));
                }
                else {
                    System.out.println("" + data.getAllocationIndex() + ") " + feature);
                }

                j++;
            }


            errorMessage = "Failed to sleep";

            Group.DreamSpec dreamedResults = DEFAULT_GROUP.sleep();
            final HashMap<Integer, Group.FeatureValueMetadata> prefs = dreamedResults.getDreamPreferences();

            System.out.println("[oOo] [oOo] [oOo] [oOo] [oOo] [oOo] [oOo] [oOo] [oOo] [oOo] [oOo]");
            System.out.println("Final preferences");
            for (int cIndex = 0;cIndex < numClusters;cIndex++){
                System.out.println("Seed: " + seed +  " O-~~ O-~~ O-~~ O-~~ O-~~ O-~~ O-~~ O-~~ O-~~");
                System.out.println("Input: " + (cIndex + 1) + Arrays.toString(rawInput[cIndex]));
            }

            boolean sortByUsageP = false;

            final HashMap<Integer, Integer> dreams = dreamedResults.getSelectionCountMap();
            double totalCount = 0;
            for (Map.Entry<Integer, Integer> e:dreams.entrySet()){
                totalCount+=e.getValue();
            }

            Integer[] keys = dreams.keySet().toArray(new Integer[0]);

            Comparator<Integer> usageComparator = new Comparator<Integer>() {
                @Override
                public int compare(Integer left, Integer right)
                {
                    return Double.compare(dreams.get(right), dreams.get(left));
                }
            };

            Comparator<Integer> prefComparator = new Comparator<Integer>() {
                @Override
                public int compare(Integer left, Integer right)
                {
                    return Double.compare(prefs.get(right).getPreferenceFraction(), prefs.get(left).getPreferenceFraction());
                }
            };

            if (sortByUsageP)
                Arrays.sort(keys, usageComparator);
            else
                Arrays.sort(keys, prefComparator);

            int i = 0;
            for (Integer index:keys){
                FeatureModel model = DEFAULT_GROUP.getFeature(index);
                Group.FeatureMetaData meta = (Group.FeatureMetaData)model.getMetaData();
                int dreamCount = dreams.get(Integer.valueOf(meta.getAllocationIndex()));
                String format = "< %1$d : %2$d%%> (" + index + ") *" + prefs.get(index) + "* " + model.toString() + " " + model.displayExtrapolatedFeature();
                System.out.println(String.format(format, dreamCount, (int)(dreamCount/totalCount*100)));
                i++;
            }


            System.out.println("Dreams: " + dreams);
            System.out.println("Dream sequence: " + dreamedResults.getDreamedValue());
        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testLearningUnlimitedSequencesWithDreamingAndClustering(){
        String errorMessage = "Failed to create Initial input sequence";
        try
        {
            System.out.println(String.format("%1$.3f, %1$.7f  %2$02d, %3$02d", 2.3455, 1, 22));
            int maxAllocation = 30;
            int initialGroupWeight = 1;
            int featureBufferSize = 4;
            int minimumBufferOverlap = 3;

            long baseSeed = 1528684442328L;

            boolean useSeed = false;
            int size = 25;
            long seed = System.currentTimeMillis();
            if (useSeed)
                seed = baseSeed;

            int radiix = 10;
            int numCycles = 20;

            int numClusters = 2;

            int[][] rawInput = new int[numClusters][size];

            for (int i = 0; i < numClusters;i++){
                rawInput[i] = getRandomTestInput(size, radiix, seed);
                seed++;
            }

            errorMessage = "Failed to create World Model";
            // using default LearningConfiguration

            // Step (1) - Create WorldModel

            LearningConfiguration base = WorldModel.getFeatureLearningConfiguration();
            base.setInputValidator((float[] input) -> (tallyVectorToInteger(input) != null));
            base.setInputToStringConverter((float[] input) -> ("" + tallyVectorToInteger(NNTools.roundToInt(input))));
            WorldModel world = new WorldModel(maxAllocation, base);

            errorMessage = "Failed to create new group type";

            // Step (2) - Create the Group type if not using the predefined BASIC (world.BASIC)
            WorldModel.GroupType DEFAULT_TYPE = world.createGroupType("DEFAULT", radiix, 30, initialGroupWeight, featureBufferSize, minimumBufferOverlap, base);

            Group DEFAULT_GROUP = world.addGroup("DEFAULT", DEFAULT_TYPE);

            DEFAULT_GROUP.setSleepToFreeMemory(true);
            DEFAULT_GROUP.setDebugEnabled(false);

            DEFAULT_GROUP.setMinimumRecycleUsageCount(0);
            DEFAULT_GROUP.setMode(Group.MODE.MIXED);

            ArrayList<String> recycledNames = new ArrayList<String>();
            Group.MemoryManagementListener managementListener = new Group.MemoryManagementListener() {
                @Override
                public void onStartMemoryManagement(int totalAllocation)
                {
                    System.out.println(".i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.");
                    System.out.println("Staring memory management: total allocation: " + totalAllocation);
                }

                @Override
                public void onFinishedMemoryManagement(ArrayList<Triple<FeatureModel, String, ArrayList<Vector>>> recycled)
                {
                    System.out.println("~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~");
                    System.out.println("Finished memory managements");
                    System.out.println("Recycled: " + recycled);
                    recycled.stream().forEach((Triple<FeatureModel,String, ArrayList<Vector>> pair)->{
                        recycledNames.add(pair.getLeft().toString());
                    });
                }
            };

            DEFAULT_GROUP.setMemoryListener(managementListener);


            int j = 0;

            int testIndex = 0;

            int loopCount = 2;
            outer: for (int kk = 0;kk<numCycles;kk++){
                if (kk > 15){
                    DEFAULT_GROUP.setMemoryManagement(false);
                }
                for (int clusterIndex = 0; clusterIndex < numClusters; clusterIndex++){
                    System.out.println("<o><o><o><o><o><o><o><o><o><o><o><o>");
                    System.out.println("Learning cluster (" + (1 + clusterIndex) + "): " + Arrays.toString(rawInput[clusterIndex]));

                    for (int k = 0;k < loopCount;k++){
                        for (int i = 0; i < rawInput[clusterIndex].length; i++){
                            testIndex++;
                            Vector input = intToTallyVector(rawInput[clusterIndex][i], radiix);

                            errorMessage = "Failed to process input " + rawInput[clusterIndex][i];
                            System.out.println("Test (" + testIndex + "): Trying to learn: " + rawInput[clusterIndex][i]);

                            // Step (3) process all group types that you want
                            ArrayList<WorldModel.GroupSpecification> results = world.processNextInput(input, DEFAULT_TYPE);

                            errorMessage = "Failed to obtain correct result size.  Expected 1 but found " + results.size();
                            Assert.assertTrue(errorMessage, results.size() == 1);
                            WorldModel.GroupSpecification result = results.get(0);
                            j = 0;
                            errorMessage = "Failed to get all features";
                            ArrayList<FeatureModel> existingModels = result.getGroup().getAllFeatures();

                            for (FeatureModel feature: existingModels){

                                System.out.println("" + j + ") " + feature.toString());
                                j++;
                            }

                            if (result.getGroup().getMode() == Group.MODE.EXTRAPOLATION){
                                System.out.println("......................................");
                                System.out.println("Memory allocation exhausted, finished");
                                System.out.println("......................................");
                                break outer;
                            }
                        }
                    }

                    DEFAULT_GROUP.resetAll();
                }
            }

            System.out.println("(-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-)");
            System.out.println("Seed: " + seed);
            ArrayList<FeatureModel> existingModels = DEFAULT_GROUP.getAllFeatures();

            j = 0;
            for (FeatureModel feature: existingModels){
                Group.FeatureMetaData data = (Group.FeatureMetaData)feature.getMetaData();
                if (feature.isComplete()){
                    System.out.println("" + data.getAllocationIndex() + ") " + Arrays.toString(vectorsToInts(feature.extrapFeature())));
                }
                else {
                    System.out.println("" + data.getAllocationIndex() + ") " + feature);
                }

                j++;
            }

            Group.DreamSpec dream = DEFAULT_GROUP.sleep();
            System.out.println(dream.getDreamedValue());
        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testConcurrentLearningUnlimitedSequencesWithDreamingAndClustering(){
        String errorMessage = "Failed to create Initial input sequence";
        try
        {
            FeatureModel.THREAD_COUNT = 4;
            long start = System.currentTimeMillis();
            System.out.println(String.format("%1$.3f, %1$.7f  %2$02d, %3$02d", 2.3455, 1, 22));
            int maxAllocation = 30;
            int initialGroupWeight = 1;
            int featureBufferSize = 4;
            int minimumBufferOverlap = 3;

            long baseSeed = 1528684442328L;

            boolean useSeed = true;
            int size = 25;
            long seed = System.currentTimeMillis();
            if (useSeed)
                seed = baseSeed;

            int radiix = 10;
            int numCycles = 20;

            int numClusters = 2;

            int[][] rawInput = new int[numClusters][size];

            for (int i = 0; i < numClusters;i++){
                rawInput[i] = getRandomTestInput(size, radiix, seed);
                seed++;
            }

            errorMessage = "Failed to create World Model";
            // using default LearningConfiguration

            // Step (1) - Create WorldModel

            LearningConfiguration base = WorldModel.getFeatureLearningConfiguration();
            base.setInputValidator((float[] input) -> (tallyVectorToInteger(input) != null));
            base.setInputToStringConverter((float[] input) -> ("" + tallyVectorToInteger(NNTools.roundToInt(input))));
            WorldModel world = new WorldModel(maxAllocation, base);

            errorMessage = "Failed to create new group type";

            // Step (2) - Create the Group type if not using the predefined BASIC (world.BASIC)
            WorldModel.GroupType DEFAULT_TYPE = world.createGroupType("DEFAULT", radiix, 30, initialGroupWeight, featureBufferSize, minimumBufferOverlap, base);

            Group DEFAULT_GROUP = world.addGroup("DEFAULT", DEFAULT_TYPE);

            DEFAULT_GROUP.setSleepToFreeMemory(true);
            DEFAULT_GROUP.setDebugEnabled(false);

            DEFAULT_GROUP.setMinimumRecycleUsageCount(0);
            DEFAULT_GROUP.setMode(Group.MODE.MIXED);

            ArrayList<String> recycledNames = new ArrayList<String>();
            Group.MemoryManagementListener managementListener = new Group.MemoryManagementListener() {
                @Override
                public void onStartMemoryManagement(int totalAllocation)
                {
                    System.out.println(".i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.");
                    System.out.println("Staring memory management: total allocation: " + totalAllocation);
                }

                @Override
                public void onFinishedMemoryManagement(ArrayList<Triple<FeatureModel,String, ArrayList<Vector>>> recycled)
                {
                    System.out.println("~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~");
                    System.out.println("Finished memory managements");
                    System.out.println("Recycled: " + recycled);
                    recycled.stream().forEach((Triple<FeatureModel,String, ArrayList<Vector>> pair)->{
                        recycledNames.add(pair.getLeft().toString());
                    });
                }
            };

            DEFAULT_GROUP.setMemoryListener(managementListener);


            int j = 0;

            int testIndex = 0;

            int loopCount = 2;
            outer: for (int kk = 0;kk<numCycles;kk++){
                if (kk > 15){
                    DEFAULT_GROUP.setMemoryManagement(false);
                }
                for (int clusterIndex = 0; clusterIndex < numClusters; clusterIndex++){
                    System.out.println("<o><o><o><o><o><o><o><o><o><o><o><o>");
                    System.out.println("Learning cluster (" + (1 + clusterIndex) + "): " + Arrays.toString(rawInput[clusterIndex]));

                    for (int k = 0;k < loopCount;k++){
                        for (int i = 0; i < rawInput[clusterIndex].length; i++){
                            testIndex++;
                            Vector input = intToTallyVector(rawInput[clusterIndex][i], radiix);

                            errorMessage = "Failed to process input " + rawInput[clusterIndex][i];
                            System.out.println("Test (" + testIndex + "): Trying to learn: " + rawInput[clusterIndex][i]);

                            // Step (3) process all group types that you want
                            ArrayList<WorldModel.GroupSpecification> results = world.processNextInput(input, DEFAULT_TYPE);

                            errorMessage = "Failed to obtain correct result size.  Expected 1 but found " + results.size();
                            Assert.assertTrue(errorMessage, results.size() == 1);
                            WorldModel.GroupSpecification result = results.get(0);
                            j = 0;
                            errorMessage = "Failed to get all features";
                            ArrayList<FeatureModel> existingModels = result.getGroup().getAllFeatures();

                            for (FeatureModel feature: existingModels){

                                System.out.println("" + j + ") " + feature.toString());
                                j++;
                            }

                            if (result.getGroup().getMode() == Group.MODE.EXTRAPOLATION){
                                System.out.println("......................................");
                                System.out.println("Memory allocation exhausted, finished");
                                System.out.println("......................................");
                                break outer;
                            }
                        }
                    }

                    DEFAULT_GROUP.resetAll();
                }
            }

            System.out.println("(-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-)");
            System.out.println("Seed: " + seed);
            ArrayList<FeatureModel> existingModels = DEFAULT_GROUP.getAllFeatures();

            j = 0;
            for (FeatureModel feature: existingModels){
                Group.FeatureMetaData data = (Group.FeatureMetaData)feature.getMetaData();
                if (feature.isComplete()){
                    System.out.println("" + data.getAllocationIndex() + ") " + feature.toString() + ": " + Arrays.toString(vectorsToInts(feature.extrapFeature())));
                }
                else {
                    System.out.println("" + data.getAllocationIndex() + ") " + feature);
                }

                j++;
            }

            Group.DreamSpec dream = DEFAULT_GROUP.sleep();
            System.out.println(dream.getDreamedValue());

            long finished = (System.currentTimeMillis() - start)/1000;
            System.out.println("Finished after " + finished + " seconds using " + FeatureModel.THREAD_COUNT + " threads");
        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testSerializationOfGroups(){
        String errorMessage = "Failed to create Initial input sequence";
        try
        {
            FeatureModel.THREAD_COUNT = 2;
            long start = System.currentTimeMillis();
            System.out.println(String.format("%1$.3f, %1$.7f  %2$02d, %3$02d", 2.3455, 1, 22));
            int maxAllocation = 30;
            int initialGroupWeight = 1;
            int featureBufferSize = 4;
            int minimumBufferOverlap = 3;

            long baseSeed = 1528684442328L;

            boolean useSeed = false;
            int size = 25;
            long seed = System.currentTimeMillis();
            if (useSeed)
                seed = baseSeed;

            int radiix = 10;
            int numCycles = 20;

            int numClusters = 2;

            int[][] rawInput = new int[numClusters][size];

            for (int i = 0; i < numClusters;i++){
                rawInput[i] = getRandomTestInput(size, radiix, seed);
                seed++;
            }

            errorMessage = "Failed to create World Model";
            // using default LearningConfiguration

            // Step (1) - Create WorldModel

            LearningConfiguration base = WorldModel.getFeatureLearningConfiguration();
            base.setInputValidator((float[] input) -> (tallyVectorToInteger(input) != null));
            base.setInputToStringConverter((float[] input) -> ("" + tallyVectorToInteger(NNTools.roundToInt(input))));
            WorldModel world = new WorldModel(maxAllocation, base);

            errorMessage = "Failed to create new group type";

            // Step (2) - Create the Group type if not using the predefined BASIC (world.BASIC)
            WorldModel.GroupType DEFAULT_TYPE = world.createGroupType("DEFAULT", radiix, 30, initialGroupWeight, featureBufferSize, minimumBufferOverlap, base);

            Group DEFAULT_GROUP = world.addGroup("DEFAULT", DEFAULT_TYPE);

            DEFAULT_GROUP.setSleepToFreeMemory(true);
            DEFAULT_GROUP.setDebugEnabled(false);

            DEFAULT_GROUP.setMinimumRecycleUsageCount(0);
            DEFAULT_GROUP.setMode(Group.MODE.MIXED);

            ArrayList<String> recycledNames = new ArrayList<String>();
            Group.MemoryManagementListener managementListener = new Group.MemoryManagementListener() {
                @Override
                public void onStartMemoryManagement(int totalAllocation)
                {
                    System.out.println(".i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.");
                    System.out.println("Staring memory management: total allocation: " + totalAllocation);
                }

                @Override
                public void onFinishedMemoryManagement(ArrayList<Triple<FeatureModel,String, ArrayList<Vector>>> recycled)
                {
                    System.out.println("~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~");
                    System.out.println("Finished memory managements");
                    System.out.println("Recycled: " + recycled);
                    recycled.stream().forEach((Triple<FeatureModel,String, ArrayList<Vector>> pair)->{
                        recycledNames.add(pair.getLeft().toString());
                    });
                }
            };

            DEFAULT_GROUP.setMemoryListener(managementListener);


            int j = 0;

            int testIndex = 0;

            int loopCount = 2;
            outer: for (int kk = 0;kk<numCycles;kk++){
                if (kk > 15){
                    DEFAULT_GROUP.setMemoryManagement(false);
                }
                for (int clusterIndex = 0; clusterIndex < numClusters; clusterIndex++){
                    System.out.println("<o><o><o><o><o><o><o><o><o><o><o><o>");
                    System.out.println("Learning cluster (" + (1 + clusterIndex) + "): " + Arrays.toString(rawInput[clusterIndex]));

                    for (int k = 0;k < loopCount;k++){
                        for (int i = 0; i < rawInput[clusterIndex].length; i++){
                            testIndex++;
                            Vector input = intToTallyVector(rawInput[clusterIndex][i], radiix);

                            errorMessage = "Failed to process input " + rawInput[clusterIndex][i];
                            System.out.println("Test (" + testIndex + "): Trying to learn: " + rawInput[clusterIndex][i]);

                            // Step (3) process all group types that you want
                            ArrayList<WorldModel.GroupSpecification> results = world.processNextInput(input, DEFAULT_TYPE);

                            errorMessage = "Failed to obtain correct result size.  Expected 1 but found " + results.size();
                            Assert.assertTrue(errorMessage, results.size() == 1);
                            WorldModel.GroupSpecification result = results.get(0);
                            j = 0;
                            errorMessage = "Failed to get all features";
                            ArrayList<FeatureModel> existingModels = result.getGroup().getAllFeatures();

                            for (FeatureModel feature: existingModels){

                                System.out.println("" + j + ") " + feature.toString());
                                j++;
                            }

                            if (result.getGroup().getMode() == Group.MODE.EXTRAPOLATION){
                                System.out.println("......................................");
                                System.out.println("Memory allocation exhausted, finished");
                                System.out.println("......................................");
                                break outer;
                            }
                        }
                    }

                    DEFAULT_GROUP.resetAll();
                }
            }

            System.out.println("(-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-)");
            System.out.println("Seed: " + seed);
            ArrayList<FeatureModel> existingModels = DEFAULT_GROUP.getAllFeatures();

            j = 0;
            for (FeatureModel feature: existingModels){
                Group.FeatureMetaData data = (Group.FeatureMetaData)feature.getMetaData();
                if (feature.isComplete()){
                    System.out.println("" + data.getAllocationIndex() + ") " + Arrays.toString(vectorsToInts(feature.extrapFeature())));
                }
                else {
                    System.out.println("" + data.getAllocationIndex() + ") " + feature);
                }

                j++;
            }

            Group.DreamSpec dream = DEFAULT_GROUP.sleep();
            System.out.println(dream.getDreamedValue());

            long finished = (System.currentTimeMillis() - start)/1000;
            System.out.println("Finished base Group after " + finished + " seconds using " + FeatureModel.THREAD_COUNT + " threads");


            errorMessage = "Failed to serialize group";
            byte[] serializedGroupData = DEFAULT_GROUP.serializeBytes();

            errorMessage = "failed to deserialize group";
            Group deserializedGroup = Group.deserializeBytes(serializedGroupData, DEFAULT_GROUP.getType());
            System.out.println("Showing deserialized group state");

            deserializedGroup.getLearningConfig().setInputToStringConverter((float[] input) -> ("" + tallyVectorToInteger(NNTools.roundToInt(input))));

            ArrayList<FeatureModel> deserializedModels = deserializedGroup.getAllFeatures();

            errorMessage = "Failed to get deserialized models";

            Assert.assertTrue(errorMessage, deserializedModels!=null && deserializedModels.size() == existingModels.size());

            j = 0;
            for (FeatureModel feature: deserializedModels){
                errorMessage = "failed to get model meta-data: " + feature;
                Group.FeatureMetaData data = (Group.FeatureMetaData)feature.getMetaData();
                if (feature.isComplete()){
                    System.out.println("" + data.getAllocationIndex() + ") " + Arrays.toString(vectorsToInts(feature.extrapFeature())));
                }
                else {
                    System.out.println("" + data.getAllocationIndex() + ") " + feature);
                }

                j++;
            }

            System.out.println("Successfully deserialized group");
        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testSerializationOfWorld(){
        String errorMessage = "Failed to create Initial input sequence";
        try
        {
            FeatureModel.THREAD_COUNT = 2;
            long start = System.currentTimeMillis();
            System.out.println(String.format("%1$.3f, %1$.7f  %2$02d, %3$02d", 2.3455, 1, 22));
            int maxAllocation = 30;
            int initialGroupWeight = 1;
            int featureBufferSize = 4;
            int minimumBufferOverlap = 3;

            long baseSeed = 1528684442328L;

            boolean useSeed = false;
            int size = 25;
            long seed = System.currentTimeMillis();
            if (useSeed)
                seed = baseSeed;

            int radiix = 10;
            int numCycles = 20;

            int numClusters = 2;

            int[][] rawInput = new int[numClusters][size];

            for (int i = 0; i < numClusters;i++){
                rawInput[i] = getRandomTestInput(size, radiix, seed);
                seed++;
            }

            errorMessage = "Failed to create World Model";
            // using default LearningConfiguration

            // Step (1) - Create WorldModel

            LearningConfiguration base = WorldModel.getFeatureLearningConfiguration();
            base.setInputValidator((float[] input) -> (tallyVectorToInteger(input) != null));
            base.setInputToStringConverter((float[] input) -> ("" + tallyVectorToInteger(NNTools.roundToInt(input))));
            WorldModel world = new WorldModel(maxAllocation, base);

            errorMessage = "Failed to create new group type";

            // Step (2) - Create the Group type if not using the predefined BASIC (world.BASIC)
            WorldModel.GroupType DEFAULT_TYPE = world.createGroupType("DEFAULT", radiix, 30, initialGroupWeight, featureBufferSize, minimumBufferOverlap, base);

            Group DEFAULT_GROUP = world.addGroup("DEFAULT", DEFAULT_TYPE);

            DEFAULT_GROUP.setSleepToFreeMemory(true);
            DEFAULT_GROUP.setDebugEnabled(false);

            DEFAULT_GROUP.setMinimumRecycleUsageCount(0);
            DEFAULT_GROUP.setMode(Group.MODE.MIXED);

            ArrayList<String> recycledNames = new ArrayList<String>();
            Group.MemoryManagementListener managementListener = new Group.MemoryManagementListener() {
                @Override
                public void onStartMemoryManagement(int totalAllocation)
                {
                    System.out.println(".i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.");
                    System.out.println("Staring memory management: total allocation: " + totalAllocation);
                }

                @Override
                public void onFinishedMemoryManagement(ArrayList<Triple<FeatureModel,String, ArrayList<Vector>>> recycled)
                {
                    System.out.println("~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~");
                    System.out.println("Finished memory managements");
                    System.out.println("Recycled: " + recycled);
                    recycled.stream().forEach((Triple<FeatureModel,String, ArrayList<Vector>> pair)->{
                        recycledNames.add(pair.getLeft().toString());
                    });
                }
            };

            DEFAULT_GROUP.setMemoryListener(managementListener);

            int j = 0;

            int testIndex = 0;

            int loopCount = 2;
            outer: for (int kk = 0;kk<numCycles;kk++){
                if (kk > 15){
                    DEFAULT_GROUP.setMemoryManagement(false);
                }
                for (int clusterIndex = 0; clusterIndex < numClusters; clusterIndex++){
                    System.out.println("<o><o><o><o><o><o><o><o><o><o><o><o>");
                    System.out.println("Learning cluster (" + (1 + clusterIndex) + "): " + Arrays.toString(rawInput[clusterIndex]));

                    for (int k = 0;k < loopCount;k++){
                        for (int i = 0; i < rawInput[clusterIndex].length; i++){
                            testIndex++;
                            Vector input = intToTallyVector(rawInput[clusterIndex][i], radiix);

                            errorMessage = "Failed to process input " + rawInput[clusterIndex][i];
                            System.out.println("Test (" + testIndex + "): Trying to learn: " + rawInput[clusterIndex][i]);

                            // Step (3) process all group types that you want
                            ArrayList<WorldModel.GroupSpecification> results = world.processNextInput(input, DEFAULT_TYPE);

                            errorMessage = "Failed to obtain correct result size.  Expected 1 but found " + results.size();
                            Assert.assertTrue(errorMessage, results.size() == 1);
                            WorldModel.GroupSpecification result = results.get(0);
                            j = 0;
                            errorMessage = "Failed to get all features";
                            ArrayList<FeatureModel> existingModels = result.getGroup().getAllFeatures();

                            for (FeatureModel feature: existingModels){

                                System.out.println("" + j + ") " + feature.toString());
                                j++;
                            }

                            if (result.getGroup().getMode() == Group.MODE.EXTRAPOLATION){
                                System.out.println("......................................");
                                System.out.println("Memory allocation exhausted, finished");
                                System.out.println("......................................");
                                break outer;
                            }
                        }
                    }

                    DEFAULT_GROUP.resetAll();
                }
            }

            System.out.println("(-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-)");
            System.out.println("Seed: " + seed);
            ArrayList<FeatureModel> existingModels = DEFAULT_GROUP.getAllFeatures();

            j = 0;
            for (FeatureModel feature: existingModels){
                Group.FeatureMetaData data = (Group.FeatureMetaData)feature.getMetaData();
                if (feature.isComplete()){
                    System.out.println("" + data.getAllocationIndex() + ") " + Arrays.toString(vectorsToInts(feature.extrapFeature())));
                }
                else {
                    System.out.println("" + data.getAllocationIndex() + ") " + feature);
                }

                j++;
            }

            Group.DreamSpec dream = DEFAULT_GROUP.sleep();
            System.out.println(dream.getDreamedValue());

            long finished = (System.currentTimeMillis() - start)/1000;
            System.out.println("Finished base Group after " + finished + " seconds using " + FeatureModel.THREAD_COUNT + " threads");


            errorMessage = "Failed to serialize group data";
            byte[] serializedGroupData = world.serializeGroupBytes();


            // Standard way of
            WorldModel worldCopy = new WorldModel(maxAllocation);
            WorldModel.GroupType newType = worldCopy.createGroupType("DEFAULT", radiix, 30, initialGroupWeight, featureBufferSize, minimumBufferOverlap);
            newType.setInputValidator((float[] input) -> (tallyVectorToInteger(input) != null));
            newType.getLearningConfig().setInputToStringConverter((float[] input) -> ("" + tallyVectorToInteger(NNTools.roundToInt(input))));

            errorMessage = "Failed to load group data";

            worldCopy.deserializeGroups(serializedGroupData);

            errorMessage = "Failed to retrieve reconstructed groups";
            ArrayList<WorldModel.GroupSpecification> groups = worldCopy.getGroups(newType);

            Assert.assertTrue(errorMessage, groups != null && groups.size() == 1);

            Group deserializedGroup = groups.get(0).getGroup();

            ArrayList<FeatureModel> deserializedModels = deserializedGroup.getAllFeatures();

            errorMessage = "Failed to get deserialized models";

            Assert.assertTrue(errorMessage, deserializedModels!=null && deserializedModels.size() == existingModels.size());

            j = 0;
            for (FeatureModel feature: deserializedModels){
                errorMessage = "failed to get model meta-data: " + feature;
                Group.FeatureMetaData data = (Group.FeatureMetaData)feature.getMetaData();
                if (feature.isComplete()){
                    System.out.println("" + data.getAllocationIndex() + ") " + Arrays.toString(vectorsToInts(feature.extrapFeature())));
                }
                else {
                    System.out.println("" + data.getAllocationIndex() + ") " + feature);
                }

                j++;
            }

            System.out.println("Successfully deserialized group");
        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testConfabulation(){
        String errorMessage = "Failed to create Initial input sequence";
        try
        {
            FeatureModel.THREAD_COUNT = 4;
            long start = System.currentTimeMillis();
            System.out.println(String.format("%1$.3f, %1$.7f  %2$02d, %3$02d", 2.3455, 1, 22));
            int maxAllocation = 30;
            int initialGroupWeight = 1;
            int featureBufferSize = 4;
            int minimumBufferOverlap = 3;

            long baseSeed = 1528684442328L;

            boolean useSeed = false;
            int size = 25;
            long seed = System.currentTimeMillis();
            if (useSeed)
                seed = baseSeed;
            else
                seed = 0;

            int radiix = 10;
            int numCycles = 20;

            int numClusters = 2;

            int[][] rawInput = new int[numClusters][size];

            for (int i = 0; i < numClusters;i++){
                rawInput[i] = getRandomTestInput(size, radiix, seed);
                seed++;
            }

            errorMessage = "Failed to create World Model";
            // using default LearningConfiguration

            // Step (1) - Create WorldModel

            LearningConfiguration base = WorldModel.getFeatureLearningConfiguration();
            base.setInputValidator((float[] input) -> (tallyVectorToInteger(input) != null));
            base.setInputToStringConverter((float[] input) -> ("" + tallyVectorToInteger(NNTools.roundToInt(input))));
            WorldModel world = new WorldModel(maxAllocation, base);

            errorMessage = "Failed to create new group type";

            // Step (2) - Create the Group type if not using the predefined BASIC (world.BASIC)
            WorldModel.GroupType DEFAULT_TYPE = world.createGroupType("DEFAULT", radiix, 30, initialGroupWeight, featureBufferSize, minimumBufferOverlap, base);

            Group DEFAULT_GROUP = world.addGroup("DEFAULT", DEFAULT_TYPE);

            DEFAULT_GROUP.setSleepToFreeMemory(true);
            DEFAULT_GROUP.setDebugEnabled(false);

            DEFAULT_GROUP.setMinimumRecycleUsageCount(0);
            DEFAULT_GROUP.setMode(Group.MODE.MIXED);

            ArrayList<FeatureModel> recycledNames = new ArrayList<FeatureModel>();
            Group.MemoryManagementListener managementListener = new Group.MemoryManagementListener() {
                @Override
                public void onStartMemoryManagement(int totalAllocation)
                {
                    System.out.println(".i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.i!i.");
                    System.out.println("Staring memory management: total allocation: " + totalAllocation);
                }

                @Override
                public void onFinishedMemoryManagement(ArrayList<Triple<FeatureModel, String, ArrayList<Vector>>> recycled)
                {
                    System.out.println("~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~o).(o~");
                    System.out.println("Finished memory managements");
                    System.out.println("Recycled: " + recycled);
                    recycled.stream().forEach((Triple<FeatureModel, String, ArrayList<Vector>> pair)->{
                        recycledNames.add(pair.getLeft());
                    });
                }
            };

            DEFAULT_GROUP.setMemoryListener(managementListener);

            int j = 0;

            int testIndex = 0;

            int loopCount = 2;
            outer: for (int kk = 0;kk<numCycles;kk++){
                if (kk > 15){
                    DEFAULT_GROUP.setMemoryManagement(false);
                }
                for (int clusterIndex = 0; clusterIndex < numClusters; clusterIndex++){
                    System.out.println("<o><o><o><o><o><o><o><o><o><o><o><o>");
                    System.out.println("Learning cluster (" + (1 + clusterIndex) + "): " + Arrays.toString(rawInput[clusterIndex]));

                    for (int k = 0;k < loopCount;k++){
                        for (int i = 0; i < rawInput[clusterIndex].length; i++){
                            testIndex++;
                            Vector input = intToTallyVector(rawInput[clusterIndex][i], radiix);

                            errorMessage = "Failed to process input " + rawInput[clusterIndex][i];
                            System.out.println("Test (" + testIndex + "): Trying to learn: " + rawInput[clusterIndex][i]);

                            // Step (3) process all group types that you want
                            ArrayList<WorldModel.GroupSpecification> results = world.processNextInput(input, DEFAULT_TYPE);

                            errorMessage = "Failed to obtain correct result size.  Expected 1 but found " + results.size();
                            Assert.assertTrue(errorMessage, results.size() == 1);
                            WorldModel.GroupSpecification result = results.get(0);
                            j = 0;
                            errorMessage = "Failed to get all features";
                            ArrayList<FeatureModel> existingModels = result.getGroup().getAllFeatures();

                            for (FeatureModel feature: existingModels){

                                System.out.println("" + j + ") " + feature.toString());
                                j++;
                            }

                            if (result.getGroup().getMode() == Group.MODE.EXTRAPOLATION){
                                System.out.println("......................................");
                                System.out.println("Memory allocation exhausted, finished");
                                System.out.println("......................................");
                                break outer;
                            }
                        }
                    }

                    DEFAULT_GROUP.resetAll();
                }
            }

            System.out.println("(-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-) (-+-)");
            System.out.println("Seed: " + seed);
            ArrayList<FeatureModel> existingModels = DEFAULT_GROUP.getAllFeatures();

            j = 0;
            for (FeatureModel feature: existingModels){
                Group.FeatureMetaData data = (Group.FeatureMetaData)feature.getMetaData();
                if (feature.isComplete()){
                    System.out.println("" + data.getAllocationIndex() + ") " + Arrays.toString(vectorsToInts(feature.extrapFeature())));
                }
                else {
                    System.out.println("" + data.getAllocationIndex() + ") " + feature);
                }

                j++;
            }

            Group.DreamSpec dream = DEFAULT_GROUP.sleep();
            System.out.println(dream.getDreamedValue());

            long finished = (System.currentTimeMillis() - start)/1000;
            System.out.println("Finished after " + finished + " seconds using " + FeatureModel.THREAD_COUNT + " threads");

            for (int i = 0;i < numClusters;i++){
                System.out.println("Learned cluster (" + (1 + i) + "): " + Arrays.toString(rawInput[i]));
            }

            errorMessage = "Failed to remove redundant patterns";
            DEFAULT_GROUP.removeDuplicates(20);

            System.out.println("..........................................");
            System.out.println("After cleanup");
            existingModels = DEFAULT_GROUP.getAllFeatures();

            j = 0;
            for (FeatureModel feature: existingModels){
                Group.FeatureMetaData data = (Group.FeatureMetaData)feature.getMetaData();
                if (feature.isComplete()){
                    System.out.println("" + data.getAllocationIndex() + ") " + Arrays.toString(vectorsToInts(feature.extrapFeature())));
                }
                else {
                    System.out.println("" + data.getAllocationIndex() + ") " + feature);
                }

                j++;
            }

            errorMessage = "Failed to do chunked confabulation";

            ArrayList<Vector> confabulated = DEFAULT_GROUP.confabulate(true, 30, false);
            Assert.assertTrue(errorMessage, confabulated.size()>0);
            System.out.println("Confabulated chunked: " + Arrays.toString(vectorsToInts(confabulated)));


            confabulated = DEFAULT_GROUP.confabulate(false, 40, false);
            errorMessage = "Failed to do unchunked confabulation";
            Assert.assertTrue(errorMessage, confabulated.size()>0);

            System.out.println("Confabulated imaginmation: " + Arrays.toString(vectorsToInts(confabulated)));


        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testCustomFeatureModelFocus(){
        int[][] focusTest = {{1, 9}, {2,8}, {3, 7}, {4, 6}, {5, 5}, {6, 4}, {7, 3}, {8, 2}, {9,1}};


        String errorMessage = null;

        try
        {
            errorMessage = "Failed to create augmented vector list";
            int radiix = 10;
            Vector[] augmented = getAugmentedVectors(focusTest, radiix);

            int bDim = 2*radiix;
            int maxAllocation = 25;
            int initialGroupWeight = 1;
            int featureBufferSize = 4;
            int minimumBufferOverlap = 3;

            errorMessage = "Failed to create world model";

            LearningConfiguration base = WorldModel.getFeatureLearningConfiguration();
            base.setInputValidator(getAugmentedVectorInputValidator(radiix ,radiix));
            base.setInputToStringConverter(getInputToStringConverter(radiix, radiix));
            WorldModel world = new WorldModel(maxAllocation, base);

            errorMessage = "Failed to create new group";

            WorldModel.GroupType DEFAULT_TYPE = world.createGroupType("DEFAULT", bDim, 30, initialGroupWeight, featureBufferSize, minimumBufferOverlap, base);

            Group DEFAULT_GROUP = world.addGroup("DEFAULT", DEFAULT_TYPE);

            DEFAULT_GROUP.setDebugEnabled(true);

            DEFAULT_GROUP.setMinimumRecycleUsageCount(0);
            DEFAULT_GROUP.setMode(Group.MODE.LEARNING);

            // Learn the pattern focusing on the increasing
            float[] maskIncreasing = getMask(new int[]{0}, radiix, radiix);
            float[] maskDecreasing = getMask(new int[]{1}, radiix, radiix);

            Value[] lispIncreasingMask = new Value[maskIncreasing.length];
            Value[] lispDecreasingMask = new Value[maskDecreasing.length];

            for (int i = 0;i<lispDecreasingMask.length;i++){
                lispDecreasingMask[i] = NLispTools.makeValue(maskDecreasing[i]);
            }

            for (int i = 0;i<lispIncreasingMask.length;i++){
                lispIncreasingMask[i] = NLispTools.makeValue(maskIncreasing[i]);
            }

            Value increasingMaskValue = new ListValue(lispIncreasingMask);
            Value decreasingMaskValue = new ListValue(lispDecreasingMask);

            final String INCREASING_TYPE_KEY = "increasing";
            final String DECREASING_TYPE_KEY = "decreasing";
            HashMap<String, Value> increasingModelMetaMap = new HashMap<String, Value>(){
                {
                    put(Group.FOCUS_KEY, increasingMaskValue);
                    put("TYPE", NLispTools.makeValue(INCREASING_TYPE_KEY));

                }
            };

            HashMap<String, Value> decreasingModelMetaMap = new HashMap<String, Value>(){
                {
                    put(Group.FOCUS_KEY, decreasingMaskValue);
                    put("TYPE", NLispTools.makeValue(DECREASING_TYPE_KEY));
                }
            };

            StringHashtableValue increasingModelMeta = new StringHashtableValue(increasingModelMetaMap);
            StringHashtableValue decreasingModelMeta = new StringHashtableValue(decreasingModelMetaMap);


            // Train the group on the increasing mask

            errorMessage = "Errors processing input";

            FeatureModel focus = null, prevFocus = null;
            for (Vector input:augmented){
                DEFAULT_GROUP.processInput(input);
                focus = DEFAULT_GROUP.getFocusModel();
                if (focus != null){
                    DEFAULT_GROUP.setCustomMetadata(focus, increasingModelMeta);
                }
            }
            DEFAULT_GROUP.resetAll(false);

            for (Vector input:augmented){
                DEFAULT_GROUP.processInput(input);
                focus = DEFAULT_GROUP.getFocusModel();
                if (focus != null){
                    DEFAULT_GROUP.setCustomMetadata(focus, decreasingModelMeta);

                }
            }
            DEFAULT_GROUP.resetAll(false);

            // Now extrapolate using the corrupted inputs

            errorMessage = "Faild to generate random inputs";
            long seed = System.currentTimeMillis();

            System.out.println("Using seed: " + seed);
            Random ranGen = new Random();
            ranGen.setSeed(seed);

            int[][] noisyIncreasing = new int[focusTest.length][];

            int[][] noisyDecreasing = new int[focusTest.length][];


            for (int k = 0; k < focusTest.length;k++){
                int[] element = focusTest[k];
                int sample1 = (int)(ranGen.nextDouble()*radiix);
                int sample2 = (int)(ranGen.nextDouble()*radiix);

                noisyIncreasing[k] = new int[]{element[0], sample1};
                noisyDecreasing[k] = new int[]{sample2, element[1]};
            }

            System.out.println("Constructed noisy increasing input: " + displayIntGroup(noisyIncreasing));
            System.out.println("Constructed noisy decreasing input: " + displayIntGroup(noisyDecreasing));

            errorMessage = "Failed to create augmented inputs";

            Vector[] noisyIncVectorSeq = getAugmentedVectors(noisyIncreasing, radiix);

            Vector[] noisyDecVectorSeq = getAugmentedVectors(noisyDecreasing, radiix);

            System.out.println("Constructed noisy augmented increasing input: " + Arrays.toString(noisyIncVectorSeq));
            System.out.println("Constructed noisy augmented decreasing input: " + Arrays.toString(noisyDecVectorSeq));

            int increasingCount = 0, decreasingCount = 0;

            int totalSteps = focusTest.length;

            boolean resetFirstP = false;

            if (resetFirstP)
                DEFAULT_GROUP.resetAll(false);

            System.out.println("Extrapolating from increasing noisy sequence: " + displayIntGroup(noisyIncreasing));
            DEFAULT_GROUP.setMode(Group.MODE.EXTRAPOLATION);
            {
                errorMessage = "Failed to tally increasing/decreasing recognition counts";
                for (int k = 0; k<totalSteps;k++){
                    DEFAULT_GROUP.processInput(noisyIncVectorSeq[k]);
                    FeatureModel model = DEFAULT_GROUP.getFocusModel();
                    if (model != null) {
                        StringHashtableValue metaMap = (StringHashtableValue)DEFAULT_GROUP.getCustomMetadata(model);
                        if (metaMap != null){
                            String type = metaMap.getStringHashtable().get("TYPE").getString();
                            if (INCREASING_TYPE_KEY.equals(type)){
                                increasingCount++;
                            }

                            if (DECREASING_TYPE_KEY.equals(type)){
                                decreasingCount++;
                            }
                        }

                    }
                }

                Assert.assertTrue(errorMessage, increasingCount > decreasingCount);

                System.out.println(String.format("For increasing sequence, increasing/decreasing (%1$d, %2$d)", increasingCount, decreasingCount));
            }

            increasingCount = 0;
            decreasingCount = 0;
            if (resetFirstP)
                DEFAULT_GROUP.resetAll(false);

            System.out.println("Extrapolating from decreasing noisy sequence: " + displayIntGroup(noisyDecreasing));
            DEFAULT_GROUP.setMode(Group.MODE.EXTRAPOLATION);
            {
                errorMessage = "Failed to tally increasing/decreasing recognition counts";
                for (int k = 0; k<totalSteps;k++){
                    DEFAULT_GROUP.processInput(noisyDecVectorSeq[k]);
                    FeatureModel model = DEFAULT_GROUP.getFocusModel();
                    if (model != null){
                        StringHashtableValue metaMap = (StringHashtableValue)DEFAULT_GROUP.getCustomMetadata(model);
                        if (metaMap != null){
                            String type = metaMap.getStringHashtable().get("TYPE").getString();
                            if (INCREASING_TYPE_KEY.equals(type)){
                                increasingCount++;
                            }

                            if (DECREASING_TYPE_KEY.equals(type)){
                                decreasingCount++;
                            }
                        }

                    }
                }

                Assert.assertTrue(errorMessage, increasingCount < decreasingCount);

                System.out.println(String.format("For decreasing sequence, increasing/decreasing (%1$d, %2$d)", increasingCount, decreasingCount));
            }


        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testAugmentedVectorViewer(){
        String errorMessage = null;
        try
        {
            int[] input = {5, 2};
            int radiix = 10;
            errorMessage = "Failed to create augmented vector from " + Arrays.toString(input);
            Vector augmented = getAugmentedVector(input, radiix);
            errorMessage = "Failed to get string form";
            LearningConfiguration.InputToStringConverter converter = getInputToStringConverter(radiix, radiix);

            String toString = converter.toString(augmented.rawFloat());
            System.out.println("Created: " + toString);
        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testAugmentedVector(){
        int[] input = {1, 9};

        String errorMessage = null;
        try
        {
            int radiix = 10;
            errorMessage = "Failed to get augmented vector";
            Vector v = getAugmentedVector(input, radiix);
            System.out.println(Arrays.toString(input) + "->" + v);

            errorMessage = "Failed to recover int vector";
            int[] recovered = getIntVectorFromAugmentedVector(v, radiix);
            errorMessage = "Failed to match input.  Expected " + Arrays.toString(input) + " but found: " + Arrays.toString(recovered);

            Assert.assertTrue(errorMessage, recovered != null && recovered.length == input.length);
            for (int j = 0;j<input.length;j++){
                Assert.assertTrue(errorMessage, recovered[j] == input[j]);
            }

        }
        catch(Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testAugmentedVectors(){
        String errorMessage = "Failed to construct augmented vectors";
        int[][] focusTest = {{1, 9}, {2,8}, {3, 7}, {4, 6}, {5, 5}, {6, 4}, {7, 3}, {8, 2}, {9,1}};
        int radiix = 10;
        try{

            Vector[] augmented = getAugmentedVectors(focusTest, radiix);

            Assert.assertTrue(errorMessage, augmented != null && augmented.length == focusTest.length);
            for (int i = 0; i < augmented.length;i++){
                errorMessage = "Failed to recover int vector: " + Arrays.toString(focusTest[i]);

                int[] recovered = getIntVectorFromAugmentedVector(augmented[i], radiix);

                Assert.assertTrue(errorMessage, recovered != null && recovered.length == focusTest[i].length);

                errorMessage = "Failed to recover int vector.  Expected " + Arrays.toString(focusTest[i]) + " but found: " + Arrays.toString(recovered);
                for (int j = 0; j < recovered.length;j++){

                    Assert.assertTrue(errorMessage, recovered[j] == focusTest[i][j]);
                }


            }

            Vector[] reconstructed = reconstructFromPackedIntVectors(augmented, radiix);

            System.out.println("Recovered: " + Arrays.toString(reconstructed));

        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testAugmentedVectorValidator(){
        String errorMessage = null;

        try
        {
            long seed = 1543001323604L;
            Random r = new Random();
            r.setSeed(seed);

            System.out.println("Using seed: " + seed);
            int radiix = 10;
            int[] intInput = {1, 9};
            errorMessage = "Failed to create augmented vector from " + Arrays.toString(intInput);
            Vector augmentedVector = getAugmentedVector(intInput, radiix);
            errorMessage = "Failed to create input validator";
            LearningConfiguration.InputValidator validator = getAugmentedVectorInputValidator(radiix ,radiix);
            errorMessage = "Failed to validate input: " + augmentedVector;
            Assert.assertTrue(errorMessage, validator.isValid(augmentedVector.rawFloat()));
            float[] dat = augmentedVector.rawFloat();
            errorMessage = "Failed to introduce noise into input";

            int seletedIndex = (int)(dat.length*r.nextDouble());
            System.out.println("Corrupting bit: " + seletedIndex);
            dat[seletedIndex] = 1 - dat[seletedIndex];

            errorMessage = "Failed to invalidate: " + Arrays.toString(dat);
            Assert.assertTrue(errorMessage, !validator.isValid(dat));
            System.out.println("Verified with noisy input: " + Arrays.toString(dat));
        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    public LearningConfiguration.InputToStringConverter getInputToStringConverter(int... dimen){
        return (float[] input)->{
            int width = input.length;
            int index = 0, total = 0;
            int[] out = new int[dimen.length];
            float[] buffer = new float[dimen[index]];
            for (int i = 0; i < width;i++){
                if (i >= total + dimen[index]){
                    total+=dimen[index];
                    out[index] = tallyVectorToInteger(NNTools.roundToInt(buffer));
                    index++;

                    buffer = new float[dimen[index]];
                }
                buffer[i - total] = input[i];
            }
            out[dimen.length - 1] = tallyVectorToInteger(NNTools.roundToInt(buffer));
            return Arrays.toString(out);
        };
    }

    public LearningConfiguration.InputValidator getAugmentedVectorInputValidator(int...dimen){

        return (float[] input)->{

            int index = 0;
            int total = 0;
            boolean seenZero = false;
            boolean isZero = false;

            for (int i =0; i < input.length;i++){
                if (i >= total + dimen[index]){
                    total+=dimen[index];
                    seenZero = false;
                    index++;
                }
                isZero = Math.round(input[i]) == 0F;
                if (seenZero && !isZero)
                    return false;

                seenZero = seenZero || isZero;

            }
            return true;
        };
    }

    float[] getMask(int[] includedDimens, int... dimen){
        HashSet<Integer> inclusion = new HashSet<>();
        Arrays.stream(includedDimens).forEach((x)->inclusion.add(x));
        HashSet<Integer> ones = new HashSet<>();
        int index = 0;
        for (int i = 0; i < dimen.length;i++){
            if (inclusion.contains(i)){
                for (int j = 0; j < dimen[i];j++){
                    ones.add(index);
                    index++;
                }
            }
            else {
                for (int j = 0; j < dimen[i];j++){
                    index++;
                }
            }

        }

        float[] mask = new float[index];
        for (int j = 0; j < index;j++){
            if (ones.contains(j))
                mask[j]=1.0F;
            else
                mask[j]=0.0F;
        }
        return mask;
    }


    int[] vectorsToInts(Vector[] in){
        int[] out = new int[in.length];

        for (int i = 0; i < out.length;i++){
            if (in[i] == null)
            {
                out[i] = -1;
                continue;
            }
            out[i] = tallyVectorToInteger(NNTools.roundToInt(in[i].rawFloat()));
        }
        return out;
    }

    Vector getAugmentedVector(int[] intVect, int radiix){
        int numIntDim = intVect.length;
        int dim = radiix * numIntDim;
        float[] elements = new float[dim];
        for (int j = 0;j<dim;j++){
            int index = j/radiix;
            elements[j] = intToTallyVector(intVect[index], radiix).rawFloat()[j % radiix];
        }
        return new Vector(elements);
    }

    int[] getIntVectorFromAugmentedVector(Vector v, int radiix){

        float[] raw = v.rawFloat();

        int ivLength = raw.length/radiix;
        int[] o = new int[ivLength];
        float[] fout = null;
        for (int i = 0; i < raw.length;i++){
            int index = i/radiix;

            if (i % radiix == 0 && i > 0){
                o[index-1] = tallyVectorToInteger(fout).intValue();
            }

            if (i % radiix == 0){
                fout = new float[radiix];
            }

            fout[i % radiix] = raw[i];
        }

        o[ivLength-1] = tallyVectorToInteger(fout).intValue();
        return o;
    }


    String displayIntGroup(int[][] a){

        String[] o = new String[a.length];
        for (int i = 0;i<a.length;i++){
            o[i] = Arrays.toString(a[i]);
        }

        return Arrays.toString(o);
    }


    Vector[] getAugmentedVectors(int[][] data, int radiix){
        Vector[] out = new Vector[data.length];

        for (int i = 0; i < out.length;i++){
            int[] dimensions = data[i];

            out[i] = getAugmentedVector(dimensions, radiix);
        }

        return out;
    }

    Vector[] reconstructFromPackedIntVectors(Vector[] packed, int radiix){
        Vector[] out = new Vector[packed.length];

        for (int i = 0; i < packed.length; i++){
            Vector element = packed[i];
            out[i] = new Vector(getIntVectorFromAugmentedVector(element, radiix));
        }

        return out;
    }

    int[] vectorsToInts(ArrayList<Vector> in){
        return vectorsToInts(in.toArray(new Vector[0]));
    }

    ArrayList<Vector> convert(int[] base, int radiix, int max){
        ArrayList<Vector> out = new ArrayList<>();

        for (int i = 0;i< Math.min(base.length, max);i++){
            out.add(intToTallyVector(base[i], radiix));
        }
        return out;
    }

    int[] getSuffix(int[] base, int suffixLength){
        int[] out = new int[base.length - suffixLength];
        for (int i = suffixLength;i < base.length;i++){
            out[i -  suffixLength] = base[i];
        }
        return out;
    }

    ArrayList<Vector> convert(int[] base, int radiix){
        return convert(base, radiix, base.length);
    }


    Vector intToTallyVector(int value, int radiix){
        float[] out = new float[radiix];
        for (int i = 1; i <= radiix;i++)
        {
            if (i <= value)
            {
                out[i - 1] = 1F;
            }
        }

        return new Vector(out);
    }

    public Integer tallyVectorToInteger(float[] value){
        int radiix = value.length;

        Integer out = null;
        int j = 0;
        boolean zeros = false;
        for (int i = 0; i < radiix;i++){
            if (value[i] == 1.0F){
                if (zeros)
                    return null;
                j++;
            }
            else {
                zeros = true;
            }
        }
        return j;
    }

    @Test
    public void testSingleConcurrentGenerators(){
        String errorMessage = "Failed to create concurrent generator";

        try
        {
            ConcurrentGenerator<Long> generator = new ConcurrentGenerator<Long>();

            Long result = null;
            generator.addSupplier(new InterruptibleResultProducer<Long>() {
                @Override
                public Long evaluate()
                {
                    Long time = System.nanoTime();
                    int maxValue = 100000000;
                    int max = (int)(maxValue*Math.random());
                    for (int i = 0;i < max;i++){

                    }
                    return Long.valueOf(System.nanoTime() - time);
                }
            });

            errorMessage = "Failed to get result";
            result = generator.getResult();
            errorMessage = "Failed to compute result";

            Assert.assertTrue(result != null);
            System.out.println("Result is: " + result);
        }
        catch (Exception e){
            Assert.assertTrue(errorMessage,false);
        }

    }


    @Test
    public void testMultipleConcurrentGenerators(){
        String errorMessage = "Failed to create concurrent generator";

        try
        {

            ConcurrentGenerator<Long> generator = new ConcurrentGenerator<Long>();

            Long result = null;
            generator.addSupplier(new InterruptibleResultProducer<Long>() {
                @Override
                public Long evaluate()
                {
                    Long time = System.nanoTime();
                    int maxValue = 100000000;
                    int max = (int)(maxValue*Math.random());
                    for (int i = 0;i < max;i++){
                        if (Thread.currentThread().isInterrupted())
                            return null;
                    }
                    return Long.valueOf(System.nanoTime() - time);
                }
            }).addSupplier(new InterruptibleResultProducer<Long>() {
                @Override
                public Long evaluate()
                {
                    Long time = System.nanoTime();
                    int maxValue = 100000000;
                    int max = (int)(maxValue*Math.random());
                    for (int i = 0;i < max;i++){
                        if (Thread.currentThread().isInterrupted())
                            return null;
                    }
                    return Long.valueOf(System.nanoTime() - time);
                }
            }).addSupplier(new InterruptibleResultProducer<Long>() {
                    @Override
                    public Long evaluate()
                    {
                        Long time = System.nanoTime();
                        int maxValue = 100000000;
                        int max = (int)(maxValue*Math.random());
                        for (int i = 0;i < max;i++){
                            if (Thread.currentThread().isInterrupted())
                                return null;
                        }
                        return Long.valueOf(System.nanoTime() - time);
                    }
                }).addSupplier(new InterruptibleResultProducer<Long>() {
                    @Override
                    public Long evaluate()
                    {
                        Long time = System.nanoTime();
                        int maxValue = 100000000;
                        int max = (int)(maxValue*Math.random());
                        for (int i = 0;i < max;i++){
                            if (Thread.currentThread().isInterrupted())
                                return null;
                        }

                        return Long.valueOf(System.nanoTime() - time);
                    }
                });

            errorMessage = "Failed to get result";
            result = generator.getResult();
            errorMessage = "Failed to compute result";

            Assert.assertTrue(errorMessage, result != null);
            System.out.println("Result is: " + result);

            boolean isValid = generator.isResultValid();
            errorMessage = "Result not valid";
            Assert.assertTrue(errorMessage, isValid);
        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage,false);
        }

    }

    @Test
    public void testMultipleConcurrentGeneratorsWithConditions(){
        String errorMessage = "Failed to create concurrent generator";

        try
        {
            long MaxValue = 100000000;
            long cutoffValid = System.currentTimeMillis();
            ConcurrentGenerator<Long> generator = new ConcurrentGenerator<Long>((Long result)->result>cutoffValid);

            Long result = null;
            generator.addSupplier(new InterruptibleResultProducer<Long>() {
                @Override
                public Long evaluate()
                {
                    Long time = System.nanoTime();
                    long maxValue = MaxValue;
                    int max = (int)(maxValue*Math.random());
                    for (int i = 0;i < max;i++){
                        if (Thread.currentThread().isInterrupted())
                            return null;
                    }
                    return Long.valueOf(System.nanoTime() - time);
                }
            }).addSupplier(new InterruptibleResultProducer<Long>() {
                @Override
                public Long evaluate()
                {
                    Long time = System.nanoTime();
                    int maxValue = 100000000;
                    int max = (int)(maxValue*Math.random());
                    for (int i = 0;i < max;i++){
                        if (Thread.currentThread().isInterrupted())
                            return null;
                    }
                    return Long.valueOf(System.nanoTime() - time);
                }
            }).addSupplier(new InterruptibleResultProducer<Long>() {
                @Override
                public Long evaluate()
                {
                    Long time = System.nanoTime();
                    int maxValue = 100000000;
                    int max = (int)(maxValue*Math.random());
                    for (int i = 0;i < max;i++){
                        if (Thread.currentThread().isInterrupted())
                            return null;
                    }
                    return Long.valueOf(System.nanoTime() - time);
                }
            }).addSupplier(new InterruptibleResultProducer<Long>() {
                @Override
                public Long evaluate()
                {
                    Long time = System.nanoTime();
                    int maxValue = 100000000;
                    int max = (int)(maxValue*Math.random());
                    for (int i = 0;i < max;i++){
                        if (Thread.currentThread().isInterrupted())
                            return null;
                    }

                    return Long.valueOf(System.nanoTime() - time);
                }
            });

            errorMessage = "Failed to get result";
            result = generator.getResult();
            errorMessage = "Failed to compute result";

            Assert.assertTrue(errorMessage, result != null);
            System.out.println("Result is: " + result);

            boolean isValid = generator.isResultValid();
            errorMessage = "Result not valid";
            Assert.assertTrue(errorMessage, isValid && result>cutoffValid || !isValid && result<cutoffValid);
            System.out.println("Result is valid: " + isValid + " cutoff" + cutoffValid);
        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage,false);
        }

    }


    public LearningConfiguration getSimpleLearningConfiguration(){
        LearningConfiguration configuration = new LearningConfiguration();
        configuration.set(LearningConfiguration.KEY.ANNEALING_FRACTION, Double.valueOf(0.1F));
        configuration.set(LearningConfiguration.KEY.BEST_SOLUTION_BONUS_MILLI, Integer.valueOf(1000));
        configuration.set(LearningConfiguration.KEY.NUM_SOLUTION_BUFFER, Integer.valueOf(5));
        configuration.set(LearningConfiguration.KEY.INITIAL_RANDOM_FRACTION, Float.valueOf(0.5F));
        //configuration.set(LearningConfiguration.KEY.MAX_ITERATIONS, Integer.valueOf(3000));
        //configuration.set(LearningConfiguration.KEY.BEST_SOLUTION_BONUS_ITERATIONS, Integer.valueOf(400));
        return configuration;
    }

    public String dateParts(){
        Calendar calendar = Calendar.getInstance();
        int year = calendar.get(Calendar.YEAR);
        int month = calendar.get(Calendar.MONTH);
        int dayOfMonth = calendar.get(Calendar.DAY_OF_MONTH);
        int dayOfYear = calendar.get(Calendar.DAY_OF_YEAR);
        int hourOfDay = calendar.get(Calendar.HOUR_OF_DAY);
        int hourAMPM = calendar.get(Calendar.HOUR);
        int am_pm = calendar.get(Calendar.AM_PM);
        String am_pm_label;
        if (am_pm == Calendar.AM)
            am_pm_label = "AM";
        else
            am_pm_label = "PM";
        int minute = calendar.get(Calendar.MINUTE);
        int second = calendar.get(Calendar.SECOND);
        int millisecond = calendar.get(Calendar.MILLISECOND);

        return String.format("%d/%d/%d:%d:%d:%d", year, month, dayOfMonth, hourOfDay, minute, second, millisecond);
    }

    public void debug(String tag, String text) {
        String log = String.format("%s [%s] - %s", dateParts(), tag, text);
        System.out.println(log);
    }

    public IncrementalUpdateSpec simpleLearnSequence(LSTMNetworkProxy network, ArrayList<Pair<Vector, Vector>> inputOutputSpec, LearningConfiguration configuration){
        String message = null;
        long startTime = System.currentTimeMillis();
        long stopTime = 0;
        Integer maxDuration = null;
        if (configuration.hasMaxDuration()) {
            maxDuration = configuration.getMaxDurationMilli();
            stopTime = startTime + maxDuration;
        }

        if (configuration.initialRandomFraction() != null)
            network.randomizeNetworkWeights(configuration.initialRandomFraction());

        double randomFraction = 1;
        double minRandomFraction = 0.05;
        double maxRandomFraction = 1;

        double convergenceThreshold = 0.00001;
        double successFraction = 0;
        double previousSequenceError = 0;
        boolean finishedP = false;
        int i = 0;
        // TODO: initialize this from prior steps provided in configuration
        int baseImprovementIndex = 0;
        int highestSegmentMatchCount = 0;
        int segmentMatchCount = 0;
        int numInputOutputPairs = inputOutputSpec.size();
        int newSolutionFailureCount = 0;

        int maxFailureCount = 0;
        boolean isFirstPair = true;
        boolean isLastPair = true;
        boolean firstPairMatchedP = false;
        boolean lastPairMatchedP = false;
        boolean allSegmentsMatchedP = false;
        boolean roundErrorsP = false;
        boolean hasPreviousSequenceError = false;
        boolean isValidP = false;
        boolean isSuccessCriteriaSatisfiedP = false;

        // TODO: initialize this from configuration
        int solutionIndex = 0;
        int improvementIndex = 0;
        int resetWidth = 50;
        if (configuration.getFailureIterations() != null){
            resetWidth = configuration.getFailureIterations();
        }

        Integer maxIterations = configuration.getMaxIterations();

        // TODO: make this part of configuration
        boolean chooseRandomlyAmongstViableSolutionsP = false;
        boolean currentNetworkIsBestP = false;
        Integer bestSolutionBufferSize = configuration.getBestSolutionBufferSize();
        if (bestSolutionBufferSize == null)
            bestSolutionBufferSize = 1;

        LSTMNetworkProxy currentNetwork = network;
        ArrayList<IncrementalUpdateSpec> bestNetworks = new ArrayList<IncrementalUpdateSpec>();
        ArrayList<Integer> annealingCountList = new ArrayList<Integer>();

        IncrementalUpdateSpec bestUpdateSpec = null, lastValidUpdateSpec = null;
        double lastValidMatchFraction = 0;

        while (!finishedP &&
                (!configuration.hasMaxDuration() || (System.currentTimeMillis() < stopTime)) &&
                (!configuration.hasMaxIterations() || i < maxIterations))
        {

            Double sequenceError = null;
            Double pairError = null;
            allSegmentsMatchedP = true;
            lastPairMatchedP = false;
            LSTMNetworkProxy.NodeState initialState = null, temp;
            LSTMNetworkProxy.NodeState finalState = null;

            currentNetwork.resetNetworkToInitialState();

            // Process all segments of the input sequence
            int j = 0;
            for (Pair<Vector, Vector> inputOutputPair:inputOutputSpec) {
                float[] input = inputOutputPair.getLeft().rawFloat();
                float[] expectedOutput = inputOutputPair.getRight().rawFloat();
                isFirstPair = j == 0;
                isLastPair = j == numInputOutputPairs - 1;

                currentNetwork.executeForwardPass(input);

                if (isFirstPair) {
                    initialState =  currentNetwork.getCurrentNodeState();
                }

                if (isLastPair) {
                    finalState =  currentNetwork.getCurrentNodeState();
                }

                pairError = Double.valueOf(currentNetwork.getOutputError(expectedOutput, false, false, true));

                debug("FORWARD-PASS", "Errors: " + pairError);
                if (sequenceError == null) {
                    sequenceError = pairError;
                }
                else {
                    sequenceError = Math.max(sequenceError.doubleValue(), pairError.doubleValue());
                }

                float[] predictedOutput = currentNetwork.getOutputVaues();

                if (roundedEquals(predictedOutput, expectedOutput)) {
                    segmentMatchCount++;
                    if (isFirstPair) {
                        firstPairMatchedP = true;
                    }

                    if (isLastPair) {
                        lastPairMatchedP = true;
                    }

                }
                else {
                    allSegmentsMatchedP = false;
                }
                j++;
            }

            // Now assess the current lstm's performance
            if (allSegmentsMatchedP) {
                debug("ALL-MATCH", "finished");
                return new IncrementalUpdateSpec(true, true, currentNetwork, true, initialState, finalState, 1);
            }

            if ((currentNetworkIsBestP = highestSegmentMatchCount < segmentMatchCount) ||
                    (bestSolutionBufferSize > bestNetworks.size() && firstPairMatchedP && highestSegmentMatchCount == segmentMatchCount)) {

                // Handle case where current network is better than any previous
                if (currentNetworkIsBestP) {
                    bestNetworks = new ArrayList<>();
                    highestSegmentMatchCount = segmentMatchCount;
                    successFraction = 1.0*segmentMatchCount/numInputOutputPairs;

                    boolean hasMaxIterationBonus = configuration.hasBestSolutionBonusIterations() && configuration.hasMaxIterations();
                    boolean hasMaxDurationBonus = configuration.hasBestSolutionBonusMilli() && configuration.hasMaxDuration();

                    boolean satisfiesLastPairCriteriaP = (lastPairMatchedP && configuration.requiresLastPairToMatch()) || !configuration.requiresLastPairToMatch();

                    if (satisfiesLastPairCriteriaP &&
                            !configuration.requiresCompleteMatchP() &&
                            configuration.earlyStopSuccessFraction() <= successFraction) {
                        if (hasMaxIterationBonus || hasMaxDurationBonus) {
                            if (hasMaxIterationBonus) {
                                maxIterations = i + maxIterations;
                            }

                            if (hasMaxDurationBonus) {
                                stopTime = System.currentTimeMillis() + configuration.getBestSolutionBonusMilli();
                            }
                        }
                        else {
                            finishedP = true;
                            debug("", "Early stop");
                        }

                    }
                    else {
                        if (hasMaxIterationBonus) {
                            maxIterations = i + maxIterations;
                        }

                        if (hasMaxDurationBonus) {
                            stopTime = System.currentTimeMillis() + configuration.getBestSolutionBonusMilli();
                        }
                    }
                }

                //

                debug("ANOTHER-SOLUTION", "Found another solution after: " + (i - improvementIndex) + " steps");
                newSolutionFailureCount = 0;
                improvementIndex = i;

                // ......................
                // Booleans
                isValidP = (lastPairMatchedP && configuration.requiresLastPairToMatch()) || bestNetworks.size() > 0;
                configuration.setResultIsValid(isValidP);

                isSuccessCriteriaSatisfiedP = isValidP &&
                        (allSegmentsMatchedP ||
                                (!configuration.requiresCompleteMatchP() &&
                                        (successFraction >= configuration.earlyStopSuccessFraction()) &&
                                        (!configuration.requiresLastPairToMatch() || lastPairMatchedP && configuration.requiresLastPairToMatch())));
                // +++++++++++++++++++++


                bestUpdateSpec = new IncrementalUpdateSpec(isValidP, isSuccessCriteriaSatisfiedP, currentNetwork, true, initialState, finalState, (float)successFraction);
                bestNetworks.add(bestUpdateSpec);
                annealingCountList.add(0);


                if (lastPairMatchedP && configuration.requiresLastPairToMatch()) {
                    debug("LAST-MATCH", "Found network satisfying last segment matching at " + successFraction);
                    lastValidUpdateSpec = bestUpdateSpec;
                    lastValidMatchFraction = successFraction;
                }
            }

            // Weight adaptation

            if (!finishedP) {
                if (hasPreviousSequenceError) {
                    // Not sure what roundErrorsP is for
                    boolean networkHasConverged = roundErrorsP || Math.abs((sequenceError - previousSequenceError)/sequenceError) < convergenceThreshold;
                    boolean tooLongSinceLastImprovement = ((i - improvementIndex) > resetWidth) && (i - baseImprovementIndex) > resetWidth;
                    if (networkHasConverged || tooLongSinceLastImprovement) {
                        int numBestSolutions = bestNetworks.size();
                        // Select one of the set of good solutions and use this
                        if (numBestSolutions > 0) {
                            int selectedIndex;
                            if (chooseRandomlyAmongstViableSolutionsP) {
                                selectedIndex = (int)(numBestSolutions * Math.random());
                            }
                            else {
                                selectedIndex = solutionIndex;
                            }
                            currentNetwork = LSTMNetworkProxy.duplicate(bestNetworks.get(selectedIndex).getNetwork());

                            newSolutionFailureCount = annealingCountList.get(selectedIndex);

                            annealingCountList.set(selectedIndex, newSolutionFailureCount + 1);

                            if (configuration.hasAnnealingFraction()) {
                                randomFraction = configuration.annealingFraction();
                            }
                            else {
                                // TODO: Fix this.  Doesn't work.  Is supposed to decrease with a longer .
                                // This else-clause is not supposed to decrease as newSolutionFailureCount increases
                                randomFraction = Math.min(maxRandomFraction, (minRandomFraction + (newSolutionFailureCount * (maxRandomFraction - minRandomFraction)/maxRandomFraction)));
                            }

                        }
                        else {
                            if (configuration.hasAnnealingFraction()) {
                                randomFraction = configuration.annealingFraction();
                            }
                            else
                                randomFraction = 1;
                        }

                        currentNetwork.randomizeNetworkWeights((float)randomFraction);

                        hasPreviousSequenceError = false;
                        newSolutionFailureCount++;
                        baseImprovementIndex = i;
                    }
                    else {
                        currentNetwork.updateWeights(LSTMNetwork.WeightUpdateType.RPROP);
                    }
                }
                else {
                    hasPreviousSequenceError = true;
                    currentNetwork.updateWeights(LSTMNetwork.WeightUpdateType.RPROP);
                }
            }

            previousSequenceError = sequenceError;
            i++;
        }

        long endTime = System.currentTimeMillis();
        int seconds = (int)((endTime - startTime)/1000);

        message = ((configuration.requiresLastPairToMatch() && lastPairMatchedP)?"Done satisfying last pair requirements after ":"Done after") +
                seconds +
                " seconds and " +
                i +
                " steps " +
                ((lastPairMatchedP)?lastValidMatchFraction:successFraction);

        debug("FINISHED", message);

        configuration.wasSuccessCriteriaSatisfied(isSuccessCriteriaSatisfiedP);

        int selectedIndex;
        if (chooseRandomlyAmongstViableSolutionsP) {
            selectedIndex = (int)(bestNetworks.size() * Math.random());
        }
        else {
            selectedIndex = solutionIndex;
        }

        return bestNetworks.get(selectedIndex);
    }

    public static boolean roundedEquals(float[] first, float[] second) {
        if (first.length != second.length)
            return false;
        for (int i = 0;i < first.length;i++)
        {
            if (roundToInt(first[i]) != roundToInt(second[i]))
                return false;
        }
        return true;
    }

    public static ArrayList<Pair<Vector, Vector>> getInputOututSpec(Vector[] raw){
        ArrayList<Pair<Vector, Vector>> output = new ArrayList<Pair<Vector, Vector>>();
        for (int i = 0;i < raw.length - 1;i++){
            output.add(Pair.of(raw[i], raw[i+1]));
        }
        return output;
    }

    public static ArrayList<Pair<Vector, Vector>> getInputOututSpec(ArrayList<Vector> raw){
        Vector[] in = raw.toArray(new Vector[0]);

        return getInputOututSpec(in);
    }

}
