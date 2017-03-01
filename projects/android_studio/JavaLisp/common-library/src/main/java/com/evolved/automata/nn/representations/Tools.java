package com.evolved.automata.nn.representations;

import com.evolved.automata.nn.FastLSTMNetwork;
import com.evolved.automata.nn.LSTMNetwork;
import com.evolved.automata.nn.NNTools;

import java.util.Arrays;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 2/27/17.
 */

public class Tools {
    static final int MATCH_COUNT = 0;
    static final int FAILURE_COUNT = 1;
    static final int LENGTH = 2;
    static final int WEIGHT = 3;
    static final int ID = 4;


    public static boolean DEBUG = true;

    static int createCappedLSTM(float[] networkSpec, float[] startInput, float[] stopInput)
    {
        LinkedList<FastLSTMNetwork.TrainingSpec> trainingSpec = new LinkedList<FastLSTMNetwork.TrainingSpec>();

        trainingSpec.add(FastLSTMNetwork.trainingSpec(startInput, stopInput, null, false, false));


        FastLSTMNetwork.initializeAllWeights(networkSpec);
        int steps = learnTrainingSpec(networkSpec, trainingSpec, 20, 0.001F, true, LSTMNetwork.WeightUpdateType.RPROP);
        FastLSTMNetwork.setCustomData(networkSpec, 0, MATCH_COUNT);
        FastLSTMNetwork.setCustomData(networkSpec, 0, FAILURE_COUNT);
        FastLSTMNetwork.setCustomData(networkSpec, 0, WEIGHT);
        return steps;
    }


    static int appendVectorToSequence(float[] cappedNetworkSpec, float[] startInput, float[] stopInput, float[] newInput, int maxSteps, float convergenceThreshold, boolean allowWeightResetsP, FastLSTMNetwork.WeightUpdateType updateType)
    {
        float[] output;
        float[] copy = Arrays.copyOf(cappedNetworkSpec, cappedNetworkSpec.length);

        LinkedList<FastLSTMNetwork.TrainingSpec> trainingSpec = new LinkedList<FastLSTMNetwork.TrainingSpec>();

        FastLSTMNetwork.TrainingSpec spec;
        int dataId = (int)getId(copy);
        float[] input = startInput;
        FastLSTMNetwork.resetNetworkToInitialState(copy);
        FastLSTMNetwork.forwardPass(copy, input);
        output = NNTools.roundToInt(FastLSTMNetwork.getOutputActivation(copy));


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

        int stepsUsed = learnTrainingSpec(copy, trainingSpec, maxSteps, convergenceThreshold, allowWeightResetsP, updateType);

        if (stepsUsed <= maxSteps)
        {
            for (int i = 0; i < cappedNetworkSpec.length;i++)
            {
                cappedNetworkSpec[i] = copy[i];
            }

            incrementLength(cappedNetworkSpec);
        }

        return stepsUsed;
    }


    static int  learnTrainingSpec(float[] networkSpec, LinkedList<FastLSTMNetwork.TrainingSpec> trainingSpec, int maxSteps, float convergenceThreshold, boolean allowWeightResetsP, FastLSTMNetwork.WeightUpdateType updateType)
    {
        int id = (int)getId(networkSpec);
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

                if (DEBUG)
                    System.out.println("Predictor (" + id + ") Learned pattern in: " + (i + 1) + " steps and error of " + compError);

                return i + 1;
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

        if (DEBUG)
            System.out.println("Predictor (" + id + ") failed to learn pattern after: " + (i + 1) + " steps and error of " + compError);
        return i + 1;
    }

    public static boolean maskedRoundedVerifyResult(float[] value, float[] expectedValue, float[] errorMask)
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

    static void decrementMatchCount(float[] networkSpec)
    {
        int prior = (int)FastLSTMNetwork.getCustomData(networkSpec, MATCH_COUNT, -1);
        FastLSTMNetwork.setCustomData(networkSpec, Math.max(0, prior - 1), MATCH_COUNT);

    }

    static void decrementFailureCount(float[] networkSpec)
    {
        decrementCustomParameter(networkSpec, FAILURE_COUNT, false);

    }


    static void resetFailureCount(float[] networkSpec)
    {
        FastLSTMNetwork.setCustomData(networkSpec, 0, FAILURE_COUNT);

    }

    static void resetMatchCount(float[] networkSpec)
    {
        FastLSTMNetwork.setCustomData(networkSpec, 0, MATCH_COUNT);

    }




    static void decrementCustomParameter(float[] networkSpec, int parameter, boolean allowNegativeP)
    {
        int prior;
        float badValue = -1;
        if (allowNegativeP)
             badValue = Float.MIN_VALUE;

        prior = (int)FastLSTMNetwork.getCustomData(networkSpec, parameter, badValue);
        if (prior == badValue){
            // Error

        }
        if (allowNegativeP)
            FastLSTMNetwork.setCustomData(networkSpec, prior - 1, parameter);
        else
            FastLSTMNetwork.setCustomData(networkSpec, Math.max(0, prior - 1), parameter);
    }


    static void incrementCustomParameter(float[] networkSpec, int parameter)
    {
        int prior;
        float badValue = -1;


        prior = (int)FastLSTMNetwork.getCustomData(networkSpec, parameter, badValue);
        if (prior == badValue){
            // Error

        }
        FastLSTMNetwork.setCustomData(networkSpec, prior + 1, parameter);
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
}
