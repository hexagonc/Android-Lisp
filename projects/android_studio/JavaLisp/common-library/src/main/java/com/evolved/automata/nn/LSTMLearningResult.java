package com.evolved.automata.nn;

import java.util.Arrays;

/**
 * Created by Evolved8 on 12/19/16.
 */
public class LSTMLearningResult {
    public int numIterations;
    public double maxStepError;
    public double[] stepErrors;

    private LSTMLearningResult()
    {

    }

    public static LSTMLearningResult make(int numIterations, double maxStepError, double[] stepErrors)
    {
        LSTMLearningResult r = new LSTMLearningResult();
        r.maxStepError = maxStepError;
        r.numIterations = numIterations;
        r.stepErrors = stepErrors;
        return r;
    }

    public static LSTMLearningResult make(double[] combinedResults)
    {
        LSTMLearningResult r = new LSTMLearningResult();
        r.maxStepError = combinedResults[0];
        r.numIterations = (int)combinedResults[1];
        r.stepErrors = Arrays.copyOfRange(combinedResults, 2, combinedResults.length-1);
        return r;
    }

}
