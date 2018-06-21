package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public class LeastSquaresError implements ErrorFunction {
    @Override
    public double error(Vector t, Vector y)
    {
        double s = 0;
        for (int k = 0;k < t.dimen();k++)
        {
            s+= (t.value(k) - y.value(k)) * (t.value(k) - y.value(k));
        }
        return 0.5*s;
    }

    /**
     * This is
     * @param t
     * @param netOutput
     * @param outputActivation
     * @param k
     * @return
     */
    @Override
    public double errorDerivative(Vector t, Vector netOutput, ActivationFunction outputActivation, int k)
    {
        return (outputActivation.value(netOutput, k) - t.value(k)) * outputActivation.valuePrime(netOutput, k);
    }
}
