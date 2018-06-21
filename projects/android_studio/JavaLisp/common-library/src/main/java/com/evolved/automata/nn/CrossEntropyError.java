package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public class CrossEntropyError implements ErrorFunction {
    @Override
    public double error(Vector t, Vector y)
    {
        // dividing log(y) by t so that the minimum error is zero
        //
        double s = 0;
        double minY = 0.001;
        for (int i = 0;i<t.dimen();i++)
        {
            if (t.value(i) != 0)
            {
                s += t.value(i) * Math.log(Math.max(minY, y.value(i))/t.value(i));
            }
        }

        return -s;
    }

    /**
     * Returns ∂E/∂net_i
     * @param t
     * @param netOutput
     * @param outputActivation
     * @param k
     * @return
     */
    @Override
    public double errorDerivative(Vector t, Vector netOutput, ActivationFunction outputActivation, int k)
    {
        return outputActivation.value(netOutput, k) -  t.value(k);
    }
}
