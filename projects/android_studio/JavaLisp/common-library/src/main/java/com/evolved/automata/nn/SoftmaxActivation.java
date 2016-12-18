package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public class SoftmaxActivation implements ActivationFunction {
    @Override
    public double value(Vector netInput, int i)
    {
        double sum = 0;
        double numerator = 0;
        for (int k = 0; k < netInput.dimen();k++)
        {
            if (k == i)
            {
                numerator = Math.exp(netInput.value(i));
                sum+=numerator;
            }
            else
                sum+=Math.exp(netInput.value(i));
        }
        return numerator/sum;
    }

    // TODO: define this properly
    @Override
    public double valuePrime(Vector netInput, int i)
    {
        return 1;
    }
}
