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

        // TODO: think of a better way of calculating e^x/∑e^k . e^20 < a billion so so long
        // as there aren't too many categories, there shouldn't be an issue with
        // arithmetic overflow of sum
        double maxPowerofe = 20;
        for (int k = 0; k < netInput.dimen();k++)
        {
            if (k == i)
            {
                numerator = Math.exp(Math.min(maxPowerofe, netInput.value(i)));
                sum+=numerator;
            }
            else
                sum+=Math.exp(Math.min(maxPowerofe, netInput.value(k)));
        }
        double s = numerator/sum;

        return s;
    }

    // TODO: define this properly
    @Override
    public double valuePrime(Vector netInput, int i)
    {
        return 1;
    }
}
