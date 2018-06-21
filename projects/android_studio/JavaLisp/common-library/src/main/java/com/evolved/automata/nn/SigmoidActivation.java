package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public class SigmoidActivation implements ActivationFunction {
    @Override
    public double value(Vector netInput, int i)
    {
        return 1/(1 + Math.exp(-1*netInput.value(i)));
    }

    @Override
    public double valuePrime(Vector netInput, int i)
    {
        return (1 - value(netInput, i))*value(netInput, i);
    }
}
