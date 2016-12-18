package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public class HypertangentActivation implements ActivationFunction {

    @Override
    public double value(Vector netInput, int i)
    {
        return Math.tanh(netInput.value(i));
    }

    @Override
    public double valuePrime(Vector netInput, int i)
    {
        double cosh = Math.cosh(netInput.value(i));
        return 1/cosh/cosh;
    }
}
