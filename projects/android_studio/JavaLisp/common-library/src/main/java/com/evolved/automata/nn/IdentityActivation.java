package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public class IdentityActivation implements ActivationFunction {
    @Override
    public double value(Vector netInput, int i)
    {
        return netInput.value(i);
    }

    @Override
    public double valuePrime(Vector netInput, int i)
    {
        return 1;
    }
}
