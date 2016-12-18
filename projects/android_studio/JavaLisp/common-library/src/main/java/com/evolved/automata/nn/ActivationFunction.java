package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public interface ActivationFunction {
    public double value(Vector netInput, int i);
    public double valuePrime(Vector netInput, int i);
}
