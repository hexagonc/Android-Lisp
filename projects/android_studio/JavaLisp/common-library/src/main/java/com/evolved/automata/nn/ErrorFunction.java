package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public interface ErrorFunction {

    double error(Vector t, Vector y);
    double errorDerivative(Vector t, Vector netOutput, ActivationFunction outputActivation,  int k);
}
