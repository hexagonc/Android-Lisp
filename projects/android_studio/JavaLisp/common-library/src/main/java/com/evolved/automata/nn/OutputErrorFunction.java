package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public interface OutputErrorFunction {
    double getError(Vector targetVector, Vector y);
    Vector errorGradient(Vector targetVector, Vector netInput, ActivationFunction activation);
}
