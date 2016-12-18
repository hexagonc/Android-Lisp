package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public class InputLayer extends NodeGroup {

    public InputLayer(int dimen, ActivationFunction activationFunct, Vector activation, Vector bias)
    {
        super(dimen, activationFunct, activation, bias);
    }

    public InputLayer(int dimen, ActivationFunction activationFunct, Vector activation)
    {
        super(dimen, activationFunct, activation);
    }

    public InputLayer(int dimen, ActivationFunction activationFunct)
    {
        super(dimen, activationFunct);
    }

    public InputLayer setActivation(Vector value)
    {
        activation = value;
        return this;
    }

}
