package com.evolved.automata.nn;


/**
 * Created by Evolved8 on 12/8/16.
 */
public class OutputLayer extends NodeGroup {

    ErrorFunction errorFunction;
    Vector targetOutput;

    public OutputLayer(int dimen, ActivationFunction activationFunct, ErrorFunction errorFunction)
    {
        super(dimen, activationFunct);
        this.errorFunction = errorFunction;
        targetOutput = null;
    }

    public void setTargetOutput(Vector t)
    {
        targetOutput = t;
        errorResponsibility = null;
    }

    public double getError()
    {
        return errorFunction.error(targetOutput, activation);
    }

    @Override
    public Vector getErrorResponsibility()
    {
        if (errorResponsibility != null)
            return errorResponsibility;
        else
        {
            errorResponsibility = new Vector(getDimen());
            errorResponsibility.mapD(new VectorMapper() {
                @Override
                public double map(double v, int i)
                {
                    // Error function is returning ∂E/∂net
                    return -1*errorFunction.errorDerivative(targetOutput, netInput, activationFunct, i);
                }
            });
            return errorResponsibility;

        }
    }
}
