package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public class HiddenLayer extends NodeGroup {

    public HiddenLayer(int dimen, ActivationFunction activationFunct, Vector activation, Vector bias)
    {
        super(dimen, activationFunct, activation, bias);
    }

    @Override
    public Vector getErrorResponsibility()
    {

        final Link output = getOutputTargetLinks();
        if (output == null)
            return new Vector(dimen);



        return netInput.map(new VectorMapper() {
            @Override
            public double map(double v, int i)
            {
                return activationFunct.valuePrime(netInput, i)*getOutputLinkError(output, i);
            }
        });
    }

    private double getOutputLinkError(Link outputLinks, int i)
    {

        double s = 0;

        WeightMatrix outputWeights = outputLinks.getWeights();
        OutputLayer output = (OutputLayer)outputLinks.getTargetNodes();
        int numOutputNodes = output.getDimen();

        for (int k = 0;k < numOutputNodes;k++)
        {
            s+=output.getErrorResponsibility().value(k)*outputWeights.value(k, i);
        }
        return s;
    }
}
