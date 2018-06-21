package com.evolved.automata.nn;

import java.util.ArrayList;

/**
 * Created by Evolved8 on 12/8/16.
 */
public class NodeGroup {

    NetConfiguration.LayerType layerType;
    int dimen;

    Vector activationBias;
    final ActivationFunction activationFunct;
    Vector activation;
    Vector netInput;
    Vector errorResponsibility;
    Vector partialNetInput;

    ArrayList<Link> targetLinks;
    ArrayList<Link> sourceLinks;



    public NodeGroup(int dimen, ActivationFunction activationFunct, Vector activation, Vector bias)
    {
        // Const

        this.dimen = dimen;
        this.activationFunct = activationFunct;
        this.activation = activation;
        activationBias = bias;

        commonInit();
    }

    public NodeGroup(int dimen, ActivationFunction activationFunct, Vector activation)
    {
        this.dimen = dimen;
        this.activationFunct = activationFunct;
        this.activation = activation;
        activationBias = new Vector(dimen);

        commonInit();
    }

    public NodeGroup(int dimen, ActivationFunction activationFunct)
    {
        this.dimen = dimen;
        this.activationFunct = activationFunct;
        activationBias = new Vector(dimen);
        this.activation = new Vector(dimen);

        commonInit();


    }

    private void commonInit()
    {
        errorResponsibility = new Vector(dimen);
        netInput = new Vector(dimen);
        targetLinks = new ArrayList<Link>();
        sourceLinks = new ArrayList<Link>();
        partialNetInput = new Vector(dimen);
        layerType = NetConfiguration.LayerType.HIDDEN;
    }


    public boolean isOutputLayer()
    {
        return layerType == NetConfiguration.LayerType.OUTPUT;
    }

    public NetConfiguration.LayerType getLayerType()
    {
        return layerType;
    }

    public int getDimen()
    {
        return dimen;
    }

    public Vector getActivation()
    {
        return activation;
    }

    public Vector getErrorResponsibility()
    {
        return errorResponsibility;
    }

    public NodeGroup setErrorResponsibility(Vector v)
    {
        errorResponsibility = v;
        return this;
    }

    public ArrayList<Link> getSourceLinks()
    {
        return sourceLinks;
    }

    public ArrayList<Link> getTargetLinks()
    {
        return targetLinks;
    }

    public NodeGroup addTargetLink(Link link)
    {
        targetLinks.add(link);
        return this;
    }

    public NodeGroup addSourceLink(Link link)
    {
        sourceLinks.add(link);
        return this;
    }

    public NodeGroup addNetinput(Vector net)
    {
        partialNetInput.addD(net);
        return this;
    }

    public Vector getNetInput()
    {
        return netInput;
    }

    public double getDerivativeOfNetInput(int i)
    {
        return activationFunct.valuePrime(netInput, i);
    }


    public Vector pushNetInputs()
    {
        netInput = partialNetInput;
        partialNetInput = new Vector(dimen);
        netInput.addD(activationBias);

        activation = new Vector(dimen);

        for (int i = 0;i<dimen;i++)
        {
            activation.setValue(activationFunct.value(netInput, i) ,i);
        }
        return activation;
    }

    public Vector pushNetInputs(Vector net)
    {
        netInput = net;
        partialNetInput = new Vector(dimen);
        netInput.addD(activationBias);

        activation.mapD(new VectorMapper() {
            @Override
            public double map(double v, int i)
            {
                return activationFunct.value(netInput, i);
            }
        });


        return activation;
    }

    public Link getOutputTargetLinks()
    {
        Link out = null;

        for (Link l:targetLinks)
        {
            if (l.getTargetNodes().isOutputLayer())
                return l;
        }
        return out;
    }

    public String toString()
    {
        return "net_i: " + netInput.toString() + ", f(net_i): " + activation.toString();
    }

    public NodeGroup setActivation(Vector value)
    {
        activation = value;
        return this;
    }
}
