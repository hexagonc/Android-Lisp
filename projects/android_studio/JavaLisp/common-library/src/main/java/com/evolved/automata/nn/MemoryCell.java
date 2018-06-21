package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public class MemoryCell {
    int stateDimen;

    public static final String NODE_GROUP_SEPARATOR = "-";
    Vector state;
    CellInputLayer cellInput;
    CellOutputLayer cellOutput;
    ForgetGateLayer forgetGate;
    InputGateLayer inputGate;
    OutputGateLayer outputGate;
    boolean usingPeepHolesP = false;
    NodeGroup peepHole;

    public MemoryCell(int stateSize)
    {
        stateDimen = stateSize;
        cellInput = new CellInputLayer(this);
        cellOutput = new CellOutputLayer(this);
        forgetGate = new ForgetGateLayer(this);
        inputGate = new InputGateLayer(this);
        outputGate = new OutputGateLayer(this);
        state = new Vector(stateSize);

        peepHole = new NodeGroup(stateSize, new IdentityActivation(), state)
        {
            @Override
            public NodeGroup setActivation(Vector value)
            {
                state = value;
                return this;
            }

            @Override
            public Vector getActivation()
            {
                return state;
            }
        };
    }

    public void reset()
    {
        state.setValue(0);
        cellInput.reset();;
        cellOutput.reset();;
        forgetGate.reset();
        inputGate.reset();
        outputGate.reset();
    }


    public double getOutputLinkError(int i)
    {
        Link outputLinks = cellOutput.getOutputTargetLinks();
        if (outputLinks == null)
        {
            return 0;
        }

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


    public boolean isUsingPeepHoles()
    {
        return usingPeepHolesP;
    }

    public double squashedState(double value)
    {
        return Math.tanh(value);
    }

    public double squashedStatePrime(double value)
    {
        return 1/Math.cosh(value)/Math.cosh(value);
    }

    public double getSquashedState(int i)
    {
        return squashedState(state.value(i));
    }

    public double getSquashedStatePrime(int i)
    {
        return squashedStatePrime(state.value(i));
    }

    void updateState()
    {

        state.mapD(new VectorMapper() {
            @Override
            public double map(double s, int i)
            {
                return s*forgetGate.getActivation().value(i) + cellInput.getActivation().value(i)*inputGate.getActivation().value(i);
            }
        });
    }

    public int getStateDimen()
    {
        return stateDimen;
    }

    public CellOutputLayer getCellOutput()
    {
        return cellOutput;
    }

    public CellInputLayer getCellInput()
    {
        return cellInput;
    }

    public InputGateLayer getInputGate()
    {
        return inputGate;
    }

    public OutputGateLayer getOutputGate()
    {
        return outputGate;
    }

    public ForgetGateLayer getForgetGate()
    {
        return forgetGate;
    }

    public NodeGroup getPeepHole()
    {
        return peepHole;
    }


    public Vector getCellState()
    {
        return state;
    }
}
