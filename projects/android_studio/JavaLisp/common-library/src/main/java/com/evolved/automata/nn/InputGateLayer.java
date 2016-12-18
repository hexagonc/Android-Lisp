package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public class InputGateLayer extends MemoryCellNodeLayer {

    public InputGateLayer(MemoryCell memory )
    {
        super(memory);
        layerType = NetConfiguration.LayerType.INPUTGATE;
    }

    @Override
    public void reset()
    {
        activation.setValue(1);
    }


    @Override
    public Vector getErrorResponsibility()
    {
        OutputGateLayer output = memoryCell.getOutputGate();
        return output.getActivation().map(new VectorMapper() {
            @Override
            public double map(double w, int i)
            {
                return w * memoryCell.getSquashedStatePrime(i) * memoryCell.getOutputLinkError(i);
            }
        });
    }

    public String getCellKey(String memoryCellName)
    {
        return memoryCellName + MemoryCell.NODE_GROUP_SEPARATOR + "IG";
    }

    @Override
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
        memoryCell.updateState();
        return activation;
    }

}
