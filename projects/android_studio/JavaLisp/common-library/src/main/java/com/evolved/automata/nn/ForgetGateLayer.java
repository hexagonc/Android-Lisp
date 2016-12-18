package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public class ForgetGateLayer extends MemoryCellNodeLayer {

    public ForgetGateLayer(MemoryCell memory )
    {
        super(memory);
        layerType = NetConfiguration.LayerType.FORGETGATE;
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
        return memoryCellName + MemoryCell.NODE_GROUP_SEPARATOR + "FG";
    }

}

