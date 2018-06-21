package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public class OutputGateLayer extends MemoryCellNodeLayer {

    public OutputGateLayer(MemoryCell memory )
    {
        super(memory);
        layerType = NetConfiguration.LayerType.OUTPUTGATE;
    }


    @Override
    public void reset()
    {
        activation.setValue(1);
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

        CellOutputLayer celloutput = memoryCell.getCellOutput();
        celloutput.updateCellOutput();
        return activation;
    }

    public Vector getErrorResponsibility()
    {
        return netInput.map(new VectorMapper() {
            @Override
            public double map(double v, int i)
            {
                return activationFunct.valuePrime(netInput, i) * memoryCell.getSquashedState(i) * memoryCell.getOutputLinkError(i);
            }
        });
    }

    public String getCellKey(String memoryCellName)
    {
        return memoryCellName + MemoryCell.NODE_GROUP_SEPARATOR + "OG";
    }


}
