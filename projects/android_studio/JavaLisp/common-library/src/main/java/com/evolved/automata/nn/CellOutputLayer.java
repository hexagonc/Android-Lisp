package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public class CellOutputLayer extends MemoryCellNodeLayer {

    public CellOutputLayer(MemoryCell memory )
    {
        super(memory);
        layerType = NetConfiguration.LayerType.CELLOUTPUT;
    }

    public void updateCellOutput()
    {
        final OutputGateLayer outputGate = memoryCell.getOutputGate();
        activation.mapD(new VectorMapper() {
            @Override
            public double map(double v, int i)
            {
                return memoryCell.squashedState(i)*outputGate.getActivation().value(i);
            }
        });
    }

    @Override
    public void reset()
    {
        pushNetInputs(new Vector(dimen));
    }

    public String getCellKey(String memoryCellName)
    {
        return memoryCellName + MemoryCell.NODE_GROUP_SEPARATOR + "CO";
    }

}
