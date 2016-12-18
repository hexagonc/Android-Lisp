package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/8/16.
 */
public abstract class MemoryCellNodeLayer extends NodeGroup {
    MemoryCell memoryCell;


    public MemoryCellNodeLayer(MemoryCell cell)
    {
        super(cell.getStateDimen(), new SigmoidActivation());
        memoryCell = cell;
    }
    public MemoryCell getMemoryCell()
    {
        return memoryCell;
    }

    public abstract void reset();

    public abstract String getCellKey(String memoryCellName);
}
