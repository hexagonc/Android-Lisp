package com.evolved.automata.nn;

import com.evolved.automata.IndexedValueMapper;

/**
 * Created by Evolved8 on 12/20/16.
 */
public abstract class VectorValueMapper <I> implements IndexedValueMapper<I, Vector> {
    @Override
    public Vector[] getEmptyOutput()
    {
        return new Vector[0];
    }
}
