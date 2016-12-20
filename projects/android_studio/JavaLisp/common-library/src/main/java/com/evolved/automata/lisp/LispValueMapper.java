package com.evolved.automata.lisp;

import com.evolved.automata.IndexedValueMapper;

/**
 * Created by Evolved8 on 12/20/16.
 */
public abstract class LispValueMapper <O> implements IndexedValueMapper<O, Value> {

    public Value[] getEmptyOutput()
    {
        return new Value[0];
    }
}
