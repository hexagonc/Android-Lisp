package com.evolved.automata;

/**
 * Created by Evolved8 on 12/19/16.
 */
public interface IndexedValueMapper <I, O> {
    public O map(I input, int index);

    /**
     * Return an array of values to use when mapping an empty input, usually this should be
     * new O[0]
     * @return
     */
    public O[] getEmptyOutput();
}
