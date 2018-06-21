package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 12/11/16.
 */
public interface WeightMatrixPredicate {
    boolean trueOfCell(double c_i_j, int i, int j);
}
