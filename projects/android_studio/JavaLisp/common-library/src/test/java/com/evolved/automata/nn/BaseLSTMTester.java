package com.evolved.automata.nn;

import com.evolved.automata.AITools;
import com.evolved.automata.ArrayMapper;

import org.apache.commons.lang3.tuple.Pair;

/**
 * Created by Evolved8 on 1/8/17.
 */
public class BaseLSTMTester {




    VectorViewer getNumericVectorViewer(final int range)
    {
        return new VectorViewer() {
            @Override
            public String toString(Vector v)
            {
                if (v == null)
                    return "null";
                else
                    return "" + NNTools.getNumericValueOutput(v, range);
            }
        };
    }

    String viewVectorList(final Vector[] v, final VectorViewer viewer)
    {
        final StringBuilder s = new StringBuilder();
        for (Vector item: v)
        {
            s.append(viewer.toString(item));
            s.append("\n");
        }
        return s.toString();
    }



    Vector[] getNeuralNetworkInputRepresentation(int[] rawInput,  int range, int bitResolution)
    {
        return getNeuralNetworkInputRepresentation(boxArray(rawInput), range, bitResolution);
    }


    Vector[] getNeuralNetworkInputRepresentation(Integer[] numericInput, final int range, final int bitResolution)
    {
        return AITools.mapValues(numericInput, new VectorValueMapper<Integer>() {
            @Override
            public Vector map(Integer input, int index)
            {
                return getNeuralNetworkInputRepresentation(input, range, bitResolution);
            }
        });
    }

    Vector getNeuralNetworkInputRepresentation(Integer numericInput, final int range, final int bitResolution)
    {
        return NNTools.getVector(NNTools.stageDiscretize(numericInput.intValue(), range, bitResolution));
    }

    Vector getNeuralNetworkInputRepresentation(int numericInput, final int range, final int bitResolution)
    {
        return NNTools.getVector(NNTools.stageDiscretize(numericInput, range, bitResolution));
    }


    Double[] boxArray(double[] d)
    {
        Double[] o = new Double[d.length];
        for (int i = 0;i<o.length;i++)
        {
            o[i] = Double.valueOf(d[i]);
        }
        return o;
    }

    Integer[] boxArray(int[] d)
    {
        Integer[] o = new Integer[d.length];
        for (int i = 0;i<o.length;i++)
        {
            o[i] = Integer.valueOf(d[i]);
        }
        return o;
    }


}
