package com.evolved.automata.nn;

import java.util.Arrays;

/**
 * Created by Evolved8 on 12/26/16.
 */
public class ListLSTM {
    int bufferSize;
    SequenceLSTM[] group;
    int bufferIndex = 0;
    int _inputNodeCount;
    int _memoryCellCount;
    double maxError = 0.1;
    int maxSteps = 300;
    String savedWeights;
    String savedStates;
    boolean rolloverP = false;
    int totalItems = 0;


    public ListLSTM(int inputNodeCount, int stateNodeCount, int bufferSize)
    {
        group = new SequenceLSTM[bufferSize];
        _inputNodeCount = inputNodeCount;
        _memoryCellCount = stateNodeCount;
        this.bufferSize = bufferSize;
        group[0] = NNTools.getStandardSequenceLSTM(inputNodeCount, stateNodeCount, null);
        totalItems = 0;
    }

    public void add(Vector v)
    {
        totalItems++;
        double[] errorResponse = group[bufferIndex].add(v,maxSteps, maxError);
        if (errorResponse[0] <= maxError)
        {
            savedWeights = group[bufferIndex].serializeLinkWeights();
            savedStates = group[bufferIndex].serializeNetworkActivationState();
        }
        else
        {
            group[bufferIndex].decodeSerializedLinksToLinkBuffer(savedWeights);
            group[bufferIndex].loadbufferedLinkWeights();
            group[bufferIndex].loadSerializedNetworkActivationState(savedStates);

            SequenceLSTM first = group[0].clear();

            if (bufferIndex == bufferSize - 1)
            {
                rolloverP = true;
                for (int i = 1;i<bufferSize;i++)
                {
                    group[i-1] = group[i];
                }
                group[bufferSize - 1] = first;
                group[bufferIndex].add(v,maxSteps, maxError);
            }
            else
                bufferIndex++;
        }

    }

    public Vector[] getTotalPattern()
    {
        int offset = 0;
        Vector[] out = new Vector[totalItems];
        for (int i = 0;i <=bufferIndex;i++)
        {

            for (Vector v:group[i].getCurrentSequence())
            {
                out[offset] = v;
                offset++;
            }
        }
        return out;
    }


}
