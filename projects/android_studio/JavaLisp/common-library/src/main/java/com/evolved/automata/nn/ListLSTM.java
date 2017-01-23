package com.evolved.automata.nn;

import com.evolved.automata.AITools;
import com.evolved.automata.WeightedValue;

import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 12/26/16.
 */
public class ListLSTM {
    int bufferSize;
    HashtableLSTM[] group;
    int bufferIndex = 0;
    int _inputNodeCount;
    int _memoryCellCount;
    boolean allowRecycleBufferIndicesP = false;

    String savedWeights;
    String savedStates;
    boolean rolloverP = false;
    int totalItems = 0;
    int bufferBits = 8;

    int nextInsertedLSTMIndex = 1;

    int[] baseMatchCounts;
    int[] currentMatchCounts;
    long[] patternAge;

    Vector[] prevPredictions;
    Vector[] currentPredictions;
    boolean canIgnoreNext = false;
    HashtableLSTM.LSTMNetworkBuilder builder = null;

    int numSegments = 0;
    int _START_KEY = 0;
    HashtableLSTM currentLSTM = null;

    double maxError = 0.1;
    int maxSteps = 200;
    public ListLSTM(int inputNodeCount, int stateNodeCount, int bufferBits)
    {
        this.bufferBits = bufferBits;
        bufferSize = (int)Math.pow(2, bufferBits);
        baseMatchCounts = new int[bufferSize];
        currentMatchCounts = new int[bufferSize];
        group = new HashtableLSTM[bufferSize];
        patternAge = new long[bufferSize];
        _inputNodeCount = inputNodeCount;
        _memoryCellCount = stateNodeCount;


        baseMatchCounts[0] = 1;
        nextInsertedLSTMIndex = getNextInsertIndex();
        builder = getBaseBuilder(inputNodeCount, stateNodeCount, bufferBits, nextInsertedLSTMIndex);



    }

    int getNextInsertIndex()
    {
        int selectedIndex = _START_KEY;
        int smallestMatchCount = Integer.MAX_VALUE;

        for (int i = 0;i < bufferSize;i++)
        {
            smallestMatchCount = Math.min(baseMatchCounts[i], smallestMatchCount);

        }
        for (int i = 0;i < bufferSize;i++)
        {
            if (baseMatchCounts[i] == smallestMatchCount)
            {
                selectedIndex = i;
                break;
            }
        }

        if (group[selectedIndex] != null)
        {
            if (!allowRecycleBufferIndicesP)
                return -1;
            group[selectedIndex].deleteKey(_START_KEY); // This basically resets the LSTM


        }
        patternAge[selectedIndex] = System.currentTimeMillis();

        return selectedIndex;
    }

    private HashtableLSTM.LSTMNetworkBuilder getBaseBuilder(int inputNodeCount, int memoryCellNodeCount, int keyWidth, int nextDelimter)
    {
        HashtableLSTM.LSTMNetworkBuilder builder = HashtableLSTM.getHashtableLSTMBuilder();
        builder.setInputNodeCount(inputNodeCount).addMemoryCell("M", memoryCellNodeCount);
        builder.setKeyBitWidth(keyWidth);
        builder.setEndDelimiterKey(nextDelimter);
        NNTools.addStandardRPROPWeightUpdatePolicies(builder);
        builder.addWeightUpdateOrder(NNTools.getStandardSingleCellWeightUpdateOrder("M"));
        builder.addFeedForwardLinkOrder(NNTools.getStandardSingleCellFeedforwardOrder("M"));
        NNTools.addNodeConnectivity(builder, NNTools.getStandardLinkConnectivityMap("M"));
        return builder;
    }

    boolean isValidSegmentIndex(int index)
    {
        return index != _START_KEY;
    }

    public Vector[] predictNextInput(Vector currentInput, boolean learnP)
    {
        if (currentLSTM == null)
        {
            currentLSTM = builder.build();

        }

        LinkedList<Pair<Integer, Integer>> bridgedSegments = new LinkedList<Pair<Integer, Integer>>();
        Vector nextPrediction;
        int nextSegmentIndex = 0;
        double[] result = null;
        if (learnP)
        {
            currentLSTM.add(currentInput, _START_KEY, maxSteps, maxError);
        }
        boolean anyPredictionsP = false;
        if (numSegments == 0)
        {
            if (result!=null && result[0]> maxError)
            {
                group[0] = currentLSTM;

                baseMatchCounts[0] = 1;
                currentMatchCounts[0] = -1; // indicates that this pattern cannot match yet
                numSegments++;
                nextSegmentIndex = getNextInsertIndex();
                if (isValidSegmentIndex(nextSegmentIndex))
                {
                    builder.setEndDelimiterKey(nextSegmentIndex);
                    currentLSTM = builder.build();
                    currentLSTM.add(currentInput, _START_KEY, maxSteps, maxError);

                }
            }
            currentPredictions = prevPredictions = null;
            return null;
        }
        else
        {
            if (currentPredictions == null)
            {
                currentPredictions = new Vector[bufferSize];
                for (int i=0;i < numSegments;i++)
                {
                    currentPredictions[i] = group[i].getNextExpectedOutput(true);
                }
            }


            prevPredictions = new Vector[bufferSize];
            for (int i = 0; i < numSegments;i++)
            {
                if (currentMatchCounts[i] == -1)
                {
                    int nextSegment = group[i].getEndDelimiter();
                    bridgedSegments.add(Pair.of(i, nextSegment));
                    continue;
                }
                nextPrediction = null;
                prevPredictions[i] = currentPredictions[i];

                if (currentPredictions[i] != null && group[i].atEndOfListP())
                {
                    int nextSegment = group[i].getEndDelimiter();
                    currentLSTM.deleteKey(_START_KEY);
                    bridgedSegments.add(Pair.of(i, nextSegment));
                    currentPredictions[i] = null;
                    continue;

                }

                // Now assessing how good previous predictions were

                if (currentPredictions[i] == null)
                {
                    group[i].resetToStartOfSequence(_START_KEY);
                    currentPredictions[i] = group[i].getNextExpectedOutput(true);
                }

                if (currentInput.equals(currentPredictions[i]))
                {

                    currentPredictions[i] = group[i].extrapNext(currentInput);
                    currentMatchCounts[i]++;
                }
                else
                {


                    if (canIgnoreNext)
                    {
                        nextPrediction = group[i].extrapNext(currentPredictions[i]);
                    }
                    else
                    {
                        currentMatchCounts[i] = Math.max(0, currentMatchCounts[i] - 1);
                        if (currentMatchCounts[i] > 0)
                        {
                            nextPrediction = group[i].extrapNext(currentInput);
                        }

                    }

                    currentPredictions[i] = nextPrediction;
                }
            }

            // All the completed segments
            for (Pair<Integer, Integer> linkedKeys:bridgedSegments)
            {
                int startSegment = linkedKeys.getLeft();
                int nextSegment = linkedKeys.getRight();


                if (group[nextSegment] == null)
                {

                    if (result!=null && result[0]> maxError)
                    {
                        group[nextSegment] = currentLSTM;

                        baseMatchCounts[nextSegment] = 1;
                        currentMatchCounts[nextSegment] = -1; // indicates that this pattern cannot match yet
                        numSegments++;
                        currentMatchCounts[startSegment] = 0;

                        nextSegmentIndex = getNextInsertIndex();
                        if (isValidSegmentIndex(getNextInsertIndex()))
                        {
                            builder.setEndDelimiterKey(nextSegmentIndex);
                            currentLSTM = builder.build();
                            currentLSTM.add(currentInput, _START_KEY, maxSteps, maxError);
                        }


                    }
                    else
                        currentMatchCounts[startSegment] = -1;
                }
                else
                {
                    if (currentMatchCounts[nextSegment]>0)
                    {
                        currentMatchCounts[nextSegment] += currentMatchCounts[startSegment];
                    }
                    currentMatchCounts[startSegment] = 0;
                }
                baseMatchCounts[startSegment]++;

            }

            return currentPredictions;

        }
    }


    //TODO: assumes that when currentPredictions is null, currentMatchCounts are all 0.  CHECK THIS!!
    /**
     *
     * @return
     */
    public ListLSTM clearPredictions()
    {
        boolean currentPredictionsExist = currentPredictions != null;
        boolean prevPredictionsExist = prevPredictions != null;
        if (currentPredictionsExist)
        {
            if (!prevPredictionsExist)
            {
                prevPredictions = new Vector[bufferSize];
            }
        }
        else
        {
            if (prevPredictionsExist)
                prevPredictions = null;
        }


        for (int i = 0;i<bufferSize;i++)
        {
            if (currentPredictionsExist)
                prevPredictions[i] = currentPredictions[i];
            currentMatchCounts[i] = 0;
        }
        currentPredictions = null;
        return this;
    }

    public Vector[] getCurrentPredictions()
    {
        return currentPredictions;
    }

    public Vector[] getPrevPredictions()
    {
        return prevPredictions;
    }

    public Vector getAggregatePrediction(Vector[] predictions)
    {
        if (predictions == null)
            return null;
        ArrayList<WeightedValue<Vector>> distribution = new ArrayList<WeightedValue<Vector>>();

        for (int i = 0;i < numSegments;i++)
        {
            if (currentMatchCounts[i] > 0 && currentPredictions[i] != null)
            {
                distribution.add( new WeightedValue<Vector>(currentPredictions[i], currentMatchCounts[i]));

            }
        }

        if (distribution.size() > 0)
        {
            WeightedValue<Vector> selected = AITools.ChooseWeightedRandomFair(distribution);
            return selected.GetValue();
        }
        return null;

    }






}
