package com.evolved.automata.nn;
import com.evolved.automata.AITools;
import com.evolved.automata.IndexedValueMapper;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.Ignore;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import static org.junit.Assert.assertTrue;
/**
 * Created by Evolved8 on 12/27/16.
 */
public class LinkedLSTMSetTester extends BaseLSTMTester {

    int numInputNodes = 6;
    int numMemoryNodes = 10;
    int[] testInput =  {59, 55, 117, 255, 255, 42, 84, 43, 255, 45, 39, 42, 255, 55, 57, 255,
            43, 42, 109, 70, 42, 115, 43, 43, 255, 255, 44, 46, 255, 255, 85, 255, 43, 44, 87, 255, 88, 120, 44,
            43, 98, 84, 85, 255, 45, 43, 255, 66, 48, 255, 255, 66, 43, 255, 48, 255, 255, 66, 44, 255, 70, 255, 255,
            66, 45, 46, 255, 255, 255, 255, 57, 48,59, 55, 117, 255, 255, 42, 84, 43, 255, 45, 39, 42, 255, 55, 57, 255,
            43, 42, 109, 70, 42, 115, 43, 43, 39, 93, 255, 41, 50, 54, 40, 71, 255, 41, 51, 255, 39, 69, 255, 42,
            50, 50, 39, 70, 255, 43, 50, 51};

    String errorMessage;

    @Test
    public void testCreateSetLSTM()
    {
        errorMessage = "Failed to create SetLSTM without exceptions";
        int inputBufferBits = 1;
        boolean success = false;
        try
        {
            LinkedLSTMSet.Builder builder = LinkedLSTMSet.getBuider();
            LinkedLSTMSet llstm = builder.
                    setLSTMBufferSize(15).
                    setMaxLearningSteps(150).
                    setMaxConsecutiveFailures(2).
                    setNumInputNodes(6).
                    setSimpleMaxPredictionAggregator().
                    setLSTMBufferSize(100).
                    setPreventFailingLSTMsToPredict().
                    setCustomVectorViewer(getNumericVectorViewer(255)).
                    build();

            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        assertTrue(errorMessage, success);

    }

    @Test
    public void testSaveLongListToSet()
    {
        errorMessage = "Failed to create SetLSTM without exceptions";
        int inputBufferBits = 1;
        boolean success = false;
        try
        {
            LinkedLSTMSet.Builder builder = LinkedLSTMSet.getBuider();
            LinkedLSTMSet llstm = builder.
                    setNumMemoryCellNodes(10).
                    setNumInputNodes(6).
                    setLSTMBufferSize(20).
                    setMaxLearningSteps(150).
                    setMaxConsecutiveFailures(2).
                    setCustomVectorViewer(getNumericVectorViewer(255)).
                    setSimpleMaxPredictionAggregator().

                    build();


            for (int i = 0;i < testInput.length;i++)
            {
                llstm.observePredictNext(discretizeInStages(testInput[i]), true);
            }

            llstm.resetPredictions();
            String rep = llstm.viewListStructure(true, true);
            System.out.println("Input: " + Arrays.toString(testInput));
            System.out.println("Saved form: " + rep);


            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        assertTrue(errorMessage, success);

    }


    @Test
    public void testMidSequenceContinuation()
    {
        errorMessage = "Failed to create SetLSTM without exceptions";
        int inputBufferBits = 1;
        boolean success = false;
        try
        {
            LinkedLSTMSet.Builder builder = LinkedLSTMSet.getBuider();
            LinkedLSTMSet llstm = builder.
                    setNumMemoryCellNodes(10).
                    setNumInputNodes(6).
                    setLSTMBufferSize(25).
                    setMaxLearningSteps(150).
                    setMaxConsecutiveFailures(2).
                    setCustomVectorViewer(getNumericVectorViewer(255)).
                    setSimpleMaxPredictionAggregator().

                    build();


            for (int i = 0;i < testInput.length;i++)
            {
                llstm.observePredictNext(discretizeInStages(testInput[i]), true);
            }
            llstm.resetPredictions();
            String rep = llstm.viewListStructure(true, true);
            System.out.println("Input: " + Arrays.toString(testInput));
            System.out.println("Saved form: " + rep);
            HashMap<Integer, HashMap<Integer, Integer>> children = llstm.getChildMap();

            System.out.println("Child map: " + children);

            int startIndex = 7;

            Vector[] best = null;
            Double[] basePredictions = null;
            Vector aggregatePrediction = null;

            for (int i = startIndex + 1;i < testInput.length;i++)
            {
                System.out.print("Expected: " + testInput[i]);
                best = llstm.observePredictNext(discretizeInStages(testInput[i-1]), false);
                if (best != null)
                {
                    basePredictions = NNTools.getNumericValueOutput(best, 255);
                    aggregatePrediction = llstm.getBestPrediction(best);
                    System.out.println(" predicts: " + NNTools.getNumericValueOutput(aggregatePrediction, 255) + " from " + Arrays.toString(basePredictions));

                }
                else
                {
                    System.out.println(" no prediction");
                }


            }

            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        assertTrue(errorMessage, success);

    }

    @Test
    public void testSemiContinuousMidSequenceContinuationList()
    {
        errorMessage = "Failed to create ListLSTM without exceptions";
        int inputBufferBits = 1;
        boolean success = false;
        try
        {
            LinkedLSTMSet.Builder builder = LinkedLSTMSet.getBuider();
            LinkedLSTMSet llstm = builder.
                    setNumMemoryCellNodes(10).
                    setNumInputNodes(6).
                    setLSTMBufferSize(10).
                    setMaxLearningSteps(150).
                    setMaxConsecutiveFailures(2).
                    setCustomVectorViewer(getNumericVectorViewer(255)).
                    setSimpleMaxPredictionAggregator().
                    setAllowFailingLSTMsToPredict().

                    build();


            for (int i = 0;i < testInput.length;i++)
            {
                llstm.observePredictNext(discretizeInStages(testInput[i]), true);
            }
            llstm.resetPredictions();
            String rep = llstm.viewListStructure(true, true);
            System.out.println("Input: " + Arrays.toString(testInput));
            System.out.println("Saved form: " + rep);
            HashMap<Integer, HashMap<Integer, Integer>> children = llstm.getChildMap();

            System.out.println("Child map: " + children);

            Vector[] best = null;
            Double[] basePredictions = null;
            Vector aggregatePrediction = null;
            int length = testInput.length;

            int startIndex = (int)(Math.random()*length);
            System.out.println("Mid sequence matching starting at: " + startIndex);
            for (int i = startIndex+1;i < length;i++)
            {
                best = llstm.observePredictNext(discretizeInStages(testInput[i-1]), false);
                if (best != null)
                {
                    basePredictions = NNTools.getNumericValueOutput(best, 255);
                    if (basePredictions == null)
                        System.out.println("null basePredictions");
                    aggregatePrediction = llstm.getBestPrediction(best);
                    if (aggregatePrediction == null)
                        System.out.println("null aggregatePrediction");
                    System.out.println("Actual : " + testInput[i] + " prediction: " + NNTools.getNumericValueOutput(aggregatePrediction, 255) + " from " + Arrays.toString(basePredictions));

                }
                else
                {
                    System.out.println("Actual : " + testInput[i] + " no prediction");
                }


            }

            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        assertTrue(errorMessage, success);

    }

    @Test
    public void testSerializeDeserializeLongList()
    {
        errorMessage = "Failed to create SetLSTM without exceptions";
        int inputBufferBits = 1;
        boolean success = false;
        try
        {



            LinkedLSTMSet.Builder builder = LinkedLSTMSet.getBuider();
            LinkedLSTMSet llstm = builder.
                    setNumMemoryCellNodes(10).
                    setNumInputNodes(6).
                    setLSTMBufferSize(20).
                    setMaxLearningSteps(150).
                    setMaxConsecutiveFailures(2).
                    setCustomVectorViewer(getNumericVectorViewer(255)).
                    setSimpleMaxPredictionAggregator().

                    build();


            for (int i = 0;i < testInput.length;i++)
            {
                llstm.observePredictNext(discretizeInStages(testInput[i]), true);
            }

            llstm.resetPredictions();
            String rep = llstm.viewListStructure(true, true);
            System.out.println("Input: " + Arrays.toString(testInput));
            System.out.println("Saved form: " + rep);
            System.out.println("Saved children: " + llstm.getChildFrequency());



            String serializedForm = llstm.serializedForm();
            LinkedLSTMSet copy = builder.build();
            copy.loadData(serializedForm);
            copy.resetPredictions();
            rep = copy.viewListStructure(true, true);
            System.out.println("Copied form: " + rep);
            System.out.println("Saved children: " + copy.getChildFrequency());



            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        assertTrue(errorMessage, success);

    }



    @Test
    public void testSemiContinuousMidSequenceContinuationSpecialCaseList()
    {
        errorMessage = "Failed to create ListLSTM without exceptions";
        int inputBufferBits = 1;
        boolean success = false;
        try
        {
            LinkedLSTMSet.Builder builder = LinkedLSTMSet.getBuider();
            LinkedLSTMSet llstm = builder.
                    setNumMemoryCellNodes(10).
                    setNumInputNodes(6).
                    setLSTMBufferSize(20).
                    setMaxLearningSteps(150).
                    setMaxConsecutiveFailures(2).
                    setCustomVectorViewer(getNumericVectorViewer(255)).
                    setSimpleMaxPredictionAggregator().
                    setAllowFailingLSTMsToPredict().

                    build();


            for (int i = 0;i < testInput.length;i++)
            {
                llstm.observePredictNext(discretizeInStages(testInput[i]), true);
            }
            llstm.resetPredictions();
            String rep = llstm.viewListStructure(true, true);
            System.out.println("Input: " + Arrays.toString(testInput));
            System.out.println("Saved form: " + rep);



            Vector[] best = null;
            Double[] basePredictions = null;
            Vector aggregatePrediction = null;
            int length = testInput.length;

            int startIndex = 0;
            System.out.println("Mid sequence matching starting at: " + startIndex);
            for (int i = startIndex+1;i < length;i++)
            {
                best = llstm.observePredictNext(discretizeInStages(testInput[i-1]), false);
                if (best != null)
                {
                    basePredictions = NNTools.getNumericValueOutput(best, 255);
                    aggregatePrediction = llstm.getBestPrediction(best);
                    System.out.println("Actual : " + testInput[i] + " prediction: " + NNTools.getNumericValueOutput(aggregatePrediction, 255) + " from " + Arrays.toString(basePredictions));

                }
                else
                {
                    System.out.println("Actual : " + testInput[i] + " no prediction");
                }


            }

            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        assertTrue(errorMessage, success);

    }



    Vector discretizeInStages(int num)
    {
        return NNTools.getVector(NNTools.stageDiscretize(num, 255, 3));
    }

}
