package com.evolved.automata.nn;

import com.evolved.automata.AITools;

import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertTrue;

/**
 * Created by Evolved8 on 1/8/17.
 */
public class ListLSTMTester extends BaseLSTMTester {


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
    public void testCreateLSTM()
    {
        errorMessage = "Failed to create ListLSTM without exceptions";
        int inputBufferBits = 1;
        boolean success = false;
        try
        {
            LinkedLSTM.Builder builder = LinkedLSTM.getLinkedLSTMBuider();
            LinkedLSTM llstm = builder.
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
    public void testSaveLongList()
    {
        errorMessage = "Failed to create ListLSTM without exceptions";
        int inputBufferBits = 1;
        boolean success = false;
        try
        {
            LinkedLSTM.Builder builder = LinkedLSTM.getLinkedLSTMBuider();
            LinkedLSTM llstm = builder.
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

            String rep = llstm.viewListStructure(true, true);
            System.out.println("Input: " + Arrays.toString(testInput));
            System.out.println("Saved form: " + rep);

            Vector[] items = llstm.getSequence();
            System.out.println("Saved list: " + Arrays.toString(NNTools.getNumericValueOutput(items, 255)));
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
        errorMessage = "Failed to serialize/deserialize ListLSTM without exceptions";
        int inputBufferBits = 1;
        boolean success = false;
        try
        {
            LinkedLSTM.Builder builder = LinkedLSTM.getLinkedLSTMBuider();
            LinkedLSTM llstm = builder.
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

            String rep = llstm.viewListStructure(true, true);
            System.out.println("Input: " + Arrays.toString(testInput));
            System.out.println("Saved form: " + rep);

            Vector[] items = llstm.getSequence();
            System.out.println("Saved list: " + Arrays.toString(NNTools.getNumericValueOutput(items, 255)));

            String serializedForm = llstm.serializedForm();
            LinkedLSTM copy = builder.build();
            copy.loadData(serializedForm);
            copy.resetPredictions();
            rep = copy.viewListStructure(true, true);
            System.out.println("Copied form: " + rep);



            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        assertTrue(errorMessage, success);

    }


    @Test
    public void testMidSeriesSerializeDeserializeLongList()
    {
        errorMessage = "Failed to serialize/deserialize ListLSTM mid sequence without exceptions";
        int inputBufferBits = 1;
        boolean success = false;
        try
        {
            LinkedLSTM.Builder builder = LinkedLSTM.getLinkedLSTMBuider();
            LinkedLSTM llstm = builder.
                    setNumMemoryCellNodes(10).
                    setNumInputNodes(6).
                    setLSTMBufferSize(18).
                    setMaxLearningSteps(150).
                    setMaxConsecutiveFailures(2).
                    setCustomVectorViewer(getNumericVectorViewer(255)).
                    setSimpleMaxPredictionAggregator().

                    build();


            int midSequence = (int)(Math.random()* testInput.length);
            LinkedLSTM copy = builder.build();
            String serialized;
            for (int i = 0;i < testInput.length;i++)
            {

                if (i < midSequence)
                {

                    llstm.observePredictNext(discretizeInStages(testInput[i]), true);
                }
                else if (i  ==  midSequence)
                {
                    serialized = llstm.serializedForm();
                    copy.loadData(serialized);
                    copy.observePredictNext(discretizeInStages(testInput[i]), true);
                }
                else
                {
                    copy.observePredictNext(discretizeInStages(testInput[i]), true);
                }
            }

            String rep = copy.viewListStructure(true, true);
            System.out.println("Input: " + Arrays.toString(testInput));
            System.out.println("Saved form: " + rep);

            Vector[] items = llstm.getSequence();
            Vector[] citems = copy.getSequence();

            System.out.println("Saved head of list: " + Arrays.toString(NNTools.getNumericValueOutput(items, 255)));
            System.out.println("Saved total from copy of list: " + Arrays.toString(NNTools.getNumericValueOutput(citems, 255)));




            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        assertTrue(errorMessage, success);

    }

    @Test
    public void testJoiningLongLists()
    {
        errorMessage = "Failed to serialize/deserialize ListLSTM mid sequence without exceptions";
        int inputBufferBits = 1;
        boolean success = false;
        try
        {
            LinkedLSTM.Builder builder = LinkedLSTM.getLinkedLSTMBuider();
            LinkedLSTM head = builder.
                    setNumMemoryCellNodes(10).
                    setNumInputNodes(6).
                    setLSTMBufferSize(18).
                    setMaxLearningSteps(150).
                    setMaxConsecutiveFailures(2).
                    setCustomVectorViewer(getNumericVectorViewer(255)).
                    setSimpleMaxPredictionAggregator().

                    build();


            int midSequence = (int)(Math.random()* testInput.length);
            LinkedLSTM tail = builder.build();
            String serialized;
            for (int i = 0;i < testInput.length;i++)
            {

                if (i < midSequence)
                {

                    head.observePredictNext(discretizeInStages(testInput[i]), true);
                }
                else
                {
                    tail.observePredictNext(discretizeInStages(testInput[i]), true);
                }

            }

            String rep = head.viewListStructure(true, true);
            System.out.println("Input: " + Arrays.toString(testInput));
            System.out.println("Head Saved form: " + rep);

            rep = tail.viewListStructure(true, true);
            System.out.println("Tail Saved form: " + rep);

            Vector[] items = head.getSequence();
            Vector[] citems = tail.getSequence();

            System.out.println("Saved head of list: " + Arrays.toString(NNTools.getNumericValueOutput(items, 255)));
            System.out.println("Saved tail of list: " + Arrays.toString(NNTools.getNumericValueOutput(citems, 255)));

            head.join(tail);
            rep = head.viewListStructure(true, true);
            System.out.println("Joined Saved form: " + rep);

            Vector[] jitems = head.getSequence();
            System.out.println("Saved jonined list: " + Arrays.toString(NNTools.getNumericValueOutput(jitems, 255)));



            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        assertTrue(errorMessage, success);

    }


    @Test
    public void testMidSequenceContinuationList()
    {
        errorMessage = "Failed to create ListLSTM without exceptions";
        int inputBufferBits = 1;
        boolean success = false;
        try
        {
            LinkedLSTM.Builder builder = LinkedLSTM.getLinkedLSTMBuider();
            LinkedLSTM llstm = builder.
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

            String rep = llstm.viewListStructure(true, true);
            System.out.println("Input: " + Arrays.toString(testInput));
            System.out.println("Saved form: " + rep);

            Vector[] items = llstm.getSequence();
            System.out.println("Saved list: " + Arrays.toString(NNTools.getNumericValueOutput(items, 255)));

            int startIndex = 7;
            llstm.resetPredictions();
            Vector[] best = null;
            Double[] basePredictions = null;
            Vector aggregatePrediction = null;

            for (int i = startIndex;i < testInput.length;i++)
            {
                best = llstm.observePredictNext(discretizeInStages(testInput[i]), false);
                if (best != null)
                {
                    basePredictions = NNTools.getNumericValueOutput(best, 255);
                    aggregatePrediction = llstm.getBestPrediction(best);
                    System.out.println("Input: " + testInput[i] + " predicts: " + NNTools.getNumericValueOutput(aggregatePrediction, 255) + " from " + Arrays.toString(basePredictions));

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
    public void testSemiContinuousMidSequenceContinuationList()
    {
        errorMessage = "Failed to create ListLSTM without exceptions";
        int inputBufferBits = 1;
        boolean success = false;
        try
        {
            LinkedLSTM.Builder builder = LinkedLSTM.getLinkedLSTMBuider();
            LinkedLSTM llstm = builder.
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

            String rep = llstm.viewListStructure(true, true);
            System.out.println("Input: " + Arrays.toString(testInput));
            System.out.println("Saved form: " + rep);

            Vector[] items = llstm.getSequence();
            System.out.println("Saved list: " + Arrays.toString(NNTools.getNumericValueOutput(items, 255)));


            llstm.resetPredictions();
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


    @Test
    public void testSemiContinuousMidSequenceContinuationSpecialCaseList()
    {
        errorMessage = "Failed to create ListLSTM without exceptions";
        int inputBufferBits = 1;
        boolean success = false;
        try
        {
            LinkedLSTM.Builder builder = LinkedLSTM.getLinkedLSTMBuider();
            LinkedLSTM llstm = builder.
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

            String rep = llstm.viewListStructure(true, true);
            System.out.println("Input: " + Arrays.toString(testInput));
            System.out.println("Saved form: " + rep);

            Vector[] items = llstm.getSequence();
            System.out.println("Saved list: " + Arrays.toString(NNTools.getNumericValueOutput(items, 255)));


            llstm.resetPredictions();
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
