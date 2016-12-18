package com.evolved.automata.nn;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;

import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

/**
 * Created by Evolved8 on 12/17/16.
 */
public class NNToolsTester {

    String failureMessage;
    String successStepMessage;

    @Test
    public void vectorTests()
    {
        int size = 3;
        Vector v = new Vector(size);
        assertTrue("Failed to set correct initial vector size", v.dimen() == size);

        assertTrue("Failed to initialize vector to zero", v.all(makeVectorValueTester(0)));



        final double[] testV = new double[size];
        int i = 0;
        for (i = 0;i<size;i++)
            testV[i] = Math.random()*2000 - 1000;

        v = new Vector(testV);

        assertTrue("Incorrect Vector size initialization with double[]", v.dimen() == size);

        assertTrue("Incorrect Vector value initialization with double[]", v.all(makeVectorValueTester(testV)));




        Vector v2 = new Vector(size);
        v2.mapD(new VectorMapper() {
            @Override
            public double map(double v, int i)
            {
                return testV[i];
            }
        });

        assertTrue("Failure to set vectors as equal: " + v2 + " vs " + v, v2.equals(v));

        // Distinguish destructive vs non-destructive modification

        Vector v3 = v2.map(new VectorMapper() {
            @Override
            public double map(double v, int i)
            {
                return 2*v;
            }
        });

        assertTrue("V2 should be different from v2", v3 != v2);

        assertTrue("Failure at scalar multiplication of Vector", v2.multiply(2).equals(v3));


        final double[] result = new double[size];

        Vector f1 = new Vector(size), f2 = new Vector(size);

        for (i = 0;i<size;i++)
        {
            f1.setValue(Math.random()*200 - 100, i);
            f2.setValue(Math.random()*200 - 100, i);

            result[i] = f1.value(i)*f2.value(i);
        }

        assertTrue("Failure to multiply vectors", f1.multiply(f2).all(makeVectorValueTester(result)));

    }

    @Test
    public void matrixTests()
    {


        int targetDimen = 2, sourceDimen = 2;
        WeightMatrix matrix = new WeightMatrix(targetDimen, sourceDimen);

        assertTrue("Weight matrix should be initially all zero", matrix.all(makeMatrixValueTester(0)));

        WeightMatrix offset = new WeightMatrix(targetDimen, sourceDimen);

        offset.mapD(new MatrixMapper() {
            @Override
            public double map(double c, int i, int j)
            {
                return i * j;
            }
        });

        assertTrue("Failure to test equality",
                matrix.map(new MatrixMapper() {
                    @Override
                    public double map(double c, int i, int j)
                    {
                        return i * j;
                    }
                }).equals(offset));

        assertTrue("Failure to non-destructively modify matrix", !offset.equals(matrix));

        assertTrue("Failed to add matrices",
                matrix.add(offset).equals(offset));

        Vector source = new Vector(new double[]{4,5});

        final double[][] weights = new double[][]{{0,1}, {1, 0}};

        WeightMatrix swap = new WeightMatrix(weights);

        assertTrue("Failed to initialize matrix to double[][]", swap.all(makeMatrixValueTester(weights)));

        Vector product = new Vector(new double[]{5,4});

        assertTrue("Failued to multiply matrix by vector", product.equals(swap.multiply(source)));

    }

    @Test
    public void testDiscretization()
    {

        try
        {
            // Discretization constant parameters
            int min = 0, max = 100;

            int trialCount = 1000;
            ArrayList<Double> discretized = null;
            int sample = 0;

            int minSteps = 1, maxSteps = 4, steps; // variable discretization parameter
            double expectedError;
            Pair<Double, Double> discretizedInterval;
            double resampledValue = sample;
            for (int i = 0;i<trialCount;i++)
            {
                sample = (int)(min + (max - min)*Math.random());
                steps = (int)(minSteps + (maxSteps - minSteps+1)*Math.random());
                expectedError = (max - min)/Math.pow(2, 2*steps);

                discretized = NNTools.stageDiscretizeAroundPivot(sample, min, max, steps);

                discretizedInterval = NNTools.stageContinuize(min, max, discretized);

                resampledValue = (discretizedInterval.getRight() + discretizedInterval.getLeft())/2;
                System.out.println("Input: " + sample + " range: " + discretizedInterval.toString() + " Steps: " + steps + " vector: " + Arrays.toString(discretized.toArray(new Double[0])));
                assertTrue("Retrieved value outside of range of expected!  Value: " + sample + " Generated: " + resampledValue + " max difference: " + expectedError, Math.abs(resampledValue - sample) < expectedError);

            }
        }
        catch (Exception e)
        {
            e.printStackTrace();
            assertTrue("Exception: " + e.toString(), false);
        }


    }

    @Test
    public void testDiscretizationCase1()
    {

        try
        {
            // Discretization constant parameters
            int min = 0, max = 100;

            int trialCount = 100;
            ArrayList<Double> discretized = null;
            int sample = 0;

            int minSteps = 1, maxSteps = 4, steps; // variable discretization parameter
            double expectedError;
            Pair<Double, Double> discretizedInterval;
            double resampledValue = sample;
            sample = 70;
            steps = 1;
            expectedError = (max - min)/Math.pow(2, 2 + steps);

            discretized = NNTools.stageDiscretizeAroundPivot(sample, min, max, steps);

            discretizedInterval = NNTools.stageContinuize(min, max, discretized);

            resampledValue = (discretizedInterval.getRight() + discretizedInterval.getLeft())/2;
            System.out.println("Input: " + sample + " range: " + discretizedInterval.toString() + " Steps: " + steps);
            assertTrue("Retrieved value outside of range of expected!  Value: " + sample + " Generated: " + resampledValue + " max difference: " + expectedError, Math.abs(resampledValue - sample) < expectedError);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            assertTrue("Exception: " + e.toString(), false);
        }

    }

    @Test
    public void testDiscretizationCase2()
    {

        try
        {
            // Discretization constant parameters
            int min = 0, max = 100;

            int trialCount = 100;
            ArrayList<Double> discretized = null;
            int sample = 0;

            int minSteps = 1, maxSteps = 4, steps; // variable discretization parameter
            double expectedError;
            Pair<Double, Double> discretizedInterval;
            double resampledValue = sample;
            sample = 59;
            steps = 4;
            expectedError = (max - min)/Math.pow(2, 1 + steps);

            discretized = NNTools.stageDiscretizeAroundPivot(sample, min, max, steps);

            discretizedInterval = NNTools.stageContinuize(min, max, discretized);

            resampledValue = (discretizedInterval.getRight() + discretizedInterval.getLeft())/2;
            System.out.println("Input: " + sample + " range: " + discretizedInterval.toString() + " Steps: " + steps);
            assertTrue("Retrieved value outside of range of expected!  Value: " + sample + " Generated: " + resampledValue + " max difference: " + expectedError, Math.abs(resampledValue - sample) < expectedError);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            assertTrue("Exception: " + e.toString(), false);
        }

    }

    @Test
    public void testDiscretizationNegValues()
    {

        try
        {
            // Discretization constant parameters
            int min = -100, max = 100;

            int trialCount = 1000;
            ArrayList<Double> discretized = null;
            int sample = 0;

            int minSteps = 1, maxSteps = 4, steps; // variable discretization parameter
            double expectedError;
            Pair<Double, Double> discretizedInterval;
            double resampledValue = sample;
            for (int i = 0;i<trialCount;i++)
            {
                sample = (int)(min + (max - min)*Math.random());
                steps = (int)(minSteps + (maxSteps - minSteps+1)*Math.random());
                expectedError = (max - min)/Math.pow(2, 2*steps);

                discretized = NNTools.stageDiscretizeAroundPivot(sample, min, max, steps);

                discretizedInterval = NNTools.stageContinuize(min, max, discretized);

                resampledValue = (discretizedInterval.getRight() + discretizedInterval.getLeft())/2;
                System.out.println("Input: " + sample + " range: " + discretizedInterval.toString() + " Steps: " + steps + " vector: " + Arrays.toString(discretized.toArray(new Double[0])));
                assertTrue("Retrieved value outside of range of expected!  Value: " + sample + " Generated: " + resampledValue + " max difference: " + expectedError, Math.abs(resampledValue - sample) < expectedError);

            }
        }
        catch (Exception e)
        {
            e.printStackTrace();
            assertTrue("Exception: " + e.toString(), false);
        }


    }


    @Test
    public void testDiscretizationDistributionValues()
    {

        try
        {
            // Discretization constant parameters
            int min = -100, max = 100;
            int steps = 3;
            double expectedError = (max - min)/Math.pow(2, 2*steps);
            int trialCount = 1000;
            ArrayList<Double> discretized = null;
            int sample = 0;




            int[] simple = new int[steps*2];
            Pair<Double, Double> discretizedInterval;
            double resampledValue = sample;
            for (int i = min;i<max;i++)
            {

                sample = i;

                discretized = NNTools.stageDiscretizeAroundPivot(i, min, max, steps);

                for (int j = 0;j < steps*2;j++)
                {
                    simple[j] = (discretized.get(j)).intValue();
                }

                discretizedInterval = NNTools.stageContinuize(min, max, discretized);

                resampledValue = (discretizedInterval.getRight() + discretizedInterval.getLeft())/2;
                System.out.println("Input: " + sample + " range: " + discretizedInterval.toString() + " Steps: " + steps + " vector: " + Arrays.toString(simple));
                assertTrue("Retrieved value outside of range of expected!  Value: " + sample + " Generated: " + resampledValue + " max difference: " + expectedError, Math.abs(resampledValue - sample) < expectedError);

            }
        }
        catch (Exception e)
        {
            e.printStackTrace();
            assertTrue("Exception: " + e.toString(), false);
        }


    }


    @Test
    public void testWeightSerialization()
    {
        failureMessage = "Failed to create weight matrix";
        successStepMessage = "Created weight matrix";
        try
        {
            int rows = 10, cols = 10;
            WeightMatrix m1 = new WeightMatrix(rows, cols);
            setNextStepFailSuccessMessage("Failed to initialize WeightMatrix", "Initialized weight matrix to random values");
            final int min = -100, max = 100;
            m1.mapD(new MatrixMapper() {
                @Override
                public double map(double c, int i, int j)
                {
                    return min + (max - min) * Math.random();
                }
            });

            assertTrue(failureMessage, m1.all(new WeightMatrixPredicate() {
                @Override
                public boolean trueOfCell(double c_i_j, int i, int j)
                {
                    return (c_i_j < max) && (c_i_j >= min);
                }
            }));

            assertTrue(failureMessage, m1.some(new WeightMatrixPredicate() {
                @Override
                public boolean trueOfCell(double c_i_j, int i, int j)
                {
                    return c_i_j!=0;
                }
            }));

            setNextStepFailSuccessMessage("Failed to construct weight matrix string serialization", "Constructed weight matrix string serialization");
            String serializedM1 = m1.serialize();

            assertTrue(failureMessage, serializedM1!=null && serializedM1.length() > 0);
            setNextStepFailSuccessMessage("Failed to construct matrix from serialized form", "Constructed new matrix from m1 serialization");

            WeightMatrix m1Copy = WeightMatrix.deserialize(serializedM1);
            assertTrue(failureMessage, m1Copy!=null);

            setNextStepFailSuccessMessage("Deserialized matrix failed to match original matrix", "Successfully serialized and deserialized matrix");
            assertTrue(failureMessage, m1.equals(m1Copy));

            System.out.println(successStepMessage);
            assertTrue(true);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            assertTrue(failureMessage, false);
        }
    }

    // .o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*
    //                  Helper functions
    // .o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*.o*

    /**
     * Print the last success message.  Calling this also defines the next message

     */
    public void setNextStepFailSuccessMessage(String failureMessage, String successMessage)
    {
        System.out.println(successStepMessage);
        this.failureMessage = failureMessage;
        successStepMessage = successMessage;
    }


    public static VectorPredicate makeVectorValueTester(final double[] value)
    {
        return new VectorPredicate()
        {
            public boolean trueOfDimen(double v, int i)
            {
                return v == value[i];
            }
        };
    }

    public static WeightMatrixPredicate makeMatrixValueTester(final double value)
    {
        return new WeightMatrixPredicate() {
            @Override
            public boolean trueOfCell(double c_i_j, int i, int j)
            {
                return c_i_j == value;
            }
        };
    }

    public static WeightMatrixPredicate makeMatrixValueTester(final double[][] value)
    {
        return new WeightMatrixPredicate() {
            @Override
            public boolean trueOfCell(double c_i_j, int i, int j)
            {
                return c_i_j == value[i][j];
            }
        };
    }

    public static VectorPredicate makeVectorValueTester(final double value)
    {
        return new VectorPredicate()
        {
            public boolean trueOfDimen(double v, int i)
            {
                return v == value;
            }
        };
    }

}
