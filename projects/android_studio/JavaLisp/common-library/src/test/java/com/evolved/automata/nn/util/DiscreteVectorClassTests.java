package com.evolved.automata.nn.util;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

public class DiscreteVectorClassTests {


    @Test
    public void testSamplingStructVectorType(){

        String errorMessage = "Failed to create structure vector";
        try
        {

            StructVector struct = new StructVector(new HashMap<String, VectorType>(){
                {
                    put("actions", new EnumVector(new String[]{"left", "right", "forward", "back", "stop"}));
                    put("internal-state", new SetVector(new String[]{"learn-with-prior-state", "include-actions", "include-boolean-sensors", "include-reward", "include-global-actions", "include--global-boolean-sensors", "include-global-reward"}));
                    put("boolean-sensors", new SetVector(new String[]{"collision-left", "collision-right", "collision-back"}));
                    put("reward", new ScaledNumVector(-1,1, 3));
                    put("motor-patterns", new SetVector(new String[]{"make-left-L", "make-right-L", "forward-short", "forward-long", "backward-short", "make-T"}));

                }

            });

            errorMessage = "Failed to construct random instance of struct";
            float[] randomInstance = struct.sampleMask(null);

            errorMessage = "Failed to convert instance to value";
            HashMap<String, Object> randomValue = (HashMap<String, Object>)struct.vectorToValue(randomInstance);


            System.out.println("Random instance: " + randomValue);

        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testStructVectorType(){

        String errorMessage = "Failed to create structure vector";
        try
        {

            StructVector struct = new StructVector(new HashMap<String, VectorType>(){
                {
                    put("actions", new EnumVector(new String[]{"left", "right", "forward", "back", "stop"}));
                    put("internal-state", new SetVector(new String[]{"learn-with-prior-state", "include-actions", "include-boolean-sensors", "include-reward", "include-global-actions", "include--global-boolean-sensors", "include-global-reward"}));
                    put("boolean-sensors", new SetVector(new String[]{"collision-left", "collision-right", "collision-back"}));
                    put("reward", new ScaledNumVector(-1,1, 3));
                    put("motor-patterns", new SetVector(new String[]{"make-left-L", "make-right-L", "forward-short", "forward-long", "backward-short", "make-T"}));

                }

            });

            HashMap<String, Object> instance = new HashMap<String, Object>(){
                {
                    put("actions", "right");
                    put("internal-state", new HashSet<String>(){
                        {
                            add("learn-with-prior-state");
                            add("include-actions");
                            add("include-boolean-sensors");
                        }
                    });
                    put("boolean-sensors", new HashSet<String>());
                    put("reward", Double.valueOf(0.75));
                    put("motor-patterns", new HashSet<String>(){
                        {
                            add("forward-long");
                            add("make-right-L");
                            add("make-left-L");
                        }
                    });
                }
            };

            errorMessage = "Failed to convert " + instance + " into vector";

            float[] sampled = struct.valueToVector(instance);

            System.out.println("Converted " + instance + " into " + Arrays.toString(sampled));
            errorMessage = "Failed to reconstruct object from vector";
            Object recovered = struct.vectorToValue(sampled);
            System.out.println("Rcovered: " + recovered);

        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testScaledNumberVectorTypes(){
        String errorMessage = "Failed to create scaled number vector type";

        try
        {
            int number = 10;
            int minValue = -50, maxValue = 50;
            int numSteps = 5;

            ScaledNumVector scaledNumVectorType = new ScaledNumVector(minValue, maxValue, numSteps);

            errorMessage = "Failed to construct vector form of " + number;
            float[] vectorForm = scaledNumVectorType.valueToVector(Integer.valueOf(number));

            Assert.assertTrue(errorMessage, vectorForm != null && vectorForm.length == scaledNumVectorType.getWidth());

            System.out.println("Converted " + number + " into a vector " + Arrays.toString(vectorForm) + " using " + numSteps);
            errorMessage = "Failed to partially reconstruct discretized form of " + number + " from " + Arrays.toString(vectorForm);
            Pair<Double, Double> partiallyReconstructed = scaledNumVectorType.sampleRaw(vectorForm);

            Assert.assertTrue(errorMessage, partiallyReconstructed != null && partiallyReconstructed.getLeft()<=number && partiallyReconstructed.getRight()>=number);
            System.out.println("Recovered " + number + " to range: " + partiallyReconstructed);

        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testSetVectorType(){
        String errorMessage = "Failed to create Set Vector type";

        String[] values = new String[]{"left", "right", "back", "forward"};

        try
        {
            VectorType type = new SetVector(values);
            errorMessage = "Failed to construct all subsets of " + Arrays.toString(values);
            ArrayList<HashSet<String>> subsets = ((SetVector) type).getAllSubsets();

            Assert.assertTrue(errorMessage, subsets!=null && subsets.size() == Math.pow(2, values.length));
            for (HashSet<String> value:subsets){

                errorMessage = String.format("Failed to convert %1$s into vector", value);
                float[] vectorForm = type.valueToVector(value);
                Assert.assertTrue(errorMessage, vectorForm != null && vectorForm.length == type.getWidth());
                System.out.println(String.format("Converted %1$s into %2$s", value, Arrays.toString(vectorForm)));

                errorMessage = String.format("Failed to convert vector %1$s back into %2$s", Arrays.toString(vectorForm), value);
                HashSet<String> inverseValue = (HashSet<String>)type.vectorToValue(vectorForm);
                Assert.assertTrue(errorMessage, inverseValue != null);

                errorMessage = String.format("Failed to convert vector %1$s back into %2$s.  Got %3$s instead ", Arrays.toString(vectorForm), value, inverseValue);
                Assert.assertTrue(errorMessage, inverseValue.equals(value));
                System.out.println(String.format("Converted %1$s back into %2$s", Arrays.toString(vectorForm), inverseValue));

            }
        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testEnumVectorType(){
        String errorMessage = "Failed to create Enum Vector type";

        String[] values = new String[]{"left", "right", "back", "forward"};
        try
        {
            VectorType type = new EnumVector(values);

            for (String value:values){
                errorMessage = String.format("Failed to convert %1$s into vector", value);
                float[] vectorForm = type.valueToVector(value);
                Assert.assertTrue(errorMessage, vectorForm != null && vectorForm.length == type.getWidth());
                System.out.println(String.format("Converted %1$s into %2$s", value, Arrays.toString(vectorForm)));

                errorMessage = String.format("Failed to convert vector %1$s back into %2$s", Arrays.toString(vectorForm), value);
                String inverseValue = (String)type.vectorToValue(vectorForm);
                Assert.assertTrue(errorMessage, inverseValue != null);

                errorMessage = String.format("Failed to convert vector %1$s back into %2$s.  Got %3$s instead ", Arrays.toString(vectorForm), value, inverseValue);
                Assert.assertTrue(errorMessage, inverseValue.equals(value));
                System.out.println(String.format("Converted %1$s back into %2$s", Arrays.toString(vectorForm), inverseValue));

            }
        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testTallyVectorType(){
        String errorMessage = "Failed to create Tally Vector type";

        int maxValue = 10;
        try
        {
            VectorType type = new TallyVector(maxValue);

            for (int i = 0;i<=maxValue;i++){
                errorMessage = String.format("Failed to convert %1$s into vector", i);
                float[] vectorForm = type.valueToVector(Integer.valueOf(i));
                Assert.assertTrue(errorMessage, vectorForm != null && vectorForm.length == type.getWidth());
                System.out.println(String.format("Converted %1$s into %2$s", i, Arrays.toString(vectorForm)));

                errorMessage = String.format("Failed to convert vector %1$s back into %2$s", Arrays.toString(vectorForm), i);
                Integer inverseValue = (Integer)type.vectorToValue(vectorForm);
                Assert.assertTrue(errorMessage, inverseValue != null);

                errorMessage = String.format("Failed to convert vector %1$s back into %2$s.  Got %3$s instead ", Arrays.toString(vectorForm), i, inverseValue);
                Assert.assertTrue(errorMessage, inverseValue.intValue()==i);
                System.out.println(String.format("Converted %1$s back into %2$s", Arrays.toString(vectorForm), inverseValue));

            }
        }
        catch (Exception e){
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


}
