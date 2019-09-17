package com.evolved.automata.nn.util;


import com.evolved.automata.nn.NNTools;

import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;

public class ScaledNumVector extends VectorType {

    double mMinValue = 0;
    double maxValue = 0;
    int numSteps = 0;
    public ScaledNumVector(double min, double max, int steps){
        super(FIELD_TYPE_NAME.SCALED_VALUE, steps*2);
        mMinValue = min;
        maxValue = max;
        numSteps = steps;
    }

    @Override
    public LearningConfiguration.InputValidator makeValidator() {
        return new LearningConfiguration.InputValidator() {
            @Override
            public boolean isValid(float[] raw) {
                try
                {
                    Pair<Double, Double> range = sampleRaw(raw);
                    if (range == null || range.getRight() == null || range.getLeft()==null || range.getRight()<mMinValue || range.getLeft()> maxValue)
                        return false;
                    return true;
                }
                catch (Exception e){
                    return false;
                }

            }
        };
    }

    @Override
    public LearningConfiguration.InputToStringConverter makeStringConverter() {
        return new LearningConfiguration.InputToStringConverter() {
            @Override
            public String toString(float[] raw) {
                return ((Double) vectorToValue(raw)).toString();
            }
        };
    }

    @Override
    public float[] valueToVector(Object value) {
        Number d = (Number)value;
        ArrayList<Double> o = NNTools.stageDiscretizeAroundPivot(d.doubleValue(), mMinValue, maxValue, numSteps);
        float[] out = new float[o.size()];
        for (int i = 0;i<o.size();i++){
            out[i]=o.get(i).floatValue();
        }
        return out;
    }

    @Override
    public Object vectorToValue(float[] data) {

        Pair<Double, Double> range = sampleRaw(data);
        return Double.valueOf(0.5*(range.getLeft()+range.getRight()));
    }

    @Override
    public float[] getMask(Object maskSpec) {
        // Masks not implemented
        float[] out = new float[mWidth];
        for (int i = 0;i<mWidth;i++){
            out[i] = 1.0F;
        }
        return out;
    }

    public double getMinValue(){
        return mMinValue;
    }

    public double getMaxValue(){
        return maxValue;
    }

    public int getNumSteps(){
        return numSteps;
    }

    @Override
    public float[] sampleMask(Object maskSpec) {
        // TODO: refactor this. Currently this just does a uniform vectorToValue
        Integer numSteps = (Integer)maskSpec;
        Float v = Float.valueOf((float)((maxValue - mMinValue)*Math.random()+ mMinValue));
        return valueToVector(v);
    }

    public Pair<Double, Double> sampleRaw(float[] data){
        ArrayList<Double> in = new ArrayList<Double>();
        for (double d:data)
        {
            in.add(d);
        }
        return NNTools.stageContinuize(mMinValue, maxValue, in);

    }


}
