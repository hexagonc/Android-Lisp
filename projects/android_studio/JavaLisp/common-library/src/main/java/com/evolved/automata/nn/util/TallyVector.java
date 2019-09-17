package com.evolved.automata.nn.util;

import java.util.ArrayList;

public class TallyVector extends VectorType {

    public TallyVector(int maxValue){
        super(FIELD_TYPE_NAME.UNSIGNED_TALLY, maxValue);
    }

    public int getMaxValue(){
        return mWidth;
    }

    public int getValue(float[] data){
        for (int i = 0;i<mWidth;i++){
            if (data[i] == 0.0F)
                return i;
        }
        return mWidth;
    }

    @Override
    public LearningConfiguration.InputValidator makeValidator() {
        return new LearningConfiguration.InputValidator() {
            @Override
            public boolean isValid(float[] raw)
            {
                boolean seenZero = false;

                for (int i = 0; i < mWidth;i++){
                    if (raw[i] == 0.0F){
                        seenZero = true;
                    }
                    else if (seenZero){
                        return false;
                    }
                }
                return true;
            }
        };
    }

    @Override
    public LearningConfiguration.InputToStringConverter makeStringConverter() {
        return new LearningConfiguration.InputToStringConverter() {
            @Override
            public String toString(float[] raw)
            {
                return vectorToValue(raw).toString();
            }
        };
    }

    @Override
    public float[] valueToVector(Object value) {
        Number nvalue = (Number)value;
        float[] out = new float[mWidth];
        for (int i=0;i<mWidth;i++){
            if (i < nvalue.intValue()){
                out[i]=1.0F;
            }
            else {
                out[i] = 0.0F;
            }
        }
        return out;
    }

    @Override
    public Object vectorToValue(float[] data) {
        int value = 0;

        for (int i = 0; i < data.length ;i++)
        {
            if (data[i] > 0.5)
            {
                value +=1;
            }
        }
        return Integer.valueOf(value);
    }

    /**
     *
     * @param maskSpec - this should be an integer, [threshold] that represents a dividing line of the space of
     *                   possible values.  If positive then all values greater than [threshold] are considered in focus.
     *                  If negative then all values less than or equal to  -[threshold] are considered in focus
     * @return
     */
    @Override
    public float[] getMask(Object maskSpec) {
        // If threshold is non-negative then represents the number of leading 0's
        // Another interpretation is all integer values greater than threshold

        // If threshold is negative then represents the number of leading 1's
        // Another interpretation is all integers less than or equal to |threshold|

        Integer threshold = (Integer)maskSpec;
        if (threshold == null){
            threshold = 0;
        }
        float[] out = new float[mWidth];
        for (int i = 0;i < out.length;i++){
            if (threshold >= 0){
                if (i >= threshold){
                    out[i] = 1.0F;
                }
                else {
                    out[i] = 0.0F;
                }

            }
            else {

                if (i < -1*threshold){
                    out[i] = 1.0F;

                }
                else {
                    out[i] = 0.0F;
                }
            }


        }
        return out;
    }

    @Override
    public float[] sampleMask(Object maskSpec) {
        float[] mask = getMask(maskSpec);
        ArrayList<Integer> indices = new ArrayList<>();
        for (int i = 0;i<mWidth;i++){
            if (mask[i] == 1.0F){
                indices.add(i + 1);
            }
        }

        if (indices.size() == 0){
            return null;
        }
        return valueToVector(indices.get((int)(indices.size()*Math.random())));
    }

}
