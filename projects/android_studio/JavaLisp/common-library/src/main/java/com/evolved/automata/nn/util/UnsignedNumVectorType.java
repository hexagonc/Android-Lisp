package com.evolved.automata.nn.util;

import java.math.BigInteger;

public class UnsignedNumVectorType extends VectorType {

    final long MAX_VALUE;

    public UnsignedNumVectorType(int width){
        super(FIELD_TYPE_NAME.UNSIGNED_BINARY_NUMBER, width);
        if (width < 64)
            MAX_VALUE = (long)Math.pow(2, width);
        else
            throw new IllegalArgumentException("UnsignedNumVectorType can be at most 63 bits wide");
    }

    @Override
    public LearningConfiguration.InputValidator makeValidator() {
        return new LearningConfiguration.InputValidator() {
            @Override
            public boolean isValid(float[] raw) {
                return raw != null && raw.length == mWidth;
            }
        };
    }

    @Override
    public LearningConfiguration.InputToStringConverter makeStringConverter() {
        return new LearningConfiguration.InputToStringConverter() {
            @Override
            public String toString(float[] raw) {
                long out = 0;
                for (int i = 0; i< mWidth;i++) {
                    out+=(raw[i] == 1.0F)?(long)Math.pow(2, i):0;
                }
                return Long.toString(out);
            }
        };
    }

    @Override
    public float[] valueToVector(Object value) {
        float[] out = null;
        if (value instanceof Long || value instanceof Integer){

            out = new float[mWidth];
            long v = ((Number) value).longValue();

            if (v > MAX_VALUE || v < 0)
                return null;

            int i = 0;
            while (v >= 2) {
                if (v % 2 == 0){
                    out[i] = 0;
                }
                else {
                    out[i] = 1;
                }
                v=v/2;
                i++;
            }
            out[i] = v;
        }
        return out;
    }

    @Override
    public Object vectorToValue(float[] data) {
        if (getValidator().isValid(data)){
            long out = 0;
            for (int i = 0; i< mWidth;i++) {
                out+=(data[i] == 1.0F)?(long)Math.pow(2, i):0;
            }
            return Long.valueOf(out);
        }
        else
            return null;


    }

    @Override
    public float[] getMask(Object maskSpec) {
        return new float[0];
    }

    @Override
    public float[] sampleMask(Object maskSpec) {
        return new float[0];
    }
}
