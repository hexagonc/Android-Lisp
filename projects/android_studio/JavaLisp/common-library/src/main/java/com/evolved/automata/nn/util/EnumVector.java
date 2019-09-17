package com.evolved.automata.nn.util;

import java.util.Arrays;
import java.util.HashSet;

import static com.evolved.automata.nn.NNTools.isTrue;

public class EnumVector extends VectorType {

    String[] mItems = null;

    public EnumVector(String[] items){
        super(FIELD_TYPE_NAME.ENUM, items.length);
        mItems = items;
    }

    public String[] getPossibleValues(){
        return mItems;
    }

    @Override
    public LearningConfiguration.InputValidator makeValidator() {
        return new LearningConfiguration.InputValidator() {
            @Override
            public boolean isValid(float[] raw)
            {
                if (raw == null)
                    return false;

                boolean foundOne = false;
                for (int i = 0; i < raw.length;i++){
                    if (raw[i] == 1.0F){
                        if (foundOne)
                            return false;
                        foundOne = true;
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

                for (int i = 0; i < raw.length;i++){
                    if (raw[i] == 1.0F){
                        if (mItems != null)
                            return mItems.toString();
                        else
                            return "null";
                    }
                }
                // This shouldn't happen if vector is valid
                return null;
            }
        };
    }

    @Override
    public float[] valueToVector(Object value) {
        float[] out = new float[mWidth];
        for (int i = 0;i<mWidth;i++){
            if (mItems[i].equals(value)){
                out[i] = 1.0F;
            }
            else {
                out[i] = 0.0F;
            }
        }
        return out;
    }

    @Override
    public Object vectorToValue(float[] data) {
        for (int i = 0;i<data.length;i++){
            if (isTrue(data[i]))
                return mItems[i];
        }
        return null;
    }

    @Override
    public float[] getMask(Object maskSpec) {
        HashSet<Object> equivalence = (HashSet<Object>)maskSpec;

        float[] mask = new float[mItems.length];
        for (int i = 0;i<mItems.length;i++){
            if (equivalence== null || equivalence.contains(mItems[i])){
                mask[i] = 1;
            }
            else
                mask[i]= 0;
        }
        return mask;
    }

    @Override
    public float[] sampleMask(Object maskSpec) {
        HashSet<String> equivalence = (HashSet<String>)maskSpec;
        String[] itemSet;
        if (equivalence == null){
            itemSet = mItems;
        }
        else
            itemSet = equivalence.toArray(new String[0]);

        String selected = itemSet[(int)(Math.random()*itemSet.length)];
        float[] mask = new float[mItems.length];
        for (int i=0;i<mItems.length;i++){
            if (mItems[i].equals(selected)){
                mask[i] = 1.0F;
            }
            else {
                mask[i] = 0.0F;
            }
        }
        return mask;
    }



}
