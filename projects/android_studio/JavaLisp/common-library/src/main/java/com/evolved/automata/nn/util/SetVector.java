package com.evolved.automata.nn.util;

import com.evolved.automata.AITools;

import java.util.ArrayList;
import java.util.HashSet;

public class SetVector extends VectorType {
    String[] mPossibleItems;


    public SetVector(String[] possibleItems){
        super(FIELD_TYPE_NAME.SET, possibleItems.length);
        mPossibleItems = possibleItems;
    }

    public ArrayList<HashSet<String>> getAllSubsets(){
        HashSet<String> items = new HashSet<>();
        for (String g:mPossibleItems){
            items.add(g);
        }
        return getAllSubsets(items);
    }

    public float[] getMask(HashSet<String> keys, boolean invertP){
        float[] mask = new float[mWidth];
        for (int i = 0;i < mPossibleItems.length;i++){
            if (!invertP && keys.contains(mPossibleItems[i]) || invertP && !keys.contains(mPossibleItems[i])){
                mask[i] = 1.0F;
            }
            else
                mask[i] = 0.0F;
        }
        return mask;
    }


    public static ArrayList<HashSet<String>> getAllSubsets(HashSet<String> items){
        ArrayList<HashSet<String>> out = new ArrayList<>();

        if (items.size() == 0){
            out.add(new HashSet<>());
            return out;
        }

        for (String item:items){
            HashSet<String> negSet = new HashSet<>();
            for (String s:items){
                if (!s.equals(item))
                    negSet.add(s);
            }

            ArrayList<HashSet<String>> missing = getAllSubsets(negSet);
            for (HashSet<String> missSet:missing){
                out.add(missSet);
                HashSet<String> with = new HashSet<>(missSet);
                with.add(item);
                out.add(with);
            }
            break;
        }
        return out;
    }


    @Override
    public LearningConfiguration.InputValidator makeValidator() {
        return new LearningConfiguration.InputValidator() {
            @Override
            public boolean isValid(float[] raw)
            {
                if (raw == null || raw.length != mWidth)
                    return false;

                for (int i = 0;i<mWidth;i++){
                    if (raw[i]<0 || raw[i]>1)
                        return false;
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
        HashSet<String> values = (HashSet<String>)value;
        float[] out = new float[mWidth];
        for (int i = 0;i<mPossibleItems.length;i++){
            if (values.contains(mPossibleItems[i]))
                out[i] = 1.0F;
            else
                out[i] = 0.0F;
        }
        return out;
    }

    @Override
    public Object vectorToValue(float[] data) {
        HashSet<String> set = new HashSet<>();
        for (int i = 0;i<data.length;i++){
            if (data[i]>0.5F)
            {
                set.add(mPossibleItems[i]);
            }
        }
        return set;
    }

    @Override
    public float[] getMask(Object maskSpec) {
        HashSet<String> keys = (HashSet<String>)maskSpec;
        return getMask(keys, false);
    }

    @Override
    public float[] sampleMask(Object maskSpec) {
        HashSet<String> keys = (HashSet<String>)maskSpec;
        ArrayList<HashSet<String>> choices = new ArrayList<>();
        outer: for (HashSet<String> example:getAllSubsets()){
            if (keys != null){
                for (String k:keys){
                    if (!example.contains(k)){
                        continue outer;
                    }
                }
            }

            choices.add(example);
        }
        HashSet<String> selected = AITools.ChooseRandom(choices);

        return valueToVector(selected);
    }


    public HashSet getSet(float[] vectorObject){
        HashSet out = new HashSet();
        for (int i = 0; i < vectorObject.length;i++){
            if (vectorObject[i] == 1.0F){
                out.add(mPossibleItems[i]);
            }
        }
        return out;
    }

    public String[] getPossibleValues(){
        return mPossibleItems;
    }

}
