package com.evolved.automata.nn.util;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

public class StructVector extends VectorType {
    HashMap<Integer, Object> mBufferMap = new HashMap<>();

    HashMap<String, Triple<VectorType, LearningConfiguration.InputValidator, LearningConfiguration.InputToStringConverter>> mTypeSpec = new HashMap<>();

    ArrayList<Pair<String, VectorType>> mStructSpec = new ArrayList<>();

    HashMap<Integer, VectorType> mTypeIndexMap = new HashMap<>();

    String[] mKeys = null;

    float[][] mDataBuffer;

    public StructVector(HashMap<String, VectorType> structMap){
        super(FIELD_TYPE_NAME.STRUCT, 0);

        int size = structMap.size();
        mDataBuffer = new float[size][];
        mKeys = structMap.keySet().toArray(new String[0]);
        Arrays.sort(mKeys, (String l, String r)->l.compareTo(r));
        String key;

        for (int i=0;i<size;i++){
            key = mKeys[i];
            VectorType type = structMap.get(key);
            int typeWidth = type.getWidth();
            mTypeIndexMap.put(mWidth, type);
            mWidth+=typeWidth;
            mDataBuffer[i] = new float[typeWidth];
            if (!mBufferMap.containsKey(typeWidth)){
                // mBufferMap is completely independent of mDataBuffer
                mBufferMap.put(typeWidth, new float[typeWidth]);
            }

            LearningConfiguration.InputValidator validator = type.getValidator();
            LearningConfiguration.InputToStringConverter toStringer = type.getStringConverter();

            mTypeSpec.put(key, Triple.of(type, validator, toStringer));
            mStructSpec.add(Pair.of(key, type));
        }
    }

    public VectorType getFieldType(String field){
        Triple<VectorType, LearningConfiguration.InputValidator, LearningConfiguration.InputToStringConverter> fieldTriple = mTypeSpec.get(field);
        if (fieldTriple == null)
            return null;
        return fieldTriple.getLeft();
    }

    public String[] getFieldNames(){
        return mKeys;
    }


    private void fillBuffer(float[] data){
        int dataIndex = 0;
        int dataWidth = mStructSpec.get(dataIndex).getRight().getWidth();
        int subIndex = 0;
        for (int i = 0;i<data.length;i++){
            if (subIndex == dataWidth){
                dataIndex++;
                dataWidth = mStructSpec.get(dataIndex).getRight().getWidth();
                subIndex = 0;
            }
            mDataBuffer[dataIndex][subIndex] = data[i];
            subIndex++;
        }
    }

    public float[] getBufferAsCompleteVector(){
        float[] out = new float[mWidth];
        int index = 0;
        for (int i =0;i<mKeys.length;i++){
            float[] buffer = mDataBuffer[i];
            for (int j=0;j<buffer.length;j++){
                out[index++] = buffer[j];
            }
        }
        return out;
    }

    public StructVector setBufferValue(int index, float value){
        float[] buffer = mDataBuffer[index];
        for (int j=0;j<buffer.length;j++){
           buffer[j] = value;
        }
        return this;
    }

    public StructVector setBufferValue(int index, int bufferIndex, float value){
        float[] buffer = mDataBuffer[index];
        buffer[bufferIndex] = value;
        return this;
    }


    @Override
    public LearningConfiguration.InputValidator makeValidator() {
        return new LearningConfiguration.InputValidator() {
            @Override
            public boolean isValid(float[] raw)
            {
                VectorType currentType = null;
                float[] buffer = null;

                int typeWidth = 0;
                int innerIndex = 0;
                int typeIndex = 0;
                LearningConfiguration.InputValidator validator = null;
                String currentTypeKey = null;
                for (int i = 0;i < mWidth;i++){
                    if (mTypeIndexMap.containsKey(i)){
                        Pair<String, VectorType> orderSpec = mStructSpec.get(typeIndex);
                        currentTypeKey = orderSpec.getLeft();
                        validator = mTypeSpec.get(currentTypeKey).getMiddle();

                        currentType = mTypeIndexMap.get(i);
                        typeWidth = currentType.getWidth();
                        buffer = (float[])mBufferMap.get(typeWidth);
                        innerIndex = 0;
                        typeIndex++;
                    }
                    buffer[innerIndex] = raw[i];

                    if (innerIndex == typeWidth){
                        if (!validator.isValid(buffer))
                            return false;
                    }
                    innerIndex++;
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
                StringBuilder builder = new StringBuilder("{");

                VectorType currentType = null;
                float[] buffer = null;

                int typeWidth = 0;
                int innerIndex = 0;
                int typeIndex = 0;
                String currentKey = null;
                LearningConfiguration.InputToStringConverter typeSerializer = null;
                for (int i = 0;i < mWidth;i++){
                    if (mTypeIndexMap.containsKey(i)){
                        Pair<String, VectorType> orderSpec = mStructSpec.get(typeIndex);
                        currentKey = orderSpec.getKey();
                        typeSerializer = mTypeSpec.get(currentKey).getRight();

                        currentType = mTypeIndexMap.get(i);
                        typeWidth = currentType.getWidth();
                        buffer = (float[])mBufferMap.get(typeWidth);
                        innerIndex = 0;
                        typeIndex++;
                    }
                    buffer[innerIndex] = raw[i];

                    if (innerIndex == typeWidth){
                        if (builder.length()>1){
                            builder.append(", ");
                        }

                        builder.append(currentKey).append(" = ").append(typeSerializer.toString(buffer));
                    }
                    innerIndex++;
                }

                builder.append("}");
                return builder.toString();
            }
        };
    }

    @Override
    public float[] valueToVector(Object value) {
        HashMap<String, Object> map = (HashMap<String, Object>)value;
        float[] total = new float[mWidth];
        int index = 0;
        for (int i = 0;i<mKeys.length;i++){
            String key = mKeys[i];

            float[] buffer = mTypeSpec.get(key).getLeft().valueToVector(map.get(key));
            for (int j = 0;j < buffer.length;j++){
                total[index++]=buffer[j];
            }
        }
        return total;
    }

    @Override
    public Object vectorToValue(float[] data) {
        fillBuffer(data);
        HashMap<String, Object> output = new HashMap<>();
        for (int i = 0;i<mKeys.length;i++){
            String key = mKeys[i];
            output.put(key, mTypeSpec.get(key).getLeft().vectorToValue(mDataBuffer[i]));
        }

        return output;
    }

    @Override
    public float[] getMask(Object maskSpec) {
        HashMap<String, Object> maskFieldMap = (HashMap<String, Object>)maskSpec;

        for (int i = 0;i < mKeys.length;i++){
            String key = mKeys[i];
            if (maskFieldMap.containsKey(key)){
                Object fieldMaskSpec = maskFieldMap.get(key);
                VectorType fieldType = mStructSpec.get(i).getRight();
                float[] fieldMask = fieldType.getMask(fieldMaskSpec);
                for (int j = 0;j < fieldType.getWidth();j++){
                    setBufferValue(i, j, fieldMask[j]);
                }
            }
            else {
                setBufferValue(i, 0);
            }
        }
        return getBufferAsCompleteVector();
    }

    @Override
    public float[] sampleMask(Object maskSpec) {

        HashMap<String, Object> maskFieldMap = (HashMap<String, Object>)maskSpec;
        if (maskFieldMap == null)
            maskFieldMap = new HashMap<>();

        for (int i = 0;i < mKeys.length;i++){
            String key = mKeys[i];
            float[] randomSample;
            Object fieldMaskSpec = maskFieldMap.get(key);
            VectorType fieldType = mStructSpec.get(i).getRight();
            randomSample = fieldType.sampleMask(fieldMaskSpec);

            for (int j = 0;j < fieldType.getWidth();j++){
                setBufferValue(i, j, randomSample[j]);
            }
        }
        return getBufferAsCompleteVector();
    }

    public boolean containsKey(String key){
        return mTypeSpec.containsKey(key);
    }
    public float[] getMask(HashSet<String> keys, boolean invertP){
        for (int i=0;i<mKeys.length;i++){
            if (keys.contains(mKeys[i]) && !invertP || !keys.contains(mKeys[i]))
                setBufferValue(i, 1);
            else
                setBufferValue(i, 0);
        }
        return getBufferAsCompleteVector();
    }
}
