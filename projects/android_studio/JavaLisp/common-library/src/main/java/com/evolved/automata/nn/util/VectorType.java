package com.evolved.automata.nn.util;

public abstract class VectorType {

    public enum FIELD_TYPE_NAME {
        ENUM, UNSIGNED_TALLY, SCALED_VALUE, SET, UNSIGNED_BINARY_NUMBER, STRUCT
    }

    int mWidth = 0;

    FIELD_TYPE_NAME mTypeName;

    LearningConfiguration.InputValidator mInputValidator;

    LearningConfiguration.InputToStringConverter mVectorToStringConverter;


    public VectorType(FIELD_TYPE_NAME type, int width){
        mWidth = width;
        mTypeName = type;
        mInputValidator = makeValidator();
        mVectorToStringConverter = makeStringConverter();
    }

    public abstract LearningConfiguration.InputValidator makeValidator();

    public abstract LearningConfiguration.InputToStringConverter makeStringConverter();

    public abstract float[] valueToVector(Object value);

    public abstract Object vectorToValue(float[] data);

    /**
     * Constructs a mask given a type specific specification object
     * @param maskSpec
     * @return
     */
    public abstract float[] getMask(Object maskSpec);

    /**
     * Generates a random instance of this type given a type specific mask specification object, the same specification
     * as used in [getMask]
     * @param maskSpec
     * @return
     */
    public abstract float[] sampleMask(Object maskSpec);

    public LearningConfiguration.InputValidator getValidator(){
        return mInputValidator;
    }

    public LearningConfiguration.InputToStringConverter getStringConverter(){
        return mVectorToStringConverter;
    }

    public int getWidth(){
        return mWidth;
    }

    public FIELD_TYPE_NAME getType(){
        return mTypeName;
    }

}
