package com.evolved.automata.speech;

import org.apache.commons.lang3.StringUtils;

public class SpeechArgumentException extends RuntimeException {
    public enum Type {
        MISSING_ARGUMENTS, WRONG_ARGUMENT_TYPE
    }

    Type mType;

    String mDesc;
    public SpeechArgumentException (String desc, Type type)
    {
        super(desc);
        mType = type;
        mDesc = desc;
    }

    public Type getErrorType()
    {
        return mType;
    }

    public String getDescription()
    {
        return mDesc;
    }

    public static SpeechArgumentException make(String incorrectArgumentTypeName)
    {
        return new SpeechArgumentException(incorrectArgumentTypeName, Type.WRONG_ARGUMENT_TYPE);
    }

    public static SpeechArgumentException make(String[] missingArguments)
    {
        return new SpeechArgumentException(StringUtils.join(missingArguments, ", "), Type.MISSING_ARGUMENTS);
    }
}
