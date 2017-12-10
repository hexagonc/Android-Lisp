package com.evolved.automata.speech;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

public class SpeechValue {


    HashMap<SpeechValueType, Object> mTypeMap = new HashMap<SpeechValueType, Object>();

    public boolean hasType(SpeechValueType type)
    {
        return mTypeMap.containsKey(type);
    }


    public SpeechValue addType(SpeechValueType type, Object value)
    {
        mTypeMap.put(type, value);
        return this;
    }

    private SpeechValue(String v)
    {
        mTypeMap.put(SpeechValueType.STRING, v);
    }

    private SpeechValue(Object v)
    {


        mTypeMap.put(SpeechValueType.MAPPED_VALUE, v);
    }

    private SpeechValue(Object v, SpeechValueType type)
    {
        mTypeMap.put(type, v);

    }

    private SpeechValue(SpeechValue[] list)
    {

        mTypeMap.put(SpeechValueType.LIST, list);
    }

    public boolean isList()
    {
        return mTypeMap.containsKey(SpeechValueType.LIST);
    }

    public boolean isNumber()
    {
        return mTypeMap.containsKey(SpeechValueType.NUMBER);
    }

    public boolean isMappedObject()
    {
        return mTypeMap.containsKey(SpeechValueType.MAPPED_VALUE);
    }

    public boolean isException()
    {
        return mTypeMap.containsKey(SpeechValueType.EXCEPTION);
    }

    public boolean isString()
    {
        return mTypeMap.containsKey(SpeechValueType.STRING);
    }





    @Override
    public String toString()
    {
        StringBuilder b = new StringBuilder();
        for (SpeechValueType type:mTypeMap.keySet())
        {
            if (b.length() > 0)
                b.append(" ");
            Object value = mTypeMap.get(type);

            switch (type)
            {

                case LIST:
                    b.append(Arrays.toString((SpeechValue[])value));
                    break;
                default:
                    b.append(value.toString());
                    break;
            }

        }

        return b.toString();
    }

    public static SpeechValue make(String[] items)
    {
        SpeechValue[] out = new SpeechValue[items.length];
        for (int i = 0;i < items.length;i++)
        {
            out[i] = SpeechValue.make(items[i]);
        }
        return make(out);
    }

    public static SpeechValue make(Object mapped)
    {
        return new SpeechValue(mapped, SpeechValueType.MAPPED_VALUE);
    }

    public static SpeechValue make(String mapped)
    {
        return new SpeechValue(mapped);
    }

    public static SpeechValue make(SpeechValue[] list)
    {
        return new SpeechValue(list);
    }


    public static SpeechValue make(double d)
    {
        return new SpeechValue(Double.valueOf(d), SpeechValueType.NUMBER);
    }

    public static SpeechValue make(Exception e)
    {
        return new SpeechValue(e, SpeechValueType.EXCEPTION);
    }



    public Exception getException()
    {
        return (Exception)mTypeMap.get(SpeechValueType.EXCEPTION);
    }

    public String getStringValue()
    {
        return (String)mTypeMap.get(SpeechValueType.STRING);
    }

    public Object getRawValue()
    {
        return mTypeMap.get(SpeechValueType.MAPPED_VALUE);
    }


    public SpeechValue[] getList()
    {
        return (SpeechValue[])mTypeMap.get(SpeechValueType.LIST);
    }

    public Double getDoubleValue()
    {
        return (Double)mTypeMap.get(SpeechValueType.NUMBER);
    }
}
