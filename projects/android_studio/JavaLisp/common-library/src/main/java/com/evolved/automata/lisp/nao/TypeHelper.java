package com.evolved.automata.lisp.nao;

import com.aldebaran.qi.QiRuntimeException;
import com.aldebaran.qi.Tuple;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.IntHashtableValue;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.StringHashtableValue;
import com.evolved.automata.lisp.Value;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

/**
 * Created by Evolved8 on 6/21/17.
 */

public class TypeHelper {
    final static Class mHashMapClass = HashMap.class;
    final static Class mListClass = ArrayList.class;
    final static Class mLongClass = Long.class;
    final static Class mIntegerClass = Integer.class;
    final static Class mCharClass = Character.class;
    final static Class mByteClass = Byte.class;
    final static Class mBooleanClass = Boolean.class;

    final static Class mDoubleClass = Double.class;
    final static Class mFloatClass = Float.class;

    final static Class mStringClass = String.class;
    final static Class mTupleClass = Tuple.class;



    public enum QiArgumentType
    {
        Float, Integer, String, Boolean, StringArray
    }
    interface QiTypeConverter
    {
        Value getLispValue(Object qiValue);
    }

    interface LispToQiTypeConverer
    {
        Object getQiValue(Value lispValue);
    }




    static void addLispToQiTypeConverters()
    {
        NAOManager.addLispValueToQiValueConversion(QiArgumentType.Boolean, new LispToQiTypeConverer() {
            @Override
            public Object getQiValue(Value lispValue)
            {
                return convertLispBoolean(lispValue);
            }
        });

        NAOManager.addLispValueToQiValueConversion(QiArgumentType.String, new LispToQiTypeConverer() {
            @Override
            public Object getQiValue(Value lispValue)
            {
                return convertLispString(lispValue);
            }
        });

        NAOManager.addLispValueToQiValueConversion(QiArgumentType.Float, new LispToQiTypeConverer() {
            @Override
            public Object getQiValue(Value lispValue)
            {
                return convertLispNumeric(lispValue);
            }
        });

        NAOManager.addLispValueToQiValueConversion(QiArgumentType.Integer, new LispToQiTypeConverer() {
            @Override
            public Object getQiValue(Value lispValue)
            {
                return convertLispInteger(lispValue);
            }
        });


        NAOManager.addLispValueToQiValueConversion(QiArgumentType.StringArray, new LispToQiTypeConverer() {
            @Override
            public Object getQiValue(Value lispValue)
            {
                return convertLispStringList(lispValue);
            }
        });
    }

    static void addQiObjectToLispTypeConverters()
    {
        NAOManager.addQiValueToLispValueConversion(mLongClass, new QiTypeConverter() {
            @Override
            public Value getLispValue(Object qiValue)
            {
                return convertQiLong(qiValue);
            }
        });

        NAOManager.addQiValueToLispValueConversion(mIntegerClass, new QiTypeConverter() {
            @Override
            public Value getLispValue(Object qiValue)
            {
                return convertQiInteger(qiValue);
            }
        });

        NAOManager.addQiValueToLispValueConversion(mCharClass, new QiTypeConverter() {
            @Override
            public Value getLispValue(Object qiValue)
            {
                return convertQiChar(qiValue);
            }
        });

        NAOManager.addQiValueToLispValueConversion(mByteClass, new QiTypeConverter() {
            @Override
            public Value getLispValue(Object qiValue)
            {
                return convertQiByte(qiValue);
            }
        });


        NAOManager.addQiValueToLispValueConversion(mBooleanClass, new QiTypeConverter() {
            @Override
            public Value getLispValue(Object qiValue)
            {
                return convertQiBoolean(qiValue);
            }
        });

        NAOManager.addQiValueToLispValueConversion(mDoubleClass, new QiTypeConverter() {
            @Override
            public Value getLispValue(Object qiValue)
            {
                return convertQiDouble(qiValue);
            }
        });

        NAOManager.addQiValueToLispValueConversion(mFloatClass, new QiTypeConverter() {
            @Override
            public Value getLispValue(Object qiValue)
            {
                return convertQiFloat(qiValue);
            }
        });


        NAOManager.addQiValueToLispValueConversion(mStringClass, new QiTypeConverter() {
            @Override
            public Value getLispValue(Object qiValue)
            {
                return convertQiString(qiValue);
            }
        });

        NAOManager.addQiValueToLispValueConversion(mListClass, new QiTypeConverter() {
            @Override
            public Value getLispValue(Object qiValue)
            {
                ArrayList base = (ArrayList)qiValue;
                int length = base.size();
                Value[] lvalues = new Value[length];
                Value element = null;

                Object qiElement;

                for (int i = 0; i < length;i++)
                {
                    qiElement = base.get(i);

                    element = convertQiValue(qiElement);
                    if (element == null)
                        return null;
                    lvalues[i] = element;
                }
                return NLispTools.makeValue(lvalues);
            }
        });

        NAOManager.addQiValueToLispValueConversion(mHashMapClass, new QiTypeConverter() {
            @Override
            public Value getLispValue(Object qiValue)
            {
                HashMap base = (HashMap)qiValue;
                int length = base.size();

                if (length == 0)
                {
                    // TODO -  come up with a better solution to this.
                    // Currently, no way to know the key type if map is empty
                    // due to type erasure

                    return new StringHashtableValue(new HashMap<String, Value>());
                }

                Set keys = base.keySet();
                Object firstKey = keys.iterator().next();

                if (firstKey instanceof Number)
                {
                    HashMap<Long, Value> intHashMap = new HashMap<Long, Value>();

                    for (Object key:keys)
                    {
                        Long lispKey = Long.valueOf (((Number)key).longValue());
                        Object qiElement = base.get(key);
                        Value element = convertQiValue(qiElement);
                        if (element == null)
                            return null;
                        intHashMap.put(lispKey, element);
                    }
                    return new IntHashtableValue(intHashMap);
                }
                else
                {
                    HashMap<String, Value> stringHashMap = new HashMap<String, Value>();
                    for (Object key:keys)
                    {
                        String lispKey = key.toString();
                        Object qiElement = base.get(key);
                        Value element = convertQiValue(qiElement);
                        if (element == null)
                            return null;
                        stringHashMap.put(lispKey, element);
                    }
                    return new StringHashtableValue(stringHashMap);
                }


            }
        });
    }

    static Value convertQiValue(Object qiResult)
    {
        if (qiResult == null)
            return null;
        Class qiClass = qiResult.getClass();
        QiTypeConverter elementConverter = NAOManager.mConversionMap.get(qiClass);
        if (elementConverter == null)
        {
            if (qiResult instanceof ByteBuffer)
            {
                return convertQiByteBuffer(qiResult);
            }
            return null;
        }
        return elementConverter.getLispValue(qiResult);
    }

    static Value convertQiLong(Object qiResult)
    {
        Long llong = (Long)qiResult;
        return NLispTools.makeValue(llong.longValue());
    }

    static Value convertQiInteger(Object qiResult)
    {
        Integer integer = (Integer)qiResult;
        return NLispTools.makeValue(integer.intValue());
    }

    static Value convertQiChar(Object qiResult)
    {
        Character cchar = (Character)qiResult;
        return NLispTools.makeValue(cchar.toString());
    }

    static Value convertQiByte(Object qiResult)
    {
        Byte actual = (Byte)qiResult;
        // TODO: This is extremely memory inefficient.  Can't store a byte as a long
        // long term
        return NLispTools.makeValue((int)actual);
    }


    static Value convertQiBoolean(Object qiResult)
    {
        Boolean bool = (Boolean)qiResult;
        return NLispTools.makeValue(bool.booleanValue());
    }

    static Value convertQiDouble(Object qiResult)
    {
        Double obj = (Double)qiResult;
        return NLispTools.makeValue(obj.doubleValue());
    }

    static Value convertQiFloat(Object qiResult)
    {
        Float actual = (Float)qiResult;
        return NLispTools.makeValue(actual.floatValue());
    }

    static Value convertQiString(Object qiResult)
    {
        String actual = (String)qiResult;
        return NLispTools.makeValue(actual);
    }


    static Value convertQiByteBuffer(Object qiResult)
    {
        ByteBuffer buffer = (ByteBuffer)qiResult;


        return ExtendedFunctions.makeValue(buffer);
    }


    static Object convertLispString(Value lvalue)
    {
        Value.Type expectedType = Value.Type.STRING;

        if (lvalue == null || lvalue.getType()!=expectedType)
        {
            String givenType;
            if (lvalue == null)
                givenType = "NULL";
            else
                givenType = lvalue.getType().name();
            throw new QiRuntimeException("Invalid Qi Lisp Argument type: expected " + expectedType + " but found " + givenType);
        }
        return lvalue.getString();
    }

    static Object convertLispNumeric(Value lvalue)
    {
        Value.Type expectedType = Value.Type.FLOAT;

        if (lvalue == null || lvalue.getType()!=expectedType)
        {
            String givenType;
            if (lvalue == null)
                givenType = "NULL";
            else
                givenType = lvalue.getType().name();
            throw new QiRuntimeException("Invalid Qi Lisp Argument type: expected " + expectedType + " but found " + givenType);
        }
        return Float.valueOf((float)lvalue.getFloatValue());
    }

    static Object convertLispInteger(Value lvalue)
    {
        Value.Type expectedType = Value.Type.INTEGER;

        if (lvalue == null || lvalue.getType()!=expectedType)
        {
            String givenType;
            if (lvalue == null)
                givenType = "NULL";
            else
                givenType = lvalue.getType().name();
            throw new QiRuntimeException("Invalid Qi Lisp Argument type: expected " + expectedType + " but found " + givenType);
        }
        return Integer.valueOf((int)lvalue.getIntValue());
    }

    static Object convertLispBoolean(Value lvalue)
    {

        if (lvalue == null)
            throw new QiRuntimeException("Invalid Qi Lisp Argument type: argument is null");

        return Boolean.valueOf(!lvalue.isNull());
    }


    // TODO: generalize this for arbitrary types of Lisp strings
    static Object convertLispStringList(Value lvalue)
    {
        Value.Type expectedType = Value.Type.LIST;

        if (lvalue == null || lvalue.getType()!=expectedType)
        {
            String givenType;
            if (lvalue == null)
                givenType = "NULL";
            else
                givenType = lvalue.getType().name();
            throw new QiRuntimeException("Invalid Qi Lisp Argument type: expected " + expectedType + " but found " + givenType);
        }

        Value[] elements = lvalue.getList();
        ArrayList qiList = new ArrayList();
        for (Value v:elements)
        {
            qiList.add(convertLispString(v));
        }
        return qiList;
    }
}
