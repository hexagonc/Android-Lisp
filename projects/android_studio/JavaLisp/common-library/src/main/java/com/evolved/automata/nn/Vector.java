package com.evolved.automata.nn;

import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;

/**
 * Created by Evolved8 on 12/8/16.
 */
public class Vector {
    double[] v;
    public Vector(double[] value)
    {
        v = value;
    }

    public Vector(int dimen)
    {
        v = new double[dimen];
    }

    public int dimen()
    {
        return v.length;
    }

    public double value(int index)
    {
        return v[index];
    }

    public double[] raw()
    {
        return v;
    }

    public Vector setValue(double value, int i)
    {
        v[i] = value;
        return this;
    }

    public Vector setValue(double value)
    {
        for (int i = 0;i<dimen();i++)
        {
            v[i] = value;
        }
        return this;
    }



    public Vector multiply(Vector value)
    {
        double[] o = new double[v.length];
        for (int i=0;i<v.length;i++)
        {
            o[i] = value.value(i) * v[i];
        }
        return new Vector(o);
    }

    public Vector multiply(double value)
    {
        double[] o = new double[v.length];
        for (int i=0;i<v.length;i++)
        {
            o[i] = value * v[i];
        }
        return new Vector(o);
    }

    public Vector multiplyD(Vector value)
    {

        for (int i=0;i<v.length;i++)
        {
            v[i] *= value.value(i);
        }
        return this;
    }

    public Vector multiplyD(double value)
    {

        for (int i=0;i<v.length;i++)
        {
            v[i] *= value;
        }
        return this;
    }

    public Vector add(Vector value)
    {
        double[] o = new double[v.length];
        for (int i=0;i<v.length;i++)
        {
            o[i] = value.value(i) + v[i];
        }
        return new Vector(o);
    }

    public Vector add(double value)
    {
        double[] o = new double[v.length];
        for (int i=0;i<v.length;i++)
        {
            o[i] = value + v[i];
        }
        return new Vector(o);
    }

    public Vector addD(Vector value)
    {

        for (int i=0;i<v.length;i++)
        {
            v[i] += value.value(i);
        }
        return this;
    }

    public Vector addD(double value)
    {

        for (int i=0;i<v.length;i++)
        {
            v[i] += value;
        }
        return this;
    }

    public Vector map(VectorMapper mapper)
    {
        double[] out = new double[v.length];
        for (int i=0;i<v.length;i++)
        {
            out[i] = mapper.map(v[i], i);
        }
        return new Vector(out);
    }

    public Vector mapD(VectorMapper mapper)
    {

        for (int i=0;i<v.length;i++)
        {
            v[i] = mapper.map(v[i], i);
        }
        return this;
    }

    public boolean all(VectorPredicate predicate)
    {
        for (int i = 0;i<dimen();i++)
        {
            if (!predicate.trueOfDimen(v[i], i))
                return false;
        }
        return true;
    }

    public boolean some(VectorPredicate predicate)
    {
        for (int i = 0;i<dimen();i++)
        {
            if (predicate.trueOfDimen(v[i], i))
                return true;
        }
        return false;
    }




    public String toString()
    {
        return Arrays.toString(v);
    }

    public static final String SERIALIZED_VALUE_SEPARATOR = ",";
    public String serialize()
    {
        StringBuilder stringBuilder = new StringBuilder();
        for (int i = 0;i<v.length;i++)
        {
            if (i > 0)
                stringBuilder.append(SERIALIZED_VALUE_SEPARATOR);
            stringBuilder.append(v[i]);
        }
        return stringBuilder.toString();
    }

    public static Vector fromSerialized(String serialized)
    {
        String[] parts = StringUtils.split(serialized, SERIALIZED_VALUE_SEPARATOR);
        double[] out = new double[parts.length];
        for (int i=0;i<out.length;i++)
            out[i] =  Double.parseDouble(parts[i]);
        return new Vector(out);
    }


    @Override
    public boolean equals(Object o)
    {
        if (o == null)
            return false;
        if (o.getClass().equals(this.getClass()))
        {
            Vector other = (Vector)o;

            if (other.dimen() != dimen())
                return false;
            for (int i = 0;i<v.length;i++)
            {
                if (other.v[i] != v[i])
                    return false;
            }
            return true;
        }
        else
            return false;
    }


}
