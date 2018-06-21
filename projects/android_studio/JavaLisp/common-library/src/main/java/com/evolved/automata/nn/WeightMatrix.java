package com.evolved.automata.nn;

import org.apache.commons.lang3.StringUtils;

import java.util.LinkedList;

/**
 * Created by Evolved8 on 12/8/16.
 */
public class WeightMatrix {
    int rows;
    int cols;

    double[][] weights;

    public WeightMatrix(Vector[] rows)
    {
        this.rows = rows.length;
        for (int i = 0;i<this.rows;i++)
        {
            weights[i] = rows[i].raw();
            this.cols = weights[i].length;
        }
    }

    public WeightMatrix(double[][] data)
    {
        this.rows = data.length;
        this.cols = data[0].length;
        weights = data;
    }

    public WeightMatrix(int rows, int cols)
    {
        this.rows = rows;
        this.cols = cols;
        weights = new double[rows][cols];
    }

    public int targetDimen()
    {
        return rows;
    }

    public int sourceDimen()
    {
        return cols;
    }

    public int rows()
    {
        return rows;
    }

    public int cols()
    {
        return cols;
    }

    public double value(int i, int j)
    {
        return weights[i][j];
    }

    public WeightMatrix setValue(double value, int i, int j)
    {
        weights[i][j] = value;
        return this;
    }

    public WeightMatrix setValue(double value)
    {
        for (int i = 0; i < rows;i++)
        {
            for (int j = 0;j < cols;j++)
            {
                weights[i][j] = value;
            }
        }
        return this;
    }

    public WeightMatrix add(WeightMatrix matrix)
    {
        double[][] data = new double[rows][cols];

        for (int i = 0; i < rows;i++)
        {
            for (int j = 0;j < cols;j++)
            {
                data[i][j] = matrix.value(i, j) + weights[i][j];
            }
        }
        return new WeightMatrix(data);
    }

    public WeightMatrix add(double value)
    {
        double[][] data = new double[rows][cols];

        for (int i = 0; i < rows;i++)
        {
            for (int j = 0;j < cols;j++)
            {
                data[i][j] = value + weights[i][j];
            }
        }
        return new WeightMatrix(data);
    }


    public WeightMatrix addD(WeightMatrix matrix)
    {

        for (int i = 0; i < rows;i++)
        {
            for (int j = 0;j < cols;j++)
            {
                weights[i][j] = matrix.value(i, j) + weights[i][j];
            }
        }
        return this;
    }

    public WeightMatrix addD(double value)
    {

        for (int i = 0; i < rows;i++)
        {
            for (int j = 0;j < cols;j++)
            {
                weights[i][j] = value + weights[i][j];
            }
        }
        return this;
    }


    public WeightMatrix map(MatrixMapper mapper)
    {
        double[][] data = new double[rows][cols];

        for (int i = 0; i < rows;i++)
        {
            for (int j = 0;j < cols;j++)
            {
                data[i][j] = mapper.map(weights[i][j], i , j);
            }
        }
        return new WeightMatrix(data);
    }

    public WeightMatrix mapD(MatrixMapper mapper)
    {

        for (int i = 0; i < rows;i++)
        {
            for (int j = 0;j < cols;j++)
            {
                weights[i][j] = mapper.map(weights[i][j], i , j);
            }
        }
        return this;
    }

    public Vector multiply(Vector value)
    {
        double[] out = new double[targetDimen()];
        for (int i = 0; i < out.length;i++)
        {
            for (int j = 0;j < value.dimen();j++)
            {
                out[i]+=weights[i][j] * value.value(j);
            }
        }
        return new Vector(out);
    }


    public WeightMatrix multiplyD(double value)
    {

        for (int i = 0; i < rows;i++)
        {
            for (int j = 0;j < cols;j++)
            {
                weights[i][j] *= value;
            }
        }
        return this;
    }

    public WeightMatrix multiply(double value)
    {
        double[][] data = new double[rows][cols];

        for (int i = 0; i < rows;i++)
        {
            for (int j = 0;j < cols;j++)
            {
                data[i][j] = value * weights[i][j];
            }
        }
        return new WeightMatrix(data);
    }

    public boolean all(WeightMatrixPredicate predicate)
    {
        for (int i = 0; i < rows;i++)
        {
            for (int j = 0;j < cols;j++)
            {
                if (!predicate.trueOfCell(weights[i][j], i, j))
                    return false;
            }
        }
        return true;
    }

    public boolean some(WeightMatrixPredicate predicate)
    {
        for (int i = 0; i < rows;i++)
        {
            for (int j = 0;j < cols;j++)
            {
                if (predicate.trueOfCell(weights[i][j], i, j))
                    return true;
            }
        }
        return false;
    }

    public double[][] getWeights()
    {
        return weights;
    }

    public WeightMatrix setWeights(double[][] newWeights, boolean makeCopyP)
    {
        if (!compatibleDimensions(weights, newWeights))
            throw new IllegalArgumentException("Can only set weights from arrays of equal dimension");

        if (makeCopyP && compatibleDimensions(weights, newWeights))
        {
            for (int i = 0;i < rows(); i++)
            {
                for (int j = 0; j < cols();j++)
                {
                    weights[i][j] = newWeights[i][j];
                }
            }
        }
        else
        {
            weights = newWeights;

        }
        return this;
    }

    public static boolean compatibleDimensions(double[][] lvalue, double[][] rvalue)
    {
        if (lvalue == null && rvalue == null)
            return true;

        if (lvalue == null || rvalue == null)
            return false;

        if (lvalue.length == rvalue.length)
        {
            for (int i = 0;i< lvalue.length;i++)
            {
                if (lvalue[i].length != rvalue[i].length)
                {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    public String serialize()
    {
        StringBuilder out = new StringBuilder("(list ");
        for (int i = 0;i<rows;i++)
        {
            out.append("(");
            for (int j = 0; j < cols; j++)
            {
                if (j > 0)
                    out.append(" ").append(weights[i][j]);
                else
                    out.append(weights[i][j]);
            }
            out.append(")");
        }
        out.append(")");
        return out.toString();
    }

    public static WeightMatrix deserialize(String data)
    {
        int rows = 0;
        int cols = 0;

        int start = 5;
        int end;
        int i;
        double[] row;
        LinkedList out = new LinkedList();
        while ((start = data.indexOf("(", start))!=-1)
        {
            end = data.indexOf(")", start);
            String[] nums = StringUtils.split(data.substring(start+1, end), ' ');
            cols = nums.length;
            row = new double[cols];
            for (i = 0;i<cols;i++)
            {
                String v = nums[i];
                row[i] = Double.parseDouble(v);
            }
            out.add(row);
            rows++;
            start = end+1;
        }

        double[][]  finalOut = new double[rows][];
        i = 0;
        for (Object v:out)
        {
            finalOut[i] = (double[])v;
            i++;
        }
        return new WeightMatrix(finalOut);
    }

    @Override
    public String toString()
    {
        return String.format("[%1$s X %2$s]", rows, cols);
    }

    @Override
    public boolean equals(Object o)
    {
        if (o == null)
            return false;
        if (o.getClass().equals(this.getClass()))
        {
            WeightMatrix other = (WeightMatrix)o;

            if (other.rows != rows || other.cols != cols)
                return false;

            for (int i=0;i<rows;i++)
            {
                for (int j=0;j<cols;j++)
                    if (weights[i][j] != other.weights[i][j])
                        return false;
            }
            return true;
        }
        else
            return false;
    }
}
