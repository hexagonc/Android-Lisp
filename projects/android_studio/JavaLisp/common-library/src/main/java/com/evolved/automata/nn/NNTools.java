package com.evolved.automata.nn;

import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;

/**
 * Created by Evolved8 on 12/16/16.
 */
public class NNTools {
    public static ArrayList<Double> stageDiscretize(double value, double range, int bits )
    {
        return stageDiscretizeAroundPivot(value, 0, range, bits);
    }

    public static ArrayList<Double> stageDiscretizeAroundPivot(double value, double min, double max, int steps)
    {
        double pivot = (max + min)/2;
        boolean leq_pivot = value <= pivot;
        double range = max - min;
        boolean closer_than_quarter_range = (Math.abs(value - pivot) < range/4);
        ArrayList<Double> base = new ArrayList<Double>();
        if (leq_pivot)
        {
            base.add(0.0);
        }
        else
            base.add(1.0);

        if (closer_than_quarter_range)
        {
            base.add(1.0);
        }
        else
            base.add(0.0);

        if (steps > 1)
        {
            base.addAll(stageDiscretizeAroundPivot(
                    value,
                    (leq_pivot)?((closer_than_quarter_range)?(pivot - range/4):min):( (closer_than_quarter_range)?pivot:pivot + range/4),
                    (leq_pivot)?((closer_than_quarter_range)?pivot:pivot - range/4):( (closer_than_quarter_range)?pivot + range/4:max),
                    steps - 1));
            return base;
        }
        else
            return base;

    }


    public static Pair<Double, Double>  stageContinuize(double range, ArrayList<Double> discretized)
    {
        return stageContinuize(0, range, discretized);
    }

    public static Pair<Double, Double> stageContinuize(double min, double max, ArrayList<Double> discretized)
    {
        boolean greater_than_midpoint = false;
        double width = max - min;;
        double v_i;
        int i = 0;
        boolean flag;
        double tmin;
        for (i = 0;i< discretized.size();i++)
        {
            v_i = discretized.get(i);
            flag = v_i > 0.5;
            if ( i % 2 == 0)
            {
                greater_than_midpoint = flag;
                width = max - min;
            }
            else if (greater_than_midpoint)
            {
                if (flag)
                {
                    tmin = (max - width/2);
                    max = min + width/2 + width/4;
                    min = tmin;
                }
                else
                {
                    min = max - width/4;
                }
            }
            else
            {
                if (flag)
                {
                    tmin = (min + width/4);
                    max = min + width/2;
                    min = tmin;
                }
                else
                {
                    max = min + width/4;
                }
            }

        }

        return Pair.of(min, max);
    }



    private static double[] conv(Double[] d)
    {
        double[] o = new double[d.length];
        for (int i = 0;i<o.length;i++)
            o[i] = d[i];
        return o;
    }


}
