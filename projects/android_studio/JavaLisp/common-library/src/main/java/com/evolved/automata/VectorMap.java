package com.evolved.automata;

import org.apache.commons.math3.util.Pair;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

public class VectorMap {

    public static class Entry {
        public float[] _vectorKey;
        public Object _data;

        public Entry(float[] d, Object o){
            _vectorKey = d;
            _data = o;
        }

        @Override
        public String toString(){
            return "<" + Arrays.toString(_vectorKey) + " -> " + _data + ">";
        }
    }

    private static class SearchNode {
        Object[] options;
        float[] currentValue;

        SearchNode(Object[] o, float[] d){
            options = o;
            currentValue = d;
        }

        public SearchNode augment(int nextDimValue, Object[] nextOptions){
            float[] n = new float[currentValue.length + 1];
            for (int i = 0; i < currentValue.length;i++)
                n[i] = currentValue[i];
            n[currentValue.length] = nextDimValue;
            return new SearchNode(nextOptions, n);
        }
    }


    Object[] mData = null;

    public VectorMap(){
        mData = new Object[3];
    }

    public VectorMap mapVectorToValue(float[] data, Object value){
        Object[] stepArray = mData;
        int dataIndex = 0;
        int dataLength = data.length;
        int dimen = 0;

        while (dataIndex != dataLength){
            dimen = (int)data[dataIndex];

            if (stepArray[dimen] == null){
                 stepArray[dimen] = stepArray = new Object[3];
            }
            else
                stepArray = (Object[])stepArray[dimen];
            dataIndex++;
        }
        stepArray[2] = value;
        return this;
    }

    public Object getVectorValue(float[] data){
        Object[] stepArray = mData;
        int dataIndex = 0;
        int dataLength = data.length;

        int dimen;
        while (dataIndex != dataLength && stepArray != null){
            dimen = (int)data[dataIndex];
            stepArray = (Object[])stepArray[dimen];

            dataIndex++;
        }
        if (stepArray == null)
            return null;
        else
            return stepArray[2];
    }

    public Object removeKey(float[] data){
        Object[] stepArray = mData, prior = null;
        int dataIndex = 0;
        int dataLength = data.length;

        int dimen;
        while (dataIndex != dataLength && stepArray != null){
            dimen = (int)data[dataIndex];
            prior = stepArray;
            stepArray = (Object[])stepArray[dimen];

            dataIndex++;
        }
        if (stepArray == null)
            return null;
        else
        {
            Object out = stepArray[2];
            stepArray[2] = null;
            if (stepArray[0] == null && stepArray[1] == null && prior != null){
                prior[(int)data[data.length-1]] = null;
            }
            return out;
        }
    }


    public ArrayList<Entry> getEntryList(){
        ArrayList<Entry> entries = new ArrayList<Entry>();
        List<SearchNode> currentSearchNodes = new LinkedList<>();
        currentSearchNodes.add(new SearchNode(mData, new float[0]));
        List<SearchNode> nextNodes = null;

        while (currentSearchNodes.size()>0){
            nextNodes = new LinkedList<>();
            for (SearchNode s:currentSearchNodes){
                if (s.options[2] != null){
                    entries.add(new Entry(s.currentValue, s.options[2]));
                }
                for (int i = 0;i<2;i++){
                    if (s.options[i] != null){
                        nextNodes.add(s.augment(i, (Object[])s.options[i]));
                    }
                }
            }
            currentSearchNodes = nextNodes;
        }
        return entries;
    }

}
