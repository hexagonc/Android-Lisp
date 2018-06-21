package com.evolved.automata.speech;

import java.util.HashMap;
import java.util.LinkedList;

public abstract class Pattern {

    String mName;


    public void setName(String name)
    {
        mName = name;
    }



    public String getName()
    {
        return mName;
    }





    public abstract LinkedList<MatchContinuation> getBestMatchingPrefix(String[] inputTokens, MatchConfiguration config);



}
