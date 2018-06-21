package com.evolved.automata.speech;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

public class TokenizedString {

    final ArrayList<String> mTokens;
    String[] mArray;
    private TokenizedString(ArrayList<String> tokens)
    {
        mTokens = tokens;
    }

    public String[] getArray()
    {
        if (mArray == null)
            mArray = mTokens.toArray(new String[0]);
        return mArray;
    }

    public List<String> getList()
    {
        return mTokens;
    }

    public static TokenizedString make(String phrase)
    {
        String[] parts = StringUtils.splitByWholeSeparator(phrase, " ");
        ArrayList<String> tokens = new ArrayList<String>();
        for (String s:parts)
        {
            tokens.add(s);
        }
        return new TokenizedString(tokens);
    }
}
