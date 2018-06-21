package com.evolved.automata.speech;

import java.util.HashMap;

public class MatchConfiguration {
    HashMap<String, Integer> mInsertCostMap;
    HashMap<String, Integer> mDeleteCostMap;
    int mDefaultInsertCost = 1;
    int mDefaultDeleteCost = 1;
    int mSubstitutionCost = 1;
    HashMap<String, String> mCanonicalWordMap;

    int mPrefixPadding = 0;

    ValueConstructor mConstructor;

    public MatchConfiguration()
    {
        mCanonicalWordMap = new HashMap<String, String>();
    }

    public MatchConfiguration copy()
    {
        MatchConfiguration config = new MatchConfiguration();
        config.mCanonicalWordMap = mCanonicalWordMap;
        config.mInsertCostMap = mInsertCostMap;
        config.mDefaultDeleteCost = mDefaultDeleteCost;
        config.mDefaultDeleteCost = mDefaultDeleteCost;
        config.mDefaultInsertCost = mDefaultInsertCost;
        config.mSubstitutionCost = mSubstitutionCost;
        config.mConstructor = mConstructor;
        return config;
    }

    public MatchConfiguration addEquivalentWords(String left, String right)
    {
        String rightGroup = getGroupWord(right);
        String leftGroup = getGroupWord(left);
        mCanonicalWordMap.put(rightGroup, leftGroup);
        return this;
    }

    public MatchConfiguration setValueConstructor(ValueConstructor constructor)
    {
        mConstructor = constructor;
        return this;
    }

    public ValueConstructor getValueConstructor()
    {

        return mConstructor;
    }

    public ValueConstructor removeValueConstructor()
    {
        ValueConstructor out = mConstructor;
        mConstructor = null;
        return out;
    }


    public MatchConfiguration setDefaultInsertCost(int cost)
    {
        mDefaultInsertCost = cost;
        return this;
    }

    public MatchConfiguration setDefaultDeleteCost(int cost)
    {
        mDefaultDeleteCost = cost;
        return this;
    }

    public MatchConfiguration setDefaultSubCost(int cost)
    {
        mSubstitutionCost = cost;
        return this;
    }

    public MatchConfiguration setInsertCostMap(HashMap<String, Integer> mInsertCostMap)
    {
        this.mInsertCostMap = mInsertCostMap;
        return this;
    }

    public MatchConfiguration setDeleteCostMap(HashMap<String, Integer> mDeleteCostMap)
    {
        this.mDeleteCostMap = mDeleteCostMap;
        return this;
    }

    public MatchConfiguration setSynonymMap(HashMap<String, String> synonymMap)
    {
        this.mCanonicalWordMap =  synonymMap;
        return this;
    }

    public String getGroupWord(String input)
    {
        String nextWord = null;
        while (mCanonicalWordMap!=null && (nextWord = mCanonicalWordMap.get(input)) != null && !nextWord.equals(input))
        {
            input = nextWord;
        }
        return input;
    }


    public int getDeleteCost(String inputToken)
    {
        if (mDeleteCostMap == null)
            return mDefaultDeleteCost;

        Integer mappedCost = mDeleteCostMap.get(inputToken);
        if (mappedCost != null)
            return mappedCost;
        else
            return mDefaultDeleteCost;
    }

    public int getInsertCost(String patternToken)
    {
        if (mInsertCostMap == null)
            return mDefaultInsertCost;

        Integer mappedCost = mInsertCostMap.get(patternToken);
        if (mappedCost != null)
            return mappedCost;
        else
            return mDefaultInsertCost;

    }

    public int getSubstitutionCost(String inputToken, String patternToken)
    {
        return mSubstitutionCost;
    }

    public MatchConfiguration addDeleteCostWord(String inputToken, int cost)
    {
        if (mDeleteCostMap == null)
        {
            mDeleteCostMap = new HashMap<String, Integer>();
        }
        mDeleteCostMap.put(inputToken, cost);
        return this;
    }
}
