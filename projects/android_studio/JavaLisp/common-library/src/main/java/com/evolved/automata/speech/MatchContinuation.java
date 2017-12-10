package com.evolved.automata.speech;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;

public class MatchContinuation {
    String[] _remainingTokens;

    int _prefixEditDistance;

    SpeechValue mValue = null;

    int mNextIndex = 0;

    MatchContinuation mPrevious = null;

    public MatchContinuation(int distance, String[] remaining)
    {
        _remainingTokens = remaining;
        _prefixEditDistance = distance;
    }

    public MatchContinuation(SpeechValue value, int distance, String[] remaining, MatchContinuation previous)
    {
        mValue = value;
        _remainingTokens = remaining;
        _prefixEditDistance = distance;
        mPrevious = previous;
    }

    public MatchContinuation getPreviousResult()
    {
        return mPrevious;
    }

    public MatchContinuation(SpeechValue value, int distance, String[] remaining, int index)
    {
        mValue = value;
        _remainingTokens = remaining;
        _prefixEditDistance = distance;
        mNextIndex = index;
        mPrevious = null;
    }


    public MatchContinuation(SpeechValue value, int distance, String[] remaining, int index, MatchContinuation previous)
    {
        mValue = value;
        _remainingTokens = remaining;
        _prefixEditDistance = distance;
        mNextIndex = index;
        mPrevious = previous;
    }

    public int getIndex()
    {
        return mNextIndex;
    }

    public SpeechValue getValue()
    {
        return mValue;
    }

    void setValue(SpeechValue v)
    {
        mValue = v;
    }

    public int getPriorDistance()
    {
        return _prefixEditDistance;
    }

    public String[] getRemainingTokens()
    {
        return _remainingTokens;
    }

    @Override
    public String toString()
    {
        return "<" + _prefixEditDistance + ", " + Arrays.toString(_remainingTokens) + " : " + getValueString() + ">";
    }

    private String getValueString()
    {
        if (mValue == null)
            return "";
        else {

            return mValue.toString();
        }
    }

    public LinkedList<MatchContinuation> getPath()
    {
        LinkedList<MatchContinuation> result = new LinkedList<MatchContinuation>();

        MatchContinuation segment = new MatchContinuation(mValue, _prefixEditDistance, _remainingTokens, mNextIndex, mPrevious);
        while (segment != null)
        {
            result.addFirst(segment);
            segment = segment.getPreviousResult();
        }
        return result;
    }
}
