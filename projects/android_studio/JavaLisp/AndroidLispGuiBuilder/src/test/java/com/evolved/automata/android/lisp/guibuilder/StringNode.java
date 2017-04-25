package com.evolved.automata.android.lisp.guibuilder;

/**
 * Created by Evolved8 on 4/23/17.
 */

public class StringNode extends AtomNode {

    boolean mPreviousDelimiterP = false;
    public StringNode(ParseNode parent)
    {
        super(parent, TYPE.STRING);
    }


    @Override
    public boolean possibleFirstCharP(char firstChar)
    {
        return firstChar == '\"';
    }

    @Override
    public ParseStatus appendChar(char value)
    {
        if (mStatus == ParseStatus.INITIAL && value == '\"')
        {
            mValue.append(value);
            setStatus(ParseStatus.BUILDING);

        }
        else if (mStatus == ParseStatus.BUILDING && !mPreviousDelimiterP && value == '\"')
        {
            mValue.append(value);
            setStatus(ParseStatus.COMPLETE_ABSORB);
        }
        else if (mStatus == ParseStatus.BUILDING && !mPreviousDelimiterP && value == '\\')
        {
            mPreviousDelimiterP = true;
            mValue.append(value);
            setStatus(ParseStatus.BUILDING);
        }
        else if (mStatus == ParseStatus.BUILDING )
        {
            mValue.append(value);
            mPreviousDelimiterP = false;
        }
        else
        {
            setStatus(ParseStatus.ERROR);
        }
        return mStatus;
    }
}
