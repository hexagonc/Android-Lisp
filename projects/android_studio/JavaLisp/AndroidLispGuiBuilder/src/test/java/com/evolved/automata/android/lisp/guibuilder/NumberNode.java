package com.evolved.automata.android.lisp.guibuilder;

/**
 * Created by Evolved8 on 4/23/17.
 */

public class NumberNode extends AtomNode {


    boolean mHasDecimalP = false;
    boolean mHasMinusSignP = false;
    boolean mPrevDecimalP = false;

    public NumberNode(ParseNode parent)
    {
        super(parent, TYPE.NUMBER);
    }


    @Override
    public boolean possibleFirstCharP(char firstChar)
    {
        if (firstChar == '-')
        {

            return true;
        }

        return Character.isDigit(firstChar);
    }

    @Override
    public ParseStatus appendChar(char value)
    {
        if (value == '-' && mStatus == ParseStatus.INITIAL)
        {
            mHasMinusSignP = true;
            mStatus = ParseStatus.BUILDING;
            mValue.append('-');
        }
        else if (Character.isDigit(value))
        {
            mValue.append(value);
            mStatus = ParseStatus.FINISHED;
        }
        else if (!mHasDecimalP && value == '.')
        {
            mValue.append(value);
            mHasDecimalP = true;
            mStatus = ParseStatus.BUILDING;
        }
        else if (Character.isWhitespace(value))
        {
            if (mStatus == ParseStatus.FINISHED)
            {
                mStatus = ParseStatus.COMPLETE_BOUNDARY;
            }
        }
        else
            mStatus = ParseStatus.IN_COMPLETE;

        return mStatus;
    }
}
