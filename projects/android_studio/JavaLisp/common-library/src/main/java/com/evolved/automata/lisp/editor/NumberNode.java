package com.evolved.automata.lisp.editor;

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
            setStatus(ParseStatus.BUILDING);
            mValue.append('-');
        }
        else if (Character.isDigit(value))
        {
            mValue.append(value);
            setStatus(ParseStatus.FINISHED);

        }
        else if (!mHasDecimalP && value == '.')
        {
            mValue.append(value);
            mHasDecimalP = true;
            setStatus(ParseStatus.BUILDING);
        }
        else if (Character.isWhitespace(value) || value == ')')
        {
            if (mStatus == ParseStatus.FINISHED)
            {
                setStatus(ParseStatus.COMPLETE_BOUNDARY);
            }
            else
                setStatus(ParseStatus.ERROR);;
        }
        else
            setStatus(ParseStatus.ERROR);

        return mStatus;
    }
}
