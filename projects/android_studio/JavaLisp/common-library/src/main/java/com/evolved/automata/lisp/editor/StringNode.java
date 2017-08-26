package com.evolved.automata.lisp.editor;

import java.util.HashMap;

/**
 * Created by Evolved8 on 4/23/17.
 */

public class StringNode extends AtomNode {

    boolean mPreviousDelimiterP = false;
    public StringNode(ParseNode parent)
    {
        super(parent, TYPE.STRING);
    }

    public StringNode()
    {
        super(TYPE.STRING);
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

    protected void fill(String[] fields, HashMap<Integer, ParseNode> inverseMap)
    {
        super.fill(fields, inverseMap);
        mPreviousDelimiterP = toBoolean(fields[4]);
    }

    protected String serialize(HashMap<ParseNode, Integer> nodeIndex)
    {
        return new StringBuilder(super.serialize(nodeIndex)).append(STANDARD_FIELD_SEPARATOR).append(serialize(mPreviousDelimiterP)).toString();

    }
}
