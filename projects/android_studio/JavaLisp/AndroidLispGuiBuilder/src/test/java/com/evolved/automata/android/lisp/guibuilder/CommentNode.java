package com.evolved.automata.android.lisp.guibuilder;

/**
 * Created by Evolved8 on 4/25/17.
 */

public class CommentNode extends AtomNode {
    public CommentNode(ParseNode parent)
    {
        super(parent, TYPE.COMMENT);
    }


    @Override
    public boolean possibleFirstCharP(char firstChar)
    {
        return firstChar == ';';
    }

    @Override
    public ParseStatus appendChar(char value)
    {
        if (mStatus == ParseStatus.INITIAL)
        {
            if (value == ';')
            {
                mValue.append(value);
                mStatus = ParseStatus.FINISHED;
            }
            else
            {
                mStatus = ParseStatus.ERROR;
            }

        }
        else if (mStatus == ParseStatus.FINISHED)
        {
            if (value == '\n')
            {
                mStatus = ParseStatus.COMPLETE_ABSORB;
            }
            mValue.append(value);
        }
        else
            return ParseStatus.COMPLETE_BOUNDARY;
        return mStatus;
    }
}
