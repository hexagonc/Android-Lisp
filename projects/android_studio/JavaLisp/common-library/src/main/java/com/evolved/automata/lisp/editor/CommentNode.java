package com.evolved.automata.lisp.editor;

/**
 * Created by Evolved8 on 4/25/17.
 */

public class CommentNode extends AtomNode {
    public CommentNode(ParseNode parent)
    {
        super(parent, TYPE.COMMENT);
    }

    public CommentNode()
    {
        super(TYPE.COMMENT);
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
                setStatus(ParseStatus.FINISHED);

            }
            else
            {
                setStatus(ParseStatus.ERROR);
            }

        }
        else if (mStatus == ParseStatus.FINISHED)
        {
            if (value == '\n')
            {
                setStatus(ParseStatus.COMPLETE_ABSORB);

            }
            mValue.append(value);
        }
        else
            return ParseStatus.COMPLETE_BOUNDARY;
        return mStatus;
    }
}
