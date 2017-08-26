package com.evolved.automata.lisp.editor;

/**
 * Created by Evolved8 on 4/23/17.
 */

public class WhiteSpaceNode extends AtomNode {
    public WhiteSpaceNode(ParseNode parent)
    {
        super(parent, TYPE.WHITE_SPACE);
    }

    public WhiteSpaceNode()
    {
        super(TYPE.WHITE_SPACE);
    }

    @Override
    public boolean possibleFirstCharP(char firstChar)
    {
        return Character.isWhitespace(firstChar);
    }


    @Override
    public ParseStatus appendChar(char value)
    {

        if (Character.isWhitespace(value))
        {
            mValue.append(value);

            return setStatus(ParseStatus.FINISHED);
        }
        else
        {
            if (mStatus != ParseStatus.FINISHED)
                return setStatus(ParseStatus.ERROR);
            else
                return setStatus(ParseStatus.COMPLETE_BOUNDARY);
        }
    }


}
