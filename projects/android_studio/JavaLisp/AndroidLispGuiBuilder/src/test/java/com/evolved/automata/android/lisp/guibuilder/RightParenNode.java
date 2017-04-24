package com.evolved.automata.android.lisp.guibuilder;

/**
 * Created by Evolved8 on 4/23/17.
 */

public class RightParenNode extends AtomNode {
    public RightParenNode(ParseNode parent)
    {
        super(parent, TYPE.RIGHT_PAREN);
        mValue = new StringBuilder(")");
        mStatus = ParseStatus.COMPLETE_ABSORB;
    }

    @Override
    public boolean possibleFirstCharP(char firstChar)
    {
        return firstChar == ')';
    }

    @Override
    public ParseStatus appendChar(char value)
    {
        return ParseStatus.COMPLETE_BOUNDARY;
    }
}
