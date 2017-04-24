package com.evolved.automata.android.lisp.guibuilder;

/**
 * Created by Evolved8 on 4/23/17.
 */

public abstract class AtomNode extends ParseNode {
    public AtomNode(ParseNode parent, TYPE type)
    {
        super(parent, type);
    }

    @Override
    public int getChildOffset()
    {
        return 0;
    }

    @Override
    public String getValue()
    {
        return mValue.toString();
    }
}
