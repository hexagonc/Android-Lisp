package com.evolved.automata.lisp.editor;

/**
 * Created by Evolved8 on 4/25/17.
 */

public class ErrorTextNode extends AtomNode {
    public ErrorTextNode(ParseNode parent)
    {
        super(parent, TYPE.INVALID_CHARS);
        setContext(parent.getParseContext());
    }


    @Override
    public boolean possibleFirstCharP(char firstChar)
    {
        return false;
    }

    @Override
    public ParseStatus appendChar(char value)
    {
        mValue.append(value);
        return setStatus(ParseStatus.ERROR);
    }
}
