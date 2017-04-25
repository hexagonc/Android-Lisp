package com.evolved.automata.android.lisp.guibuilder;

/**
 * Created by Evolved8 on 4/23/17.
 */

public class TopParseNode extends CompositeNode {

    public TopParseNode()
    {
        super(null, TYPE.TOP);
        acceptWorkingChildStatusP = true;
    }

    public boolean possibleFirstCharP(char firstChar)
    {
        return true;
    }

    @Override
    public int getChildOffset()
    {
        return 0;
    }


}
