package com.evolved.automata.android.lisp.guibuilder;

/**
 * Created by Evolved8 on 5/26/17.
 */

public abstract class PageStateEvent
{
    PageStateEventType _type;
    String _id;

    public PageStateEvent(String id, PageStateEventType type)
    {
        _type = type;
        _id = id;
    }

    public PageStateEventType getType()
    {
        return _type;
    }

    public String getId()
    {
        return _id;
    }


}
