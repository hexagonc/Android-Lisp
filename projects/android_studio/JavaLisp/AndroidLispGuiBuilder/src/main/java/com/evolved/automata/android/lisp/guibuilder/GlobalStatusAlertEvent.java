package com.evolved.automata.android.lisp.guibuilder;

/**
 * Created by Evolved8 on 5/27/17.
 */

public abstract class GlobalStatusAlertEvent {
    final StatusAlertType mType;

    protected GlobalStatusAlertEvent(StatusAlertType type)
    {
        mType = type;
    }

    public StatusAlertType getType()
    {
        return mType;
    }
}
