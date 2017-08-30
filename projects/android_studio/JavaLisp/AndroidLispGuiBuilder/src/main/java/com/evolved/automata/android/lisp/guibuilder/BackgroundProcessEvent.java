package com.evolved.automata.android.lisp.guibuilder;

import com.evolved.automata.lisp.editor.ParseNode;

/**
 * Created by Evolved8 on 8/29/17.
 */

public class BackgroundProcessEvent {
    public enum TYPE
    {
        STARTING, STOPPING, ERROR
    }

    String mId;
    String mName;
    TYPE mType;


    private BackgroundProcessEvent(String name, String id, TYPE type)
    {
        mId = id;
        mName = name;
        mType = type;
    }

    public String getName()
    {
        return mName;
    }

    public String getId()
    {
        return mId;
    }

    public TYPE getType()
    {
        return mType;
    }

    public static BackgroundProcessEvent makeProcessingStartedEvent(String name, String id)
    {
        return new BackgroundProcessEvent(name, id, TYPE.STARTING);
    }

    public static BackgroundProcessEvent makeProcessingFinishedEvent(String name, String id)
    {
        return new BackgroundProcessEvent(name, id, TYPE.STOPPING);
    }

    public static BackgroundProcessEvent makeProcessingErrorEvent(String name, String id)
    {
        return new BackgroundProcessEvent(name, id, TYPE.ERROR);
    }
}
