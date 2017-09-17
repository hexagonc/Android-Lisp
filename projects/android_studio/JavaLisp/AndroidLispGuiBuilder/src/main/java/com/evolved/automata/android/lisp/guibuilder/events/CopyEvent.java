package com.evolved.automata.android.lisp.guibuilder.events;

/**
 * Created by Evolved8 on 9/16/17.
 */

public class CopyEvent extends ALGBEvent {

    String text = null;

    private CopyEvent(String data)
    {
        super(ALGBEventTypes.COPY);
        text = data;
    }


    public static CopyEvent make(String text)
    {
        return new CopyEvent(text);
    }

    public String getText()
    {
        return text;
    }
}
