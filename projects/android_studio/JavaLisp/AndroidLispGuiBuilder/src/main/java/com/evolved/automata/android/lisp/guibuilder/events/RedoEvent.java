package com.evolved.automata.android.lisp.guibuilder.events;

/**
 * Created by Evolved8 on 9/17/17.
 */

public class RedoEvent extends ALGBEvent {
    private RedoEvent()
    {
        super(ALGBEventTypes.REDO);
    }

    public static RedoEvent make()
    {
        return new RedoEvent();
    }
}
