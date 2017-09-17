package com.evolved.automata.android.lisp.guibuilder.events;

/**
 * Created by Evolved8 on 9/17/17.
 */

public class UndoEvent extends ALGBEvent {
    private UndoEvent()
    {
        super(ALGBEventTypes.UNDO);
    }

    public static UndoEvent make()
    {
        return new UndoEvent();
    }
}
