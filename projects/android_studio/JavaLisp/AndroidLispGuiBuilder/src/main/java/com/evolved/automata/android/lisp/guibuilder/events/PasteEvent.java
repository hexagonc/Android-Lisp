package com.evolved.automata.android.lisp.guibuilder.events;

/**
 * Created by Evolved8 on 9/16/17.
 */

public class PasteEvent extends ALGBEvent {
    private PasteEvent()
    {
        super(ALGBEventTypes.PASTE);
    }

    public static PasteEvent make()
    {
        return new PasteEvent();
    }
}
