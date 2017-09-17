package com.evolved.automata.android.lisp.guibuilder.events;

import com.evolved.automata.events.Event;

/**
 * Created by Evolved8 on 9/16/17.
 */

public class ALGBEvent extends Event {

    ALGBEventTypes type;

    public ALGBEvent(ALGBEventTypes ttype)
    {
        super(ttype.getEventName());
        type = ttype;
    }

    public ALGBEventTypes getType()
    {
        return type;
    }
}
