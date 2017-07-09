package com.evolved.automata.events;

/**
 * Created by Evolved8 on 7/2/17.
 */

public class ErrorEvent {
    Event mEvent;

    private ErrorEvent(Event e)
    {
        mEvent = e;
    }

    public Event getEvent()
    {
        return mEvent;
    }

    public static ErrorEvent from(Event e)
    {
        return new ErrorEvent(e);
    }
}
