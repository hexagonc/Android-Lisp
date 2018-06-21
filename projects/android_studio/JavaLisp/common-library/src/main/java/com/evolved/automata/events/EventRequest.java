package com.evolved.automata.events;

/**
 * Created by Evolved8 on 7/2/17.
 */

public class EventRequest {
    String mEventName;


    protected EventRequest(String name)
    {
        mEventName = name;
    }

    public static EventRequest from(String name)
    {
        return new EventRequest(name);
    }

    public String getName()
    {
        return mEventName;
    }
}
