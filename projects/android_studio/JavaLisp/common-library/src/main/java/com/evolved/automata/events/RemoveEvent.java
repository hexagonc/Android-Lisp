package com.evolved.automata.events;

/**
 * Created by Evolved8 on 7/2/17.
 */

public class RemoveEvent {
    String mEventName;

    private RemoveEvent(String name)
    {
        mEventName = name;
    }

    public String getEventNameToRemove()
    {
        return mEventName;
    }

    public static RemoveEvent from(String name)
    {
        return new RemoveEvent(name);
    }
}
