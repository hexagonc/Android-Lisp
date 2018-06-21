package com.evolved.automata.events;

/**
 * Created by Evolved8 on 7/2/17.
 *
 * The purpose of this is to allow the EventManager to communicate with the outside world,
 * requesting that clients post an event of a particular type.  This is to satisfy a dependency
 * for another Event that was previously posted
 */

public interface EventRequestDispatcher {
    void postRequest(EventRequest request);
    void requestEvent(String eventName);
}
