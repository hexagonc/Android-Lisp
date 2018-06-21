package com.evolved.automata.events;

/**
 * Created by Evolved8 on 7/2/17.
 */

public interface EventAction {
    void doAction(Event event, EventManager manager);
}
