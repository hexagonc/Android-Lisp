package com.evolved.automata.android.guibuilder.core.processing;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import de.greenrobot.event.EventBus;
import de.greenrobot.event.EventBusBuilder;

/**
 * Created by Evolved8 on 9/12/16.
 */
public class EventManager {


    TreeMap<Long, HashSet<Class<? extends Event>>> _eventHistory;
    HashMap<Class<? extends Event>, Event> _stateEvents;
    EventBus _bus;

    Logger _eventLogger;
    private EventManager(Logger logger)
    {
        EventBusBuilder evb = EventBus.builder();
        _bus = evb.build();
        _stateEvents = new HashMap<Class<? extends Event>, Event>();
        _eventLogger = logger;
        _eventHistory = new TreeMap<Long, HashSet<Class<? extends Event>>>();
    }

    public static EventManager create(Logger logger)
    {
        return new EventManager(logger);

    }

    /**
     *
     * @return a shallow copy of all sticky events that have not been removed from the Eventbus
     */
    public synchronized HashMap<Class<? extends Event>, Event> getCurrentStateEvents()
    {
        HashMap<Class<? extends Event>, Event> out = new HashMap<>();
        for (Class<? extends Event> eventClass:_stateEvents.keySet())
        {
            out.put(eventClass, _stateEvents.get(eventClass));
        }
        return out;
    }

    public EventManager registerEventHandler(Object subscriber)
    {
        _bus.register(subscriber);
        return this;
    }

    public EventManager unRegisterEventHandler(Object subscriber)
    {
        _bus.unregister(subscriber);
        return this;
    }


    /**
     *
     * @param action this is an event that is intended to represent something like a command.  One of the
     *               subscribers should handle this event.  Semantically, "action" events are never
     *               sticky events
     * @return
     */
    public EventManager postActionEvent(Event action)
    {
        _eventLogger.log(Logger.Level.INFO, "[o.o] New action event", action.getName());
        _bus.post(action);
        addToHistory(action);
        return this;
    }


    /**
     * Posts a state event.  State events
     * @param state
     * @return
     */
    public synchronized EventManager postStateEvent(Event state)
    {
        _eventLogger.log(Logger.Level.INFO, "[+.+] New state event", state.getName());
        addToHistory(state);
        _bus.postSticky(state);
        return this;
    }

    synchronized void addToHistory(Event event)
    {
        Long postTime = Long.valueOf(System.currentTimeMillis());

        HashSet<Class<? extends Event>> moment = _eventHistory.get(postTime);
        if (moment == null){
            moment = new HashSet<Class<? extends Event>>();

            _eventHistory.put(postTime, moment);
        }

        moment.add(event.getClass());
    }

    /**
     * Tests if a stateEvent is currently active
     * @param eventClass
     * @return
     */
    public synchronized boolean hasStateEvent(Class<? extends Event> eventClass)
    {
        return _stateEvents.containsKey(eventClass);
    }

    /**
     * Attempts to remove a state event.
     * @param eventClass
     * @return true if the event was still active at point of removal.  Otherwise false
     */
    public synchronized boolean removeStateEvent(Class<? extends Event> eventClass)
    {
        boolean wasPresent = hasStateEvent(eventClass);
        if (wasPresent)
        {
            _bus.removeStickyEvent(eventClass);
            _stateEvents.remove(eventClass);
        }
        return wasPresent;
    }


    public EventManager stopFurtherEventPropagation(Event event)
    {
        _bus.cancelEventDelivery(event);
        return this;
    }


    /**
     *
     * @param millisecondsAgo
     * @return
     */
    public synchronized HashSet<Class<? extends Event>> getAllEventsAfter(int millisecondsAgo)
    {
        Long cutoff = Long.valueOf(System.currentTimeMillis() - millisecondsAgo);
        HashSet<Class<? extends Event>> eventSet = new HashSet<Class<? extends Event>>();

        for (HashSet<Class<? extends Event>>momentSet: _eventHistory.tailMap(cutoff).values())
        {
            for (Class<? extends Event> state:momentSet)
            {
                eventSet.add(state);
            }

        }

        return eventSet;
    }


    public synchronized EventManager removeAllOldEvents(int millisecondsAgo)
    {
        Long cutoff = Long.valueOf(System.currentTimeMillis() - millisecondsAgo);
        for (Long olderTime :_eventHistory.headMap(cutoff).keySet())
        {
            _eventHistory.remove(olderTime);
        }
        return this;

    }

}
