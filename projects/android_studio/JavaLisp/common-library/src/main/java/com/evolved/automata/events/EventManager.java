package com.evolved.automata.events;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.RunnableFuture;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * Created by Evolved8 on 7/2/17.
 */

public class EventManager {


    public interface EventPredicate
    {
        boolean isTrue(Event e);
    }


    ThreadPoolExecutor mBackgroundExecutor;

    public enum ThreadMode
    {
        MAIN, BACKGROUND, CURRENT
    }

    public static final String NEGATE_EVENT_PREFIX = "~";
    protected volatile HashMap<String, Event> mEventMap;
    protected volatile HashMap<String, Long> mRequestMap;
    protected HashMap<String, EventConstructor> mEventConstructors;
    protected EventRequestDispatcher mRequestDispatcher;

    protected Object synchObject = new Object();

    Annotation threadModeAnnotation;

    protected EventManager()
    {
        mEventMap = new HashMap<String, Event>();
        mRequestMap = new HashMap<String, Long>();
        mEventConstructors = new HashMap<String, EventConstructor>();
        mBackgroundExecutor = new ThreadPoolExecutor(8, 10, 2000, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<Runnable>());

        Class<Event> clazz = (Class<Event>)Event.class;
        Class<EventThreadMode> annotationClass = EventThreadMode.class;
        threadModeAnnotation = clazz.getAnnotation(annotationClass);
    }

    protected EventManager(EventRequestDispatcher requestDispatcher)
    {
        super();
        mRequestDispatcher = requestDispatcher;
    }



    public static EventManager make(EventRequestDispatcher requestDispatcher)
    {
        return new EventManager(requestDispatcher);
    }

    public EventManager setEventConstructor(String eventName, EventConstructor constructor)
    {
        mEventConstructors.put(eventName, constructor);
        return this;
    }



    public EventManager removeEvent(String name)
    {
        return removeEvent(name, true);
    }

    private EventManager removeEvent(String name, final boolean reprocessEvents)
    {
        synchronized (synchObject)
        {
            Event event = mEventMap.get(name);
            if (event != null)
            {
                try
                {
                    Runnable onCmplete = new Runnable()
                    {
                        public void run()
                        {
                            if (reprocessEvents)
                                processAllEvents();
                        }
                    };
                    onEventRemoved(name, onCmplete);
                }
                catch (Exception e)
                {
                    event.setError(e);
                }


            }
        }

        return this;
    }


    private void onEventRemoved(String name)
    {
        onEventRemoved(name, null);
    }
    private void onEventRemoved(String name, Runnable onComplete)
    {
        Event removedEvent = mEventMap.get(name);
        if (removedEvent.getStatus() != EventStatus.COMPLETE)
        {
            if (onComplete != null)
                onComplete.run();
            return;
        }

        LinkedList<Event> dependentEvents = getEventDependents(removedEvent, new EventPredicate() {
            @Override
            public boolean isTrue(Event e)
            {
                return e.getStatus() == EventStatus.COMPLETE;
            }
        }, false);

        dependentEvents.add(removedEvent);
        removeEventStep(dependentEvents.iterator(), onComplete);

    }

    private void removeEventStep(final Iterator<Event> iterator, final Runnable onComplete)
    {
        if (iterator.hasNext())
        {
            final Event toRemove = iterator.next();
            ThreadMode mode = getOnRemovedThreadMode(toRemove);

            Runnable toDo = new Runnable()
            {
                public void run()
                {
                    toRemove.onRemoved(EventManager.this);
                    mEventMap.remove(toRemove.getName());
                    removeEventStep(iterator, onComplete);
                }
            };

            switch (mode)
            {
                case MAIN:
                    runMainThread(toDo);
                    break;
                case BACKGROUND:
                    runBackgroundThread(toDo);
                    break;
                case CURRENT:
                    toDo.run();
                    break;
            }

        }
        else
        {

            if (onComplete != null)
                onComplete.run();
        }
    }


    public void replaceEvent(final Event event)
    {
        Runnable onComplete = new Runnable()
        {
            public void run()
            {
                onEvent(event);
            }
        };
        onEventRemoved(event.getName(), onComplete);
    }

    public void updateEventData(Event event, Object data)
    {
        updateEventData(event.getName(), data);
    }

    public Event getEvent(String name)
    {
        return mEventMap.get(name);
    }

    public synchronized void onEvent(Event event)
    {
        String name = event.getName();
        if (mRequestMap.containsKey(name))
            mRequestMap.remove(name);
        Event prior = mEventMap.get(name);


        if (prior == null || prior.getStatus() != EventStatus.WORKING)
        {
            mEventMap.put(name, event);
        }
        processAllEvents();
    }

    public boolean finishedP()
    {
        for (String name:mEventMap.keySet())
        {
            Event e = mEventMap.get(name);
            if (e.getStatus() == EventStatus.WORKING || e.getStatus() == EventStatus.OPEN)
                return false;
        }
        return true;
    }

    public static boolean isNegatedEvent(String name)
    {
        return name.startsWith(NEGATE_EVENT_PREFIX);
    }

    public boolean isNegatedEventPresent(String negatedEvent)
    {
        String baseEventName = negatedEvent.substring(NEGATE_EVENT_PREFIX.length());
        return !mEventMap.containsKey(baseEventName) || mEventMap.get(baseEventName).getStatus() != EventStatus.COMPLETE;
    }

    void requestExternalEvent(String name)
    {
        if (mRequestDispatcher != null)
        {
            mRequestDispatcher.postRequest(EventRequest.from(name));
            return;
        }
    }
    void requestEvent(String desiredEventName)
    {
        if (mEventConstructors.containsKey(desiredEventName))
        {
            EventConstructor constructor = mEventConstructors.get(desiredEventName);
            onEvent(constructor.makeEvent());
            return;
        }
        else
        {
            requestExternalEvent(desiredEventName);
        }
    }

    // TODO: consider randomizing this or using a priority queue
    protected synchronized void processNextFromRequestMap()
    {
        if (mRequestMap.size()>0)
        {
            Long oldestTime = null;
            String oldestRequestName = null;
            String[] eventNames = mRequestMap.keySet().toArray(new String[0]);
            for (String desiredEventName:eventNames)
            {
                Long requestTime = mRequestMap.get(desiredEventName);
                if (requestTime != null) // make sure event still exists
                {
                    if (oldestTime == null || requestTime < oldestTime)
                    {
                        oldestTime = requestTime;
                        oldestRequestName = desiredEventName;
                    }
                }
            }

            if (oldestTime != null)
            {
                mRequestMap.put(oldestRequestName, System.currentTimeMillis());
                requestEvent(oldestRequestName);
            }
        }
    }


    protected boolean prereqsMet(Event e)
    {

        // TODO: This is stupid. Figure out why the hell AndroidStudio throws errors on on this version of the loop.
        /*
        for (String name:e.getPrerequisites())
        {

            if (isNegatedEvent(name))
            {
                String baseEventName = name.substring(1);
                if (mEventMap.containsKey(baseEventName))
                    return false;
            }
            else
            {
                Event event = mEventMap.get(name);
                if (event == null || event.getStatus() != EventStatus.COMPLETE)
                    return false;
            }
        }
         */
        LinkedHashSet<String> stupid = e.getPrerequisites();
        for (String name:stupid)
        {

            if (isNegatedEvent(name))
            {
                String baseEventName = name.substring(1);
                if (mEventMap.containsKey(baseEventName))
                    return false;
            }
            else
            {
                Event event = mEventMap.get(name);
                if (event == null || event.getStatus() != EventStatus.COMPLETE)
                    return false;
            }
        }
        return true;
    }

    ThreadMode getOnRemovedThreadMode(Event event)
    {
        try
        {
            Class clazz = event.getClass();
            Method method = clazz.getMethod("onRemoved", new Class[]{EventManager.class});
            Annotation[] annotations = method.getAnnotations();
            for (Annotation annot:annotations)
            {
                if (annot.annotationType() == EventThreadMode.class)
                {
                    EventThreadMode eventThreadMode = (EventThreadMode)annot;
                    if ( ThreadMode.CURRENT.name().equalsIgnoreCase(eventThreadMode.mode()))
                    {
                        return ThreadMode.CURRENT;
                    }
                    else if (ThreadMode.BACKGROUND.name().equalsIgnoreCase(eventThreadMode.mode()))
                    {
                        return ThreadMode.BACKGROUND;
                    }
                    else if (ThreadMode.MAIN.name().equalsIgnoreCase(eventThreadMode.mode()))
                    {
                        return ThreadMode.MAIN;
                    }
                }
            }

        }
        catch (Exception e)
        {
            // not possible
            e.printStackTrace();
            throw new RuntimeException(e);

        }
        return ThreadMode.CURRENT;

    }


    ThreadMode getActionThreadMode(Event event)
    {
        try
        {
            Class clazz = event.getClass();
            Method method = clazz.getMethod("doActionInternal", new Class[]{EventManager.class});
            Annotation[] annotations = method.getAnnotations();
            for (Annotation annot:annotations)
            {
                if (annot.annotationType() == EventThreadMode.class)
                {
                    EventThreadMode eventThreadMode = (EventThreadMode)annot;
                    if ( ThreadMode.CURRENT.name().equalsIgnoreCase(eventThreadMode.mode()))
                    {
                        return ThreadMode.CURRENT;
                    }
                    else if (ThreadMode.BACKGROUND.name().equalsIgnoreCase(eventThreadMode.mode()))
                    {
                        return ThreadMode.BACKGROUND;
                    }
                    else if (ThreadMode.MAIN.name().equalsIgnoreCase(eventThreadMode.mode()))
                    {
                        return ThreadMode.MAIN;
                    }
                }
            }

        }
        catch (Exception e)
        {
            // not possible
            e.printStackTrace();
            throw new RuntimeException(e);

        }
        return ThreadMode.CURRENT;

    }

    ThreadMode getPrereqsUpdatedThreadMode(Event event)
    {
        try
        {
            Class clazz = event.getClass();
            Method method = clazz.getMethod("onPrereqsUpdated", new Class[]{EventManager.class});
            Annotation[] annotations = method.getAnnotations();
            for (Annotation annot:annotations)
            {
                if (annot.annotationType() == EventThreadMode.class)
                {
                    EventThreadMode eventThreadMode = (EventThreadMode)annot;
                    if ( ThreadMode.CURRENT.name().equalsIgnoreCase(eventThreadMode.mode()))
                    {
                        return ThreadMode.CURRENT;
                    }
                    else if (ThreadMode.BACKGROUND.name().equalsIgnoreCase(eventThreadMode.mode()))
                    {
                        return ThreadMode.BACKGROUND;
                    }
                    else if (ThreadMode.MAIN.name().equalsIgnoreCase(eventThreadMode.mode()))
                    {
                        return ThreadMode.MAIN;
                    }
                }
            }

        }
        catch (Exception e)
        {
            // not possible
            e.printStackTrace();
            throw new RuntimeException(e);

        }
        return ThreadMode.CURRENT;
    }






    public LinkedList<Event> getEventDependents(Event e)
    {
        return getEventDependents(e, null);
    }

    public LinkedList<Event> getEventDependents(Event e, EventPredicate pred)
    {
        return getEventDependents(e, pred, true);
    }

    public LinkedList<Event> getEventDependents(Event e, EventPredicate pred, boolean sortAscending)
    {
        String name = e.getName();
        HashMap<String, HashMap<String, Boolean>> dependencyProcessingInfo = new HashMap<String, HashMap<String, Boolean>>();
        HashSet<String> unProcessedEvents = new HashSet<String>();
        Event childEvent;
        for (String childName:mEventMap.keySet())
        {
            childEvent = mEventMap.get(childName);
            if (!childName.equals(name) && pred != null && pred.isTrue(childEvent))
                unProcessedEvents.add(childName);
        }

        HashSet<String> processedParents = new HashSet<String>();
        processedParents.add(name);

        LinkedList<Event> updateOrder = new LinkedList<Event>();


        boolean cont = false;
        do
        {
            cont = false;

            next: for (String childName:unProcessedEvents.toArray(new String[0]))
            {

                childEvent = mEventMap.get(childName);

                boolean hasUpdatedParents = false;
                HashMap<String, Boolean> childDependencyProcessing = dependencyProcessingInfo.get(childName);

                int testCount = 0, matchCount = 0;
                if (childDependencyProcessing == null)
                {
                    childDependencyProcessing = new HashMap<String, Boolean>();
                    dependencyProcessingInfo.put(childName, childDependencyProcessing);
                    for (String dependency:childEvent.getPrerequisites())
                    {
                        if (!isNegatedEvent(dependency))
                            childDependencyProcessing.put(dependency, Boolean.FALSE);
                    }
                }
                testCount = childDependencyProcessing.size();

                for (String dependency:childDependencyProcessing.keySet())
                {
                    Boolean isProcessed = childDependencyProcessing.get(dependency);

                    if (!isProcessed)
                    {
                        if (processedParents.contains(dependency))
                        {
                            matchCount++;
                            isProcessed = Boolean.TRUE;
                            hasUpdatedParents = true;
                        }
                        else
                        {
                            HashMap<String, Boolean> dependencyProcessing = dependencyProcessingInfo.get(dependency);
                            if (dependencyProcessing != null)
                            {
                                int dependencyTestCount = dependencyProcessing.entrySet().size(), dependencyMatchCount = 0;
                                for (Map.Entry<String, Boolean> entry:dependencyProcessing.entrySet())
                                {
                                    if (!entry.getValue())
                                    {
                                        break;
                                    }
                                    else
                                        dependencyMatchCount++;
                                }
                                isProcessed = Boolean.valueOf(dependencyMatchCount == dependencyTestCount);
                            }

                        }
                        childDependencyProcessing.put(dependency, isProcessed);
                    }
                    else
                    {
                        matchCount++;
                        hasUpdatedParents = hasUpdatedParents || processedParents.contains(dependency);
                    }

                }

                if (testCount == matchCount)
                {
                    if (hasUpdatedParents)
                    {

                        processedParents.add(childName);
                        if (sortAscending)
                            updateOrder.add(childEvent);
                        else
                            updateOrder.addFirst(childEvent);
                        cont = true;
                    }
                    unProcessedEvents.remove(childName);
                }
                else
                    cont = true;

            }

        }while(cont);

        return updateOrder;
    }


    public void updateEventData(String name, Object data)
    {
        Event updateEvent = mEventMap.get(name);

        if (updateEvent != null && updateEvent.getStatus() == EventStatus.COMPLETE)
        {
            updateEvent.setData(data);


            LinkedList<Event> dependents = getEventDependents(updateEvent, new EventPredicate() {
                @Override
                public boolean isTrue(Event e)
                {
                    return e.getStatus() == EventStatus.COMPLETE;
                }
            });

            updateEvent.setDataUpdated();
            updateDependentEvents(updateEvent, dependents, 0);
        }
    }

    protected void updateDependentEvents(final Event baseEvent, final LinkedList<Event> dependents, final int updateIndex)
    {
        if (dependents.size() <= updateIndex)
        {
            for (Event e:dependents)
            {
                e.clearDataUpdatedFlag();
            }
            baseEvent.clearDataUpdatedFlag();
        }
        else
        {
            final Event dependent = dependents.get(updateIndex);
            ThreadMode mode = getPrereqsUpdatedThreadMode(dependent);
            switch (mode)
            {
                case CURRENT:
                    try
                    {
                        dependent.onPrereqsUpdated(this);
                    }
                    catch (Exception e)
                    {
                        dependent.setError(e);
                    }
                    updateDependentEvents(baseEvent, dependents, updateIndex + 1);
                    break;
                case BACKGROUND:
                case MAIN:

                    Runnable runnable = new Runnable()
                    {
                        public void run()
                        {
                            try
                            {
                                dependent.onPrereqsUpdated(EventManager.this);
                            }
                            catch (Exception e)
                            {
                                dependent.setError(e);
                            }
                            updateDependentEvents(baseEvent, dependents, updateIndex + 1);
                        }
                    };
                    if (mode == ThreadMode.MAIN)
                    {
                        runMainThread(runnable);
                    }
                    else
                    {
                        runBackgroundThread(runnable);
                    }
                    break;
            }
        }
    }

    protected void runBackgroundThread(Runnable runnable)
    {
        mBackgroundExecutor.submit(runnable);
    }

    // Override this to get mainthread functionality
    protected void runMainThread(Runnable runnable)
    {
        mBackgroundExecutor.submit(runnable);
    }


    protected synchronized void processAllEvents()
    {
        boolean reprocessEventsP = false;
        do
        {
            reprocessEventsP = false;
            String[] eventNames = mEventMap.keySet().toArray(new String[0]);
            for (String eventName:eventNames)
            {
                final Event event = mEventMap.get(eventName);
                if (event == null) // check for downstream changes
                {
                    reprocessEventsP = true;
                    continue;
                }
                LinkedHashSet<String> pre = event.getPrerequisites();
                if (event.getStatus() == EventStatus.OPEN || event.getStatus() == EventStatus.WORKING)
                {
                    if (prereqsMet(event))
                    {
                        ThreadMode mode = getActionThreadMode(event);
                        switch (mode)
                        {
                            case CURRENT:
                                synchronized (synchObject)
                                {
                                    event.doAction(this);
                                }

                                reprocessEventsP = true;
                                break;
                            case MAIN:
                            case BACKGROUND:
                                Runnable r = new Runnable()
                                {
                                    public void run()
                                    {
                                        synchronized (synchObject)
                                        {
                                            event.doAction(EventManager.this);
                                        }
                                        processAllEvents();
                                    }
                                };
                                if (mode == ThreadMode.MAIN)
                                {
                                    runMainThread(r);
                                }
                                else
                                {
                                    runBackgroundThread(r);
                                }
                                return; // finish later

                        }

                    }
                    else
                    {
                        for (String prereq:pre)
                        {
                            //if (!isNegatedEvent(prereq) && (!mEventMap.containsKey(prereq) || mEventMap.get(prereq).getStatus() == EventStatus.ERROR) && !mRequestMap.containsKey(prereq))
                            if (!isNegatedEvent(prereq) && !mEventMap.containsKey(prereq) && !mRequestMap.containsKey(prereq))
                            {
                                System.out.println("Adding " + prereq);
                                mRequestMap.put(prereq, System.currentTimeMillis());
                            }
                        }
                        event.setStatus(EventStatus.WORKING);
                    }
                }

            }
            processNextFromRequestMap();
        }
        while( reprocessEventsP);

    }




}
