package com.evolved.automata.events;

import java.util.LinkedHashSet;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 7/2/17.
 */

public class Event {
    final String mName;

    Throwable mError;

    OnPrereqsLostHandler mPrereqsLostHandler;
    OnErrorHandler mErrorHandler = null;
    OnCompleteHandler mOnCompleteHandler = null;
    LinkedHashSet<String> mOnCompleteEventNames;
    LinkedHashSet<String> mPrerequistieEventNames;
    EventAction mAction = null;
    protected Object mData = null;
    EventStatus mStatus = EventStatus.OPEN;

    boolean mDataUpdatedP = false;

    public Event setDataUpdated()
    {
        mDataUpdatedP = true;
        return this;
    }

    public boolean isDataUpdated()
    {
        return mDataUpdatedP;
    }

    public Event clearDataUpdatedFlag()
    {
        mDataUpdatedP = false;
        return this;
    }

    /**
     * Supposed a threadmode annotation, ThreadMode(MAIN, CURRENT, BACKGROUND)
     * @param manager
     * @return
     */
    public void onPrereqsUpdated(EventManager manager)
    {

    }


    public Event(String name)
    {
        mOnCompleteEventNames = new LinkedHashSet<String>();
        mPrerequistieEventNames  = new LinkedHashSet<String>();
        mName = name;
    }

    public Event setData(Object data)
    {
        mData = data;
        return this;
    }

    public String getName()
    {
        return mName;
    }

    public Object getData()
    {
        return mData;
    }

    public Throwable getError()
    {
        return mError;
    }

    public Event setError(Throwable error)
    {
        mError = error;
        mStatus = EventStatus.ERROR;
        if (mErrorHandler != null)
            mErrorHandler.onError(error);
        return this;
    }

    public Event setComplete()
    {
        mStatus = EventStatus.COMPLETE;
        return this;
    }


    /**
     * Supposed a threadmode annotation, ThreadMode(MAIN, CURRENT, BACKGROUND)
     * @param manager
     * @return
     */
    public void doActionInternal(EventManager manager)
    {

    }

    public Event setAction(EventAction action)
    {
        mAction = action;
        return this;
    }

    public Event setStatus(EventStatus status)
    {
        mStatus  =status;
        return this;
    }

    public EventStatus getStatus()
    {
        return mStatus;
    }

    public Event setOnErrorHandler(OnErrorHandler handler)
    {
        mErrorHandler  =handler;
        return this;
    }


    public Event onRemoved(EventManager manager)
    {
        return this;
    }

    public Event setOnCompleteHandler(OnCompleteHandler handler)
    {
        mOnCompleteHandler = handler;
        return this;
    }

    public Event setPrerequesitesLostHandler(OnPrereqsLostHandler handler)
    {
        mPrereqsLostHandler = handler;
        return this;
    }

    public Event addOnCompleteEventName(String name)
    {
        mOnCompleteEventNames.add(name);
        return this;
    }

    public Event addOnCompleteEventName(String[] names)
    {
        for (String name:names)
            addOnCompleteEventName(name);
        return this;
    }


    public LinkedHashSet<String> getOnCompleteEventNames()
    {
        return mOnCompleteEventNames;
    }

    public Event addPrerequisiteEventName(String name)
    {
        mPrerequistieEventNames.add(name);
        return this;
    }

    public Event addPrerequisiteEventName(String[] names)
    {
        for (String name:names)
            addPrerequisiteEventName(name);
        return this;
    }


    public LinkedHashSet<String> getPrerequisites()
    {
        return mPrerequistieEventNames;
    }

    public void onPrerequesitesLost(LinkedList<String>  missingPrereqs)
    {
        if (mPrereqsLostHandler != null)
            mPrereqsLostHandler.onLostPrerequesites(missingPrereqs);
    }

    public Event doAction(EventManager manager)
    {
        // TODO: Consider whether error in onCompleteHandler should affect
        // Event status
        try
        {
            doActionInternal(manager);
            if (mAction != null)
                mAction.doAction(this, manager);

            if (mOnCompleteHandler != null)
                mOnCompleteHandler.onComplete(this);
            setStatus(EventStatus.COMPLETE);
        }
        catch (Throwable error)
        {
            setError(error);
        }
        if (mOnCompleteEventNames.size()>0)
        {
            for (String eventName:mOnCompleteEventNames)
            {
                manager.requestEvent(eventName);
            }

        }
        return this;
    }




}
