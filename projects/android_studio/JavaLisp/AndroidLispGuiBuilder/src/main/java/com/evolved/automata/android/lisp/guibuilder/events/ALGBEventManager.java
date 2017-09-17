package com.evolved.automata.android.lisp.guibuilder.events;

import android.os.Handler;
import android.os.Looper;

import com.evolved.automata.android.lisp.guibuilder.Tools;
import com.evolved.automata.events.Event;
import com.evolved.automata.events.EventManager;
import com.evolved.automata.events.EventRequest;
import com.evolved.automata.events.EventRequestDispatcher;

import org.greenrobot.eventbus.Subscribe;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;

/**
 * Created by Evolved8 on 9/16/17.
 */

public class ALGBEventManager extends EventManager {

    private static ALGBEventManager mManager = null;

    Handler mMainHandler = null;
    boolean mAllowEventPostingP = false;
    private ALGBEventManager()
    {
        super(new EventRequestDispatcher() {
            @Override
            public void postRequest(EventRequest request)
            {
                Tools.postEvent(request);
            }

            @Override
            public void requestEvent(String eventName)
            {
                Tools.postEvent(EventRequest.from(eventName));
            }
        });

        mMainHandler = new Handler(Looper.getMainLooper());
        if (mAllowEventPostingP)
            Tools.registerEventHandler(this);
    }


    public ALGBEvent getEvent(ALGBEventTypes type)
    {
        return (ALGBEvent)getEvent(type.getEventName());
    }


    public void removeEvent(ALGBEventTypes type)
    {
        removeEvent(type.getEventName());
    }
    public static ALGBEventManager get()
    {
        if (mManager == null)
        {
            mManager = new ALGBEventManager();
        }
        return mManager;
    }


    @Subscribe
    public void onNextEvent(Event e)
    {
        onEvent(e);
    }

    @Override
    protected void runMainThread(Runnable runnable)
    {
        mMainHandler.post(runnable);
    }

}
