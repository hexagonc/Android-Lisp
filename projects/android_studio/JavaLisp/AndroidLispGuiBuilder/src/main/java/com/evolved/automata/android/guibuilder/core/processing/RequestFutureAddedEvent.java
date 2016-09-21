package com.evolved.automata.android.guibuilder.core.processing;

/**
 * Created by Evolved8 on 9/18/16.
 */
public class RequestFutureAddedEvent implements Event {
    String _requestId;

    RequestFutureAddedEvent(String requestId)
    {
        _requestId = requestId;

    }

    public String toString()
    {
        return "Requesting resolution of: " + _requestId;
    }

    public String getName()
    {
        return "RequestFutureAddedEvent";
    }

    public String getRequestId()
    {
        return _requestId;
    }
}
