package com.evolved.automata.android.lisp.guibuilder.events;

/**
 * Created by Evolved8 on 9/16/17.
 */

public enum ALGBEventTypes {
    COPY("copy"), PASTE("paste"), UNDO("undo"), REDO("redo"), ADD_WORKSPACE_PAGE("add-to-current-workspace");

    String eventName;
    ALGBEventTypes(String name)
    {
        eventName = name;
    }

    public String getEventName()
    {
        return eventName;
    }

}
