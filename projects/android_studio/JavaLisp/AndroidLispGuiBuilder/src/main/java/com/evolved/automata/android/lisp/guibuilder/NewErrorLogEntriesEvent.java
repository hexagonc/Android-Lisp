package com.evolved.automata.android.lisp.guibuilder;

/**
 * Created by Evolved8 on 5/27/17.
 */

public class NewErrorLogEntriesEvent extends GlobalStatusAlertEvent {
    public NewErrorLogEntriesEvent()
    {
        super(StatusAlertType.NEW_ERROR_ENTRIES);
    }
}
