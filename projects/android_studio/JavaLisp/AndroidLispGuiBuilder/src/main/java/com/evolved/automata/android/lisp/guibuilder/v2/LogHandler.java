package com.evolved.automata.android.lisp.guibuilder.v2;

/**
 * Created by Evolved8 on 5/13/17.
 */

public interface LogHandler {
    void logError(String tag, String value);
    void logInfo(String tag, String value);
    void logDebug(String tag, String value);
}
