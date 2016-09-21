package com.evolved.automata.android.guibuilder.core.processing;

/**
 * Created by Evolved8 on 9/18/16.
 */
public interface Logger {
    public enum Level
    {
        DEBUG, INFO, WARNING, ERROR
    }

    public void log(Level level, String message, String content);

    public void logDebug(String message, String content);
    public void logInfo(String message, String content);
    public void logWarning(String message, String content);
    public void logError(String message, String content);
}
