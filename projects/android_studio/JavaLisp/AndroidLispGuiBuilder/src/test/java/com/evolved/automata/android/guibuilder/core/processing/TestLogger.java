package com.evolved.automata.android.guibuilder.core.processing;

/**
 * Created by Evolved8 on 9/19/16.
 */
public class TestLogger implements Logger {

    public static class LogMessage
    {

    }

    @Override
    public void log(Level level, String message, String content) {

    }

    @Override
    public void logDebug(String message, String content) {
        log(Level.DEBUG, message, content);
    }

    @Override
    public void logInfo(String message, String content) {
        log(Level.INFO, message, content);
    }

    @Override
    public void logWarning(String message, String content) {
        log(Level.WARNING, message, content);
    }

    @Override
    public void logError(String message, String content) {
        log(Level.ERROR, message, content);
    }
}
