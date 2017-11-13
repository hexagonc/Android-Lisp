package com.evolved.automata.android.lisp.guibuilder.events;

import android.graphics.Color;

/**
 * Created by Evolved8 on 10/20/17.
 */

public class UpdateHighLightEvent {

    public enum HIGHLIGHT_ACTION
    {
        SET, CLEAR, CLEAR_ALL
    }

    int mStartPos;
    int mStopPos;
    HIGHLIGHT_ACTION mAction;
    int mHighlightColor;

    public UpdateHighLightEvent(int start, int stop, HIGHLIGHT_ACTION action, int color)
    {
        mAction = action;
        mStartPos = start;
        mStopPos = stop;
        mHighlightColor = color;
    }

    public int getColor()
    {
        return mHighlightColor;
    }

    public int getHighlightStart()
    {
        return mStartPos;
    }

    public int getHighlightStop()
    {
        return mStopPos;
    }

    public HIGHLIGHT_ACTION getHighlightAction()
    {
        return mAction;
    }
}
