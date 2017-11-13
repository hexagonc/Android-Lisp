package com.evolved.automata.android.lisp.guibuilder.events;

/**
 * Created by Evolved8 on 10/20/17.
 */

public class GoToLineNumber {
    int mTargetLineNumber;
    public GoToLineNumber(int target)
    {
        mTargetLineNumber = target;
    }

    public GoToLineNumber()
    {
        mTargetLineNumber = 0;
    }

    public int getLineNumber()
    {
        return mTargetLineNumber;
    }

    public void setLineNumber(int num)
    {
        mTargetLineNumber = num;
    }
}
