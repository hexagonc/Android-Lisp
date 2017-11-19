package com.evolved.automata.android.lisp.guibuilder.events;

/**
 * Created by Evolved8 on 11/13/17.
 */

public class ReplaceTextEvent {

    int mStartPosition;
    int mNumCharactersToReplace;
    String mReplaceText;

    private ReplaceTextEvent(int start, int deletedChars, String replaceText)
    {
        mStartPosition = start;
        mNumCharactersToReplace = deletedChars;
        mReplaceText = replaceText;
    }

    public static ReplaceTextEvent make(int start, int deletedChars, String replaceText)
    {
        return new ReplaceTextEvent(start, deletedChars, replaceText);
    }

    public String getReplacementText()
    {
        return mReplaceText;
    }

    public int getStartPosition()
    {
        return mStartPosition;
    }

    public int getNumCharactersToDelete()
    {
        return mNumCharactersToReplace;
    }
}
