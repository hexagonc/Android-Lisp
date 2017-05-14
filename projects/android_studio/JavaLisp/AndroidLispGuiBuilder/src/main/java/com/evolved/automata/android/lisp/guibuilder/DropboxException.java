package com.evolved.automata.android.lisp.guibuilder;

/**
 * Created by Evolved8 on 5/11/17.
 */

public class DropboxException extends Exception {

    public enum EXCEPTION_TYPE {
        NOT_LOGGED_IN, NO_INTERNET
    }

    EXCEPTION_TYPE mType;

    public DropboxException(String message, EXCEPTION_TYPE t)
    {
        super(message);
        mType = t;
    }

    public EXCEPTION_TYPE getType()
    {
        return mType;
    }
}
