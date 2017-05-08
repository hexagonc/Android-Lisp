package com.evolved.automata.android.lisp.guibuilder;

import com.evolved.automata.lisp.NLispTools;

/**
 * Created by Evolved8 on 5/5/17.
 */

public class CodePage extends Page {

    final String EXPR_KEY;
    final String LOCAL_STORAGE_PATH_KEY;
    final String DROPBOX_PATH_KEY;
    final String CURSOR_POSITION_KEY;

    final String EXPR_KEY_PREFIX = "EXPR";
    final String LOCAL_STORAGE_PATH_KEY_PREFIX = "LOCAL-STORAGE";
    final String DROPBOX_PATH_KEY_PREFIX = "DROPBOX-PATH";
    final String CURSOR_POSITION_KEY_PREFIX = "CURSOR_POSITION_KEY";



    public CodePage(ALGB app)
    {
        super(app);
        EXPR_KEY = mId + "-" + EXPR_KEY_PREFIX;
        LOCAL_STORAGE_PATH_KEY = mId + "-" + LOCAL_STORAGE_PATH_KEY_PREFIX;
        DROPBOX_PATH_KEY = mId + "-" + DROPBOX_PATH_KEY_PREFIX;
        CURSOR_POSITION_KEY = mId + "-" + CURSOR_POSITION_KEY_PREFIX;
        setExpr("");
        setCursorPosition(0);
    }

    public CodePage(ALGB app, String id)
    {
        super(app, id);
        EXPR_KEY = mId + "-" + EXPR_KEY_PREFIX;
        LOCAL_STORAGE_PATH_KEY = mId + "-" + LOCAL_STORAGE_PATH_KEY_PREFIX;
        DROPBOX_PATH_KEY = mId + "-" + DROPBOX_PATH_KEY_PREFIX;
        CURSOR_POSITION_KEY = mId + "-" + CURSOR_POSITION_KEY_PREFIX;
    }

    protected void setPageType()
    {
        mMyData.put(TYPE_KEY, NLispTools.makeValue(getPageType().toString()));
    }

    public PAGE_TYPE getPageType()
    {
        return PAGE_TYPE.CODE;
    }

    public void setExpr(String expr)
    {
        setStringDataValue(EXPR_KEY, expr);
    }

    public String getExpr()
    {
        return getStringDataValue(EXPR_KEY);
    }

    public void setDropboxPath(String dropbox)
    {
        setStringDataValue(DROPBOX_PATH_KEY, dropbox);
    }

    public String getDropboxPath()
    {
        return getStringDataValue(DROPBOX_PATH_KEY);
    }


    public String getLocalStoragePath()
    {
        return getStringDataValue(LOCAL_STORAGE_PATH_KEY);
    }

    public void setLocalStoragePath(String path)
    {
        setStringDataValue(LOCAL_STORAGE_PATH_KEY, path);
    }


    public void setCursorPosition(int pos)
    {
        setLongDataValue(CURSOR_POSITION_KEY, pos);
    }

    public int getCursorPosition()
    {
        Long cursor = getLongDataValue(CURSOR_POSITION_KEY);
        return cursor.intValue();
    }



}
