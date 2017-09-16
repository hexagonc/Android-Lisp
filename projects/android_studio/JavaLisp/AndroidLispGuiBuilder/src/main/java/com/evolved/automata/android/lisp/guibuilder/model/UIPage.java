package com.evolved.automata.android.lisp.guibuilder.model;

import com.evolved.automata.android.lisp.guibuilder.model.ALGB;
import com.evolved.automata.android.lisp.guibuilder.model.Page;
import com.evolved.automata.lisp.NLispTools;

/**
 * Created by Evolved8 on 5/5/17.
 */

public class UIPage extends Page {

    final String INIT_EXPR_KEY;
    final String INIT_LOCAL_STORAGE_PATH_KEY;
    final String INIT_DROPBOX_PATH_KEY;
    final String INIT_CURSOR_POSITION_KEY;

    final String INIT_EXPR_KEY_PREFIX = "INIT-EXPR";
    final String INIT_LOCAL_STORAGE_PATH_KEY_PREFIX = "INIT-LOCAL-STORAGE";
    final String INIT_DROPBOX_PATH_KEY_PREFIX = "INIT-DROPBOX-PATH";
    final String INIT_CURSOR_POSITION_KEY_PREFIX = "INIT-CURSOR_POSITION_KEY";

    final String RENDER_EXPR_KEY;
    final String RENDER_LOCAL_STORAGE_PATH_KEY;
    final String RENDER_DROPBOX_PATH_KEY;
    final String RENDER_CURSOR_POSITION_KEY;

    final String RENDER_EXPR_KEY_PREFIX = "RENDER-EXPR";
    final String RENDER_LOCAL_STORAGE_PATH_KEY_PREFIX = "RENDER-LOCAL-STORAGE";
    final String RENDER_DROPBOX_PATH_KEY_PREFIX = "RENDER-DROPBOX-PATH";
    final String RENDER_CURSOR_POSITION_KEY_PREFIX = "RENDER-CURSOR_POSITION_KEY";


    final String DEBUG_EXPR_KEY;
    final String DEBUG_CURSOR_POSITION_KEY;

    final String DEBUG_EXPR_KEY_PREFIX = "DEBUG-EXPR";
    final String DEBUG_CURSOR_POSITION_KEY_PREFIX = "DEBUG-CURSOR-POSITION";
    
    public UIPage(ALGB app)
    {
        super(app);

        INIT_EXPR_KEY = mId + "-" + INIT_EXPR_KEY_PREFIX;
        INIT_LOCAL_STORAGE_PATH_KEY = mId + "-" + INIT_LOCAL_STORAGE_PATH_KEY_PREFIX;
        INIT_DROPBOX_PATH_KEY = mId + "-" + INIT_DROPBOX_PATH_KEY_PREFIX;
        INIT_CURSOR_POSITION_KEY = mId + "-" + INIT_CURSOR_POSITION_KEY_PREFIX;

        RENDER_EXPR_KEY = mId + "-" + RENDER_EXPR_KEY_PREFIX;
        RENDER_LOCAL_STORAGE_PATH_KEY = mId + "-" + RENDER_LOCAL_STORAGE_PATH_KEY_PREFIX;
        RENDER_DROPBOX_PATH_KEY = mId + "-" + RENDER_DROPBOX_PATH_KEY_PREFIX;
        RENDER_CURSOR_POSITION_KEY = mId + "-" + RENDER_CURSOR_POSITION_KEY_PREFIX;

        DEBUG_EXPR_KEY = mId + "-" + DEBUG_EXPR_KEY_PREFIX;
        DEBUG_CURSOR_POSITION_KEY = mId + "-" + DEBUG_CURSOR_POSITION_KEY_PREFIX;

        setInitCursorPosition(0);
        setRenderCursorPosition(0);
        setDebugExpr("");
        setInitExpr("");

    }

    public UIPage(ALGB app, String id)
    {
        super(app, id);

        INIT_EXPR_KEY = mId + "-" + INIT_EXPR_KEY_PREFIX;
        INIT_LOCAL_STORAGE_PATH_KEY = mId + "-" + INIT_LOCAL_STORAGE_PATH_KEY_PREFIX;
        INIT_DROPBOX_PATH_KEY = mId + "-" + INIT_DROPBOX_PATH_KEY_PREFIX;
        INIT_CURSOR_POSITION_KEY = mId + "-" + INIT_CURSOR_POSITION_KEY_PREFIX;

        RENDER_EXPR_KEY = mId + "-" + RENDER_EXPR_KEY_PREFIX;
        RENDER_LOCAL_STORAGE_PATH_KEY = mId + "-" + RENDER_LOCAL_STORAGE_PATH_KEY_PREFIX;
        RENDER_DROPBOX_PATH_KEY = mId + "-" + RENDER_DROPBOX_PATH_KEY_PREFIX;
        RENDER_CURSOR_POSITION_KEY = mId + "-" + RENDER_CURSOR_POSITION_KEY_PREFIX;

        DEBUG_EXPR_KEY = mId + "-" + DEBUG_EXPR_KEY_PREFIX;
        DEBUG_CURSOR_POSITION_KEY = mId + "-" + DEBUG_CURSOR_POSITION_KEY_PREFIX;
    }

    protected void setPageType()
    {
        mMyData.put(TYPE_KEY, NLispTools.makeValue(getPageType().toString()));
    }

    public PAGE_TYPE getPageType()
    {
        return PAGE_TYPE.UI;
    }

    public void setDebugExpr(String expr)
    {
        setStringDataValue(DEBUG_EXPR_KEY, expr);
    }

    public String getDebugExpr()
    {
        return getStringDataValue(DEBUG_EXPR_KEY);
    }

    public void setDebugCursorPosition(int pos)
    {
        setLongDataValue(DEBUG_CURSOR_POSITION_KEY, pos);
    }

    public int getDebugCursorPosition()
    {
        Long cursor = getLongDataValue(DEBUG_CURSOR_POSITION_KEY);
        return cursor.intValue();
    }
    
    

    public void setInitExpr(String expr)
    {
        setStringDataValue(INIT_EXPR_KEY, expr);
    }

    public String getInitExpr()
    {
        return getStringDataValue(INIT_EXPR_KEY);
    }

    public void setInitDropboxPath(String dropbox)
    {
        setStringDataValue(INIT_DROPBOX_PATH_KEY, dropbox);
    }

    public String getInitDropboxPath()
    {
        return getStringDataValue(INIT_DROPBOX_PATH_KEY);
    }


    public String getInitLocalStoragePath()
    {
        return getStringDataValue(INIT_LOCAL_STORAGE_PATH_KEY);
    }

    public void setInitLocalStoragePath(String path)
    {
        setStringDataValue(INIT_LOCAL_STORAGE_PATH_KEY, path);
    }


    public void setInitCursorPosition(int pos)
    {
        setLongDataValue(INIT_CURSOR_POSITION_KEY, pos);
    }

    public int getInitCursorPosition()
    {
        Long cursor = getLongDataValue(INIT_CURSOR_POSITION_KEY);
        return cursor.intValue();
    }
    
    
    
    //

    public void setRenderExpr(String expr)
    {
        setStringDataValue(RENDER_EXPR_KEY, expr);
    }

    public String getRenderExpr()
    {
        return getStringDataValue(RENDER_EXPR_KEY);
    }

    public void setRenderDropboxPath(String dropbox)
    {
        setStringDataValue(RENDER_DROPBOX_PATH_KEY, dropbox);
    }

    public String getRenderDropboxPath()
    {
        return getStringDataValue(RENDER_DROPBOX_PATH_KEY);
    }


    public String getRenderLocalStoragePath()
    {
        return getStringDataValue(RENDER_LOCAL_STORAGE_PATH_KEY);
    }

    public void setRenderLocalStoragePath(String path)
    {
        setStringDataValue(RENDER_LOCAL_STORAGE_PATH_KEY, path);
    }


    public void setRenderCursorPosition(int pos)
    {
        setLongDataValue(RENDER_CURSOR_POSITION_KEY, pos);
    }

    public int getRenderCursorPosition()
    {
        Long cursor = getLongDataValue(RENDER_CURSOR_POSITION_KEY);
        return cursor.intValue();
    }
}
