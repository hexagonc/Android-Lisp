package com.evolved.automata.android.lisp.guibuilder;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.StringHashtableValue;
import com.evolved.automata.lisp.Value;

import java.util.HashMap;
import java.util.UUID;

/**
 * Created by Evolved8 on 5/1/17.
 */

public abstract class Page {

    public enum PAGE_TYPE {
        CODE, UI
    }

    public static final String DEFAULT_TITLE = "default";

    public static final String CONTEXT_KEY = "SCRIPTING_PAGE";


    final String TITLE_KEY;
    final String ID_KEY;

    final String TITLE_KEY_PREFIX = "TITLE";
    final String ID_KEY_PREFIX = "ID";

    final String TYPE_KEY;
    public static final String TYPE_KEY_PREFIX = "-TYPE-KEY";

    final String mId;

    ALGB mApplication;
    Environment mMyEnvironment;

    HashMap<String, Value> mMyData;


    LispContext mBasePageLispContext;

    public Page(ALGB app)
    {
        initialize(app);
        mId = UUID.randomUUID().toString();

        TYPE_KEY = getPageTypeKey(mId);
        TITLE_KEY = mId + "-" + TITLE_KEY_PREFIX;
        ID_KEY = mId + "-" + ID_KEY_PREFIX;
        mMyData.put(TITLE_KEY, NLispTools.makeValue(getPageType().toString()));
        setTitle(DEFAULT_TITLE);
        setStringDataValue(ID_KEY, mId);
        setPageType();
    }

    public static String getPageTypeKey(String pageId)
    {
        return pageId + TYPE_KEY_PREFIX;
    }

    protected abstract PAGE_TYPE getPageType();

    public Page(ALGB app, String id)
    {
        initialize(app);
        mId = id;
        TITLE_KEY = mId + "-" + TITLE_KEY_PREFIX;
        ID_KEY = mId + "-" + ID_KEY_PREFIX;
        TYPE_KEY = mId + TYPE_KEY_PREFIX;
        mMyData.put(TITLE_KEY, NLispTools.makeValue(getPageType().toString()));

        if (!restorePage())
        {
            throw new IllegalArgumentException("Invalid page id: " + id + " doesn't exist");
        }
        else
        {
            setPageType();
        }
    }

    protected abstract void setPageType();

    void initialize(ALGB app)
    {
        mApplication = app;
        mMyEnvironment = new Environment(app.getBaseContext().getEnvironment());
        mMyData = new HashMap<String, Value>();

        mMyEnvironment.setVariableValues(mMyData);

        mBasePageLispContext = new LispContext(app.getBaseContext(), app.getContext());
    }

    public LispContext getBasePageLispContext()
    {
        return mBasePageLispContext;
    }

    public String getPageId()
    {
        return mId;
    }

    public void setTitle(String title)
    {
        mMyData.put(TITLE_KEY, NLispTools.makeValue(title));
    }

    public String getTitle()
    {
        return mMyData.get(TITLE_KEY).getString();
    }

    public void savePage()
    {
        mMyEnvironment.simpleEvaluateFunction("set-data-value", mId, new StringHashtableValue(mMyData) , CONTEXT_KEY);
    }

    public boolean restorePage()
    {
        Value stored = mMyEnvironment.simpleEvaluateFunction("get-data-value", mId, CONTEXT_KEY);

        if (stored != null)
        {
            mMyData = stored.getStringHashtable();
            mMyEnvironment.setVariableValues(mMyData);
            return true;
        }
        else
            return false;
    }

    public void setStringDataValue(String key, String value)
    {
        mMyData.put(key, NLispTools.makeValue(value));
    }

    public String getStringDataValue(String key)
    {
        Value v = mMyData.get(key);
        if (v != null)
        {
            if (v.isString())
                return v.getString();
            else
                return v.toString();
        }

        else
            return null;
    }

    public void setLongDataValue(String key, long value)
    {
        mMyData.put(key, NLispTools.makeValue(value));
    }

    public Long getLongDataValue(String key)
    {
        Value v = mMyData.get(key);
        if (v != null)
            return v.getIntValue();
        else
            return null;
    }



}
