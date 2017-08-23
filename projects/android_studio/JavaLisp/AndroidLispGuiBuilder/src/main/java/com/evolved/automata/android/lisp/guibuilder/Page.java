package com.evolved.automata.android.lisp.guibuilder;

import android.app.Activity;

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



    public static class ProcessingStartPageEvent extends PageStateEvent
    {
        public ProcessingStartPageEvent(String id)
        {
            super(id, PageStateEventType.PROCESSING_START);
        }
    }

    public static class ProcessingStopPageEvent extends PageStateEvent
    {
        public ProcessingStopPageEvent(String id)
        {
            super(id, PageStateEventType.PROCESSING_STOP);
        }
    }





    public enum PAGE_TYPE {
        CODE, UI
    }

    public static final String DEFAULT_TITLE = "default";

    public static final String CONTEXT_KEY = "SCRIPTING_PAGE_DATA";

    public static final String SCRIPT_CONTEXT_KEY = "SCRIPTING_PAGE_SCRIPT";


    final String TITLE_KEY;
    final String ID_KEY;
    final String IS_READ_ONLY_KEY;
    final String RESULT_HISTORY_KEY;

    static final String TITLE_KEY_PREFIX = "TITLE";
    static final String ID_KEY_PREFIX = "ID";
    static final String IS_READ_ONLY_KEY_PREFIX = "-IS-READ-ONLY";
    static final String RESULT_HISTORY_KEY_PREFIX = "-RESULT-HISTORY";

    final String TYPE_KEY;
    public static final String TYPE_KEY_PREFIX = "-TYPE-KEY";



    final String mId;

    ALGB mApplication;
    Environment mMyEnvironment;

    HashMap<String, Value> mMyData;

    String mScript;
    LispContext mBasePageLispContext;

    LispContext mTempUIContext = null;

    public Page(ALGB app)
    {
        initialize(app);
        mId = UUID.randomUUID().toString();

        TYPE_KEY = getPageTypeKey(mId);
        TITLE_KEY = mId + "-" + TITLE_KEY_PREFIX;
        ID_KEY = mId + "-" + ID_KEY_PREFIX;
        IS_READ_ONLY_KEY = mId + IS_READ_ONLY_KEY_PREFIX;
        RESULT_HISTORY_KEY = mId + RESULT_HISTORY_KEY_PREFIX;
        mMyData.put(TITLE_KEY, NLispTools.makeValue(getPageType().toString()));
        setTitle(DEFAULT_TITLE);
        setStringDataValue(ID_KEY, mId);
        setPageType();
        setReadOnlyMode(false);
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
        IS_READ_ONLY_KEY = mId + IS_READ_ONLY_KEY_PREFIX;
        RESULT_HISTORY_KEY = mId + RESULT_HISTORY_KEY_PREFIX;
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


    public String getDataKey(String data)
    {
        return mId + "-" + data;
    }

    public LispContext defineUIContext(Activity context)
    {
        mTempUIContext = new LispContext(getBasePageLispContext(), context);
        mTempUIContext.setActivity(context);
        return mTempUIContext;
    }

    public void updateActivity(Activity activity)
    {
        mTempUIContext.updateActivity(activity);
    }

    public LispContext getUILispContext()
    {
        return mTempUIContext;
    }

    public ALGB getApplication()
    {
        return mApplication;
    }


    protected abstract void setPageType();

    void initialize(ALGB app)
    {
        mApplication = app;

        mMyData = new HashMap<String, Value>();

        mBasePageLispContext = new LispContext(app.getBaseContext(), app.getContext());
        mBasePageLispContext.setPage(this);
        mMyEnvironment = mBasePageLispContext.getEnvironment();
        mMyEnvironment.setVariableValues(mMyData);
        mMyEnvironment.mapValue(RenderFragment.VIEW_PROXY_VAR_NAME, Environment.getNull());

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

    public void setReadOnlyMode(boolean isReadOnly)
    {
        mMyData.put(IS_READ_ONLY_KEY, NLispTools.makeValue(isReadOnly));
    }

    public boolean isReadOnlyEnabled()
    {
        Value readOly = mMyData.get(IS_READ_ONLY_KEY);
        return readOly != null && !readOly.isNull();
    }

    public void savePage()
    {
        mMyEnvironment.simpleEvaluateFunction("set-data-value", mId, new StringHashtableValue(mMyData) , CONTEXT_KEY);
        mApplication.setRawData(getPageId(), SCRIPT_CONTEXT_KEY, mScript);
    }

    public void setResultHistory(String[] history)
    {
        Value[] v = new Value[history.length];
        for (int i = 0;i < v.length;i++)
        {
            v[i] = NLispTools.makeValue(history[i]);
        }

        mMyData.put(RESULT_HISTORY_KEY, NLispTools.makeValue(v));
    }

    public String[] getResultHistory()
    {
        Value v = mMyData.get(RESULT_HISTORY_KEY);
        if (v == null)
            return new String[0];
        Value[] vList = v.getList();
        String[] out = new String[vList.length];
        for (int i = 0;i < out.length;i++)
            out[i] = vList[i].getString();
        return out;
    }


    public boolean restorePage()
    {
        Value stored = mMyEnvironment.simpleEvaluateFunction("get-data-value", mId, CONTEXT_KEY);

        if (stored != null)
        {
            mMyData = stored.getStringHashtable();
            mMyEnvironment.setVariableValues(mMyData);
            mMyEnvironment.mapValue(RenderFragment.VIEW_PROXY_VAR_NAME, Environment.getNull());
            if (mApplication.hasData(getPageId(), SCRIPT_CONTEXT_KEY))
                mScript = mApplication.getRawData(getPageId(), SCRIPT_CONTEXT_KEY);
            else
                mScript = "";
            return true;
        }
        else
            return false;
    }


    public void setPageData(String key, Value value)
    {
        if (value == null || !value.isSerializable())
            throw new IllegalArgumentException("Can't save non-serializable data to page: " + mId);
        mMyData.put(getDataKey(key), value);
    }

    public Value getPageData(String key)
    {
        return mMyData.get(getDataKey(key));
    }

    public boolean hasPageData(String key)
    {
        return mMyData.containsKey(getDataKey(key));
    }


    public boolean removePageData(String key)
    {
        if (mMyData.containsKey(getDataKey(key)))
        {
            mMyData.remove(getDataKey(key));
            return true;
        }
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
