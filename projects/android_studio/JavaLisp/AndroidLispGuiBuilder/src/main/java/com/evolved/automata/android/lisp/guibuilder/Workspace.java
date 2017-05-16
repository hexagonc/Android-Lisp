package com.evolved.automata.android.lisp.guibuilder;

import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.StringHashtableValue;
import com.evolved.automata.lisp.Value;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.UUID;

/**
 * Created by Evolved8 on 5/1/17.
 */

public class Workspace {
    public static final String CONTEXT_KEY = "SCRIPTING_WORKSPACEST";

    ALGB mApplication;

    LinkedList<String> mPages;

    String mId;

    final String WORKSPACE_ID_KEY;
    final String WORKSPACE_ID_KEY_PREFIX = "-WORKSPACE_ID";

    final String CHILD_PAGE_LIST_KEY;
    final String CHILD_PAGE_LIST_KEY_PREFIX = "-CHILD-PAGES";

    final String CURRENT_PAGE_INDEX_KEY;
    final String CURRENT_PAGE_INDEX_KEY_PREFIX = "-CURRENT-PAGE-INDEX";

    final String WORKSPACE_TITLE_KEY;
    final String WORKSPACE_TITLE_KEY_PREFIX = "-WORKSPACE-TITLE";

    HashMap<String, Value> mMyData;


    public Workspace(ALGB app) throws IllegalAccessException, InstantiationException
    {
        mMyData = new HashMap<String, Value>();
        mApplication = app;
        mPages = new LinkedList<String>();
        Page current = mApplication.createNewCodePage();
        mPages.add(current.getPageId());

        mId = UUID.randomUUID().toString();

        WORKSPACE_TITLE_KEY = mId + WORKSPACE_TITLE_KEY_PREFIX;
        CURRENT_PAGE_INDEX_KEY = mId + CURRENT_PAGE_INDEX_KEY_PREFIX;
        CHILD_PAGE_LIST_KEY = mId + CHILD_PAGE_LIST_KEY_PREFIX;
        WORKSPACE_ID_KEY = mId + WORKSPACE_ID_KEY_PREFIX;

        setTitle("default");
        setCurrentPageIndex(0);
        setWorkspaceId(mId);
        setPageList(mPages);
    }

    public Workspace(ALGB app, String id) throws IllegalAccessException, InstantiationException
    {
        mApplication = app;
        mId = id;

        WORKSPACE_TITLE_KEY = mId + WORKSPACE_TITLE_KEY_PREFIX;
        CURRENT_PAGE_INDEX_KEY = mId + CURRENT_PAGE_INDEX_KEY_PREFIX;
        CHILD_PAGE_LIST_KEY = mId + CHILD_PAGE_LIST_KEY_PREFIX;
        WORKSPACE_ID_KEY = mId + WORKSPACE_ID_KEY_PREFIX;

        mMyData = app.getData(mId, CONTEXT_KEY).getStringHashtable();

        mPages = getChildPageIds();

    }

    public ALGB getApplication()
    {
        return mApplication;
    }

    public void save(boolean savePages)
    {
        mApplication.saveData(getWorkspaceId(), CONTEXT_KEY, new StringHashtableValue(mMyData));
        if (savePages)
        {
            for (String key:mPages)
            {
                Page loaded = mApplication.retrievePage(key);
                if (loaded!=null)
                {
                    loaded.savePage();
                }
            }
        }

    }

    public boolean deletePage(int index)
    {
        if (mPages.size() > 1 && index < mPages.size() && index >= 0)
        {
            mPages.remove(index);
            if (index == mPages.size() - 1)
            {
                setCurrentPageIndex(index - 1);
            }

            setPageList(mPages);
            return true;
        }
        else
            return false;
    }

    public Page addPage()
    {
        Page newPage  = mApplication.createNewCodePage();
        String pageId = newPage.getPageId();
        int index = getCurrentPageIndex();
        mPages.add(index + 1, pageId);
        setCurrentPageIndex(index + 1);
        setPageList(mPages);
        return newPage;
    }

    public int getNumPages()
    {
        return mPages.size();
    }

    public Page gotoNextPage()
    {

        if (canMoveNextP())
        {
            setCurrentPageIndex(getCurrentPageIndex() + 1);
        }
        return getCurrentPage();
    }

    public Page gotoPrevPage()
    {
        if (canMovePrevP())
        {
            setCurrentPageIndex(getCurrentPageIndex() - 1);
        }
        return getCurrentPage();
    }

    public boolean canMoveNextP()
    {
        int numPages = mPages.size();
        int currentPageIndex = getCurrentPageIndex();

        return numPages > 0 && currentPageIndex < numPages - 1;
    }

    public boolean canMovePrevP()
    {
        int numPages = mPages.size();
        int currentPageIndex = getCurrentPageIndex();

        return numPages > 0 && currentPageIndex > 0;
    }


    public Page getCurrentPage()
    {
        String pageId = mPages.get(getCurrentPageIndex());

        return mApplication.retrievePage(pageId);
    }

    public String getTitle()
    {
        return mMyData.get(WORKSPACE_TITLE_KEY).getString();
    }

    public void setTitle(String title)
    {
        mMyData.put(WORKSPACE_TITLE_KEY, NLispTools.makeValue(title));
    }



    public int getCurrentPageIndex()
    {
        return (int)mMyData.get(CURRENT_PAGE_INDEX_KEY).getIntValue();
    }

    public void setCurrentPageIndex(int i)
    {
        setLongDataValue(CURRENT_PAGE_INDEX_KEY, i);
    }

    public String getWorkspaceId()
    {
        return mId;
    }

    void setWorkspaceId(String id)
    {
        setStringDataValue(WORKSPACE_ID_KEY, id);
    }

    void setPageList(LinkedList<String> list)
    {
        setStringListDataValue(CHILD_PAGE_LIST_KEY, list);
    }

    public LinkedList<String> getChildPageIds()
    {
        return getStringListData(CHILD_PAGE_LIST_KEY);
    }

    void setStringListDataValue(String key, LinkedList<String> list)
    {
        Value[] v = new Value[list.size()];
        int i = 0;
        for (String s:list)
        {
            v[i++] = NLispTools.makeValue(s);
        }
        mMyData.put(key, NLispTools.makeValue(v));
    }

    LinkedList<String> getStringListData(String key)
    {
        LinkedList<String> out = new LinkedList<String>();
        Value v = mMyData.get(key);
        if (v != null && v.isList())
        {
            Value[] list = v.getList();
            for (Value pageId:list)
            {
                out.add(pageId.getString());
            }
        }
        return out;
    }

    void setStringDataValue(String key, String value)
    {
        mMyData.put(key, NLispTools.makeValue(value));
    }

    String getStringDataValue(String key)
    {
        Value v = mMyData.get(key);
        if (v != null)
            return v.toString();
        else
            return null;
    }

    void setLongDataValue(String key, long value)
    {
        mMyData.put(key, NLispTools.makeValue(value));
    }

    Long getLongDataValue(String key)
    {
        Value v = mMyData.get(key);
        if (v != null)
            return v.getIntValue();
        else
            return null;
    }
}