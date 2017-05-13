package com.evolved.automata.android.lisp.guibuilder;

import android.content.Context;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;

/**
 * Created by Evolved8 on 5/3/17.
 */

public class ALGB {

    String APP_DATA_CONTEXT = "APP_DATA";
    String CURRENT_WORKSPACE_KEY = "current-workspace";

    HashMap<String, Page> mPageCache;
    HashMap<String, Workspace> mWorkspaceCache;

    Environment mTop;
    Context mContext;
    Workspace mCurrentWorkspace;

    AndroidLispDAI mData;

    LispContext mBaseLispContext;

    public ALGB(Context con) throws IllegalAccessException, InstantiationException
    {
        mTop = new Environment();
        mPageCache = new HashMap<String, Page>();
        mWorkspaceCache = new HashMap<String, Workspace>();
        mContext = con;
        mData = new AndroidLispDAI(mContext);
        NLispTools.addDefaultFunctionsAddMacros(mTop);
        ExtendedFunctions.addExtendedFunctions(mTop);
        mBaseLispContext = new LispContext(mContext, mTop, mData);

        retrieveCurrentWorkspace();

    }

    public String[] getAllWorkspaceId()
    {
        HashSet<String> keys = new HashSet<String>();

        for (String w:mData.getAllKeys(Workspace.CONTEXT_KEY))
        {
            keys.add(w);
        }

        for (String key:mWorkspaceCache.keySet())
        {
            keys.add(key);
        }

        String[] ordered = keys.toArray(new String[0]);

        Arrays.sort(ordered, new Comparator<String>() {
            @Override
            public int compare(String lhs, String rhs)
            {
                return lhs.compareTo(rhs);
            }
        });
        return ordered;
    }

    public Workspace setCurrentWorkspace(String id)
    {
        Workspace w = getWorkspace(id);
        if (w == null)
            throw new IllegalArgumentException("Workspace: " + id + " does not exist");
        mCurrentWorkspace = w;
        mData.setData(CURRENT_WORKSPACE_KEY, APP_DATA_CONTEXT, id);
        return w;
    }


    private void retrieveCurrentWorkspace() throws IllegalAccessException, InstantiationException
    {
        String[] workspaces = getAllWorkspaceId();
        if (workspaces.length == 0)
        {
            mCurrentWorkspace = createNewWorkspace();
        }
        else
        {
            String previousID = mData.getData(CURRENT_WORKSPACE_KEY, APP_DATA_CONTEXT);
            mCurrentWorkspace = new Workspace(this, previousID);
        }
    }




    public void save(boolean saveWorkspace)
    {
        mData.setData(CURRENT_WORKSPACE_KEY, APP_DATA_CONTEXT, mCurrentWorkspace.getWorkspaceId());
        if (saveWorkspace)
            mCurrentWorkspace.save(true);
    }

    public void deleteAllData() throws IllegalAccessException, InstantiationException
    {
        mData.deleteAllData();
        mCurrentWorkspace = createNewWorkspace();
    }

    public Context getContext()
    {
        return mContext;
    }

    public Workspace getCurrentWorkspace()
    {
        return mCurrentWorkspace;
    }


    public LispContext getBaseContext()
    {
        return mBaseLispContext;
    }

    public CodePage createNewCodePage()
    {
        CodePage p = new CodePage(this);
        mPageCache.put(p.getPageId(), p);
        return p;
    }

    public UIPage createNewUIPage()
    {
        UIPage p = new UIPage(this);
        mPageCache.put(p.getPageId(), p);
        return p;
    }


    public Page retrievePage(String pageId)
    {
        Page p = mPageCache.get(pageId);
        if (p == null)
        {
            Value lispDat = mBaseLispContext.getEnvironment().simpleEvaluateFunction("get-data-value", pageId, Page.CONTEXT_KEY);
            HashMap<String, Value> pageData = lispDat.getStringHashtable();
            String type = pageData.get(Page.getPageTypeKey(pageId)).getString();
            switch (Page.PAGE_TYPE.valueOf(type))
            {
                case UI:
                    p = new UIPage(this, pageId);

                    break;
                case CODE:
                    p = new CodePage(this, pageId);
                    break;
            }
        }
        return p;
    }

    public CodePage retrieveCodePage(String pageId)
    {
        CodePage p = (CodePage)mPageCache.get(pageId);
        if (p == null)
        {
            p = new CodePage(this, pageId);
            mPageCache.put(pageId, p);
        }
        return p;
    }

    public UIPage retrieveUIPage(String pageId)
    {
        UIPage p = (UIPage)mPageCache.get(pageId);
        if (p == null)
        {
            p = new UIPage(this, pageId);
            mPageCache.put(pageId, p);
        }
        return p;
    }

    public Workspace getWorkspace(String workspaceId)
    {
        Workspace w = mWorkspaceCache.get(workspaceId);
        if (w == null)
        {
            Value work = getData(workspaceId, Workspace.CONTEXT_KEY);
            if (work != null)
            {
                try
                {
                    w = new Workspace(this, workspaceId);
                    mWorkspaceCache.put(workspaceId, w);
                }
                catch (Exception e)
                {
                    throw new RuntimeException(e);
                }

            }
        }
        return w;
    }

    public boolean deleteWorkspace(String id, String replacementIfCurrent)
    {
        Workspace w = getWorkspace(id);
        if (w != null)
        {

            if (getAllWorkspaceId().length == 1)
            {
                return false;
            }
            else
            {
                if (id.equals(mCurrentWorkspace.getWorkspaceId()))
                {
                    if (id.equals(replacementIfCurrent))
                    {
                        throw new IllegalArgumentException("Can't delete a workspace and replace with self");
                    }

                    Workspace replacement = getWorkspace(replacementIfCurrent);
                    if (replacement == null)
                    {
                        throw new IllegalArgumentException("Can't replace current workspace with non-existent one");
                    }

                    deleteData(id, Workspace.CONTEXT_KEY);
                    mWorkspaceCache.remove(id);

                    setCurrentWorkspace(replacementIfCurrent);
                }
                else
                {
                    deleteData(id, Workspace.CONTEXT_KEY);
                    mWorkspaceCache.remove(id);
                }
                return true;
            }
        }
        else
            throw new IllegalArgumentException("Cannot delete workspace that does not exist");
    }


    public Workspace getCachedWorkspace(String workId)
    {
        return mWorkspaceCache.get(workId);
    }

    public Workspace createNewWorkspace() throws InstantiationException, IllegalAccessException
    {
        try
        {
            Workspace workspace = new Workspace(this);
            mWorkspaceCache.put(workspace.getWorkspaceId(), workspace);
            return workspace;
        }
        catch (Exception e)
        {
            throw new RuntimeException(e);
        }

    }

    public Value getData(String key, String context)
    {

        Value result = mBaseLispContext.getEnvironment().simpleEvaluateFunction("get-data-value", key, context);

        return result;
    }

    public void saveData(String key, String context, Object data)
    {
        mBaseLispContext.getEnvironment().simpleEvaluateFunction("set-data-value", key, data, context);
    }

    public boolean hasData(String key, String context)
    {
        Value result = mBaseLispContext.getEnvironment().simpleEvaluateFunction("check-data-exists", key, context);
        return !result.isNull();
    }

    public void deleteData(String key, String context)
    {
        mBaseLispContext.getEnvironment().simpleEvaluateFunction("delete-data-value", key, context);
    }

}
