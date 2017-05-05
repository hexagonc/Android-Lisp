package com.evolved.automata.android.lisp.guibuilder;

import android.content.Context;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;

import java.util.HashMap;

/**
 * Created by Evolved8 on 5/3/17.
 */

public class ALGB {


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
