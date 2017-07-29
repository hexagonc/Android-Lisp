package com.evolved.automata.android.lisp.guibuilder;

import android.content.Context;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;

import com.evolved.automata.android.lisp.guibuilder.media.MediaEvaluator;
import com.evolved.automata.android.mindstorms.NXTBluetoothManager;
import com.evolved.automata.android.mindstorms.lisp.NXTLispFunctions;
import com.evolved.automata.android.speech.SpeechInterface;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.lisp.nn.NeuralNetLispInterface;
import com.evolved.automata.lisp.speech.SpeechLispFunctions;
import com.evolved.automata.lisp.vision.google.GoogleVisionEvaluator;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import io.reactivex.Observer;
import io.reactivex.annotations.NonNull;
import io.reactivex.disposables.Disposable;

/**
 * Created by Evolved8 on 5/3/17.
 */

public class ALGB {

    public static class DeletePageEvent extends PageStateEvent
    {
        public DeletePageEvent(String id)
        {
            super(id, PageStateEventType.DELETE);
        }
    }


    public static final String _SPEECH_LOG_LABEL = "GUI-BUILDER-SPEECH";
    HashSet<String> _expectedWords = new HashSet<String>();
    String APP_DATA_CONTEXT = "APP_DATA";
    String CURRENT_WORKSPACE_KEY = "current-workspace";

    HashMap<String, Page> mPageCache;
    HashMap<String, Workspace> mWorkspaceCache;

    Environment mTop;
    Context mContext;
    Workspace mCurrentWorkspace;

    AndroidLispDAI mData;

    LispContext mBaseLispContext;

    SpeechInterface mSpeechInterface;

    Handler mMainHandler;


    public ALGB(Context con) throws IllegalAccessException, InstantiationException
    {
        mMainHandler = new Handler(Looper.getMainLooper());
        mTop = new Environment();
        mPageCache = new HashMap<String, Page>();
        mWorkspaceCache = new HashMap<String, Workspace>();
        mContext = con;
        mData = new AndroidLispDAI(mContext);

        addStandardFunctions();
        mSpeechInterface = new SpeechInterface(mContext, _SPEECH_LOG_LABEL, null,_expectedWords );


        mBaseLispContext = new LispContext(mContext, mTop, mData);

        mBaseLispContext.setSpeechInterface(mSpeechInterface.setSpeechListener(mBaseLispContext));
        mSpeechInterface.startup();

        retrieveCurrentWorkspace();

        NXTLispFunctions.setInterpreter(mBaseLispContext.getForegroundInterpreter());

    }

    private void addStandardFunctions() throws IllegalAccessException, InstantiationException
    {
        NLispTools.addDefaultFunctionsAddMacros(mTop);
        ExtendedFunctions.addExtendedFunctions(mTop);
        NXTLispFunctions.addFunctions(mTop, NXTBluetoothManager.getInstance());

        SpeechLispFunctions.addSpeechFunctions(mTop);
        NeuralNetLispInterface.addNeuralNetFunctions(mTop);
        NAOLispEvaluator.addFunctions(mTop);
        GoogleVisionEvaluator.addVisionFunctions(mTop);
        MediaEvaluator.addVisionFunctions(mTop);
        Tools.addAndroidToolFunctions(mTop);
        mTop.mapFunction("println", getPrintln());
        mTop.mapFunction("log", getLog());

    }




    public Handler getMainhandler()
    {
        return mMainHandler;
    }


    SimpleFunctionTemplate getPrintln()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)getPrintln();
            }

            @Override
            public Value evaluate(Environment env,Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);

                StringBuilder sBuilder = new StringBuilder();
                for (int i = 0;i<evaluatedArgs.length;i++)
                {
                    sBuilder.append((evaluatedArgs[i].isString())?evaluatedArgs[i].getString():evaluatedArgs[i].toString());
                }
                String out = sBuilder.toString();

                System.out.println(out);

                return NLispTools.makeValue(out);
            }
        };
    }

    SimpleFunctionTemplate getGetDrawableResourceId()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)getGetDrawableResourceId();
            }

            @Override
            public Value evaluate(Environment env,Value[] evaluatedArgs) {
                checkActualArguments(2, true, true);

                String tag = evaluatedArgs[0].getString();
                StringBuilder message = new StringBuilder();

                Value extra = null;

                for (int i = 1; i < evaluatedArgs.length;i++)
                {
                    extra = evaluatedArgs[i];
                    if (i > 1)
                        message.append(' ');
                    if (extra.isString())
                        message.append(extra.getString());
                    else
                        message.append(extra.toString());
                }

                Log.i(tag, message.toString());
                return NLispTools.makeValue(message.toString());
            }
        };
    }


    SimpleFunctionTemplate getLog()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)getLog();
            }

            @Override
            public Value evaluate(Environment env,Value[] evaluatedArgs) {
                checkActualArguments(2, true, true);

                String tag = evaluatedArgs[0].getString();
                StringBuilder message = new StringBuilder();

                Value extra = null;

                for (int i = 1; i < evaluatedArgs.length;i++)
                {
                    extra = evaluatedArgs[i];
                    if (i > 1)
                        message.append(' ');
                    if (extra.isString())
                        message.append(extra.getString());
                    else
                        message.append(extra.toString());
                }

                Log.i(tag, message.toString());
                return NLispTools.makeValue(message.toString());
            }
        };
    }


    public boolean isSpeechSystemRunning()
    {
        return mSpeechInterface.isRunning();
    }

    public void restartSpeechSystem()
    {
        if (!isSpeechSystemRunning())
        {
            mSpeechInterface.startup();
        }
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

    public void saveAll()
    {
        mData.setData(CURRENT_WORKSPACE_KEY, APP_DATA_CONTEXT, mCurrentWorkspace.getWorkspaceId());
        for (Map.Entry<String, Workspace> entry:mWorkspaceCache.entrySet())
        {
            entry.getValue().save(true);
        }

    }

    public void deleteAllData() throws IllegalAccessException, InstantiationException
    {
        mData.deleteAllData();
        mCurrentWorkspace = createNewWorkspace();
    }

    public boolean deletePage(String pageid)
    {
        if (mPageCache.containsKey(pageid))
        {
            mPageCache.remove(pageid);
        }

        if (hasData(pageid, Page.CONTEXT_KEY))
        {
            deleteData(pageid, Page.CONTEXT_KEY);
            Tools.postEvent(new DeletePageEvent(pageid));
            return true;
        }
        else
            return false;

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


            if (hasData(workspaceId, Workspace.CONTEXT_KEY))
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

    public String getRawData(String key, String context)
    {
        return mData.getData(key, context, false);
    }

    public void setRawData(String key, String context, String content)
    {
        mData.setData(key, context, content);
    }

    public boolean hasRawData(String key, String context)
    {
        return mData.hasData(key, context);
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
