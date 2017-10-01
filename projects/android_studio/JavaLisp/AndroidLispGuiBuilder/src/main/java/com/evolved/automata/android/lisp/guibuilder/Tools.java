package com.evolved.automata.android.lisp.guibuilder;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.res.AssetManager;
import android.support.v7.preference.PreferenceManager;

import com.dropbox.core.v2.files.Metadata;
import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;

import org.greenrobot.eventbus.EventBus;
import org.json.JSONObject;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.UUID;


/**
 * Created by Evolved8 on 5/11/17.
 */

public class Tools {



    public static final int DEFAULT_UNDO_HISTORY_LENGTH = 20;
    static ALGBApplication mApplication = null;
    private static AssetManager mAssetManager = null;
    public static final String DEFAULT_SHARED = "-*DEFAULT*-";

    public static Environment addAndroidToolFunctions(Environment env)
    {
        env.mapFunction("notify-progress-start", notify_progress_started());
        env.mapFunction("notify-progress-completed", notify_progress_completed());
        env.mapFunction("notify-progress-failed", notify_progress_failed());
        return env;
    }

    public static AssetManager getAssets()
    {
        if (mAssetManager == null)
            mAssetManager = mApplication.getAssets();
        return mAssetManager;
    }

    public static SimpleFunctionTemplate notify_progress_started()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)notify_progress_started();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(0, true, false);
                String processId = null;
                String progressMessage = "";

                if (evaluatedArgs.length > 0)
                {
                    processId = evaluatedArgs[0].getString();
                    if (evaluatedArgs.length > 1)
                        progressMessage = evaluatedArgs[1].getString();
                }
                else
                    processId = UUID.randomUUID().toString();


                postEvent(BackgroundProcessEvent.makeProcessingStartedEvent(progressMessage, processId));

                return NLispTools.makeValue(processId);

            }
        };
    }

    public static SimpleFunctionTemplate notify_progress_completed()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)notify_progress_completed();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(0, true, false);
                String processId = null;
                String progressMessage = "";

                if (evaluatedArgs.length > 0)
                {
                    processId = evaluatedArgs[0].getString();
                    if (evaluatedArgs.length > 1)
                        progressMessage = evaluatedArgs[1].getString();
                }
                else
                    processId = UUID.randomUUID().toString();


                postEvent(BackgroundProcessEvent.makeProcessingFinishedEvent(progressMessage, processId));

                return NLispTools.makeValue(processId);

            }
        };
    }

    public static SimpleFunctionTemplate notify_progress_failed()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)notify_progress_failed();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(0, true, false);
                String processId = null;
                String progressMessage = "";

                if (evaluatedArgs.length > 0)
                {
                    processId = evaluatedArgs[0].getString();
                    if (evaluatedArgs.length > 1)
                        progressMessage = evaluatedArgs[1].getString();
                }
                else
                    processId = UUID.randomUUID().toString();


                postEvent(BackgroundProcessEvent.makeProcessingErrorEvent(progressMessage, processId));

                return NLispTools.makeValue(processId);

            }
        };
    }


    static EventBus mMain = EventBus.getDefault();



    public static void postEvent(Object o)
    {
        mMain.post(o);
    }

    public static void postStickyEvent(Object o)
    {
        mMain.postSticky(o);
    }


    public static void registerEventHandler(Object o)
    {

        mMain.register(o);
    }

    public static void unRegisterEventHandler(Object o)
    {
        mMain.unregister(o);
    }



    public static boolean isFolder(Metadata dbxMetaData)
    {
        String sForm = dbxMetaData.toString();
        try
        {
            JSONObject jobject = new JSONObject(sForm);
            String typeName = jobject.getString(".tag");
            return  "folder".equals(typeName);
        }
        catch (Exception e)
        {
            throw new RuntimeException(e.toString());
        }

    }

    public static String getParentFolder(String fullFileName)
    {
        StringBuilder parentReady = new StringBuilder("/"), segment = new StringBuilder();

        for (char c:fullFileName.toCharArray())
        {
            if (c == '/')
            {
                if (parentReady.length() > 1)
                    parentReady.append('/');
                parentReady.append(segment);
                segment = new StringBuilder();
            }
            else
                segment.append(c);
        }

        return parentReady.toString();

    }


    public static SharedPreferences getDefaultSharedPreference()
    {

        return PreferenceManager.getDefaultSharedPreferences(mApplication);
    }
    public static boolean getSampleWorkspaceEnabled()
    {
         SharedPreferences pref = getDefaultSharedPreference();
        return pref.getBoolean(mApplication.getString(R.string.pref_bool_key_include_sample_workspace), true);
    }


    public static int getEditorUndoHistoryLength(Context con)
    {
        String key = con.getString(R.string.pref_int_key_undo_history_length);
        SharedPreferences preferences = getDefaultSharedPreference();
        return preferences.getInt(key, DEFAULT_UNDO_HISTORY_LENGTH);
    }

    public static void setIntegerPreference(int prefKey, int value)
    {
        SharedPreferences preferences = getDefaultSharedPreference();
        SharedPreferences.Editor editor = preferences.edit();
        editor.putInt(mApplication.getString(prefKey), value);
        editor.commit();
    }


    static HashMap<String, HashSet<String>> mAssetsInPathMap = new HashMap<String, HashSet<String>>();



    public static HashSet<String> getAssetsInPath(String path)
    {

        if (mAssetsInPathMap.containsKey(path))
        {
            return mAssetsInPathMap.get(path);
        }
        else
        {
            HashSet<String> fileNames = null;
            try
            {
                String[] items = getAssets().list(path);
                fileNames = new HashSet<String>();
                for (String s:items)
                    fileNames.add(s);

            }
            catch (IOException ie)
            {

            }
            mAssetsInPathMap.put(path, fileNames);
            return fileNames;
        }

    }




    public static String getAssertStringData(String fullFilenamePath)
    {
        BufferedInputStream bistream = null;
        try
        {
            StringBuilder builder = new StringBuilder();
            bistream = new BufferedInputStream(getAssets().open(fullFilenamePath, AssetManager.ACCESS_STREAMING));

            int byteData;

            while ((byteData = bistream.read()) != -1)
            {
                builder.appendCodePoint(byteData);
            }
            return builder.toString();

        }
        catch (IOException ie)
        {
            EventLog.get().logcatError("<><<><><><>", ie.getMessage());
            return null;
        }
        finally
        {
            if (bistream != null)
            {
                try
                {
                    bistream.close();
                } catch (IOException e)
                {
                    e.printStackTrace();
                }
            }
        }
    }


}
