package com.evolved.automata.android.lisp.guibuilder;

import android.content.Context;
import android.content.SharedPreferences;

import com.dropbox.core.v2.files.Metadata;
import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;

import org.greenrobot.eventbus.EventBus;
import org.json.JSONObject;

import java.util.HashMap;
import java.util.UUID;


/**
 * Created by Evolved8 on 5/11/17.
 */

public class Tools {


    public static Environment addAndroidToolFunctions(Environment env)
    {
        env.mapFunction("notify-progress-start", notify_progress_started());
        env.mapFunction("notify-progress-completed", notify_progress_completed());
        env.mapFunction("notify-progress-failed", notify_progress_failed());
        return env;
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

    static final String DEFAULT_SHARED = "-*DEFAULT*-";
    public static int getEditorUndoHistoryLength(Context con)
    {
        String key = con.getString(R.string.pref_int_key_undo_history_length);
        SharedPreferences preferences = con.getSharedPreferences(DEFAULT_SHARED, Context.MODE_PRIVATE);
        return preferences.getInt(key, 20);
    }



}
