package com.evolved.automata.android.lisp.guibuilder;

import android.app.Activity;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.database.Cursor;
import android.media.Ringtone;
import android.media.RingtoneManager;
import android.net.Uri;
import android.os.Handler;
import android.os.Looper;
import android.support.v4.app.NotificationCompat;
import android.support.v4.app.TaskStackBuilder;
import android.util.Log;

import com.evolved.automata.android.lisp.views.ViewProxy;
import com.evolved.automata.android.speech.SpeechInterface.SpeechListener;
import com.evolved.automata.android.lisp.AndroidLispInterpreter;
import com.evolved.automata.android.lisp.views.ViewEvaluator;
import com.evolved.automata.android.speech.SpeechInterface;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.StringHashtableValue;
import com.evolved.automata.lisp.Value;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import java.io.BufferedReader;
import java.io.StringReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;

import java.util.Set;
import java.util.UUID;
import java.util.WeakHashMap;
import java.util.concurrent.ConcurrentHashMap;

import io.reactivex.ObservableEmitter;
import io.reactivex.ObservableOnSubscribe;
import io.reactivex.Observer;
import io.reactivex.Observable;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.annotations.NonNull;
import io.reactivex.disposables.Disposable;
import io.reactivex.schedulers.Schedulers;

/**
 * Created by Evolved8 on 5/4/17.
 */

public class LispContext implements SpeechListener{

    static final int DEFAULT_NOTIFICATION_ID = 1;
    static final String _DEFAULT_CONTEXT = "DEFAULT";
    Context mContext;

    LispDataAccessInterface mDAI;
    Environment mBaseEnvironment;
    Environment mEnv;

    LispContext mParent=null;

    Activity mActivity;

    AndroidLispInterpreter mAndroidInterpreter;

    SpeechInterface.SpeechControlInterface mSpeechInterface;

    Page mPage = null;

    Observer<Value> mDefaultBackgroundResultHandler;

    public void setDefaultResultHandler(Observer<Value> observer)
    {
        mDefaultBackgroundResultHandler = observer;
    }

    Object mSynch = new Object();

    boolean mTtsAvailableP = false;
    boolean mAsrAvailableP = false;



    public static class Request
    {
        Observer<Value> rListener;
        Value result;
        String expr;
        Value precompiledExpr;
        Environment evaluationEnv;
        boolean deletedP = false;
    }

    LispSpeechInterface.ListeningStateListener mListeningStateHandler;
    LispSpeechInterface.SpeechStateListener mSpeechStateHandler;

    FunctionTemplate _asrListenerLambda = null;
    FunctionTemplate _ttsListenerLambda = null;


    ConcurrentHashMap<String, Request> mRequestMap = new ConcurrentHashMap<String, Request>();

    WeakHashMap<Request, String> mServiceRequestMap = new WeakHashMap<Request, String>();

    static Handler  _mainHandler = new Handler(Looper.getMainLooper());

    public static String _SPEECH_AVAILABLE_VAR_NAME = "SPEECH_AVAILABLE_P";
    public static String _ASR_AVAILABLE_VAR_NAME = "ASR_AVAILABLE_P";
    public static String _TTS_STATUS_VAR_NAME = "TTS_STATUS";
    public static String _ASR_STATUS_VAR_NAME = "ASR_STATUS";

    public static final String _TTS_STATUS_STARTED_STATUS_VALUE = "TTS_STARTED";
    public static final String _TTS_STATUS_COMPLETE_VALUE = "TTS_COMPLETE";
    public static final String _TTS_STATUS_INTERRUPTED_VALUE = "TTS_INTERRUPTED";

    public static final String _ASR_STATUS_STARTED = "ASR_STARTED";
    public static final String _ASR_STATUS_RECOGNITION_ERROR = "RECOGNITION_ERROR";
    public static final String _ASR_STATUS_RECOGNITION_COMPLETE = "RECOGNITION_COMPLETE";
    public static final String _LAST_ASR_VARIABLE_NAME = "LAST-RECOGNITION";


    boolean mRootContextP = false;

    RingtoneManager mRingtoneManager;

    public LispContext(Context con, Environment base, LispDataAccessInterface dai)
    {
        mContext = con;
        mBaseEnvironment  = base;
        mDAI = dai;
        mAndroidInterpreter = new AndroidLispInterpreter(mContext, mEnv, new AndroidLispInterpreter.ResponseListener()
        {

            @Override
            public void onError(Exception e)
            {
                EventLog.EventSource source = new EventLog.LispPageSource(EventLog.LispSourceType.FOREGROUND, mPage);
                EventLog.get().logEvent(source, EventLog.EntryType.ERROR, "Internal error", e.toString());
            }

            @Override
            public void onResult(Value v)
            {

            }
        });

        mRingtoneManager = new RingtoneManager(con);
        mRootContextP = true;
        resetEnvironment();

    }



    protected LispContext(LispContext parent, Context con)
    {
        mContext = con;
        mParent = parent;
        mBaseEnvironment = mParent.getEnvironment();
        mEnv = new Environment(mBaseEnvironment);
    }

    RingtoneManager getRingtoneManager()
    {
        if (mRingtoneManager == null && mParent != null)
            return mParent.getRingtoneManager();
        return mRingtoneManager; // this shouldn't happen
    }

    private Ringtone getRingtoneByIndex(int index)
    {
        int count = 0;
        RingtoneManager rmanager = getRingtoneManager();

        Cursor ringtoneCursor = rmanager.getCursor();
        Ringtone ringtone = null;
        if (ringtoneCursor.moveToFirst())
        {
            if (count == index)
            {
                ringtone = rmanager.getRingtone(count);
                return ringtone;
            }
            count++;
            while (ringtoneCursor.moveToNext())
            {
                if (count == index)
                {
                    ringtone = rmanager.getRingtone(count);
                    return ringtone;
                }
                count++;
            }
        }
        return ringtone;
    }

    public LinkedList<Pair<String, String>> getSupportedRingtones()
    {
        RingtoneManager rmanager = getRingtoneManager();
        LinkedList<Pair<String, String>> ringtones = new LinkedList<Pair<String, String>>();

        Cursor ringtoneCursor = rmanager.getCursor();
        if (ringtoneCursor.moveToFirst())
        {
            String uriName =ringtoneCursor.getString(RingtoneManager.URI_COLUMN_INDEX);
            String title = ringtoneCursor.getString(RingtoneManager.TITLE_COLUMN_INDEX);

            ringtones.add(Pair.of(uriName, title));
            while (ringtoneCursor.moveToNext())
            {
                uriName = ringtoneCursor.getString(RingtoneManager.URI_COLUMN_INDEX);
                title = ringtoneCursor.getString(RingtoneManager.TITLE_COLUMN_INDEX);
                ringtones.add(Pair.of(uriName, title));
            }
        }
        return ringtones;
    }

    public Ringtone getRingtone(String uri)
    {
        RingtoneManager rmanager = getRingtoneManager();
        return rmanager.getRingtone(mContext, Uri.parse(uri));
    }



    public void playRingtone(Ringtone ringtone, int numRepetitions, int repeatDelayMilli, int maxDurationMilli)
    {
        RingtoneManager ringtoneManager= getRingtoneManager();
        ringtoneManager.stopPreviousRingtone();
        try
        {
            long stopTime = System.currentTimeMillis() + maxDurationMilli;
            for (int i = 0; i< numRepetitions;i++)
            {
                ringtone.play();
                while (ringtone.isPlaying())
                {
                    if (stopTime < System.currentTimeMillis())
                    {
                        ringtone.stop();
                        return;
                    }
                    Thread.sleep(100);

                }
                Thread.sleep(repeatDelayMilli);
            }
            ringtone.stop();
        }
        catch (InterruptedException e)
        {

        }
    }

    public HashSet<String> getActiveProcesses()
    {
        HashSet<String> active = new HashSet<String>();
        synchronized (mSynch)
        {
            for (String name:mRequestMap.keySet())
            {
                active.add(name);
            }
            return active;
        }
    }

    public void setPage(Page page)
    {
        mPage = page;
        resetEnvironment();
    }

    public void setDataAccessInterface(LispDataAccessInterface dai)
    {
        mDAI = dai;
        resetEnvironment();
    }

    public AndroidLispInterpreter getForegroundInterpreter()
    {
        if (mRootContextP || mParent == null)
            return mAndroidInterpreter;
        else
            return getRootContext().getForegroundInterpreter();
    }

    public Page getCurrentPage()
    {
        if (mPage == null && mParent != null)
        {
            return mParent.getCurrentPage();
        }
        return mPage;
    }


    public void setActivity(Activity activity)
    {
        mActivity = activity;
        resetEnvironment();
    }

    public void updateActivity(Activity activity)
    {
        mActivity = activity;
        setActivityFunctions();
    }


    public void breakEvaluation()
    {
        synchronized (mSynch)
        {
            mRequestMap.clear();
        }

    }

    public void breakEvaluation(String id)
    {
        synchronized (mSynch)
        {
            mRequestMap.remove(id);

            for (Request r:mServiceRequestMap.keySet())
            {
                if (mServiceRequestMap.get(r).equals(id))
                {
                    r.deletedP = true;
                }
            }

        }

    }

    public Value evaluateSynchronous(String expression)
    {
        try
        {
            return mEnv.evaluate(stripComments(expression), false);
        } catch (InstantiationException e)
        {
            throw new RuntimeException(e);
        } catch (IllegalAccessException e)
        {
            throw new RuntimeException(e);
        }
    }



    private void addPageFunctions()
    {
        mEnv.mapFunction("get-page-data", getPageData());
        mEnv.mapFunction("set-page-data", setPageData());
        mEnv.mapFunction("has-page-data", hasPageData());
        mEnv.mapFunction("delete-page-data", deletePageData());
        mEnv.mapFunction("get-page-active-processes", getActivePageProcesses());
        mEnv.mapFunction("set-top-view", setTopView());

        mEnv.mapFunction("log-error", logError());
        mEnv.mapFunction("log-info", logInfo());
        mEnv.mapFunction("log-result", logResult());
    }


    SimpleFunctionTemplate logError()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)logError();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);
                String summary = evaluatedArgs[0].getString();
                String detail = summary;
                if (evaluatedArgs.length > 1)
                {
                    detail = evaluatedArgs[1].getString();
                }

                boolean isForegroundP = Looper.getMainLooper() == Looper.myLooper();
                EventLog.LispSourceType ltype;

                if (isForegroundP)
                    ltype = EventLog.LispSourceType.FOREGROUND;
                else
                    ltype = EventLog.LispSourceType.BACKGROUND;

                EventLog.EventSource source = new EventLog.LispPageSource(ltype, mPage);
                EventLog.get().logEvent(source, EventLog.EntryType.ERROR, summary, detail);

                return evaluatedArgs[0];

            }
        };
    }

    SimpleFunctionTemplate logInfo()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)logInfo();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);
                String summary = evaluatedArgs[0].getString();
                String detail = summary;
                if (evaluatedArgs.length > 1)
                {
                    detail = evaluatedArgs[1].getString();
                }

                boolean isForegroundP = Looper.getMainLooper() == Looper.myLooper();
                EventLog.LispSourceType ltype;

                if (isForegroundP)
                    ltype = EventLog.LispSourceType.FOREGROUND;
                else
                    ltype = EventLog.LispSourceType.BACKGROUND;

                EventLog.EventSource source = new EventLog.LispPageSource(ltype, mPage);
                EventLog.get().logEvent(source, EventLog.EntryType.INFO, summary, detail);

                return evaluatedArgs[0];

            }
        };
    }


    SimpleFunctionTemplate logResult()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)logResult();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);
                String summary = evaluatedArgs[0].toString();
                String detail = summary;
                if (evaluatedArgs.length > 1)
                {
                    detail = evaluatedArgs[1].getString();
                }

                boolean isForegroundP = Looper.getMainLooper() == Looper.myLooper();
                EventLog.LispSourceType ltype;

                if (isForegroundP)
                    ltype = EventLog.LispSourceType.FOREGROUND;
                else
                    ltype = EventLog.LispSourceType.BACKGROUND;

                EventLog.EventSource source = new EventLog.LispPageSource(ltype, mPage);
                EventLog.get().logEvent(source, EventLog.EntryType.RESULT, summary, detail);

                return evaluatedArgs[0];

            }
        };
    }

    SimpleFunctionTemplate logGlobalError()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)logGlobalError();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);
                String summary = evaluatedArgs[0].getString();
                String detail = summary;
                if (evaluatedArgs.length > 1)
                {
                    detail = evaluatedArgs[1].getString();
                }

                EventLog.EventSource source = new EventLog.LispGlobalSource();
                EventLog.get().logEvent(source, EventLog.EntryType.ERROR, summary, detail);

                return evaluatedArgs[0];

            }
        };
    }

    SimpleFunctionTemplate logGlobalInfo()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)logGlobalInfo();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);
                String summary = evaluatedArgs[0].getString();
                String detail = summary;
                if (evaluatedArgs.length > 1)
                {
                    detail = evaluatedArgs[1].getString();
                }


                EventLog.EventSource source = new EventLog.LispGlobalSource();
                EventLog.get().logEvent(source, EventLog.EntryType.INFO, summary, detail);

                return evaluatedArgs[0];

            }
        };
    }


    SimpleFunctionTemplate logGlobalResult()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)logGlobalResult();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);
                String summary = evaluatedArgs[0].toString();
                String detail = summary;
                if (evaluatedArgs.length > 1)
                {
                    detail = evaluatedArgs[1].getString();
                }

                EventLog.EventSource source = new EventLog.LispGlobalSource();
                EventLog.get().logEvent(source, EventLog.EntryType.RESULT, summary, detail);

                return evaluatedArgs[0];

            }
        };
    }

    SimpleFunctionTemplate getActivePageProcesses()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)getActivePageProcesses();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {

                String pageKey = mPage.getPageId();
                if (evaluatedArgs.length>0)
                {
                    pageKey = evaluatedArgs[0].getString();
                }

                ALGB app = mPage.getApplication();
                Page actualPage = app.retrievePage(pageKey);

                LispContext con;
                if (actualPage.getUILispContext() != null)
                {
                    con = actualPage.getUILispContext();
                }
                else
                    con = actualPage.getBasePageLispContext();

                HashSet<String> processes = con.getActiveProcesses();
                while (con.mParent != null)
                {
                    con = con.mParent;
                    processes.addAll(con.getActiveProcesses());
                }

                HashMap<String, Value> processMap = new HashMap<String, Value>();
                for (String n:processes)
                {
                    processMap.put(n, NLispTools.makeValue(1));
                }

                return new StringHashtableValue(processMap);


            }
        };
    }

    SimpleFunctionTemplate setTopView()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)setTopView();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, false, false);

                Value viewproxy = evaluatedArgs[0];
                if (!viewproxy.isUserObject() ||
                        (!(viewproxy.getObjectValue() instanceof ViewProxy)))
                    throw new IllegalArgumentException("set-top-view requires view proxy argument");

                mEnv.mapValue(RenderFragment.VIEW_PROXY_VAR_NAME, viewproxy);
                return viewproxy;


            }
        };
    }


    SimpleFunctionTemplate getPageData()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)getPageData();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, false);

                String name = evaluatedArgs[0].getString();
                String pageKey = mPage.getPageId();
                if (evaluatedArgs.length>1)
                {
                    pageKey = evaluatedArgs[1].getString();
                }

                ALGB app = mPage.getApplication();
                Page actualPage = app.retrievePage(pageKey);
                return actualPage.getPageData(name);


            }
        };
    }


    SimpleFunctionTemplate setPageData()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)setPageData();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(2, true, false);

                String name = evaluatedArgs[0].getString();
                Value data = evaluatedArgs[1];
                String pageKey = mPage.getPageId();
                if (evaluatedArgs.length>2)
                {
                    pageKey = evaluatedArgs[2].getString();
                }

                ALGB app = mPage.getApplication();
                Page actualPage = app.retrievePage(pageKey);
                actualPage.setPageData(name, data);
                return data;

            }
        };
    }


    SimpleFunctionTemplate hasPageData()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)hasPageData();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, false);

                String name = evaluatedArgs[0].getString();
                String pageKey = mPage.getPageId();
                if (evaluatedArgs.length>1)
                {
                    pageKey = evaluatedArgs[1].getString();
                }

                ALGB app = mPage.getApplication();
                Page actualPage = app.retrievePage(pageKey);
                Value v = actualPage.getPageData(name);
                if (v == null)
                    return Environment.getNull();
                else
                    return v;

            }
        };
    }

    SimpleFunctionTemplate deletePageData()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)deletePageData();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, false);

                String name = evaluatedArgs[0].getString();
                String pageKey = mPage.getPageId();
                if (evaluatedArgs.length>1)
                {
                    pageKey = evaluatedArgs[1].getString();
                }

                ALGB app = mPage.getApplication();
                Page actualPage = app.retrievePage(pageKey);
                return NLispTools.makeValue(actualPage.removePageData(pageKey));

            }
        };
    }

    SimpleFunctionTemplate isBackgroundServiceProcessRunningP()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)isBackgroundServiceProcessRunningP();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                String tagName = evaluatedArgs[0].getString();


                for (Request r:mServiceRequestMap.keySet())
                {
                    if (mServiceRequestMap.get(r).equals(tagName))
                    {
                        return NLispTools.makeValue(true);
                    }
                }

                return NLispTools.makeValue(false);
            }
        };
    }


    SimpleFunctionTemplate getBreak()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)getBreak();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                if (evaluatedArgs.length > 0)
                {
                    String tag = evaluatedArgs[0].getString();
                    breakEvaluation(tag);
                }
                else
                    breakEvaluation();

                return Environment.getNull();


            }
        };
    }


    public void evaluateMainThreadExpression(final Environment env, final Value preparsed, final Observer<Value> xlistener)
    {
        AndroidLispInterpreter.ResponseListener listener = new AndroidLispInterpreter.ResponseListener() {
            @Override
            public void onError(Exception e)
            {
                if (xlistener != null)
                    xlistener.onError(e);
                else
                    Log.e("<><<>", "Error in foreground process: " + e.toString());
            }

            @Override
            public void onResult(Value v)
            {
                if (xlistener != null)
                {
                    xlistener.onNext(v);
                    xlistener.onComplete();
                }
                else
                {
                    Log.i("<><<>", "Finished foreground process result:" + v.toString());
                }
            }
        };

        AndroidLispInterpreter interpreter = new AndroidLispInterpreter(mContext, env, listener);
        interpreter.evaluatePreParsedValue(env, preparsed, true);

    }

    public void evaluateBackgroundServiceExpression(final Environment env, String tagName, final Value preparsed, final Observer<Value> xlistener, boolean startService)
    {
        Request request = new Request();
        request.rListener = xlistener;
        request.evaluationEnv = env;
        request.precompiledExpr = preparsed;

        BackgroundLispService.addRequest(request);
        mServiceRequestMap.put(request, tagName);

        if (startService)
        {
            Intent intent = new Intent(mContext, BackgroundLispService.class);
            mContext.startService(intent);
        }

    }

    public void evaluateMainThreadExpression(final String expression, final Observer<Value> listener)
    {

        Observable<Value> out = Observable.create(new ObservableOnSubscribe<Value>() {
            @Override
            public void subscribe(final @NonNull ObservableEmitter<Value> subscriber) throws Exception
            {
                AndroidLispInterpreter.ResponseListener listener = new AndroidLispInterpreter.ResponseListener() {
                    @Override
                    public void onError(Exception e)
                    {
                        subscriber.onError(e);
                    }

                    @Override
                    public void onResult(Value v)
                    {
                        subscriber.onNext(v);
                        subscriber.onComplete();
                    }
                };
                mAndroidInterpreter.setResponseListener(listener);
                mAndroidInterpreter.evaluateExpression(expression, true);
            }
        }).subscribeOn(AndroidSchedulers.mainThread()).observeOn(AndroidSchedulers.mainThread());

        out.subscribe(listener);


    }

    public void evaluateExpression( String expression,  Observer<Value> resultListener)
    {
        evaluateExpression(expression, UUID.randomUUID().toString(), resultListener);
    }

    public void evaluateExpression(Value preCompiledValue, final String requestId, final Observer<Value> resultListener)
    {

        final Request r = new Request();
        r.expr = null;
        r.precompiledExpr = preCompiledValue;
        r.result = null;
        r.rListener = resultListener;

        Observable<Value> out = Observable.create(new ObservableOnSubscribe<Value>() {
            @Override
            public void subscribe(@NonNull ObservableEmitter<Value> subscriber) throws Exception
            {
                synchronized (mSynch)
                {
                    if (mRequestMap.size() == 0)
                    {
                        mRequestMap.put(requestId, r);
                        startWorkerThread();
                    }
                    else
                        mRequestMap.put(requestId, r);
                }

            }
        }).observeOn(AndroidSchedulers.mainThread());
        out.subscribe();
    }


    public void evaluateExpression(Value preCompiledValue, final String requestId, final Observer<Value> resultListener, Environment runtimeEnvironment)
    {

        final Request r = new Request();
        r.expr = null;
        r.precompiledExpr = preCompiledValue;
        r.result = null;
        r.rListener = resultListener;
        r.evaluationEnv = runtimeEnvironment;

        Observable<Value> out = Observable.create(new ObservableOnSubscribe<Value>() {
            @Override
            public void subscribe(@NonNull ObservableEmitter<Value> subscriber) throws Exception
            {
                synchronized (mSynch)
                {
                    if (mRequestMap.size() == 0)
                    {
                        mRequestMap.put(requestId, r);
                        startWorkerThread();
                    }
                    else
                        mRequestMap.put(requestId, r);
                }

            }
        }).observeOn(AndroidSchedulers.mainThread());
        out.subscribe();
    }


    public void evaluateExpression(Value preCompiledValue,  Observer<Value> resultListener)
    {
        evaluateExpression(preCompiledValue, UUID.randomUUID().toString(), resultListener);
    }


    private String stripComments(String expr)
    {
        BufferedReader breader = new BufferedReader(new StringReader(expr ));
        StringBuilder builder = new StringBuilder();
        String line = null;
        try
        {
            while ((line = breader.readLine())!=null)
            {
                if (!line.trim().startsWith(";"))
                    builder.append(line).append(' ');
            }
        }
        catch (Exception e)
        {
            throw new RuntimeException(e.toString());
        }
        return builder.toString();
    }

    public void evaluateExpression(final String expression, final String requestId, final Observer<Value> resultListener)
    {

        final Request r = new Request();
        r.expr = stripComments(expression);
        r.result = null;
        r.rListener = resultListener;

        Observable<Value> out = Observable.create(new ObservableOnSubscribe<Value>() {
            @Override
            public void subscribe(@NonNull ObservableEmitter<Value> subscriber) throws Exception
            {
                synchronized (mSynch)
                {
                    if (mRequestMap.size() == 0)
                    {
                        mRequestMap.put(requestId, r);
                        startWorkerThread();
                    }
                    else
                        mRequestMap.put(requestId, r);
                }

            }
        }).observeOn(AndroidSchedulers.mainThread());
        out.subscribe(resultListener);
    }

    public boolean isBusy()
    {
        return mRequestMap.size() > 0;
    }

    private void startWorkerThread()
    {
        Thread d = new Thread()
        {
            public void run()
            {
                LinkedList<String> toBeDeleted = new LinkedList<String>();
                Set<String> keys;
                boolean cont;
                synchronized (mSynch)
                {
                    cont = mRequestMap.size() > 0;
                }

                while (cont)
                {

                    synchronized (mSynch)
                    {
                        keys = mRequestMap.keySet();
                    }
                    for (String key: keys)
                    {
                        Request request = mRequestMap.get(key);
                        if (request == null)
                            continue;
                        try
                        {
                            Value response = null;
                            if (request.precompiledExpr != null)
                            {
                                if (request.evaluationEnv != null)
                                    response = request.evaluationEnv.evaluate(request.precompiledExpr);
                                else
                                    response = mEnv.evaluate(request.precompiledExpr);
                            }
                            else if (request.expr != null)
                            {
                                response  = mEnv.evaluate(request.expr, false);

                            }
                            else if (request.result != null)
                            {

                                response = request.result.getContinuingFunction().evaluate(mEnv, true);
                            }
                            else
                            {
                                if (request.rListener != null)
                                    request.rListener.onError(new IllegalStateException("Invalid data response"));
                                toBeDeleted.add(key);
                                continue;
                            }

                            if (response.isContinuation())
                            {
                                request.result = response;
                                request.expr = null;
                                request.precompiledExpr = null;
                            }
                            else
                            {
                                // Quick hack.  Should use RxJava to do this better
                                postResponse(response, request);

                                toBeDeleted.add(key);
                            }
                        }
                        catch (Exception e)
                        {
                            postResponse(e, request);

                            toBeDeleted.add(key);
                        }

                    }

                    synchronized (mSynch)
                    {
                        for (String key: toBeDeleted)
                        {
                            mRequestMap.remove(key);
                        }
                        toBeDeleted.clear();
                        cont = mRequestMap.size() > 0;
                    }

                }


            }
        };
        d.setDaemon(true);
        d.start();
    }

    private void postResponse(final Value result, final Request request)
    {
        if (request.rListener != null)
        {
            Runnable r = new Runnable()
            {
                public void run()
                {

                    request.rListener.onNext(result);
                    request.rListener.onComplete();
                }
            };
            _mainHandler.post(r);
        }


    }

    private void postResponse(final Exception e, final Request request)
    {
        if (request.rListener != null)
        {
            Runnable r = new Runnable()
            {
                public void run()
                {
                    request.rListener.onError(e);
                }
            };
            _mainHandler.post(r);
        }

    }

    private void addDataFunctions()
    {
        mEnv.mapFunction("set-data-value", setObjectDataValue());
        mEnv.mapFunction("get-data-value", getObjectDataValue());
        mEnv.mapFunction("delete-all-data", deleteAllData());
        mEnv.mapFunction("delete-data-value", deleteDataValue());
        mEnv.mapFunction("check-data-exists", testDataExists());
        mEnv.mapFunction("delete-old-data", deleteOldData());
        mEnv.mapFunction("select-old-data-names", selectOldData());
        mEnv.mapFunction("get-all-names", selectAllDataKeys());
    }

    public void resetEnvironment()
    {

        mEnv = new Environment(mBaseEnvironment);

        if (mRootContextP)
        {
            addGlobalOnlyFunctions();
        }
        if (mDAI != null)
        {
            addDataFunctions();
        }

        if (mActivity != null)
        {
            setActivityFunctions();
        }

        if (mPage != null)
            addPageFunctions();
        addProcessingFunctions();
    }

    private void setActivityFunctions()
    {
        ViewEvaluator.bindFunctions(mEnv, mActivity, getForegroundInterpreter());
        DropboxManager.get().addDropboxLispFunctions(mEnv, mActivity, this);
    }




    private void addGlobalOnlyFunctions()
    {
        mEnv.mapFunction("log-error", logGlobalError());
        mEnv.mapFunction("log-info", logGlobalInfo());
        mEnv.mapFunction("log-result", logGlobalResult());
    }

    private FunctionTemplate evaluateGlobal()
    {
        return new FunctionTemplate() {


            @Override
            public Object clone()
            {
                return evaluateGlobal();
            }


            @Override
            public Value evaluate(final Environment env, boolean resume)
                    throws InstantiationException, IllegalAccessException
            {

                Environment runEnvironment = getRootEnvironment();
                if (!resume)
                {
                    resetFunctionTemplate();
                }

                Value result = Environment.getNull();
                for (; _instructionPointer < _actualParameters.length; _instructionPointer++)
                {
                    if (resume && _lastFunctionReturn.getContinuingFunction() != null)
                        result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(runEnvironment, resume);
                    else
                        result = _lastFunctionReturn = runEnvironment.evaluate(_actualParameters[_instructionPointer], false);

                    if (result.isContinuation())
                        return continuationReturn(result);
                    if (result.isBreak() || result.isReturn() || result.isSignal() || result.isSignalOut())
                        return resetReturn(result);
                }

                return resetReturn(result);

            }
        };
    }

    public void addProcessingFunctions()
    {
        mEnv.mapFunction("on-main-thread-p", onMainThread());
        mEnv.mapFunction("evaluate-background", evaluate_background());
        mEnv.mapFunction("evaluate-foreground", evaluate_foreground());
        mEnv.mapFunction("evaluate-background-service", evaluate_background_service());
        mEnv.mapFunction("stop-background-service", stop_background_service());
        mEnv.mapFunction("break-process", getBreak());
        mEnv.mapFunction("is-background-service-process-running-p", isBackgroundServiceProcessRunningP());
        mEnv.mapFunction("global", evaluateGlobal());
        mEnv.mapFunction("get-ringtones", get_ringtones());

        mEnv.mapFunction("repeat-ringtone-by-index", repeat_ringtone_by_index());
        mEnv.mapFunction("is-playing-ringtone", playing_ringtone());
        mEnv.mapFunction("get-ringtone-name-by-index", get_ringtone_by_index() );

        mEnv.mapFunction("show-notificaton", show_notification() );
        mEnv.mapFunction("load-page-by-title", load_page_by_title());
    }

    public Environment getEnvironment()
    {
        return mEnv;
    }

    public Environment getRootEnvironment()
    {
        if (mRootContextP)
            return getEnvironment();
        else
            return getRootContext().getRootEnvironment();
    }

    public LispContext getRootContext()
    {
        if (mRootContextP)
            return this;
        else if (mParent != null)
        {
            return mParent.getRootContext();
        }
        else
            return this;
    }

    SimpleFunctionTemplate onMainThread()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)onMainThread();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                return NLispTools.makeValue(Looper.getMainLooper() == Looper.myLooper());
            }
        };
    }


    SimpleFunctionTemplate testDataExists()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)testDataExists();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);

                String name = evaluatedArgs[0].getString();
                String context = _DEFAULT_CONTEXT;
                if (evaluatedArgs.length>1)
                    context = evaluatedArgs[1].getString();

                try
                {
                    return NLispTools.makeValue(mDAI.hasData(name, context));
                }
                catch (Exception e)
                {
                    throw new RuntimeException(e);
                }
            }
        };
    }


    SimpleFunctionTemplate getObjectDataValue()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)getObjectDataValue();
            }

            @Override
            public Value evaluate(Environment env,Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);

                String name = evaluatedArgs[0].getString();
                String context = _DEFAULT_CONTEXT;
                if (evaluatedArgs.length>1)
                    context = evaluatedArgs[1].getString();
                boolean updateLastAccess =false;
                updateLastAccess =  evaluatedArgs.length > 2 && !evaluatedArgs[2].isNull();
                try
                {

                    String serializedData = mDAI.getData(name, context, updateLastAccess);
                    if (serializedData == null)
                        return Environment.getNull();
                    serializedData = StringUtils.replace(serializedData, "\\\\\"\\\"", "\\\"");
                    LinkedList<Value> out = env.parse(serializedData, true);
                    if (out.size() == 0)
                        return Environment.getNull();
                    return env.evaluate(out.getFirst());
                }
                catch (Exception e)
                {
                    throw new RuntimeException(e);
                }
            }
        };
    }

    SimpleFunctionTemplate deleteDataValue()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)deleteDataValue();
            }

            @Override
            public Value evaluate(Environment env,Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);

                String name = evaluatedArgs[0].getString();
                String context = _DEFAULT_CONTEXT;
                if (evaluatedArgs.length>1)
                    context = evaluatedArgs[1].getString();

                try
                {
                    return NLispTools.makeValue(mDAI.deleteData(name, context));
                }
                catch (Exception e)
                {
                    throw new RuntimeException(e);
                }
            }
        };
    }

    SimpleFunctionTemplate deleteAllData()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)deleteAllData();
            }

            @Override
            public Value evaluate(Environment env,Value[] evaluatedArgs) {


                try
                {
                    return NLispTools.makeValue(mDAI.deleteAllData());
                }
                catch (Exception e)
                {
                    throw new RuntimeException(e);
                }
            }
        };
    }


    SimpleFunctionTemplate setObjectDataValue()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)setObjectDataValue();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(2, true, true);

                String name = evaluatedArgs[0].getString();
                String value = evaluatedArgs[1].serializedForm();
                String context = _DEFAULT_CONTEXT;
                if (evaluatedArgs.length>2)
                    context = evaluatedArgs[2].getString();

                int rowId;

                try
                {
                    rowId = mDAI.setData(name, context, value);

                    return NLispTools.makeValue(rowId);
                }
                catch (Exception e)
                {
                    throw new RuntimeException(e);
                }
            }
        };
    }

    SimpleFunctionTemplate deleteOldData()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)deleteOldData();
            }

            @Override
            public Value evaluate(Environment env,Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);

                Long age = evaluatedArgs[0].getIntValue();
                String context = _DEFAULT_CONTEXT;
                if (evaluatedArgs.length>1)
                    context = evaluatedArgs[1].getString();

                try
                {
                    return NLispTools.makeValue(mDAI.deleteOldData(context, age));
                }
                catch (Exception e)
                {
                    throw new RuntimeException(e);
                }
            }
        };
    }

    SimpleFunctionTemplate selectAllDataKeys()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)selectAllDataKeys();
            }

            @Override
            public Value evaluate(Environment env,Value[] evaluatedArgs) {

                String context = _DEFAULT_CONTEXT;
                if (evaluatedArgs.length>0)
                    context = evaluatedArgs[0].getString();

                try
                {
                    String[] names = mDAI.getAllKeys(context);
                    Value[] lnames = new Value[names.length];
                    for (int i = 0; i < names.length;i++)
                    {
                        lnames[i] = NLispTools.makeValue(names[i]);
                    }
                    return NLispTools.makeValue(lnames);
                }
                catch (Exception e)
                {
                    throw new RuntimeException(e);
                }
            }
        };
    }

    SimpleFunctionTemplate selectOldData()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)selectOldData();
            }

            @Override
            public Value evaluate(Environment env,Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);

                Long age = evaluatedArgs[0].getIntValue();
                String context = _DEFAULT_CONTEXT;
                if (evaluatedArgs.length>1)
                    context = evaluatedArgs[1].getString();

                try
                {
                    String[] keys = mDAI.selectOldDataNames(context, age);
                    Value[] lresults = new Value[keys.length];
                    for (int i = 0; i < keys.length; i++)
                    {
                        lresults[i] = NLispTools.makeValue(keys[i]);
                    }
                    return NLispTools.makeValue(lresults);
                }
                catch (Exception e)
                {
                    throw new RuntimeException(e);
                }
            }
        };
    }


    /**
     * Speech Functions
     */

    @Override
    public void onTTSComplete(SpeechInterface.TTS_STATUS status)
    {
        ttsComplete(status);
        if (_ttsListenerLambda != null)
        {
            Value[] args =  new Value[]{NLispTools.makeValue(status.toString())};

            FunctionTemplate actual=(FunctionTemplate)_ttsListenerLambda.clone();
            actual.setActualParameters(args);
            try
            {
                actual.evaluate(mEnv, false);
            }
            catch (Exception e)
            {
                log("Error calling on tts complete handler", e.toString());
            }


        }
    }

    @Override
    public void onASRComplete(SpeechInterface.SPEECH_STATUS status, String speech, int errorCode)
    {
        asrCompleted(status);
        if (status == SpeechInterface.SPEECH_STATUS.RECOGNITION_COMPLETE || status == SpeechInterface.SPEECH_STATUS.RECOGNITION_COMPLETE)
            mEnv.mapValue(_LAST_ASR_VARIABLE_NAME, NLispTools.makeValue(speech));
        else
            mEnv.mapValue(_LAST_ASR_VARIABLE_NAME, Environment.getNull());
        if (_asrListenerLambda != null)
        {
            Value[] args = new Value[]{(speech == null)?Environment.getNull():NLispTools.makeValue(speech), NLispTools.makeValue(status.toString()), (speech == null)?NLispTools.makeValue(SpeechInterface.mapSpeechErrorToResponse(errorCode)):Environment.getNull()};

            FunctionTemplate actual= (FunctionTemplate)_asrListenerLambda.clone();
            actual.setActualParameters(args);

            try
            {
                actual.evaluate(mEnv, false);
            }
            catch (Exception e)
            {
                log("Error calling on asr complete handler", e.toString());
            }

        }
    }

    @Override
    public void onInit(int ttsInitStatus, boolean asrAvailable)
    {
        mTtsAvailableP = ttsInitStatus == android.speech.tts.TextToSpeech.SUCCESS;
        mAsrAvailableP = asrAvailable;
        if (mAsrAvailableP)
        {
            try
            {

                mEnv.mapFunction("register-asr-listener", register_asr_listener());
                mEnv.mapFunction("start-speech-recognition", startSpeechRecognition());
            }
            catch (Exception e)
            {
                mAsrAvailableP = false;
            }
            mEnv.mapValue(_ASR_AVAILABLE_VAR_NAME, NLispTools.makeValue(mAsrAvailableP));

        }

        setupTTS();
    }

    @Override
    public void log(String tag, String message)
    {

    }

    private void setupTTS()
    {
        mEnv.mapValue(_SPEECH_AVAILABLE_VAR_NAME, NLispTools.makeValue(mTtsAvailableP));
        if (mTtsAvailableP)
        {
            mEnv.mapFunction("register-tts-listener", register_tts_listener());
            mEnv.mapFunction("tts", tts());
        }
    }

    public void setSpeechInterface(SpeechInterface.SpeechControlInterface sinterface)
    {
        mSpeechInterface = sinterface;

        mEnv.mapValue(_ASR_STATUS_VAR_NAME, Environment.getNull());
        mEnv.mapValue(_TTS_STATUS_VAR_NAME, Environment.getNull());
        mEnv.mapValue(_LAST_ASR_VARIABLE_NAME, Environment.getNull());

        onInit((mTtsAvailableP)?android.speech.tts.TextToSpeech.SUCCESS:android.speech.tts.TextToSpeech.ERROR , mAsrAvailableP);

    }

    private String ttsRequested()
    {
        mEnv.mapValue(_TTS_STATUS_VAR_NAME, NLispTools.makeValue(_TTS_STATUS_STARTED_STATUS_VALUE));
        return _TTS_STATUS_STARTED_STATUS_VALUE;
    }

    private void ttsComplete(SpeechInterface.TTS_STATUS status)
    {
        mEnv.mapValue(_TTS_STATUS_VAR_NAME, NLispTools.makeValue(status.toString()));
    }

    private String asrRequested()
    {
        mEnv.mapValue(_ASR_STATUS_VAR_NAME, NLispTools.makeValue(_ASR_STATUS_STARTED));
        return _ASR_STATUS_STARTED;

    }

    private void asrCompleted(SpeechInterface.SPEECH_STATUS status)
    {
        mEnv.mapValue(_ASR_STATUS_VAR_NAME, NLispTools.makeValue(status.toString()));
    }

    private SimpleFunctionTemplate register_tts_listener()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)register_tts_listener();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                _ttsListenerLambda = (FunctionTemplate)evaluatedArgs[0].getLambda();
                return evaluatedArgs[0];
            }

        };
    }




    private SimpleFunctionTemplate register_asr_listener()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)register_asr_listener();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                _asrListenerLambda = (FunctionTemplate)evaluatedArgs[0].getLambda();
                return evaluatedArgs[0];
            }

        };
    }

    private SimpleFunctionTemplate startSpeechRecognition()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)startSpeechRecognition();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {

                if (Looper.myLooper() != Looper.getMainLooper())
                {
                    _mainHandler.post(new Runnable()
                                      {
                                          public void run()
                                          {
                                              mSpeechInterface.initiateListening();

                                          }
                                      }
                    );
                }
                else
                {
                    mSpeechInterface.initiateListening();

                }
                return NLispTools.makeValue(asrRequested());

            }

        };
    }

    private SimpleFunctionTemplate tts()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)tts();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                final String text = evaluatedArgs[0].getString();


                mSpeechInterface.speakMessage(text);
                ttsRequested();
                return evaluatedArgs[0];
            }

        };
    }



    /**
     * First argument is a string label for the background process.
     * Second argument is a lambda function that receives the response.  Lambda shuld
     * take two arguments, first argument is the result of computation.  Second
     * argument is NULL if there was no error, otherwise, will be the string form of the error
     *
     * @return label of the process that can be used to break the computation in the background
     */
    private FunctionTemplate evaluate_background()
    {
        return new FunctionTemplate ()
        {

            public Object clone()
            {
                return evaluate_background();
            }

            @Override
            public Value evaluate(final Environment env, boolean resume)
                    throws InstantiationException, IllegalAccessException {

                try
                {
                    Value name = env.evaluate(_actualParameters[0]);

                    String tagName = name.getString();

                    Value resultCallback = env.evaluate(_actualParameters[1]);

                    if (resultCallback.isLambda())
                    {
                        final FunctionTemplate handlerFunction = resultCallback.getLambda();
                        Observer<Value> resultHandler = new Observer<Value>() {

                            @Override
                            public void onSubscribe(@NonNull Disposable d)
                            {

                            }

                            @Override
                            public void onNext(@NonNull Value value)
                            {
                                Value[] inputs = new Value[]{value, Environment.getNull()};
                                try
                                {
                                    handlerFunction.setActualParameters(inputs);
                                    handlerFunction.evaluate(env, false);
                                }
                                catch (Exception e)
                                {
                                    if (mDefaultBackgroundResultHandler!=null)
                                    {
                                        mDefaultBackgroundResultHandler.onError(e);
                                    }
                                }
                            }

                            @Override
                            public void onError(@NonNull Throwable e)
                            {
                                Value[] inputs = new Value[]{Environment.getNull(), NLispTools.makeValue(e.toString())};
                                try
                                {
                                    handlerFunction.setActualParameters(inputs);
                                    handlerFunction.evaluate(env, false);
                                }
                                catch (Exception e2)
                                {
                                    if (mDefaultBackgroundResultHandler!=null)
                                    {
                                        mDefaultBackgroundResultHandler.onError(e2);
                                    }
                                }
                            }

                            @Override
                            public void onComplete()
                            {

                            }
                        };

                        Value[] actualArguments = new Value[_actualParameters.length - 2];
                        for (int i = 2;i < _actualParameters.length;i++)
                            actualArguments[i - 2] = _actualParameters[i];
                        Value raw = Environment.wrapValuesInProgn(actualArguments);

                        evaluateExpression(raw, tagName, resultHandler, env);
                    }
                    else
                    {

                        Value[] actualArguments = new Value[_actualParameters.length - 2];
                        for (int i = 2;i < _actualParameters.length;i++)
                            actualArguments[i - 2] = _actualParameters[i];
                        Value raw = Environment.wrapValuesInProgn(actualArguments);

                        if (mDefaultBackgroundResultHandler != null)
                            evaluateExpression(raw, tagName, mDefaultBackgroundResultHandler, env);
                        else
                            evaluateExpression(raw, tagName, null, env);
                    }
                    return _actualParameters[0];
                }
                catch (Exception e)
                {
                    return Environment.getNull();
                }

            }

        };
    }


    SimpleFunctionTemplate load_page_by_title()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T) load_page_by_title();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {

                String title = evaluatedArgs[0].getString();
                Page thisPage = getCurrentPage();
                if (thisPage != null)
                {
                    String contents = thisPage.getApplication().readPageByName(title);
                    if (contents != null)
                    {
                        return env.loadFromFileLines(StringUtils.split(contents, '\n'));
                    }
                }
                return Environment.getNull();

            }
        };
    }




    SimpleFunctionTemplate show_notification()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)show_notification();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {

                int small_icon_resource_id = (int)evaluatedArgs[0].getIntValue();
                String mainTitle = evaluatedArgs[1].getString();
                Value items = evaluatedArgs[2];

                int notificationId = DEFAULT_NOTIFICATION_ID;
                if (evaluatedArgs.length > 3)
                {
                    notificationId = (int)evaluatedArgs[3].getIntValue();
                }


                NotificationCompat.Builder nBuilder = new NotificationCompat.Builder(mContext);
                nBuilder.setContentTitle(mainTitle);
                nBuilder.setSmallIcon(small_icon_resource_id);

                Intent targetIntent = new Intent(mContext, ALGBBaseActivity.class);
                TaskStackBuilder stackBuilder = TaskStackBuilder.create(mContext);
                stackBuilder.addParentStack(ALGBBaseActivity.class);
                stackBuilder.addNextIntent(targetIntent);

                PendingIntent pendingIntent = stackBuilder.getPendingIntent(0, PendingIntent.FLAG_UPDATE_CURRENT);
                nBuilder.setContentIntent(pendingIntent);
                if (items.isList() && items.getList().length>1)
                {
                    NotificationCompat.InboxStyle inboxStyle = new NotificationCompat.InboxStyle();
                    inboxStyle.setBigContentTitle(mainTitle);


                    Value[] itemValues = items.getList();
                    String[] itemNames = new String[itemValues.length];
                    for (int i = 0; i < itemNames.length;i++)
                    {

                        inboxStyle.addLine(itemValues[i].getString());
                    }
                    nBuilder.setStyle(inboxStyle);

                }
                else if (items.isString() || items.getList().length == 1)
                {
                    String message = null;
                    if (items.isString())
                        message = items.getString();
                    else
                        message = items.getList()[0].getString();
                    nBuilder.setContentText(message);

                }
                else
                {
                    return Environment.getNull();
                }

                NotificationManager mNotificationManager = (NotificationManager)mContext.getSystemService(Context.NOTIFICATION_SERVICE);

                mNotificationManager.notify(notificationId, nBuilder.build());
                return NLispTools.makeValue(mainTitle);

            }
        };
    }


    SimpleFunctionTemplate get_ringtones()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)get_ringtones();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {

                LinkedList<Pair<String, String>> ringtones = getSupportedRingtones();
                Value[] uValues = new Value[ringtones.size()];
                Value[] uriTitlePair = null;
                int i = 0;
                for (Pair<String, String> pair:ringtones)
                {
                    uriTitlePair = new Value[2];
                    uriTitlePair[0] = NLispTools.makeValue(pair.getLeft());
                    uriTitlePair[1] = NLispTools.makeValue(pair.getRight());
                    uValues[i++] = NLispTools.makeValue(uriTitlePair);
                }
                return NLispTools.makeValue(uValues);

            }
        };
    }

    SimpleFunctionTemplate get_ringtone_by_index()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T) get_ringtone_by_index();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {

                int index  = (int)evaluatedArgs[0].getIntValue();
                LinkedList<Pair<String, String>> ringtones = getSupportedRingtones();

                if (ringtones.size() > index)
                {
                    Pair<String, String> pair = ringtones.get(index);
                    return NLispTools.makeValue(pair.getRight());
                }

                return Environment.getNull();

            }
        };
    }


    SimpleFunctionTemplate playing_ringtone()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)playing_ringtone();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {

                if (evaluatedArgs.length > 0)
                {
                    int index = (int)evaluatedArgs[0].getIntValue();
                    Ringtone rtone = getRingtoneByIndex(index);
                    return NLispTools.makeValue(rtone.isPlaying());
                }

                return Environment.getNull();

            }
        };
    }





    /**
     * Use this as a template for accessing Android services that need to be executed
     * First argument is the ringtone index
     * Second argument is the number of repeations of the tone
     * Third argument is the delay between repetitions
     * Fourth argument is a result callback function
     * @return false if there is no ringtone by this index
     */
    private SimpleFunctionTemplate repeat_ringtone_by_index()
    {
        return new SimpleFunctionTemplate ()
        {

            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)repeat_ringtone_by_index();
            }

            @Override
            public Value evaluate(final Environment env, Value[] evaluatedArgs) {

                try
                {
                    final int index = (int)evaluatedArgs[0].getIntValue();

                    final int repeatCount = (int)evaluatedArgs[1].getIntValue();

                    final int pauseMilli = (int)evaluatedArgs[2].getIntValue();

                    final int maxDurationMilli = (int)evaluatedArgs[3].getIntValue();

                    Value resultCallback = evaluatedArgs[4];

                    final Ringtone ringtone = getRingtoneByIndex(index);

                    if (ringtone == null)
                        return Environment.getNull();

                    final FunctionTemplate handlerFunction = resultCallback.getLambda();
                    Observer<Value> resultHandler = new Observer<Value>() {

                        @Override
                        public void onSubscribe(@NonNull Disposable d)
                        {

                        }

                        @Override
                        public void onNext(@NonNull Value value)
                        {
                            Value[] inputs = new Value[]{value, Environment.getNull()};
                            try
                            {
                                handlerFunction.setActualParameters(inputs);
                                handlerFunction.evaluate(env, false);

                            }
                            catch (Exception e)
                            {
                                if (mDefaultBackgroundResultHandler!=null)
                                {
                                    mDefaultBackgroundResultHandler.onError(e);
                                }
                            }
                        }

                        @Override
                        public void onError(@NonNull Throwable e)
                        {
                            Value[] inputs = new Value[]{Environment.getNull(), NLispTools.makeValue(e.toString())};
                            try
                            {
                                handlerFunction.setActualParameters(inputs);
                                handlerFunction.evaluate(env, false);
                            }
                            catch (Exception e2)
                            {
                                if (mDefaultBackgroundResultHandler!=null)
                                {
                                    mDefaultBackgroundResultHandler.onError(e2);
                                }
                            }
                        }

                        @Override
                        public void onComplete()
                        {

                        }
                    };

                    ObservableOnSubscribe<Value> worker = new ObservableOnSubscribe<Value>() {
                        @Override
                        public void subscribe(@NonNull ObservableEmitter<Value> resultHandler) throws Exception
                        {

                            try
                            {

                                playRingtone(ringtone, repeatCount, pauseMilli, maxDurationMilli);
                                resultHandler.onNext(NLispTools.makeValue(index));
                            }
                            catch (Throwable er)
                            {
                                resultHandler.onError(er);
                            }
                        }
                    };

                    Observable.create(worker).subscribeOn(Schedulers.computation()).observeOn(AndroidSchedulers.mainThread()).subscribe(resultHandler);

                    return evaluatedArgs[0];
                }
                catch (Exception e)
                {
                    return Environment.getNull();
                }




            }

        };
    }


    /**
     * Stops background lisp service
     *
     * @return label of the process that can be used to break the computation in the background
     */
    private SimpleFunctionTemplate stop_background_service()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)stop_background_service();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {

                Intent intent = new Intent(mContext, BackgroundLispService.class);
                mContext.stopService(intent);
                return NLispTools.makeValue(1);

            }

        };
    }



    /**
     * First argument is a string label for the background process.
     * Second argument is a boolean parameter indicating whether to start the background service
     * Third argument is a lambda function that receives the response.  Lambda shuld
     * take two arguments, first argument is the result of computation.  Second
     * argument is NULL if there was no error, otherwise, will be the string form of the error
     *
     * @return label of the process that can be used to break the computation in the background
     */
    private FunctionTemplate evaluate_background_service()
    {
        return new FunctionTemplate ()
        {

            public Object clone()
            {
                return evaluate_background_service();
            }

            @Override
            public Value evaluate(final Environment env, boolean resume)
                    throws InstantiationException, IllegalAccessException {

                try
                {
                    Value name = env.evaluate(_actualParameters[0]);

                    String tagName = name.getString();

                    boolean startService = !env.evaluate(_actualParameters[1]).isNull();

                    Value resultCallback = env.evaluate(_actualParameters[2]);

                    if (resultCallback.isLambda())
                    {
                        final FunctionTemplate handlerFunction = resultCallback.getLambda();
                        Observer<Value> resultHandler = new Observer<Value>() {

                            @Override
                            public void onSubscribe(@NonNull Disposable d)
                            {

                            }

                            @Override
                            public void onNext(@NonNull Value value)
                            {
                                Value[] inputs = new Value[]{value, Environment.getNull()};
                                try
                                {
                                    handlerFunction.setActualParameters(inputs);
                                    handlerFunction.evaluate(env, false);
                                }
                                catch (Exception e)
                                {
                                    if (mDefaultBackgroundResultHandler!=null)
                                    {
                                        mDefaultBackgroundResultHandler.onError(e);
                                    }
                                }
                            }

                            @Override
                            public void onError(@NonNull Throwable e)
                            {
                                Value[] inputs = new Value[]{Environment.getNull(), NLispTools.makeValue(e.toString())};
                                try
                                {
                                    handlerFunction.setActualParameters(inputs);
                                    handlerFunction.evaluate(env, false);
                                }
                                catch (Exception e2)
                                {
                                    if (mDefaultBackgroundResultHandler!=null)
                                    {
                                        mDefaultBackgroundResultHandler.onError(e2);
                                    }
                                }
                            }

                            @Override
                            public void onComplete()
                            {

                            }
                        };

                        Value[] actualArguments = new Value[_actualParameters.length - 3];
                        for (int i = 3;i < _actualParameters.length;i++)
                            actualArguments[i - 3] = _actualParameters[i];
                        Value raw = Environment.wrapValuesInProgn(actualArguments);


                        evaluateBackgroundServiceExpression(env, tagName, raw, resultHandler, startService);
                    }
                    else
                    {

                        Value[] actualArguments = new Value[_actualParameters.length - 3];
                        for (int i = 3;i < _actualParameters.length;i++)
                            actualArguments[i - 3] = _actualParameters[i];
                        Value raw = Environment.wrapValuesInProgn(actualArguments);

                        if (mDefaultBackgroundResultHandler != null)
                            evaluateBackgroundServiceExpression(env, tagName, raw, mDefaultBackgroundResultHandler, startService);

                        else
                            evaluateBackgroundServiceExpression(env, tagName, raw, null, startService);
                    }
                    return _actualParameters[0];
                }
                catch (Exception e)
                {
                    return Environment.getNull();
                }




            }

        };
    }

    /**
     * First argument is a lambda function that receives the response.  Lambda shuld
     * take two arguments, first argument is the result of computation.  Second
     * argument is NULL if there was no error, otherwise, will be the string form of the error
     *
     * @return
     */
    private FunctionTemplate evaluate_foreground()
    {
        return new FunctionTemplate ()
        {

            public Object clone()
            {
                return evaluate_foreground();
            }

            @Override
            public Value evaluate(final Environment env, boolean resume)
                    throws InstantiationException, IllegalAccessException {

                try
                {

                    Value resultCallback = env.evaluate(_actualParameters[0]);

                    if (resultCallback.isLambda())
                    {
                        final FunctionTemplate handlerFunction = resultCallback.getLambda();
                        Observer<Value> resultHandler = new Observer<Value>() {

                            @Override
                            public void onSubscribe(@NonNull Disposable d)
                            {

                            }

                            @Override
                            public void onNext(@NonNull Value value)
                            {
                                Value[] inputs = new Value[]{value, Environment.getNull()};
                                try
                                {
                                    handlerFunction.setActualParameters(inputs);
                                    handlerFunction.evaluate(env, false);
                                }
                                catch (Exception e)
                                {
                                    if (mDefaultBackgroundResultHandler!=null)
                                    {
                                        mDefaultBackgroundResultHandler.onError(e);
                                    }
                                }
                            }

                            @Override
                            public void onError(@NonNull Throwable e)
                            {
                                Value[] inputs = new Value[]{Environment.getNull(), NLispTools.makeValue(e.toString())};
                                try
                                {
                                    handlerFunction.setActualParameters(inputs);
                                    handlerFunction.evaluate(env, false);
                                }
                                catch (Exception e2)
                                {
                                    if (mDefaultBackgroundResultHandler!=null)
                                    {
                                        mDefaultBackgroundResultHandler.onError(e2);
                                    }
                                }
                            }

                            @Override
                            public void onComplete()
                            {

                            }
                        };

                        Value[] actualArguments = new Value[_actualParameters.length - 1];
                        for (int i = 1;i < _actualParameters.length;i++)
                            actualArguments[i - 1] = _actualParameters[i];
                        Value raw = Environment.wrapValuesInProgn(actualArguments);

                        evaluateMainThreadExpression(env, raw, resultHandler);
                    }
                    else
                    {

                        Value[] actualArguments = new Value[_actualParameters.length - 1];
                        for (int i = 1;i < _actualParameters.length;i++)
                            actualArguments[i - 1] = _actualParameters[i];
                        Value raw = Environment.wrapValuesInProgn(actualArguments);

                        if (mDefaultBackgroundResultHandler != null)
                            evaluateMainThreadExpression(env, raw,  mDefaultBackgroundResultHandler);
                        else
                            evaluateMainThreadExpression(env, raw,  null);
                    }
                    return _actualParameters[0];
                }
                catch (Exception e)
                {
                    return Environment.getNull();
                }

            }

        };
    }

}
