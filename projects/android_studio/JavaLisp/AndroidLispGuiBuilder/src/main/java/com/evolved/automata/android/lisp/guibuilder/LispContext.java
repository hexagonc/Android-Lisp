package com.evolved.automata.android.lisp.guibuilder;

import android.app.Activity;
import android.content.Context;
import android.os.Handler;
import android.os.Looper;

import com.evolved.automata.android.lisp.AndroidLispInterpreter;
import com.evolved.automata.android.lisp.views.ViewEvaluator;
import com.evolved.automata.android.speech.SpeechInterface;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;

import java.util.LinkedList;

import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import io.reactivex.ObservableEmitter;
import io.reactivex.ObservableOnSubscribe;
import io.reactivex.Observer;
import io.reactivex.Observable;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.annotations.NonNull;

/**
 * Created by Evolved8 on 5/4/17.
 */

public class LispContext {

    static final String _DEFAULT_CONTEXT = "DEFAULT";
    Context mContext;

    LispDataAccessInterface mDAI;
    Environment mBaseEnvironment;
    Environment mEnv;

    LispContext mParent=null;

    Activity mActivity;

    AndroidLispInterpreter mAndroidInterpreter;

    LispSpeechInterface mSpeechInterface;

    Object mSynch = new Object();

    public static class Request
    {
        Observer<Value> rListener;
        Value result;
        String expr;
    }

    LispSpeechInterface.ListeningStateListener mListeningStateHandler;
    LispSpeechInterface.SpeechStateListener mSpeechStateHandler;

    FunctionTemplate _asrListenerLambda = null;
    FunctionTemplate _ttsListenerLambda = null;


    ConcurrentHashMap<String, Request> mRequestMap = new ConcurrentHashMap<String, Request>();

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


    public LispContext(Context con, Environment base, LispDataAccessInterface dai)
    {
        mContext = con;
        mBaseEnvironment  = base;
        mDAI = dai;
        mAndroidInterpreter = new AndroidLispInterpreter(mContext, mEnv, null);

        resetEnvironment();

    }

    protected LispContext(LispContext parent, Context con)
    {
        mContext = con;
        mParent = parent;
        mBaseEnvironment = mParent.getEnvironment();
        mEnv = new Environment(mBaseEnvironment);
    }

    public void setDataAccessInterface(LispDataAccessInterface dai)
    {
        mDAI = dai;
        resetEnvironment();
    }


    public void setActivity(Activity activity)
    {
        mActivity = activity;
        resetEnvironment();
    }

    public void breakEvaluation()
    {
        mRequestMap.clear();
    }

    public void breakEvaluation(String id)
    {
        mRequestMap.remove(id);
    }

    public Value evaluateSynchronous(String expression)
    {
        try
        {
            return mEnv.evaluate(expression, false);
        } catch (InstantiationException e)
        {
            throw new RuntimeException(e);
        } catch (IllegalAccessException e)
        {
            throw new RuntimeException(e);
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

    public void evaluateExpression(final String expression, final String requestId, final Observer<Value> resultListener)
    {

        final Request r = new Request();
        r.expr = expression;
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
        new Thread()
        {
            public void run()
            {
                LinkedList<String> toBeDeleted = new LinkedList<String>();
                while (mRequestMap.size() > 0)
                {
                    for (String key: mRequestMap.keySet())
                    {
                        Request request = mRequestMap.get(key);
                        try
                        {
                            Value response = null;
                            if (request.expr != null)
                            {
                                response  = mEnv.evaluate(request.expr, false);
                            }
                            else if (request.result != null)
                            {
                                response = mEnv.evaluate(request.result, true);
                            }
                            else
                            {
                                request.rListener.onError(new IllegalStateException("Invalid data response"));
                                toBeDeleted.add(key);
                            }

                            if (response.isContinuation())
                            {
                                request.result = response;
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
                    }

                }


            }
        }.start();
    }

    private void postResponse(final Value result, final Request request)
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

    private void postResponse(final Exception e, final Request request)
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
        if (mDAI != null)
        {
            addDataFunctions();
        }

        if (mActivity != null)
        {
            ViewEvaluator.bindFunctions(mEnv, mActivity, null);
        }
    }

    public Environment getEnvironment()
    {
        return mEnv;
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
                                              mSpeechInterface.startListening(mListeningStateHandler);
                                          }
                                      }
                    );
                }
                else
                {
                    mSpeechInterface.startListening(mListeningStateHandler);

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


                mSpeechInterface.startSpeaking(text, mSpeechStateHandler);
                return evaluatedArgs[0];
            }

        };
    }

}
