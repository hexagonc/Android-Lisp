package com.evolved.automata.android.lisp.guibuilder;

import android.app.Activity;
import android.content.Context;
import android.os.Handler;
import android.os.Looper;
import com.evolved.automata.android.speech.SpeechInterface.SpeechListener;
import com.evolved.automata.android.lisp.AndroidLispInterpreter;
import com.evolved.automata.android.lisp.views.ViewEvaluator;
import com.evolved.automata.android.speech.SpeechInterface;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.LambdaValue;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;

import org.apache.commons.lang3.StringUtils;

import java.util.LinkedList;

import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import io.reactivex.ObservableEmitter;
import io.reactivex.ObservableOnSubscribe;
import io.reactivex.Observer;
import io.reactivex.Observable;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.annotations.NonNull;
import io.reactivex.disposables.Disposable;

/**
 * Created by Evolved8 on 5/4/17.
 */

public class LispContext implements SpeechListener{

    static final String _DEFAULT_CONTEXT = "DEFAULT";
    Context mContext;

    LispDataAccessInterface mDAI;
    Environment mBaseEnvironment;
    Environment mEnv;

    LispContext mParent=null;

    Activity mActivity;

    AndroidLispInterpreter mAndroidInterpreter;

    SpeechInterface.SpeechControlInterface mSpeechInterface;


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
    public static final String _LAST_ASR_VARIABLE_NAME = "LAST-RECOGNITION";


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


    public void evaluateMainThreadExpression(final Environment env, final Value preparsed, final Observer<Value> listener)
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
                mAndroidInterpreter.evaluatePreParsedValue(env, preparsed, true);
            }
        }).subscribeOn(AndroidSchedulers.mainThread()).observeOn(AndroidSchedulers.mainThread());

        out.subscribe(listener);

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
        out.subscribe(resultListener);
    }


    public void evaluateExpression(Value preCompiledValue,  Observer<Value> resultListener)
    {
        evaluateExpression(preCompiledValue, UUID.randomUUID().toString(), resultListener);
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
                    }

                }


            }
        }.start();
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
        if (mDAI != null)
        {
            addDataFunctions();
        }

        if (mActivity != null)
        {
            ViewEvaluator.bindFunctions(mEnv, mActivity, null);
        }


        addProcessingFunctions();
    }

    public void addProcessingFunctions()
    {
        mEnv.mapFunction("evaluate-background", evaluate_background());
        mEnv.mapFunction("evaluate-foreground", evaluate_foreground());
        mEnv.mapFunction("break", getBreak());
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

                        evaluateExpression(raw, tagName, resultHandler);
                    }
                    else
                    {

                        Value[] actualArguments = new Value[_actualParameters.length - 2];
                        for (int i = 2;i < _actualParameters.length;i++)
                            actualArguments[i - 2] = _actualParameters[i];
                        Value raw = Environment.wrapValuesInProgn(actualArguments);

                        if (mDefaultBackgroundResultHandler != null)
                            evaluateExpression(raw, tagName, mDefaultBackgroundResultHandler);
                        else
                            evaluateExpression(raw, tagName, null);
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
