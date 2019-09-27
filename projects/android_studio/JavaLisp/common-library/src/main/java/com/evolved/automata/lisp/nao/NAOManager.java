package com.evolved.automata.lisp.nao;
import com.aldebaran.qi.AnyObject;
import com.aldebaran.qi.Application;
import com.aldebaran.qi.Future;
import com.aldebaran.qi.QiRuntimeException;
import com.aldebaran.qi.Session;
import com.aldebaran.qi.Tuple;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.IntHashtableValue;
import com.evolved.automata.lisp.LambdaValue;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.StringHashtableValue;
import com.evolved.automata.lisp.Value;

import org.apache.commons.lang3.concurrent.ConcurrentException;

import java.sql.Time;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import static com.evolved.automata.lisp.nao.TypeHelper.convertQiValue;

/**
 * Created by Evolved8 on 6/19/17.
 */

public class NAOManager {

    public int DEFAULT_SERVICE_REQUEST_TIMEOUT_MILLI = 4000;
    private int mServicePort = 9559;

    public static final String UNHANDLED_QI_TYPE_ERROR_MESSAGE_PREFIX = "Unhandled Qi Return Type: ";
    public static final String CANCELLED_FUTURE_RESULT_ERROR_MESSAGE = "Result cancelled";
    public interface AsyncResponseHandler
    {
        /**
         * Receives a lambda function with actual arguments preset with the result of the
         * future.  This handler should not throw any exceptions as they will be ignored.
         * @param value
         */
        void onResultToEvaluate(LambdaValue value);

        /**
         * Receives a lambda function with actual parameters preset to a string value
         * representing an error message.  This handler should not throw any exceptions
         * @param value
         */
        void onErrorToEvaluate(LambdaValue value);
    }



    public static class QiCommandSpec
    {
        TypeHelper.QiArgumentType[] _arguments;
        String _functionName;
        boolean _hasReturn;

        private QiCommandSpec(String name, TypeHelper.QiArgumentType[] args, boolean hasReturn)
        {
            _functionName = name;
            _arguments = args;
            _hasReturn = hasReturn;
        }

        public static QiCommandSpec from(String name, TypeHelper.QiArgumentType[] args, boolean hasReturn)
        {
            return new QiCommandSpec(name, args, hasReturn);
        }
    }





    static final HashMap<TypeHelper.QiArgumentType,TypeHelper.LispToQiTypeConverer> mLispToQiConversionMap;


    static class FutureWrapper <T>
    {
        Future<T> _future;
        LambdaValue _resultReceiver;
        LambdaValue _errorReceiver;
        AsyncResponseHandler _responseHandler;
        boolean _finishedP = false;

        public FutureWrapper(Future<T> payload, LambdaValue receiver, LambdaValue errorHandler, AsyncResponseHandler responseHandler)
        {
            _future = payload;
            _resultReceiver = receiver;
            _errorReceiver = errorHandler;
            _responseHandler = responseHandler;
        }

        boolean isFutureComplete()
        {
            return _future.isDone() || _future.hasError() || _future.isCancelled();
        }

        boolean isHandled()
        {
            return _finishedP;
        }

        void handleResponse()
        {
            try
            {
                if (isFutureComplete())
                {
                    if (!_finishedP)
                    {
                        if (_future.isDone())
                        {
                            Object qiResult = _future.getValue();
                            Value result = convertQiValue(qiResult);
                            if (result != null)
                            {
                                if (_resultReceiver != null && _responseHandler != null)
                                {
                                    _resultReceiver.getLambda().setActualParameters(new Value[]{result});
                                    _responseHandler.onResultToEvaluate(_resultReceiver);
                                }
                            }
                            else if (_errorReceiver != null && _responseHandler != null)
                            {
                                String errorMessage;
                                if (qiResult == null)
                                    errorMessage = "Null Qi return type";
                                else
                                {
                                    errorMessage = UNHANDLED_QI_TYPE_ERROR_MESSAGE_PREFIX + " (" + qiResult.getClass().toString() + ")" + qiResult.toString();
                                }
                                _errorReceiver.getLambda().setActualParameters(new Value[]{NLispTools.makeValue(errorMessage)});
                                _responseHandler.onErrorToEvaluate(_errorReceiver);
                            }
                        }
                        else if (_future.hasError())
                        {
                            if (_errorReceiver != null && _responseHandler != null)
                            {
                                String errorMessage = _future.getErrorMessage();
                                _errorReceiver.getLambda().setActualParameters(new Value[]{NLispTools.makeValue(errorMessage)});
                                _responseHandler.onErrorToEvaluate(_errorReceiver);
                            }
                        }
                        else if (_future.isCancelled())
                        {
                            if (_errorReceiver != null && _responseHandler != null)
                            {
                                String errorMessage = CANCELLED_FUTURE_RESULT_ERROR_MESSAGE;
                                _errorReceiver.getLambda().setActualParameters(new Value[]{NLispTools.makeValue(errorMessage)});
                                _responseHandler.onErrorToEvaluate(_errorReceiver);
                            }
                        }
                        _finishedP = true;
                    }

                }

            }
            catch (Exception e)
            {
                // this shouldn't be possible since future is complete
                e.printStackTrace();
            }


        }
    }

    CopyOnWriteArrayList<FutureWrapper> mFutureWaitList = new CopyOnWriteArrayList<FutureWrapper>();


    private <T> void  addNewWrapper(FutureWrapper wrapper)
    {
        synchronized (mFutureWaitList)
        {
            mFutureWaitList.add(wrapper);
            if (mFutureWaitList.size() == 0)
            {
                startNewWorkerThread();
            }
        }
    }




    static Session mCurrentSession;
    static Application mCurrentApplication;
    static String mSessionIp = null;
    static HashMap<String, AnyObject> mServiceMap = new HashMap<String, AnyObject>();

    NAOMovementManager mMovementManager = null;





    static final HashMap<Class, TypeHelper.QiTypeConverter> mConversionMap;

    static final HashMap<String, HashMap<String, QiCommandSpec>> mQiFunctionMap = new HashMap<String, HashMap<String, QiCommandSpec>>();


    Session.ConnectionListener mSessionListener;
    static {
        mConversionMap = new HashMap<Class, TypeHelper.QiTypeConverter>();

        mLispToQiConversionMap = new HashMap<TypeHelper.QiArgumentType,TypeHelper.LispToQiTypeConverer>();

        // Add type conversion functions

        TypeHelper.addLispToQiTypeConverters();
        TypeHelper.addQiObjectToLispTypeConverters();

        // Add movement functions
        NAOMovementManager.addMovementFunctions();
        NAOSensorManager.addMemoryFunctions();
        NAOVisionManager.addVisionFunctions();
    }

    public NAOManager()
    {

    }


    static void addLispValueToQiValueConversion(TypeHelper.QiArgumentType targetType, TypeHelper.LispToQiTypeConverer converter)
    {
        mLispToQiConversionMap.put(targetType, converter);
    }

    /**
     * Add a Functionoid that converts an object representing a Qi type to a Value type
     * @param qiClass
     * @param converter
     */
    static void addQiValueToLispValueConversion(Class qiClass, TypeHelper.QiTypeConverter converter)
    {
        mConversionMap.put(qiClass, converter);
    }

    static void addQiFunctionSpec(String serviceName, String name, TypeHelper.QiArgumentType[] args, boolean hasReturn)
    {
        HashMap<String, QiCommandSpec> serviceMap = mQiFunctionMap.get(serviceName);
        if (serviceMap == null)
        {
            mQiFunctionMap.put(serviceName, serviceMap = new HashMap<String, QiCommandSpec>());
        }
        serviceMap.put(name, new QiCommandSpec(name, args, hasReturn));
    }




    private Session getSession()
    {
        if (mCurrentSession == null)
        {
            // Can't create Application on Android.  Have to comment this out for now
            // until figure out what's going on
            /*
            if (mCurrentApplication == null)
            {
                mCurrentApplication = new Application(new String[0]);
                mCurrentApplication.start();
                mCurrentSession = mCurrentApplication.session();
            }
            else
                mCurrentSession = mCurrentApplication.session();
            if (mCurrentSession == null)
                throw new QiRuntimeException("Cannot create new session object");
                */
            mCurrentSession = new Session();
        }

        return mCurrentSession;
    }

    public void connectToNAO(String ipEquivalent, int timeout, TimeUnit unit) throws TimeoutException, ExecutionException
    {
        String address = "tcp://" + ipEquivalent + ":" + mServicePort;
        Session session = getSession();

        if (session.isConnected())
        {
            if (mSessionIp.equals(address))
                return;
            else
            {
                if (mSessionListener!=null)
                    session.removeConnectionListener(mSessionListener);
                session.close();
                mServiceMap.clear();
                session.addConnectionListener(mSessionListener);
            }
        }

        if (unit == null)
            session.connect(address).get();
        else
            session.connect(address).get(timeout, TimeUnit.MILLISECONDS);
        mSessionIp = address;
    }

    public void connectToNAO(String ipEquivalent) throws ExecutionException
    {
        try
        {
            connectToNAO(ipEquivalent, 0, null);
        }
        catch (TimeoutException te)
        {
            // Never called
        }
    }

    public void disconnectFromCurrentSession()
    {
        Session session = getSession();
        if (session!=null)
        {
            session.close();
        }
    }

    public boolean addNAOConnectionListener(Session.ConnectionListener listener)
    {
        mSessionListener = listener;
        Session s = getSession();
        if (s != null)
        {
            s.addConnectionListener(listener);
            return true;
        }
        else
            return false;
    }

    AnyObject getService(String name)
    {
        Session session = getSession();
        if (session != null)
        {
            AnyObject existing = mServiceMap.get(name);
            if (existing == null)
            {
                try
                {
                    existing = session.service(name).get(DEFAULT_SERVICE_REQUEST_TIMEOUT_MILLI, TimeUnit.MILLISECONDS);
                    mServiceMap.put(name, existing);
                    return existing;
                }
                catch (ExecutionException ee)
                {
                    throw new QiRuntimeException("Service name is not implemented or available: " + name);
                }
                catch (TimeoutException te)
                {
                    throw new QiRuntimeException("Service name is not implemented or available: " + name);
                }
            }
            else
                return existing;

        }
        else
            throw new QiRuntimeException("Can't connect to service: " + name + " because the NAO is disconnected");

    }

    Value evaluateFunction(String serviceName, QiCommandSpec spec, Value[] args) throws ExecutionException
    {
        Session session = getSession();
        if (!session.isConnected())
        {
            throw new QiRuntimeException("Not connected to a session");
        }
        AnyObject serviceProxy = getService(serviceName);

        Object[] qiargs = new Object[spec._arguments.length];
        Value lispArg;
        TypeHelper.QiArgumentType[] expectedArgs = spec._arguments;
        for (int i = 0;i < args.length;i++)
        {
            lispArg = args[i];
            TypeHelper.QiArgumentType expectedArgType = expectedArgs[i];
            TypeHelper.LispToQiTypeConverer converter = mLispToQiConversionMap.get(expectedArgType);
            if (converter == null)
                throw new QiRuntimeException("Unconvertible Qi argument type: " + expectedArgType.name());

            qiargs[i] = converter.getQiValue(lispArg);
        }

        boolean hasReturnType = spec._hasReturn;
        String functionName = spec._functionName;
        if (hasReturnType)
        {
            Object result = serviceProxy.call(functionName, qiargs).get();
            return convertQiValue(result);
        }
        else
        {
            serviceProxy.call(functionName, qiargs).get();
            return null;
        }
    }

    public Value evaluateFunction(String serviceName, String functionName, Value[] args) throws ExecutionException
    {
        HashMap<String, QiCommandSpec> serviceMap = mQiFunctionMap.get(serviceName);
        if (serviceMap == null)
        {
            throw new QiRuntimeException("Undefined service name: " + serviceName);
        }


        QiCommandSpec spec = serviceMap.get(functionName);
        if (spec == null)
        {
            throw new QiRuntimeException("Function name: " + functionName + " is undefined for service: " + serviceName);

        }
        return evaluateFunction(serviceName, spec, args);
    }



    public NAOMovementManager getMovementManager()
    {
        if (mMovementManager == null)
        {
            mMovementManager = new NAOMovementManager(this);
        }
        return mMovementManager;
    }



    private void startNewWorkerThread()
    {


        java.util.concurrent.Future<String> result = new java.util.concurrent.Future<String>()
        {

            @Override
            public boolean cancel(boolean b)
            {
                return false;
            }

            @Override
            public boolean isCancelled()
            {
                return false;
            }

            @Override
            public boolean isDone()
            {
                return false;
            }

            @Override
            public String get() throws InterruptedException, ExecutionException
            {
                return null;
            }

            @Override
            public String get(long l, TimeUnit timeUnit) throws InterruptedException, ExecutionException, TimeoutException
            {
                return null;
            }
        };



        Thread processThread = new Thread()
        {
            public void run()
            {
                boolean cont = false;
                int initialItemCount = 0;
                try
                {
                    do
                    {
                        synchronized (mFutureWaitList)
                        {
                            initialItemCount = mFutureWaitList.size();
                        }
                        for (FutureWrapper future: mFutureWaitList)
                        {
                            if (future.isFutureComplete())
                            {
                                if (!future.isHandled())
                                {
                                    future.handleResponse();
                                }
                            }
                            else
                                cont = true;
                        }
                        if (!cont)
                        {
                            synchronized (mFutureWaitList)
                            {
                                if (initialItemCount  == mFutureWaitList.size())
                                    mFutureWaitList.clear();
                                else
                                    cont = true;
                            }
                        }

                    }while (cont);
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    // shouldn't be any exceptions from this
                }

            }
        };
        processThread.start();
    }





    @Override
    protected void finalize() throws Throwable {
        if (mCurrentSession != null && mCurrentSession.isConnected())
        {
            mCurrentSession.close();

        }
        if (mCurrentApplication != null)
            mCurrentApplication.stop();
        super.finalize();
    }
}
