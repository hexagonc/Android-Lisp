package com.evolved.automata.android.lisp.guibuilder;


import android.app.IntentService;
import android.content.Context;
import android.content.Intent;

import android.os.Handler;
import android.os.PowerManager;
import android.support.annotation.Nullable;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.Value;

import java.util.concurrent.LinkedBlockingQueue;

import io.reactivex.Observable;

/**
 * Created by Evolved8 on 8/9/17.
 */

public class BackgroundLispService extends IntentService {

    final String _LOCK_TIMEOUT_SECS = "LOCK TIMEOUT SECONDS";

    static LinkedBlockingQueue<LispContext.Request> mRequestQueue = new LinkedBlockingQueue<LispContext.Request>();
    private static PowerManager.WakeLock mLispLock = null;

    static Object mLockSynch = new Object();

    PowerManager mPowerManager;

    final String mLockName = "LispService";

    Handler mMainHandler;

    public BackgroundLispService()
    {
        super("ALGB Lisp Processing");

        mMainHandler = new Handler();
    }



    public static void addRequest(LispContext.Request request)
    {
        try
        {
            mRequestQueue.put(request);

        }
        catch (InterruptedException e)
        {

        }
    }

    void acquireLock()
    {
        synchronized (mLockSynch)
        {
            if (mLispLock == null)
            {

                mLispLock = mPowerManager.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, mLockName);

            }
            if (!mLispLock.isHeld())
                mLispLock.acquire();
        }
    }

    void acquireLock(int timeout)
    {
        synchronized (mLockSynch)
        {
            if (mLispLock == null)
            {

                mLispLock = mPowerManager.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, mLockName);

            }
            if (!mLispLock.isHeld())
                mLispLock.acquire(timeout);
        }
    }

    void releaseLock()
    {
        synchronized (mLockSynch)
        {
            if (mLispLock != null && mLispLock.isHeld())
            {
                mLispLock.release();

            }

        }
    }


    @Override
    protected void onHandleIntent(@Nullable Intent intent)
    {
        if (mPowerManager == null)
            mPowerManager = (PowerManager)getSystemService(Context.POWER_SERVICE);
        try
        {
            boolean hasTimeout = intent.hasExtra(_LOCK_TIMEOUT_SECS);
            if (hasTimeout)
            {
                int timeout = intent.getIntExtra(_LOCK_TIMEOUT_SECS, 0);
                acquireLock(timeout);
            }
            else
                acquireLock();
            LispContext.Request request = null;
            while (mRequestQueue.size() > 0)
            {
                request = mRequestQueue.take();
                if (request.deletedP)
                    continue;
                try
                {

                    Environment env = request.evaluationEnv;
                    Value result=null;
                    FunctionTemplate template = null;
                    if (request.expr != null)
                    {
                        result = env.evaluate(request.expr, true);

                    }
                    else if (request.precompiledExpr != null)
                    {
                        result = env.evaluate(request.precompiledExpr, false);
                    }
                    else if (request.result != null && request.result.isContinuation() && request.result.getContinuingFunction()!=null)
                    {
                        template = request.result.getContinuingFunction();
                        result = template.evaluate(env, true);
                    }

                    if (result!=null && result.isContinuation())
                    {
                        request.expr = null;
                        request.precompiledExpr = null;
                        request.result = result;
                        mRequestQueue.put(request);
                    }
                    else if (request.rListener != null && result != null)
                    {

                        postResponse(result, request);

                    }
                }
                catch (InterruptedException ie)
                {
                    Thread.currentThread().interrupt();
                }
                catch (Exception e)
                {
                    if (request.rListener != null)
                    {
                        postResponse(e, request);
                    }
                    else
                    {
                        EventLog.EventSource source = new EventLog.LispGlobalSource();
                        EventLog.get().logEvent(source, EventLog.EntryType.ERROR, "Background service error", e.toString());
                    }
                }
            }
            releaseLock();


        }
        catch (InterruptedException ie)
        {

        }
        catch (Exception e)
        {
            EventLog.EventSource source = new EventLog.LispGlobalSource();
            EventLog.get().logEvent(source, EventLog.EntryType.ERROR, "Background service error", e.toString());

        }
    }

    private void postResponse(final Value result, final LispContext.Request request)
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
            mMainHandler.post(r);
        }
    }

    private void postResponse(final Exception e, final LispContext.Request request)
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
            mMainHandler.post(r);
        }

    }
}