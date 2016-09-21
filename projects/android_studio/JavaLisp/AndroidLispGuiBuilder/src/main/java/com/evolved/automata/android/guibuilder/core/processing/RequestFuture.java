package com.evolved.automata.android.guibuilder.core.processing;

import com.evolved.automata.android.guibuilder.core.processing.futures.FutureResultListener;
import com.evolved.automata.android.guibuilder.core.processing.futures.NotifyFuture;

import java.util.Timer;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * Created by Evolved8 on 9/12/16.
 */
public class RequestFuture<V> implements NotifyFuture<V> {

    FutureResultListener<V> flistener;
    boolean cancelled;
    boolean done =false;
    CountDownLatch waitLatch = new CountDownLatch(1);
    V result;

    synchronized void setResult(V theResult, Exception e)
    {
        if (!done && !cancelled) {
            result = theResult;
            if (flistener != null)
                flistener.onResult(theResult, e);
            done = true;
            waitLatch.countDown();
        }

    }


    @Override
    public void setFutureListener(FutureResultListener<V> listener) {
        flistener = listener;
    }

    @Override
    public Runnable getProcessRunnable() {
        return null;
    }

    @Override
    public synchronized boolean cancel(boolean mayInterruptIfRunning) {
        if (cancelled)
            return true;
        if (waitLatch.getCount() > 0) {
            waitLatch.countDown();
            return cancelled = true;

        }
        return false;
    }

    @Override
    public synchronized boolean isCancelled() {

        return cancelled;
    }

    @Override
    public synchronized boolean isDone() {
        return done;
    }

    @Override
    public synchronized V get() throws InterruptedException, ExecutionException {
        return result;
    }

    @Override
    public synchronized V get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {

        if (done || cancelled)
            return result;

        try {
            waitLatch.await(timeout, unit);
        }
        finally {
            done = true;
            return result;
        }

    }
}
