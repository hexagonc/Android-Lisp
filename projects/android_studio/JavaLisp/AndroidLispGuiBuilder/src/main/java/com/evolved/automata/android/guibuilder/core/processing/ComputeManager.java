package com.evolved.automata.android.guibuilder.core.processing;

import com.evolved.automata.android.guibuilder.core.processing.futures.FutureResultListener;
import com.evolved.automata.android.guibuilder.core.processing.futures.NotifyFuture;

import java.util.HashMap;
import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * Created by Evolved8 on 9/12/16.
 */
public class ComputeManager {
    ScheduledExecutorService _executor = null;

    HashMap<String, RequestFuture> _requests;

    public final int _threadPoolSize;

    Logger _logger;
    EventManager _eventManager;
    private ComputeManager(int poolSize, EventManager eventManager, Logger logger)
    {
        _threadPoolSize = poolSize;
        _requests = new HashMap<>();
        _eventManager = eventManager;
        _logger = logger;
    }

    private void logWarning(String title, String message)
    {
        if (_logger != null)
            _logger.logWarning(title, message);
    }

    public static ComputeManager create(int poolSize, EventManager eventManager)
    {
        return new ComputeManager(poolSize, eventManager, null);
    }

    public static ComputeManager create(int poolSize, EventManager eventManager, Logger logger)
    {
        return new ComputeManager(poolSize, eventManager, logger);
    }



    public <V> void setRequestValue(String requestId, V value, Exception e)
    {
        RequestFuture<V> request = _requests.get(requestId);
        request.setResult(value, e);
    }

    public <V> RequestFuture<V> addRequest(String requestId)
    {
        if (requestId == null)
            throw new IllegalArgumentException("requestId cannot be null");


        if (_requests.containsKey(requestId)) {
            logWarning("ComputeManger", "Trying to add second request future before servicing the first.  Only one request future at a time can be active of a given type");
            return null;
        }
        else
        {

            RequestFuture<V> request = new RequestFuture<>();
            _requests.put(requestId, request);
            return request;
        }

    }

    public <V> RequestFuture<V> getOpenRequest(String requestId)
    {
        RequestFuture<V> request = _requests.get(requestId);
        if (request != null && !request.isDone() && !request.isCancelled())
            return request;
        else
            return null;
    }

    public boolean hasOpenRequest(String requestId)
    {
        RequestFuture request = _requests.get(requestId);
        return (request != null && !request.isDone() && !request.isCancelled());
    }




    public void start()
    {
        if (_executor!= null && !_executor.isShutdown())
            _executor.shutdown();
        _executor = new ScheduledThreadPoolExecutor(_threadPoolSize);
    }

    public void shutdown(boolean block, int timeoutMilli) throws InterruptedException
    {
        if (!_executor.isShutdown()) {
            _executor.shutdown();
            if (block)
            {
                _executor.awaitTermination(timeoutMilli, TimeUnit.MILLISECONDS);
            }
        }
        _executor = null;
    }


    public boolean isShutdown()
    {
        return _executor.isShutdown();
    }


    /**
     * This is a future that really just generates its return value immediately.  Use this
     * when you need to wrap a result as a future to satisfy an interface but you know that
     * the actual computation can be done immediately in the same thread
     * @param resultComputer computes the result
     * @param <V>
     * @return
     */
    public <V> NotifyFuture<V> getSyncResultFuture(Callable<V> resultComputer)
    {
        try {
            final V result = resultComputer.call();

            return new NotifyFuture<V>() {
                @Override
                public void setFutureListener(FutureResultListener<V> listener) {
                    listener.onResult(result, null);
                }

                @Override
                public Runnable getProcessRunnable() {
                    return null;
                }

                @Override
                public boolean cancel(boolean mayInterruptIfRunning) {
                    return false;
                }

                @Override
                public boolean isCancelled() {
                    return false;
                }

                @Override
                public boolean isDone() {
                    return true;
                }

                @Override
                public V get() throws InterruptedException, ExecutionException {
                    return result;
                }

                @Override
                public V get(long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
                    return result;
                }
            };

        }
        catch (final Exception e)
        {
            // TODO: throw an error event
            return new NotifyFuture<V>() {


                @Override
                public void setFutureListener(FutureResultListener<V> listener) {
                    listener.onResult(null, e);
                }

                @Override
                public Runnable getProcessRunnable() {
                    return null;
                }

                @Override
                public boolean cancel(boolean mayInterruptIfRunning) {
                    return false;
                }

                @Override
                public boolean isCancelled() {
                    return false;
                }

                @Override
                public boolean isDone() {
                    return false;
                }

                @Override
                public V get() throws InterruptedException, ExecutionException {
                    return null;
                }

                @Override
                public V get(long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
                    return null;
                }
            };
        }




    }


    /**
     * Use this when you need to wrap a computation that should be executed on the current thread as a future
     * to satisfy an interface.  The result is computed immediately on the thread that calls get() at the
     * point that the method is called.  If get() is called with a timeout then the execution of resultComputer
     * will be interrupted if it exceeds the timeout.  While waiting for the timeout, you can also call cancel(true)
     * which will also interrupt execution of the resultComputer
     * @param resultComputer computes the result
     * @param <V>
     * @return a future
     */
    public <V> NotifyFuture<V> getLazySyncResultFuture(final Callable<V> resultComputer)
    {

        return new NotifyFuture<V>() {
            Thread timeoutThread = null;
            FutureResultListener<V> flistener;
            Exception callException;

            V result;
            boolean done = false;
            boolean cancelled;

            @Override
            public void setFutureListener(FutureResultListener<V> listener) {
                if (done)
                    listener.onResult(result, callException);
                else
                    flistener = listener;
            }

            @Override
            public Runnable getProcessRunnable() {
                return null;
            }

            @Override
            public boolean cancel(boolean mayInterruptIfRunning) {
                if (cancelled)
                    return true;
                if (!done) {
                    if (mayInterruptIfRunning && timeoutThread != null && timeoutThread.isAlive())
                        timeoutThread.interrupt();
                    return cancelled = true;
                }
                return false;

            }

            @Override
            public boolean isCancelled() {
                return cancelled;
            }

            @Override
            public boolean isDone() {
                return done;
            }

            @Override
            public V get() throws InterruptedException, ExecutionException {
                if (done || cancelled)
                    return result;
                try
                {
                    result = resultComputer.call();

                    if (flistener != null)
                        flistener.onResult(result, null);
                    return result;
                }
                catch (Exception e)
                {
                    // TODO: throw an error event
                    if (flistener != null)
                        flistener.onResult(result, e);
                    throw new RuntimeException(e);
                }
                finally {
                    done = true;
                }

            }

            @Override
            public V get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
                if (done  || cancelled)
                    return result;

                if (timeout == 0)
                    return get();
                final Thread current = Thread.currentThread();
                timeoutThread = new Thread() {
                    public void run() {
                        try {
                            if (cancelled)
                            {
                                current.interrupt();
                                return;
                            }
                            Thread.sleep(unit.toMillis(timeout));
                            current.interrupt();

                        }
                        catch (InterruptedException ie) {
                            // this is normal
                            if (cancelled) // if interrupted due to cancel
                                current.interrupt();
                        }

                    }
                };

                try
                {

                    timeoutThread.start();
                    result = resultComputer.call();
                    if (flistener != null)
                        flistener.onResult(result, null);
                    done = true;
                    return result;
                }
                catch (ExecutionException | TimeoutException | InterruptedException ie) {
                    if (flistener != null)
                        flistener.onResult(result, ie);
                    throw ie;
                }
                catch (Exception e)
                {
                    // TODO: throw an error event

                    // This is a normal execution error
                    if (flistener != null)
                        flistener.onResult(result, e);
                    throw new RuntimeException(e);
                }
                finally {

                    timeoutThread.interrupt();

                }

            }
        };

    }





    /**
     * This performs a background task in a lazy manner in that it doesn't even schedule the
     * computation of the future's result until the get() method is called
     * @param resultComputer object that computes the result of the future
     * @param <V>
     * @return Future<V>
     */
    public <V> NotifyFuture<V> getLazyBackgroundComputeFuture(final Callable<V> resultComputer)
    {

        final CountDownLatch latch = new CountDownLatch(1);
        return new NotifyFuture<V>()
        {
            V out;
            FutureResultListener<V> listener;
            boolean cancelled = false;
            boolean started = false;

            NotifyFuture me;


            public Runnable getProcessRunnable()
            {
                return new Runnable() {
                    @Override
                    public void run() {

                        synchronized (me)
                        {
                            if (cancelled)
                                return;
                            started = true;
                        }


                        try
                        {
                            V temp  = resultComputer.call();

                            synchronized (me)
                            {
                                out = temp;

                                if (listener != null)
                                    listener.onResult(out, null);
                            }

                            latch.countDown();
                        }
                        catch (Exception e)
                        {
                            synchronized (me)
                            {
                                if (listener != null)
                                    listener.onResult(out, e);
                                out = null;
                            }

                        }


                    }
                };
            }

            @Override
            public synchronized boolean cancel(boolean mayInterruptIfRunning) {

                if (out != null)
                    return false;
                else if (!started)
                    cancelled = true;
                return false;

            }

            @Override
            public synchronized  boolean isCancelled() {

                return cancelled;
            }

            @Override
            public synchronized boolean isDone() {
                return false;
            }

            @Override
            public synchronized V get() throws InterruptedException, ExecutionException {
                if (out != null || cancelled || _executor == null || _executor.isShutdown())
                    return out;

                _executor.submit(getProcessRunnable());
                latch.await();
                return out;
            }

            @Override
            public synchronized V get(long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {

                if (out != null || cancelled || _executor == null || _executor.isShutdown())
                    return out;
                _executor.submit(getProcessRunnable());
                latch.await(timeout, unit);
                return out;
            }

            @Override
            public synchronized void setFutureListener(FutureResultListener listener) {
                this.listener = listener;
            }
        };
    }

    /**
     * This Future schedules the execution of resultComputer immediately, at the point that the
     * future is created so that by the time get() is called, the computation may already be complete
     * @param resultComputer Object that does the computation
     * @param <V>
     * @return
     */
    public <V> NotifyFuture<V> getBackgroundComputeFuture(final Callable<V> resultComputer)
    {
        final CountDownLatch latch = new CountDownLatch(1);
        NotifyFuture f =  new NotifyFuture<V>()
        {
            V out;
            FutureResultListener<V> listener;
            boolean cancelled = false;
            boolean started = false;

            NotifyFuture me;


            public Runnable getProcessRunnable()
            {
                return new Runnable() {
                    @Override
                    public void run() {

                        synchronized (me)
                        {
                            if (cancelled)
                                return;
                            started = true;
                        }


                        try
                        {
                            V temp  = resultComputer.call();

                            synchronized (me)
                            {
                                out = temp;

                                if (listener != null)
                                    listener.onResult(out, null);
                            }

                            latch.countDown();
                        }
                        catch (Exception e)
                        {
                            synchronized (me)
                            {
                                if (listener != null)
                                    listener.onResult(out, e);
                                out = null;
                            }

                        }


                    }
                };
            }

            @Override
            public synchronized boolean cancel(boolean mayInterruptIfRunning) {

                if (out != null)
                    return false;
                else if (!started)
                    cancelled = true;
                return false;

            }

            @Override
            public synchronized  boolean isCancelled() {

                return cancelled;
            }

            @Override
            public synchronized boolean isDone() {
                return false;
            }

            @Override
            public synchronized V get() throws InterruptedException, ExecutionException {
                if (out != null || cancelled || _executor == null  || _executor.isShutdown())
                    return out;

                latch.await();
                return out;
            }

            @Override
            public synchronized V get(long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {

                if (out != null || cancelled || _executor == null  || _executor.isShutdown())
                    return out;

                latch.await(timeout, unit);
                return out;
            }

            @Override
            public synchronized void setFutureListener(FutureResultListener listener) {
                this.listener = listener;
            }
        };

        _executor.submit(f.getProcessRunnable());
        return f;

    }


}
