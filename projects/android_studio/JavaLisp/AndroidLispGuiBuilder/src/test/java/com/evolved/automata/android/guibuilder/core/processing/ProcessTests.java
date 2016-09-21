package com.evolved.automata.android.guibuilder.core.processing;


import com.evolved.automata.android.guibuilder.core.processing.futures.NotifyFuture;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * Created by Evolved8 on 9/19/16.
 */
public class ProcessTests {


    boolean useRealEventManager = false;
    public static class ValueWrapper {
        public Object value = null;


    }


    Logger _logger = null;

    @Before
    public void initialize()
    {
        _logger = new TestLogger();
    }

    EventManager getEventManager()
    {

        EventManager emanager = null;
        if (useRealEventManager)
            emanager = EventManager.create(_logger);

        return emanager;
    }

    @Ignore
    @Test
    public void testEventManagerCreation()
    {


        boolean success = false;
        try
        {
            EventManager emanager = getEventManager();
            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        Assert.assertTrue(success);
    }

    @Test
    public void testComputeManagerCreation()
    {

        boolean success = false;
        try
        {
            EventManager emanager = getEventManager();
            ComputeManager cmanager = ComputeManager.create(10, emanager,_logger);
            success = true;
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        Assert.assertTrue(success);

    }

    @Test
    public void testSimpleSynchFutureWithTimeout()
    {

        boolean success = false;

        try
        {
            EventManager emanager = getEventManager();
            ComputeManager cmanager = ComputeManager.create(10, emanager,_logger);


            final ValueWrapper wrapper = new ValueWrapper();
            final Thread callThread = Thread.currentThread();

            Callable<Long> getTime = new Callable<Long>() {
                @Override
                public Long call() throws Exception {

                    Long out = Long.valueOf(System.currentTimeMillis());
                    wrapper.value = out;

                    Thread runThread = Thread.currentThread();

                    Assert.assertTrue("Call thread different from run thread", runThread.equals(callThread));
                    return out;
                }
            };

            NotifyFuture<Long> result = cmanager.getSyncResultFuture(getTime);

            // Since this isn't a lazy Future, the result is computed at the point of
            // creation.  wrapper should have a value at this point
            success = wrapper.value != null && result.isDone();

            Long resultValue = result.get(2000, TimeUnit.MILLISECONDS);
            success = success && wrapper.value.equals(resultValue);

            // result is calculated immediately so there is no delay.  Less than 10 millisecond expected
            // run time

            success = success && (System.currentTimeMillis() - resultValue.longValue() < 10);

        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        Assert.assertTrue("Failed to satisfy all conditions for synchronized Future with delay", success);
    }


    @Test
    public void testSimpleSynchFutureWithoutTimeout()
    {

        boolean success = false;

        try
        {
            EventManager emanager = getEventManager();
            ComputeManager cmanager = ComputeManager.create(10, emanager,_logger);


            final ValueWrapper wrapper = new ValueWrapper();
            final Thread callThread = Thread.currentThread();

            Callable<Long> getTime = new Callable<Long>() {
                @Override
                public Long call() throws Exception {

                    Long out = Long.valueOf(System.currentTimeMillis());
                    wrapper.value = out;

                    Thread runThread = Thread.currentThread();

                    Assert.assertTrue("Call thread different from run thread", runThread.equals(callThread));
                    return out;
                }
            };

            NotifyFuture<Long> result = cmanager.getSyncResultFuture(getTime);

            // Since this isn't a lazy Future, the result is computed at the point of
            // creation.  wrapper should have a value at this point
            success = wrapper.value != null && result.isDone();

            success = success && wrapper.value.equals(result.get());


        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        Assert.assertTrue("Failed to satisfy all conditions for synchronized Future", success);
    }



    @Test
    public void testSimpleLazySynchFuture()
    {

        boolean success = false;

        try
        {
            EventManager emanager = getEventManager();
            ComputeManager cmanager = ComputeManager.create(10, emanager,_logger);


            final Thread callThread = Thread.currentThread();

            final ValueWrapper wrapper = new ValueWrapper();

            Callable<Long> getTime = new Callable<Long>() {
                @Override
                public Long call() throws Exception {

                    Long out = Long.valueOf(System.currentTimeMillis());
                    wrapper.value = out;


                    Thread runThread = Thread.currentThread();
                    Assert.assertTrue("Run thread of delayed synch future different from call thread", runThread.equals(callThread));
                    return out;
                }
            };

            NotifyFuture<Long> result = cmanager.getLazySyncResultFuture(getTime);

            // Since this is a lazy Future, the result is computed at the point of request

            success = wrapper.value == null && !result.isDone();

            Long out = result.get();

            success = success && wrapper.value == out;

            success = success && result.isDone();

        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        Assert.assertTrue("Failed to create Lazy Synchronzed Future", success);
    }


    @Test
    public void testSimpleLazySynchFutureWithCancel()
    {

        boolean success = false;
        try
        {
            EventManager emanager = getEventManager();
            ComputeManager cmanager = ComputeManager.create(10, emanager,_logger);


            final Thread callThread = Thread.currentThread();

            final ValueWrapper wrapper = new ValueWrapper();

            Callable<Long> getTime = new Callable<Long>() {
                @Override
                public Long call() throws Exception {

                    Long out = Long.valueOf(System.currentTimeMillis());
                    wrapper.value = out;


                    Thread runThread = Thread.currentThread();
                    Assert.assertTrue("Run thread of delayed synch future different from call thread", runThread.equals(callThread));
                    return out;
                }
            };

            NotifyFuture<Long> result = cmanager.getLazySyncResultFuture(getTime);

            // Since this is a lazy Future, the result is computed at the point of request

            success = wrapper.value == null && !result.isDone();

            result.cancel(false);

            success = success && result.isCancelled();

            Long out = result.get();

            // can't get result that is cancelled


            success = success && null == out;

            // can't be done if cancelled
            success = success && !result.isDone();

        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        Assert.assertTrue("Failed to create Lazy Synchronzed Future", success);
    }


    @Test
    public void testSimpleLazySynchFutureWithTimeout()
    {

        boolean success = false;
        try
        {
            EventManager emanager = getEventManager();
            ComputeManager cmanager = ComputeManager.create(10, emanager,_logger);

            long startTime = System.currentTimeMillis();

            long timeoutMilli = 3000;

            final Thread callThread = Thread.currentThread();

            final ValueWrapper wrapper = new ValueWrapper();

            Callable<Long> getTime = new Callable<Long>() {
                @Override
                public Long call() throws Exception {

                    Long out = Long.valueOf(System.currentTimeMillis());
                    wrapper.value = out;


                    Thread runThread = Thread.currentThread();
                    Assert.assertTrue("Run thread of delayed synch future different from call thread", runThread.equals(callThread));
                    return out;
                }
            };

            NotifyFuture<Long> result = cmanager.getLazySyncResultFuture(getTime);

            // Since this is a lazy Future, the result is computed at the point of request

            success = wrapper.value == null && !result.isDone();


            Long out = result.get(timeoutMilli, TimeUnit.MILLISECONDS);

            // since execution takes no time, actual delay should be near zero
            success = success && (System.currentTimeMillis() - startTime < 5);

            success = success && wrapper.value == out;

            success = success && result.isDone();

        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        Assert.assertTrue("Failed to create Lazy Synchronzed Future with timeout", success);
    }

    @Test
    public void testLongLazySynchFutureWithTimeoutAndBlocking()
    {

        boolean success = false;
        try
        {
            EventManager emanager = getEventManager();
            ComputeManager cmanager = ComputeManager.create(10, emanager,_logger);

            long startTime = System.currentTimeMillis();

            long timeoutMilli = 3000;

            final Thread callThread = Thread.currentThread();

            final ValueWrapper wrapper = new ValueWrapper();

            Callable<Long> getTime = new Callable<Long>() {
                @Override
                public Long call() throws Exception {

                    int sleepMilli = 2000;
                    Thread runThread = Thread.currentThread();
                    Assert.assertTrue("Run thread of delayed synch future different from call thread", runThread.equals(callThread));

                    Long out = Long.valueOf(System.currentTimeMillis());
                    wrapper.value = out;

                    Thread.sleep(sleepMilli);
                    return out;
                }
            };

            NotifyFuture<Long> result = cmanager.getLazySyncResultFuture(getTime);

            // Since this is a lazy Future, the result is computed at the point of request

            success = wrapper.value == null && !result.isDone();


            Long out = result.get(timeoutMilli, TimeUnit.MILLISECONDS);

            success = success && (System.currentTimeMillis() - startTime > 2000);

            success = success && wrapper.value == out;

            success = success && result.isDone();

        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        Assert.assertTrue("Failed to create Lazy Synchronzed Future with timeout", success);
    }


    @Test
    public void testSlowLazySynchFutureWithTimeoutAndBlocking()
    {

        boolean success = false;
        try
        {
            EventManager emanager = getEventManager();
            ComputeManager cmanager = ComputeManager.create(10, emanager,_logger);

            long startTime = System.currentTimeMillis();

            long timeoutMilli = 1000;

            final Thread callThread = Thread.currentThread();

            final ValueWrapper wrapper = new ValueWrapper();

            Callable<Long> getTime = new Callable<Long>() {
                @Override
                public Long call() throws Exception {

                    int sleepMilli = 2000;
                    Thread runThread = Thread.currentThread();
                    Assert.assertTrue("Run thread of delayed synch future different from call thread", runThread.equals(callThread));

                    Long out = Long.valueOf(System.currentTimeMillis());

                    Thread.sleep(sleepMilli);
                    wrapper.value = out;
                    return out;
                }
            };

            NotifyFuture<Long> result = cmanager.getLazySyncResultFuture(getTime);

            // Since this is a lazy Future, the result is computed at the point of request

            success = wrapper.value == null && !result.isDone();

            Long out = null;
            try {
                out = result.get(timeoutMilli, TimeUnit.MILLISECONDS);
                success = false;
            }
            catch (TimeoutException | InterruptedException ie)
            {
                success = success && (System.currentTimeMillis() - startTime < 2000);
            }


            success = success && !result.isDone() && wrapper.value == null && out == null;

        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        Assert.assertTrue("Failed to create Lazy Synchronzed Future with timeout", success);
    }


    @Test
    public void testLazySynchFutureWithTimeoutAndBlockingAndExternalInterrupt()
    {

        boolean success = false;
        try
        {
            EventManager emanager = getEventManager();
            ComputeManager cmanager = ComputeManager.create(10, emanager,_logger);

            long startTime = System.currentTimeMillis();

            long timeoutMilli = 3000;

            final Thread callThread = Thread.currentThread();

            final ValueWrapper wrapper = new ValueWrapper();

            Callable<Long> getTime = new Callable<Long>() {
                @Override
                public Long call() throws Exception {

                    int sleepMilli = 2000;
                    Thread runThread = Thread.currentThread();
                    Assert.assertTrue("Run thread of delayed synch future different from call thread", runThread.equals(callThread));

                    Long out = Long.valueOf(System.currentTimeMillis());

                    Thread.sleep(sleepMilli);
                    wrapper.value = out;
                    return out;
                }
            };

            final NotifyFuture<Long> result = cmanager.getLazySyncResultFuture(getTime);

            // Since this is a lazy Future, the result is computed at the point of request

            success = wrapper.value == null && !result.isDone();
            Assert.assertTrue("wrapper has value or result is done", success);

            Thread externalInterrupt = new Thread()
            {
                public void run()
                {
                    try
                    {
                        Thread.sleep(1000);
                        result.cancel(true);
                    }
                    catch (InterruptedException ie)
                    {

                    }
                }
            };
            Long out = null;
            try {
                externalInterrupt.start();
                out = result.get(timeoutMilli, TimeUnit.MILLISECONDS);
                success = false;
            }
            catch (TimeoutException | InterruptedException ie)
            {
                success = success && (System.currentTimeMillis() - startTime < 1500);
            }

            Assert.assertTrue("result not interrupted without 2000 milliseconds", success);
            success = success && result.isCancelled();

            Assert.assertTrue("result not cancelled", success);
            success = success && !result.isDone();
            Assert.assertTrue("result is done", success);
            success = success && wrapper.value == null;
            Assert.assertTrue("wrapper has value", success);

            success = success && out == null;
            Assert.assertTrue("out is defined", success);
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        Assert.assertTrue("Failed to create Lazy Synchronzed Future with timeout and external cancel", success);
    }


    @Test
    public void testSimpleLazySynchFutureWithTimeoutAndCancel()
    {

        boolean success = false;
        try
        {
            EventManager emanager = getEventManager();
            ComputeManager cmanager = ComputeManager.create(10, emanager,_logger);

            long startTime = System.currentTimeMillis();

            long timeoutMilli = 3000;

            final Thread callThread = Thread.currentThread();

            final ValueWrapper wrapper = new ValueWrapper();

            Callable<Long> getTime = new Callable<Long>() {
                @Override
                public Long call() throws Exception {

                    Long out = Long.valueOf(System.currentTimeMillis());
                    wrapper.value = out;


                    Thread runThread = Thread.currentThread();
                    Assert.assertTrue("Run thread of delayed synch future different from call thread", runThread.equals(callThread));
                    return out;
                }
            };

            NotifyFuture<Long> result = cmanager.getLazySyncResultFuture(getTime);

            // Since this is a lazy Future, the result is computed at the point of request

            success = wrapper.value == null && !result.isDone();

            result.cancel(false);

            Long out = result.get(timeoutMilli, TimeUnit.MILLISECONDS);

            // since execution takes no time, actual delay should be near zero
            success = success && (System.currentTimeMillis() - startTime < 5);

            success = success && out == null && result.isCancelled();

            success = success && !result.isDone();

        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        Assert.assertTrue("Failed to create Lazy Synchronzed Future with timeout and cancel", success);
    }



    // Test background processing


}
