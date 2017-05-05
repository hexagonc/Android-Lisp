package com.evolved.automata.android.lisp.guibuilder;

import android.app.Instrumentation;
import android.content.Context;
import android.support.test.InstrumentationRegistry;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import java.util.HashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import io.reactivex.Observer;
import io.reactivex.annotations.NonNull;
import io.reactivex.disposables.Disposable;

/**
 * Created by Evolved8 on 5/4/17.
 */

public class InternalTests {

    String mSingleData = "3.141592";
    String mSingleDatakey = "pi";
    String mSingleDataContext = "default";

    @Test
    public void testDataKeyDeletion()
    {
        String errorMessage = "failed to create Data Access Interface";
        try
        {
            Instrumentation instrumentation = InstrumentationRegistry.getInstrumentation();
            Context context = instrumentation.getTargetContext();

            AndroidLispDAI data = new AndroidLispDAI(context);
            data.deleteData(mSingleDatakey, mSingleDataContext);
            boolean dataPresentP = data.hasData(mSingleDatakey, mSingleDataContext);
            errorMessage = "failed to verify initial delete";
            Assert.assertTrue(errorMessage, !dataPresentP);


        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testDataAccessInterface()
    {
        String errorMessage = "failed to verify initial delete";
        try
        {
            Instrumentation instrumentation = InstrumentationRegistry.getInstrumentation();
            Context context = instrumentation.getTargetContext();

            AndroidLispDAI data1 = new AndroidLispDAI(context);

            data1.deleteData(mSingleDatakey, mSingleDataContext);
            boolean dataPresentP = data1.hasData(mSingleDatakey, mSingleDataContext);

            Assert.assertTrue(errorMessage, !dataPresentP);

            String value = data1.getData(mSingleDatakey, mSingleDataContext);
            Assert.assertTrue(errorMessage, value == null);
            errorMessage = "failed to store data";
            int updateCount;
            updateCount = data1.setData(mSingleDatakey, mSingleDataContext, mSingleData);

            Assert.assertTrue(errorMessage, updateCount > 0);

            AndroidLispDAI data2 = new AndroidLispDAI(context);

            errorMessage = "failed to verify data present";
            dataPresentP = data2.hasData(mSingleDatakey, mSingleDataContext);
            Assert.assertTrue(errorMessage, dataPresentP);


            errorMessage = "failed to verify data value";
            value = data2.getData(mSingleDatakey, mSingleDataContext);

            Assert.assertTrue(errorMessage, mSingleData.equals(value));

        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testMultiItemStorage()
    {
        String errorMessage = "failed to verify initial delete";
        try
        {
            Instrumentation instrumentation = InstrumentationRegistry.getInstrumentation();
            Context context = instrumentation.getTargetContext();

            AndroidLispDAI data = new AndroidLispDAI(context);

            String[] keys = new String[]{"x", "y", "z"};
            String[] values = new String[]{"(x)", "(y)", "(z)"};
            HashMap<String, String> map = new HashMap<String, String>();
            String ctx = "multi-test";

            int i;
            for (i = 0; i < keys.length;i++)
            {
                data.deleteData(keys[i], ctx);
                map.put(keys[i], values[i]);
            }

            for (i = 0; i < keys.length;i++)
            {
                data.setData(keys[i], ctx, values[i]);
            }
            errorMessage = "Failed to retrieve context, [" + ctx + "] items";
            String[] results = data.getAllKeys(ctx);

            errorMessage = "Failed to retrieve keys.  Expected: " + keys.length + ": found " + results.length;
            Assert.assertTrue(errorMessage, results.length == keys.length);


            for (String k:results)
            {
                String v = data.getData(k, ctx);
                errorMessage = String.format("Failed to match values.  Expected [%1$s -> %2$s], found: [%3$s -> %4$s]", k, map.get(k), k, v);
                Assert.assertTrue(errorMessage, map.get(k).equals(v));
            }


        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testLispDataStorage()
    {
        String errorMessage = "Failed to create LispContext";
        try
        {
            Instrumentation instrumentation = InstrumentationRegistry.getInstrumentation();
            Context context = instrumentation.getTargetContext();

            AndroidLispDAI dai = new AndroidLispDAI(context);
            Environment env = new Environment(), internal;
            NLispTools.addDefaultFunctionsAddMacros(env);
            ExtendedFunctions.addExtendedFunctions(env);
            LispContext lispContext = new LispContext(context, env, dai);

            internal = lispContext.getEnvironment();

            String key = "x";
            String ctx = "<>";
            Value value = NLispTools.makeValue(12.56);
            internal.simpleEvaluateFunction("delete-data-value", key, ctx);

            Value hasData = internal.simpleEvaluateFunction("check-data-exists", key, ctx);

            errorMessage = "Failed to verify data non-existence";
            Assert.assertTrue(errorMessage, hasData.isNull());

            Value setResult = internal.simpleEvaluateFunction("set-data-value", key, value, ctx);

            errorMessage = "Failed to verify data existence";
            Assert.assertTrue(errorMessage, !setResult.isNull());

            Value newData = internal.simpleEvaluateFunction("get-data-value", key, ctx);

            Value retrievedValue = internal.simpleEvaluateFunction("=", newData, value);
            errorMessage = "Failed to match retrieved data with stored data";
            Assert.assertTrue(errorMessage, !retrievedValue.isNull());
        }
        catch (Exception e)
        {
            e.printStackTrace();;
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testLispBackgroundExecution()
    {
        String errorMessage = "";
        try
        {
            Instrumentation instrumentation = InstrumentationRegistry.getInstrumentation();
            Context context = instrumentation.getTargetContext();

            AndroidLispDAI dai = new AndroidLispDAI(context);
            Environment env = new Environment(), internal;
            NLispTools.addDefaultFunctionsAddMacros(env);
            ExtendedFunctions.addExtendedFunctions(env);
            LispContext lispContext = new LispContext(context, env, dai);

            final CountDownLatch latch = new CountDownLatch(1);

            lispContext.evaluateExpression("(+ 12 34)", new Observer<Value>() {
                @Override
                public void onSubscribe(@NonNull Disposable d)
                {

                }

                @Override
                public void onNext(@NonNull Value value)
                {
                    System.out.println("Result: " + value.toString());
                    Assert.assertTrue("Failed to execute", true);

                }

                @Override
                public void onError(@NonNull Throwable e)
                {
                    Assert.assertTrue("Failed to execute", false);
                    latch.countDown();
                }

                @Override
                public void onComplete()
                {
                    latch.countDown();
                }
            });
            latch.await(3, TimeUnit.SECONDS);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Ignore("later")
    @Test
    public void testMainThreadLispContext()
    {

    }


}
