package com.evolved.automata.android.lisp.guibuilder;

import android.app.Instrumentation;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.os.Handler;
import android.os.Looper;
import android.support.test.InstrumentationRegistry;
import android.support.test.annotation.UiThreadTest;
import android.support.test.espresso.core.deps.dagger.Component;
import android.support.test.rule.ActivityTestRule;
import android.support.test.rule.UiThreadTestRule;
import android.support.test.runner.AndroidJUnit4;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.Value;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * Created by Evolved8 on 4/20/17.
 */

@RunWith(AndroidJUnit4.class)
public class ALGBApplicationTest {


    @Rule
    public UiThreadTestRule mUIThreadTestRule = new UiThreadTestRule();



    /*
    public ActivityTestRule<MainActivity> mActivityRule = new ActivityTestRule<MainActivity>(MainActivity.class)
    {
        @Override
        protected void afterActivityFinished()
        {
            super.afterActivityFinished();
            System.out.println("Finished<><<><><<><><<<");
        }
    };
    */


    @UiThreadTest
    @Test
    public void testApplicationStart()
    {
        Instrumentation instrumentation = InstrumentationRegistry.getInstrumentation();
        Context context = instrumentation.getTargetContext();
        String errorMessage = "Failed to Create instrumentation Application";
        final CountDownLatch latch = new CountDownLatch(1);
        try
        {
            ALGBApplication application = (ALGBApplication)instrumentation.newApplication(ALGBApplication.class.getClassLoader(), "com.evolved.automata.android.lisp.guibuilder.ALGBApplication", context);
            errorMessage = "Failed to successfully call application onCreate";

            instrumentation.callApplicationOnCreate(application);
            GlobalInterface global = application.getGlobalData();
            errorMessage = "Failed to get Environment";
            Environment topEnvironment = global.getEnvironment();

            Value result = topEnvironment.evaluate("(+ 12 12)",false);
            Assert.assertTrue("Failed to evaluate result: ", result!=null && result.toString() != null);
            System.out.println(result.toString());
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }


    }

    String errorMessage = null;
    @Test
    public void testActivityStart()
    {
        errorMessage = "Failed to start activity";
        try
        {
            //ALGBBaseActivity base = mActivityRule.getActivity();
            final Instrumentation instrument = InstrumentationRegistry.getInstrumentation();
            Context context = instrument.getTargetContext();


            ALGBApplication application = (ALGBApplication)instrument.newApplication(ALGBApplication.class.getClassLoader(), "com.evolved.automata.android.lisp.guibuilder.ALGBApplication", context);
            //instrument.callApplicationOnCreate(application);


            Intent intent = new Intent();
            intent.setAction(Intent.ACTION_MAIN);
            intent.setClass(context, ALGBBaseActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);



            final ALGBBaseActivity base = (ALGBBaseActivity)instrument.startActivitySync(intent);




            mUIThreadTestRule.runOnUiThread(new Runnable()
            {
                public void run()
                {

                    try
                    {
                        errorMessage = "Failed to call activity onCreate";

                        instrument.callActivityOnCreate(base, null);
                    }
                    catch (Exception e)
                    {
                        Assert.assertTrue(errorMessage, false);
                    }
                }
            });

            mUIThreadTestRule.runOnUiThread(new Runnable()
            {
                public void run()
                {

                    try
                    {
                        errorMessage = "Failed to call activity onStart";
                        instrument.callActivityOnStart(base);
                    }
                    catch (Exception e)
                    {
                        Assert.assertTrue(errorMessage, false);
                    }
                }
            });

            mUIThreadTestRule.runOnUiThread(new Runnable()
            {
                public void run()
                {


                    try
                    {
                        errorMessage = "Failed to call activity onResume";
                        instrument.callActivityOnResume(base);

                    }
                    catch (Exception e)
                    {
                        Assert.assertTrue(errorMessage, false);
                    }
                }
            });



            Thread.sleep(10000);
            mUIThreadTestRule.runOnUiThread(new Runnable()
            {
                public void run()
                {


                    try
                    {
                        errorMessage = "Failed to call activity onPause";
                        instrument.callActivityOnPause(base);
                    }
                    catch (Exception e)
                    {
                        Assert.assertTrue(errorMessage, false);
                    }
                }
            });

            mUIThreadTestRule.runOnUiThread(new Runnable()
            {
                public void run()
                {


                    try
                    {
                        errorMessage = "Failed to call activity onStop";
                        instrument.callActivityOnStop(base);
                    }
                    catch (Exception e)
                    {
                        Assert.assertTrue(errorMessage, false);
                    }
                }
            });

            mUIThreadTestRule.runOnUiThread(new Runnable()
            {
                public void run()
                {


                    try
                    {
                        errorMessage = "Failed to call activity onDestroy";
                        instrument.callActivityOnDestroy(base);
                    }
                    catch (Exception e)
                    {
                        Assert.assertTrue(errorMessage, false);
                    }
                }
            });



        }
        catch (Throwable e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @UiThreadTest
    @Test
    public void testApplicationAndActivity()
    {
        String errorMessage = "Failed to start activity";
        final CountDownLatch latch = new CountDownLatch(5);
        try
        {
            final Instrumentation instrument = InstrumentationRegistry.getInstrumentation();
            Context context = instrument.getTargetContext();


            ALGBApplication application = (ALGBApplication)instrument.newApplication(ALGBApplication.class.getClassLoader(), "com.evolved.automata.android.lisp.guibuilder.ALGBApplication", context);
            instrument.callApplicationOnCreate(application);


            Intent intent = new Intent();
            intent.setAction(Intent.ACTION_MAIN);
            intent.setClass(context, ALGBBaseActivity.class);
            ComponentName cname = new ComponentName("com.evolved.automata.android.lisp.guibuilder", "com.evolved.automata.android.lisp.guibuilder.ALGBBaseActivity");
            intent.setComponent(cname);
            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);


            PackageManager pm = context.getPackageManager();
            ActivityInfo ai = pm.getActivityInfo( cname, 0);


            final ALGBBaseActivity base = (ALGBBaseActivity)instrument.newActivity(ALGBBaseActivity.class,
                    context,
                    null, application, intent, ai, "Test", null, null, null);



            Handler handler = new Handler(Looper.getMainLooper());
            handler.post(new Runnable()
            {
               public void run()
               {
                   try
                   {
                       instrument.callActivityOnCreate(base, null);
                       latch.countDown();
                   }
                   catch (Exception e)
                   {
                       Assert.assertTrue("Failed to call activity onCreate", false);
                   }
               }
            });

            handler.post(new Runnable()
            {
                public void run()
                {
                    try
                    {
                        instrument.callActivityOnStart(base);
                        latch.countDown();
                    }
                    catch (Exception e)
                    {
                        Assert.assertTrue("Failed to call activity onStart", false);
                    }
                }
            });


            handler.post(new Runnable()
            {
                public void run()
                {
                    try
                    {
                        instrument.callActivityOnResume(base);
                        latch.countDown();
                    }
                    catch (Exception e)
                    {
                        e.printStackTrace();
                        Assert.assertTrue("Failed to call activity onResume", false);
                    }
                }
            });


            handler.postDelayed(new Runnable()
            {
                public void run()
                {
                    try
                    {
                        instrument.callActivityOnPause(base);
                        latch.countDown();
                    }
                    catch (Exception e)
                    {
                        e.printStackTrace();
                        Assert.assertTrue("Failed to call activity onPause", false);
                    }
                }
            },
            10000);

            handler.postDelayed(new Runnable()
            {
                public void run()
                {
                    try
                    {

                        instrument.callActivityOnStop(base);
                        latch.countDown();
                    }
                    catch (Exception e)
                    {
                        Assert.assertTrue("Failed to call activity onStop", false);
                    }
                }
            }, 10000);


            handler.postDelayed(new Runnable()
            {
                public void run()
                {
                    try
                    {

                        instrument.callActivityOnDestroy(base);
                        latch.countDown();
                    }
                    catch (Exception e)
                    {
                        Assert.assertTrue("Failed to call activity onDestroy", false);
                    }
                }
            }, 10000);

            latch.await(20, TimeUnit.SECONDS);



        }
        catch (Throwable e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

}
