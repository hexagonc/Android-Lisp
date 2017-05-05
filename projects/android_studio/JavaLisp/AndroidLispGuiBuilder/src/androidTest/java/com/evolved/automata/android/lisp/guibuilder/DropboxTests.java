package com.evolved.automata.android.lisp.guibuilder;

import android.app.Instrumentation;
import android.content.Context;
import android.content.res.Resources;
import android.os.Looper;
import android.support.test.InstrumentationRegistry;
import android.support.test.annotation.UiThreadTest;
import android.support.test.espresso.core.deps.guava.util.concurrent.AbstractScheduledService;
import android.support.test.rule.UiThreadTestRule;
import android.support.test.runner.AndroidJUnit4;

import com.dropbox.core.DbxRequestConfig;
import com.dropbox.core.v2.DbxClientV2;
import com.dropbox.core.v2.files.DbxUserFilesRequests;
import com.dropbox.core.v2.files.ListFolderResult;
import com.dropbox.core.v2.files.Metadata;

import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.concurrent.CountDownLatch;

import io.reactivex.Observable;
import io.reactivex.ObservableEmitter;
import io.reactivex.ObservableOnSubscribe;
import io.reactivex.Observer;
import io.reactivex.Scheduler;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.annotations.NonNull;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Function;
import io.reactivex.schedulers.Schedulers;

/**
 * Created by Evolved8 on 4/26/17.
 */

@RunWith(AndroidJUnit4.class)
public class DropboxTests {

    public static class ResultMonitorException extends RuntimeException
    {
        public String statusMessage;
        Exception internalError;
        public ResultMonitorException(String message, Exception e)
        {
            statusMessage = message;
            internalError = e;
        }

        public String getAssertMessage()
        {
            return statusMessage;
        }

        public Exception getInternalError()
        {
            return internalError;
        }


    }


    public static class AsyncResultMonitor
    {
        CountDownLatch latch;
        public boolean wasSuccessP;
        Exception exception = null;
        String errorMessage = "There was an exception";

        int stepCount = 0;

        Thread initialThread;
        public AsyncResultMonitor(int stepCount)
        {
            wasSuccessP = true;
            this.stepCount = stepCount;
            latch = new CountDownLatch(stepCount);
            initialThread = Thread.currentThread();
        }

        public void assertTrue(String message, boolean value)
        {
            wasSuccessP = value;
            errorMessage = message;
            if (!value)
            {
                finish();
            }

        }

        public void assertFailure(String message, Exception e)
        {
            wasSuccessP = false;
            errorMessage = message;
            exception = e;
            finish();
        }

        private void finish()
        {
            initialThread.interrupt();
        }

        public void setSuccess()
        {
            wasSuccessP = true;
            latch.countDown();
        }

        public void setError(String message, Exception e)
        {
            errorMessage = message;
            exception = e;
        }

        public void checkStatus()
        {
            if (exception != null)
                exception.printStackTrace();
            Assert.assertTrue(errorMessage, wasSuccessP);
        }

        public void waitForCompletion()
        {
            try
            {
                latch.await();
            }
            catch (InterruptedException ie)
            {

            }

        }
    }

    public static class ListEvent
    {
        public String[] _directoryList;

        public ListEvent(String[] folder)
        {
            _directoryList = folder;
        }

        public String[] getFolder()
        {
            return _directoryList;
        }
    }

    @Rule
    public UiThreadTestRule mUIThreadTestRule = new UiThreadTestRule();


    @UiThreadTest
    @Test
    public void testDropboxListFile()
    {
        String errorMessage = "Failed to create application";
        try
        {

            Instrumentation instrument = InstrumentationRegistry.getInstrumentation();

            Context con = instrument.getTargetContext();

            ALGBApplication app = (ALGBApplication)instrument.newApplication(ALGBApplication.class.getClassLoader(), "com.evolved.automata.android.lisp.guibuilder.ALGBApplication", con);

            errorMessage = "failed to call application on create";
            instrument.callApplicationOnCreate(app);


            Resources res = con.getResources();
            String accessToken = res.getString(R.string.v2_test_dropbox_token);

            errorMessage = "Failed to create DbxRequestConfig";
            DbxRequestConfig.Builder builder = DbxRequestConfig.newBuilder("AndroidLispGUIBuilder");
            DbxRequestConfig config = builder.build();
            errorMessage = "Failed to create DbxClientV2";

            final DbxClientV2 clientV2 = new DbxClientV2(config, accessToken);

            final AsyncResultMonitor monitor = new AsyncResultMonitor(1);
            final String listPath = "";
            if (accessToken.length() != 0)
            {
                Observable.create(new ObservableOnSubscribe<ListEvent>() {
                    @Override
                    public void subscribe(@NonNull ObservableEmitter<ListEvent> observer) throws Exception
                    {
                        String errorMessage = "Failed to get files";
                        try
                        {
                            DbxUserFilesRequests files = clientV2.files();

                            errorMessage = "Failed to get list folder result";

                            ListFolderResult folderResult = files.listFolder(listPath);

                            LinkedList<String> names = new LinkedList<String>();

                            for (Metadata meta:folderResult.getEntries())
                            {
                                names.add(meta.getName());
                            }

                            errorMessage = "Failed to get meta data";

                            monitor.assertTrue(errorMessage, names.size() > 0);
                            observer.onNext(new ListEvent(names.toArray(new String[0])));
                            observer.onComplete();
                        }
                        catch (Exception e)
                        {

                            observer.onError( new ResultMonitorException(errorMessage, e));
                        }
                    }
                }).subscribeOn(Schedulers.computation()).subscribe(new Observer<ListEvent>(){

                    @Override
                    public void onSubscribe(@NonNull Disposable d)
                    {

                    }

                    @Override
                    public void onNext(@NonNull ListEvent listEvent)
                    {

                        System.out.println("Response: " + Arrays.toString(listEvent.getFolder()));
                        monitor.setSuccess();
                    }

                    @Override
                    public void onError(@NonNull Throwable e)
                    {
                        ResultMonitorException ex = (ResultMonitorException)e;
                        monitor.assertFailure(ex.getMessage(), ex.getInternalError());
                    }

                    @Override
                    public void onComplete()
                    {

                    }
                });

                monitor.waitForCompletion();

                monitor.checkStatus();

            }

        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }



    /*
    @UiThreadTest
    @Test
    public void testDropboxListFileFull()
    {
        String errorMessage = "Failed to create application";
        try
        {

            Instrumentation instrument = InstrumentationRegistry.getInstrumentation();

            Context con = instrument.getTargetContext();

            ALGBApplication app = (ALGBApplication)instrument.newApplication(ALGBApplication.class.getClassLoader(), "com.evolved.automata.android.lisp.guibuilder.ALGBApplication", con);

            errorMessage = "failed to call application on create";
            instrument.callApplicationOnCreate(app);


            Resources res = con.getResources();
            String accessToken = res.getString(R.string.v2_test_dropbox_token);

            errorMessage = "Failed to create DbxRequestConfig";
            DbxRequestConfig.Builder builder = DbxRequestConfig.newBuilder("AndroidLispGUIBuilder");
            DbxRequestConfig config = builder.build();
            errorMessage = "Failed to create DbxClientV2";

            final DbxClientV2 clientV2 = new DbxClientV2(config, accessToken);

            final AsyncResultMonitor monitor = new AsyncResultMonitor(1);
            final String listPath = "";
            if (accessToken.length() != 0)
            {
                Observable.create(new ObservableOnSubscribe<ListEvent>() {
                    @Override
                    public void subscribe(@NonNull ObservableEmitter<ListEvent> observer) throws Exception
                    {
                        String errorMessage = "Failed to get files";
                        try
                        {
                            DbxUserFilesRequests files = clientV2.files();

                            errorMessage = "Failed to get list folder result";

                            ListFolderResult folderResult = files.listFolder(listPath);

                            LinkedList<String> names = new LinkedList<String>();

                            for (Metadata meta:folderResult.getEntries())
                            {
                                names.add(meta.getName());
                            }

                            errorMessage = "Failed to get meta data";

                            monitor.assertTrue(errorMessage, names.size() > 0);
                            observer.onNext(new ListEvent(names.toArray(new String[0])));
                            observer.onComplete();
                        }
                        catch (Exception e)
                        {

                            observer.onError( new ResultMonitorException(errorMessage, e));
                        }
                    }
                }).subscribeOn(Schedulers.computation()).flatMap(new Function<>)



                        subscribe(new Observer<ListEvent>(){

                    @Override
                    public void onSubscribe(@NonNull Disposable d)
                    {

                    }

                    @Override
                    public void onNext(@NonNull ListEvent listEvent)
                    {

                        System.out.println("Response: " + Arrays.toString(listEvent.getFolder()));
                        monitor.setSuccess();
                    }

                    @Override
                    public void onError(@NonNull Throwable e)
                    {
                        ResultMonitorException ex = (ResultMonitorException)e;
                        monitor.assertFailure(ex.getMessage(), ex.getInternalError());
                    }

                    @Override
                    public void onComplete()
                    {

                    }
                });

                monitor.waitForCompletion();

                monitor.checkStatus();

            }

        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }
    */
}
