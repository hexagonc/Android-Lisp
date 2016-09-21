package com.evolved.automata.android.guibuilder.core.processing.futures;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * Created by Evolved8 on 9/12/16.
 */
public interface NotifyFuture <V> extends Future<V> {



    public void setFutureListener(FutureResultListener<V> listener);
    public Runnable getProcessRunnable();

}
