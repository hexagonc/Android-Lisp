package com.evolved.automata.android.lisp.guibuilder.v2.workflow;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * Created by Evolved8 on 4/26/17.
 */

public class WorkManager extends ScheduledThreadPoolExecutor {


    public WorkManager(int corePoolSize)
    {
        super(corePoolSize);
    }

    public WorkManager(int corePoolSize, ThreadFactory threadFactory)
    {
        super(corePoolSize, threadFactory);
    }

    public WorkManager(int corePoolSize, RejectedExecutionHandler handler)
    {
        super(corePoolSize, handler);
    }

    public WorkManager(int corePoolSize, ThreadFactory threadFactory, RejectedExecutionHandler handler)
    {
        super(corePoolSize, threadFactory, handler);
    }
}
