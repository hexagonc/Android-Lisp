package com.evolved.automata.lisp;

import java.util.AbstractQueue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class RuntimeFunctions {

    private static final Environment BREAK = new Environment();
    private static final Environment BINDING_DATA = new Environment();


    public interface LispInterface {
        int getContinuationCount();
        void evaluate(Environment env, Value exp);
        void evaluate(Value exp);
        void onError(Throwable t, String message);
        boolean breakAll();
    }

    public static ThreadPoolExecutor sThreadPoolExecutor = null;

    private static void configureExector(int maxNumThreads){
        sThreadPoolExecutor = new ThreadPoolExecutor(2, maxNumThreads, 3000, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>());
    }

    public static void addRuntimeFunctions(Environment env, LispInterface parentInterface){
        configureExector(5);
        env.setRuntimeInterface(parentInterface);

        env.mapFunction("concurrency-make-queue", new SimpleFunctionTemplate()
                {

                    @Override
                    public Value evaluate(Environment env,Value[] evaluatedArgs) {


                        return ExtendedFunctions.makeValue(new ConcurrentLinkedQueue<Value>());

                    }

                }
        );

        env.mapFunction("concurrency-dequeue", new SimpleFunctionTemplate()
                {

                    @Override
                    public Value evaluate(Environment env,Value[] evaluatedArgs) {
                        checkActualArguments(1, false, true);
                        ConcurrentLinkedQueue<Value> queue = (ConcurrentLinkedQueue)evaluatedArgs[0].getObjectValue();
                        if (queue.size()>0)
                            return queue.remove();
                        else
                            return Environment.getNull();

                    }

                }
        );

        env.mapFunction("concurrency-queue-size", new SimpleFunctionTemplate()
                {

                    @Override
                    public Value evaluate(Environment env,Value[] evaluatedArgs) {
                        checkActualArguments(1, false, true);
                        AbstractQueue<Value> queue = (AbstractQueue)evaluatedArgs[0].getObjectValue();
                        return NLispTools.makeValue(queue.size());
                    }

                }
        );

        env.mapFunction("concurrency-queue-is-empty", new SimpleFunctionTemplate()
                {

                    @Override
                    public Value evaluate(Environment env,Value[] evaluatedArgs) {
                        checkActualArguments(1, false, true);
                        AbstractQueue<Value> queue = (AbstractQueue)evaluatedArgs[0].getObjectValue();
                        return NLispTools.makeValue(queue.size() == 0);
                    }

                }
        );

        env.mapFunction("concurrency-clear-queue", new SimpleFunctionTemplate()
                {
                    @Override
                    public Value evaluate(Environment env,Value[] evaluatedArgs) {
                        checkActualArguments(1, false, true);
                        AbstractQueue<Value> queue = (AbstractQueue)evaluatedArgs[0].getObjectValue();
                        queue.clear();
                        return evaluatedArgs[0];
                    }

                }
        );

        env.mapFunction("concurrency-enqueue", new SimpleFunctionTemplate()
                {
                    @Override
                    public Value evaluate(Environment env,Value[] evaluatedArgs) {
                        checkActualArguments(2, false, true);
                        ConcurrentLinkedQueue<Value> queue = (ConcurrentLinkedQueue)evaluatedArgs[0].getObjectValue();
                        queue.add(evaluatedArgs[1]);
                        return evaluatedArgs[1];

                    }

                }
        );

        // **********
        env.mapFunction("concurrency-linked-make-queue", new SimpleFunctionTemplate()
                {

                    @Override
                    public Value evaluate(Environment env,Value[] evaluatedArgs) {


                        return ExtendedFunctions.makeValue(new LinkedBlockingQueue());

                    }

                }
        );

        env.mapFunction("concurrency-linked-dequeue", new SimpleFunctionTemplate()
                {

                    @Override
                    public Value evaluate(Environment env,Value[] evaluatedArgs) {
                        checkActualArguments(1, false, true);
                        LinkedBlockingQueue<Value> queue = (LinkedBlockingQueue)evaluatedArgs[0].getObjectValue();
                        try {
                            return queue.take();
                        } catch (InterruptedException e) {
                            Thread.currentThread().interrupt();
                            throw new RuntimeException(e);
                        }
                    }

                }
        );

        env.mapFunction("concurrency-linked-enqueue", new SimpleFunctionTemplate()
                {
                    @Override
                    public Value evaluate(Environment env,Value[] evaluatedArgs) {
                        checkActualArguments(2, false, true);
                        LinkedBlockingQueue<Value> queue = (LinkedBlockingQueue)evaluatedArgs[0].getObjectValue();
                        try {
                            queue.put(evaluatedArgs[1]);
                        } catch (InterruptedException e) {
                            Thread.currentThread().interrupt();
                            throw new RuntimeException(e);
                        }
                        return evaluatedArgs[1];

                    }

                }
        );

        env.mapFunction("get-runtime-interface", new SimpleFunctionTemplate()
                {

                    @Override
                    public Value evaluate(Environment env,Value[] evaluatedArgs) {

                        return ExtendedFunctions.makeValue(env.getRuntimeInterface());

                    }

                }
        );

        env.mapFunction("bind-into-runtime", new SimpleFunctionTemplate()
                {

                    @Override
                    public Value evaluate(Environment env,Value[] evaluatedArgs) {

                        checkActualArguments(2, false, true);
                        LispInterface interfac = (LispInterface)evaluatedArgs[0].getObjectValue();
                        Value bindingSpecList = evaluatedArgs[1];
                        interfac.evaluate(BINDING_DATA, bindingSpecList);
                        return evaluatedArgs[0];

                    }

                }
        );

        env.mapFunction("break-runtime", new SimpleFunctionTemplate()
                {

                    @Override
                    public Value evaluate(Environment env,Value[] evaluatedArgs) {
                        checkActualArguments(1, false, true);
                        LispInterface interfac = (LispInterface)evaluatedArgs[0].getObjectValue();
                        interfac.breakAll();
                        return evaluatedArgs[0];

                    }

                }
        );

        env.mapFunction("set-runtime-poolsize", new SimpleFunctionTemplate()
                {

                    @Override
                    public Value evaluate(Environment env,Value[] evaluatedArgs) {
                        checkActualArguments(1, true, true);

                        int threadPoolSize = (int)evaluatedArgs[0].getIntValue();

                        configureExector(threadPoolSize);
                        return evaluatedArgs[0];

                    }

                }
        );


        env.mapFunction("set-thread-priority", new SimpleFunctionTemplate()
                {

                    @Override
                    public Value evaluate(Environment env,Value[] evaluatedArgs) {
                        checkActualArguments(1, true, true);

                        int priority = (int)evaluatedArgs[0].getIntValue();

                        Thread.currentThread().setPriority(priority);
                        return evaluatedArgs[0];

                    }

                }
        );

        env.mapFunction("get-thread-priority", new SimpleFunctionTemplate()
                {

                    @Override
                    public Value evaluate(Environment env,Value[] evaluatedArgs) {

                        return NLispTools.makeValue(Thread.currentThread().getPriority());

                    }

                }
        );


        env.mapFunction("get-process-count", new SimpleFunctionTemplate()
                {

                    @Override
                    public Value evaluate(Environment env,Value[] evaluatedArgs) {

                        return NLispTools.makeValue(sThreadPoolExecutor.getActiveCount());

                    }

                }
        );

        env.mapFunction("thread", getThreadFunction());

        env.mapFunction("queue-thread", getThreadQueueFunction());
        env.mapFunction("queue-runtime-expr", getRunExpression());


    }




    public static FunctionTemplate getThreadFunction( ){
        return new FunctionTemplate() {

            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) getThreadFunction();
            }

            @Override
            public Value evaluate(Environment env, boolean resume)
                    throws InstantiationException, IllegalAccessException {
                // arguments are:
                // binding list
                // binding values
                // background expr
                // return expr
                checkActualArguments(4, false, false);

                LispInterface parentInterface = env.getRuntimeInterface();

                if (!resume)
                    resetFunctionTemplate();

                String [] bindingVariables = NLispTools.getStringArrayFromValue(_actualParameters[0]);

                Value bindingValues;

                if (resume && _lastFunctionReturn.getContinuingFunction()!=null)
                    bindingValues = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
                else
                    bindingValues = _lastFunctionReturn = env.evaluate(_actualParameters[1]);

                if (bindingValues.isContinuation())
                    return continuationReturn(bindingValues);
                if (bindingValues.isReturn() || bindingValues.isSignal() || bindingValues.isSignalOut() || bindingValues.isBreak())
                    return resetReturn(bindingValues);


                Value[] bindingArray = new Value[0];
                if (!bindingValues.isNull()){
                    bindingArray = bindingValues.getList();
                }

                Environment backgroundEnv = new Environment(env.getCleanEnvironment(false));

                for (int i = 0;i<bindingVariables.length;i++){
                    if (i < bindingArray.length){
                        backgroundEnv.mapValue(bindingVariables[i], bindingArray[i]);
                    }
                }

                ThreadPoolExecutor executor = sThreadPoolExecutor;

                executor.submit(()->{
                    try
                    {
                        Value processArg = _actualParameters[2];
                        Value result = backgroundEnv.evaluate(processArg, false);
                        while (result.isContinuation()){
                            result = result.getContinuingFunction().evaluate(backgroundEnv, true);
                        }

                        Environment finalEnv = backgroundEnv.bindVariablesInto(new Environment(env));
                        parentInterface.evaluate(finalEnv, _actualParameters[3]);
                    }
                    catch (Throwable t){
                        parentInterface.onError(t, "Background thread process error: " + t.getMessage());
                    }
                });

                return resetReturn(NLispTools.makeValue(parentInterface.getContinuationCount()));

            }
        };
    }


    public static FunctionTemplate getRunExpression( ){
        return new FunctionTemplate() {

            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) getRunExpression();
            }

            @Override
            public Value evaluate(Environment env, boolean resume)
                    throws InstantiationException, IllegalAccessException {
                // arguments are:
                // binding list
                // binding values
                // background expr
                // return expr
                checkActualArguments(2, false, false);

                if (!resume)
                    resetFunctionTemplate();

                Value runtimeValue;

                if (resume && _lastFunctionReturn.getContinuingFunction()!=null)
                    runtimeValue = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
                else
                    runtimeValue = _lastFunctionReturn = env.evaluate(_actualParameters[0]);

                if (runtimeValue.isContinuation())
                    return continuationReturn(runtimeValue);
                if (runtimeValue.isReturn() || runtimeValue.isSignal() || runtimeValue.isSignalOut() || runtimeValue.isBreak())
                    return resetReturn(runtimeValue);


                RuntimeFunctions.LispInterface linterface = (RuntimeFunctions.LispInterface)runtimeValue.getObjectValue();

                Value expr = _actualParameters[1];

                linterface.evaluate(expr);

                return resetReturn(runtimeValue);

            }
        };
    }


    public static FunctionTemplate getThreadQueueFunction( ){
        return new FunctionTemplate() {

            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) getThreadQueueFunction();
            }

            @Override
            public Value evaluate(Environment env, boolean resume)
                    throws InstantiationException, IllegalAccessException {
                // arguments are:
                // binding list
                // binding values
                // background expr
                // return expr
                checkActualArguments(4, false, false);

                LispInterface parentInterface = env.getRuntimeInterface();

                if (!resume)
                    resetFunctionTemplate();

                String [] bindingVariables = NLispTools.getStringArrayFromValue(_actualParameters[0]);

                Value bindingValues;

                if (resume && _lastFunctionReturn.getContinuingFunction()!=null)
                    bindingValues = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
                else
                    bindingValues = _lastFunctionReturn = env.evaluate(_actualParameters[1]);

                if (bindingValues.isContinuation())
                    return continuationReturn(bindingValues);
                if (bindingValues.isReturn() || bindingValues.isSignal() || bindingValues.isSignalOut() || bindingValues.isBreak())
                    return resetReturn(bindingValues);


                Value[] bindingArray = new Value[0];
                if (!bindingValues.isNull()){
                    bindingArray = bindingValues.getList();
                }

                //Environment backgroundEnv = new Environment(env.getCleanEnvironment(false));
                Environment backgroundEnv = new Environment(env);

                for (int i = 0;i<bindingVariables.length;i++){
                    if (i < bindingArray.length){
                        backgroundEnv.mapValue(bindingVariables[i], bindingArray[i]);
                    }
                }

                ThreadPoolExecutor executor = sThreadPoolExecutor;

                LinkedBlockingQueue<EvalRequest> requestQueue = new LinkedBlockingQueue<>();

                RuntimeFunctions.LispInterface innerInterface = new RuntimeFunctions.LispInterface(){

                    @Override
                    public int getContinuationCount() {
                        return requestQueue.size();
                    }

                    @Override
                    public void evaluate(Environment env, Value exp) {
                        try {
                            requestQueue.put(EvalRequest.make(exp, env));
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                    }

                    @Override
                    public void evaluate(Value exp) {
                        try {
                            requestQueue.put(EvalRequest.make(exp, backgroundEnv));
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                    }

                    @Override
                    public void onError(Throwable t, String message) {
                        parentInterface.onError(t, message);
                    }

                    @Override
                    public boolean breakAll() {
                        try {
                            requestQueue.put(EvalRequest.make(null, BREAK));
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                        return true;
                    }
                };

                backgroundEnv.setRuntimeInterface(innerInterface);

                try {
                    requestQueue.put(EvalRequest.make(_actualParameters[2], backgroundEnv));
                } catch (InterruptedException e) {
                    e.printStackTrace();
                    throw new RuntimeException(e);
                }

                executor.submit(()->{
                    try
                    {
                        EvalRequest request = null;

                        Value processArg = null;
                        Environment processEnv = null;

                        Value result = null;

                        while (requestQueue.size()>0){
                            request = requestQueue.take();
                            processArg = request._item;
                            processEnv = request._env;

                            if (BREAK == processEnv){
                                for (EvalRequest existing:requestQueue){
                                    existing._env.getRuntimeInterface().breakAll();
                                }
                                requestQueue.clear();
                                break;
                            }
                            else if (BINDING_DATA == processEnv){
                                Value keys = processArg.getList()[0];
                                Value values = processArg.getList()[1];

                                for (int i = 0;i < keys.getList().length;i++){
                                    backgroundEnv.mapValue(keys.getList()[i].getString(), values.getList()[i]);
                                }
                                continue;
                            }
                            FunctionTemplate lastFunction = processArg.getContinuingFunction();
                            if (lastFunction!= null && processArg.isContinuation()){
                                result = lastFunction.evaluate(processEnv, true);

                            }
                            else {
                                result = processEnv.evaluate(processArg, false);
                            }

                            if (result.isContinuation())
                                requestQueue.put(EvalRequest.make(result, processEnv));

                        }

                        Environment finalEnv = backgroundEnv.bindVariablesInto(new Environment(env));
                        parentInterface.evaluate(finalEnv, _actualParameters[3]);
                    }
                    catch (Throwable t){
                        parentInterface.onError(t, "Background thread process error: " + t.getMessage());
                    }
                });

                return resetReturn(ExtendedFunctions.makeValue(innerInterface));

            }
        };
    }

    private static class EvalRequest {
        public Value _item;
        public Environment _env;

        private EvalRequest(){

        }

        public static EvalRequest make(Value value, Environment env){
            EvalRequest r = new EvalRequest();
            r._item = value;
            r._env = env;
            return r;
        }

    }

}
