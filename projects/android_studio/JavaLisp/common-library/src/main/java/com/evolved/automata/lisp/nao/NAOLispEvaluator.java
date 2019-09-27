package com.evolved.automata.lisp.nao;

import com.aldebaran.qi.Session;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.Lambda;
import com.evolved.automata.lisp.LambdaValue;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;


/**
 * Created by Evolved8 on 6/20/17.
 */

public class NAOLispEvaluator {
    public static void addFunctions(Environment env)
    {
        env.mapFunction("create-nao-manager", create_nao_manager());
        env.mapFunction("connect-to-nao", connect_to_nao());
        env.mapFunction("disconnect-from-nao", disconnect_from_nao());
        env.mapFunction("call-qi-function", call_qi_function());
    }

    public static SimpleFunctionTemplate create_nao_manager()
    {
        return new SimpleFunctionTemplate()
        {

            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)create_nao_manager();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(0, false, false);
                NAOManager manager = new NAOManager();

                return ExtendedFunctions.makeValue(manager);



            }
        };
    }

    public static SimpleFunctionTemplate disconnect_from_nao()
    {
        return new SimpleFunctionTemplate()
        {

            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)disconnect_from_nao();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, false, false);
                NAOManager manager = (NAOManager)evaluatedArgs[0].getObjectValue();

                manager.disconnectFromCurrentSession();
                return ExtendedFunctions.makeValue(manager);



            }
        };
    }


    public static SimpleFunctionTemplate connect_to_nao()
    {
        return new SimpleFunctionTemplate()
        {

            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)connect_to_nao();
            }

            @Override
            public Value evaluate(final Environment env, Value[] evaluatedArgs) {
                checkActualArguments(2, true, true);
                NAOManager manager = (NAOManager)evaluatedArgs[0].getObjectValue();
                String ipEquivalent = evaluatedArgs[1].getString();

                int timeoutMilli = 0;
                TimeUnit unit = null;
                if (evaluatedArgs.length > 2)
                {
                    timeoutMilli  = (int)evaluatedArgs[2].getIntValue();
                    unit = TimeUnit.MILLISECONDS;
                }
                try
                {

                    Session.ConnectionListener listener = null;

                    if (evaluatedArgs.length > 3 && evaluatedArgs[3].isLambda())
                    {
                        final Lambda llistener = (Lambda)evaluatedArgs[3].getLambda();
                        listener = new Session.ConnectionListener() {
                            @Override
                            public void onConnected()
                            {
                                llistener.setActualParameters(new Value[]{NLispTools.makeValue(true)});
                                try
                                {
                                    llistener.evaluate(env, false);
                                } catch (InstantiationException e)
                                {
                                    e.printStackTrace();
                                } catch (IllegalAccessException e)
                                {
                                    e.printStackTrace();
                                }
                            }

                            @Override
                            public void onDisconnected(String reason)
                            {
                                llistener.setActualParameters(new Value[]{NLispTools.makeValue(true)});
                                try
                                {
                                    llistener.evaluate(env, false);
                                } catch (InstantiationException e)
                                {
                                    e.printStackTrace();
                                } catch (IllegalAccessException e)
                                {
                                    e.printStackTrace();
                                }
                            }
                        };
                    }
                    if (unit == null)
                        manager.connectToNAO(ipEquivalent);
                    else
                        manager.connectToNAO(ipEquivalent, timeoutMilli, unit);
                    if (listener!=null)
                        manager.addNAOConnectionListener(listener);
                }
                catch (TimeoutException e)
                {
                    throw new RuntimeException(e);
                } catch (ExecutionException e)
                {
                    return Environment.getNull();
                }
                return evaluatedArgs[0];

            }
        };
    }


    public static SimpleFunctionTemplate call_qi_function()
    {
        return new SimpleFunctionTemplate()
        {

            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)call_qi_function();
            }

            @Override
            public Value evaluate(final Environment env, Value[] evaluatedArgs) {
                checkActualArguments(3, true, true);
                NAOManager manager = (NAOManager)evaluatedArgs[0].getObjectValue();
                String serviceName = evaluatedArgs[1].getString();
                String functionName = evaluatedArgs[2].getString();

                int numArguments = evaluatedArgs.length - 3;
                Value[] qiArgs = new Value[numArguments];

                for (int i = 0; i < numArguments;i++)
                {
                    qiArgs[i] = evaluatedArgs[i + 3];
                }

                try
                {
                    Value result =  manager.evaluateFunction(serviceName, functionName, qiArgs);
                    if (result == null)
                        return Environment.getNull();
                    else
                        return result;
                }
                catch (Exception e)
                {
                    throw new RuntimeException(e);
                }
            }
        };
    }
}
