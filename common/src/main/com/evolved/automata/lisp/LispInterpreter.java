package com.evolved.automata.lisp;

import java.util.LinkedList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import com.evolved.automata.KeyValuePair;

public class LispInterpreter 
{
	 
	
	Environment _env;
	LinkedBlockingQueue<KeyValuePair<Environment, Value>> _commandQueue = new LinkedBlockingQueue<KeyValuePair<Environment, Value>>();
	Thread _processThread = null;
	private LispResponseListener _responseListener = null;
	volatile CountDownLatch _shutdownIndicator = null;
	Object _synch = new Object();
	volatile boolean _runningP = false;
	boolean _breakRequested = false;
	Object _breakSynch = new Object();
	boolean _idle = true;
	
	public interface LispResponseListener
	{
		public void onOutput(Value out);
		public void onIncompleteInputException(String message);
		public void onGeneralException(Throwable e);
	}
	
	public interface LispInputListener
	{
		public void evaluateExpression(String exp);
		public Value evaluateExpressionSynchronous(String exp);
		public void evaluateValue(Value value);
		
		public void evaluateExpression(String exp, Environment env);
		public Value evaluateExpressionSynchronous(String exp, Environment env);
		public void evaluateValue(Value value, Environment env);
		
		public void breakExecution();
	}
	
	
	
	
	public LispInterpreter(Environment env)
	{
		_env = env;
	}
	
	/**
	 * Starts the LispCommandInterpreter.  Has no effect if the interpreter is already running
	 * @param responseListener
	 * @return
	 */
	public LispInputListener start(LispResponseListener responseListener, boolean blockUntilStartedP)
	{
		
		synchronized (_synch)
		{
			_responseListener = responseListener;
			if (_processThread != null)
				return null;
			
			if (blockUntilStartedP)
			{
				CountDownLatch latch = new CountDownLatch(1);
				_processThread = getProcessThread(latch);
				_processThread.start();
				try {
					latch.await();
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
			else
			{
				_processThread = getProcessThread();
				_processThread.start();
			}
				
			 return new LispInputListener()
			 {

				@Override
				public void evaluateExpression(String exp) {
					try
					{
						synchronized (_breakSynch)
						{
							_commandQueue.put(new KeyValuePair<Environment, Value>(null, NLispTools.makeValue(exp)));
						}
					}
					catch (InterruptedException ie)
					{
						
					}
				}
				
				@Override
				public void evaluateValue(Value value) {
					try
					{
						synchronized (_breakSynch)
						{
							_commandQueue.put(new KeyValuePair<Environment, Value>(null, value));
						}
					}
					catch (InterruptedException ie)
					{
						
					}
				}
				

				@Override
				public void breakExecution() {
					
					synchronized (_breakSynch)
					{
						if (_idle && _commandQueue.size() == 0)
							return;
						_breakRequested = true;
					}
					
				}

				@Override
				public Value evaluateExpressionSynchronous(String exp) 
				{
					try
					{
						return _env.evaluate(exp, true);
					}
					catch (Exception e)
					{
						throw new RuntimeException(e);
					}
				}

				@Override
				public void evaluateExpression(String exp, Environment env) {
					try
					{
						synchronized (_breakSynch)
						{
							_commandQueue.put(new KeyValuePair<Environment, Value>(env, NLispTools.makeValue(exp)));
						}
					}
					catch (InterruptedException e)
					{
						
					}
				}

				@Override
				public Value evaluateExpressionSynchronous(String exp,
						Environment env) {
					try
					{
						return env.evaluate(exp, true);
					}
					catch (Exception e)
					{
						throw new RuntimeException(e);
					}
				}

				@Override
				public void evaluateValue(Value value, Environment env) {
					try
					{
						synchronized (_breakSynch)
						{
							_commandQueue.put(new KeyValuePair<Environment, Value>(env, value));
						}
					}
					catch (InterruptedException e)
					{
						
					}
				}
				 
			 };
		}
		
		
	}
	
	public LispInputListener setResponseListener(LispResponseListener responseListener)
	{
		synchronized (_synch)
		{
			_responseListener = responseListener;
			
				
			 return new LispInputListener()
			 {

				@Override
				public void evaluateExpression(String exp) {
					try
					{
						synchronized (_breakSynch)
						{
							_commandQueue.put(new KeyValuePair<Environment, Value>(null, NLispTools.makeValue(exp)));
						}
					}
					catch (InterruptedException ie)
					{
						
					}
				}
				
				@Override
				public void evaluateValue(Value value) {
					try
					{
						synchronized (_breakSynch)
						{
							_commandQueue.put(new KeyValuePair<Environment, Value>(null, value));
						}
					}
					catch (InterruptedException ie)
					{
						
					}
				}
				

				@Override
				public void breakExecution() {
					synchronized (_breakSynch)
					{
						_breakRequested = true;
					}
				}

				@Override
				public Value evaluateExpressionSynchronous(String exp) 
				{
					try
					{
						return _env.evaluate(exp, true);
					}
					catch (Exception e)
					{
						throw new RuntimeException(e);
					}
				}

				@Override
				public void evaluateExpression(String exp, Environment env) {
					try
					{
						synchronized (_breakSynch)
						{
							_commandQueue.put(new KeyValuePair<Environment, Value>(env, NLispTools.makeValue(exp)));
						}
					}
					catch (Exception e)
					{
						throw new RuntimeException(e);
					}
				}

				@Override
				public Value evaluateExpressionSynchronous(String exp,
						Environment env) {
					try
					{
						return env.evaluate(exp, true);
					}
					catch (Exception e)
					{
						throw new RuntimeException(e);
					}
				}

				@Override
				public void evaluateValue(Value value, Environment env) {
					try
					{
						synchronized (_breakSynch)
						{
							_commandQueue.put(new KeyValuePair<Environment, Value>(env, value));
						}
					}
					catch (InterruptedException ie)
					{
						
					}
				}
				 
			 };
		}
	}
	
	/**
	 * Stops the interpreter.  Can block until the interpreter has shutdown.  If maxWaitMilli
	 * is provided and it takes longer than that to stop the interpreter than returns false,
	 * otherwise returns true
	 */
	public boolean stop(boolean blockUntilFinished, int maxWaitMilli)
	{
		boolean success = true;
		synchronized (_synch)
		{
			if (_processThread != null)
			{
				_shutdownIndicator = new CountDownLatch(1);
				synchronized (_breakSynch)
				{
					_commandQueue.clear();
				}
				_processThread.interrupt();
				try {
					if (maxWaitMilli < 0)
						_shutdownIndicator.await();
					else
						success = _shutdownIndicator.await(maxWaitMilli, TimeUnit.MILLISECONDS);
					
				} catch (InterruptedException e) {

					e.printStackTrace();
				}
				_processThread = null;
			}
		}
		return success;	
	}
	
	
	Thread getProcessThread()
	{
		return getProcessThread(null);
	}
	
	Thread getProcessThread(final CountDownLatch startup)
	{
		return new Thread()
		{
			public void run()
			{
				_runningP = true;
				KeyValuePair<Environment, Value> command = null;
				Value out = null;
				LinkedList<Value> parsedResult = null;
				int i;
				Value commandValue = null;
				Environment customEnvironment = null;
				try
				{
					if (startup!=null)
						startup.countDown();
					while (true)
					{
						
						
						synchronized (_breakSynch)
						{
							_idle = true;
						}
						command = _commandQueue.take();
						
						synchronized (_breakSynch)
						{
							_idle = false;
							if (_breakRequested)
							{
								_commandQueue.clear();
								_breakRequested = false;
								continue;
							}
						}
						
						commandValue = command.GetValue();
						customEnvironment = command.GetKey();
						if (commandValue.isString() && !commandValue.isContinuation())
						{
							try
							{
								parsedResult = Environment.parse(commandValue.getString(), true);
								
							}
							catch (IncompleteLispExpressionException ile)
							{
								if (_responseListener != null)
									_responseListener.onIncompleteInputException(ile.toString());
								continue;
							}
							catch (Exception e)
							{
								if (e instanceof InterruptedException)
									return;
								if (_responseListener != null)
									_responseListener.onGeneralException(e);
								continue;
							}
							
							try
							{
								i = 0;
								for (Value comp:parsedResult)
								{
									if (customEnvironment!=null)
										out = customEnvironment.evaluate(comp, false);
									else
										out = _env.evaluate(comp, false);
									
									// Processing continuations of intermediate expressions immediately
									if (parsedResult.size() - 1 > i)
									{
										while (out.isContinuation())
										{
											if (customEnvironment!=null)
												out = out.getContinuingFunction().evaluate(customEnvironment, true);
											else
												out = out.getContinuingFunction().evaluate(_env, true);
											
											
										}
									}
									i++;
								}
							}
							catch (Exception e)
							{
								if (e instanceof InterruptedException)
									return;
								if (_responseListener != null)
									_responseListener.onGeneralException(e);
								continue;
							}
							
						}
						else if (commandValue.isContinuation())
						{
							if (customEnvironment!=null)
								out = commandValue.getContinuingFunction().evaluate(customEnvironment, true);
							else
								out = commandValue.getContinuingFunction().evaluate(_env, true);
						}
						else
						{
							if (customEnvironment!=null)
								out = customEnvironment.evaluate(commandValue, false);
							else
								out = _env.evaluate(commandValue, false);
							
							
						}
						
						
						if (out.isContinuation())
						{
							synchronized (_breakSynch)
							{
								_commandQueue.add(new KeyValuePair<Environment, Value>(customEnvironment, out));
							}
						}
						else if (_responseListener!=null)
						{
							_responseListener.onOutput(out);
						}
					}
				}
				catch (InterruptedException ie)
				{
					
				}
				catch (Exception e)
				{
					if (_responseListener != null)
						_responseListener.onGeneralException(e);
				}
				catch (Error e)
				{
					if (_responseListener != null)
						_responseListener.onGeneralException(e);
				}
				finally
				{
					if (_shutdownIndicator != null)
						_shutdownIndicator.countDown();
				}
				
				
			}
		};
	}
	
	public boolean isRunning()
	{
		return _runningP;
	}
}
