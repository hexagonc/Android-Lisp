package com.evolved.automata.lisp;

import java.util.LinkedList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

public class LispInterpreter 
{
	Environment _env;
	LinkedBlockingQueue<Value> _commandQueue = new LinkedBlockingQueue<Value>();
	Thread _processThread = null;
	private LispResponseListener _responseListener = null;
	volatile CountDownLatch _shutdownIndicator = null;
	Object _synch = new Object();
	volatile boolean _runningP = false;
	
	public interface LispResponseListener
	{
		public void onOutput(Value out);
		public void onIncompleteInputException(String message);
		public void onGeneralException(Exception e);
	}
	
	public interface LispInputListener
	{
		public void evaluateExpression(String exp);
		public Value evaluateExpressionSynchronous(String exp);
		public void evaluateValue(Value value);
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
						_commandQueue.put(NLispTools.makeValue(exp));
					}
					catch (InterruptedException ie)
					{
						
					}
				}
				
				@Override
				public void evaluateValue(Value value) {
					try
					{
						_commandQueue.put(value);
					}
					catch (InterruptedException ie)
					{
						
					}
				}
				

				@Override
				public void breakExecution() {
					_commandQueue.clear();
					
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
				_commandQueue.clear();
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
				Value command = null, out = null;
				LinkedList<Value> parsedResult = null;
				int i;
				try
				{
					if (startup!=null)
						startup.countDown();
					while (true)
					{
						command = _commandQueue.take();
						
						if (command.isString() && !command.isContinuation())
						{
							try
							{
								parsedResult = Environment.parse(command.getString(), true);
								
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
									out = _env.evaluate(comp, false);
									
									// Processing continuations of intermediate expressions immediately
									if (parsedResult.size() - 1 > i)
									{
										while (out.isContinuation())
										{
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
						else if (command.isContinuation())
						{
							out = command.getContinuingFunction().evaluate(_env, true);
						}
						else
						{
							out = _env.evaluate(command, false);
						}
						if (out.isContinuation())
						{
							_commandQueue.add(out);
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
