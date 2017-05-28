package com.evolved.automata.android.lisp;

import java.util.HashSet;
import java.util.LinkedList;

import android.content.Context;
import android.os.Handler;
import android.os.Looper;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.Value;

public class AndroidLispInterpreter 
{
	public interface ResponseListener
	{
		public void onError(Exception e);
		public void onResult(Value v);
	}
	
	
	Environment _env;
	Context _context;
	Handler _mainThreadHandler;
	ResponseListener _eListener;
	boolean _breakRequestedP = false;
	LinkedList<Runnable> _pendingEvaluationMap = new LinkedList<Runnable>();
	boolean _runningProcessP = false;
	Object _breakSynch = new Object();
	
	
	public AndroidLispInterpreter(Context context, Environment env, ResponseListener listener)
	{
		_context = context;
		_env = env;
		_mainThreadHandler = new Handler(Looper.getMainLooper());
		_eListener = listener;
	}
	
	public void notifyError(Exception e)
	{
		if (_eListener!=null)
			_eListener.onError(e);
	}
	
	public void setResponseListener(ResponseListener listener)
	{
		_eListener = listener;
	}
	
	public void setNewEnvironment(Environment env)
	{
		_env = env;
	}
	
	public void breakProcessing()
	{
		synchronized(_breakSynch)
		{
			if (_runningProcessP)
				_breakRequestedP = true;
			while (_pendingEvaluationMap.size()>0)
			{
				Runnable next = _pendingEvaluationMap.removeFirst();
				_mainThreadHandler.removeCallbacks(next);
			}
		}
		
		
	}
	
	
	public Value evaluateExpression(Environment env, String exp, boolean alwaysUseCallbackP) 
	{
		LinkedList<Value> temp = null;
		try
		{
			temp = Environment.parse(exp, true);
		}
		catch (Exception e)
		{
			notifyError(e);
			return null;
		}
		
		return evaluatePreParsedValue(env, temp.removeFirst(), temp, alwaysUseCallbackP);
	}
	
	/**
	 * Evaluates the lisp expression on the main thread and tries to return the result.  
	 * If already running on the main thread and if the result doesn't involve continuations, 
	 * then this returns the result directly. Otherwise returns null immediately. If 
	 * alwaysUseCallbackP is true then always returns null and the result is delivered via 
	 * the ResponseListener
	 * @param exp
	 * @param alwaysUseCallbackP
	 * @return
	 */
	public Value evaluateExpression(String exp, boolean alwaysUseCallbackP) 
	{
		return evaluateExpression(_env, exp, alwaysUseCallbackP);
	}
	
	private Value evaluatePreParsedValue(Environment env, Value value, LinkedList<Value> remaining, boolean alwaysUseCallbackP)
	{
		
		if (Looper.myLooper() == Looper.getMainLooper())
		{
			Value tout = null;
			try {
				tout = env.evaluate(value, false);
			} catch (Exception e) {
				notifyError(e);
				return null;
			}
			if (_breakRequestedP && tout.isContinuation())
			{
				_breakRequestedP = false;
				return null;
			}
			synchronized(_breakSynch)
			{
				if (tout.isContinuation())
				{
					_mainThreadHandler.post(getEvaluationSlice(env, tout, remaining));
					return null;
				}
				else if (alwaysUseCallbackP)
				{
					if (remaining.size()>0)
						evaluatePreParsedValue(env, remaining.removeFirst(), remaining, alwaysUseCallbackP);
					else if (_eListener!=null)
						_eListener.onResult(tout);
					return null;
				}
				else
				{
					if (remaining.size()>0)
						return evaluatePreParsedValue(env, remaining.removeFirst(), remaining, alwaysUseCallbackP);
					else
						return tout;
				}
			}
			
		}
		else
		{
			synchronized(_breakSynch)
			{
				if (!_breakRequestedP)
					_mainThreadHandler.post(getEvaluationSlice(env, value, remaining));
				else
					_breakRequestedP = false;
			}
			
			return null;
		}
	}
	
	public Value evaluatePreParsedValue(Value value, boolean alwaysUseCallbackP)
	{
		return evaluatePreParsedValue(_env, value, new LinkedList<Value>(), alwaysUseCallbackP);
	}
	
	public Value evaluatePreParsedValue(Environment env, Value value, boolean alwaysUseCallbackP)
	{
		return evaluatePreParsedValue(env, value, new LinkedList<Value>(), alwaysUseCallbackP);
	}
	
	public Value evaluateFunction(final FunctionTemplate function)
	{
		try
		{
			
			if (Looper.myLooper() == Looper.getMainLooper())
			{
				_runningProcessP = true;
				try
				{

					Value out = function.evaluate(_env, false);
					synchronized (_breakSynch)
					{
						_runningProcessP = false;
						if (_breakRequestedP && out.isContinuation())
						{
							_breakRequestedP = false;
							return null;
						}
						
						if (out.isContinuation())
						{
							if (!_breakRequestedP)
								_mainThreadHandler.post(getEvaluationSlice(_env, out, new LinkedList<Value>()));
							else
								_breakRequestedP = false;
							return null;
						}
						else
							return out;
					}
					
				}
				catch (Exception e)
				{
					notifyError(e);
				}
				return null;
			}
			else
			{
				synchronized (_breakSynch)
				{
					if (!_breakRequestedP)
					{				
						_mainThreadHandler.post(new Runnable()
						{
							public void run()
							{
								evaluateFunction(function);
							}
						});
					}
					else
						_breakRequestedP = false;
				}
				
				return null;
			}
		}
		finally
		{
			_runningProcessP = false;
		}
		
		
		
	}

	public Value evaluateFunction(final FunctionTemplate function, final Environment env)
	{
		try
		{

			if (Looper.myLooper() == Looper.getMainLooper())
			{
				_runningProcessP = true;
				try
				{

					Value out = function.evaluate(env, false);
					synchronized (_breakSynch)
					{
						_runningProcessP = false;
						if (_breakRequestedP && out.isContinuation())
						{
							_breakRequestedP = false;
							return null;
						}

						if (out.isContinuation())
						{
							if (!_breakRequestedP)
								_mainThreadHandler.post(getEvaluationSlice(env, out, new LinkedList<Value>()));
							else
								_breakRequestedP = false;
							return null;
						}
						else
							return out;
					}

				}
				catch (Exception e)
				{
					notifyError(e);
				}
				return null;
			}
			else
			{
				synchronized (_breakSynch)
				{
					if (!_breakRequestedP)
					{
						_mainThreadHandler.post(new Runnable()
						{
							public void run()
							{
								evaluateFunction(function, env);
							}
						});
					}
					else
						_breakRequestedP = false;
				}

				return null;
			}
		}
		finally
		{
			_runningProcessP = false;
		}



	}
	
	private Runnable getEvaluationSlice(final Environment env, final Value input, final LinkedList<Value> remaining)
	{
		
		Runnable record = new Runnable()
		{
			public void run()
			{
				_runningProcessP = true;
				try
				{
					_pendingEvaluationMap.remove(this);
					Value tout = null;
					try {
						
						if (input.isContinuation())
							tout = input.getContinuingFunction().evaluate(env, true);
						else
							tout = env.evaluate(input, false);
					} catch (Exception e) {
						notifyError(e);
						return;
					}
					
					synchronized (_breakSynch)
					{
						_runningProcessP = false;
						if (_breakRequestedP && tout.isContinuation())
							return;
						if (tout.isContinuation())
						{
							if (!_breakRequestedP)
								_mainThreadHandler.post(getEvaluationSlice(env, tout, remaining));
							
						}
						else if (remaining.size() > 0) 
						{
							if (!_breakRequestedP)
								_mainThreadHandler.post(getEvaluationSlice(env, remaining.removeFirst(), remaining));
							
						} else if (_eListener!=null)
							_eListener.onResult(tout);
						
					}
				}
				finally
				{
					_breakRequestedP = false;
					_runningProcessP = false;
					
				}
				
			}
		};
		_pendingEvaluationMap.add(record);
		return record;
	}
	
	
	
	
}
