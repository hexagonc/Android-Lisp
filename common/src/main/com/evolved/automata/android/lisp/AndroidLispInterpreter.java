package com.evolved.automata.android.lisp;

import java.util.LinkedList;

import android.content.Context;
import android.os.Handler;
import android.os.Looper;

import com.evolved.automata.lisp.Environment;
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
	
	public void setErrorListener(ResponseListener listener)
	{
		_eListener = listener;
	}
	
	public void setNewEnvironment(Environment env)
	{
		_env = env;
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
		else
		{
			_mainThreadHandler.post(getEvaluationSlice(env, value, remaining));
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
	
	
	
	private Runnable getEvaluationSlice(final Environment env, final Value input, final LinkedList<Value> remaining)
	{
		return new Runnable()
		{
			public void run()
			{
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
				if (tout.isContinuation())
				{
					_mainThreadHandler.post(getEvaluationSlice(env, tout, remaining));
				}
				else if (remaining.size() > 0) 
				{
					_mainThreadHandler.post(getEvaluationSlice(env, remaining.removeFirst(), remaining));
				} else if (_eListener!=null)
					_eListener.onResult(tout);
			}
		};
	}
	
	
	
	
}
