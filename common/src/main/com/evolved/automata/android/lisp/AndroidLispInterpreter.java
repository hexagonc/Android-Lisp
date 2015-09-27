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
		final LinkedList<Value> out = _env.parse(exp, true);
		
		if (Looper.myLooper() == Looper.getMainLooper())
		{
			Value tout = null;
			try {
				tout = _env.evaluate(out.getFirst(), false);
			} catch (Exception e) {
				notifyError(e);
				return null;
			} 
			if (tout.isContinuation())
			{
				_mainThreadHandler.post(getEvaluationSlice(tout));
				return null;
			}
			else if (alwaysUseCallbackP)
			{
				_eListener.onResult(tout);
				return null;
			}
			else
				return tout;
		}
		else
		{
			_mainThreadHandler.post(new Runnable()
			{
				public void run()
				{
					Value tout = null;
					try {
						tout = _env.evaluate(out.getFirst(), false);
					} catch (Exception e) {
						notifyError(e);
						return;
					} 
					if (tout.isContinuation())
						_mainThreadHandler.post(getEvaluationSlice(tout));
					else
						_eListener.onResult(tout);
				}
			});
		}
		return null;
	}
	
	
	private Runnable getEvaluationSlice(final Value input)
	{
		return new Runnable()
		{
			public void run()
			{
				Value tout = null;
				try {
					tout = _env.evaluate(input, true);
				} catch (Exception e) {
					notifyError(e);
					return;
				} 
				if (tout.isContinuation())
				{
					_mainThreadHandler.post(getEvaluationSlice(tout));
				}
			}
		};
	}
}
