package com.evolved.automata.android.lisp.views;

import java.util.HashSet;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.Value;

public abstract class ViewFunctionTemplate extends FunctionTemplate 
{
	
	public static final HashSet<String> _nonEvaluatedKeyNames = new HashSet<String>()
			{
				{
					add(ViewProxy.ON_CLICK);
					add(ViewProxy.ON_LONG_CLICK);
				}
			};
			
	public ViewFunctionTemplate()
	{
		
	}
	
	boolean _nonEvaluatedKeyPresent = false;
	boolean _continuationReturn = false;
	Value _previousContinuedOutput = null;
	private Value[] _evaluatedArgs = null;
	
	@Override
	public void resetFunctionTemplate()
	{
		_lastFunctionReturn = null;
		_instructionPointer = 0;
		_argumentInstructionPointer = 0;
		_nonEvaluatedKeyPresent = false;
		_previousContinuedOutput = null;
		_continuationReturn = false;
		_evaluatedArgs = new Value[_actualParameters.length];
	}
	
	@Override
	public Value evaluate(Environment env, boolean resume)
			throws InstantiationException, IllegalAccessException {
		
		if (!resume)
			resetFunctionTemplate();
		else
		{
			if (_continuationReturn && _previousContinuedOutput != null)
			{
				_previousContinuedOutput.setContinuation(false, null);
				return resetReturn(_previousContinuedOutput);
			}
			
			_lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, true);
			if (_lastFunctionReturn.isContinuation())
				return continuationReturn(_lastFunctionReturn);
			_evaluatedArgs[_instructionPointer] = _lastFunctionReturn;
			_instructionPointer++;
		}
		
		
		for (;_instructionPointer<_actualParameters.length;_instructionPointer++)
		{
			if (_actualParameters[_instructionPointer].isKeyName() && _nonEvaluatedKeyNames.contains(_actualParameters[_instructionPointer].getString()))
			{
				_nonEvaluatedKeyPresent = true;
				_evaluatedArgs[_instructionPointer] = _actualParameters[_instructionPointer];
			}
			else
			{
				if (_nonEvaluatedKeyPresent)
				{
					_nonEvaluatedKeyPresent = false;
					_evaluatedArgs[_instructionPointer] = _actualParameters[_instructionPointer];
				}
				else
				{
					_evaluatedArgs[_instructionPointer] = _lastFunctionReturn = env.evaluate(_actualParameters[_instructionPointer], false);
					if (_evaluatedArgs[_instructionPointer].isContinuation()) 
						return continuationReturn(_evaluatedArgs[_instructionPointer]);
					if (_evaluatedArgs[_instructionPointer].isReturn() || _evaluatedArgs[_instructionPointer].isBreak() || _evaluatedArgs[_instructionPointer].isSignal() || _evaluatedArgs[_instructionPointer].isSignalOut())
						return resetReturn(_evaluatedArgs[_instructionPointer]);
				}
			}
			
		}
		Value temp = evaluate(env, _evaluatedArgs); 
		if (temp.isContinuation())
		{
			_previousContinuedOutput = temp;
			_continuationReturn = true;
			return temp;
		}
		else
			return resetReturn(temp); 
	}

	public abstract Value evaluate(Environment env, Value[] evaluatedArgs);
}
