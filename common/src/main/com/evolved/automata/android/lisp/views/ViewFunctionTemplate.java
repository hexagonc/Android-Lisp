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
	
	public void resetFunctionTemplate()
	{
		_lastFunctionReturn = null;
		_instructionPointer = 0;
		_argumentInstructionPointer = 0;
		_nonEvaluatedKeyPresent = false;
	}
	
	@Override
	public Value evaluate(Environment env, boolean resume)
			throws InstantiationException, IllegalAccessException {
		checkActualArguments(0,true, true);
		if (!resume)
			resetFunctionTemplate();
		else
		{
			_lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
			if (_lastFunctionReturn.isContinuation())
				return continuationReturn(_lastFunctionReturn);
			_instructionPointer++;
		}
		
		Value[] evaluatedArgs = new Value[_actualParameters.length];
		for (;_instructionPointer<_actualParameters.length;_instructionPointer++)
		{
			if (_actualParameters[_instructionPointer].isKeyName() && _nonEvaluatedKeyNames.contains(_actualParameters[_instructionPointer].getString()))
			{
				_nonEvaluatedKeyPresent = true;
				evaluatedArgs[_instructionPointer] = _actualParameters[_instructionPointer];
			}
			else
			{
				if (_nonEvaluatedKeyPresent)
				{
					_nonEvaluatedKeyPresent = false;
					evaluatedArgs[_instructionPointer] = _actualParameters[_instructionPointer];
				}
				else
				{
					evaluatedArgs[_instructionPointer] = _lastFunctionReturn = env.evaluate(_actualParameters[_instructionPointer], resume);
					if (evaluatedArgs[_instructionPointer].isContinuation()) 
						return continuationReturn(evaluatedArgs[_instructionPointer]);
					if (evaluatedArgs[_instructionPointer].isReturn() || evaluatedArgs[_instructionPointer].isBreak() || evaluatedArgs[_instructionPointer].isSignal() || evaluatedArgs[_instructionPointer].isSignalOut())
						return resetReturn(evaluatedArgs[_instructionPointer]);
				}
			}
			
		}
		return resetReturn(evaluate(env, evaluatedArgs));
	}

	public abstract Value evaluate(Environment env, Value[] evaluatedArgs);
}
