package com.evolved.automata.lisp;

public abstract class SimpleFunctionTemplate extends FunctionTemplate {

	public SimpleFunctionTemplate()
	{
		
	}
	
	private Value[] _evaluatedArgs = null;
	
	@Override
	public void resetFunctionTemplate()
	{
		_lastFunctionReturn = null;
		_instructionPointer = 0;
		_argumentInstructionPointer = 0;
		_evaluatedArgs = new Value[_actualParameters.length];
	}
	
	
	@Override
	public Value evaluate(Environment env, boolean resume)
			throws InstantiationException, IllegalAccessException {
		checkActualArguments(0,true, true);
		if (!resume)
			resetFunctionTemplate();
		else
		{
			_lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, true);
			if (_lastFunctionReturn.isContinuation())
				return continuationReturn(_lastFunctionReturn);
			_evaluatedArgs[_instructionPointer] = _lastFunctionReturn;
			_instructionPointer++;
		}
		
		
		for (;_instructionPointer<_actualParameters.length;_instructionPointer++)
		{
			_evaluatedArgs[_instructionPointer] = _lastFunctionReturn = env.evaluate(_actualParameters[_instructionPointer], false);
			if (_evaluatedArgs[_instructionPointer].isContinuation()) 
				return continuationReturn(_evaluatedArgs[_instructionPointer]);
			if (_evaluatedArgs[_instructionPointer].isReturn() || _evaluatedArgs[_instructionPointer].isBreak() || _evaluatedArgs[_instructionPointer].isSignal() || _evaluatedArgs[_instructionPointer].isSignalOut())
				return resetReturn(_evaluatedArgs[_instructionPointer]);
		}
		return resetReturn(evaluate(env, _evaluatedArgs));
	}

	public abstract Value evaluate(Environment env, Value[] evaluatedArgs);
}
