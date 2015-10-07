package com.evolved.automata.lisp;

public abstract class SimpleFunctionTemplate extends FunctionTemplate {

	public SimpleFunctionTemplate()
	{
		
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
			evaluatedArgs[_instructionPointer] = _lastFunctionReturn = env.evaluate(_actualParameters[_instructionPointer], resume);
			if (evaluatedArgs[_instructionPointer].isContinuation()) 
				return continuationReturn(evaluatedArgs[_instructionPointer]);
			if (evaluatedArgs[_instructionPointer].isReturn() || evaluatedArgs[_instructionPointer].isBreak() || evaluatedArgs[_instructionPointer].isSignal() || evaluatedArgs[_instructionPointer].isSignalOut())
				return resetReturn(evaluatedArgs[_instructionPointer]);
		}
		return resetReturn(evaluate(env, evaluatedArgs));
	}

	public abstract Value evaluate(Environment env, Value[] evaluatedArgs);
}
