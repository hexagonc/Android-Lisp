package com.evolved.automata.lisp;

public class AndFunctionTemplate extends FunctionTemplate
{
	public AndFunctionTemplate()
	{
		_name = "and";	
	}

	public void resetFunctionTemplate()
	{
		_lastFunctionReturn = null;
		_instructionPointer = 0;
	}
	
	@Override
	public Value evaluate(Environment env, boolean resume)
			throws InstantiationException, IllegalAccessException {
		checkActualArguments(1, true, true);
		Value result = Environment.getNull();
		if (!resume)
		{
			resetFunctionTemplate();
		}
		
		while (_instructionPointer<_actualParameters.length)
		{
			if (resume && _lastFunctionReturn.getContinuingFunction() != null)
				result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
			else
				result = _lastFunctionReturn = env.evaluate(_actualParameters[_instructionPointer], false);
			if (result.isContinuation())
				return continuationReturn(result);
			if (result.isNull() || result.isBreak() || result.isReturn() || result.isSignal() || result.isSignalOut())
				break;
			
			_instructionPointer++;
		}
		return resetReturn(result);
	}
}
