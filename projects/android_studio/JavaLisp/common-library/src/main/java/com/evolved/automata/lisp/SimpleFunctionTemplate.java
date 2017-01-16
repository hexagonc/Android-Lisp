package com.evolved.automata.lisp;

import java.util.HashMap;
import java.util.LinkedList;

public abstract class SimpleFunctionTemplate extends FunctionTemplate {

	public SimpleFunctionTemplate()
	{
		
	}
	

	protected HashMap<String, Value> keywordParameters;
	LinkedList<Value> inputArgs = new LinkedList<Value>();
	Value previousKey = null;
	@Override
	public void resetFunctionTemplate()
	{
		_lastFunctionReturn = null;
		_instructionPointer = 0;
		_argumentInstructionPointer = 0;

		keywordParameters = new HashMap<String, Value>();
		inputArgs = new LinkedList<Value>();
		previousKey = null;
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
			if (previousKey != null)
			{
				if (_lastFunctionReturn.isContinuation())
					return continuationReturn(_lastFunctionReturn);
				if (_lastFunctionReturn.isReturn() || _lastFunctionReturn.isBreak() || _lastFunctionReturn.isSignal() || _lastFunctionReturn.isSignalOut())
					return resetReturn(_lastFunctionReturn);
				keywordParameters.put(previousKey.getString(), _lastFunctionReturn);
				previousKey = null;
			}
			else
			{
				inputArgs.add(_lastFunctionReturn);

			}
			_instructionPointer++;
		}


		for (;_instructionPointer<_actualParameters.length;_instructionPointer++)
		{
			if (previousKey != null)
			{

				if (_actualParameters[_instructionPointer].isKeyName())
				{
					keywordParameters.put(previousKey.getString(), _actualParameters[_instructionPointer]);
				}
				else
				{
					_lastFunctionReturn = env.evaluate(_actualParameters[_instructionPointer], false);
					if (_lastFunctionReturn.isContinuation())
						return continuationReturn(_lastFunctionReturn);
					if (_lastFunctionReturn.isReturn() || _lastFunctionReturn.isBreak() || _lastFunctionReturn.isSignal() || _lastFunctionReturn.isSignalOut())
						return resetReturn(_lastFunctionReturn);
					keywordParameters.put(previousKey.getString(), _lastFunctionReturn);
				}
				previousKey = null;
			}
			else
			{
				if (_actualParameters[_instructionPointer].isKeyName())
				{
					previousKey = _actualParameters[_instructionPointer];
				}
				else
				{
					_lastFunctionReturn = env.evaluate(_actualParameters[_instructionPointer], false);
					if (_lastFunctionReturn.isContinuation())
						return continuationReturn(_lastFunctionReturn);
					if (_lastFunctionReturn.isReturn() || _lastFunctionReturn.isBreak() || _lastFunctionReturn.isSignal() || _lastFunctionReturn.isSignalOut())
						return resetReturn(_lastFunctionReturn);
					inputArgs.add(_lastFunctionReturn);
				}
			}


		}
		return resetReturn(evaluate(env, inputArgs.toArray(new Value[0])));
	}

	public abstract Value evaluate(Environment env, Value[] evaluatedArgs);
}
