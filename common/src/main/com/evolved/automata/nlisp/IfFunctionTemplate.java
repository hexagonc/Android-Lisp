package com.evolved.automata.nlisp;

public class IfFunctionTemplate extends FunctionTemplate
{
	public IfFunctionTemplate()
	{
		_name = "if";	
	}
	
	final int EVALUATING_CONDITION = 0;
	final int EVALUATION_ANTECEDENT = 1;
	final int EVALUATION_ELSE = 2;

	@Override
	public Value evaluate(Environment env, boolean resume)
			throws InstantiationException, IllegalAccessException {
		checkActualArguments(2, true, true);
		Value result = null;
		if (!resume)
			resetFunctionTemplate();
		
		while (true)
		{
			switch (_instructionPointer)
			{
				case  EVALUATING_CONDITION:
					if (resume && _lastFunctionReturn.getContinuingFunction() != null)
						result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
					else
						result = _lastFunctionReturn = env.evaluate(_actualParameters[0], false);
					
					if (result.isContinuation())
						return continuationReturn(result);
					if (result.isReturn() || result.isBreak() || result.isSignal() || result.isSignalOut())
						return resetReturn(result);
					if (!result.isNull())
						_instructionPointer = EVALUATION_ANTECEDENT;
					else if (_actualParameters.length == 3)
						_instructionPointer = EVALUATION_ELSE;
					else
						return resetReturn(Environment.getNull());
					break;
				case EVALUATION_ANTECEDENT:
					if (resume && _lastFunctionReturn.getContinuingFunction() != null)
						result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
					else
						result = _lastFunctionReturn = env.evaluate(_actualParameters[1], false);
					if (result.isContinuation())
						return continuationReturn(result);
					return resetReturn(result);
				case EVALUATION_ELSE:
					if (resume && _lastFunctionReturn.getContinuingFunction() != null)
						result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
					else
						result = _lastFunctionReturn = env.evaluate(_actualParameters[2], false);
					if (result.isContinuation())
						return continuationReturn(result);
					return resetReturn(result);
					
			}
		}
		
	}
	
}
