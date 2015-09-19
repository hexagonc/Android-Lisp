package com.evolved.automata.nlisp;

public class DefunFunctionTemplate extends FunctionTemplate
{
	
	
	public DefunFunctionTemplate()
	{
		_name = "defun";
		
	}
	
	@Override
	public Value evaluate(Environment env, boolean resume)
			throws InstantiationException, IllegalAccessException 
	{
		checkActualArguments(3, true, true);
		
		String name = _actualParameters[0].getString();
		String[] formalParameters = NLispTools.getStringArrayFromValue(_actualParameters[1]);
		Value[] body = new Value[_actualParameters.length-2];
		for (int i=0;i<body.length;i++)
			body[i] = _actualParameters[i+2];
		Lambda lam = new Lambda(env, formalParameters, body);
		
		env.mapFunction(name, lam);
		LambdaValue lv = new LambdaValue(lam);
		return lv;
	}

}
