package com.evolved.automata.lisp;

public class DefMacroFunctionTemplate extends FunctionTemplate
{
	public DefMacroFunctionTemplate()
	{
		_name = "defmacro";
		
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
		MacroTemplate lam = new MacroTemplate(env, formalParameters, body);
		
		env.mapMacro(name, lam);
		
		// Can't return macro templates
		return _actualParameters[0];
	}
}
