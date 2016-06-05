package com.evolved.automata.lisp;

public abstract class FunctionTemplate 
{
	protected Value[] _actualParameters;
	protected Environment _callingEnvironment;
	protected int _argumentIndex = 0;
	boolean _isMacro = false;
	protected String _name = "";
	public static final String _VAR_ARGNAME = "...";
	protected int _instructionPointer = 0;
	protected Value _lastFunctionReturn = null;
	protected int _numArguments = 0;
	protected int _argumentInstructionPointer = 0;
	
	
	public abstract Value evaluate(Environment env, boolean resume)  throws InstantiationException, IllegalAccessException;
	
	@Override
	public Object clone()
	{
		try
		{
			Object o = innerClone(); 
			((FunctionTemplate)o).setName(_name);
			return o;
		}
		catch (Exception e)
		{
			return null;
		}
	}
	
	public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
	{
		Class<T> c = (Class<T>)this.getClass();
		T base = (T)c.newInstance();
		base._name = _name;
		return base;
	}
	
	public String checkActualArguments(int numArguments, boolean allowExtraP,boolean suppressWarningP)
	{
		if (_actualParameters == null || _actualParameters.length<numArguments ||( _actualParameters.length>numArguments && !allowExtraP))
		{
			throw new RuntimeException("Incorrect parameters for " + _name);
		}
		if (_actualParameters.length>numArguments && !suppressWarningP)
		{
			String warning = "Warning: more parameters than expected for: " + _name;
			Environment.warn(warning);
			return warning;
		}
		else
			return null;
	}
	
	public void setActualParameters(Value[] p)
	{
		_actualParameters = p;
		_numArguments = p.length;
	}
	
	public boolean isMacroP(){
		return _isMacro;
	}
	
	public String getName()
	{
		return _name;
	}
	
	public void setName(String name)
	{
		_name = name;
	}
	
	public boolean hasNameP()
	{
		return _name.trim().length()>0;
	}
	
	public String toString()
	{
		return "(#' " + _name + ")";
	}
	
	public String serialize()
	{
		return toString();
	}
	
	public void resetFunctionTemplate()
	{
		_lastFunctionReturn = null;
		_instructionPointer = 0;
		_argumentInstructionPointer = 0;
	}
	
	public Value resetReturn(Value v)
	{
		resetFunctionTemplate();
		return v;
	}
	
	public Value continuationReturn(Value v)
	{
		return v.clone().setContinuation(true, this);
	}
	
	
	// Only set allowRelativeBindingP as true if the bindingEnvironment is a child of the evaluationEnvironment
	public Value evaluateBindingList(Environment bindingEnvironment, Environment evaluationEnvironment, Value[] bindingPairList, boolean allowRelativeBindingP, boolean resume) throws InstantiationException, IllegalAccessException
	{
		String name;
		Value value = Environment.getNull(), bindingPair;
		
		Environment target = (allowRelativeBindingP)?bindingEnvironment:evaluationEnvironment;
		for (;_argumentInstructionPointer<bindingPairList.length;_argumentInstructionPointer++)
		{
			bindingPair = bindingPairList[_argumentInstructionPointer];
			if (bindingPair.isList())
			{
				if (bindingPair.getList().length == 2)
				{
					if (bindingPair.getList()[0].isIdentifier())
					{
						name = bindingPair.getList()[0].getString();
						
						if (resume && _lastFunctionReturn.getContinuingFunction() != null)
							value = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(target, resume);
						else
							value = _lastFunctionReturn = target.evaluate(bindingPair.getList()[1], false);
						
						if (value.isContinuation())
							return continuationReturn(value);
						if (value.isBreak() || value.isReturn() || value.isSignal() || value.isSignalOut())
							return resetReturn(value);
						
						bindingEnvironment.mapValue(name, value);
					}
					else
						throw new RuntimeException("Incorrect variable binding target: " + bindingPair.getList()[0]);
				}
				else
					throw new RuntimeException("Incorrect argument count for binding list: " + bindingPair.getList().length);
			}
			else
				throw new RuntimeException("Incorrect argument type for binding list: " + bindingPair);
		}
		return value;
	}
	
	protected void checkNumericArguments(Value[] args)
	{
		for (Value v:args)
		{
			if (!NLispTools.isNumericType(v))
				throw new RuntimeException(_name + "requires numeric argument types: " + v);
		}
	}
	
	protected void checkStringArguments(Value[] args)
	{
		for (Value v:args)
		{
			if (!v.isString())
				throw new RuntimeException(_name + "requires string argument types: " + v);
		}
	}
	
	protected void checkListsArguments(Value[] args)
	{
		for (Value v:args)
		{
			if (!v.isList())
				throw new RuntimeException(_name + "requires list argument types: " + v);
		}
	}
	
}
