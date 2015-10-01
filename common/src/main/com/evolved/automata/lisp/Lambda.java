package com.evolved.automata.lisp;

import java.util.HashMap;
import java.util.LinkedList;



public class Lambda extends FunctionTemplate {
	final String[] _formalParameters;
	final Environment _innerEnvironment;
	final Value[] _bodyArguments;
	boolean _appendToVargs = false;
	static final String _THIS_VAR_NAME = "this";
	static final String _KEY_VALUE_MAP_VAR_NAME = "key-map";
	boolean _processedArgs = false;
	LinkedList<Value> _variableArgs = null;
	String _previousKeyArgumentName = null;
	HashMap<String, Value> _argumentKeyMap = null;
	
	public Lambda(Environment innerEnv, String[] formalParameters, Value[] bodyArgs)
	{
		assert innerEnv!=null;
		assert formalParameters != null;
		assert bodyArgs != null;
		_innerEnvironment = new Environment(innerEnv);
		_formalParameters = formalParameters;
		_bodyArguments = bodyArgs;
		_previousKeyArgumentName = null;
		_argumentKeyMap = new HashMap<String, Value>();
		
	}
	
	
	@Override
	public String toString()
	{
		StringBuilder sBuilder = null;
		if (hasNameP())
		{
			sBuilder = new StringBuilder("(#' " + _name + ")");
		}
		else
		{
			sBuilder = new StringBuilder("(lambda ");
			sBuilder.append(getParameterListString());
			sBuilder.append(" ");
			sBuilder.append(getBodyListString(" "));
			sBuilder.append(")");
		}
		
		
		return sBuilder.toString();
		
	}
	
	public String getBodyListString(String delimiter)
	{
		StringBuilder sBuilder = new StringBuilder();
		if (_bodyArguments.length>0)
			sBuilder.append(_bodyArguments[0]);
		for (int i=1;i<_bodyArguments.length;i++)
		{
			sBuilder.append(delimiter);
			sBuilder.append(_bodyArguments[i]);
		}
		
		return sBuilder.toString();
	}
	
	public String getParameterListString()
	{
		StringBuilder sBuilder = new StringBuilder("(");
		if (_formalParameters.length>0)
			sBuilder.append(_formalParameters[0]);
		for (int i=1;i<_formalParameters.length;i++)
		{
			sBuilder.append(" ");
			sBuilder.append(_formalParameters[i]);
		}
		sBuilder.append(")");
		return sBuilder.toString();
	}
	
	Environment getInnerEnvironment()
	{
		return _innerEnvironment;
	}
	
	
	
	@Override
	public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
	{
		T l = (T) new Lambda(_innerEnvironment, _formalParameters, _bodyArguments);
		l.setName(_name);
		return l;
	}
	
	public void resetFunctionTemplate()
	{
		_lastFunctionReturn = null;
		_instructionPointer = 0;
		_argumentInstructionPointer = 0;
		_processedArgs = false;
		_variableArgs = new LinkedList<Value>();
		_previousKeyArgumentName = null;
		_argumentKeyMap = new HashMap<String, Value>();
	}
	
	public Value evaluate(Environment env, boolean resume) throws InstantiationException, IllegalAccessException
	{
		if (!resume)
			resetFunctionTemplate();
		Value parameterValue;
		String name;
		if (!_processedArgs)
		still_binding_arguments_for_function:{
			if (resume && _lastFunctionReturn.getContinuingFunction() != null)
			only_true_if_resuming:{
				
				if (_appendToVargs)
				{
					parameterValue = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
					if (parameterValue.isContinuation())
						return continuationReturn(parameterValue);
					if (parameterValue.isReturn() || parameterValue.isBreak() || parameterValue.isSignal() || parameterValue.isSignalOut())
						return resetReturn(parameterValue);
					if (_previousKeyArgumentName != null)
					{
						_argumentKeyMap.put(_previousKeyArgumentName, parameterValue);
						_previousKeyArgumentName = null;
					}
					else if (parameterValue.isKeyName())
					{
						_previousKeyArgumentName = parameterValue.getString();
					}
					else
						_variableArgs.add(parameterValue);
					_argumentInstructionPointer++;
				}
				else
				{
					name = _formalParameters[_argumentInstructionPointer];
					if (name.equals(_VAR_ARGNAME))
					{
						_appendToVargs = true;
						assert _argumentInstructionPointer == _formalParameters.length - 1;
					}
					if (_argumentInstructionPointer < _actualParameters.length)
					{
						parameterValue = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
						if (parameterValue.isContinuation())
							return continuationReturn(parameterValue);
						if (parameterValue.isReturn() || parameterValue.isBreak())
						{
							return resetReturn(parameterValue);
						}
						if (parameterValue.isSignal() || parameterValue.isSignalOut())
							return resetReturn(parameterValue);
						
						
						if (_appendToVargs)
						{
							if (parameterValue.isKeyName())
								_previousKeyArgumentName = parameterValue.getString();
							else
							{
								_previousKeyArgumentName = null;
								_variableArgs.add(parameterValue);
							}
						}
						else
							_innerEnvironment.mapValue(name, parameterValue);
						_argumentInstructionPointer++;
					}
					else
					{
						_innerEnvironment.mapValue(name, Environment.getNull());
					}
				}
			}
			
			
			for (;_argumentInstructionPointer<_formalParameters.length;_argumentInstructionPointer++)
			{
				name = _formalParameters[_argumentInstructionPointer];
				if (name.equals(_VAR_ARGNAME))
				{
					_appendToVargs = true;
					assert _argumentInstructionPointer == _formalParameters.length - 1;
				}
				if (_argumentInstructionPointer < _actualParameters.length)
				{
					parameterValue = _lastFunctionReturn = env.evaluate(_actualParameters[_argumentInstructionPointer]);
					if (parameterValue.isContinuation())
						return continuationReturn(parameterValue);
					if (parameterValue.isReturn() || parameterValue.isBreak())
					{
						
						return resetReturn(parameterValue.setBreak(false).setReturn(false));
					}
					if (parameterValue.isSignal() || parameterValue.isSignalOut())
						return resetReturn(parameterValue);
					
					if (_appendToVargs)
					{
						if (parameterValue.isKeyName())
						{
							_previousKeyArgumentName = parameterValue.getString();
						}
						else
							_variableArgs.add(parameterValue);
						
					}
					else
						_innerEnvironment.mapValue(name, parameterValue);
				}
				else
				missing_actual_parameters:{
					_innerEnvironment.mapValue(name, Environment.getNull());
				}
			}
			
			if (_appendToVargs)
			{
				for (;_argumentInstructionPointer<_actualParameters.length;_argumentInstructionPointer++)
				{
					parameterValue = _lastFunctionReturn = env.evaluate(_actualParameters[_argumentInstructionPointer]);
					if (parameterValue.isContinuation())
						return continuationReturn(parameterValue);
					if (parameterValue.isReturn() || parameterValue.isBreak() || parameterValue.isSignal() || parameterValue.isSignalOut())
						return resetReturn(parameterValue);
					if (_previousKeyArgumentName != null)
					{
						_argumentKeyMap.put(_previousKeyArgumentName, parameterValue);
						_previousKeyArgumentName = null;
					}
					else if (parameterValue.isKeyName())
					{
						_previousKeyArgumentName = parameterValue.getString();
					}
					else
						_variableArgs.add(parameterValue);
				}
				_innerEnvironment.mapValue(_VAR_ARGNAME, new ListValue(_variableArgs.toArray(new Value[0])));
				_innerEnvironment.mapValue(_KEY_VALUE_MAP_VAR_NAME, new StringHashtableValue(_argumentKeyMap));
			}
			_innerEnvironment.mapValue(_THIS_VAR_NAME, new LambdaValue(this));
			
			_instructionPointer = 0;
			_processedArgs = true;
		}
		
		
		
		Value result = Environment.getNull();
		for (;_instructionPointer<_bodyArguments.length;_instructionPointer++)
		{
			result = _lastFunctionReturn = _innerEnvironment.evaluate(_bodyArguments[_instructionPointer]);
			if (result.isReturn())
			{
				result.setReturn(false);
				return resetReturn(result);
			}
			if (result.isSignal() || result.isSignalOut())
				return resetReturn(result);
			if (result.isContinuation())
				return continuationReturn(result);
		}
		if (result.isBreak())
			result.setBreak(false);
		return resetReturn(result);
	}
	
	
}