package com.evolved.automata.lisp;

import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.LinkedList;





public class Lambda extends FunctionTemplate {
	final String[] _formalParameters;
	final Environment _innerEnvironment;
	final Environment _argEnv;
	final Value[] _bodyArguments;
	FunctionTemplate[] _cachedFunctionList = null;
	
	boolean _appendToVargs = false;
	static final String _THIS_VAR_NAME = "this";
	static final String _KEY_VALUE_MAP_VAR_NAME = "key-map";
	boolean _processedArgs = false;
	LinkedList<Value> _variableArgs = null;
	String _previousKeyArgumentName = null;
	HashMap<String, Value> _argumentKeyMap = null;
	Value[] _executed;
    // TOOD: Replace this recurseCount mechanism eventually
    static int recurseCount = 0;


    public static final int RECURSE_LIMIT = 10000;
    static final String CYCLIC_REFERENCE_EXCEPTION_BASE_MESSAGE = "Recursive serialization limit exceeded.  Do you have cyclic object references in your local variables? \n";

	
	public Lambda(Environment innerEnv, String[] formalParameters, Value[] bodyArgs)
	{
		assert innerEnv!=null;
		assert formalParameters != null;
		assert bodyArgs != null;
		_argEnv = new Environment(innerEnv);
		_innerEnvironment = new Environment(_argEnv);
		_formalParameters = formalParameters;
		_bodyArguments = bodyArgs;
		_previousKeyArgumentName = null;
		_argumentKeyMap = new HashMap<String, Value>();
		_appendToVargs = false;
	}
	
	@Override
	public String serialize()
	{

		if (hasNameP())
		{
			return serializeAsNamedFunction();
		}
		else
		{
			if (isObject())
			{
                return serializeAsObject();
			}
			else
			{
                return serializeAsSimpleLambdaNoClosure();
			}

		}

	}

    String serializeAsSimpleLambdaNoClosure()
    {
        String innerfunctions = getInnerFunctions();
        StringBuilder sBuilder = null;
        sBuilder = new StringBuilder("(lambda ");
        sBuilder.append(getParameterListString());
        sBuilder.append("\n");
        sBuilder.append(innerfunctions);
        if (innerfunctions.length()>0)
            sBuilder.append("\n");
        sBuilder.append(getBodyListString(" "));
        sBuilder.append(") \n");
        return sBuilder.toString();
    }

    String serializeAsNamedFunction()
    {
        if (hasNameP())
        {
            return "(#' " + _name + ")";
        }
        else
            return serialize();
    }

    /**
     * Call this from the top level method that requests serialization
     */
    static void resetRecursionLimit()
    {
        recurseCount = 0;
    }

	public String serializeAsObject()
	{
        String innerfunctions = getInnerFunctions();
		StringBuilder sBuilder = null;
		String local;
        if (isObject())
        {
            local = getLocalVariableValues();
            sBuilder = new StringBuilder("(with* (_make-serialized-base-object_) ");
            sBuilder.append("\n");
            sBuilder.append(innerfunctions);
            if (innerfunctions.length()>0)
                sBuilder.append("\n");
            sBuilder.append(local).append(")");
            return sBuilder.toString();
        }
        else
        {
            return serialize();
        }

	}

    private boolean isObject()
    {
        return _bodyArguments!=null && _bodyArguments.length == 1 && _bodyArguments[0].isIdentifier() && "this".equals(_bodyArguments[0].getString());
    }
	
	@Override
	public String toString()
	{

		if (hasNameP())
		{
			return "(#' " + _name + ")";
		}
		else
		{
			String serialized = serialize();
			try {

				return "(# " + ExtendedFunctions.getSha1Sum(serialized) + ")";
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
				throw new RuntimeException(e);
			}
			
		}

	}
	
	
	public String getInnerFunctions (){
		
		
		StringBuilder serialized = new StringBuilder();
		
		for (String key: _innerEnvironment._functionMap.keySet())
		{
			serialized.append(" \n").append(getInnerFunctionDefinition(key));
		}
		return serialized.toString();
	}
	
	private String getInnerFunctionDefinition(String name){
		
		FunctionTemplate t = _innerEnvironment._functionMap.get(name);
		Lambda l = (Lambda)t;
		StringBuilder sBuilder = new StringBuilder("(defun " + name + " ");
		
		sBuilder.append(l.getParameterListString());
		sBuilder.append(l.getLocalVariableValues());
		sBuilder.append(" ");
		sBuilder.append(l.getBodyListString(" "));
		sBuilder.append(")");
		
		return sBuilder.toString();
	}



	private String getLocalVariableValues() {
		if (_innerEnvironment._valueMap.size()== 0)
			return "";


        // reset this recurseCount at the top level call (currently this is done in serialize function
        // in ExtendedFunctions.  Ultimately, this mechanism needs to be improved and replaced
        // by a Map<Lambda> argument that is passed to serialize.  This would require changing
        // serialize in all Value classes, unfortunately
        recurseCount++;
        if (recurseCount > RECURSE_LIMIT)
        {

            throw new RuntimeException(CYCLIC_REFERENCE_EXCEPTION_BASE_MESSAGE);

        }
		StringBuilder binding = new StringBuilder("\n(multiple-bind ("), values = new StringBuilder("(list ");
		Value check;
		boolean first = true;
		for (String name: _innerEnvironment._valueMap.keySet()){
			if (name.equals("this") || !_innerEnvironment._valueMap.get(name).isSerializable())
				continue;
			if (first) {
				binding.append(name);
				first =false;
				check = _innerEnvironment._valueMap.get(name);
				if (check.isLambda() && check.getLambda() == this)
					values.append("this");
				else
					values.append(check.serializedForm());
			}
			else
			{
				binding.append(" ").append(name);
				check = _innerEnvironment._valueMap.get(name);
                boolean isLambda = check.isLambda();
                FunctionTemplate lambda = check.getLambda();
                Lambda me = this;
				if (isLambda && me == lambda)
					values.append(" ").append("this");
				else
					values.append(" ").append(check.serializedForm());
			}
		}
		binding.append(") ").toString();
		values.append("))");
		return binding.append(values).toString();
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
		T l = (T) new Lambda(_innerEnvironment.getParent().getParent(), _formalParameters, (Value[])_bodyArguments.clone());
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
		_executed = new Value[_bodyArguments.length];
	}
	
	public Value evaluate(Environment env, boolean resume) throws InstantiationException, IllegalAccessException
	{
		if (!resume)
			resetFunctionTemplate();
		Value parameterValue;
		String name;
		if (!_processedArgs)
		still_binding_arguments_for_function:{
			if (resume && _lastFunctionReturn == null)
			{
				System.out.println("bad");
			}
			if (resume && _lastFunctionReturn != null && _lastFunctionReturn.getContinuingFunction() != null)
			only_true_if_resuming:{
				
				if (_appendToVargs)
				{
					parameterValue = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, true);
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
						parameterValue = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, true);
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
							_argEnv.mapValue(name, parameterValue);
						_argumentInstructionPointer++;
					}
					else
					{
						_argEnv.mapValue(name, Environment.getNull());
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
						_argEnv.mapValue(name, parameterValue);
				}
				else
				missing_actual_parameters:{
					_argEnv.mapValue(name, Environment.getNull());
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
				_argEnv.mapValue(_VAR_ARGNAME, new ListValue(_variableArgs.toArray(new Value[0])));
				_argEnv.mapValue(_KEY_VALUE_MAP_VAR_NAME, new StringHashtableValue(_argumentKeyMap));
			}
			_argEnv.mapValue(_THIS_VAR_NAME, new LambdaValue(this));
			
			_instructionPointer = 0;
			_processedArgs = true;
		}
		
		_argEnv.mapValue("this", new LambdaValue(this));
		if (hasNameP())
			_argEnv.mapValue("this-name", NLispTools.makeValue(getName()));
		else
			_argEnv.mapValue("this-name", Environment.getNull());
		
		Value result = Environment.getNull();
		for (;_instructionPointer<_bodyArguments.length;_instructionPointer++)
		{
			if (Environment._DEBUG) 
			{
				String log = Thread.currentThread().getName() + " - [" + ((_name!=null)?_name:"null") + " - raw instance " + this.hashCode() + "][logical instance - " + this.toString() + "]:" + _instructionPointer + " " + _bodyArguments[_instructionPointer];
				Environment.log(log);
			}
			if (resume && _lastFunctionReturn.getContinuingFunction() != null)
				result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_innerEnvironment, true);
			else
			{
				result = _lastFunctionReturn = _innerEnvironment.evaluate(_bodyArguments[_instructionPointer]);
			}
			_executed[_instructionPointer] = result;
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
