package com.evolved.automata.lisp;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.UUID;

import com.evolved.automata.KeyValuePair;



public class NLispTools 
{
	public static final Class stackTraceClass = FunctionTemplate.class;
	public static final String _signalKeyBindingName = "signal-key";
	
	public static LinkedList<String> getLispStackTrace() throws ClassNotFoundException
	{
		StackTraceElement[] stack = Thread.currentThread().getStackTrace();
		int offset = 0;
		
		StackTraceElement selement;
		LinkedList<String> elementList = new LinkedList<String>();
		Class c;
		for (int i=offset;i<stack.length;i++)
		{
			selement = stack[i];
			c = Class.forName(selement.getClassName());
			while (c!=Object.class)
			{
				if (c.equals(stackTraceClass))
				{
					elementList.add(selement.getClassName());
					break;
				}
			}
			
//			element[0] = Environment.makeAtom(selement.getClassName());
//			element[1] = Environment.makeAtom(selement.getMethodName());
//			element[2] = Environment.makeAtom(selement.getLineNumber());
//			element[3] = Environment.makeAtom(selement.getFileName());
//			
//			strace[i-offset] = new Argument(null, null, element);
			
		}
		return elementList;
	}
	
	public static SimpleFunctionTemplate format(final Environment env)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, true, true);
				String formatString = evaluatedArgs[0].getString();
				int argLength = evaluatedArgs.length - 1;
				Object[] remaining = new Object[argLength];
				for (int i=0;i < argLength;i++)
					remaining[i] = (evaluatedArgs[i+1].isString())?evaluatedArgs[i+1].getString():evaluatedArgs[i+1].toString();
				String out = String.format(formatString, remaining);
				return NLispTools.makeValue(out);
			}
		};
	}
	
	public static Environment addDefaultFunctionsAddMacros(Environment env) throws InstantiationException, IllegalAccessException
	{
		
		// Special Functions
		env.mapFunction("defun", new DefunFunctionTemplate());
		env.mapFunction("defmacro", new DefMacroFunctionTemplate());
		env.mapFunction("and", new AndFunctionTemplate());
		env.mapFunction("or", new OrFunctionTemplate());
		env.mapFunction("if", new IfFunctionTemplate());
		
		// Define if as a macro
		//env.evaluate("(defmacro if (cond ant else) (list 'or (list 'and (list 'setq 'xx*xx cond) ant) (list 'and (list 'not 'xx*xx) else)))", true);
		//env.evaluate("(defmacro if-2 (cond ant else) (setq x (gensym)) `(or (and (setq ,x ,cond) ,ant) (and (not ,x) ,else)))", true);
		// (or (and (setq xx*xx 1) "yes") (and (not xx*xx) "no"))
		// (defmacro if (cond ant else) `(or ,`(and ,`(setq xx*xx ,cond) ,ant) ,`(and (not xx*xx) ,else)))   
		env.mapFunction("macro-expand", new FunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(1, true, true);
				
				String name = _actualParameters[0].getString();
				
				Value[] actual = new Value[_actualParameters.length-1];
				for (int i=0;i<actual.length;i++)
					actual[i] = _actualParameters[i+1];
				MacroTemplate mt = env.getMacro(name);
				if (mt!=null)
				{
					mt.setActualParameters(actual);
					return mt.evaluate(env, false);
				}
				else
					return Environment.getNull();
			}
			
			
		}
		);
		
		env.mapFunction("quote", new FunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(1, false, false);
				
				return _actualParameters[0];
			}
		}
		);
		
		env.mapFunction("eval", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				
				try
				{
					return env.evaluate(evaluatedArgs[0]);
				}
				catch (Exception e)
				{
					throw new RuntimeException(e.getMessage());
				}
			}
		}
		);
		
		env.mapFunction("make-id", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				
				if (evaluatedArgs[0].isString())
					return new StringValue(evaluatedArgs[0].getString(), true);
				else
					throw new RuntimeException("Argument to 'make-id' must be a string");
			}
		}
		);
		
		
		env.mapFunction("break", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(0, true, true);
				if (_numArguments == 0)
					return Environment.getNull().setBreak(true);
				return evaluatedArgs[0].setBreak(true);
			}
		}
		);
		
		env.mapFunction("return", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(0, true, true);
				if (_numArguments == 0)
					return Environment.getNull().setReturn(true);
				return evaluatedArgs[0].setReturn(true);
			}
		}
		);
		
		
		env.mapFunction("signal", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				Value value;
				if (_numArguments == 1)
					value = Environment.getNull();
				else
					value = evaluatedArgs[1];
				return new SignalValue(evaluatedArgs[0], value, false);
			}
		}
		);
		
		env.mapFunction("signal-out", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				Value value;
				if (_numArguments == 1)
					value = Environment.getNull();
				else
					value = evaluatedArgs[1];
				return new SignalValue(evaluatedArgs[0], value, true);
			}
		}
		);
		
		
		env.mapFunction("back-quote", new FunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(1, false, false);
				
				if (!_actualParameters[0].isList())
					return _actualParameters[0];
				
				return processBackQuote(env, _actualParameters[0]);
			}
		}
		);
		
		env.mapFunction("lambda", new FunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(2, true, true);
				
				
				String[] formalParameters = NLispTools.getStringArrayFromValue(_actualParameters[0]);
				Value[] body = new Value[_actualParameters.length-1];
				for (int i=0;i<body.length;i++)
					body[i] = _actualParameters[i+1];
				Lambda lam = new Lambda(env, formalParameters, body);
				
				LambdaValue lv = new LambdaValue(lam);
				return lv;
			}
		}
		);
		
		env.mapFunction("lambda-p", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
					
				checkActualArguments(1, false, true);
				if (evaluatedArgs[0].isLambda())
					return evaluatedArgs[0];
				else
					return Environment.getNull();
			}
		}
		
		);
		
		
		env.mapFunction("#'", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				
				if (evaluatedArgs[0].isString())
				{
					FunctionTemplate template = null;
					try {
						template = env.getFunction(evaluatedArgs[0].getString());
					} catch (InstantiationException e) {
						
						throw new RuntimeException(e);
					} catch (IllegalAccessException e) {
						throw new RuntimeException(e);
					}
					if (template == null)
						throw new RuntimeException("Undefined function name for #': " + evaluatedArgs[0].getString());
					return new LambdaValue(template);
				}
				else
					throw new RuntimeException("Invalid argument type for #': " + evaluatedArgs[0]);
				
			}
			
		}
		);
		
		
		env.mapFunction("apply", new FunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(1, true, true);
				
				Value functionSpec = env.evaluate(_actualParameters[0]);
				FunctionTemplate f = null;
				if (!functionSpec.isLambda())
				{
					if (functionSpec.isString())
					{
						
						try {
							f = env.getFunction(functionSpec.getString());
						} catch (InstantiationException e) {
							
							throw new RuntimeException(e);
						} catch (IllegalAccessException e) {
							throw new RuntimeException(e);
						}
						if (f == null)
							throw new RuntimeException("Undefined function name for apply: " + functionSpec.getString());
					}
					else
						throw new RuntimeException("First argument to apply must be a FunctionTemplate or the name of a function: " + functionSpec);
				}
				else
					f = functionSpec.getLambda();
				
				Value v = makeValue(new Value[0]);
				if (_actualParameters.length > 1)
					v = env.evaluate(_actualParameters[1]);
				if (!v.isList())
					throw new RuntimeException("Second argument to apply must be a list");
				
				f.setActualParameters(v.getList());
				
				return f.evaluate(env, resume);
			}
			
			
		}
		);
		
		env.mapFunction("apply-method", new FunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(2, true, true);
				
				Value functionSpec = env.evaluate(_actualParameters[1]);
				Value object = env.evaluate(_actualParameters[0]);
				FunctionTemplate f = null;
				if (!functionSpec.isLambda())
				{
					if (functionSpec.isString())
					{
						
						try {
							if (!object.isNull())
							{
								Lambda l = (Lambda)object.getLambda();
								f = l.getInnerEnvironment().getFunction(functionSpec.getString());
							}
							else
								f = env.getFunction(functionSpec.getString());
						} catch (InstantiationException e) {
							
							throw new RuntimeException(e);
						} catch (IllegalAccessException e) {
							throw new RuntimeException(e);
						}
						if (f == null)
							throw new RuntimeException("Undefined function name for apply: " + functionSpec.getString());
					}
					else
						throw new RuntimeException("First argument to apply must be a FunctionTemplate or the name of a function: " + functionSpec);
				}
				else
					f = functionSpec.getLambda();
				
				Value v = makeValue(new Value[0]);
				if (_actualParameters.length > 2)
					v = env.evaluate(_actualParameters[2]);
				if (!v.isList())
					throw new RuntimeException("Second argument to apply must be a list");
				
				f.setActualParameters(v.getList());
				
				return f.evaluate(env, resume);
			}
			
			
		}
		);
		
		env.mapFunction("funcall", new FunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(1, true, true);
				
				Value functionSpec = env.evaluate(_actualParameters[0]);
				FunctionTemplate f = null;
				if (!functionSpec.isLambda())
				{
					if (functionSpec.isString())
					{
						
						try {
							f = env.getFunction(functionSpec.getString());
						} catch (InstantiationException e) {
							
							throw new RuntimeException(e);
						} catch (IllegalAccessException e) {
							throw new RuntimeException(e);
						}
						if (f == null)
							throw new RuntimeException("Undefined function name for apply: " + functionSpec.getString());
					}
					else
						throw new RuntimeException("First argument to apply must be a FunctionTemplate or the name of a function: " + functionSpec);
				}
				else
					f = functionSpec.getLambda();
				
				Value[] remaining = new Value[_actualParameters.length - 1];
				for (int i = 1;i<_actualParameters.length;i++)
					remaining[i - 1] = _actualParameters[i];
				
				f.setActualParameters(remaining);
				
				return f.evaluate(env, resume);
			}
			
			
		}
		);
		
		
		env.mapFunction("method-call", new FunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(2, true, true);
				
				Value functionSpec = env.evaluate(_actualParameters[1]);
				Value object = env.evaluate(_actualParameters[0]); 
				FunctionTemplate f = null;
				if (!functionSpec.isLambda())
				{
					if (functionSpec.isString())
					{
						
						try {
							if (!object.isNull())
							{
								Lambda l = (Lambda)object.getLambda();
								f = l.getInnerEnvironment().getFunction(functionSpec.getString());
							}
							else
								f = env.getFunction(functionSpec.getString());
						} catch (InstantiationException e) {
							
							throw new RuntimeException(e);
						} catch (IllegalAccessException e) {
							throw new RuntimeException(e);
						}
						if (f == null)
							throw new RuntimeException("Undefined function name for apply: " + functionSpec.getString());
					}
					else
						throw new RuntimeException("First argument to apply must be a FunctionTemplate or the name of a function: " + functionSpec);
				}
				else
					f = functionSpec.getLambda();
				
				Value[] remaining = new Value[_actualParameters.length - 2];
				for (int i = 2;i<_actualParameters.length;i++)
					remaining[i - 2] = _actualParameters[i];
				
				f.setActualParameters(remaining);
				
				return f.evaluate(env, resume);
			}
			
			
		}
		);
		
		
		// -~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//  Functions required for minimal functionality
		// -~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		
		env.mapFunction("gensym", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(0, false, true);
				StringValue s = new StringValue(java.util.UUID.randomUUID().toString(), true);
				
				return s;
				
			}
			
		}
		);
		
		env.mapFunction("not", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				
				if (evaluatedArgs[0].isNull())
					return new IntegerValue(1);
				else
					return Environment.getNull();
				
			}
			
		}
		);
		
		env.mapFunction("=", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				if (isNumericType(evaluatedArgs[0]) && isNumericType(evaluatedArgs[1]))
				{
					if (evaluatedArgs[0].getFloatValue() == evaluatedArgs[1].getFloatValue())
						return evaluatedArgs[1];
					else
						return makeValue(false);
				}
				else throw new RuntimeException("== requires numeric argument types");
				
			}
			
		}
		);
		
		env.mapFunction("equals", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				return makeValue(evaluatedArgs[0].equals(evaluatedArgs[1]));
				
			}
			
		}
		);
		
		
		env.mapFunction("setq", new FunctionTemplate()
		{

			
			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(2, true, true);
				
				if (_actualParameters[0].isIdentifier())
				{
					if (resume && _lastFunctionReturn.getContinuingFunction()!=null)
						_lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
					else
						_lastFunctionReturn = env.evaluate(_actualParameters[1], false);
					if (_lastFunctionReturn.isContinuation())
						return continuationReturn(_lastFunctionReturn);
					
					if (_lastFunctionReturn.isReturn() || _lastFunctionReturn.isBreak() || _lastFunctionReturn.isSignal() || _lastFunctionReturn.isSignalOut())
						return resetReturn(_lastFunctionReturn);
					
					return resetReturn(env.mapValue(_actualParameters[0].getString(), _lastFunctionReturn));
				}
				else
					throw new RuntimeException("Invalid argument error: first parameter to setq must be an identifier");
				
			}
		}
		);
		
		env.mapFunction("set", new FunctionTemplate()
		{

			Environment _target = null;
			
			@Override
			public void resetFunctionTemplate()
			{
				_lastFunctionReturn = null;
				_target = null;
			}
			
			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(2, true, true);
				
				if (_actualParameters[0].isIdentifier())
				{
					String targetName = _actualParameters[0].getString();
					if (_target == null)
					{
						_target = env;
						while (_target!=null && !_target.hasVariable(targetName)){
							_target = _target.getParent();
						}
						
						if (_target==null)
							_target = env;
					}
					
					
					
					if (resume && _lastFunctionReturn.getContinuingFunction()!=null)
						_lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
					else
						_lastFunctionReturn = env.evaluate(_actualParameters[1]);
					if (_lastFunctionReturn.isContinuation())
						return continuationReturn(_lastFunctionReturn);
					
					if (_lastFunctionReturn.isReturn() || _lastFunctionReturn.isBreak() || _lastFunctionReturn.isSignal() || _lastFunctionReturn.isSignalOut())
						return resetReturn(_lastFunctionReturn);
					
					return resetReturn(_target.mapValue(_actualParameters[0].getString(), _lastFunctionReturn));
						
				}
				else
					throw new RuntimeException("Invalid argument error: first parameter to setq must be an identifier");
				
			}
		}
		);
		
		env.mapFunction("while", new FunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(1, true, true);
				
				if (!resume)
					resetFunctionTemplate();
				
				Value result = Environment.getNull();
				while (true)
				{
					for (;_instructionPointer<_actualParameters.length;_instructionPointer++)
					{
						if (resume && _lastFunctionReturn.getContinuingFunction() != null)
							result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
						else
							result = _lastFunctionReturn = env.evaluate(_actualParameters[_instructionPointer], false);
						
						if (result.isContinuation())
							return continuationReturn(result);
						if ((_instructionPointer == 0 && result.isNull()) || result.isReturn() || result.isSignal() || result.isSignalOut())
							return resetReturn(result);
						if (result.isBreak())
							return resetReturn(result.setBreak(false));
					}
					
					_instructionPointer = 0;
				}
				
			}
		}
		);
		
		env.mapFunction("for", new FunctionTemplate()
		{

			
			final int EVALUATING_LIST = 0;
			final int BINDING_VARIABLES = 1;
			final int EVALUATING_EXP = 2;
			int _currentState = EVALUATING_LIST;
			long _loopIndex = 0;
			long _maxIndex = 0;
			String _bindingLoopVariable = null;
			String _bindingLoopIndex = null;
			Value[] _loopList = null;
			boolean _isListIteration = true;
			Environment _innerEnv = null;
			public void resetFunctionTemplate()
			{
				_currentState = EVALUATING_LIST;
				_lastFunctionReturn = null;
				_instructionPointer = 0;
				_loopIndex = 0;
				_bindingLoopVariable = null;
				_bindingLoopIndex = null;
				_loopList = null;
				_maxIndex = 0;
				_isListIteration = true;
				_innerEnv = null;
			}
			
			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(1, true, true);
				
				if (!resume)
					resetFunctionTemplate();
				
				Value result = Environment.getNull();
				if (_innerEnv == null)
					_innerEnv = new Environment(env);
				while (true)
				{
					switch (_currentState)
					{
						case EVALUATING_LIST:
							
							if (resume && _lastFunctionReturn.getContinuingFunction() != null)
								result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_innerEnv, resume);
							else
								result = _lastFunctionReturn = _innerEnv.evaluate(_actualParameters[1], false);
							if (result.isContinuation())
								return continuationReturn(result);
							if (result.isReturn() || result.isSignal() || result.isSignalOut())
								return resetReturn(result);
							if (result.isBreak())
								return resetReturn(result.setBreak(false));
							
							if (result.isList())
							{
								_loopList = result.getList();
								_isListIteration = true;
								_maxIndex = _loopList.length;
							}
							else if (NLispTools.isNumericType(result) && result.getIntValue()>=0)
							{
								_maxIndex = result.getIntValue();
								_isListIteration = false;
							}
							else 
								throw new RuntimeException("Second argument to 'for' must be a list or a non-negative number");
							 _currentState = BINDING_VARIABLES;
							 _loopIndex = 0;
							 break;
						case BINDING_VARIABLES:
							if (_loopIndex>=_maxIndex)
							{
								if (resume && _lastFunctionReturn.getContinuingFunction() != null)
									result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_innerEnv, resume);
								else
									result = _lastFunctionReturn = _innerEnv.evaluate(_actualParameters[2], false);
								if (result.isContinuation())
									return continuationReturn(result);
								if (result.isBreak())
									return resetReturn(result.setBreak(false));
								else
									return resetReturn(result);
							}
							if (_isListIteration)
							{
								
								if (_actualParameters[0].isList())
								{
									if (_bindingLoopVariable == null)
										_bindingLoopVariable = _actualParameters[0].getList()[0].getString();
									_innerEnv.mapValue(_bindingLoopVariable, _loopList[(int)_loopIndex]);
									
									if (_actualParameters[0].getList().length>1)
									{
										if (_bindingLoopIndex == null)
											_bindingLoopIndex = _actualParameters[0].getList()[1].getString();
										_innerEnv.mapValue(_bindingLoopIndex, makeValue(_loopIndex));
									}
									
								}
								else
								{
									_bindingLoopVariable = _actualParameters[0].getString();
									_innerEnv.mapValue(_bindingLoopVariable, _loopList[(int)_loopIndex]);
								}
								
							}
							else
							{
								_bindingLoopVariable = _actualParameters[0].getString();
								_innerEnv.mapValue(_bindingLoopVariable, makeValue(_loopIndex));
							}
							_loopIndex++;
							_currentState = EVALUATING_EXP;
							break;
						case EVALUATING_EXP:
							if (resume && _lastFunctionReturn.getContinuingFunction() != null)
								result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_innerEnv, resume);
							else
								result = _lastFunctionReturn = _innerEnv.evaluate(_actualParameters[3], false);
							if (result.isContinuation())
								return continuationReturn(result);
							if (result.isReturn() || result.isSignal() || result.isSignalOut())
								return resetReturn(result);
							if (result.isBreak())
								return resetReturn(result.setBreak(false));
							_currentState = BINDING_VARIABLES;
							break;
					}
					
				}
				
			}
		}
		);
		
		// (binding-variable index-variable) 
		// {iteration-list}
		// minimizing expression
		// minimized output
		
		env.mapFunction("minimal-value-map", new FunctionTemplate()
		{

			
			final int EVALUATING_LIST = 0;
			final int BINDING_VARIABLES = 1;
			final int EVALUATING_EXP = 2;
			int _currentState = EVALUATING_LIST;
			long _loopIndex = 0;
			long _maxIndex = 0;
			String _bindingLoopVariable = null;
			String _bindingLoopIndex = null;
			Value[] _loopList = null;
			boolean _isListIteration = true;
			Environment _innerEnv = null;
			double _extremumValue = 0;
			Value _minimizingValue;
			int _minimizingIndex;
			public void resetFunctionTemplate()
			{
				_currentState = EVALUATING_LIST;
				_lastFunctionReturn = null;
				_instructionPointer = 0;
				_loopIndex = 0;
				_bindingLoopVariable = null;
				_bindingLoopIndex = null;
				_loopList = null;
				_maxIndex = 0;
				_isListIteration = true;
				_minimizingIndex = 0;
				_innerEnv = null;
			}
			
			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(1, true, true);
				
				if (!resume)
					resetFunctionTemplate();
				
				Value result = Environment.getNull();
				if (_innerEnv == null)
					_innerEnv = new Environment(env);
				while (true)
				{
					switch (_currentState)
					{
						case EVALUATING_LIST:
							
							if (resume && _lastFunctionReturn.getContinuingFunction() != null)
								result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_innerEnv, resume);
							else
								result = _lastFunctionReturn = _innerEnv.evaluate(_actualParameters[1], false);
							if (result.isContinuation())
								return continuationReturn(result);
							if (result.isReturn() || result.isSignal() || result.isSignalOut())
								return resetReturn(result);
							if (result.isBreak())
								return resetReturn(result.setBreak(false));
							
							if (result.isList())
							{
								_loopList = result.getList();
								_isListIteration = true;
								_maxIndex = _loopList.length;
							}
							else if (NLispTools.isNumericType(result) && result.getIntValue()>=0)
							{
								_maxIndex = result.getIntValue();
								_isListIteration = false;
							}
							else 
								throw new RuntimeException("Second argument to 'for' must be a list or a non-negative number");
							 _currentState = BINDING_VARIABLES;
							 _loopIndex = 0;
							 break;
						case BINDING_VARIABLES:
							if (_loopIndex>=_maxIndex)
							{
								if (_isListIteration)
								{
									
									if (_actualParameters[0].isList())
									{
										if (_bindingLoopVariable == null)
											_bindingLoopVariable = _actualParameters[0].getList()[0].getString();
										_innerEnv.mapValue(_bindingLoopVariable, _loopList[(int)_minimizingIndex]);
										
										if (_actualParameters[0].getList().length>1)
										{
											if (_bindingLoopIndex == null)
												_bindingLoopIndex = _actualParameters[0].getList()[1].getString();
											_innerEnv.mapValue(_bindingLoopIndex, makeValue(_minimizingIndex));
										}
										
									}
									else
									{
										_bindingLoopVariable = _actualParameters[0].getString();
										_innerEnv.mapValue(_bindingLoopVariable, _loopList[(int)_minimizingIndex]);
									}
									
								}
								else
								{
									_bindingLoopVariable = _actualParameters[0].getString();
									_innerEnv.mapValue(_bindingLoopVariable, makeValue(_loopIndex));
								}
								
								if (resume && _lastFunctionReturn.getContinuingFunction() != null)
									result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_innerEnv, resume);
								else
									result = _lastFunctionReturn = _innerEnv.evaluate(_actualParameters[3], false);
								if (result.isContinuation())
									return continuationReturn(result);
								if (result.isBreak())
									return resetReturn(result.setBreak(false));
								else
									return resetReturn(result);
							}
							if (_isListIteration)
							{
								
								if (_actualParameters[0].isList())
								{
									if (_bindingLoopVariable == null)
										_bindingLoopVariable = _actualParameters[0].getList()[0].getString();
									_innerEnv.mapValue(_bindingLoopVariable, _loopList[(int)_loopIndex]);
									
									if (_actualParameters[0].getList().length>1)
									{
										if (_bindingLoopIndex == null)
											_bindingLoopIndex = _actualParameters[0].getList()[1].getString();
										_innerEnv.mapValue(_bindingLoopIndex, makeValue(_loopIndex));
									}
									
								}
								else
								{
									_bindingLoopVariable = _actualParameters[0].getString();
									_innerEnv.mapValue(_bindingLoopVariable, _loopList[(int)_loopIndex]);
								}
								
							}
							else
							{
								_bindingLoopVariable = _actualParameters[0].getString();
								_innerEnv.mapValue(_bindingLoopVariable, makeValue(_loopIndex));
							}
							_loopIndex++;
							_currentState = EVALUATING_EXP;
							break;
						case EVALUATING_EXP:
							if (resume && _lastFunctionReturn.getContinuingFunction() != null)
								result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_innerEnv, resume);
							else
								result = _lastFunctionReturn = _innerEnv.evaluate(_actualParameters[2], false);
							if (result.isContinuation())
								return continuationReturn(result);
							if (result.isReturn() || result.isSignal() || result.isSignalOut())
								return resetReturn(result);
							if (result.isBreak())
								return resetReturn(result.setBreak(false));
							double newValue = result.getFloatValue();
							if ((_loopIndex - 1) == 0 || newValue < _extremumValue)
							{
								_extremumValue = newValue;
								_minimizingIndex = (int)(_loopIndex - 1);
							}
							_currentState = BINDING_VARIABLES;
							break;
					}
					
				}
				
			}
		}
		);
		
		// (binding-variable index-variable) 
		// {iteration-list}
		// maximizing expression
		// maximized output
		env.mapFunction("maximal-value-map", new FunctionTemplate()
		{

			final int EVALUATING_LIST = 0;
			final int BINDING_VARIABLES = 1;
			final int EVALUATING_EXP = 2;
			int _currentState = EVALUATING_LIST;
			long _loopIndex = 0;
			long _maxIndex = 0;
			String _bindingLoopVariable = null;
			String _bindingLoopIndex = null;
			Value[] _loopList = null;
			boolean _isListIteration = true;
			Environment _innerEnv = null;
			double _extremumValue = 0;
			
			int _minimizingIndex;
			public void resetFunctionTemplate()
			{
				_currentState = EVALUATING_LIST;
				_lastFunctionReturn = null;
				_instructionPointer = 0;
				_loopIndex = 0;
				_bindingLoopVariable = null;
				_bindingLoopIndex = null;
				_loopList = null;
				_maxIndex = 0;
				_isListIteration = true;
				_minimizingIndex = 0;
				_innerEnv = null;
			}
			
			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(1, true, true);
				
				if (!resume)
					resetFunctionTemplate();
				
				Value result = Environment.getNull();
				if (_innerEnv == null)
					_innerEnv = new Environment(env);
				while (true)
				{
					switch (_currentState)
					{
						case EVALUATING_LIST:
							
							if (resume && _lastFunctionReturn.getContinuingFunction() != null)
								result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_innerEnv, resume);
							else
								result = _lastFunctionReturn = _innerEnv.evaluate(_actualParameters[1], false);
							if (result.isContinuation())
								return continuationReturn(result);
							if (result.isReturn() || result.isSignal() || result.isSignalOut())
								return resetReturn(result);
							if (result.isBreak())
								return resetReturn(result.setBreak(false));
							
							if (result.isList())
							{
								_loopList = result.getList();
								_isListIteration = true;
								_maxIndex = _loopList.length;
							}
							else if (NLispTools.isNumericType(result) && result.getIntValue()>=0)
							{
								_maxIndex = result.getIntValue();
								_isListIteration = false;
							}
							else 
								throw new RuntimeException("Second argument to 'for' must be a list or a non-negative number");
							 _currentState = BINDING_VARIABLES;
							 _loopIndex = 0;
							 break;
						case BINDING_VARIABLES:
							if (_loopIndex>=_maxIndex)
							{
								if (_isListIteration)
								{
									
									if (_actualParameters[0].isList())
									{
										if (_bindingLoopVariable == null)
											_bindingLoopVariable = _actualParameters[0].getList()[0].getString();
										_innerEnv.mapValue(_bindingLoopVariable, _loopList[(int)_minimizingIndex]);
										
										if (_actualParameters[0].getList().length>1)
										{
											if (_bindingLoopIndex == null)
												_bindingLoopIndex = _actualParameters[0].getList()[1].getString();
											_innerEnv.mapValue(_bindingLoopIndex, makeValue(_minimizingIndex));
										}
										
									}
									else
									{
										_bindingLoopVariable = _actualParameters[0].getString();
										_innerEnv.mapValue(_bindingLoopVariable, _loopList[(int)_minimizingIndex]);
									}
									
								}
								else
								{
									_bindingLoopVariable = _actualParameters[0].getString();
									_innerEnv.mapValue(_bindingLoopVariable, makeValue(_loopIndex));
								}
								
								if (resume && _lastFunctionReturn.getContinuingFunction() != null)
									result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_innerEnv, resume);
								else
									result = _lastFunctionReturn = _innerEnv.evaluate(_actualParameters[3], false);
								if (result.isContinuation())
									return continuationReturn(result);
								if (result.isBreak())
									return resetReturn(result.setBreak(false));
								else
									return resetReturn(result);
							}
							if (_isListIteration)
							{
								
								if (_actualParameters[0].isList())
								{
									if (_bindingLoopVariable == null)
										_bindingLoopVariable = _actualParameters[0].getList()[0].getString();
									_innerEnv.mapValue(_bindingLoopVariable, _loopList[(int)_loopIndex]);
									
									if (_actualParameters[0].getList().length>1)
									{
										if (_bindingLoopIndex == null)
											_bindingLoopIndex = _actualParameters[0].getList()[1].getString();
										_innerEnv.mapValue(_bindingLoopIndex, makeValue(_loopIndex));
									}
									
								}
								else
								{
									_bindingLoopVariable = _actualParameters[0].getString();
									_innerEnv.mapValue(_bindingLoopVariable, _loopList[(int)_loopIndex]);
								}
								
							}
							else
							{
								_bindingLoopVariable = _actualParameters[0].getString();
								_innerEnv.mapValue(_bindingLoopVariable, makeValue(_loopIndex));
							}
							_loopIndex++;
							_currentState = EVALUATING_EXP;
							break;
						case EVALUATING_EXP:
							if (resume && _lastFunctionReturn.getContinuingFunction() != null)
								result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_innerEnv, resume);
							else
								result = _lastFunctionReturn = _innerEnv.evaluate(_actualParameters[2], false);
							if (result.isContinuation())
								return continuationReturn(result);
							if (result.isReturn() || result.isSignal() || result.isSignalOut())
								return resetReturn(result);
							if (result.isBreak())
								return resetReturn(result.setBreak(false));
							double newValue = result.getFloatValue();
							if ((_loopIndex - 1) == 0 || newValue > _extremumValue)
							{
								_extremumValue = newValue;
								_minimizingIndex = (int)(_loopIndex - 1);
							}
							_currentState = BINDING_VARIABLES;
							break;
					}
					
				}
				
			}
		}
		);
		
		
		env.mapFunction("find", new FunctionTemplate()
		{
			final int EVALUATING_LIST = 0;
			final int BINDING_VARIABLES = 1;
			final int EVALUATING_EXP = 2;
			int _currentState = EVALUATING_LIST;
			long _loopIndex = 0;
			long _maxIndex = 0;
			String _bindingLoopVariable = null;
			String _bindingLoopIndex = null;
			Value[] _loopList = null;
			LinkedList<Value> _outList;
			Value _lastValue = null;
			Environment _innerEnv = null;
			public void resetFunctionTemplate()
			{
				_currentState = EVALUATING_LIST;
				_lastFunctionReturn = null;
				_instructionPointer = 0;
				_loopIndex = 0;
				_bindingLoopVariable = null;
				_bindingLoopIndex = null;
				_loopList = null;
				_maxIndex = 0;
				_outList = null;
				_innerEnv = null;
			}
			
			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(1, true, true);
				
				if (!resume)
					resetFunctionTemplate();
				
				Value result = Environment.getNull();
				if (_innerEnv == null)
					_innerEnv = new Environment(env);
				while (true)
				{
					switch (_currentState)
					{
						case EVALUATING_LIST:
							
							if (resume && _lastFunctionReturn.getContinuingFunction() != null)
								result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_innerEnv, resume);
							else
								result = _lastFunctionReturn = _innerEnv.evaluate(_actualParameters[1], false);
							if (result.isContinuation())
								return continuationReturn(result);
							if (result.isReturn() || result.isSignal() || result.isSignalOut())
								return resetReturn(result);
							if (result.isBreak())
								return resetReturn(result.setBreak(false));
							
							if (result.isList())
							{
								_loopList = result.getList();
								_outList = new LinkedList<Value>();
								_maxIndex = _loopList.length;
							}
							else 
								throw new RuntimeException("Second argument to 'find' must be a list");
							 _currentState = BINDING_VARIABLES;
							 _loopIndex = 0;
							 break;
						case BINDING_VARIABLES:
							if (_loopIndex>=_maxIndex)
							{
								return resetReturn(makeValue(_outList.toArray(new Value[0])));
							}
							if (_actualParameters[0].isList())
							{
								if (_bindingLoopVariable == null)
									_bindingLoopVariable = _actualParameters[0].getList()[0].getString();
								_lastValue = _loopList[(int)_loopIndex];
								_innerEnv.mapValue(_bindingLoopVariable, _lastValue);
								
								if (_actualParameters[0].getList().length>1)
								{
									if (_bindingLoopIndex == null)
										_bindingLoopIndex = _actualParameters[0].getList()[1].getString();
									_innerEnv.mapValue(_bindingLoopIndex, makeValue(_loopIndex));
								}
								
							}
							else
							{
								_bindingLoopVariable = _actualParameters[0].getString();
								_lastValue = _loopList[(int)_loopIndex];
								_innerEnv.mapValue(_bindingLoopVariable, _lastValue);
							}
							_loopIndex++;
							_currentState = EVALUATING_EXP;
							break;
						case EVALUATING_EXP:
							if (_actualParameters.length == 3)
							{
								if (resume && _lastFunctionReturn.getContinuingFunction() != null)
									result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_innerEnv, resume);
								else
									result = _lastFunctionReturn = _innerEnv.evaluate(_actualParameters[2], false);
								if (result.isContinuation())
									return continuationReturn(result);
								if (result.isReturn() || result.isSignal() || result.isSignalOut())
									return resetReturn(result);
								if (result.isBreak())
									return resetReturn(result.setBreak(false));
								if (!result.isNull())
									_outList.add(_lastValue);
							}
							else
							{
								if (!_lastValue.isNull())
									_outList.add(_lastValue);
							}
							
							
							_currentState = BINDING_VARIABLES;
							break;
					}
					
				}
				
			}
		}
		);
		
		
		env.mapFunction("mapcar", new FunctionTemplate()
		{

			final int EVALUATING_LIST = 0;
			final int BINDING_VARIABLES = 1;
			final int EVALUATING_EXP = 2;
			int _currentState = EVALUATING_LIST;
			long _loopIndex = 0;
			long _maxIndex = 0;
			String _bindingLoopVariable = null;
			String _bindingLoopIndex = null;
			Value[] _loopList = null;
			Value[] _outList = null;
			
			Environment _innerEnv = null;
			public void resetFunctionTemplate()
			{
				_currentState = EVALUATING_LIST;
				_lastFunctionReturn = null;
				_instructionPointer = 0;
				_loopIndex = 0;
				_bindingLoopVariable = null;
				_bindingLoopIndex = null;
				_loopList = null;
				_maxIndex = 0;
				
				_innerEnv = null;
				_outList = null;
			}
			
			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(1, true, true);
				
				if (!resume)
					resetFunctionTemplate();
				
				Value result = Environment.getNull();
				if (_innerEnv == null)
					_innerEnv = new Environment(env);
				while (true)
				{
					switch (_currentState)
					{
						case EVALUATING_LIST:
							
							if (resume && _lastFunctionReturn.getContinuingFunction() != null)
								result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_innerEnv, resume);
							else
								result = _lastFunctionReturn = _innerEnv.evaluate(_actualParameters[1], false);
							if (result.isContinuation())
								return continuationReturn(result);
							if (result.isReturn() || result.isSignal() || result.isSignalOut())
								return resetReturn(result);
							if (result.isBreak())
								return resetReturn(result.setBreak(false));
							
							if (result.isList())
							{
								_loopList = result.getList();
								
								_maxIndex = _loopList.length;
								_outList = new Value[(int)_maxIndex];
							}
							else 
								throw new RuntimeException("Second argument to 'mapcar' must be a list");
							 _currentState = BINDING_VARIABLES;
							 _loopIndex = 0;
							 break;
						case BINDING_VARIABLES:
							if (_loopIndex>=_maxIndex)
							{
								return resetReturn(makeValue(_outList));
							}
							if (_actualParameters[0].isList())
							{
								if (_bindingLoopVariable == null)
									_bindingLoopVariable = _actualParameters[0].getList()[0].getString();
								_innerEnv.mapValue(_bindingLoopVariable, _loopList[(int)_loopIndex]);
								
								if (_actualParameters[0].getList().length>1)
								{
									if (_bindingLoopIndex == null)
										_bindingLoopIndex = _actualParameters[0].getList()[1].getString();
									_innerEnv.mapValue(_bindingLoopIndex, makeValue(_loopIndex));
								}
								
							}
							else
							{
								_bindingLoopVariable = _actualParameters[0].getString();
								_innerEnv.mapValue(_bindingLoopVariable, _loopList[(int)_loopIndex]);
							}
							_loopIndex++;
							_currentState = EVALUATING_EXP;
							break;
						case EVALUATING_EXP:
							if (resume && _lastFunctionReturn.getContinuingFunction() != null)
								result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_innerEnv, resume);
							else
								result = _lastFunctionReturn = _innerEnv.evaluate(_actualParameters[2], false);
							if (result.isContinuation())
								return continuationReturn(result);
							if (result.isReturn() || result.isSignal() || result.isSignalOut())
								return resetReturn(result);
							if (result.isBreak())
								return resetReturn(result.setBreak(false));
							_outList[(int)_loopIndex-1] = result;
							_currentState = BINDING_VARIABLES;
							break;
					}
					
				}
				
			}
		}
		);
		
		env.mapFunction("map-filter", new FunctionTemplate()
		{

			final int EVALUATING_LIST = 0;
			final int BINDING_VARIABLES = 1;
			final int EVALUATING_EXP = 2;
			int _currentState = EVALUATING_LIST;
			long _loopIndex = 0;
			long _maxIndex = 0;
			String _bindingLoopVariable = null;
			String _bindingLoopIndex = null;
			Value[] _loopList = null;
			LinkedList<Value> _outList = null;
			
			Environment _innerEnv = null;
			public void resetFunctionTemplate()
			{
				_currentState = EVALUATING_LIST;
				_lastFunctionReturn = null;
				_instructionPointer = 0;
				_loopIndex = 0;
				_bindingLoopVariable = null;
				_bindingLoopIndex = null;
				_loopList = null;
				_maxIndex = 0;
				
				_innerEnv = null;
				_outList = null;
			}
			
			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(1, true, true);
				
				if (!resume)
					resetFunctionTemplate();
				
				Value result = Environment.getNull();
				if (_innerEnv == null)
					_innerEnv = new Environment(env);
				while (true)
				{
					switch (_currentState)
					{
						case EVALUATING_LIST:
							
							if (resume && _lastFunctionReturn.getContinuingFunction() != null)
								result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_innerEnv, resume);
							else
								result = _lastFunctionReturn = _innerEnv.evaluate(_actualParameters[1], false);
							if (result.isContinuation())
								return continuationReturn(result);
							if (result.isReturn() || result.isSignal() || result.isSignalOut())
								return resetReturn(result);
							if (result.isBreak())
								return resetReturn(result.setBreak(false));
							
							if (result.isList())
							{
								_loopList = result.getList();
								
								_maxIndex = _loopList.length;
								_outList = new LinkedList<Value>();
							}
							else 
								throw new RuntimeException("Second argument to 'mapcar' must be a list");
							 _currentState = BINDING_VARIABLES;
							 _loopIndex = 0;
							 break;
						case BINDING_VARIABLES:
							if (_loopIndex>=_maxIndex)
							{
								return resetReturn(makeValue(_outList.toArray(new Value[0])));
							}
							if (_actualParameters[0].isList())
							{
								if (_bindingLoopVariable == null)
									_bindingLoopVariable = _actualParameters[0].getList()[0].getString();
								_innerEnv.mapValue(_bindingLoopVariable, _loopList[(int)_loopIndex]);
								
								if (_actualParameters[0].getList().length>1)
								{
									if (_bindingLoopIndex == null)
										_bindingLoopIndex = _actualParameters[0].getList()[1].getString();
									_innerEnv.mapValue(_bindingLoopIndex, makeValue(_loopIndex));
								}
								
							}
							else
							{
								_bindingLoopVariable = _actualParameters[0].getString();
								_innerEnv.mapValue(_bindingLoopVariable, _loopList[(int)_loopIndex]);
							}
							_loopIndex++;
							_currentState = EVALUATING_EXP;
							break;
						case EVALUATING_EXP:
							if (resume && _lastFunctionReturn.getContinuingFunction() != null)
								result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_innerEnv, resume);
							else
								result = _lastFunctionReturn = _innerEnv.evaluate(_actualParameters[2], false);
							if (result.isContinuation())
								return continuationReturn(result);
							if (result.isReturn() || result.isSignal() || result.isSignalOut())
								return resetReturn(result);
							if (result.isBreak())
								return resetReturn(result.setBreak(false));
							if (!result.isNull())
								_outList.add(result);
							_currentState = BINDING_VARIABLES;
							break;
					}
					
				}
				
			}
		}
		);
		
		env.mapFunction("map-some", new FunctionTemplate()
		{

			final int EVALUATING_LIST = 0;
			final int BINDING_VARIABLES = 1;
			final int EVALUATING_EXP = 2;
			int _currentState = EVALUATING_LIST;
			long _loopIndex = 0;
			long _maxIndex = 0;
			String _bindingLoopVariable = null;
			String _bindingLoopIndex = null;
			Value[] _loopList = null;
			LinkedList<Value> _outList = null;
			
			Environment _innerEnv = null;
			public void resetFunctionTemplate()
			{
				_currentState = EVALUATING_LIST;
				_lastFunctionReturn = null;
				_instructionPointer = 0;
				_loopIndex = 0;
				_bindingLoopVariable = null;
				_bindingLoopIndex = null;
				_loopList = null;
				_maxIndex = 0;
				
				_innerEnv = null;
				_outList = null;
			}
			
			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(1, true, true);
				
				if (!resume)
					resetFunctionTemplate();
				
				Value result = Environment.getNull();
				if (_innerEnv == null)
					_innerEnv = new Environment(env);
				while (true)
				{
					switch (_currentState)
					{
						case EVALUATING_LIST:
							
							if (resume && _lastFunctionReturn.getContinuingFunction() != null)
								result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_innerEnv, resume);
							else
								result = _lastFunctionReturn = _innerEnv.evaluate(_actualParameters[1], false);
							if (result.isContinuation())
								return continuationReturn(result);
							if (result.isReturn() || result.isSignal() || result.isSignalOut())
								return resetReturn(result);
							if (result.isBreak())
								return resetReturn(result.setBreak(false));
							
							if (result.isList())
							{
								_loopList = result.getList();
								
								_maxIndex = _loopList.length;
								_outList = new LinkedList<Value>();
							}
							else 
								throw new RuntimeException("Second argument to 'mapcar' must be a list");
							 _currentState = BINDING_VARIABLES;
							 _loopIndex = 0;
							 break;
						case BINDING_VARIABLES:
							if (_loopIndex>=_maxIndex)
							{
								return resetReturn(Environment.getNull());
							}
							if (_actualParameters[0].isList())
							{
								if (_bindingLoopVariable == null)
									_bindingLoopVariable = _actualParameters[0].getList()[0].getString();
								_innerEnv.mapValue(_bindingLoopVariable, _loopList[(int)_loopIndex]);
								
								if (_actualParameters[0].getList().length>1)
								{
									if (_bindingLoopIndex == null)
										_bindingLoopIndex = _actualParameters[0].getList()[1].getString();
									_innerEnv.mapValue(_bindingLoopIndex, makeValue(_loopIndex));
								}
								
							}
							else
							{
								_bindingLoopVariable = _actualParameters[0].getString();
								_innerEnv.mapValue(_bindingLoopVariable, _loopList[(int)_loopIndex]);
							}
							_loopIndex++;
							_currentState = EVALUATING_EXP;
							break;
						case EVALUATING_EXP:
							if (resume && _lastFunctionReturn.getContinuingFunction() != null)
								result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_innerEnv, resume);
							else
								result = _lastFunctionReturn = _innerEnv.evaluate(_actualParameters[2], false);
							if (result.isContinuation())
								return continuationReturn(result);
							if (result.isReturn() || result.isSignal() || result.isSignalOut())
								return resetReturn(result);
							if (result.isBreak())
								return resetReturn(result.setBreak(false));
							if (!result.isNull())
								return resetReturn(result);
							_currentState = BINDING_VARIABLES;
							break;
					}
					
				}
				
			}
		}
		);
		
		
		
		env.mapFunction("unless", new FunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(1, true, true);
				
				if (!resume)
					resetFunctionTemplate();
				
				Value result = Environment.getNull();
				for (;_instructionPointer<_actualParameters.length;_instructionPointer++)
				{
					if (resume && _lastFunctionReturn.getContinuingFunction() != null)
						result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
					else
						result = _lastFunctionReturn = env.evaluate(_actualParameters[_instructionPointer], false);
					
					if (result.isContinuation())
						return continuationReturn(result);
					if ((_instructionPointer == 0 && !result.isNull()) || result.isReturn() || result.isSignal() || result.isSignalOut())
						return resetReturn(result);
					
					if (result.isBreak())
						return resetReturn(result.setBreak(false));
				}
				
				_instructionPointer = 0;
				return result.clone().setContinuation(true, this);
				
			}
		}
		);
		
		env.mapFunction("cond", new FunctionTemplate()
		{
			int _blockInstructionPointer = 0;
			public void resetFunctionTemplate()
			{
				
				_lastFunctionReturn = null;
				_instructionPointer = 0;
				_blockInstructionPointer = 0;
				
			}
			
			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				
				if (!resume)
					resetFunctionTemplate();
				Value[] innerArgs;
				Value result = null;
				outer: for (;_blockInstructionPointer<_actualParameters.length; _blockInstructionPointer++)
				{
					innerArgs = _actualParameters[_blockInstructionPointer].getList();
					for (;_instructionPointer < innerArgs.length;_instructionPointer++)
					{
						if (_instructionPointer == 0)
						{
							if (resume && _lastFunctionReturn.getContinuingFunction()!=null)
								result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
							else
								result = _lastFunctionReturn = env.evaluate(innerArgs[_instructionPointer]);
							
							if (result.isContinuation())
								return continuationReturn(result);
							if (result.isReturn() || result.isSignal() || result.isSignalOut() || result.isBreak())
								return resetReturn(result);
							
							if (result.isNull())
							{
								continue outer;
							}
							
						}
						else
						{
							if (resume && _lastFunctionReturn.getContinuingFunction()!=null)
								result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
							else
								result = _lastFunctionReturn = env.evaluate(innerArgs[_instructionPointer]);
							
							if (result.isContinuation())
								return continuationReturn(result);
							if (result.isReturn() || result.isSignal() || result.isSignalOut() || result.isBreak())
								return resetReturn(result);
						}
					}
					return resetReturn(result);
				}
				return Environment.getNull();
			}
			
		});
		
		env.mapFunction("try", new FunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(1, false, false);
				
				if (!resume)
					resetFunctionTemplate();
				
				Value result;
				
				try
				{
					if (resume && _lastFunctionReturn.getContinuingFunction()!=null)
						result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
					else
						result = _lastFunctionReturn = env.evaluate(_actualParameters[0]);
					
					if (result.isContinuation())
						return continuationReturn(result);
					if (result.isReturn() || result.isSignal() || result.isSignalOut() || result.isBreak())
						return resetReturn(result);
					return resetReturn(result);
				}
				catch (Throwable e)
				{
					Value signalName = makeValue("RUNTIME_ERROR");
					Value signalValue;
					StackTraceElement[] stack = e.getStackTrace();
					int offset = 0;
					
					Value[] strace = new Value[stack.length - offset];;
					Value[] element;
					StackTraceElement selement;
					for (int i=offset;i<stack.length;i++)
					{
						selement = stack[i]; 
						element = new Value[4];
						element[0] = makeValue(selement.getClassName());
						element[1] = makeValue(selement.getMethodName());
						element[2] = makeValue(selement.getLineNumber());
						element[3] = makeValue(selement.getFileName());
						
						strace[i-offset] = makeValue(element);
						
					}
					
					signalValue = makeValue(new Value[]{makeValue(e.getMessage()), makeValue(strace)});
					Value exceptionArg = new SignalValue(signalName, signalValue, false);
					return resetReturn(exceptionArg);
					
				}
				
			
			}
			
		});
		
		
		env.mapFunction("signal-block", new FunctionTemplate()
		{
			
			
			final int EVALUATING_INITIAL_SIGNAL = 0;
			final int FINDING_MATCHING_SIGNAL_BLOCK = 1;
			final int EVALUATING_SIGNAL_BLOCK = 2;
			int _currentState = EVALUATING_INITIAL_SIGNAL;
			Value _signalKey;
			Value _signalValue;
			
			boolean _processingInitialSignal = true;
			int _blockInstructionPointer = 0;
			
			public void resetFunctionTemplate()
			{
				_currentState = EVALUATING_INITIAL_SIGNAL;
				_lastFunctionReturn = null;
				_instructionPointer = 0;
				_argumentInstructionPointer = 0;
				_blockInstructionPointer = 1;
				_signalKey = Environment.getNull();
				_signalValue = Environment.getNull();
				_processingInitialSignal = true;
			}
			
			
			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException{
				checkActualArguments(2, true, true);
				
				if (!resume)
					resetFunctionTemplate();
				Value[] parameters;
				if (_actualParameters[0].isList())
					parameters = _actualParameters[0].getList();
				else
					parameters = new Value[]{_actualParameters[0], Environment.getNull()};
				Value result = Environment.getNull();
				loop: while (true)
				{
					switch (_currentState){
						case EVALUATING_INITIAL_SIGNAL:
						
							for (;_argumentInstructionPointer< parameters.length;_argumentInstructionPointer++)
							{
								if (_argumentInstructionPointer == 0)
								{
									if (resume && _lastFunctionReturn.getContinuingFunction()!=null)
										result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
									else
										result = _lastFunctionReturn = env.evaluate(parameters[_argumentInstructionPointer]);
									
									if (result.isContinuation())
										return continuationReturn(result);
									if (result.isReturn() || result.isSignal() || result.isSignalOut() || result.isBreak())
										return resetReturn(result);
									_signalKey = result;
									
								}
								else
								{
									if (resume && _lastFunctionReturn.getContinuingFunction()!=null)
										result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
									else
										result = _lastFunctionReturn = env.evaluate(parameters[_argumentInstructionPointer]);
									
									if (result.isContinuation())
										return continuationReturn(result);
									if (result.isReturn() || result.isSignal() || result.isSignalOut() || result.isBreak())
										return resetReturn(result);
									_signalValue = result;
								}
							}
							_blockInstructionPointer = 1;
							_currentState = FINDING_MATCHING_SIGNAL_BLOCK;
							break;
						case FINDING_MATCHING_SIGNAL_BLOCK:
							Value[] testValues = null; 
							for (;_blockInstructionPointer<_actualParameters.length;_blockInstructionPointer++)
							{
								testValues = _actualParameters[_blockInstructionPointer].getList()[0].getList();
								Value matchingKey = env.evaluate(testValues[0]);
								if (matchingKey.isNull() || matchingKey.equals(_signalKey))
								{
									if (testValues.length == 2)
										env.mapValue(testValues[1].getString(), _signalValue);
									
									_instructionPointer = 1;
									env.mapValue(_signalKeyBindingName, _signalKey);
									_currentState = EVALUATING_SIGNAL_BLOCK;
									continue loop;
								}
								else if (testValues.length == 2)
									env.mapValue(testValues[1].getString(), Environment.getNull());
								env.mapValue(_signalKeyBindingName, Environment.getNull());	
							}
							if (_processingInitialSignal)
								return resetReturn(Environment.getNull());
							else
								return new SignalValue(_signalKey, _signalValue, false);
						case EVALUATING_SIGNAL_BLOCK:
							_processingInitialSignal = false;
							for (;_instructionPointer<_actualParameters[_blockInstructionPointer].getList().length;_instructionPointer++)
							{
								if (resume && _lastFunctionReturn.getContinuingFunction() != null)
									result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
								else
									result = _lastFunctionReturn = env.evaluate(_actualParameters[_blockInstructionPointer].getList()[_instructionPointer]);
								
								if (result.isContinuation())
									return continuationReturn(result);
								if (result.isReturn() || result.isBreak())
									return resetReturn(result);
								
								if (result.isSignal())
								{
									_signalKey = result.getSignalName();
									_signalValue = result.getSignalValue();
									_instructionPointer = 1;
									_blockInstructionPointer = 1;
									_currentState = FINDING_MATCHING_SIGNAL_BLOCK;
									continue loop;
								}
								
								if (result.isSignalOut())
								{
									return resetReturn(result.setSignalOut(false));
								}
							}
							
							return resetReturn(result);
					}
				
				}
			}
		}
			
		);
		
		
		env.mapFunction("with", new FunctionTemplate()
		{

			Lambda _lambda;
			
			public void resetFunctionTemplate()
			{
				_lastFunctionReturn = null;
				_instructionPointer = 0;
				_lambda = null;
			}
			
			
			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(1, true, true);
				if (!resume)
					resetFunctionTemplate();
				Value result = Environment.getNull();
				
				
				for (;_instructionPointer<_actualParameters.length;_instructionPointer++)
				{
					if (_instructionPointer == 0)
					{
						if (resume && _lastFunctionReturn.getContinuingFunction() != null)
							result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
						else
							result = _lastFunctionReturn = env.evaluate(_actualParameters[_instructionPointer], false);
						
						if (result.isContinuation())
							return continuationReturn(result);
						if (result.isNull() || result.isBreak() || result.isReturn() || result.isSignal() || result.isSignalOut())
							return resetReturn(result);
						
						if (!result.isLambda())
							throw new RuntimeException("First argument to 'with' must be a lambda function: " + result);
						if (result.getLambda().hasNameP())
							throw new RuntimeException("Only annonymous lambda functions can be the first argument to 'with' functions");
						
						_lambda = (Lambda)result.getLambda();
						
					}
					else
					{
						if (resume && _lastFunctionReturn.getContinuingFunction() != null)
							result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_lambda.getInnerEnvironment(), resume);
						else
							result = _lastFunctionReturn = _lambda.getInnerEnvironment().evaluate(_actualParameters[_instructionPointer], false);
						
						if (result.isContinuation())
							return continuationReturn(result);
						if (result.isBreak() || result.isReturn() || result.isSignal() || result.isSignalOut())
							return resetReturn(result);
						
					}
					
				}
				
				return resetReturn(result);
			}
		}
		);
		
		env.mapFunction("progn", new FunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(0, true, true);
				if (!resume)
					resetFunctionTemplate();
				
				Value result = Environment.getNull();
				for (;_instructionPointer<_actualParameters.length;_instructionPointer++)
				{
					if (resume && _lastFunctionReturn.getContinuingFunction() != null)
						result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
					else
						result = _lastFunctionReturn = env.evaluate(_actualParameters[_instructionPointer], false);
					
					if (result.isContinuation())
						return continuationReturn(result);
					if (result.isBreak() || result.isReturn() || result.isSignal() || result.isSignalOut())
						return resetReturn(result);
				}
				
				return resetReturn(result);
			}
		}
		);
		
		
		// First argument is a list of declarations and can be empty
		env.mapFunction("let", new FunctionTemplate()
		{
			Environment _currentEnv = null;
			boolean _evaluatedArgs = false;
			@Override
			public void resetFunctionTemplate()
			{
				_lastFunctionReturn = null;
				_argumentIndex = 0;
				_evaluatedArgs = false;
				_currentEnv = null;
			}
			
			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(0, true, true);
				
				if (!resume)
				{
					resetFunctionTemplate();
					_currentEnv = new Environment(env);
				}
			
				Value bindingList = _actualParameters[0];
				if (bindingList.isList())
				{
					
					if (!_evaluatedArgs)
					{
						Value bindingResult = evaluateBindingList(_currentEnv, env, bindingList.getList(), false, resume);
						if (isBreakLike(bindingResult))
						{
							return bindingResult;
						}
						else if (bindingResult.isContinuation())
							return bindingResult;
						_evaluatedArgs = true;
						_instructionPointer = 1;
					}
					
					Value result = Environment.getNull();
					for (;_instructionPointer<_actualParameters.length;_instructionPointer++)
					{
						if (resume && _lastFunctionReturn.getContinuingFunction() != null)
							result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_currentEnv, resume);
						else
							result = _lastFunctionReturn = _currentEnv.evaluate(_actualParameters[_instructionPointer], false);
						
						if (result.isContinuation())
							return continuationReturn(result);
						if ( result.isBreak() || result.isReturn() || result.isSignal() || result.isSignalOut())
							return resetReturn(result);
					}
					return resetReturn(result);
				}
				else
					throw new RuntimeException("Incorrect type for binding list: " + bindingList);
				
				
			}
		}
		);
		
		// First argument is a list of declarations and can be empty
		env.mapFunction("let*", new FunctionTemplate()
		{
			Environment _currentEnv = null;
			boolean _evaluatedArgs = false;
			@Override
			public void resetFunctionTemplate()
			{
				_lastFunctionReturn = null;
				_argumentInstructionPointer = 0;
				_evaluatedArgs = false;
				_instructionPointer = 0;
				_currentEnv = null;
			}
			
			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(0, true, true);
				
				
				
				if (!resume)
				{
					resetFunctionTemplate();
					_currentEnv = new Environment(env);;
				}
				
				Value bindingList = _actualParameters[0];
				if (bindingList.isList())
				{
					
					if (!_evaluatedArgs)
					{
						Value bindingResult = evaluateBindingList(_currentEnv, env, bindingList.getList(), true, resume);
						if (isBreakLike(bindingResult))
						{
							return bindingResult;
						}
						else if (bindingResult.isContinuation())
							return bindingResult;
						_evaluatedArgs = true;
						_instructionPointer = 1;
					}
					
					Value result = Environment.getNull();
					for (;_instructionPointer<_actualParameters.length;_instructionPointer++)
					{
						if (resume && _lastFunctionReturn.getContinuingFunction() != null)
							result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(_currentEnv, resume);
						else
							result = _lastFunctionReturn = _currentEnv.evaluate(_actualParameters[_instructionPointer], false);
						
						if (result.isContinuation())
							return continuationReturn(result);
						if ( result.isBreak() || result.isReturn() || result.isSignal() || result.isSignalOut())
							return resetReturn(result);
					}
					return resetReturn(result);
				}
				else
					throw new RuntimeException("Incorrect type for binding list: " + bindingList);
				
				
			}
		}
		);
		
		// -~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// Utility Functions and macros
		// -~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		
		env.mapFunction("make-int-hashtable", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(0, true, true);
				HashMap<Long, Value> out = new HashMap<Long, Value>();
				if (evaluatedArgs.length == 0)
				{
					
					return new IntHashtableValue(out);
				}
				
				if (evaluatedArgs[0].isList())
				{
					
					for (Value pair:evaluatedArgs[0].getList())
					{
						out.put(pair.getList()[0].getIntValue(), pair.getList()[1]);
					}
					return new IntHashtableValue(out);
				}
		
				throw new RuntimeException("Incorrect argument type: expected list of key-value pairs");
				
			}
			
		}
		);
		
		env.mapFunction("make-string-hashtable", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(0, true, true);
				HashMap<String, Value> out = new HashMap<String, Value>();
				if (evaluatedArgs.length == 0)
				{
					
					return new StringHashtableValue(out);
				}
				
				if (evaluatedArgs[0].isList())
				{
					
					for (Value pair:evaluatedArgs[0].getList())
					{
						out.put(pair.getList()[0].getString(), pair.getList()[1]);
					}
					return new StringHashtableValue(out);
				}
		
				throw new RuntimeException("Incorrect argument type: expected list of key-value pairs");
				
			}
			
		}
		);
		
		env.mapFunction("is-int-hashtable", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				if (evaluatedArgs[0].isIntHashtable())
					return evaluatedArgs[0];
				else
					return makeValue(false);
			}
			
		}
		);
		
		env.mapFunction("is-string-hashtable", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				if (evaluatedArgs[0].isStringHashtable())
					return evaluatedArgs[0];
				else
					return makeValue(false);
			}
			
		}
		);
		
		
		
		env.mapFunction("get-merged-map", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, false);
				if (evaluatedArgs[0].isIntHashtable() && evaluatedArgs[1].isIntHashtable())
				{
					HashMap<Long, Value> map1 = evaluatedArgs[0].getIntHashtable(), map2 = evaluatedArgs[1].getIntHashtable();
					HashMap<Long, Value> out = new HashMap<Long, Value>();
					if (map1 != null)
						out.putAll(map1);
					
					if (map2 != null)
						out.putAll(map2);
					return new IntHashtableValue(out);
				}
				else if (evaluatedArgs[0].isStringHashtable() && evaluatedArgs[1].isStringHashtable())
				{
					HashMap<String, Value> map1 = evaluatedArgs[0].getStringHashtable(), map2 = evaluatedArgs[1].getStringHashtable();
					HashMap<String, Value> out = new HashMap<String, Value>();
					if (map1 != null)
						out.putAll(map1);
					
					if (map2 != null)
						out.putAll(map2);
					return new StringHashtableValue(out);
				}
				else throw new RuntimeException("Arguments to get-merged-map either be both int hashtables or both string hashtables");
				
			}
			
		});
		
		env.mapFunction("string", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, false);
				
				if (evaluatedArgs[0].isUserObject())
					return makeValue(evaluatedArgs[0].getObjectValue().toString());
				else if (evaluatedArgs[0].isString())
					return makeValue(evaluatedArgs[0].getString());
				else
					return makeValue(evaluatedArgs[0].toString());
		
			}
			
		}
		);
		
		
		env.mapFunction("defhash", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(3, false, false);
				if (evaluatedArgs[0].isHashtable())
				{
					if (evaluatedArgs[0].isIntHashtable())
					{
						HashMap<Long, Value> map = evaluatedArgs[0].getIntHashtable();
						map.put(Long.valueOf(evaluatedArgs[1].getIntValue()), evaluatedArgs[2]);
						return evaluatedArgs[2];
					}
					if (evaluatedArgs[0].isStringHashtable())
						
					{
						HashMap<String, Value> map = evaluatedArgs[0].getStringHashtable();
						map.put(evaluatedArgs[1].getString(), evaluatedArgs[2]);
						return evaluatedArgs[2];
					} 
					
					
				}
		
				throw new RuntimeException("Incorrect argument type: expected hashtable");
				
			}
			
		}
		);
		
		env.mapFunction("defhash*", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(3, false, false);
				if (evaluatedArgs[0].isHashtable())
				{
					if (evaluatedArgs[0].isIntHashtable())
					{
						HashMap<Long, Value> map = evaluatedArgs[0].getIntHashtable();
						HashMap<Long, Value> nMap = new HashMap<Long, Value>();
						nMap.putAll(map);
						nMap.put(Long.valueOf(evaluatedArgs[1].getIntValue()), evaluatedArgs[2]);
						return new IntHashtableValue(nMap);
					}
					if (evaluatedArgs[0].isStringHashtable())
						
					{
						HashMap<String, Value> map = evaluatedArgs[0].getStringHashtable();
						HashMap<String, Value> nMap = new HashMap<String, Value>();
						nMap.putAll(map);
						nMap.put(evaluatedArgs[1].getString(), evaluatedArgs[2]);
						return new StringHashtableValue(nMap);
					} 
					
					
				}
		
				throw new RuntimeException("Incorrect argument type: expected hashtable");
				
			}
			
		}
		);
		
		env.mapFunction("gethash", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, false, false);
				if (evaluatedArgs[0].isHashtable())
				{
					if (evaluatedArgs[0].isIntHashtable())
					{
						HashMap<Long, Value> map = evaluatedArgs[0].getIntHashtable();
						Value out = map.get(Long.valueOf( evaluatedArgs[1].getIntValue()));
						if (out!=null)
							return out.clone();
						else
							return Environment.getNull();
						
					}
					if (evaluatedArgs[0].isStringHashtable())
						
					{
						HashMap<String, Value> map = evaluatedArgs[0].getStringHashtable();
						Value out = map.get(evaluatedArgs[1].getString());
						if (out!=null)
							return out.clone();
						else
							return Environment.getNull();
					} 
					
					
				}
		
				throw new RuntimeException("Incorrect argument type: expected hashtable");
				
			}
			
		}
		);
		
		env.mapFunction("remhash", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, false, false);
				if (evaluatedArgs[0].isHashtable())
				{
					if (evaluatedArgs[0].isIntHashtable())
					{
						HashMap<Long, Value> map = evaluatedArgs[0].getIntHashtable();
						map.remove(Long.valueOf( evaluatedArgs[1].getIntValue()));
						return evaluatedArgs[0];
						
					}
					if (evaluatedArgs[0].isStringHashtable())
					{
						HashMap<String, Value> map = evaluatedArgs[0].getStringHashtable();
						
						map.remove(evaluatedArgs[1].getString());
						return evaluatedArgs[0];
						
					} 
				}
		
				throw new RuntimeException("Incorrect argument type: expected hashtable");
				
			}
			
		}
		);
		
		env.mapFunction("get-hash-keys", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, false);
				if (evaluatedArgs[0].isHashtable())
				{
					Value[] out = null;
					if (evaluatedArgs[0].isIntHashtable())
					{
						HashMap<Long, Value> map = evaluatedArgs[0].getIntHashtable();
						out = new Value[map.size()];
						int i = 0;
						for (Long l:map.keySet())
							out[i++] = makeValue(l.longValue());
						 
						
					}
					else
					{
						HashMap<String, Value> map = evaluatedArgs[0].getStringHashtable();
						out = new Value[map.size()];
						int i = 0;
						for (String s:map.keySet())
							out[i++] = makeValue(s);
						
					} 
					return makeValue(out);
				}
		
				throw new RuntimeException("Incorrect argument type: expected hashtable");
				
			}
			
		}
		);
		
		
		
		// Define increment macro.  Increments a variable 
		env.evaluate("(defmacro incr (x) `(setq ,x (+ 1 ,x)))", true);
		env.evaluate("(defmacro decr (x) `(setq ,x (- ,x 1)))", true);
		env.evaluate("(defmacro all-but-last (list) `(subseq ,list 0 (- (length ,list) 1)))", true);
		env.evaluate(" (defmacro unless-once (cond ...) (if (= (length ...) 1) `(unless ,cond (break ,(last ...))) `(unless-once ,cond ,(all-but-last ...) (break ,(last ...)))))", true);
		//env.evaluate("(defun append (list item) (if (list-p item) `(,@list ,@item) `(,@list ,item)))", true);
		
		env.evaluate("(defmacro multiple-bind* (binding-list value-list) (setq setq-list ()) (setq n (length binding-list)) (setq i 0) (while (< i n) (setq setq-list (append setq-list (list `(setq ,(nth binding-list i) ,(nth value-list i))))) (incr i)) `(progn ,@setq-list ,value-list))", true);
		//env.evaluate("(defmacro funcall (fn ...) `(apply ,fn ,...))", true);
		//env.evaluate("(defmacro find (loop-spec list condition) 		(setq remaining-list (gensym))		(setq matching-list (gensym))			(if (list-p loop-spec)		(progn			(if (not condition)				(setq condition					  (first loop-spec)))			`(let () 							(setq ,remaining-list ,list)							(setq ,matching-list ()) 							(setq ,(first loop-spec) 					  (first ,remaining-list)) 							(setq ,(second loop-spec) 0) 							(while (> (length ,remaining-list) 0) 									(if ,condition											(set ,matching-list						 							 (append-item ,matching-list						 		 							 	     ,(first loop-spec))))									(incr ,(second loop-spec)) 									(setq ,remaining-list 						  (rest ,remaining-list)) 									(setq ,(first loop-spec) 						  (first ,remaining-list))) 				,matching-list))		(progn			(if (not condition)				(setq condition					  loop-spec))			`(let () 							(setq ,remaining-list ,list)							(setq ,matching-list ()) 							(setq ,loop-spec (first ,remaining-list)) 							(while (> (length ,remaining-list) 0)									(if ,condition											(set ,matching-list						 							(append-item ,matching-list						 		 								,loop-spec)))				 					(setq ,remaining-list 				 	   						(rest ,remaining-list)) 				 					(setq ,loop-spec 						  (first ,remaining-list)))					,matching-list))))", true);
		env.evaluate("(defmacro some (loop-spec list condition) 	(if (list-p loop-spec) `(for ,loop-spec ,list F (if ,condition (break ,(first loop-spec)))) `(for ,loop-spec ,list F (if ,condition (break ,loop-spec)))))", true);
		env.evaluate("(defmacro all (loop-spec list condition) 	(if (list-p loop-spec) `(for ,loop-spec ,list ,list (if (not ,condition) (break F))) `(for ,loop-spec ,list ,list (if (not ,condition) (break F)))))", true);
		//env.evaluate("(defmacro mapcar (loop-spec list transform) 	(setq remaining-list (gensym))	(setq new-list (gensym))	(if (list-p loop-spec) 		`(let () 			(setq ,remaining-list ,list)			(setq ,(first loop-spec) (first ,remaining-list)) 			(setq ,(second loop-spec) 0)			(setq ,new-list ()) 			(while ,(first loop-spec) 				(setq ,new-list					  (append ,new-list (if (list-p ,transform) (list ,transform) ,transform)))				(incr ,(second loop-spec)) 				(setq ,remaining-list (rest ,remaining-list)) 				(setq ,(first loop-spec) (first ,remaining-list)))			,new-list) 		`(let () 			(setq ,remaining-list ,list)			(setq ,new-list ()) 			(setq ,loop-spec (first ,remaining-list)) 			(while ,loop-spec				(setq ,new-list					  (append ,new-list (if (list-p ,transform) (list ,transform) ,transform)))					 (setq ,remaining-list 				 	   (rest ,remaining-list)) 				 (setq ,loop-spec (first ,remaining-list)))			,new-list				 )))", true);
		//env.evaluate("(defmacro map-filter (loop-spec list transform) `(find x (mapcar ,loop-spec ,list ,transform)))", true);
		//env.evaluate("(defmacro append-item (list item) (if (list-p item) `(append ,list (list ,item)) `(append ,list ,item)))", true);
		// supports two usage patterns
		// (1) (for {loop-variable} {list} {return-expression} {iteration-expression})
		// (2) (for ({loop-variable} {loop-index}) {list} {return-expression} {iteration-expression})
		//nv.evaluate("(defmacro for (loop-spec list return iter) (if (list-p loop-spec) `(let () (setq xx*xx ,list) (setq ,(first loop-spec) (first xx*xx)) (setq ,(second loop-spec) 0) (while ,(first loop-spec) ,iter (incr ,(second loop-spec)) (setq xx*xx (rest xx*xx)) (setq ,(first loop-spec) (first xx*xx))) ,return) `(let () (setq xx*xx ,list) (setq ,loop-spec (first xx*xx)) (while ,loop-spec ,iter (setq xx*xx (rest xx*xx)) (setq ,loop-spec (first xx*xx))) ,return)))", true);
		
		env.mapFunction("multiple-bind", new FunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				if (!resume)
					resetFunctionTemplate();
				Value result;
				if (resume && _lastFunctionReturn.getContinuingFunction()!=null)
					result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
				else
					result = _lastFunctionReturn = env.evaluate(_actualParameters[1]);
				
				if (result.isContinuation())
					return continuationReturn(result);
				if (result.isNull() || result.isReturn() || result.isSignal() || result.isSignalOut() || result.isBreak())
					return resetReturn(result);
				
				if (result.isList())
				{
					Value bindingName;
					Value[] mappingValues = result.getList();
					for (int i=0;i<_actualParameters[0].getList().length;i++)
					{
						bindingName = _actualParameters[0].getList()[i];
						if (i<mappingValues.length)
							env.mapValue(bindingName.getString(), mappingValues[i]);
						else
							env.mapValue(bindingName.getString(), Environment.getNull());
					}
				}
				else
					throw new RuntimeException("Second argument to multiple-bind must be a list");
				
				return result;
			}
			
		}
		);
		
		env.mapFunction("multiple-set", new FunctionTemplate()
		{
			
			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				if (!resume)
					resetFunctionTemplate();
				
				Value result;
				if (resume && _lastFunctionReturn.getContinuingFunction()!=null)
					result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
				else
					result = _lastFunctionReturn = env.evaluate(_actualParameters[1]);
				
				if (result.isContinuation())
					return continuationReturn(result);
				if (result.isNull() || result.isReturn() || result.isSignal() || result.isSignalOut() || result.isBreak())
					return resetReturn(result);
				
				
				Environment targetEnv = null;
				if (result.isList())
				{
					String bindingName;
					Value[] mappingValues = result.getList();
					for (int i=0;i<_actualParameters[0].getList().length;i++)
					{
						targetEnv = env;
						bindingName = _actualParameters[0].getList()[i].getString();
						while (targetEnv!=null && !targetEnv.hasVariable(bindingName)){
							targetEnv = targetEnv.getParent();
						}
						
						if (targetEnv==null)
							targetEnv = env;
						
						if (i<mappingValues.length)
							targetEnv.mapValue(bindingName, mappingValues[i]);
						else
							targetEnv.mapValue(bindingName, Environment.getNull());
					}
				}
				else
					throw new RuntimeException("Second argument to multiple-bind must be a list");
				
				return result;
			}
			
		}
		);
		
		
		env.mapFunction("load", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, false);
				String filename = evaluatedArgs[0].getString();
				Environment target =  env.getRootEnvironment();
				
				return target.loadFromFile(filename);
				
			}
			
		}
		);
		
		env.mapFunction("append-item", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, false, false);
				if (evaluatedArgs[0].isList())
				{
					Value[] n = new Value[evaluatedArgs[0].getList().length+1];
					for (int i=0;i<n.length-1;i++)
					{
						n[i] = evaluatedArgs[0].getList()[i];
					}
					n[evaluatedArgs[0].getList().length] = evaluatedArgs[1].clone();
					return makeValue(n);
				}
				throw new RuntimeException("First argument to append-item must be a list");
			}
			
		}
		);
		
		env.mapFunction("append", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				LinkedList<Value> out = new LinkedList<Value>();
				int i = 0;
				Value v;
				int length = evaluatedArgs.length;
				for (i=0;i<length;i++)
				{
					v = evaluatedArgs[i];
					if (i == 0)
					{
						if (v.isList())
						{
							
							for (Value s:v.getList())
							{
								out.add(s);
							}
							
						}
						else
							throw new RuntimeException("First argument to append-item must be a list");
					}
					else
					{
						if (v.isList())
						{
							
							for (Value s:v.getList())
							{
								out.add(s);
							}
							
						}
						else
							out.add(v);
					}
				}
				
				return NLispTools.makeValue(out.toArray(new Value[0]));
			}
			
		}
		);
		
		
		env.mapFunction("insert-list-into-sequence", new SimpleFunctionTemplate()
		{
			// first argument is a list
			// second argument is the replace area start index
			// third argument is the replacement area stop index
			// fourth argument is a list to insert 
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(4, false, false);
				if (evaluatedArgs[0].isList())
				{
					Value[] list = evaluatedArgs[0].getList();
					if (!evaluatedArgs[1].isInteger())
						throw new RuntimeException("second parameter to 'insert-list-into-sequence' must be integer");
					int startIndex = (int)evaluatedArgs[1].getIntValue();
					
					int stopIndex = (int)evaluatedArgs[2].getIntValue();
					if (!evaluatedArgs[2].isInteger())
						throw new RuntimeException("third parameter to 'insert-list-into-sequence' must be integer");
					if (!evaluatedArgs[3].isList())
						throw new RuntimeException("fourth parameter to 'insert-list-into-sequence' must be a list");
					Value[] iList = evaluatedArgs[3].getList();
					Value[] out = new Value[list.length - stopIndex + startIndex + iList.length];
					for (int i = 0;i<out.length;i++)
					{
						if (i<startIndex)
							out[i] = list[i];
						else if (i>=startIndex && i<(startIndex + iList.length))
						{
							out[i] = iList[i - startIndex];
						}
						else
							out[i] = list[stopIndex + (i - startIndex - iList.length)];
					}
					return NLispTools.makeValue(out);
				}
				throw new RuntimeException("First argument to append-item must be a list");
			}
			
		}
		);
		
		env.mapFunction("comment", new FunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				
				return Environment.getNull();
			}
		}
		);
		
		
		env.mapFunction("length", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, false);
				return NLispTools.makeValue((long)evaluatedArgs[0].size());
				
			}
			
		}
		);
		
		env.mapFunction("last", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				checkListsArguments(evaluatedArgs);
				Value[] list = evaluatedArgs[0].getList();
				
				if (list.length == 0)
					return Environment.getNull();
				else
					return list[list.length-1].clone();
				
			}
			
		}
		);
		
		env.mapFunction("subseq", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, true, true);
				
				Value[] list = evaluatedArgs[0].getList();
				int startIndex = (int)evaluatedArgs[1].getIntValue();
				int endIndex = list.length;
				if (_numArguments == 3)
					endIndex = (int)evaluatedArgs[2].getIntValue();
				Value[] sub = subSequence(list, startIndex, endIndex);
				return makeValue(sub);
				
			}
			
		}
		);
		
		env.mapFunction("replace", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(3, false, true);
				
				String base = evaluatedArgs[0].getString();
				String orig = evaluatedArgs[1].getString();
				String replacement = evaluatedArgs[2].getString();
				
				
				return makeValue(base.replaceAll(orig, replacement));
				
			}
			
		}
		);
		
		env.mapFunction("index-of", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, true, true);
				
				String base = evaluatedArgs[0].getString();
				String search = evaluatedArgs[1].getString();
				int start = 0;
				if (evaluatedArgs.length>2)
					start = (int)evaluatedArgs[2].getIntValue();
				
				return makeValue(base.indexOf(search, start));
				
			}
			
		}
		);
		
		
		env.mapFunction("fill-list", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				
				long length = evaluatedArgs[0].getIntValue();
				Value fillValue = (evaluatedArgs.length == 1)?makeValue(0):evaluatedArgs[1];
				Value[] list = new Value[(int)length];
				for (int i=0;i<length;i++)
					list[i] = fillValue.clone();
				return NLispTools.makeValue(list);
				
			}
			
		}
		);
		
		env.mapFunction("make-range", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, true, true);
				
				int start = (int)evaluatedArgs[0].getIntValue();
				int end = (int)evaluatedArgs[1].getIntValue();
				Value[] list = new Value[(start - end)];
				for (int i=start;i<=end;i++)
					list[i] = NLispTools.makeValue(i); 
				return NLispTools.makeValue(list);
				
			}
			
		}
		);
		
		env.mapFunction("format", format(env));
		
		env.mapFunction("substring", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, true, true);
				
				String base = evaluatedArgs[0].getString();
				int start = (int)evaluatedArgs[1].getIntValue();
				int end = base.length();
				if (evaluatedArgs.length>2)
					end = (int)evaluatedArgs[2].getIntValue();
				
				
				return makeValue(base.substring(start, end));
				
			}
			
		}
		);
		
		
		
		env.mapFunction("join", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, true, true);
				
				String separator = evaluatedArgs[1].getString();
				StringBuilder sBuilder = new StringBuilder();
				String[] parts = getStringArrayFromValue(evaluatedArgs[0]);
				
				for (int i=0;i<parts.length;i++)
				{
					if (i==0)
						sBuilder.append(parts[i]);
					else
					{
						sBuilder.append(separator);
						sBuilder.append(parts[i]);
					}
				}
				return makeValue(sBuilder.toString());
				
			}
			
		}
		);
		
		env.mapFunction("trim-start", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				
				String base = evaluatedArgs[0].getString();
				
				String trimpatt = " ";
				if (evaluatedArgs.length>1)
				{
					trimpatt = evaluatedArgs[1].getString();
				}
				String out = base;
				while (out.startsWith(trimpatt))
					out = out.substring(trimpatt.length());
				return makeValue(out);
				
			}
			
		}
		);
		
		env.mapFunction("trim-end", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				
				String base = evaluatedArgs[0].getString();
				
				String trimpatt = " ";
				if (evaluatedArgs.length>1)
				{
					trimpatt = evaluatedArgs[1].getString();
				}
				String out = base;
				while (out.endsWith(trimpatt))
					out = out.substring(0, out.length() - trimpatt.length());
				return makeValue(out);
				
			}
			
		}
		);
		
		env.mapFunction("trim", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				
				String base = evaluatedArgs[0].getString();
				
				String trimpatt = " ";
				if (evaluatedArgs.length>1)
				{
					trimpatt = evaluatedArgs[1].getString();
				}
				else
					return makeValue(base.trim());
				String out = base;
				while (out.endsWith(trimpatt))
					out = out.substring(0, out.length() - trimpatt.length());
				while (out.startsWith(trimpatt))
					out = out.substring(trimpatt.length());
				return makeValue(out);
				
			}
			
		}
		);
		
		env.mapFunction("starts-with", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				String base = evaluatedArgs[0].getString();
				String test = evaluatedArgs[1].getString();
				
				return makeValue(base.startsWith(test));
				
			}
			
		}
		);
		
		env.mapFunction("ends-with", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				String base = evaluatedArgs[0].getString();
				String test = evaluatedArgs[1].getString();
				
				return makeValue(base.endsWith(test));
				
			}
			
		}
		);
		
		
		env.mapFunction("split", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				String base = evaluatedArgs[0].getString();
				String split = evaluatedArgs[1].getString();
				
				if (split.length() == 0)
				{
					Value[] out = new Value[base.length()];
					
					char[] chars = base.toCharArray();
					for (int i=0; i < chars.length;i++)
					{ 
						out[i] = makeValue(String.valueOf(chars[i]));
					}
					return makeValue(out);
				}
				else
				{
					LinkedList<Value> words = new LinkedList<Value>();
					StringBuilder word = new StringBuilder(), temp;
					char c;
					for (int i=0;i<base.length();i++)
					{
						c = base.charAt(i);
						temp = new StringBuilder();
						for (int j = i;j< i + split.length() && j<base.length();j++)
						{
							temp.append(base.charAt(j));
						}
						
						if (temp.toString().equals(split))
						{
							words.add(makeValue(word.toString()));
							word = new StringBuilder();
							i = i + split.length() - 1;
						}
						else
							word.append(c);
					}
					
					if (word.length()>0)
						words.add(makeValue(word.toString()));
					return makeValue(words.toArray(new Value[0]));
				}
				
			}
			
		}
		);
		
		
		env.mapFunction("list", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(0, true, true);
				return new ListValue(evaluatedArgs);
			}
			
		}
		);
		
		env.mapFunction("first", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				if (evaluatedArgs[0].isList())
				{
					Value[] values = evaluatedArgs[0].getList();
					if (values.length < 1)
						return Environment.getNull();
					else
						return values[0].clone();
				}
				else 
					throw new RuntimeException("First argument to 'first' must be a list");
				
			}
			
		}
		);
		
		env.mapFunction("second", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				if (evaluatedArgs[0].isList())
				{
					Value[] values = evaluatedArgs[0].getList();
					if (values.length < 2)
						return Environment.getNull();
					else
						return values[1].clone();
				}
				else 
					throw new RuntimeException("First argument to 'second' must be a list");
				
			}
			
		}
		);
		
		env.mapFunction("nth", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				if (evaluatedArgs[0].isList())
				{
					Value[] values = evaluatedArgs[0].getList();
					return values[(int)evaluatedArgs[1].getIntValue()].clone();
				}
				else 
					throw new RuntimeException("First argument to 'second' must be a list");
				
			}
			
		}
		);
		
		env.mapFunction("rest", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				if (evaluatedArgs[0].isList())
				{
					Value[] values = evaluatedArgs[0].getList();
					if (values.length < 2)
						return new ListValue(new Value[0]);
					else
						return new ListValue(subSequence(values, 1));
				}
				else 
					throw new RuntimeException("First argument to 'rest' must be a list");
				
			}
			
		}
		);
		
		env.mapFunction("random-select", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				if (evaluatedArgs[0].isList())
				{
					int max = evaluatedArgs[0].getList().length;
					if (max == 0)
						return makeValue(new Value[0]);
					int index = Math.min(max-1, (int)(Math.random()*max));
					return evaluatedArgs[0].getList()[index];
				}
				else 
					throw new RuntimeException("First argument to 'random-select' must be a list");
				
			}
			
		}
		);
		
		env.mapFunction("random", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				double start = evaluatedArgs[0].getFloatValue();
				double end = evaluatedArgs[1].getFloatValue();
				if (start==0.0&&end==1.0)
					return makeValue(Math.random());
				else
					return makeValue(Math.random()*(end-start)+start);
				
			}
			
		}
		);
		
		env.mapFunction("random-perm", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				if (evaluatedArgs[0].isList())
				{
					int len = evaluatedArgs[0].getList().length;
					Value[] newCons = new Value[len];
					int selectedIndex;
					Value temp;
					for (int i=0;i<len;i++)
					{
						selectedIndex = Math.min(len-1, (int)(Math.random()*(len-i))+i);
						if (newCons[selectedIndex]==null)
						{
							newCons[i]=evaluatedArgs[0].getList()[selectedIndex];
							newCons[selectedIndex]=evaluatedArgs[0].getList()[i];
						}
						else
						{
							temp=newCons[selectedIndex];
							newCons[i] = temp;
							newCons[selectedIndex]=temp;
						}
							
					}
					return makeValue(newCons);
				}
				else 
					throw new RuntimeException("First argument to 'random-select' must be a list");
				
			}
			
		}
		);
		
		env.mapFunction("cross-join", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(final Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				LinkedList<Value> out = new LinkedList<Value>(), newout = null;
				Value[] elements = null;
				for (Value dimension:evaluatedArgs)
				{
					newout = new LinkedList<Value>();;
					for (int i=0;i < dimension.getList().length;i++)
					{
						if (out.size() > 0)
						{
							for (Value prior:out)
							{
								elements = new Value[prior.getList().length + 1];
								for (int j = 0;j<prior.getList().length;j++)
									elements[j] = prior.getList()[j];
								elements[prior.getList().length] = dimension.getList()[i];
								newout.add(NLispTools.makeValue(elements));
							}
						}
						else
						{
							elements = new Value[]{dimension.getList()[i]};
							newout.add(NLispTools.makeValue(elements));
						}
					}
					
					out = newout;
					
					
				}
				
				return NLispTools.makeValue(newout.toArray(new Value[0]));
				
			}
			
		});
		
		env.mapFunction("sort", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(final Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				if (evaluatedArgs[0].isList())
				{
					Value[] values = evaluatedArgs[0].getList();
					Value[] newValues = new Value[values.length];
					final FunctionTemplate lambda = evaluatedArgs[1].getLambda();
					
					Comparator<Value> comparator = 
							new Comparator<Value>(){
								public int compare(Value s1, Value s2)
								{
									Value[] sargs = new Value[]{s2, s1};
									
									lambda.setActualParameters(sargs);
									
									Value result = makeValue("exception");
									try {
										result = lambda.evaluate(env, false);
									} catch (InstantiationException e) {
										throw new RuntimeException(e.toString());
									} catch (IllegalAccessException e) {
										throw new RuntimeException(e.toString());
									}
									
									return (int)result.getIntValue();
								}
								
								public boolean equals(Object o)
								{
									return true;
								}
							};
					
					for (int i=0;i<values.length;i++)
						newValues[i]= values[i];
					Arrays.sort(newValues, comparator);
					return makeValue(newValues);
					
				}
				else 
					throw new RuntimeException("First argument to 'random-select' must be a list");
				
			}
			
		}
		);
		
		env.mapFunction("list-p", new SimpleFunctionTemplate()
		{

			private void listP()
			{
				
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				
				if (evaluatedArgs[0].isList())
					return evaluatedArgs[0];
				else
					return makeValue(false);
				
			}
			
		}
		);
		
		env.mapFunction("number-p", new SimpleFunctionTemplate()
		{

			private void numberP()
			{
				
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				
				if (isNumericType(evaluatedArgs[0]))
					return evaluatedArgs[0];
				else
					return makeValue(false);
			}
			
		}
		);
		
		env.mapFunction("string-p", new SimpleFunctionTemplate()
		{

			private void stringP()
			{
				
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				if (evaluatedArgs[0].isString())
					return evaluatedArgs[0];
				else
					return makeValue(false);
				
			}
			
		}
		);
		
		env.mapFunction("time", new SimpleFunctionTemplate()
		{

			private void time()
			{
				
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(0, false, true);
				
				return makeValue(System.currentTimeMillis());
			}
			
		}
		);
		
		env.mapFunction("sleep-milli", new SimpleFunctionTemplate()
		{

			private void thread_sleep()
			{
				
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, false);
				long time = evaluatedArgs[0].getIntValue();
				try {
					Thread.sleep(time);
				} catch (InterruptedException e) {
					Thread.currentThread().interrupt();
				}
				return evaluatedArgs[0];
			}
			
		}
		);
		
		
		env.mapFunction("concat", new SimpleFunctionTemplate()
		{

			private void concat()
			{
				
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				String sub; 
				StringBuilder builder = new StringBuilder();
				
				for (int i=0;i<evaluatedArgs.length;i++)
				{
					sub = (evaluatedArgs[i].isString())?evaluatedArgs[i].getString():evaluatedArgs[i].toString();
					builder.append(sub);
				}
				
				return makeValue(builder.toString());
			}
			
		}
		);
		
		env.mapFunction("unique-id", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				
				return makeValue(UUID.randomUUID().toString());
			}
			
		}
		);
		
		
		// -o~o_o-o~o_o-o~o_o-o~o_o-o~o_o-o~o_o-o~o_o-o~o_o
		// Simple Arithmetic Routines
		// -o~o_o-o~o_o-o~o_o-o~o_o-o~o_o-o~o_o-o~o_o-o~o_o
		
		env.mapFunction("+", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				checkNumericArguments(evaluatedArgs);
				double out = evaluatedArgs[0].getFloatValue();
				for (int i=1;i<evaluatedArgs.length;i++)
					out+=evaluatedArgs[i].getFloatValue();
				return makeValue(out);
			}
			
		}
		);
		
		env.mapFunction("integer", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				
				
				if (isNumericType(evaluatedArgs[0]))
					return makeValue(evaluatedArgs[0].getIntValue());
				else if (evaluatedArgs[0].isString())
					return makeValue(Long.parseLong(evaluatedArgs[0].getString()));
				else throw new RuntimeException("argument to 'integer' must be a string or a numeric type: " + evaluatedArgs[0]);
			}
			
		}
		);
		
		env.mapFunction("double", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				
				
				if (isNumericType(evaluatedArgs[0]))
					return makeValue(evaluatedArgs[0].getFloatValue());
				else if (evaluatedArgs[0].isString())
					return makeValue(Double.parseDouble(evaluatedArgs[0].getString()));
				else throw new RuntimeException("argument to 'double' must be a string or a numeric type: " + evaluatedArgs[0]);
			}
			
		}
		);
		
		
		env.mapFunction("mod", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				checkNumericArguments(evaluatedArgs);
				long base = evaluatedArgs[0].getIntValue();
				long modulus = evaluatedArgs[1].getIntValue();
				long mod = base % modulus;
				return makeValue(mod);
			}
			
		}
		);
		
		env.mapFunction("min", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				checkNumericArguments(evaluatedArgs);
				
				double min = 0;
				for (int i=0;i<evaluatedArgs.length;i++)
				{
					if (i == 0)
						min = evaluatedArgs[i].getFloatValue();
					else
						min = Math.min(evaluatedArgs[i].getFloatValue(), min);
				}
				
				return makeValue(min);
			}
			
		}
		);
		
		
		env.mapFunction("max", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				checkNumericArguments(evaluatedArgs);
				
				double max = 0;
				for (int i=0;i<evaluatedArgs.length;i++)
				{
					if (i == 0)
						max = evaluatedArgs[i].getFloatValue();
					else
						max = Math.max(evaluatedArgs[i].getFloatValue(), max);
				}
				
				return makeValue(max);
			}
			
		}
		);
		
		env.mapFunction("-", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				checkNumericArguments(evaluatedArgs);
				double out = evaluatedArgs[0].getFloatValue();
				
				if (evaluatedArgs.length == 1)
					return makeValue(-out);
				for (int i=1;i<evaluatedArgs.length;i++)
					out-=evaluatedArgs[i].getFloatValue();
				return makeValue(out);
			}
			
		}
		);
		
		env.mapFunction("/", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				checkNumericArguments(evaluatedArgs);
				double out = evaluatedArgs[0].getFloatValue();
				
				if (evaluatedArgs.length == 1)
					return makeValue(1/out);
				for (int i=1;i<evaluatedArgs.length;i++)
					out/=evaluatedArgs[i].getFloatValue();
				return makeValue(out);
			}
			
		}
		);
		
		env.mapFunction("*", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				checkNumericArguments(evaluatedArgs);
				double out = evaluatedArgs[0].getFloatValue();
				
				for (int i=1;i<evaluatedArgs.length;i++)
					out*=evaluatedArgs[i].getFloatValue();
				return makeValue(out);
			}
			
		}
		);
		
		env.mapFunction("<", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				checkNumericArguments(evaluatedArgs);
				if (evaluatedArgs[0].getFloatValue() < evaluatedArgs[1].getFloatValue())
					return evaluatedArgs[1];
				else
					return makeValue(false);
				
				
			}
			
		}
		);
		
		env.mapFunction(">", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				checkNumericArguments(evaluatedArgs);
				if (evaluatedArgs[0].getFloatValue() > evaluatedArgs[1].getFloatValue())
					return evaluatedArgs[1];
				else
					return makeValue(false);
				
			}
			
		}
		);
		
		env.mapFunction("<=", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				checkNumericArguments(evaluatedArgs);
				if (evaluatedArgs[0].getFloatValue() <= evaluatedArgs[1].getFloatValue())
					return evaluatedArgs[1];
				else
					return makeValue(false);
				
			}
			
		}
		);
		
		env.mapFunction(">=", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				checkNumericArguments(evaluatedArgs);
				
				if (evaluatedArgs[0].getFloatValue() >= evaluatedArgs[1].getFloatValue())
					return evaluatedArgs[1];
				else
					return makeValue(false);
				
				
			}
			
		}
		);
		
		env.mapFunction("trace-label", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				
				return evaluatedArgs[0];
				
				
			}
			
		}
		);
		
		
		return env;
	}
	
	public static Value processBackQuote(Environment env, Value backArgument) throws InstantiationException, IllegalAccessException
	{
		LinkedList<Value> newValues = new LinkedList<Value>();
		Value[] backList = backArgument.getList();
		for (int j=0;j<backList.length;j++)
		{
			if (backList[j].isCommaDelimited())
			{
				
				Value nValue = env.evaluate(backList[j].clone().setCommaDelimited(false));
				
				newValues.add(nValue);
			}else
			if (backList[j].isCommaListDelimited())
			{
				Value nValue = env.evaluate(backList[j].clone().setCommaListDelimited(false));
				
				if (nValue.isList())
				{
					for (Value vk:nValue.getList())
						newValues.add(vk);
				}
				else
					newValues.add(nValue);
			}
			else if (backList[j].isList())
			{
				newValues.add(processBackQuote(env, backList[j]));
			} 
			else 
				newValues.add(backList[j]);
				
		}
		return new ListValue(newValues.toArray(new Value[0]));
	}
	
	public static Value evaluateBlock(Environment env, int start, Value[] args) throws InstantiationException, IllegalAccessException
	{
		Value out = Environment.getNull();
		for (int i = start;i<args.length;i++)
		{
			out = env.evaluate(args[i]);
			if (isBreakLike(out))
				return out;
		}
		return out;
	}
	
	// Only set allowRelativeBindingP as true if the bindingEnvironment is a child of the evaluationEnvironment
	public static Value evaluateBindingList(Environment bindingEnvironment, Environment evaluationEnvironment, Value[] bindingPairList, boolean allowRelativeBindingP) throws InstantiationException, IllegalAccessException
	{
		String name;
		Value value = Environment.getNull(), bindingPair;
		for (int i=0;i<bindingPairList.length;i++)
		{
			bindingPair = bindingPairList[i];
			if (bindingPair.isList())
			{
				if (bindingPair.getList().length == 2)
				{
					if (bindingPair.getList()[0].isIdentifier())
					{
						name = bindingPair.getList()[0].getString();
						if (allowRelativeBindingP)
							value = bindingEnvironment.evaluate(bindingPair.getList()[1]);
						else
							value = evaluationEnvironment.evaluate(bindingPair.getList()[1]);
						if (isBreakLike(value))
							return value;
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
	
	
	public static boolean isBreakLike(Value v)
	{
		return (v.isBreak() || v.isSignal() || v.isReturn() || v.isSignalOut());
	}
	
	public static boolean isNumericType(Value v)
	{
		return v.isFloat() || v.isInteger();
	}
	
	
	public static String[] getStringArrayFromValue(Value stringListValue)
	{
		if (stringListValue.isList())
		{
			String[] out = new String[stringListValue.getList().length];
			for (int i=0;i<out.length;i++)
				out[i] = stringListValue.getList()[i].getString();
			return out;
		}
		throw new RuntimeException("Incorrect Value type");
	}
	
	public static Value makeValue(int v)
	{
		return new IntegerValue(v);
	}
	
	public static Value makeValue(double v)
	{
		return new FloatValue(v);
	}
	
	public static Value makeValue(Value[] args)
	{
		return new ListValue(args);
	}
	
	public static Value makeValue(long args)
	{
		return new IntegerValue(args);
	}
	
	public static Value makeValue(String v, boolean identifier)
	{
		return new StringValue(v, identifier);
	}
	
	public static Value makeValue(String v)
	{
		return makeValue(v, false);
	}
	
	public static Value makeValue(boolean v)
	{
		if (v)
			return new IntegerValue(1);
		else
			return Environment.getNull();
	}
	
	public static Value[] subSequence(Value[] input, int start)
	{
		return subSequence(input, start, input.length);
	}
	
	public static Value[] subSequence(Value[] input, int start, int end)
	{
		Value[] out = new Value[end - start];
		for (int i=start;i<end;i++)
			out[i-start] = input[i];
		return out;
	}
	
	public static KeyValuePair<Value[], HashMap<String, Value>> getPartitionValues(Value[] args)
	{
		LinkedList<Value> normalValues = new LinkedList<Value>();
		HashMap<String, Value> keyMap = new HashMap<String, Value>();
		String prevKeyName = null;
		for (Value v:args)
		{
			if (v.isKeyName())
				prevKeyName = v.getString();
			else if (prevKeyName != null)
			{
				keyMap.put(prevKeyName, v);
				prevKeyName = null;
			}
			else
			{
				normalValues.add(v);
			}
				
		}
		return new KeyValuePair<Value[], HashMap<String, Value>>(normalValues.toArray(new Value[0]), keyMap);
	}
	
}
