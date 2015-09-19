package com.evolved.automata.lisp;
import java.util.*;
public class MacroTemplate extends FunctionTemplate
{

	protected boolean _isMacro = true;
	
	final String[] _formalParameters;
	final Environment _innerEnvironment;
	final Value[] _bodyArguments;
	
	public MacroTemplate(Environment innerEnv, String[] formalParameters, Value[] bodyArgs)
	{
		_innerEnvironment = new Environment(innerEnv);
		_formalParameters = formalParameters;
		_bodyArguments = bodyArgs;
	}
	
	@Override
	public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
	{
		T l = (T) new MacroTemplate(_innerEnvironment, _formalParameters, _bodyArguments);
		return l;
	}
	
	
	public Value evaluate(Environment env, boolean resume) throws InstantiationException, IllegalAccessException
	{
		LinkedList<Value> variableArgs = new LinkedList<Value>();
		boolean appendToVargs = false;
		
		
		for (int i=0;i<_formalParameters.length;i++)
		{
			String name = _formalParameters[i];
			if (name.equals(_VAR_ARGNAME))
			{
				appendToVargs = true;
			}
			
			if (_actualParameters.length > i)
			{
				_innerEnvironment.mapValue(name, _actualParameters[i]);
				if (appendToVargs)
					variableArgs.add(_actualParameters[i]);
			}
			else
			{
				_innerEnvironment.mapValue(name, Environment.getNull());
			}
		}
		
		if (appendToVargs)
		{
			for (int i= _formalParameters.length;i<_actualParameters.length;i++)
			{
				variableArgs.add(_actualParameters[i]);
			}
			_innerEnvironment.mapValue(_VAR_ARGNAME, new ListValue(variableArgs.toArray(new Value[0])));
		}
		
		Value result = Environment.getNull();
		for (int i=0;i<_bodyArguments.length;i++)
		{
			result = _innerEnvironment.evaluate(_bodyArguments[i]);
			
			if (result.isReturn())
			{
				result.setReturn(false);
				break;
			}
			if (result.isSignal() || result.isSignalOut())
				break;
			
		}
		if (result.isBreak())
			result.setBreak(false);
		return result;
	}
	
//	Value processBackQuote(Environment env, Value backArgument) throws InstantiationException, IllegalAccessException
//	{
//		LinkedList<Value> newValues = new LinkedList<Value>();
//		Value[] backList = backArgument.getList();
//		for (int j=0;j<backList.length;j++)
//		{
//			if (backList[j].isBackQuoted() && backList[j].isList())
//			{
//				newValues.add(processBackQuote(env, backList[j]));
//			}else if (backList[j].isCommaDelimited() && backList[j].isIdentifier())
//			{
//				
//				Value nValue = env.evaluate(backList[j].clone().setCommaDelimited(false));
//				
//				newValues.add(nValue);
//			}
//			else if (backList[j].isCommaListDelimited() && backList[j].isIdentifier())
//			{
//				Value nValue = env.evaluate(backList[j].clone().setCommaListDelimited(false));
//				
//				if (nValue.isList())
//				{
//					for (Value vk:nValue.getList())
//						newValues.add(vk);
//				}
//				else
//					newValues.add(nValue);
//			}
//			else
//				newValues.add(backList[j]);
//				
//		}
//		return new ListValue(newValues.toArray(new Value[0]));
//	}

}
