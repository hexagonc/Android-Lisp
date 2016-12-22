package com.evolved.automata.lisp;
import java.util.*;


public class Environment 
{
	public static final boolean _DEBUG = false;
	
	volatile HashMap<String, Value> _valueMap = new HashMap<String, Value>();
	volatile HashMap<String, FunctionTemplate> _functionMap = new HashMap<String, FunctionTemplate>();
	volatile HashMap<String, MacroTemplate> _macroMap = new HashMap<String, MacroTemplate>();
	volatile HashSet<String> _userFunctionSet = new HashSet<String>();
	volatile HashMap<String, Value> _environmentProperties = new HashMap<String, Value>();
	boolean _throwExceptionOnUndefinedP = true;
	volatile Environment _parentEnv = null;
	
	static LinkedList<String> logs = new LinkedList<String>();
	
	public final String _NULL_NAME = "F";
	
	
	
	public static class ParserResult
	{
		public Value argument = null;
		public int endIndex=0;
		
		public ParserResult(Value arg, int end)
		{
			argument=arg;
			endIndex=end;
		}
	}
	
	
	public Environment()
	{
		mapValue(_NULL_NAME, getNull());
		
		// 
	}
	
	
	
	public Environment(Environment parent)
	{
		_parentEnv = parent;
	}
	
	public static Value getNull()
	{
		return new Value()
		{

			@Override
			public boolean equals(Value v) {
				
				return v.isNull();
			}
			
			@Override
			public String toString()
			{
				return "NULL";
			}

			@Override
			public String serializedForm()
			{
				return "F";
			}

			
			@Override
			public Value clone() {
				
				return getNull();
			}
			
			@Override
			public boolean isNull()
			{
				return true;
			}
		};
	}
	
	
	public static void warn(String message)
	{
		System.out.println(message);
	}
	
	public synchronized static void log(String message){
		logs.add(message);
	}
	
	public static void flush()
	{
		logs.clear();
	}
	
	public synchronized Value mapValue(String name, Value v)
	{
		_valueMap.put(name, v);
		return v;
	}
	
	public synchronized Value mapEnvironmentProperty(String name, Value v)
	{
		_environmentProperties.put(name, v);
		return v;
	}
	
	public synchronized Value getVariableValue(String name)
	{
		if (_valueMap.containsKey(name))
			return _valueMap.get(name);
		if (_parentEnv!=null)
			return _parentEnv.getVariableValue(name);
		return null;
	}
	
	public synchronized Value getEnvironmentProperty(String name)
	{
		if (_environmentProperties.containsKey(name))
			return _environmentProperties.get(name);
		if (_parentEnv!=null)
			return _parentEnv.getEnvironmentProperty(name);
		return null;
	}
	
	public synchronized FunctionTemplate mapFunction(String name, FunctionTemplate f)
	{
		f.setName(name);
		_functionMap.put(name,  f);
		return f;
	}

	public synchronized FunctionTemplate removeFunction(String name)
	{
		FunctionTemplate  f = _functionMap.remove(name);

		return f;
	}

	
	public synchronized void mapMacro(String name, MacroTemplate f)
	{
		_macroMap.put(name,  f);
	}
	
	public synchronized FunctionTemplate getFunction(String name) throws InstantiationException, IllegalAccessException
	{
		if (hasFunction(name))
			return (FunctionTemplate)_functionMap.get(name).clone();
		if (_parentEnv!=null)
			return _parentEnv.getFunction(name);
		return null;
	}
	
	public synchronized boolean hasFunction(String name)
	{
		return _functionMap.containsKey(name);
	}
	
	public synchronized MacroTemplate getMacro(String name) throws InstantiationException, IllegalAccessException
	{
		if (hasMacro(name))
			return (MacroTemplate)_macroMap.get(name).clone();
		if (_parentEnv!=null)
			return _parentEnv.getMacro(name);
		return null;
	}
	
	public synchronized boolean hasMacro(String name)
	{
		return _macroMap.containsKey(name);
	}
	
	public synchronized boolean hasVariable(String name)
	{
		return _valueMap.containsKey(name);
	}
	
	public synchronized boolean hasProperty(String name)
	{
		return _environmentProperties.containsKey(name);
	}
	
	public Value evaluate(Value v) throws InstantiationException, IllegalAccessException
	{
		return evaluate(v, false);
	}
	
	public Environment getRootEnvironment()
	{
		Environment start = this;
		while (start._parentEnv!=null)
			start = start._parentEnv;
		return start;
	}

	public Value loadFromFileLines(String[] dataLines)
	{
		Value result = null;
		try
		{

			StringBuilder command = new StringBuilder();
			LinkedList<Value> parsedResult = null;

			for (String lineinput:dataLines)
			{
				if (lineinput.trim().length()==0 || lineinput.trim().startsWith(";"))
					continue;
				command.append(" ");
				command.append(lineinput);

				try
				{
					parsedResult = Environment.parse(command.toString(), true);
				}
				catch (IncompleteLispExpressionException icl)
				{
					continue;
				}
				command = new StringBuilder();
				for (Value comp:parsedResult)
				{
					result = evaluate(comp, false);
				}
			}
		}
		catch (Exception e)
		{
			if (e instanceof RuntimeException)
				throw (RuntimeException)e;
			else
				throw new RuntimeException(e.toString());
		}
		return result;
	}

	public Value evaluate(Value v, boolean resume) throws InstantiationException, IllegalAccessException
	{
		
		if (Thread.currentThread().isInterrupted())
			throw new RuntimeException("Done");
		if (v.isNull())
			return v;
		if (v.isCommaDelimited() || v.isCommaListDelimited())
		{
			v = v.clone();
			v.setQuoted(false);
			v.setBackQuoted(false);
			v.setCommaDelimited(false);
			v.setCommaListDelimited(false);
			return v;
		}
		if (v.isIdentifier())
		{
			String name = v.getString();
			if (v.isKeyName())
				return v;
			Value val = getVariableValue(name);
			if (val !=null)
				return val;
			else if (_throwExceptionOnUndefinedP)
				throw new RuntimeException("Undefined variable: " + name);
			return v;
		}
		
		if (v.isList())
		{
			Value[] values = v.getList();
			if (values.length>0)
			{
				Value first = values[0];
				if (first.isIdentifier() && !first.isKeyName())
				{
					MacroTemplate mtemplate = getMacro(first.getString());
					if (mtemplate!=null)
					{
						Value[] actualArgs = new Value[values.length-1];
						for (int i=0;i<actualArgs.length;i++)
							actualArgs[i] = values[i+1];
						mtemplate.setActualParameters(actualArgs);
						Value result = mtemplate.evaluate(this, resume);
						
						return evaluate(result);
					}
					
					FunctionTemplate template = getFunction(first.getString());
					if (template!=null)
					{
						Value[] actualArgs = new Value[values.length-1];
						for (int i=0;i<actualArgs.length;i++)
							actualArgs[i] = values[i+1];
						template.setActualParameters(actualArgs);
						Value result = template.evaluate(this, resume);
						
						return result;
					}
					else if (_throwExceptionOnUndefinedP)
						throw new RuntimeException("Undefined function: " + first.getString());
				}
			}
		}
			
		return v;
	}
	
	public Environment getParent()
	{
		return _parentEnv;
	}
	
	public Value evaluate(String expression, boolean errorOnIncompleteParserP) throws InstantiationException, IllegalAccessException
	{
		Value out = null;
		for (Value v:Environment.parse(expression, errorOnIncompleteParserP))
		{
			out = evaluate(v);
		}
		return out;
	}
	
	
	public static LinkedList<Value> parse(String input, boolean errorOnIncompleteParserP)
	{
		LinkedList<Value> resultList = new LinkedList<Value>();
		int index = 0, lastIndex = 0;
		ParserResult result = parse(input, index);
		while (result != null && result.argument!=null)
		{
			resultList.add(result.argument);
			lastIndex = index;
			index = result.endIndex;
			if (index<input.length())
				result = parse(input, index);
			else
				break;
		}
		if (result == null && errorOnIncompleteParserP)
			throw new IncompleteLispExpressionException("Parse error near: " + input.substring(lastIndex));
		return resultList;
	}
	
	
	private static ParserResult parse(String input, int start)
	{
		
				
		//leading whitespace
		while (start<input.length() && Character.isWhitespace(input.charAt(start)))
		{
			start++;
		}
		
		// Can't match for tokens at the end of the string
		if (start==input.length())
		{
			return new ParserResult(null, start);
		}
		
		
		// Check for quoted
		if (input.charAt(start) == '\'')
		{
			ParserResult rest = parse(input, start+1);
			if (rest!=null && rest.argument!=null)
			{
				
				return new ParserResult(new ListValue(new Value[]{new StringValue("quote", true), rest.argument}), rest.endIndex);
			}
		}
		
		// Check for comma delimited
		if (input.charAt(start) == '`')
		{
			ParserResult rest = parse(input, start+1);
			if (rest!=null && rest.argument!=null)
			{
				return new ParserResult(new ListValue(new Value[]{new StringValue("back-quote", true), rest.argument}), rest.endIndex);
			}
		}
		
		// Check for comma list delimited
		if (input.charAt(start) == ',' && start<input.length()-2 && input.charAt(start+1) == '@')
		{
			ParserResult rest = parse(input, start+2);
			if (rest!=null && rest.argument!=null)
			{
				rest.argument.setCommaListDelimited(true);
				return rest;
			}
		}
		
		// Check for comma delimited
		if (input.charAt(start) == ',' && start<input.length()-1)
		{
			ParserResult rest = parse(input, start+1);
			if (rest!=null && rest.argument!=null)
			{
				rest.argument.setCommaDelimited(true);
				return rest;
			}
		}
				
		// Try numeric
		int index=start;
		char c;
		StringBuilder num = new StringBuilder();
		boolean contains_decimal=false, contains_integer=false, contains_fraction=false;
		boolean contains_negative=false;
		boolean contains_exponential = false;
		while (index<input.length())
		{
			c = input.charAt(index);
			if (Character.isDigit(c))
			{
				if (contains_decimal)
					contains_fraction=true;
				if (!contains_fraction)
					contains_integer=true;
				num.append(c);
				
			}
			else if (c=='-')
			{
				if (!contains_negative&&(contains_exponential && !contains_integer || !contains_integer&&!contains_decimal&&!contains_fraction))
				{
					contains_negative=true;
				}
				else
					break;
				
				num.append(c);
				
			}
			else if (c=='.')
			{
				if (!contains_decimal)
				{
					contains_decimal=true;
					num.append(c);
				}
				else
					break;
				
			}
			else if (c == 'e' || c == 'E')
			{
				if (!contains_exponential && contains_integer)
				{
					contains_exponential = true;
					contains_negative = false;
					contains_integer = false;
					num.append(c);
				}
				else
					break;
			}
			else if (Character.isWhitespace(c) || c=='(' || c==')')
			{
				if (contains_fraction || (contains_integer && !contains_decimal))
				{
					if (contains_fraction)
						return new ParserResult(new FloatValue(Double.parseDouble(num.toString())), index);
					else
						return new ParserResult(new IntegerValue(Long.parseLong(num.toString())), index);
				}
				else
					break;
				
			}
			else
			{
				break;
			}
			if (index==input.length()-1)
			{
				if (contains_fraction || (contains_integer && !contains_decimal))
				{
					if (contains_fraction)
						return new ParserResult(new FloatValue(Double.parseDouble(num.toString())), index+1);
					else
						return new ParserResult(new IntegerValue(Long.parseLong(num.toString())), index+1);
				}
			}
			index++;
		}
		
		// s-expression
		index=start;
		LinkedList<Value> listArgs = new LinkedList<Value>();
		ParserResult result=null;
		
		
		if (input.charAt(index)=='(')
		{
			index++;
			while (index<input.length())
			{
				if (Character.isWhitespace(input.charAt(index)))
				{
					index++;
					continue;
				}
				else if (input.charAt(index) == ')')
				{
					return new ParserResult(new ListValue(listArgs.toArray(new Value[0])), index+1);
				}
				
				result = parse(input, index);
				
				if (result!=null && result.argument!=null)
				{
					listArgs.add(result.argument);
					index = result.endIndex;
				}
				else
					return null;
			}
			return null;
		}
		
		// String
		index=start;
		boolean previous_delimiter=false;
		StringBuilder sBuilder = new StringBuilder();
		
		if (input.charAt(index)=='\"')
		{
			index++;
			while (index<input.length())
			{
				c = input.charAt(index);
				if (c=='\"')
				{
					if (previous_delimiter)
					{
						previous_delimiter=false;
						sBuilder.append(c);
					}
					else
					{
						return new ParserResult(new StringValue(sBuilder.toString(), false), index+1);
					}
				}
				else if (c == '\\')
				{
					if (previous_delimiter)
						sBuilder.append(c);
					previous_delimiter=!previous_delimiter;
				}
				else
				{
					if (previous_delimiter)
					{
						previous_delimiter=false;
					}
					sBuilder.append(c);
				}
				index++;
			}
		}
			
		// Identifiers
		index=start;
		
		StringBuilder id = new StringBuilder();
		c = input.charAt(index);
		if (!Character.isDigit(c) && c!=')') // already implied that it isn't '(' or '"'
		{
			id.append(c);
			index++;
			
			while (index<input.length())
			{
				c = input.charAt(index);
				if (!Character.isWhitespace(c) && c!=')' && c!='(' && c!='\"')
				{
					id.append(c);
				}
				else
				{
					StringValue out = new StringValue(id.toString(), true);
					
					return new ParserResult(out, index);
				}
				index++;
			}
			StringValue out = new StringValue(id.toString(), true);
			
			return new ParserResult(out, index+1);
		}
		return null;
	}
	
	
}
