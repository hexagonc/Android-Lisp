package com.evolved.automata.lisp;

import java.util.HashMap;

public abstract class Value {
	public static enum Type
	{
		STRING, INTEGER, FLOAT, LIST, LAMBDA, NULL, STRING_HASHTABLE, INT_HASHTABLE, OBJECT
	}
	
	public static enum ExitAttribute
	{
		BREAK, CONTINUATION, RETURN, SIGNAL_OUT, SIGNAL, NORMAL
	}
	
	ExitAttribute _attrib = ExitAttribute.NORMAL;
	Type _type;
	
	boolean _isQuote = false;
	boolean _isBackQuote = false;
	boolean _isCommaDelimited = false;
	boolean _isCommaListDelimited = false;
	FunctionTemplate _continuingFunction = null;
	Value _signalValue = null;
	Value _signalName = null;
	Value _metaData = null;
	FunctionTemplate _cachedFunction = null;
	
	boolean _hasMetaData = false;
	
	public Value[] getList()
	{
		return null;
	}
	
	public Value setMetaData(Value data)
	{
		_metaData = data;
		_hasMetaData = true;
		return this;
	}
	
	/**
	 * Make the meta-data accessible at least one more time after the clearing it so that it can be
	 * accessed after being passed out of a function
	 */
	public Value getMetaData()
	{
		Value out = _metaData;
		if (_hasMetaData == false)
			_metaData = null;
		return out;
	}
	
	
	public boolean hasMetaData()
	{
		if (_metaData != null)
			return true;
		else
			return _hasMetaData;
	}
	
	/**
	 * This doesn't immediately make the meta-data accessible but from this point forward, you can 
	 * only access the meta-data one more time
	 * @return
	 */
	public Value clearMetaData()
	{
		_hasMetaData = false;
		return this;
	}
	
	public Value appendItem(Value v)
	{
		
		return  null;
	}
	
	public boolean isIdentifier()
	{
		return false;
	}
	
	public boolean isKeyName()
	{
		return isIdentifier() && getString().startsWith(":");
	}
	
	public FunctionTemplate getCachedFunctionTemplate()
	{
		return _cachedFunction;
	}
	
	public void setCachedFunctionTemplate(FunctionTemplate cachedFunction)
	{
		_cachedFunction = _cachedFunction;
	}
	
	public boolean isContinuation()
	{
		return _attrib ==  ExitAttribute.CONTINUATION;
	}
	
	public Value setContinuation(boolean status, FunctionTemplate template)
	{
		if (status)
		{
			_continuingFunction = template;
			_attrib = ExitAttribute.CONTINUATION;
		}
		else
		{
			_attrib = ExitAttribute.NORMAL;
			_continuingFunction = null;
		}
		return this;
	}
	
	public int size()
	{
		return 0;
	}
	
	public FunctionTemplate getContinuingFunction()
	{
		return _continuingFunction;
	}
	
	public boolean isBreak()
	{
		return _attrib ==  ExitAttribute.BREAK;
	}
	
	public Value setBreak(boolean status)
	{
		if (status)
			_attrib = ExitAttribute.BREAK;
		else
			_attrib = ExitAttribute.NORMAL;
		return this;
	}
	
	public boolean isReturn()
	{
		return _attrib ==  ExitAttribute.RETURN;
	}
	
	public Value setReturn(boolean status)
	{
		if (status)
			_attrib = ExitAttribute.RETURN;
		else
			_attrib = ExitAttribute.NORMAL;
		return this;
	}
	
	public boolean isSignal()
	{
		return _attrib ==  ExitAttribute.SIGNAL;
	}
	
	/**
	 * Function for changing the natural of Value from or to a signal
	 * @param status when true, this Value is changed into a signal, when false, this value is normal
	 * @param name - Signal key, can be any type, even NULL.  Signal name defines where the Signal can be caught at
	 * @param value - Optional Signal value
	 * @return
	 */
	public Value setSignal(boolean status, Value name, Value value)
	{
		if (status)
			_attrib = ExitAttribute.SIGNAL;
		else
			_attrib = ExitAttribute.NORMAL;
		_signalName = name;
		_signalValue = value;
		return this;
	}
	
	public Value getSignalValue()
	{
		return _signalValue;
	}
	
	public Value getSignalName()
	{
		return _signalName;
	}
	
	public boolean isSignalOut()
	{
		return _attrib ==  ExitAttribute.SIGNAL_OUT;
	}
	
	public Value setSignalOut(boolean status)
	{
		if (status)
			_attrib = ExitAttribute.SIGNAL_OUT;
		else
			_attrib = ExitAttribute.NORMAL;
		return this;
	}
	
	public boolean isUserObject()
	{
		return _type == Type.OBJECT;
	}
	
	public Object getObjectValue()
	{
		return null;
	}
	
	public boolean isList()
	{
		return _type ==  Type.LIST;
	}
	
	public boolean isString()
	{
		return _type ==  Type.STRING;
	}
	
	public boolean isStringHashtable()
	{
		return _type ==  Type.STRING_HASHTABLE;
	}
	
	public boolean isIntHashtable()
	{
		return _type ==  Type.INT_HASHTABLE;
	}
	
	public boolean isHashtable()
	{
		return _type ==  Type.STRING_HASHTABLE || _type ==  Type.INT_HASHTABLE;
	}
	
	public boolean isInteger()
	{
		return _type ==  Type.INTEGER;
	}
	
	public boolean isFloat()
	{
		return _type ==  Type.FLOAT;
	}
	
	public boolean isNull()
	{
		return _type ==  Type.NULL;
	}
	
	public Value setNull()
	{
		_type = Type.NULL;
		return this;
	}
	
	public boolean isLambda()
	{
		return _type ==  Type.LAMBDA;
	}
	
	
	public boolean isQuoted()
	{
		return _isQuote;
	}
	
	public boolean isBackQuoted()
	{
		return _isBackQuote;
	}
	
	public boolean isCommaDelimited()
	{
		return _isCommaDelimited;
	}
		
	public boolean isCommaListDelimited()
	{
		return _isCommaListDelimited;
	}
	
	public Value setQuoted(boolean set)
	{
		_isQuote = set;
		return this;
	}
	
	public Value setBackQuoted(boolean set)
	{
		_isBackQuote = set;
		return this;
	}
	
	public Value setCommaDelimited(boolean set)
	{
		_isCommaDelimited = set;
		return this;
	}
		
	public Value setCommaListDelimited(boolean set)
	{
		_isCommaListDelimited = set;
		return this;
	}
	
	public Type getType()
	{
		return _type;
	}
	
	public String getString()
	{
		return null;
	}
	
	public long getIntValue()
	{
		return 0;
	}
	
	public double getFloatValue()
	{
		return 0;
	}
	
	public FunctionTemplate getLambda()
	{
		return null;
	}
	
	public HashMap<String, Value> getStringHashtable()
	{
		return null;
	}
	
	public HashMap<Long, Value> getIntHashtable()
	{
		return null;
	}
	
	
	protected Value copyStatus(Value v)
	{
		
		v._isBackQuote = _isBackQuote;
		v._isCommaDelimited = _isCommaDelimited;
		v._isCommaListDelimited = _isCommaListDelimited;
		v._isQuote = _isQuote;
		return v;
	}
	
	
	
	public abstract boolean equals(Value v);
	public abstract String toString();
	public abstract Value clone();
	
	@Override
	public boolean equals(Object obj)
	{
		if (obj instanceof Value)
			return equals((Value)obj);
		else
			return false;
	}
	
	public String serializedForm()
	{
		return toString();
	}
	
	public String addQualifiers(String value)
	{
		StringBuilder s = new StringBuilder();
		if (_isBackQuote)
			s.append("`");
		if (_isQuote)
			s.append("'");
		if (_isCommaDelimited)
			s.append(",");
		if (_isCommaListDelimited)
			s.append(",@");
		s.append(value);
		return s.toString();
	}
}
