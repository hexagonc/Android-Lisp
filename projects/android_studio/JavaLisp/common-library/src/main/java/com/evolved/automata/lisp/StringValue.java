package com.evolved.automata.lisp;

public class StringValue extends Value {

	String _value = null;
	final boolean _isIdentifier;
	
	public StringValue(String v, boolean identifier)
	{
		_type = Type.STRING;
		_value = v;
		_isIdentifier = identifier;
	}

	@Override
	public boolean equals(Value v) {
	
		return v.isString() && v.getString().equals(_value);
	}
	
	@Override
	public boolean isIdentifier()
	{
		return _isIdentifier;
	}
	
	
	@Override
	public int size()
	{
		return _value.length();
	}
	
	@Override
	public String getString()
	{
		return _value;
	}

	@Override
	public String toString() {
		if (isIdentifier())
			return addQualifiers(_value);
		else
			return addQualifiers("\"" + _value.replaceAll("\\\\", "\\\\").replace("\"", "\\\"") + "\"");
	}

	@Override
	public Value clone() {
		
		return copyStatus(new StringValue(_value, _isIdentifier));
	}
	
	
}
