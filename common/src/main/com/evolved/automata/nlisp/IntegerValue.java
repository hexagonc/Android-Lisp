package com.evolved.automata.nlisp;

import com.evolved.automata.nlisp.Value.Type;

public class IntegerValue extends Value
{
	final long _value;
	
	public IntegerValue(long value)
	{
		_value = value;
		_type = Type.INTEGER;
	}
	
	@Override
	public long getIntValue()
	{
		return _value;
	}

	@Override
	public double getFloatValue()
	{
		return _value;
	}
	
	
	@Override
	public boolean equals(Value v) {
		
		return v.isInteger() && v.getIntValue() == _value;
	}

	@Override
	public String toString() {
		
		return addQualifiers("" + _value);
	}

	@Override
	public Value clone() {
		return copyStatus(new IntegerValue(_value));
	}
}
