package com.evolved.automata.nlisp;

import com.evolved.automata.nlisp.Value.Type;

public class FloatValue extends Value
{
	final double _value;
	
	public FloatValue(double v)
	{
		_value = v;
		_type = Type.FLOAT;
	}
	
	@Override
	public double getFloatValue()
	{
		return _value;
	}
	
	@Override
	public long getIntValue()
	{
		return (long)_value;
	}
	
	
	@Override
	public boolean equals(Value v) {
		
		return v.isFloat() && v.getFloatValue() == _value;
	}
	
	

	@Override
	public String toString() {
		
		return addQualifiers("" + _value);
	}

	@Override
	public Value clone() {
		
		return copyStatus(new FloatValue(_value));
	}

}
