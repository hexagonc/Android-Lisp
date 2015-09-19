package com.evolved.automata.nlisp;


public class UserObjectValue extends Value
{
	Object _obj;
	public UserObjectValue(Object obj)
	{
		_obj = obj;
		_type = Type.OBJECT;
	}
	
	@Override
	public Object getObjectValue()
	{
		return _obj;
	}

	@Override
	public boolean equals(Value v) {

		return _obj.equals(v);
	}

	@Override
	public String toString() {
		
		return _obj.toString();
	}

	@Override
	public Value clone() {
		
		return new UserObjectValue(_obj);
	}
}
