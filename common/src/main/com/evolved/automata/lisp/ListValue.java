package com.evolved.automata.lisp;


public class ListValue extends Value
{
	Value[] _values = null;
	public ListValue(Value[] values)
	{
		_type = Type.LIST;
		_values = values;
	}
	
	@Override
	public Value[] getList()
	{
		return _values;
	}
	
	@Override
	public int size()
	{
		return _values.length;
	}
	

	@Override
	public boolean equals(Value v) {
		if (v.isList() && v.getList().length == _values.length)
		{
			for (int i=0;i<_values.length;i++)
			{
				if (!_values[i].equals(v.getList()[i]))
					return false;
			}
			return true;
		}
		return false;
	}

	@Override
	public String toString() {
		if (_values.length==0)
			return "()";
		StringBuilder builder = new StringBuilder("(" + _values[0].toString());
		
		for (int i=1;i<_values.length;i++)
		{
			builder.append(" ");
			builder.append(_values[i].toString());
		}
		builder.append(")");
		return addQualifiers(builder.toString());
	}

	@Override
	public Value clone() {
		Value[] l = new Value[_values.length];
		for (int i=0;i<l.length;i++)
			l[i] = _values[i];
		return copyStatus(new ListValue(l));
	}
	
	
}
