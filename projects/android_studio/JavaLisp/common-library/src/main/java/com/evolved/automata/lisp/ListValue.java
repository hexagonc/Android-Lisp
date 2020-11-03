package com.evolved.automata.lisp;

import java.util.ArrayList;
import java.util.Map;


public class ListValue extends Value
{
	Value[] _values = null;
	ArrayList<Value> _backingStructure = null;
	boolean _listValidP = true;
	public ListValue(Value[] values)
	{
		_type = Type.LIST;
		_values = values;
		_backingStructure = new ArrayList<Value>(values.length);
		for (int i=0;i<values.length;i++)
			_backingStructure.add(values[i]);
	}
	
	@Override
	public synchronized Value appendItem(Value v)
	{
		_listValidP = false;
		_backingStructure.add(v);
		return this;
	}
	
	@Override
	public synchronized Value[] getList()
	{
		if (_listValidP)
			return _values;
		else
		{
			_listValidP = true;
			return _values = _backingStructure.toArray(new Value[0]);
		}
	}
	
	@Override
	public synchronized int size()
	{
		if (_listValidP)
			return _values.length;
		else
		{
			_listValidP = true;
			return (_values = _backingStructure.toArray(new Value[0])).length;
		}
		
	}
	

	@Override
	public synchronized boolean equals(Value v) {
		if (!_listValidP)
		{
			_listValidP = true;
			_values = _backingStructure.toArray(new Value[0]);
		}
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
	public synchronized String toString() {
		if (!_listValidP)
		{
			_listValidP = true;
			_values = _backingStructure.toArray(new Value[0]);
		}
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
	public String serializedForm()
	{
		if (!_listValidP)
		{
			_listValidP = true;
			_values = _backingStructure.toArray(new Value[0]);
		}
		if (_values.length==0)
			return "()";

		StringBuilder builder = new StringBuilder();
		if (_values[0].isIdentifier()){
			builder.append("(" + _values[0].serializedForm());
		}
		else {
			builder.append("(list " + _values[0].serializedForm());
		}

		
		for (int i=1;i<_values.length;i++)
		{
			builder.append(" ");
			builder.append(_values[i].serializedForm());
		}
		builder.append(")");
		return addQualifiers(builder.toString());
	}
	

	@Override
	public synchronized Value clone() {
		if (!_listValidP)
		{
			_listValidP = true;
			_values = _backingStructure.toArray(new Value[0]);
		}
		Value[] l = new Value[_values.length];
		for (int i=0;i<l.length;i++)
			l[i] = _values[i];
		return copyStatus(new ListValue(l));
	}

	@Override
	public boolean isSerializable()
	{

		for (Value v:_values)
		{
			if (!v.isSerializable())
				return false;
		}
		return true;
	}
	
}
