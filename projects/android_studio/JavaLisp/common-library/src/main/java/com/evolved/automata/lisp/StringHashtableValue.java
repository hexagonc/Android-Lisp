package com.evolved.automata.lisp;

import java.util.HashMap;
import java.util.Map;

public class StringHashtableValue extends Value
{
	HashMap<String, Value> _map;
	public StringHashtableValue(HashMap<String, Value> map)
	{
		_map = map;
		_type = Type.STRING_HASHTABLE;
	}
	
	@Override
	public HashMap<String, Value> getStringHashtable()
	{
		return _map;
	}
	
	@Override
	public int size()
	{
		return _map.size();
	}
	
	
	@Override
	public boolean equals(Value v) {
		
		return v.isStringHashtable() && v.getStringHashtable() == getStringHashtable();
	}

	@Override
	public String toString() {
		StringBuilder b = new StringBuilder("(make-string-hashtable (list ");
		
		for (String key:_map.keySet())
		{
			b.append(String.format(" (list \"%1$s\" %2$s)", key.replace("\"", "\\\""), _map.get(key).serializedForm()));
				
		}
		return b.append("))").toString();
	}

	@Override
	public Value clone() {
		
		return new StringHashtableValue(_map);
	}

	@Override
	public boolean isSerializable()
	{

		for (Map.Entry<String, Value> pair: _map.entrySet())
		{
			if (!pair.getValue().isSerializable())
				return false;
		}
		return true;
	}
}
