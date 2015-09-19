package com.evolved.automata.nlisp;

import java.util.HashMap;

public class IntHashtableValue extends Value 
{
	HashMap<Long, Value> _map;
	public IntHashtableValue(HashMap<Long, Value> map)
	{
		_map = map;
		_type = Type.INT_HASHTABLE;
	}
	
	@Override
	public HashMap<Long, Value> getIntHashtable()
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
		
		return v.isIntHashtable() && v.getIntHashtable() == getIntHashtable();
	}

	@Override
	public String toString() {
		StringBuilder b = new StringBuilder("(make-int-hashtable (list ");
		
		for (Long key:_map.keySet())
		{
			b.append(String.format(" (list %1$s %2$s)", key, _map.get(key).serializedForm()));
				
		}
		return b.append("))").toString();
	}

	@Override
	public Value clone() {
		
		return new IntHashtableValue(_map);
	}
	
	
}
