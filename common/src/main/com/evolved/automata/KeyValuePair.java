package com.evolved.automata;
import java.io.*;
public class KeyValuePair<Key, Value> implements Serializable
{
	static final long serialVersionUID = 1;
	Key a_Key;
	Value a_Value;
	
	public KeyValuePair()
	{
		
	}
	
	public KeyValuePair(Key key, Value value)
	{
		a_Key=key;
		a_Value=value;
	}
	
	public Key GetKey()
	{
		return a_Key;
	}
	
	public Value GetValue()
	{
		return a_Value;
	}
	
	public String toString()
	{
		return String.format("<%1$s, %2$s>", a_Key, a_Value);
	}
}
