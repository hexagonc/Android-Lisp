package com.evolved.automata.parser.general;

public class ControlCharacter extends GeneralizedCharacter
{
	public enum TYPE
	{
		SKIP, START_OF_PATTERN
	}
	
	TYPE _type;
	
	private ControlCharacter(TYPE type)
	{
		_type = type;
		_isControlCharacter = true;
		_isStartCharacter = type == TYPE.START_OF_PATTERN;
		_isSkipCharacter = type == TYPE.SKIP;
	}
	
	public TYPE getType()
	{
		return _type;
	}
	
	public static ControlCharacter createStartCharacter()
	{
		return new ControlCharacter(TYPE.START_OF_PATTERN);
	}
	
	public static ControlCharacter createSkipCharacter()
	{
		return new ControlCharacter(TYPE.SKIP);
	}
	
	public String toString()
	{
		return "CONTROL:" + _type.toString();
	}
}
