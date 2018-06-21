package com.evolved.automata;

public abstract class CorrectableException extends Exception
{
	public CorrectableException(String message)
	{
		super(message);
	}
	
	public CorrectableException(Exception cause)
	{
		super(cause);
	}
	
	public abstract void fix();
	
}
