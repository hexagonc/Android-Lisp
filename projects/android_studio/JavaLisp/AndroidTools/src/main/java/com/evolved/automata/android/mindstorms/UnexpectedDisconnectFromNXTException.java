package com.evolved.automata.android.mindstorms;

public class UnexpectedDisconnectFromNXTException extends RuntimeException 
{
	String _nxtName;
	public UnexpectedDisconnectFromNXTException(String message, String nxt)
	{
		super(message);
		_nxtName = nxt;
	}
	
	public UnexpectedDisconnectFromNXTException(Exception ex, String nxt)
	{
		super(ex);
		_nxtName = nxt;
	}
	
	
	public String getDeviceName()
	{
		return _nxtName;
	}
}
