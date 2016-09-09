package com.evolved.automata;

public class IntPair {
	private int x;
	private int y;
	public int Width;
	public int Height;
	public IntPair(int first, int second)
	{
		x=Width=first;
		y=Height=second;
		
	}
	
	public int GetFirst()
	{
		return x;
	}
	
	public int GetSecond()
	{
		return y;
	}
	
	public int GetWidth()
	{
		return GetFirst();
	}
	
	public int GetHeight()
	{
		return GetSecond();
	}
	
	public void SetFirst(int first)
	{
		x=Width=first;
	}
	
	public void SetSecond(int second)
	{
		y=Height=second;
	}
	
	
	public void SetWidth(int newWidth)
	{
		SetFirst(newWidth);
	}
	
	public void SetHeight(int newHeight)
	{
		SetSecond(newHeight);
	}
}
