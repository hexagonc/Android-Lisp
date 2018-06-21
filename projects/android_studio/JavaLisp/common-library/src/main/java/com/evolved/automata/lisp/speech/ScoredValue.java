package com.evolved.automata.lisp.speech;

import java.util.LinkedList;

public class ScoredValue {
	public Object value;
	public double score;
	public String[] rawString;
	public LinkedList<String> rawTokens;
	public String simpleString;
	
	private ScoredValue(LinkedList<String> tokens, double s)
	{
		rawTokens = (LinkedList<String>)tokens.clone();
		score = s;
	}
	
	protected ScoredValue()
	{
		
	}
	
	
	public ScoredValue copy()
	{
		ScoredValue nValue = new ScoredValue();
		nValue.score = score;
		nValue.value = value;
		nValue.rawTokens = rawTokens;
		nValue.simpleString = simpleString;
		nValue.rawString = rawString;
		return nValue;
	}
	
	
	public ScoredValue(Object v, double s)
	{
		value = v;
		score = s;
	}
	
	private ScoredValue(String[] raw, double s)
	{
		rawString = raw;
		score = s;
	}
	
	private ScoredValue(String raw, double s)
	{
		simpleString = raw;
		score = s;
	}
	
	
	public static ScoredValue from(String[] raw)
	{
		return new ScoredValue(raw, 1);
	}
	
	public static ScoredValue from(String raw)
	{
		return new ScoredValue(raw, 1);
	}
	
	public static ScoredValue from(LinkedList<String> raw)
	{
		return new ScoredValue(raw, 1);
	}
	
	
	public ScoredValue setScore(double s)
	{
		score = s;
		return this;
	}
	
	@Override
	public String toString()
	{
		StringBuilder sbuilder = new StringBuilder();
		if (simpleString!=null)
			return simpleString;
		
		if (rawTokens!=null)
		{
			int i = 0;
			for (String token:rawTokens)
			{
				if (i == 0)
				{
					sbuilder.append(token);
				}
				else
				{
					sbuilder.append(" ").append(token);
				}
				i++;
			}
			return sbuilder.toString();
		}
		else if (rawString!=null)
		{
			int i = 0;
			String token;
			for (i = 0;i<rawString.length;i++)
			{
				token = rawString[i];
				if (i == 0)
				{
					sbuilder.append(token);
				}
				else
				{
					sbuilder.append(" ").append(token);
				}
			}
			return sbuilder.toString();
		}
		else
		{
			return value.toString();
		}
	}
}
