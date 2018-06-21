package com.evolved.automata.parser.general;

import java.util.Hashtable;

public class TextCharacter extends GeneralizedCharacter
{
	String charValue;
	
	public static final String CHARACTER_KEY = "CHARACTER_KEY"; 
	boolean singleCharP = false;
	
	public TextCharacter(String character, boolean singleCharP)
	{
		super();
		j_table = new Hashtable<String, String>();
		j_table.put(CHARACTER_KEY, character);
		charValue = character;
		this.singleCharP = singleCharP;
	}
	
	public TextCharacter(char character)
	{
		super();
		j_table = new Hashtable<String, String>();
		charValue = ""+character;
		j_table.put(CHARACTER_KEY, charValue);
		
		this.singleCharP = true;
	}
	
	public boolean isSingleCharacter()
	{
		return singleCharP;
	}
	
	
	public String getTextValue()
	{
		return charValue;
	}
	
	public String toString()
	{
		return charValue;
	}
	
	public boolean equals(Object item)
	{
		return item!=null && item instanceof TextCharacter && ((TextCharacter)item).getTextValue().equals(charValue);
	}
}
