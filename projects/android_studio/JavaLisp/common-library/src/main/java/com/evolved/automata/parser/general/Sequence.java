package com.evolved.automata.parser.general;
import java.util.*;

public class Sequence 
{
	Vector<GeneralizedCharacter> j_list;
	
	public Sequence()
	{
		j_list = new Vector<GeneralizedCharacter>();
	}
	
	public void add(GeneralizedCharacter newChar)
	{
		j_list.add(newChar);
	}
	
	public GeneralizedCharacter get(int i)
	{
		return j_list.get(i);
	}
	
	public void reset()
	{
		j_list.clear();
	}
	
	public int size()
	{
		return j_list.size();
	}
	
	public void clear()
	{
		j_list.clear();
	}
	
	public void removeLast()
	{
		if (j_list.size()>0)
			j_list.remove(j_list.size()-1);
	}
	
}
