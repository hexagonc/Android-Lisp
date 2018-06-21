package com.evolved.automata.parser.general;
import java.util.*;
public class GeneralizedCharacter 
{

	
	protected boolean _isControlCharacter =  false;
	protected boolean _isStartCharacter = false;
	protected boolean _isSkipCharacter = false;
	protected Hashtable<String, String> j_table;
	HashMap<Matcher, HashSet<String>> _tags;
	
	protected GeneralizedCharacter()
	{
		_tags = new HashMap<Matcher, HashSet<String>>();
	}
	
	public boolean isControlCharacter()
	{
		return _isControlCharacter;
	}
	
	public boolean isStartCharacter()
	{
		return _isStartCharacter;
	}
	
	public boolean isSkipCharacter()
	{
		return _isSkipCharacter;
	}
	
	public GeneralizedCharacter(Hashtable<String, String> gchar)
	{
		j_table = gchar;
	}
	
	
	
	
	
	public Hashtable<String, String> getData()
	{
		return j_table;
	}
	
	public GeneralizedCharacter getTextForm()
	{

		if (j_table.containsKey(TextCharacter.CHARACTER_KEY))
		{
			String arg = j_table.get(TextCharacter.CHARACTER_KEY);
			return new TextCharacter(arg, false);
		}
		return null; 
	}
	
	public String toString()
	{
		if (j_table.containsKey(TextCharacter.CHARACTER_KEY))
		{
			 
			return j_table.get(TextCharacter.CHARACTER_KEY);
		}
		else
			if (j_table!=null)
				return j_table.toString();
		return null; 
	}
	
	public void addTag(Matcher parent, String tag)
	{
		HashSet<String> tags = _tags.get(parent);
		if (tags == null)
		{
			_tags.put(parent, tags = new HashSet<String>());
		}
			
		tags.add(tag);
	}
	
	public void removeTags(Matcher m)
	{
		_tags.remove(m);
	}
	
	public void takeoverTags(Matcher source, Matcher target)
	{
		HashSet<String> s = _tags.get(source);
		if (s!=null)
		{
			HashSet<String> prior = _tags.get(target);
			if (prior == null)
				_tags.put(target, s);
			else
				prior.addAll(s);
			_tags.remove(source);
		}
	}
	
	public HashSet<String> getTagSet()
	{
		HashSet<String> total = new HashSet<String>();
		for (Matcher m:_tags.keySet())
			total.addAll(_tags.get(m));
		return total;
	}
	
	public boolean hasTagsFrom(Matcher m)
	{
		
		return m!=null && _tags.containsKey(m);
	}
}
