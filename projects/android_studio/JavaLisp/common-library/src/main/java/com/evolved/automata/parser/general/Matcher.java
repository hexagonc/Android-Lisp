package com.evolved.automata.parser.general;


import java.util.HashMap;
import java.util.LinkedList;



public abstract class Matcher {
	
	public String _grammarComponent;
	PatternParser.GlobalState _global;
	PatternParser.LocalState _local;
	boolean _finalized = false;
	int _endIndex=0;
	long _referenceId;
	boolean _isNonterminal = false;
	boolean _isAlternation = false;
	boolean _isTerminal = false;
	Matcher _templateParent;
	int _templateIndexInParent = 0;
	LinkedList<Matcher> _actualChildren = new LinkedList<Matcher>();
	String _tag;
	HashMap<String, String> _childOverrides = null;
	
	
	public void setTemplateParameters(Matcher parent, int indexInParent)
	{
		_templateParent= parent;
		_templateIndexInParent = indexInParent;
	}
	
	public Matcher(String component)
	{
		String[] components = Parser.partitionTag(component);
		_grammarComponent = components[0];
		_tag = components[1];
		_referenceId = System.currentTimeMillis();
		
	}
	
	public Matcher(PatternParser.GlobalState global, PatternParser.LocalState local, String grammarComponent)
	{
		_grammarComponent = grammarComponent;
		_global = global;
		_local = local;
		if (_local!=null)
			_endIndex = _local._startIndex;
		else
			_endIndex = 0;
		_referenceId = System.currentTimeMillis();
	}

	
	
	
	public String getPatternToMatch()
	{
		return _grammarComponent;
	}
	
	public boolean isNonTerminal()
	{
		return _isNonterminal;
	}
	
	public boolean isAlternation()
	{
		return _isAlternation;
	}
	
	public boolean isTerminal()
	{
		return _isTerminal;
	}
	
	public boolean isFinalized()
	{
		return _finalized;
	}
	
	public boolean isFinalized(boolean reset)
	{
		return _finalized && ((reset && !(_finalized = false)) || !reset);
	}
	
	public int getEndIndex()
	{
		return _endIndex;
	}
	
	public Matcher getParent()
	{
		Matcher parent = null;
		if (_templateParent!=null || _local == null)
		{
			if (_templateParent != null && _local == null)
				parent = _templateParent.clone(_global, null);
			if (_local == null)
			{
				_local = new PatternParser.LocalState(parent, _templateIndexInParent, 0);
			}
		}
		if (_local._parent!=null)
			return _local._parent;
		else
			return null;
	}
	
	protected void finalize()
	{
		
		_finalized=true;

	}
	
	protected  LinkedList<Matcher> updateFromParseSuccess(int childIndex, int endIndex)
	{
		return null;
	}
	
	protected  LinkedList<Matcher> updateFromParseFailure(int startIndex)
	{
		
		Matcher parent = null;
		if (_templateParent!=null || _local == null)
		{
			if (_templateParent != null && _local == null)
				parent = _templateParent.clone(_global, null);
			if (_local == null)
			{
				_local = new PatternParser.LocalState(parent, _templateIndexInParent, 0);
			}
		}
		
		GeneralizedCharacter gchar = null;
		for (int i=_local._startIndex;i<_endIndex;i++)
		{
			gchar = _global._buffer.get(i);
			killChildTags(gchar);
		}
		
		if (_local._parent!=null)
			return _local._parent.updateFromParseFailure(_local._indexInParent);
		else
			return null;
	}
	
	public String buildSuccessPath(String prior)
	{
		Matcher parent = null;
		if (_templateParent!=null || _local == null)
		{
			if (_templateParent != null && _local == null)
				parent = _templateParent.clone(_global, null);
			if (_local == null)
			{
				_local = new PatternParser.LocalState(parent, _templateIndexInParent, 0);
			}
		}
		
		if (_local._parent==null)
			return ((prior.length()>0)?prior+":":"")+_grammarComponent;
		else
			return _local._parent.buildSuccessPath(((prior.length()>0)?prior+":":"")+_grammarComponent);
	}
	
	public void setGlobalState(PatternParser.GlobalState global)
	{
		_global = global;
	}
	
	public abstract LinkedList<Matcher> match();

	public abstract Matcher clone(PatternParser.GlobalState global, PatternParser.LocalState local);
	
	protected LinkedList<Matcher> onInvalidGrammar(int indexInParent)
	{
		Matcher parent = null;
		if (_templateParent!=null || _local == null)
		{
			if (_templateParent != null && _local == null)
				parent = _templateParent.clone(_global, null);
			if (_local == null)
			{
				_local = new PatternParser.LocalState(parent, _templateIndexInParent, 0);
			}
		}
		
		if (_local._parent !=null)
		{
			return _local._parent.onInvalidGrammar(_local._indexInParent);
		}
		else
			return null;
	}
	
	protected void updateForCapture(GeneralizedCharacter gChar)
	{
		if (_local._parent !=null)
		{
			 _local._parent.updateForCapture(gChar);
		}
	}
	
	public String toString()
	{
		if (null!=_grammarComponent)
			return _grammarComponent;
		else
			return "null";
	}
	
	public void setTag(String tagName)
	{
		_tag = tagName;
	}
	
	public String getTag()
	{
		return _tag;
	}
	
	public void setChildTagOverrides(HashMap<String, String> childOverrides)
	{
		_childOverrides = childOverrides;
	}
	
	public void updateTagOverides(HashMap<String, String> tags)
	{
		if (_childOverrides == null)
		{
			_childOverrides = tags;
		}
		else
		{
			if (tags!=null)
			{
				for (String tname:tags.keySet())
				{
					_childOverrides.put(tname, tags.get(tname));
				}
			}
			
		}
	}
	
	protected void takeoverChildTags(GeneralizedCharacter gchar)
	{
		for (Matcher m:_actualChildren)
		{
			gchar.takeoverTags(m, this);
		}
	}
	
	protected void killChildTags(GeneralizedCharacter gchar)
	{
		for (Matcher m:_actualChildren)
		{
			gchar.removeTags(m);
		}
	}
	
	protected void takeoverChildTags(Matcher[] children, GeneralizedCharacter gchar)
	{
		for (Matcher m:children)
		{
			gchar.takeoverTags(m, this);
		}
	}
	
	protected void killChildTags(Matcher[] children, GeneralizedCharacter gchar)
	{
		for (Matcher m:children)
		{
			gchar.removeTags(m);
		}
	}
	
	protected void takeoverChildTags(Matcher m, GeneralizedCharacter gchar)
	{
		gchar.takeoverTags(m, this);
	}
	
	protected void killChildTags(Matcher m, GeneralizedCharacter gchar)
	{
		gchar.removeTags(m);
	}
}
