package com.evolved.automata.parser.general;


import java.io.Serializable;
import java.util.LinkedList;

class ConjunctionMatcher extends Matcher implements Serializable
{
	static final long serialVersionUID = 1;
	
	Matcher[] _childMatchers;
	int _childIndex = 0;
	int _maxChildIndex=0;
	int _maxTestedIndex = 0;
	
	public ConjunctionMatcher(String grammarComponent,  Matcher[] childMatchers)
	{
		super(grammarComponent);
		_childMatchers = childMatchers;
		_maxChildIndex = childMatchers.length-1;
		_childIndex = 0;
		for (int i=0;i<childMatchers.length;i++)
		{
			_childMatchers[i].setTemplateParameters(this, i);
			
		}
	}
	
	public ConjunctionMatcher(PatternParser.GlobalState global, PatternParser.LocalState local, String grammarComponent,  Matcher[] childMatchers)
	{
		super(global, local, grammarComponent);
		_childMatchers = childMatchers;
		_maxChildIndex = childMatchers.length-1;
		_childIndex = 0;
		
		for (int i=0;i<childMatchers.length;i++)
		{
			_childMatchers[i].updateTagOverides(_childOverrides);
		}
	}
	
	
	public Matcher clone(PatternParser.GlobalState global, PatternParser.LocalState local)
	{
		ConjunctionMatcher cm = new ConjunctionMatcher(global, local, _grammarComponent, _childMatchers);
		cm._templateIndexInParent = _templateIndexInParent;
		cm._templateParent = _templateParent;
		cm._childOverrides = _childOverrides;
		return cm;
	}
	
	
	public LinkedList<Matcher> match()
	{
		LinkedList<Matcher> grammarList = new LinkedList<Matcher>();
		Matcher firstState = null;
		firstState = _childMatchers[0].clone(_global, new PatternParser.LocalState(this, _childIndex, _local._startIndex));
		firstState.updateTagOverides(_childOverrides);
		grammarList.add(firstState);
		_actualChildren.add(firstState);
		return grammarList;
	}
	
	protected LinkedList<Matcher> updateFromParseSuccess(int childIndex, int endIndex )
	{
		_endIndex = endIndex;
		
		LinkedList<Matcher> grammarList = new LinkedList<Matcher>();
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
		
		if (childIndex == _maxChildIndex)
		{
			GeneralizedCharacter gchar = null;
			for (int i=_local._startIndex;i<endIndex;i++)
			{
				gchar = _global._buffer.get(i);
				if (_tag!=null)
				{
					gchar.addTag(this, _tag);
				}
				takeoverChildTags(gchar);
			}
			
			if (_local._parent!=null)
			{
				return _local._parent.updateFromParseSuccess(_local._indexInParent, endIndex);
			}
			else
			{
				finalize();
				grammarList.add(this);
				return grammarList;
			}
		}
		else
		{
			_maxTestedIndex = Math.max(childIndex+1, _maxTestedIndex);
			
			Matcher nextState = _childMatchers[childIndex+1].clone(_global, new PatternParser.LocalState(this, childIndex+1, _endIndex));
			nextState.updateTagOverides(_childOverrides);
			grammarList.add(nextState);
			_actualChildren.add(nextState);
			return grammarList;
		}
		
	}
	
	protected  LinkedList<Matcher> updateFromParseFailure(int startIndex)
	{
		if (startIndex<_maxTestedIndex)
			return null;
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
	
}
