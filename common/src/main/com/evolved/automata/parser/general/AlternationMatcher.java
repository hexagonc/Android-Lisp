package com.evolved.automata.parser.general;

import java.util.*;
 
class AlternationMatcher extends Matcher{
	
	Matcher[] _childMatchers;
	int _childIndex = 0;
	int _maxChildIndex=0;
	int _failureCount = 0;
	public AlternationMatcher(String grammarComponent,  Matcher[] childMatchers)
	{
		super(grammarComponent);
		_childMatchers = childMatchers;
		_maxChildIndex = childMatchers.length-1;
		_isAlternation = true;
		
		for (int i=0;i<childMatchers.length;i++)
		{
			_childMatchers[i].setTemplateParameters(this, i);
		}
	}

	public AlternationMatcher(PatternParser.GlobalState global, PatternParser.LocalState local, String grammarComponent,  Matcher[] childMatchers)
	{
		super(global, local, grammarComponent);
		_childMatchers = childMatchers;
		_maxChildIndex = childMatchers.length-1;
		_childIndex = 0;
		_isAlternation = true;
		for (int i=0;i<childMatchers.length;i++)
		{
			_childMatchers[i].updateTagOverides(_childOverrides);
		}
	}
	
	
	public Matcher clone(PatternParser.GlobalState global, PatternParser.LocalState local)
	{
		AlternationMatcher am = new AlternationMatcher(global, local, _grammarComponent, _childMatchers);
		am._templateIndexInParent = _templateIndexInParent;
		am._templateParent = _templateParent;
		am._childOverrides = _childOverrides;
		return am;
	}
	
	
	
	public LinkedList<Matcher> match()
	{
		if (_local == null && _templateParent!=null)
		{
			Matcher parent = _templateParent.clone(_global, null);
			_local = new PatternParser.LocalState(parent, _templateIndexInParent, 0);
		}
		
		LinkedList<Matcher> grammarList = new LinkedList<Matcher>();
		Matcher matcher = null;
		for (int i=0;i<=_maxChildIndex;i++)
		{
			matcher = _childMatchers[i].clone(_global, new PatternParser.LocalState(this, i, _local._startIndex));
			matcher.updateTagOverides(_childOverrides);
			grammarList.add(matcher);
			_childMatchers[i] = matcher;
		}
		
		return grammarList;
	}
	
	/**
	 * This is deprecated
	 * @param matcher
	 * @param addParenthesis
	 */
	public void addChildMatcher(Matcher matcher, boolean addParenthesis)
	{
		if (addParenthesis)
			_grammarComponent += " | (" + matcher._grammarComponent + ")";
		else
			_grammarComponent += " | " + matcher._grammarComponent;
		_maxChildIndex = _maxChildIndex+1;
		Matcher[] newChildren = new Matcher[_maxChildIndex+1];
		for (int i=0;i<_maxChildIndex;i++)
		{
			newChildren[i]=_childMatchers[i];
		}
		newChildren[_maxChildIndex] = matcher;
		matcher.setTemplateParameters(this, _maxChildIndex);
		_childMatchers = newChildren;
		for (int i=0;i<_childMatchers.length;i++)
		{
			_childMatchers[i].updateTagOverides(_childOverrides);
		}
	}
	
	public void addChildMatcher(Matcher matcher)
	{
		_grammarComponent += " | " + generateChildGrammarDef(matcher);
		_maxChildIndex = _maxChildIndex+1;
		Matcher[] newChildren = new Matcher[_maxChildIndex+1];
		for (int i=0;i<_maxChildIndex;i++)
		{
			newChildren[i]=_childMatchers[i];
		}
		newChildren[_maxChildIndex] = matcher;
		matcher.setTemplateParameters(this, _maxChildIndex);
		_childMatchers = newChildren;
		for (int i=0;i<_childMatchers.length;i++)
		{
			_childMatchers[i].updateTagOverides(_childOverrides);
		}
	}
	
	
	
	public Matcher findMatcherByName(String grammarName)
	{
		for (Matcher m:_childMatchers)
		{
			if (m.getPatternToMatch().equals(grammarName))
				return m;
		}
		return null;
	}
	
	public String removeMatcher(String grammarName)
	{
		Matcher target = findMatcherByName(grammarName);
		if (target != null)
		{
			return removeMatcher(target);
		}
		return _grammarComponent;
	}
	public String removeMatcher(Matcher matcher)
	{
		Matcher[] newMatchers = new Matcher[Math.max(0, _childMatchers.length - 1)];
		int offsetIndex = 0;
		StringBuilder sBuilder = new StringBuilder();
		for (int i = 0;i<_childMatchers.length;i++)
		{
			if (_childMatchers[i] == matcher)
			{
				offsetIndex = 1;
			}
			else
			{
				if (i == 0)
				{
					sBuilder.append(generateChildGrammarDef(_childMatchers[i]));
				}
				else
				{
					sBuilder.append(" | ").append(generateChildGrammarDef(_childMatchers[i]));
				}
				newMatchers[i - offsetIndex] = _childMatchers[i];
				newMatchers[i - offsetIndex].setTemplateParameters(this, i - offsetIndex);
			}
		}
		_grammarComponent = sBuilder.toString();
		_childMatchers = newMatchers;
		_maxChildIndex = _maxChildIndex - offsetIndex;
		return _grammarComponent;
		
	}
	
	private String generateChildGrammarDef(Matcher m)
	{
		if (shouldAddParenthesis(m))
			return "(" + m.getPatternToMatch() + ")";
		else
			return m.getPatternToMatch();
	}
	
	private boolean shouldAddParenthesis(Matcher m)
	{
		return !m.isNonTerminal() && !m.isTerminal();
	}

	protected LinkedList<Matcher> updateFromParseSuccess(int childIndex, int endIndex)
	{
		
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
		
		GeneralizedCharacter gchar = null;
		for (int i=_local._startIndex;i<endIndex;i++)
		{
			gchar = _global._buffer.get(i);
			if (_tag!=null)
			{
				gchar.addTag(this, _tag);
			}
			takeoverChildTags(_childMatchers[childIndex], gchar);
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
	
	protected LinkedList<Matcher> updateFromParseFailure(int startIndex)
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
			killChildTags(_childMatchers[startIndex], gchar);
		}
		
		if (_failureCount == _maxChildIndex)
		{
			_failureCount++;
			if (_local._parent!=null)
				return _local._parent.updateFromParseFailure(_local._indexInParent);
			else
				return null;
		}
			
		_failureCount++;
		return null;
	}
	
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
		// Treat this similarily to a failure
		if (_failureCount == _maxChildIndex)
			return _local._parent.onInvalidGrammar(_local._indexInParent);
		
		_failureCount++;
		return null;
	}
}
