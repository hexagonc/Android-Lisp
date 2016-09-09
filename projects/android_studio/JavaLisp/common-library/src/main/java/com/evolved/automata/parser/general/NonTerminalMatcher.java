package com.evolved.automata.parser.general;

import java.util.*;

import com.evolved.automata.parser.general.PatternParser.GlobalState;

class NonTerminalMatcher extends Matcher
{
	Matcher _childState = null;
	
	public NonTerminalMatcher(String grammarComponent, GlobalState global)
	{
		super(grammarComponent);
		_isNonterminal = true;
		if (global!=null && global.bottomMatchersEnabledP())
		{
			Matcher m = global.getParseMap().get(_grammarComponent);
			String nt = m.getPatternToMatch();
			if (nt.trim().equals(_grammarComponent))
				throw new IllegalStateException("Cannot have a non-terminal mapping to itself: " + nt);
			
			Matcher top = Parser.parse(global, nt, _childOverrides);
			top.setTemplateParameters(this, 0);
		}
	}
	
	public NonTerminalMatcher(String grammarComponent, PatternParser.GlobalState global, PatternParser.LocalState local)
	{
		super( global, local, grammarComponent);
		_isNonterminal = true;
	}
	
	
	
	public Matcher clone(PatternParser.GlobalState global, PatternParser.LocalState local)
	{
		NonTerminalMatcher nm = new NonTerminalMatcher(_grammarComponent, global, local);
		nm._templateIndexInParent = _templateIndexInParent;
		nm._templateParent = _templateParent;
		nm._tag = _tag;
		nm._childOverrides = _childOverrides;
		return nm;
	}
	
	
	
	public LinkedList<Matcher> match()
	{
		LinkedList<Matcher> grammarList = new LinkedList<Matcher>();
		if (_global._parseMap.containsKey(_grammarComponent) && !_global._deletedPatterns.contains(_grammarComponent))
		{
			_childState = _global._parseMap.get(_grammarComponent).clone(_global, new PatternParser.LocalState(this, 0,_local._startIndex));
			_childState.updateTagOverides(_childOverrides);
			grammarList.add(_childState);
			_actualChildren.add(_childState);
			return grammarList;
		}
		else
			return super.onInvalidGrammar(_local._indexInParent);
		
	}
	
	
	protected LinkedList<Matcher> updateFromParseSuccess(int subIndex, int endIndex)
	{
		finalize();
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
			takeoverChildTags(gchar);
		}
		
		if (_local!=null && _local._parent!=null)
		{
			return _local._parent.updateFromParseSuccess(_local._indexInParent, endIndex);
		}
		else
		{
			LinkedList<Matcher> grammarList = new LinkedList<Matcher>();
			grammarList.add(this);
			return grammarList;
		}
		
	}
	
	public void addDisjunctiveChild(Matcher child)
	{
		Matcher mapped = _global._parseMap.get(_grammarComponent);
		_global.clearBottomMatchers();
		if (mapped instanceof AlternationMatcher)
		{
			AlternationMatcher disj = (AlternationMatcher)mapped;
			if (child instanceof NonTerminalMatcher)
				disj.addChildMatcher(child, false);
			else
				disj.addChildMatcher(child, true);
		}
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
		_global.deleteNonTerminal(_grammarComponent);
		return super.onInvalidGrammar(_local._indexInParent);
	}
	
	Matcher getChild()
	{
		return _childState;
	}
	
	@Override
	public void updateTagOverides(HashMap<String, String> tags)
	{
		super.updateTagOverides(tags);
		if (_childOverrides != null && _childOverrides.containsKey(_grammarComponent))
		{
			_tag = _childOverrides.get(_grammarComponent);
		}
		
	}

}
