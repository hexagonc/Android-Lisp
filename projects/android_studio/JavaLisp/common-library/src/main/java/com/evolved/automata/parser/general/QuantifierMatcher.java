package com.evolved.automata.parser.general;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.LinkedList;

class QuantifierMatcher extends Matcher
{
	Integer minimumRequiredMatches;
	Integer maximimRequiredMatches;

	boolean matchingNegative;
	Matcher _childMatcher;
	int _matchCount = 0;
	HashMap<Integer, Matcher> _childMap = new HashMap<Integer, Matcher>();
	public QuantifierMatcher(String grammarComponent, Integer minimumMatches, Integer maximumMatches, Matcher childMatcher)
	{
		super(grammarComponent);
		_childMatcher=childMatcher;
		minimumRequiredMatches = minimumMatches;
		maximimRequiredMatches = maximumMatches;
		
		matchingNegative = (maximimRequiredMatches!=null)&&(maximimRequiredMatches.intValue()==0);
		childMatcher.setTemplateParameters(this, 0);
		childMatcher.updateTagOverides(_childOverrides);
		
	}
	
	public QuantifierMatcher(PatternParser.GlobalState global, PatternParser.LocalState local, String grammarComponent, Integer minimumMatches, Integer maximumMatches, Matcher childMatcher)
	{
		super(global, local, grammarComponent);
		_childMatcher=childMatcher;
		minimumRequiredMatches = minimumMatches;
		maximimRequiredMatches = maximumMatches;
		
		matchingNegative = (maximimRequiredMatches!=null)&&(maximimRequiredMatches.intValue()==0);
		childMatcher.updateTagOverides(_childOverrides);
		
		
	}
	
	
	public Matcher clone(PatternParser.GlobalState global, PatternParser.LocalState local)
	{
		QuantifierMatcher qm = new QuantifierMatcher(global, local, _grammarComponent, minimumRequiredMatches, maximimRequiredMatches, _childMatcher);
		qm._templateIndexInParent = _templateIndexInParent;
		qm._templateParent = _templateParent;
		qm._childOverrides = _childOverrides;
		return qm;
	}
	
	
	
	public LinkedList<Matcher> match()
	{
		LinkedList<Matcher> grammarList = new LinkedList<Matcher>();
		Matcher nextMatcher;
		// _matchCount
		if (matchingNegative)
		{
			nextMatcher = _childMatcher.clone(_global, new PatternParser.LocalState(this, 0, _local._startIndex));
			nextMatcher.updateTagOverides(_childOverrides);
			grammarList.add(nextMatcher);
			return grammarList;
		}
		else
		{
			nextMatcher = _childMatcher.clone(_global, new PatternParser.LocalState(this, 0, _local._startIndex));
			nextMatcher.updateTagOverides(_childOverrides);
			grammarList.add(nextMatcher);
			_childMap.put(Integer.valueOf(0), nextMatcher);
			if (minimumRequiredMatches!=null && _matchCount>=minimumRequiredMatches)
			{
				if (_local._parent!=null)
				{
					grammarList.addAll(_local._parent.updateFromParseSuccess(_local._indexInParent, _local._startIndex));
				}
				
			}
			
			
			return grammarList;
			
		}
		
	}
	
	protected LinkedList<Matcher> updateFromParseFailure(int matchCount)
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
		
		
		if (matchingNegative)
		{
			if (_local!=null && _local._parent!=null)
			{
				return _local._parent.updateFromParseSuccess(_local._indexInParent, _local._startIndex);
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
			// TODO: Does this still need to be done?
			GeneralizedCharacter gchar = null;
			for (int i=_local._startIndex;i<_endIndex;i++)
			{
				gchar = _global._buffer.get(i);
				killChildTags(_childMap.get(Integer.valueOf(matchCount)), gchar);
			}
			
			if (minimumRequiredMatches!=null && matchCount<=minimumRequiredMatches)
			{
				for (int i=_local._startIndex;i<_endIndex;i++)
				{
					gchar = _global._buffer.get(i);
					gchar.removeTags(this);
				}
				
				if (_local._parent!=null)
					return _local._parent.updateFromParseFailure(_local._indexInParent);
				else
					return null;
			}
			
			if (_local._parent==null)
//				return _local._parent.updateFromParseSuccess(_local._indexInParent, _endIndex);
//			else
			{
				finalize();
				grammarList.add(this);
				return grammarList;
			}
			else
				return null;
			
		}
	}
	
	protected LinkedList<Matcher> updateFromParseSuccess(int childIndex, int endIndex)
	{
		LinkedList<Matcher> grammarList = new LinkedList<Matcher>();
		_endIndex = endIndex;
		
		_matchCount = childIndex+1;
		Matcher nextMatcher = null;
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
		
		if (matchingNegative)
		{
			
			if (_local._parent!=null)
				return _local._parent.updateFromParseFailure(_local._indexInParent);
			else
				return null;
		}
		
		GeneralizedCharacter gchar = null;
		for (int i=_local._startIndex;i<endIndex;i++)
		{
			gchar = _global._buffer.get(i);
			if (_tag!=null)
			{
				gchar.addTag(this, _tag);
			}
			takeoverChildTags(_childMap.get(Integer.valueOf(childIndex)), gchar);
		}
		
		if (maximimRequiredMatches==null || _matchCount<maximimRequiredMatches.intValue())
		{
			nextMatcher = _childMatcher.clone(_global, new PatternParser.LocalState(this, _matchCount, _endIndex));
			nextMatcher.updateTagOverides(_childOverrides);
			_childMap.put(Integer.valueOf(_matchCount), nextMatcher);
			grammarList.add(nextMatcher);
			
		}
		if (_local._parent!=null && (minimumRequiredMatches==null || _matchCount>=minimumRequiredMatches.intValue()))
			grammarList.addAll(_local._parent.updateFromParseSuccess(_local._indexInParent, _endIndex));
		else
		{
			finalize();
			grammarList.add(this);
			
		}
		
		return grammarList;
	}
}
