package com.evolved.automata.parser.general;

import java.util.LinkedList;

import com.evolved.automata.parser.general.PatternParser.GlobalState;
import com.evolved.automata.parser.general.PatternParser.LocalState;

public class ReplaceMatcher extends Matcher 
{
	Matcher _superiorMatcher;
	Matcher _inferiorMatcher;
	
	boolean matchingSuperior;
	
	
	public ReplaceMatcher(String component, Matcher infMatcher, Matcher supMatcher)
	{
		super(component);
		_inferiorMatcher = infMatcher;
		_superiorMatcher = supMatcher;
	}
	
	public ReplaceMatcher(PatternParser.GlobalState global, PatternParser.LocalState local, String grammarComponent,  Matcher inferiorMatcher, Matcher superiorMatcher)
	{
		super(global, local, grammarComponent);
		_inferiorMatcher = inferiorMatcher;
		_superiorMatcher = superiorMatcher;
		
	}
	

	@Override
	public LinkedList<Matcher> match() {
		_endIndex = _local._startIndex;
		LinkedList<Matcher> grammarList = new LinkedList<Matcher>();
		Matcher firstState = null;
		matchingSuperior = true;
		firstState = _superiorMatcher.clone(_global, new PatternParser.LocalState(this, 1, _local._startIndex));
		firstState.updateTagOverides(_childOverrides);
		grammarList.add(firstState);
		return grammarList;
		
	}

	@Override
	public Matcher clone(GlobalState global, LocalState local) {
		ReplaceMatcher cm = new ReplaceMatcher(global, local, _grammarComponent, _inferiorMatcher, _superiorMatcher);
		cm._templateIndexInParent = _templateIndexInParent;
		cm._templateParent = _templateParent;
		cm._childOverrides = _childOverrides;
		return cm;
	}
	
	protected LinkedList<Matcher> updateFromParseSuccess(int childIndex, int endIndex )
	{
		_endIndex = endIndex;
		LinkedList<Matcher> grammarList = new LinkedList<Matcher>();
		if (childIndex == 1)
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
				return _local._parent.updateFromParseFailure(_local._startIndex);
			else
				return null;
		}
		else
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
		
	}
	
	protected LinkedList<Matcher> updateFromParseFailure(int matchCount)
	{
		LinkedList<Matcher> grammarList = new LinkedList<Matcher>();
		if (matchCount == 1)
		{
			matchingSuperior = false;
			
			Matcher nextState = _inferiorMatcher.clone(_global, new PatternParser.LocalState(this, 0, _endIndex));
			nextState.updateTagOverides(_childOverrides);
			grammarList.add(nextState);
			return grammarList;
		}
		else
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
				return _local._parent.updateFromParseFailure(_local._startIndex);
			else
				return null;
			
		}
	}
}
