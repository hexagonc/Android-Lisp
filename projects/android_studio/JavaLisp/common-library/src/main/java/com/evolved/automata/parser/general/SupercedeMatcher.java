package com.evolved.automata.parser.general;

import java.util.LinkedList;

import com.evolved.automata.parser.general.PatternParser.GlobalState;
import com.evolved.automata.parser.general.PatternParser.LocalState;

public class SupercedeMatcher extends Matcher 
{
	Matcher _superiorMatcher;
	Matcher _inferiorMatcher;
	
	boolean matchingSuperior;
	boolean finishedSuperior = false;
	
	Matcher _actualSuperior;
	Matcher _actualInferior;
	
	
	public SupercedeMatcher(String component, Matcher infMatcher, Matcher supMatcher)
	{
		super(component);
		_inferiorMatcher = infMatcher;
		_superiorMatcher = supMatcher;
	}
	
	public SupercedeMatcher(PatternParser.GlobalState global, PatternParser.LocalState local, String grammarComponent,  Matcher inferiorMatcher, Matcher superiorMatcher)
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
		_actualSuperior = firstState;
		return grammarList;
		
	}

	@Override
	public Matcher clone(GlobalState global, LocalState local) {
		SupercedeMatcher cm = new SupercedeMatcher(global, local, _grammarComponent, _inferiorMatcher, _superiorMatcher);
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
			finishedSuperior = true;
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
				for (Matcher m:_actualChildren)
				{
					if (gchar.hasTagsFrom(m) && gchar.hasTagsFrom(_actualSuperior))
					{
						gchar.removeTags(m);
					}
				}
				
				gchar.takeoverTags(_actualSuperior, this);
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
			if (finishedSuperior)
			{
				GeneralizedCharacter gchar = null;
				for (int i=_local._startIndex;i<endIndex;i++)
				{
					gchar = _global._buffer.get(i);
					gchar.removeTags(_actualInferior);
				}
				return null;
			}
			matchingSuperior=true;
			Matcher nextState = _superiorMatcher.clone(_global, new PatternParser.LocalState(this, 1, endIndex));
			nextState.updateTagOverides(_childOverrides);
			grammarList.add(nextState);
			_actualSuperior = nextState;
			
			return grammarList;
		}
		
	}
	
	protected LinkedList<Matcher> updateFromParseFailure(int matchCount)
	{
		LinkedList<Matcher> grammarList = new LinkedList<Matcher>();
		if (matchCount == 1)
		{
			GeneralizedCharacter gchar = null;
			for (int i=_local._startIndex;i<_endIndex;i++)
			{
				gchar = _global._buffer.get(i);
				killChildTags(_actualSuperior, gchar);
			}
			
			if (!matchingSuperior)
				return null;
			matchingSuperior = false;
			
			Matcher nextState = _inferiorMatcher.clone(_global, new PatternParser.LocalState(this, 0, _endIndex));
			nextState.updateTagOverides(_childOverrides);
			grammarList.add(nextState);
			_actualInferior = nextState;
			_actualChildren.add(_actualInferior);
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
			
			GeneralizedCharacter gchar = null;
			for (int i=_local._startIndex;i<_endIndex;i++)
			{
				gchar = _global._buffer.get(i);
				killChildTags(gchar);
				
			}
			
			if (_local._parent!=null)
				return _local._parent.updateFromParseFailure(_local._startIndex);
			else
				return null;
			
		}
	}
}
