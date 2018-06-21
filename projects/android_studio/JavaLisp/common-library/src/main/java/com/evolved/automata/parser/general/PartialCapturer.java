package com.evolved.automata.parser.general;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;


public class PartialCapturer 
{
	private static class CaptureStateData
	{
		LinkedList<GeneralizedCharacter> partialQueue;
		LinkedList<LinkedList<GeneralizedCharacter>> matchList;
		
		LinkedList<GeneralizedCharacter> currentQueue;
		GeneralizedCharacter gchar;
		boolean _createCurrentQueue = true;
		CaptureStateData()
		{
			matchList = new LinkedList<LinkedList<GeneralizedCharacter>>();
			partialQueue = new LinkedList<GeneralizedCharacter>();  
					
		}
		
		private static CaptureStateData getInitial()
		{
			CaptureStateData defaultData = new CaptureStateData();
			return defaultData;
		}
		
	}
	
	
	
	private static abstract class CaptureState
	{
		CaptureStateData _data;
		protected String name;
		
		public String toString()
		{
			return name;
		}
		public CaptureState(CaptureStateData data)
		{
			_data = data;
		}
		
		public abstract CaptureState noMatches(GeneralizedCharacter gchar);
		public abstract CaptureState partialAndFullMatches(GeneralizedCharacter gchar);
		public abstract CaptureState fullMatchesOnly(GeneralizedCharacter gchar);
		public abstract CaptureState partialMatchesOnly(GeneralizedCharacter gchar);
		
		
		
		void incrementCurrentQueue()
		{
			_data._createCurrentQueue = true;
			
		}
		
		void appendToPartialQueue(GeneralizedCharacter gchar)
		{
			if (_data.partialQueue == null)
			{
				_data.partialQueue = new LinkedList<GeneralizedCharacter>();
			}
			_data.partialQueue.add(gchar);
		}
		
		void appendToCurrentQueue(GeneralizedCharacter gchar)
		{
			if (_data._createCurrentQueue)
			{
				_data._createCurrentQueue = false;
				_data.currentQueue = new LinkedList<GeneralizedCharacter>();
				_data.matchList.add(_data.currentQueue);
			}
			
			_data.currentQueue.add(gchar);
		}
		
		void clearPartialQueue()
		{
			if (_data.partialQueue!=null)
				_data.partialQueue.clear();
		}
		
		void dequeuePartialQueueIntoCurrentQueue()
		{
			if (_data.partialQueue!=null)
			{
				if (_data.currentQueue==null)
				{
					_data.currentQueue = new LinkedList<GeneralizedCharacter>();
					_data.matchList.add(_data.currentQueue);
					_data._createCurrentQueue =false;
				}
				_data.currentQueue.addAll(_data.partialQueue);
				_data.partialQueue.clear();
			}
		}
		
		public LinkedList<LinkedList<GeneralizedCharacter>> getData()
		{
			return _data.matchList;
		}
	}
	
	public static class PartialResult
	{
		HashSet<String> partiallyMatchedTagSet;
		HashSet<String> fullyMatchedTagSet;
		boolean completeP;
		
		public PartialResult(HashSet<String> ppartiallyMatchedTagSet, HashSet<String> ffullyMatchedTagSet, boolean completed)
		{
			fullyMatchedTagSet =  ffullyMatchedTagSet;
			partiallyMatchedTagSet = ppartiallyMatchedTagSet;
			completeP = completed;
		}
		
		public boolean isCompleted()
		{
			return completeP;
		}
		
		public boolean resultsP()
		{
			return partiallyMatchedTagSet!=null && partiallyMatchedTagSet.size()>0 || fullyMatchedTagSet!=null && fullyMatchedTagSet.size()>0;
		}
		
		public boolean hasPartialMatches()
		{
			return partiallyMatchedTagSet!=null && partiallyMatchedTagSet.size()>0;
		}
		
		public boolean hasFullMatches()
		{
			return fullyMatchedTagSet!=null && fullyMatchedTagSet.size()>0;
		}
		
		public HashSet<String> getPartialMatchedSet()
		{
			return partiallyMatchedTagSet;
		}
		
		public HashSet<String> getFullyMatchedSet()
		{
			return fullyMatchedTagSet;
		}
	}
	
	
	PatternParser.MatcherController _controller;
	HashMap<String, CaptureState> stateMap = new HashMap<String, CaptureState>();
	String[] _captureTags;
	LinkedList<Matcher> priorPostponed;
	int _remainingErrorsAllow = 0;
	int _maxAllowedErrors = 1;
	
	public PartialCapturer(PatternParser parser, String grammar, String[] captureTags)
	{
		_controller = parser.getIncrementalParser(grammar);
		_captureTags = captureTags;
		initialize();
	}
	
	public PartialCapturer(PatternParser.MatcherController controller, String[] captureTags)
	{
		_controller = controller;
		_captureTags = captureTags;
		initialize();
	}
	
	private void initialize()
	{
		stateMap.clear();
		for (String tag:_captureTags)
		{
			stateMap.put(tag, getNotMatchingState(CaptureStateData.getInitial()));
		}
		priorPostponed = null;
	}
	
	
	public void reset()
	{
		initialize();
		partialReset();
	}
	
	public HashMap<String, LinkedList<LinkedList<GeneralizedCharacter>>> getCurrentTagMatches()
	{
		HashMap<String, LinkedList<LinkedList<GeneralizedCharacter>>> output = new HashMap<String, LinkedList<LinkedList<GeneralizedCharacter>>>();
		for (String tag:stateMap.keySet())
		{
			output.put(tag, stateMap.get(tag).getData());
		}
		return output;
	}
	
	public void partialReset()
	{
		_controller.reset();
		priorPostponed = null;
	}
	
	
	
	
	public  PartialResult update(GeneralizedCharacter gchar, boolean allowErrors)
	{
		CaptureState nextState, currentState;
		String tagName;
		HashSet<Matcher> parentMatchers = null;
		HashSet<Matcher> expectedMatches = null;
		HashSet<Matcher> smaller = null;
		HashSet<Matcher> larger = null;
		HashSet<Matcher> partialMatches = new HashSet<Matcher>();
		HashSet<Matcher> fullAndPartialMatches = new HashSet<Matcher>();
		HashSet<String> nonMatchingTags = new HashSet<String>(), partialMatchingTags = new HashSet<String>(), fullyMatchingTags = new HashSet<String>();
		HashSet<Matcher> matches = null;
		
		_controller.update(gchar);
		if (_controller._postponedMatchers.size()==0)
		{
			matches = _controller.getFullyMatchedMatchers(false, true);
			if (matches.size()>0)
			{
				matches = filterForTagged(matches, stateMap);
				
				HashSet<Matcher> test = _controller.getMatchedMatchers(true, false);
				parentMatchers = filterForTagged(test, stateMap);
				test = _controller.getMatchersExpectedToMatch();
				expectedMatches = filterForTagged(test, stateMap);
				smaller = (parentMatchers.size()<expectedMatches.size())?parentMatchers:expectedMatches;
				larger = (smaller == parentMatchers)?expectedMatches:parentMatchers;
				
				for (Matcher m:smaller)
				{
					if (larger.contains(m))
					{
						if (matches.contains(m))
						{
							matches.remove(m);
							fullAndPartialMatches.add(m);
						}
						else
							partialMatches.add(m);
					}
				}
				
				for (Matcher m:matches)
				{
					tagName = m.getTag();
					currentState = stateMap.get(tagName);
					nextState = currentState.fullMatchesOnly(gchar);
					stateMap.put(tagName, nextState);
					nonMatchingTags.remove(tagName);
					fullyMatchingTags.add(tagName);
				}
				
				for (Matcher m:partialMatches)
				{
					tagName = m.getTag();
					currentState = stateMap.get(tagName);
					nextState = currentState.partialMatchesOnly(gchar);
					stateMap.put(tagName, nextState);
					nonMatchingTags.remove(tagName);
					partialMatchingTags.add(tagName);
					
				}
				
				for (Matcher m:fullAndPartialMatches)
				{
					tagName = m.getTag();
					currentState = stateMap.get(tagName);
					nextState = currentState.partialAndFullMatches(gchar);
					stateMap.put(tagName, nextState);
					nonMatchingTags.remove(tagName);
					partialMatchingTags.add(tagName);
					fullyMatchingTags.add(tagName);
				}
				
				for (String tag:nonMatchingTags)
				{
					currentState = stateMap.get(tag);
					nextState = currentState.noMatches(gchar);
					stateMap.put(tag, nextState);
				}
				
				
				return new PartialResult(partialMatchingTags, fullyMatchingTags, false);
			}
			
			
			
			if (allowErrors)
			{
				if (priorPostponed!=null)
				{
					_controller._postponedMatchers = priorPostponed;
					_controller.rollbackOne();
					_remainingErrorsAllow--;
					if (_remainingErrorsAllow == 0)
						priorPostponed = null;
				}
				else
					reset();
				
			}
			
			return new PartialResult(null, null, !allowErrors);
		}
		priorPostponed = _controller._postponedMatchers;
		_remainingErrorsAllow = _maxAllowedErrors;
		
		for (String tag:_captureTags)
		{
			nonMatchingTags.add(tag);
		}
		matches = _controller.getFullyMatchedMatchers(false, true);
		matches = filterForTagged(matches, stateMap);
		
		HashSet<Matcher> test = _controller.getMatchedMatchers(true, false);
		parentMatchers = filterForTagged(test, stateMap);
		test = _controller.getMatchersExpectedToMatch();
		expectedMatches = filterForTagged(test, stateMap);
		smaller = (parentMatchers.size()<expectedMatches.size())?parentMatchers:expectedMatches;
		larger = (smaller == parentMatchers)?expectedMatches:parentMatchers;
		
		for (Matcher m:smaller)
		{
			if (larger.contains(m))
			{
				if (matches.contains(m))
				{
					matches.remove(m);
					fullAndPartialMatches.add(m);
				}
				else
					partialMatches.add(m);
			}
		}
		
		for (Matcher m:matches)
		{
			tagName = m.getTag();
			currentState = stateMap.get(tagName);
			nextState = currentState.fullMatchesOnly(gchar);
			stateMap.put(tagName, nextState);
			nonMatchingTags.remove(tagName);
			fullyMatchingTags.add(tagName);
		}
		
		for (Matcher m:partialMatches)
		{
			tagName = m.getTag();
			currentState = stateMap.get(tagName);
			nextState = currentState.partialMatchesOnly(gchar);
			stateMap.put(tagName, nextState);
			nonMatchingTags.remove(tagName);
			partialMatchingTags.add(tagName);
			
		}
		
		for (Matcher m:fullAndPartialMatches)
		{
			tagName = m.getTag();
			currentState = stateMap.get(tagName);
			nextState = currentState.partialAndFullMatches(gchar);
			stateMap.put(tagName, nextState);
			nonMatchingTags.remove(tagName);
			partialMatchingTags.add(tagName);
			fullyMatchingTags.add(tagName);
		}
		
		for (String tag:nonMatchingTags)
		{
			currentState = stateMap.get(tag);
			nextState = currentState.noMatches(gchar);
			stateMap.put(tag, nextState);
		}
		
		
		return new PartialResult(partialMatchingTags, fullyMatchingTags, false);
	}
	
	
	// `\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_`\_
	// 							Helper functions
	// _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
	
	private static HashSet<Matcher> filterForTagged(HashSet<Matcher> map, HashMap<String, CaptureState> tagMap)
	{
		HashSet<Matcher> filtered = new HashSet<Matcher>();
		if (map == null)
			return filtered;
		for (Matcher m:map)
		{
			if (tagMap.containsKey(m.getTag()))
				filtered.add(m);
		}
		return filtered;
	}
	
	
	
	// ~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~
	// 				Capture State Transition functions
	// ~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~~^~
	
	private static CaptureState getNotMatchingState(final CaptureStateData data)
	{
		CaptureState state = new CaptureState(data)
		{
			
			{
				name = "<No Matches>";
				clearPartialQueue();
			}
			
			@Override
			public CaptureState noMatches(GeneralizedCharacter gchar) {
				
				return this;
			}

			@Override
			public CaptureState partialAndFullMatches(GeneralizedCharacter gchar) {
				data.gchar = gchar;
				return getFullAndPartialMatchesState(data);
			}

			@Override
			public CaptureState fullMatchesOnly(GeneralizedCharacter gchar) 
			{
				_data.gchar = gchar;
				return getFullMatchesOnlyState(_data);
			}

			@Override
			public CaptureState partialMatchesOnly(GeneralizedCharacter gchar) {
				_data.gchar = gchar;
				return getPartialMatchesOnlyState(_data);
			}

		};
		
		return state;
	}
	
	private static CaptureState getFullAndPartialMatchesState(CaptureStateData data)
	{
		CaptureState state = new CaptureState(data)
		{
			
			{
				name = "<Full and Partial Matches>";
				appendToCurrentQueue(_data.gchar);
			}
			
			
			@Override
			public CaptureState noMatches(GeneralizedCharacter gchar) 
			{
				incrementCurrentQueue();
				_data.gchar = gchar;
				return getNotMatchingState(_data);
			}

			@Override
			public CaptureState partialAndFullMatches(GeneralizedCharacter gchar) {
				appendToCurrentQueue(gchar);
				return this;
			}

			@Override
			public CaptureState fullMatchesOnly(GeneralizedCharacter gchar) {
				_data.gchar = gchar;
				return getFullMatchesOnlyState(_data);
			}

			@Override
			public CaptureState partialMatchesOnly(GeneralizedCharacter gchar) {
				_data.gchar =gchar;
				return getPartialMatchesOnlyState(_data);
			}

			
			
		};
		
		return state;
	}
	
	// Fullmatches
	private static CaptureState getFullMatchesOnlyState(CaptureStateData data)
	{
		CaptureState state = new CaptureState(data)
		{
			
			{
				name = "<Only Full Matches>";
				appendToCurrentQueue(_data.gchar);
			}
			
			
			@Override
			public CaptureState noMatches(GeneralizedCharacter gchar) 
			{
				incrementCurrentQueue();
				_data.gchar = gchar;
				return getNotMatchingState(_data);
			}

			@Override
			public CaptureState partialAndFullMatches(GeneralizedCharacter gchar) 
			{	
				_data.gchar = gchar;
				return getFullAndPartialMatchesState(_data);
			}

			@Override
			public CaptureState fullMatchesOnly(GeneralizedCharacter gchar) {
				appendToCurrentQueue(gchar);
				return this;
			}

			@Override
			public CaptureState partialMatchesOnly(GeneralizedCharacter gchar) 
			{
				_data.gchar = gchar;
				incrementCurrentQueue();
				return getPartialMatchesOnlyState(_data);
			}

			
			
		};
		
		return state;
	}
	
	// Partial matches
	private static CaptureState getPartialMatchesOnlyState(CaptureStateData data)
	{
		CaptureState state = new CaptureState(data)
		{
			
			{
				name = "<Only Partial Matches>";
				appendToPartialQueue(_data.gchar);
			}
			
			
			@Override
			public CaptureState noMatches(GeneralizedCharacter gchar) 
			{
				incrementCurrentQueue();
				_data.gchar = gchar;
				return getNotMatchingState(_data);
			}

			@Override
			public CaptureState partialAndFullMatches(GeneralizedCharacter gchar) 
			{	
				dequeuePartialQueueIntoCurrentQueue();
				_data.gchar = gchar;
				return getFullAndPartialMatchesState(_data);
			}

			@Override
			public CaptureState fullMatchesOnly(GeneralizedCharacter gchar) 
			{
				dequeuePartialQueueIntoCurrentQueue();
				_data.gchar = gchar;
				return getFullMatchesOnlyState(_data);
			}

			@Override
			public CaptureState partialMatchesOnly(GeneralizedCharacter gchar) 
			{
				appendToPartialQueue(gchar);
				return this;
			}
			
		};
		
		return state;
	}
}
