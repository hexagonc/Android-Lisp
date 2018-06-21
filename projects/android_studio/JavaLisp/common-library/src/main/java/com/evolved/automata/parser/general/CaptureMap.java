package com.evolved.automata.parser.general;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.LinkedList;

import com.evolved.automata.KeyValuePair;

public class CaptureMap 
{
	HashMap<String, LinkedList<HashSet<String>>> _subGrammarOutputMap;
	HashSet<String> _subGrammars;
	HashSet<String> _nonTerminalsToCapture;
	HashMap<Matcher, StringBuilder > _fullCaptureBuffer;
	HashMap<String, LinkedList<String>> _outMap;
	HashSet<Matcher> _lastMatchedNonterminals;
	HashMap<Matcher, HashSet<String>> _fullSubGrammarBuffer;
	HashSet<String> _recentCaptures;
	
	
	public CaptureMap()
	{
		reset();
	}
	
	public void reset()
	{
		_subGrammarOutputMap = new HashMap<String, LinkedList<HashSet<String>>>();
		_subGrammars = new HashSet<String>();
		_nonTerminalsToCapture = new HashSet<String>();
		_fullCaptureBuffer = new HashMap<Matcher, StringBuilder >();
		_outMap = new HashMap<String, LinkedList<String>>();
		_lastMatchedNonterminals = new HashSet<Matcher>();
		_fullSubGrammarBuffer = new HashMap<Matcher, HashSet<String>>();
		_recentCaptures = new HashSet<String>();
	}
	
	public void setSubGrammars(String[] sub)
	{
		if (sub == null)
		{
			_subGrammars.clear();
		}
		else
		{
			for (String s:sub)
				_subGrammars.add(s);
		}
	}
	
	public void setGrammarsToCapture(String[] gram)
	{
		if (gram == null)
			_nonTerminalsToCapture.clear();
		else
		{
			for (String g:gram)
				_nonTerminalsToCapture.add(g);
		}
	}
	
	public void update(String gchar, HashSet<Matcher> fullMatchers)
	{
		HashSet<Matcher> newMatchSet = new HashSet<Matcher>();
		_recentCaptures = new HashSet<String>();
		if ((fullMatchers == null || fullMatchers.size() == 0))
		{
			return;
		}
		
		for (Matcher m:fullMatchers)
			updateForParent(newMatchSet, m);
		
		StringBuilder captureList;
		LinkedList<String> fullOut;
		HashSet<String> processedNT = new HashSet<String>();
		LinkedList<Matcher> workAround = new LinkedList<Matcher>();
		for (Matcher m: _fullCaptureBuffer.keySet())
			workAround.addLast(m);
		
		for (Matcher previousMatch:workAround)
		{
			captureList = _fullCaptureBuffer.get(previousMatch);
			if (!newMatchSet.contains(previousMatch))
			{
				if (previousMatch.isFinalized() && !processedNT.contains(previousMatch.getPatternToMatch()))
				{
					fullOut = _outMap.get(previousMatch.getPatternToMatch());
					if (fullOut == null)
					{
						fullOut = new LinkedList<String>();
						_outMap.put(previousMatch.getPatternToMatch(), fullOut);
					}
					fullOut.addLast(captureList.toString());
					processedNT.add(previousMatch.getPatternToMatch());
				}
				_fullCaptureBuffer.remove(previousMatch);
				
			}	 
		}
		
		processedNT.clear();
		HashSet<String> subcaptureSet;
		LinkedList<HashSet<String>> subcaptureList;
		workAround = new LinkedList<Matcher>();
		for (Matcher m: _fullSubGrammarBuffer.keySet())
			workAround.addLast(m);
		for (Matcher previousMatch:workAround)
		{
			subcaptureSet = _fullSubGrammarBuffer.get(previousMatch);
			if (!newMatchSet.contains(previousMatch))
			{
				if (previousMatch.isFinalized() && !processedNT.contains(previousMatch.getPatternToMatch()))
				{
					subcaptureList = _subGrammarOutputMap.get(previousMatch.getPatternToMatch());
					if (subcaptureList == null)
					{
						subcaptureList = new LinkedList<HashSet<String>>();
						_subGrammarOutputMap.put(previousMatch.getPatternToMatch(), subcaptureList);
					}
					subcaptureList.addLast(subcaptureSet);
					processedNT.add(previousMatch.getPatternToMatch());
				}
				_fullSubGrammarBuffer.remove(previousMatch);
			}	 
		}
		
		_lastMatchedNonterminals.clear();
		// The patterns in the previous recent matches that
		for (Matcher full:fullMatchers)
		{
			updateForCompletion(gchar, full.getParent(), null, null);
		}
		fullMatchers.clear();
		
		
	}
	
	public void finalize()
	{
		HashSet<String> processedNT = new HashSet<String>();
		StringBuilder captureList;
		for (Matcher previousMatch:_fullCaptureBuffer.keySet())
		{
			captureList = _fullCaptureBuffer.get(previousMatch);
			if (previousMatch.isFinalized() && !processedNT.contains(previousMatch.getPatternToMatch()))
			{
				LinkedList<String> previous = _outMap.get(previousMatch.getPatternToMatch());
				if (previous == null)
				{
					previous = new LinkedList<String>();
					_outMap.put(previousMatch.getPatternToMatch(), previous);
				}
				previous.addLast(captureList.toString());
				processedNT.add(previousMatch.getPatternToMatch());
			}
				 
		}
		processedNT.clear();
		
		HashSet<String> subcaptureSet;
		LinkedList<HashSet<String>> subcaptureList;
		for (Matcher previousMatch:_fullSubGrammarBuffer.keySet())
		{
			subcaptureSet = _fullSubGrammarBuffer.get(previousMatch);
			if (previousMatch.isFinalized() && !processedNT.contains(previousMatch.getPatternToMatch()))
			{
				subcaptureList = _subGrammarOutputMap.get(previousMatch.getPatternToMatch());
				if (subcaptureList == null)
				{
					subcaptureList = new LinkedList<HashSet<String>>();
					_subGrammarOutputMap.put(previousMatch.getPatternToMatch(), subcaptureList);
				}
				subcaptureList.addLast(subcaptureSet);
				processedNT.add(previousMatch.getPatternToMatch());
			}
		}
		
		_fullCaptureBuffer.clear();
		_fullSubGrammarBuffer.clear();
		
	}

	private String getSubGrammar(Matcher base, Matcher prev, Matcher target)
	{
		if (prev!=null && target!=null && prev.isAlternation() && target.isNonTerminal())
			return target.getPatternToMatch();
		else
			return null;
	}
	
	private void updateForCompletion(String gchar, Matcher full, Matcher previous, Matcher prevprevious)
	{
		if (full == null)
			return;
		String nonterminalName = full.getPatternToMatch();
		if (full.isNonTerminal()  && patternToCaptureP(full))
		{
			if (_nonTerminalsToCapture.contains(nonterminalName) && !_lastMatchedNonterminals.contains(full))
			{
				StringBuilder prior = _fullCaptureBuffer.get(full);
				if (prior == null)
				{
					prior = new StringBuilder();
					_fullCaptureBuffer.put(full, prior);
				}
				prior.append(gchar);
				_lastMatchedNonterminals.add(full);
			}
			
			if (_subGrammars.contains(nonterminalName))
			{
				String subgrammarName =  getSubGrammar(full, previous, prevprevious);
				if (subgrammarName!=null)
				{
					HashSet<String> subMatches = _fullSubGrammarBuffer.get(full);
					if (subMatches == null)
					{
						subMatches = new HashSet<String>();
						_fullSubGrammarBuffer.put(full, subMatches);
					}
					subMatches.add(subgrammarName);
				}
				
			}
			
		}
		
		updateForCompletion(gchar, full.getParent(), full, previous);
	}
	
	private boolean patternToCaptureP(Matcher m)
	{
		return _subGrammars.contains(m.getPatternToMatch()) || _nonTerminalsToCapture.contains(m.getPatternToMatch());
	}
	
	private void updateForParent(HashSet<Matcher> newNonterminals, Matcher partial)
	{
		if (partial == null)
			return;
		
		if (partial.isNonTerminal())
		{
			_recentCaptures.add(partial.getPatternToMatch());
			if (patternToCaptureP(partial))
				newNonterminals.add(partial);
		}
		
		updateForParent(newNonterminals, partial.getParent());
	}
	
	
	public  LinkedList<String> getAllCompletedMatches(String ntName)
	{
		return _outMap.get(ntName);
	}
	
	public  LinkedList<HashSet<String>> getAllSubGrammarMatches(String ntName)
	{
		return _subGrammarOutputMap.get(ntName);
	}
	
	
	public void addCapturedGrammars(String nt, LinkedList<String> values)
	{
		LinkedList<String> o = _outMap.get(nt);
	}
	
	public HashSet<String> recentCaptures()
	{
		return _recentCaptures;
	}
	
	public static KeyValuePair<String, CaptureMap> capturePrefixPattern(PatternParser parser, String grammar, String input, String[] captureStrings, String[] subGrammars)
	{
		return capturePrefixPattern(parser, grammar, input, captureStrings, subGrammars, false, true);
	}
	
	public static KeyValuePair<String, CaptureMap> capturePrefixPattern(PatternParser parser, String grammar, String input, String[] captureStrings, String[] subGrammars, boolean stopOnFinalize, boolean partition)
	{
		PatternParser.MatcherController controller = parser.getIncrementalParser(grammar);
		return capturePrefixPattern(controller, input, captureStrings, subGrammars, stopOnFinalize, partition);
	}
	
//	public static KeyValuePair<String, CaptureMap> captureAll(PatternParser parser, String grammar, String input, String[] captureStrings, String[] subGrammars)
//	{
//		String remaining = input;
//		CaptureMap map = new CaptureMap(), tmap;
//		map.setGrammarsToCapture(captureStrings);
//		map.setSubGrammars(subGrammars);
//		PatternParser.MatcherController controller = parser.getIncrementalParser(grammar);
//		KeyValuePair<String, CaptureMap> out = capturePrefixPattern(parser, grammar, remaining, captureStrings, subGrammars);
//		
//		
//		LinkedList<String> temp;
//		if ((tmap = out.GetValue())!=null)
//		{
//			
//		}
//		while (out.GetKey()!=null && out.GetKey().length()>0)
//		{
//			
//		}
//		return out;
//	}
	
	
	public static KeyValuePair<String, CaptureMap> capturePrefixPattern(PatternParser.MatcherController controller, String input, String[] captureStrings, String[] subGrammars, boolean stopOnFinalize, boolean partition)
	{
		CaptureMap map = new CaptureMap();
		map.setGrammarsToCapture(captureStrings);
		map.setSubGrammars(subGrammars);

		String remaining = input;
		
		for (char gchar:input.toCharArray())
		{
			controller.update(gchar, map);
			
			
			if (map.recentCaptures().size()>0 && (!stopOnFinalize || !controller.isFinalized()))
			{
				remaining = remaining.substring(1);
			}
			else
				break;
		}
		map.finalize();
		
		if (!controller.isFinalized())
			return new KeyValuePair(remaining, null);
		else
			return new KeyValuePair(remaining, map);
	}
	
	
	public static KeyValuePair<String, CaptureMap> captureAll(PatternParser.MatcherController controller, String input, String[] captureStrings, String[] subGrammars, boolean stopOnFinalize, boolean partition)
	{
		CaptureMap map = new CaptureMap();
		map.setGrammarsToCapture(captureStrings);
		map.setSubGrammars(subGrammars);

		boolean stop = false;
		String remaining = input;
		boolean finish = false;
		
		for (char gchar:input.toCharArray())
		{
			stop = controller.update(gchar, map);
			
			if (map.recentCaptures().size()>0 && (!stopOnFinalize || !controller.isFinalized()))
				remaining = remaining.substring(1);
			else
				controller.reset();
			
			
		}
		map.finalize();
		
		if (!controller.isFinalized())
			return new KeyValuePair(remaining.toString(), null);
		else
			return new KeyValuePair(remaining.toString(), map);
	}
	
	public static KeyValuePair<KeyValuePair<String, String>, CaptureMap> partition(PatternParser.MatcherController controller, String input, String[] captureStrings, String[] subGrammars)
	{
		StringBuilder prefix = new StringBuilder();
		String suffix = "";
		
		CaptureMap outMap = null;
		String remaining = input;
		KeyValuePair<String, CaptureMap> out = null;
		while (remaining.length()>0 && outMap == null)
		{
			out = capturePrefixPattern(controller, remaining, captureStrings, subGrammars, false, true);
			outMap = out.GetValue();
			if (outMap == null)
			{
				prefix.append(remaining.charAt(0));
				remaining =remaining.substring(1);
				controller.reset();
			}
			else
			{
				suffix = out.GetKey();
			}
			
		}
		
		KeyValuePair<String, String> prefixSuffixPair = new KeyValuePair<String, String>(prefix.toString(), suffix);
		return new KeyValuePair<KeyValuePair<String, String>, CaptureMap>(prefixSuffixPair, outMap);
		
	}
	
	public static KeyValuePair<KeyValuePair<String, String>, CaptureMap> partition(PatternParser parser, String grammar, String input, String[] captureStrings, String[] subGrammars)
	{
		PatternParser.MatcherController controller = parser.getIncrementalParser(grammar);
		return partition(controller, input, captureStrings, subGrammars);
	}
	
	public static HashMap<String, LinkedList<LinkedList<GeneralizedCharacter>>> captureByTagTokenized(PatternParser parser, String grammar, String[] inputTokens, String[] tagsToCapture)
	{
		PatternParser.MatcherController controller = parser.getIncrementalParser(grammar);
		return captureByTagTokenized(controller, inputTokens, tagsToCapture);
		
	}
	
	
	public static HashMap<String, LinkedList<LinkedList<GeneralizedCharacter>>> captureByTagTokenized(PatternParser.MatcherController controller, String[] inputTokens, String[] tagsToCapture)
	{
		
		HashMap<String, CaptureState> stateMap = new HashMap<String, CaptureMap.CaptureState>();
		for (String tag:tagsToCapture)
		{
			stateMap.put(tag, getNotMatchingState(CaptureStateData.getInitial()));
			
		}
		
		boolean finish = false;
		HashSet<Matcher> matches = null; 
		HashSet<Matcher> parentMatchers = null;
		HashSet<Matcher> expectedMatches = null;
		HashSet<Matcher> smaller = null;
		HashSet<Matcher> larger = null;
		
		HashSet<Matcher> partialMatches = null;
		HashSet<Matcher> fullAndPartialMatches = new HashSet<Matcher>();
		HashSet<String> nonMatchingTags = null ;
		CaptureState nextState, currentState;
		String tagName;
		TextCharacter gchar;
		for (String token:inputTokens)
		{
			gchar = new TextCharacter(token, false);
			finish = controller.update(gchar);
			if (controller._postponedMatchers.size()==0)
				break;
			nonMatchingTags = new HashSet<String>();
			for (String tag:tagsToCapture)
			{
				nonMatchingTags.add(tag);
			}
			matches = controller.getFullyMatchedMatchers(false, true, true);
			matches = filterForTagged(matches, stateMap);
			
			HashSet<Matcher> test = controller.getMatchedMatchers(true, false, true);
			parentMatchers = filterForTagged(test, stateMap);
			test = controller.getMatchersExpectedToMatch(true);
			expectedMatches = filterForTagged(test, stateMap);
			smaller = (parentMatchers.size()<expectedMatches.size())?parentMatchers:expectedMatches;
			larger = (smaller == parentMatchers)?expectedMatches:parentMatchers;
			partialMatches = new HashSet<Matcher>();
			fullAndPartialMatches.clear();
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
			}
			
			for (Matcher m:partialMatches)
			{
				tagName = m.getTag();
				currentState = stateMap.get(tagName);
				nextState = currentState.partialMatchesOnly(gchar);
				stateMap.put(tagName, nextState);
				nonMatchingTags.remove(tagName);
			}
			
			for (Matcher m:fullAndPartialMatches)
			{
				tagName = m.getTag();
				currentState = stateMap.get(tagName);
				nextState = currentState.partialAndFullMatches(gchar);
				stateMap.put(tagName, nextState);
				nonMatchingTags.remove(tagName);
			}
			
			for (String tag:nonMatchingTags)
			{
				currentState = stateMap.get(tag);
				nextState = currentState.noMatches(gchar);
				stateMap.put(tag, nextState);
			}
		}
		
		HashMap<String, LinkedList<LinkedList<GeneralizedCharacter>>> output = new HashMap<String, LinkedList<LinkedList<GeneralizedCharacter>>>();
		for (String tag:stateMap.keySet())
		{
			output.put(tag, stateMap.get(tag).getData());
		}
		return output;
		
	}
	
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
	
	private static class CaptureStateData
	{
		LinkedList<GeneralizedCharacter> partialQueue;
		LinkedList<LinkedList<GeneralizedCharacter>> matchList;
		int matchIndex=-1;
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
					_data.matchIndex++;
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
	
	private static CaptureState getNotMatchingState(final CaptureStateData data)
	{
		CaptureState state = new CaptureState(data)
		{

			{
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
