package com.evolved.automata.parser.general;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.LinkedList;

import com.evolved.automata.KeyValuePair;

public class GeneralizedCaptureMap <T extends GeneralizedCharacter>
{
	
	private HashMap<Matcher, LinkedList<T>> _fullCaptureBuffer;
	private HashMap<Matcher, HashSet<String>> _fullSubGrammarBuffer;
	
	private HashSet<Matcher> _lastMatchedNonterminals;
	
	private HashSet<String> _nonTerminalsToCapture;
	private HashSet<String> _subGrammars;
	private HashMap<String, LinkedList<LinkedList<T>>> _outMap; 
	
	private HashMap<String, LinkedList<HashSet<String>>> _subGrammarOutputMap;
	
	private HashSet<String> _recentCaptures;
	
	
	
	public GeneralizedCaptureMap()
	{
		reset();
	}
	
	public void reset()
	{
		_subGrammarOutputMap = new HashMap<String, LinkedList<HashSet<String>>>();
		_subGrammars = new HashSet<String>();
		_nonTerminalsToCapture = new HashSet<String>();
		_fullCaptureBuffer = new HashMap<Matcher, LinkedList<T>>();
		_outMap = new HashMap<String, LinkedList<LinkedList<T>>>();
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
	
	public void update(T gchar, HashSet<Matcher> fullMatchers)
	{
		HashSet<Matcher> newMatchSet = new HashSet<Matcher>();
		_recentCaptures = new HashSet<String>();
		if ((fullMatchers == null || fullMatchers.size() == 0))
		{
			return;
		}
		
		for (Matcher m:fullMatchers)
			updateForParent(newMatchSet, m);
		
		LinkedList<T> captureList;
		LinkedList<LinkedList<T>> fullOut;
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
						fullOut = new LinkedList<LinkedList<T>>();
						_outMap.put(previousMatch.getPatternToMatch(), fullOut);
					}
					fullOut.addLast(captureList);
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
		LinkedList<T> captureList;
		for (Matcher previousMatch:_fullCaptureBuffer.keySet())
		{
			captureList = _fullCaptureBuffer.get(previousMatch);
			if (previousMatch.isFinalized() && !processedNT.contains(previousMatch.getPatternToMatch()))
			{
				LinkedList<LinkedList<T>> previous = _outMap.get(previousMatch.getPatternToMatch());
				if (previous == null)
				{
					previous = new LinkedList<LinkedList<T>>();
					_outMap.put(previousMatch.getPatternToMatch(), previous);
				}
				previous.addLast(captureList);
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
	
	private void updateForCompletion(T gchar, Matcher full, Matcher previous, Matcher prevprevious)
	{
		if (full == null)
			return;
		String nonterminalName = full.getPatternToMatch();
		if (full.isNonTerminal()  && patternToCaptureP(full))
		{
			if (_nonTerminalsToCapture.contains(nonterminalName) && !_lastMatchedNonterminals.contains(full))
			{
				LinkedList<T> prior = _fullCaptureBuffer.get(full);
				if (prior == null)
				{
					prior = new LinkedList<T>();
					_fullCaptureBuffer.put(full, prior);
				}
				prior.addLast(gchar);
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
	
	
	public  LinkedList<LinkedList<T>> getAllCompletedMatches(String ntName)
	{
		return _outMap.get(ntName);
	}
	
	public  LinkedList<HashSet<String>> getAllSubGrammarMatches(String ntName)
	{
		return _subGrammarOutputMap.get(ntName);
	}
	
	public HashSet<String> recentCaptures()
	{
		return _recentCaptures;
	}
	
	public static KeyValuePair<LinkedList<TextCharacter>, GeneralizedCaptureMap<TextCharacter>> captureAll(PatternParser parser, String grammar, LinkedList<TextCharacter> input, String[] captureStrings, String[] subGrammars)
	{
		return captureAll(parser, grammar, input, captureStrings, subGrammars, false, true);
	}
	public static KeyValuePair<LinkedList<TextCharacter>, GeneralizedCaptureMap<TextCharacter>> captureAll(PatternParser parser, String grammar, LinkedList<TextCharacter> input, String[] captureStrings, String[] subGrammars, boolean stopOnFinalize, boolean partition)
	{
		PatternParser.MatcherController controller = parser.getIncrementalParser(grammar);
		return captureAll(controller, input, captureStrings, subGrammars, stopOnFinalize, partition);
	}
	
	public static KeyValuePair<LinkedList<TextCharacter>, GeneralizedCaptureMap<TextCharacter>> captureAll(PatternParser.MatcherController controller, LinkedList<TextCharacter> input, String[] captureStrings, String[] subGrammars, boolean stopOnFinalize, boolean partition)
	{
		GeneralizedCaptureMap<TextCharacter> map = new GeneralizedCaptureMap<TextCharacter>();
		map.setGrammarsToCapture(captureStrings);
		map.setSubGrammars(subGrammars);

		boolean stop = false;
		LinkedList<TextCharacter> remaining = new LinkedList<TextCharacter>();
		remaining.addAll(input);
		
		for (TextCharacter gchar:input)
		{
			stop = controller.update(gchar, map);
			
			if (map.recentCaptures().size()>0 && (!stopOnFinalize || !controller.isFinalized()))
				remaining.removeFirst();
			else
				break;
			
		}
		map.finalize();
		
		if (!controller.isFinalized())
			return new KeyValuePair(remaining, null);
		else
			return new KeyValuePair(remaining, map);
	}
	
}
