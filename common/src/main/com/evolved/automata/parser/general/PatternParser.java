package com.evolved.automata.parser.general;
import java.io.BufferedReader;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Serializable;
import java.util.*;
import java.util.regex.*;


public class PatternParser 
{

	/**
	 * CFGParser is the main class for parsing and matching strings against a context-free grammar.  The syntax for the <br/>
	 * context-free grammar uses an EBNF-like syntax.  See http://phase-summary.blogspot.com/2012/05/context-free-grammar-parser-and-pattern.html<br/>
	 * CFGParser actually supports a syntax that is more expressive than context-free grammars, which is why I use the <br/>
	 * term, EBNF-like syntax.  The actual syntax is a combination of regex notation and Extended Backus-Naur form.
	 * @author Evolved8
	 *
	 */
	
	public static class GlobalState
	{
		static final long serialVersionUID = 1;
		HashSet<String> _predicateFunctions;
		Hashtable<String, Matcher> _parseMap;
		
		HashMap<String, CustomTerminalMatcher> _customMatchers = new HashMap<String, CustomTerminalMatcher>();
		
		HashSet<String> _deletedPatterns;
		HashSet<String> captureNames;
		Sequence _buffer;
		boolean _robotMode = false;
		String[] _definitions;
		boolean _stringAsSingleCharacterP = false;
		LinkedList<TerminalMatcher> bottomStartMatchers;
		boolean _getBottomMatchersP = false;
		Hashtable<String, String> _nonTerminalBaseDefinition;
		
		public Hashtable<String, Matcher> getParseMap()
		{
			return _parseMap;
		}
		
		public String[] getNonterminalGrammarDefinition()
		{
			String[] out = new String[_parseMap.size()];
			int i=0;
			for (String nt:_parseMap.keySet())
			{
				out[i++] = nt + " == " + _parseMap.get(nt).getPatternToMatch() + ";";
			}
			return out;
		}
		
		public String getNonterminalGrammarDefinition(String nt)
		{
			if (_nonTerminalBaseDefinition.containsKey(nt))
				return _nonTerminalBaseDefinition.get(nt);
			return null;
		}
		
		public void setNonterminalDefinition(Hashtable<String, String > ntDef)
		{
			_nonTerminalBaseDefinition = ntDef;
		}
		
		public String[] getNonterminalDefinition()
		{
			String[] out = new String[_nonTerminalBaseDefinition.size()];
			int i=0;
			for (String nt:_nonTerminalBaseDefinition.keySet())
			{
				out[i++] = nt + " == " + _nonTerminalBaseDefinition.get(nt) + ";";
			}
			return out;
		}
		
		public void clearBottomMatchers()
		{
			bottomStartMatchers.clear();
		}
		
		public boolean bottomMatchersEnabledP()
		{
			return _getBottomMatchersP;
		}
		
		public void enableBottomMatchers(boolean newState)
		{
			_getBottomMatchersP = newState;
		}
		
		public void addToBottomMatchers(TerminalMatcher matcher)
		{
			bottomStartMatchers.add(matcher);
		}
		
		public LinkedList<TerminalMatcher> getBottomMatchers()
		{
			return bottomStartMatchers;
		}
		
		public GlobalState(HashSet<String> predicateFunctions, Hashtable<String, Matcher> parseMap, boolean stringAsCharacter)
		{
			bottomStartMatchers = new LinkedList<TerminalMatcher>();
			_predicateFunctions = predicateFunctions;
			_parseMap = parseMap;
			_buffer = new Sequence();
			_deletedPatterns = new HashSet<String>();
			_stringAsSingleCharacterP = stringAsCharacter;
		}
		
		
		
		
		
		public GlobalState clone()
		{
			GlobalState g = new GlobalState(_predicateFunctions, _parseMap, _stringAsSingleCharacterP);
			g._robotMode=_robotMode;
			g._deletedPatterns = new HashSet<String>();
			g._customMatchers = this._customMatchers;
			for (String key:_deletedPatterns)
				g._deletedPatterns.add(key);
			if (captureNames!=null)
			{
				g.captureNames = captureNames;
			}
			g._nonTerminalBaseDefinition = (Hashtable<String, String>)_nonTerminalBaseDefinition.clone();
			return g;
		}
		
		public void setCustomTerminalMatcher(String name, CustomTerminalMatcher cm)
		{
			_customMatchers.put(name, cm);
			_predicateFunctions.add(name);                                             
		}
		
		public CustomTerminalMatcher getCustomTerminalMatcher(String name)
		{
			return _customMatchers.get(name);
		}
		
		/**
		 * Adds a new non-terminal to this PatternParser
		 * @param nonTerminalName
		 * @param grammarDef
		 * @return
		 */
		public Matcher addNonterminal(String nonTerminalName, String grammarDef)
		{
			Matcher matcher = Parser.parse(this, grammarDef, null);
			_parseMap.put(nonTerminalName, matcher);
			_nonTerminalBaseDefinition.put(nonTerminalName, grammarDef);
			_deletedPatterns.remove(nonTerminalName);
			return matcher;
		}
		
		/**
		 * Adds a nonterminal to the number of possible alternatives to the mapped value of another<br/>
		 * nonterminal
		 * @param nonTerminalName
		 * @param disjunctiveNonTerminal
		 * @param When true, creates [targetNonTerminalName] when it does not exists
		 */
		public void addNonterminalToDisjunction(String targetNonTerminalName, String disjunctiveChildNonTerminal, boolean createIfTargetNotExists)
		{
			NonTerminalMatcher nt = new NonTerminalMatcher(disjunctiveChildNonTerminal, null);
			if (_parseMap.containsKey(targetNonTerminalName))
			{
				Matcher target = _parseMap.get(targetNonTerminalName);
				if (target instanceof AlternationMatcher)
				{
					((AlternationMatcher)target).addChildMatcher(nt, false);
					_nonTerminalBaseDefinition.put(targetNonTerminalName, target.getPatternToMatch());
				}
				else if (target instanceof NonTerminalMatcher)
				{
					AlternationMatcher root = new AlternationMatcher(disjunctiveChildNonTerminal + " | " + target.getPatternToMatch() , new Matcher[]{nt, target});
					_parseMap.put(targetNonTerminalName, root);
					_nonTerminalBaseDefinition.put(targetNonTerminalName, root.getPatternToMatch());
				}
				
			}
			else if (createIfTargetNotExists)
			{
				AlternationMatcher root = new AlternationMatcher(disjunctiveChildNonTerminal, new Matcher[]{nt});
				_parseMap.put(targetNonTerminalName, root);
				_nonTerminalBaseDefinition.put(targetNonTerminalName, root.getPatternToMatch());
			}
			
		}
		
		public void addNonterminalToDisjunction(String targetNonTerminalName, String disjunctiveChildNonTerminal)
		{
			addNonterminalToDisjunction(targetNonTerminalName, disjunctiveChildNonTerminal, true);
		}
		
		
		public String[] getNonterminals()
		{
			return _parseMap.keySet().toArray(new String[0]);
		}
		
		public void deleteNonTerminal(String nonTerminal, boolean permanently)
		{
			_deletedPatterns.add(nonTerminal);
			if (permanently)
				_parseMap.remove(nonTerminal);
			_nonTerminalBaseDefinition.remove(nonTerminal);
		}
		
		public void deleteNonTerminal(String nonTerminal)
		{
			deleteNonTerminal(nonTerminal, true);
		}
		

	}
	
	public static class LocalState
	{
		static final long serialVersionUID = 1;
		
		int _startIndex;
		int _indexInParent;
		Matcher _parent;
		
		public LocalState(Matcher parent, int indexInParent, int startIndex)
		{
			_startIndex = startIndex;
			_parent = parent;
			_indexInParent = indexInParent;
		}
	}
	
	static class EndOfStreamException extends RuntimeException
	{
		Matcher _postponed;
		public EndOfStreamException(Matcher postponed)
		{
			super();
			_postponed=postponed;
		}
	}
	
	public static class MatcherController
	{
		LinkedList<Matcher> _activeMatchers;
		Sequence _buffer;
		LinkedList<Matcher> _postponedMatchers;
		HashSet<Matcher> _completedMatchers;
		Matcher _rootMatcher;
		HashSet<String> _successPaths;
		GlobalState _global;
		String _pattern;
		boolean _unknownCharacterP = false;
		boolean _anyMatches = false;
		public MatcherController(Matcher root, GlobalState baseState)
		{
			_global = baseState;
			_buffer = new Sequence(); 
			_rootMatcher = root.clone(_global, new LocalState(null, 0, 0));
			_global._buffer=_buffer;
			_pattern = root.getPatternToMatch();
			initializeState();
			_activeMatchers.add(_rootMatcher);
		}
		
		private void initializeState()
		{
			_activeMatchers = new LinkedList<Matcher>();
			_buffer.clear();
			_successPaths = new HashSet<String>();
			_postponedMatchers = new LinkedList<Matcher>();
			_completedMatchers = new HashSet<Matcher>();
			
		}
		
		public String getRootPattern()
		{
			return _pattern;
		}
		
		public Sequence getProcessedSequence()
		{
			return _buffer;
		}
		
		public boolean isFinalized()
		{
			return _rootMatcher.isFinalized();
		}
		
		public void clearBottomMatchers()
		{
			_global.clearBottomMatchers();
		}
		
		public LinkedList<TerminalMatcher> sampleTerminals(String pattern)
		{
			Matcher m = Parser.parse(_global, pattern, null);
			return sampleTerminals(m);
		}
		
		
		
		public LinkedList<TerminalMatcher> sampleTerminals()
		{
			return sampleTerminals((Matcher)null);
		}
		
		public LinkedList<TerminalMatcher> sampleTerminals(Matcher base)
		{
			LinkedList<TerminalMatcher> list = new LinkedList<TerminalMatcher>();
			
			reset();
			if (base != null)
			{
				base = base.clone(_global, new LocalState(null, 0, 0)); 
				_activeMatchers = new LinkedList<Matcher>();
				_activeMatchers.add(base);
			}
			
			boolean finished = update(ControlCharacter.createStartCharacter());
			Matcher selected = null;
			while (!finished)
			{
				selected = ChooseRandom(_postponedMatchers);
				list.add((TerminalMatcher)selected);
				_postponedMatchers.clear();
				_postponedMatchers.add(selected);
				finished = update(ControlCharacter.createSkipCharacter());
			}
			return list;
		}
		
		public LinkedList<GeneralizedCharacter> sample(HashMap<String, LinkedList<GeneralizedCharacter>> tagSampleMap)
		{
			String sampledTag = null;
			LinkedList<GeneralizedCharacter> list = new LinkedList<GeneralizedCharacter>();
			boolean finished = false;
			if (getProcessedSequence().size() == 0 || _postponedMatchers.size()==0)
			{
				reset();
				finished = update(ControlCharacter.createStartCharacter());
			}
			
			Matcher selected = null;
			GeneralizedCharacter controlCharacter;
			
			HashSet<String> tagset;
			
			HashSet<String> previous = new HashSet<String>(), buffer, buffer2;
			
			Matcher node = null;
			while (!finished)
			{
				selected = ChooseRandom(_postponedMatchers);
				
				_postponedMatchers.clear();
				_postponedMatchers.add(selected);
				controlCharacter = ControlCharacter.createSkipCharacter();
				finished = update(controlCharacter);
				
				tagset = controlCharacter.getTagSet();
				boolean tagsCaptured = tagset!=null && tagset.size()>0;
				if (tagSampleMap == null || tagSampleMap.size() == 0 || tagset == null || tagset.size() == 0 || previous.size() == 0)
				{
					if (previous.size()>0)
					{
						buffer = new HashSet<String>();
						sampledTag = null;
						int length = 0;
						if (tagSampleMap!=null)
						{
							for (String tag:previous)
							{
								if (tagSampleMap.containsKey(tag))
								{
									if (length == 0 || tagSampleMap.get(tag).size() > length)
									{
										length = tagSampleMap.get(tag).size();
										sampledTag = tag;
									}
									
								}
								
							}
						}
						
						if (sampledTag != null)
						{
							list.addAll(tagSampleMap.get(sampledTag));
							
						}
						previous.clear();
					}
					
					if (!tagsCaptured || tagSampleMap == null || tagSampleMap.size() == 0)
						list.add(((TerminalMatcher)selected).sample());
					else
					{
						for (String tag:tagset)
						{
							if (tagSampleMap.containsKey(tag))
							{
								tagsCaptured = true;
								break;
							}
							tagsCaptured = false;
						}
						
						if (!tagsCaptured)
							list.add(((TerminalMatcher)selected).sample());
					}
					
					if (previous.size() == 0 && tagsCaptured)
					{
						previous.addAll(tagset);
					}
					tagsCaptured = false;
					
				}
				else
				{
					buffer = new HashSet<String>();
					buffer2 = new HashSet<String>();
					sampledTag = null;
					for (String tag:previous)
					{
						if (tagset.contains(tag) && tagSampleMap.containsKey(tag))
						{
							buffer.add(tag);
							
						}
						else
						{
							sampledTag = tag;
							if (tagSampleMap.containsKey(tag))
								buffer2.add(tag);
						}
					}
					
					if (buffer.size() == 0)
					{
						list.addAll(tagSampleMap.get(sampledTag));
						if (buffer2.size()>0)
						{
							previous.addAll(buffer2);
							
						}
						else
							previous = buffer;
					}
					else
					{
						tagsCaptured = true;
						previous = buffer;
					}
					
					
				}
				
				while (selected.getParent()!=null)
				{
					selected = selected.getParent();
				}
				if (selected.isFinalized())
				{
					buffer = new HashSet<String>();
					sampledTag = null;
					int length = 0;
					for (String tag:previous)
					{
						if (tagset.contains(tag) && tagSampleMap!=null && tagSampleMap.containsKey(tag))
						{
							if (length == 0 || tagSampleMap.get(tag).size() > length)
							{
								length = tagSampleMap.get(tag).size();
								sampledTag = tag;
							}
							
						}
						
					}
					
					if (sampledTag != null)
					{
						list.addAll(tagSampleMap.get(sampledTag));
						
					}
					break;
				}
				
			}
			
			return list;
		}
		
		public LinkedList<LinkedList<TerminalMatcher>> extrude()
		{
			return extrude((Matcher)null, true);
		}
		
		public LinkedList<LinkedList<TerminalMatcher>> extrude(String pattern)
		{
			Matcher m = Parser.parse(_global, pattern, null);
			initializeState();
			_global.enableBottomMatchers(false);
			return extrude(m.clone(_global, new LocalState(null, 0, 0)), true);
		}
		
		private LinkedList<LinkedList<TerminalMatcher>> extrude(Matcher base, boolean useStart)
		{
			if (base != null)
				_activeMatchers.add(base);
			else if (_postponedMatchers.size() == 0)
				reset();
			
			LinkedList<LinkedList<TerminalMatcher>> allists = new LinkedList<LinkedList<TerminalMatcher>>();
			
			
			
			boolean finished;
			if (useStart)
				finished = update(ControlCharacter.createStartCharacter());
			else
				finished = update(ControlCharacter.createSkipCharacter());
			
			if (!finished)
			{
				LinkedList<Matcher> myPostponed = new LinkedList<Matcher>();
				myPostponed.addAll(_postponedMatchers);
				_postponedMatchers.clear();
				for (Matcher selected:myPostponed)
				{
					LinkedList<LinkedList<TerminalMatcher>> recursiveResult = extrude(selected, false);
					_buffer.removeLast();
					if (recursiveResult.size()>0)
					{
						for (LinkedList<TerminalMatcher> ilist:recursiveResult)
						{
							ilist.addFirst((TerminalMatcher)selected);
							allists.add(ilist);
						}
					}
					else
					{
						LinkedList<TerminalMatcher> ilist = new LinkedList<TerminalMatcher>();
						ilist.add((TerminalMatcher)selected);
						allists.add(ilist);
					}
					
				}
				
				
			}
			return allists;
		}
		
		public void rollbackOne()
		{
			_buffer.removeLast();
		}
		
		public boolean checkAnyMatches(GeneralizedCharacter gchar)
		{
			reset();
			if (!gchar._isStartCharacter)
				_buffer.add(gchar);
			LinkedList<Matcher> nextSet=null;
			LinkedList<Matcher> nextIter = null;
			Matcher root = null;
			HashMap<Matcher, Matcher> rootMap = new HashMap<Matcher, Matcher>();
			
			LinkedList<Matcher> newPostponed = new LinkedList<Matcher>();
			while (_activeMatchers.size()>0)
			{
				nextIter = new LinkedList<Matcher>();
				for (Matcher prior:_activeMatchers)
				{
					try
					{
						nextSet = prior.match();
						if (nextSet != null && prior.isTerminal())
							return true;
					}
					catch (EndOfStreamException ee)
					{
						
						newPostponed.add(ee._postponed);
						nextSet = null;
					}
					catch (ConcurrentModificationException ce)
					{
						return false;
					}
					
					
					if (nextSet!=null)
					{
						root = rootMap.get(prior);
						if (root == null && prior.isTerminal() && prior.isFinalized())
						{
							root = prior;
							rootMap.put(prior, prior);
						}
						
						
						for (Matcher nextMatcher:nextSet)
						{
							
							if (!nextMatcher.isFinalized())
							{
								nextIter.add(nextMatcher);
								if (root!=null)
									rootMap.put(nextMatcher, root);
							}
							
						}
					}
				}
				_activeMatchers = nextIter;
			}
			
			return false;
		}
		
		/**
		 * Returns true if the last gchar that was processed in update(gchar) matched any terminal
		 * matchers.  This flag resets to false when reset() is called
		 * @return
		 */
		public boolean anyMatches()
		{
			return _anyMatches;
		}
		
		
		/**
		 * 
		 * @param gchar
		 * @return Returns true when matching process has completed, either due to a match failure or
		 * 		   or success.  Check getCurrentMatches to see if the parsing was successful or not
		 */
		public boolean update(GeneralizedCharacter gchar)
		{
			if (!gchar._isStartCharacter)
				_buffer.add(gchar);
			LinkedList<Matcher> nextSet=null;
			LinkedList<Matcher> nextIter = null;
			_activeMatchers.addAll(_postponedMatchers);
			Matcher root = null;
			HashMap<Matcher, Matcher> rootMap = new HashMap<Matcher, Matcher>();
			
			LinkedList<Matcher> newPostponed = new LinkedList<Matcher>();
			String successPath;
			_anyMatches = false;
			while (_activeMatchers.size()>0)
			{
				nextIter = new LinkedList<Matcher>();
				for (Matcher prior:_activeMatchers)
				{
					try
					{
						nextSet = prior.match();
					}
					catch (EndOfStreamException ee)
					{
						
						newPostponed.add(ee._postponed);
						nextSet = null;
					}
					catch (ConcurrentModificationException ce)
					{
						
						throw new RuntimeException(ce.toString());
					}
					
					_anyMatches = _anyMatches || (prior.isTerminal() && prior.isFinalized());
					if (nextSet!=null)
					{
						
						root = rootMap.get(prior);
						if (root == null && prior.isTerminal() && prior.isFinalized())
						{
							root = prior;
							rootMap.put(prior, prior);
						}
						if (root!=null)
							_completedMatchers.add(root);
						
						for (Matcher nextMatcher:nextSet)
						{
							
							if (!nextMatcher.isFinalized())
							{
								nextIter.add(nextMatcher);
								if (root!=null)
									rootMap.put(nextMatcher, root);
							}
							else
							{
								if (root!=null)
								{
									successPath = root.buildSuccessPath("");
									_successPaths.add(successPath);
								}
								
							}
							
						}
						
						
					}
				}
				_activeMatchers = nextIter;
			}
			
			
			_postponedMatchers = newPostponed;
			return _postponedMatchers.size() == 0;
		}
		
		public <GCHAR extends GeneralizedCharacter> boolean  update(GCHAR gchar, GeneralizedCaptureMap<GCHAR> map)
		{
			boolean ovalue = update(gchar);
			map.update(gchar, _completedMatchers);
			return ovalue;
		}
		
		public boolean update(TextCharacter gchar, CaptureMap map)
		{
			
			boolean ovalue = update(gchar);
			map.update(gchar.getTextValue(), _completedMatchers);
			return ovalue;
		}
		
		public boolean update(String gchar, CaptureMap map)
		{
			
			boolean ovalue = update(new TextCharacter(gchar, false));
			map.update(gchar, _completedMatchers);
			return ovalue;
		}
		
		public boolean update(char gchar, CaptureMap map)
		{
			boolean ovalue = update(new TextCharacter(gchar));
			map.update(gchar + "", _completedMatchers);
			return ovalue;
		}
		
		public boolean removeExpectedMatch(Matcher postponedMatcher)
		{
			if (_postponedMatchers!=null && _postponedMatchers.contains(postponedMatcher))
			{
				_postponedMatchers.remove(postponedMatcher);
				return true;
			}
			else
				return false;
		}
		
		public HashSet<String> getCurrentMatches()
		{
			return _successPaths;
		}
		
		public LinkedList<Matcher> getExpectedTerminalMatches()
		{
			return _postponedMatchers;
		}
		
		public HashSet<Matcher> getCompletedTerminalMatchers()
		{
			return _completedMatchers;
		}
		
		public HashSet<String> getExpectedNonterminalsToMatch()
		{
			HashSet<String> out = new HashSet<String>();
			for (Matcher m:_postponedMatchers)
				addParentNonterminals(out, m, false, false);
			return out;
		}
		
		public HashSet<String> getPartiallyMatchedNonterminals()
		{
			HashSet<String> out = new HashSet<String>();
			for (Matcher m:_completedMatchers)
				addIncompleteParentNonterminals(out, m);
			return out;
		}
		
		public HashSet<Matcher> getMatchersExpectedToMatch()
		{
			HashSet<Matcher> out = new HashSet<Matcher>();
			for (Matcher m:_postponedMatchers)
				addParentMatchers(out, m);
			
			return out;
		}
			
		public HashSet<Matcher> getMatchersExpectedToMatch(boolean allowTerminalsP)
		{
			HashSet<Matcher> out = new HashSet<Matcher>();
			for (Matcher m:_postponedMatchers)
			{
				if (allowTerminalsP)
					out.add(m);
				addParentMatchers(out, m);
			}
			
			return out;
		}
		
		public HashSet<String> getFullyMatchedNonterminals(boolean clear, boolean resetFinalization)
		{
			HashSet<String> out = new HashSet<String>();
			for (Matcher m:_completedMatchers)
				addParentNonterminals(out, m, true, resetFinalization);
			if (clear)
				_completedMatchers.clear();
			return out;
		}
		
		public HashSet<Matcher> getFullyMatchedMatchers(boolean clear, boolean resetFinalization)
		{
			HashSet<Matcher> out = new HashSet<Matcher>();
			for (Matcher m:_completedMatchers)
				addParentMatchers(out, m, true, resetFinalization);
			if (clear)
				_completedMatchers.clear();
			return out;
		}
		
		public HashSet<Matcher> getFullyMatchedMatchers(boolean clear, boolean resetFinalization, boolean allowTerminalsP)
		{
			HashSet<Matcher> out = new HashSet<Matcher>();
			for (Matcher m:_completedMatchers)
			{
				if (allowTerminalsP)
					out.add(m);
				addParentMatchers(out, m, true, resetFinalization);
			}
			if (clear)
				_completedMatchers.clear();
			return out;
		}
		
		
		public HashSet<Matcher> getMatchedMatchers(boolean clear, boolean resetFinalization)
		{
			HashSet<Matcher> out = new HashSet<Matcher>();
			for (Matcher m:_completedMatchers)
				addParentMatchers(out, m, false, resetFinalization);
			if (clear)
				_completedMatchers.clear();
			return out;
		}
		
		public HashSet<Matcher> getMatchedMatchers(boolean clear, boolean resetFinalization, boolean allowTerminalsP)
		{
			HashSet<Matcher> out = new HashSet<Matcher>();
			for (Matcher m:_completedMatchers)
			{
				if (allowTerminalsP)
					out.add(m);
				addParentMatchers(out, m, false, resetFinalization);
			}
			if (clear)
				_completedMatchers.clear();
			return out;
		}
		
		private void addParentNonterminals(HashSet<String> newNonterminals, Matcher partial, boolean checkFinalized, boolean resetFinalization)
		{
			if (partial == null)
				return ;
			
			if (partial.isNonTerminal() && ( !checkFinalized || partial.isFinalized(resetFinalization)))
			{
				newNonterminals.add(partial.getPatternToMatch());
			}
			
			addParentNonterminals(newNonterminals, partial.getParent(), checkFinalized, resetFinalization);
		}
		
		private void addIncompleteParentNonterminals(HashSet<String> newNonterminals, Matcher partial)
		{
			if (partial == null)
				return ;
			
			if (partial.isNonTerminal() &&  !partial.isFinalized())
			{
				newNonterminals.add(partial.getPatternToMatch());
			}
			
			addIncompleteParentNonterminals(newNonterminals, partial.getParent());
		}
		
		
		private void addParentMatchers(HashSet<Matcher> newNonterminals, Matcher partial)
		{
			if (partial == null)
				return ;
			
			if (partial.isNonTerminal() )
			{
				newNonterminals.add(partial);
			}
			
			addParentMatchers(newNonterminals, partial.getParent());
		}
		
		private void addParentMatchers(HashSet<Matcher> newNonterminals, Matcher partial, boolean checkFinalized, boolean resetFinalization)
		{
			if (partial == null)
				return ;
			
			if (partial.isNonTerminal() && ( !checkFinalized || partial.isFinalized(resetFinalization)))
			{
				newNonterminals.add(partial);
			}
			
			addParentMatchers(newNonterminals, partial.getParent(), checkFinalized, resetFinalization);
		}
		
		public HashSet<String> removeCurrentMatches()
		{
			HashSet<String> o = new HashSet<String>();
			if (_successPaths!=null && _successPaths.size()>0)
				o.addAll(_successPaths);
			_successPaths = new HashSet<String>();
			return o;
		}
		
		public HashSet<String> getPartialMatches()
		{
			HashSet<String> out = new HashSet<String>();
			for (Matcher m:_postponedMatchers)
			{
				out.add(m.buildSuccessPath(""));
			}
			return out;
		}
		
		public void reset()
		{
			reset(false);
			
		}
		
		public void reset(boolean bottomUpParsing)
		{
			initializeState();
			_unknownCharacterP = false;
			_anyMatches = false;
			if (bottomUpParsing)
			{
				_global.enableBottomMatchers(true);
				LinkedList<TerminalMatcher> base = _global.getBottomMatchers();
				if (base == null || base.size()==0)
				{
					Parser.parse(_global, _rootMatcher.getPatternToMatch(), null);
					base = _global.bottomStartMatchers;
				}
				Matcher temp;
				for (TerminalMatcher t:base)
				{
					temp = t.clone(_global, null);
					_activeMatchers.add(temp);
				}
			}
			else
			{
				_global.enableBottomMatchers(false);
				_rootMatcher = _rootMatcher.clone(_global, new LocalState(null, 0, 0));
				_activeMatchers.add(_rootMatcher);
			}
			
			
			
		}
	}
	static final long serialVersionUID = 1;
	
	GlobalState _baseState;
	
	String[] _nonTerminals;
	
	Hashtable<String, Integer> _weightDistribution;
	
	
	
	public PatternParser(String inputfileFullname) throws IOException
	{
		String[] definitionComponent = com.evolved.automata.filetools.StandardTools.getDataFileLines(inputfileFullname);
		init(definitionComponent, false);
	}
	
	public PatternParser(String[] grammarDefinition) 
	{
		init(grammarDefinition,  false);
	}
	
	public HashSet<String> getWordTerminals()
	{
		HashSet<String> outMap = new HashSet<String>();
		for (String grammar:_baseState._nonTerminalBaseDefinition.keySet())
			Parser.fillTerminalMap(_baseState, outMap, _baseState._nonTerminalBaseDefinition.get(grammar));
		return outMap;
	}
	
	public Matcher addNonterminal(String nonTerminalName, String grammarDef)
	{
		Matcher out = _baseState.addNonterminal(nonTerminalName, grammarDef);
		if (out!=null)
			_nonTerminals = _baseState._parseMap.keySet().toArray(new String[0]);
		return out; 
	}
	
	public void setCustomTerminalMatcher(String name, CustomTerminalMatcher cm)
	{
		_baseState.setCustomTerminalMatcher(name, cm);
	}
	
	public void removeNonterminalFromDisjunction(String nonTerminalName, String disjunctiveNonTerminal)
	{
		Matcher prior = _baseState._parseMap.get(disjunctiveNonTerminal);
//		if (prior != null)
//		{
			Matcher target = _baseState._parseMap.get(nonTerminalName);
			if (target != null && target.isAlternation())
			{
				String newDefinition =((AlternationMatcher)target).removeMatcher(disjunctiveNonTerminal);
				addNonterminal(nonTerminalName, newDefinition);
			}
//		}
		
	}
	
	public void addNonterminalToDisjunction(String nonTerminalName, String disjunctiveNonTerminal)
	{
		_baseState.addNonterminalToDisjunction(nonTerminalName, disjunctiveNonTerminal);
	}
	
	public void addNonterminalToDisjunction(String targetNonTerminalName, String disjunctiveNonTerminal, String disjunctiveDef)
	{
		if (!_baseState._parseMap.containsKey(disjunctiveNonTerminal))
			addNonterminal(disjunctiveNonTerminal, disjunctiveDef);
		addNonterminalToDisjunction(targetNonTerminalName, disjunctiveNonTerminal);
	}
	
	
	public String[] getSubDefinitions(String nonTerminalName)
	{
		Hashtable<String, Matcher> map = _baseState._parseMap;
		Matcher alter = map.get(nonTerminalName);
		if (alter instanceof AlternationMatcher)
		{
			LinkedList<String> nt = new LinkedList<String>();
			AlternationMatcher top  = (AlternationMatcher)alter;
			for (Matcher sub:top._childMatchers)
			{
				nt.add(sub._grammarComponent);
			}
			return nt.toArray(new String[0]);
		}
		else if (alter instanceof NonTerminalMatcher)
			return new String[]{alter.getPatternToMatch()};
		
		return null;
	}
	
	public boolean nonTerminalContainsChild(String parentNonTerminalName, String childNonTerminalName)
	{
		Hashtable<String, Matcher> map = _baseState._parseMap;
		Matcher alter = map.get(parentNonTerminalName);
		if (alter instanceof AlternationMatcher)
		{
			
			AlternationMatcher top  = (AlternationMatcher)alter;
			for (Matcher sub:top._childMatchers)
			{
				if (childNonTerminalName.equals(sub.getPatternToMatch()))
					return true;
			}
			return false;
		}
		return false;
	}
	
	public String getNonterminalDefinition(String nonTerminal)
	{
		if (_baseState._parseMap.containsKey(nonTerminal))
		{
			Matcher m = _baseState._parseMap.get(nonTerminal);
			return m._grammarComponent;
		}
		else
			return null;
	}
	
	public HashMap<String, LinkedList<String>> getNTTagTree(String pattern)
	{
		HashMap<String, LinkedList<String>> map = new HashMap<String, LinkedList<String>>();
		Parser.fillTagTreeMap(_baseState, pattern, new HashMap<String, String>(), map, null, new HashSet<String>());
		return map;
	}
	
	public String[] getConjunctionParts(String nonTerminal)
	{
		if (_baseState._parseMap.containsKey(nonTerminal))
		{
			Matcher m = _baseState._parseMap.get(nonTerminal);
			if (m instanceof ConjunctionMatcher)
			{
				ConjunctionMatcher conj = (ConjunctionMatcher)m;
				String[] parts = new String[conj._childMatchers.length];
				for (int i=0;i<parts.length;i++)
					parts[i] = conj._childMatchers[i]._grammarComponent;
				return parts;
			}
			
		}
		return null;
	}
	
	public void deleteNonTerminal(String nonTerminal, boolean permanently)
	{
		_baseState._deletedPatterns.add(nonTerminal);
		if (permanently)
			_baseState._parseMap.remove(nonTerminal);
		_nonTerminals = _baseState._parseMap.keySet().toArray(new String[0]);
	}
	
	public void deleteNonTerminal(String nonTerminal)
	{
		deleteNonTerminal(nonTerminal, true);
	}
	
	
	public String[] getNonTerminalNames()
	{
		return _nonTerminals;
	}
	
	public String[] getNonterminalDefinition()
	{
		if (_baseState!=null)
			return _baseState.getNonterminalDefinition();
		else
			return new String[0];
	}
	
	public Hashtable<String, Integer> getWeightMap()
	{
		return _weightDistribution;
	}
	
	
	
	
	private void init(String[] grammarDefinition,  boolean stringAsCharP)
	{
		_baseState = Parser.buildGlobalState(grammarDefinition, stringAsCharP);
		_nonTerminals = _baseState.getNonterminals();

	}
	
	// Using custom Environments is deprecated in conjunction with CustomTerminalMatchers
	public void reInit(String[] grammarDefinition, boolean stringAsCharP)
	{
		GlobalState prior = _baseState;
		_baseState = Parser.buildGlobalState(prior._predicateFunctions, prior._customMatchers, grammarDefinition, stringAsCharP);
		
		_nonTerminals = _baseState.getNonterminals();
		
	}
	
	public MatcherController getIncrementalParser(String grammar)
	{
		GlobalState state = _baseState.clone();
		Matcher root = Parser.parse(state, grammar, null);
		
		MatcherController controller = new MatcherController(root, state);
		
		return controller;
	}
	
	public MatcherController getIncrementalParser(String grammar, String[] captureNames, boolean robotMode)
	{
		GlobalState state = _baseState.clone();
		if (captureNames!=null)
		{
			state.captureNames = new HashSet<String>();
			for (String n:captureNames)
				state.captureNames.add(n);
		}
		
		Matcher root = Parser.parse(state, grammar, null);
		
		MatcherController controller = new MatcherController(root, state);
		
		return controller;
	}
	
	/**
	 * Uniformly selects a random element of generic type V from a list of values
	 * 
	 * @param itemList
	 */
	public static <V> V ChooseRandom(List<V> itemList)
	{
		if ((itemList==null)||(itemList.size()<1))
			return null;
		else
		{
			int maxIndex=itemList.size();
			int chosenIndex=(int)Math.min(Math.random()*maxIndex, maxIndex-1);
			return itemList.get(chosenIndex);
		}
	}
}
	
	
