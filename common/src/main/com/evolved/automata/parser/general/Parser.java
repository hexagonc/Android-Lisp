package com.evolved.automata.parser.general;
import java.io.IOException;
import java.util.*;

import com.evolved.automata.KeyValuePair;

class Parser 
{
	
	
	public static boolean debug=true;
	
	public static final String wildcard = "~";
	public static final String whitespace = "$";
	public static final String numeric = "#";
	public static final String letter = "@";
	public static final String endString="^";
	public static final String commentChar="#";
	public static final String lineTerminator=";";
	public static final String tagOverrideSeparator = "->";
	public static final String ntTagSeparator = ":";
	public static final String ntTagPairSeparator = ",";
	
	public static final String terminalConjunction = "&";
	public static final String matchOne="?";
	public static final String kleene="*";
	public static final String onePlus="+";
	public static final String negation = "`";
	public static final String supercede = ">>";
	public static final String replacement = "<<";
	
	public static final String nonterminalDefinitionSeparator = "\\=\\=";
	
	public static final String stringCharLiteralStart = "[";
	public static final String stringCharLiteralEnd = "]";
	
	public static final int t_TERMINAL=0;
	public static final int t_NONTERMINAL=1;
	public static final int t_GROUP=2;
	public static final int t_CONJUNCTION=3;
	public static final int t_DISJUNCTION=4;
	public static final int t_KLEENE=5;
	public static final int t_WILDCARD=6;
	public static final int t_QUANTIFIER=7;
	
	public static final int DEFAULT_WEIGHT = 1;
	
	public static PatternParser.GlobalState buildGlobalState( LinkedList<String> definitionComponent, boolean stringsAsCharacter)
	{
		
		return buildGlobalState( definitionComponent.toArray(new String[0]), stringsAsCharacter);
		
	}
	
	public static PatternParser.GlobalState buildGlobalState(String inputFileFullName, boolean stringsAsCharacter) throws IOException
	{
		String[] definitionComponent = com.evolved.automata.filetools.StandardTools.getDataFileLines(inputFileFullName);
		return buildGlobalState( definitionComponent, stringsAsCharacter);
		
	}
	
	public static PatternParser.GlobalState buildGlobalState(HashSet<String> functions, HashMap<String, CustomTerminalMatcher> customMap,  String[] definitionComponent, boolean stringsAsCharacter) 
	{
		String[] defParts;
		Hashtable<String, Matcher> parseMap = new Hashtable<String, Matcher>();
		
		StringBuilder sBuilder = null;
		
		String nonTerminalName = null;
		String nonTerminalDef;
		Hashtable<String, String> nonterminalMap = new Hashtable<String, String>();
		String lineDefinition;
		
		
		
		NonTerminalMatcher placeholderMatcher = new NonTerminalMatcher(nonTerminalName, null);
		
		for (String definition:definitionComponent)
		{
			definition = definition.trim();
			if (definition.length()>0)
			{
				if (!definition.substring(0, 1).equals(Parser.commentChar))
				{
					defParts = definition.split(nonterminalDefinitionSeparator);
					if (defParts.length>1)
					{
						
						nonTerminalName = defParts[0].trim();
						
						lineDefinition = defParts[1].trim();
						sBuilder = new StringBuilder();
					}
					else
						lineDefinition = definition.trim();
					
					if (lineDefinition.endsWith(lineTerminator))
					{
						sBuilder.append(lineDefinition.substring(0, lineDefinition.length()-1));
						nonterminalMap.put(nonTerminalName, sBuilder.toString());
						// Have to define a placeholder matcher for this non-terminal 
						// so that you can parse a non-terminal before all dependent
						// NonTerminals are defined
						parseMap.put(nonTerminalName, placeholderMatcher);
					}
					else
						sBuilder.append(lineDefinition);
					
					
				}
			}
		}
		PatternParser.GlobalState global;
		global = new PatternParser.GlobalState(functions, parseMap, stringsAsCharacter);
		global.setNonterminalDefinition(nonterminalMap);
		global._customMatchers = customMap;
		Matcher rootMatcher;
		if (nonterminalMap.size()>0)
		{
			
			for (String name:nonterminalMap.keySet())
			{
				nonTerminalDef = nonterminalMap.get(name);
				rootMatcher = parse(global, nonTerminalDef, null);
				parseMap.put(name, rootMatcher);
			}
		}
		
		return global;
	}
	
	public static PatternParser.GlobalState buildGlobalState(String[] definitionComponent, boolean stringsAsCharacter) 
	{
		
		
		return buildGlobalState(new HashSet<String>(), new HashMap<String, CustomTerminalMatcher>(), definitionComponent, stringsAsCharacter);
	}
	
	
	/**
	 * Recursively builds a parse tree from a pattern string. This is the main method.  
	 * 
	 * @param global This represents global state for all pattern matchers. 
	 * @param component This is the the pattern string the parse tree is being built from
	 * @return root of parse tree for component.  Throws exception if grammar is inconsistent<br/>
	 * pattern syntax.
	 */

	
	/**
	 * Recursively builds a parse tree from a pattern string. This is the main method.  
	 * 
	 * @param global This represents global state for all pattern matchers. 
	 * @param component This is the the pattern string the parse tree is being built from
	 * @return root of parse tree for component.  Throws exception if grammar is inconsistent<br/>
	 * pattern syntax.
	 */
	public static Matcher parse(PatternParser.GlobalState global,  String component, HashMap<String, String> ntTagOverrides)
	{
		
		component = component.trim();
		
		String mappedGrammar = isNonterminal(global, component);
		
		if (mappedGrammar!=null)
		{	
			NonTerminalMatcher nt = new NonTerminalMatcher(mappedGrammar, global);
			String comp = nt.getPatternToMatch();
			String replace;
			if (ntTagOverrides!=null && (replace = ntTagOverrides.get(comp))!=null)
				nt.setTag(replace);
			nt.setChildTagOverrides(ntTagOverrides);
			return nt;
		}
		
		String[] terminalSymbol = isLambdaTerminal(global, component);
		if (terminalSymbol!=null)
		{
			TerminalMatcher tm = new TerminalMatcher(component, terminalSymbol);
			if (global.bottomMatchersEnabledP())
			{
				global.addToBottomMatchers(tm);
			}
			return tm;
		}
		
		String grammar = isTerminal(component);
		if (grammar!=null)
		{
			return new TerminalMatcher(component);
		}
		
		String[] parts;
		Matcher[] subStates;
		
		parts = disjuctionofLiterals(component);
		if (parts!=null && parts.length == 1)
		{
			subStates = new Matcher[parts[0].length()];
			
			for (int i = 0;i<parts[0].length(); i++)
			{
				subStates[i] = new TerminalMatcher(parts[0].substring(i,i+1));
			}
			return new ConjunctionMatcher(component, subStates);
		}
		
		if (isStringAsTerminalP(component))
		{
			return new TerminalMatcher(component);
		}
		
		mappedGrammar = isGroup(component);
		if (mappedGrammar!=null)
		{
			return Parser.parse(global, mappedGrammar, ntTagOverrides);
		}
		
		
		
		KeyValuePair<HashMap<String, String>, String> compPair = isTagOverride(component, global);
		
		if (compPair!=null)
		{
			HashMap<String, String> bmap = compPair.GetKey();
			if (ntTagOverrides!=null)
			{
				for (String key:ntTagOverrides.keySet())
				{
					bmap.put(key, ntTagOverrides.get(key));
				}
			}
			
			return parse(global, compPair.GetValue(), bmap);
		}
		
		parts = isSupercede(component);
		if (parts!=null)
		{
			subStates = new Matcher[2];
			subStates[0] = Parser.parse(global, parts[0], ntTagOverrides);
			subStates[1] = Parser.parse(global, parts[1], ntTagOverrides);
			return new SupercedeMatcher(component, subStates[0], subStates[1]);
		}
		
		parts = isReplacement(component);
		if (parts!=null)
		{
			subStates = new Matcher[2];
			subStates[0] = Parser.parse(global, parts[0], ntTagOverrides);
			subStates[1] = Parser.parse(global, parts[1], ntTagOverrides);
			return new ReplaceMatcher(component, subStates[0], subStates[1]);
		}
		
		
		parts = isConjunction(component);
		if (parts!=null)
		{
			subStates = new Matcher[parts.length];
			for (int i=0;i<subStates.length;i++)
				subStates[i]=Parser.parse(global, parts[i], ntTagOverrides);
			
			return new ConjunctionMatcher(component, subStates);
		}
		
		parts = isAlternation(component.trim());
		if (parts!=null)
		{
			subStates = new Matcher[parts.length];
			for (int i=0;i<subStates.length;i++)
				subStates[i]=Parser.parse(global, parts[i], ntTagOverrides);
			
			return new AlternationMatcher(component, subStates);
		}
		QuantifierInfo qInfo;
		qInfo = isQuantifier(component);
		if (qInfo!=null)
		{
			
			return new QuantifierMatcher(component,qInfo.miniMatches, qInfo.maxMatches, parse(global, qInfo.grammar, ntTagOverrides));
			
			
		}
		
		
		parts = splitPattern(component, ntTagSeparator, false);
		if (parts!=null && parts.length == 2)
		{
			Matcher comp = Parser.parse(global, parts[0], ntTagOverrides);
			comp.setTag(parts[1]);
			return comp;
		}
		throw new RuntimeException("Undefined grammar: "+ component);
	}
	
	
	

	/**
	 * Recursively extracts all terminal names from the grammar name
	 */
	public static void fillTerminalMap(PatternParser.GlobalState global, HashSet<String> terminalMap, String component)
	{
		
		component = component.trim();
		
		String mappedGrammar = isNonterminal(global, component);
		
		if (mappedGrammar!=null)
		{	
			return;
		}
		
		String[] terminalSymbol = isLambdaTerminal(global, component);
		if (terminalSymbol!=null)
		{
			return;
		}
		
		String grammar = isTerminal(component);
		if (grammar!=null)
		{
			terminalMap.add(grammar);
			return;
		}
		
		String[] parts;
		Matcher[] subStates;
		
		parts = disjuctionofLiterals(component);
		if (parts!=null && parts.length == 1)
		{
			terminalMap.add(parts[0]);
			return;
		}
		
		if (isStringAsTerminalP(component))
		{
			terminalMap.add(component.substring(1, component.length()-1));
			return;
		}
		
		mappedGrammar = isGroup(component);
		if (mappedGrammar!=null)
		{
			
			fillTerminalMap(global, terminalMap, mappedGrammar);	
			return;
		}
		
		KeyValuePair<HashMap<String, String>, String> compPair = isTagOverride(component, global);
		
		if (compPair!=null)
		{
			
			fillTerminalMap(global, terminalMap, compPair.GetValue());
		}
		
		
		parts = isConjunction(component);
		if (parts!=null)
		{
			subStates = new Matcher[parts.length];
			for (int i=0;i<subStates.length;i++)
				fillTerminalMap(global, terminalMap, parts[i]);
				
			return;
		}
		
		
		
		
			
		parts = isAlternation(component.trim());
		if (parts!=null)
		{
			
			for (int i=0;i<parts.length;i++)
				fillTerminalMap(global, terminalMap, parts[i]);
			
			return;
		}
		QuantifierInfo qInfo;
		qInfo = isQuantifier(component);
		if (qInfo!=null)
		{
			fillTerminalMap(global, terminalMap, qInfo.grammar);
			return;
		}
		
		
	}
	
	/**
	 * Recursively extracts all tag names from the grammar name
	 */
	public static void fillTagTreeMap(PatternParser.GlobalState global, String component, HashMap<String, String> ntTagOverrides, HashMap<String, LinkedList<String>> tagMap, String parentTag, HashSet<String> ntUpline)
	{
		
		component = component.trim();
		
		String[] mappedGrammar = checkTaggedNonterminal(global, component);
		
		LinkedList<String> children = null;
		
		if (mappedGrammar!=null)
		{	
			if (ntUpline.contains(mappedGrammar[0]))
				return;
			
			HashSet<String> newMap = new HashSet<String>();
			newMap.add(mappedGrammar[0]);
			newMap.addAll(ntUpline);
			
			String tag = mappedGrammar[1];
			
			if (ntTagOverrides!=null && ntTagOverrides.containsKey(mappedGrammar[0]) )
			{
				tag = ntTagOverrides.get(mappedGrammar[0]);
			}
			
			if (tag!=null)
			{
				if (parentTag!=null)
				{
					children = tagMap.get(parentTag);
					
					if(children == null)
					{
						children = new LinkedList<String>();
						tagMap.put(tag, children);
					}
					
					children.add(tag);
				}
				tagMap.put(tag, new LinkedList<String>());
				fillTagTreeMap(global, global.getNonterminalGrammarDefinition(mappedGrammar[0]), ntTagOverrides, tagMap, tag, newMap);
			}
			else
				fillTagTreeMap(global, global.getNonterminalGrammarDefinition(mappedGrammar[0]), ntTagOverrides, tagMap, parentTag, newMap);
			return;
		}
		
		String[] terminalSymbol = isLambdaTerminal(global, component);
		if (terminalSymbol!=null)
		{
			return;
		}
		
		String grammar = isTerminal(component);
		if (grammar!=null)
		{
			return;
		}
		
		String[] parts;
		Matcher[] subStates;
		
		parts = disjuctionofLiterals(component);
		if (parts!=null && parts.length == 1)
		{
			
			return;
		}
		
		if (isStringAsTerminalP(component))
		{
			
			return;
		}
		
		String groupedGrammar = isGroup(component);
		if (groupedGrammar!=null)
		{
			fillTagTreeMap(global, groupedGrammar, ntTagOverrides, tagMap, parentTag, ntUpline);
			return;
		}
		
		KeyValuePair<HashMap<String, String>, String> compPair = isTagOverride(component, global);
		
		if (compPair!=null)
		{
			fillTagTreeMap(global, compPair.GetValue(), ntTagOverrides, tagMap, parentTag, ntUpline);
			
		}
		
		
		parts = isConjunction(component);
		if (parts!=null)
		{
			subStates = new Matcher[parts.length];
			for (int i=0;i<subStates.length;i++)
				fillTagTreeMap(global, parts[i], ntTagOverrides, tagMap, parentTag, ntUpline);
				
				
			return;
		}
		
		parts = isAlternation(component.trim());
		if (parts!=null)
		{
			
			for (int i=0;i<parts.length;i++)
				fillTagTreeMap(global, parts[i], ntTagOverrides, tagMap, parentTag, ntUpline);
			
			return;
		}
		QuantifierInfo qInfo;
		qInfo = isQuantifier(component);
		if (qInfo!=null)
		{
			fillTagTreeMap(global, qInfo.grammar, ntTagOverrides, tagMap, parentTag, ntUpline);
			
			return;
		}
		
		
	}
	
	
	/**
	 * Determines if an EBNF-like pattern string can be parsed into a conjunction of subpatterns
	 * @param groupString
	 * @return
	 */
	public static String[] segmentGroup(String groupString)
	{
		return splitPatternByCommas(groupString);
	}
	
	
	
	public static KeyValuePair<HashMap<String, String>, String> isTagOverride(String grammar, PatternParser.GlobalState state)
	{
		int delimiterPos = grammar.indexOf(tagOverrideSeparator);
		if (delimiterPos <3)
			return null;
		
		String remaining = grammar.substring(delimiterPos + tagOverrideSeparator.length());
		HashMap<String, String> map = new HashMap<String, String>();
		boolean suc = parseNTTagList(map, grammar.substring(0, delimiterPos), state);
		if (suc && remaining.trim().length()>0 )
			return new KeyValuePair<HashMap<String, String>, String>(map,remaining );
		return null;
	}
	
	public static boolean parseNTTagList(HashMap<String, String> output, String input, PatternParser.GlobalState state)
	{
		int tagSeparator = input.indexOf(ntTagSeparator);
		int pairSeparator = input.indexOf(ntTagPairSeparator);
		boolean cont = pairSeparator>2;
		boolean valid = true;
		while (cont && valid)
		{
			valid = parseNTTagList(output, input.substring(0, pairSeparator), state);
			input = input.substring(pairSeparator+1);
			pairSeparator = input.indexOf(ntTagPairSeparator);
			cont = pairSeparator>2;
			tagSeparator = input.indexOf(ntTagSeparator);
		}
		
		if (valid && tagSeparator>0)
		{
			String nt = input.substring(0, tagSeparator).trim();
			
			if (state._parseMap.containsKey(nt))
			{
				String tag = input.substring(tagSeparator+1).trim();
				output.put(nt, tag);
				return true;
			}
		}
		return false;
	}
	
	public static String[] isLambdaTerminal(PatternParser.GlobalState state, String inString)
	{
		if (inString==null || state._predicateFunctions==null)
			return null;
		HashSet<String> functions = state._predicateFunctions;
		String[] terms = inString.split("\\" + terminalConjunction);
		if (terms==null)
			return null;
		String pred;
		
		for (int i=0;i<terms.length;i++)
		{
			pred = terms[i].trim();
			if (pred.endsWith(negation)&&functions.contains(pred.substring(0, pred.length()-1)))
				terms[i]=pred;
			else if (functions.contains(pred))
				terms[i]=pred;
			
			else
				return null;
		}
		return terms;
		
	}
	
	public static String[] isSupercede(String component)
	{
		String[] split = splitPattern(component, supercede, true);
		if (split != null && split.length>1)
		{
			return new String[]{split[0], join(split, supercede, 1)};
		}
		else
			return null;
	}
	
	public static String[] isReplacement(String component)
	{
		String[] split = splitPattern(component, replacement, true);
		if (split != null && split.length>1)
		{
			return new String[]{split[0], join(split, replacement, 1)};
		}
		else
			return null;
	}
	
	public static String join(String[] parts, String com)
	{
		StringBuilder b = new StringBuilder(parts[0]);
		for (int i=1;i<parts.length;i++ )
			b.append(com).append(parts[i]);
		return b.toString();
	}
	
	public static String join(String[] parts, String com, int offset)
	{
		StringBuilder b = new StringBuilder(parts[offset]);
		for (int i=offset + 1;i<parts.length;i++ )
			b.append(com).append(parts[i]);
		return b.toString();
	}
	
	public static boolean isStringAsTerminalP(String string)
	{
		 
		return string.lastIndexOf(stringCharLiteralStart) == 0 && string.indexOf(stringCharLiteralEnd) == string.length()-1;
	}
	
	/**
	 * Determines whether a string representing an EBNF-like pattern can be interpreted <br/>
	 * as a terminal symbol
	 * @param inString
	 * @return returns the raw character represented by the terminal or null if <br/>
	 * the pattern can not be considered a terminal symbol
	 */
	public static String isTerminal(String inString)
	{
		if (inString==null)
			return null;
		
		boolean len3 = inString.length() == 3;
		boolean len4 = inString.length() == 4;
		boolean firstCharDelimiter = inString.length()>0 && "'".equals(inString.substring(0, 1));
		boolean thirdCharDelimiter = inString.length()>2 && "'".equals(inString.substring(2, 3));
		boolean fourthCharDelimiter = inString.length()>3 && "'".equals(inString.substring(3, 4));
		boolean secondCharEscape = inString.length()>1 && "\\".equals(inString.substring(1, 2));
		
		if (len3 && firstCharDelimiter && thirdCharDelimiter)
			return inString.substring(1,2);
		if (len4 && firstCharDelimiter && secondCharEscape && fourthCharDelimiter)
			return inString.substring(1,3);
		return null;
		
	}
	
	public static String isNonterminal(PatternParser.GlobalState state, String inString)
	{
		if (state._parseMap==null)
			return null;
		String[] s = partitionTag(inString);
		if (s[0]!=null && state._parseMap.containsKey(s[0]))
			return inString.trim();
		return null;
	}
	
	public static String[] checkTaggedNonterminal(PatternParser.GlobalState state, String inString)
	{
		if (state._parseMap==null)
			return null;
		String[] s = partitionTag(inString);
		if (s[0]!=null && state._parseMap.containsKey(s[0]))
			return s;
		return null;
	}
	
	public static String[] partitionTag(String variable)
	{
		if (variable == null)
			return new String[]{null, null};
		int colonPos = variable.indexOf(':');
		if (colonPos>0 && colonPos< variable.length()-1)
		{
			StringBuilder tag = new StringBuilder();
			char[] chars = variable.toCharArray();
			for (int i=colonPos+1;i<chars.length;i++)
			{
				if (Character.isLetterOrDigit(chars[i]) || chars[i] == '-' || chars[i] == '_')
					tag.append(chars[i]);
				else 
					return new String[]{null, null};
			}
			return new String[]{variable.substring(0, colonPos), tag.toString()};
		}
		else
			return new String[]{variable, null};
			
	}
	
	/**
	 * Determines if an EBNF-like pattern string can be interpreted as an alternation of
	 * smaller patterns
	 * @param inString pattern
	 * @return Returns an array of each smaller pattern or null if the other overall pattern <br/>
	 * isn't an alternation.
	 */
	public static  String[] isAlternation(String inString)
	{
		String[] parts = splitPattern(inString, '|', true);
		if ((parts!=null)&&(parts.length>1))
			return parts;
		else
			return null;
	}
	

	/**
	 * Determines whether an EBNF-like pattern can be interpreted as a smaller pattern contained <br/>
	 * in parenthesis
	 * @param inString
	 * @return The parenthesized pattern or null if the string is not a parenthesized pattern
	 */
	public static  String isGroup(String inString)
	{
		char[] values = inString.toCharArray();
		StringBuffer sBuffer = null;
		
		int pcounter = 0;
		for (int i=0;i<values.length;i++)
		{
			if ((i==0)&&(values[i]=='('))
			{
				pcounter++;
				sBuffer = new StringBuffer();
			}
			else
			{
				if (values[i]=='(')
					pcounter++;
				if (values[i]==')')
					pcounter--;
				if (pcounter==0)
				{
					if ((i!=values.length-1)||(i==0))
						return null;
					else
						return sBuffer.toString();
				}
				sBuffer.append(values[i]);
			}
		}
		return null;
	}
	
	/**
	 * Determines whether a string EBNF-like pattern can be parsed into a Alternation of literal <br/>
	 * strings.  Literal strings are delimited by double-quotes and can not contain the pipe <br/>
	 * character itself.  If the string literal contains the pipe character than consider using the <br/>
	 * method convertToTerminalSequencePattern to convert the literal to a raw terminal sequence.  <br/>
	 * Also matches a single string literal.
	 * @param grammarComponent EBNF-like pattern 
	 * @return String array representing each subcomponent of the alternation or null <br/>
	 * if the string can not be interpreted as a top-level alternation.  Does not include <br/>
	 * the double-quote delimiters.
	 */
	public static String[] disjuctionofLiterals(String grammarComponent)
	{
		String[] parts = splitPattern(grammarComponent, '|', true);
		
		if ((parts!=null)&&(parts.length>1))
		{
			for (int i=0;i<parts.length;i++)
			{
				if ((parts[i].charAt(0)=='"')&&(parts[i].charAt(parts[i].length()-1) == '"'))
				{
					parts[i] = parts[i].substring(1,parts[i].length()-1 );
				}
				else
					return null;
			}
			return parts;
		}
		else
		{
			int L = grammarComponent.length();
			if (grammarComponent.startsWith("\"")&&grammarComponent.endsWith("\"")&&grammarComponent.substring(1,L-1).indexOf("\"")==-1)
				return new String[]{grammarComponent.substring(1,grammarComponent.length()-1 )};
			else
				return null;
		}
			
	}
	
	
	/**
	 * Determines whether an EBNF-like pattern can be interpreted as another pattern <br/>
	 * quantified.  
	 * @param inString
	 * @return Returns a QuantifierInfo object containing quantifier properties or null if <br/>
	 * the pattern is not a quantifier
	 */
	public static  QuantifierInfo isQuantifier(String inString)
	{
		int len = inString.length();
		
		if (len>1)
		{
			char currentChar;
			int step; // (/d,/d)
			final int rparen=1, rdigit=2, ldigit=4, quantif=5;
			boolean range=false;
			step=rparen;
			
			int digit=0;
			int counter=0;
			Integer max=null, min=null;
			
			for (int i=len-1;i>=0;i--)
			{
				currentChar = inString.charAt(i);
				if (!range)
				{
					switch (currentChar)
					{
						case '?':
							return new QuantifierInfo(0, 1, "?", inString.substring(0, len - 1));
						case '+':
							return new QuantifierInfo(1, null, "+", inString.substring(0, len - 1));
						case '*':
							return new QuantifierInfo(0, null, "*", inString.substring(0, len - 1));
						case '`':
							return new QuantifierInfo(0, 0, "`", inString.substring(0, len - 1));
						default:
							range=true;
					}
					
					switch (step)
					{
						case rparen:
							if (currentChar == ')')
							{
								step = rdigit;
								counter=0;
							}
							else
								return null;
							break;
						case rdigit:
							if (Character.isDigit(currentChar))
							{
								digit+= convertChar(currentChar)*Math.pow(10, counter++);
							}
							else
							{
								if (currentChar == ',')
								{
									if (counter>0)
										max = new Integer(digit);
									else
										max = null;
									step=ldigit;
									digit=0;
									counter=0;
								}
								else
								{
									if ((currentChar == '(')&&(counter>0))
									{
										max = new Integer(digit);
										min = new Integer(digit);
										
										step=quantif;
									}
									else
										return null;
								}
							}
							
							break;
						case ldigit:
							if (Character.isDigit(currentChar))
							{
								digit+= convertChar(currentChar)*Math.pow(10, counter++);
							}
							else
							{
								
								if ((currentChar == '(')&&(counter>0))
								{
									min = new Integer(digit);
									step=quantif;
								}
								else
									return null;
								
							}
							break;
						case quantif:
							if (currentChar=='*')
								return new QuantifierInfo(min, max, "*", inString.substring(0, i));
							else
								return null;
					}
					
				}
				else
				{
					switch (step)
					{
						case rparen:
							if (currentChar == ')')
							{
								step = rdigit;
								counter=0;
							}
							else
								return null;
							break;
						case rdigit:
							if (Character.isDigit(currentChar))
							{
								digit+= convertChar(currentChar)*Math.pow(10, counter++);
							}
							else
							{
								if (currentChar == ',')
								{
									if (counter>0)
										max = new Integer(digit);
									else
										max = null;
									step=ldigit;
									digit=0;
									counter=0;
								}
								else
								{
									if ((currentChar == '(')&&(counter>0))
									{
										max = new Integer(digit);
										min = new Integer(digit);
										
										step=quantif;
									}
									else
										return null;
								}
							}
							
							break;
						case ldigit:
							if (Character.isDigit(currentChar))
							{
								digit+= convertChar(currentChar)*Math.pow(10, counter++);
							}
							else
							{
								
								if ((currentChar == '(')&&(counter>0))
								{
									min = new Integer(digit);
									step=quantif;
								}
								else
									return null;
								
							}
							break;
						case quantif:
							if (currentChar=='*')
								return new QuantifierInfo(min, max, "*", inString.substring(0, i));
							else
								return null;
					}
				}
				
			}
			
			
		}
		return null;
	}
	
	private static int convertChar(char c)
	{
		switch (c)
		{
			case '0':
				return 0;
			case '1':
				return 1;
			case '2':
				return 2;
			case '3':
				return 3;
			case '4':
				return 4;
			case '5':
				return 5;
			case '6':
				return 6;
			case '7':
				return 7;
			case '8':
				return 8;
			case '9':
				return 9;
		}
		return 0;
	}
	
	/**
	 * Determines whether an EBNF-like pattern can be interpreted as a conjunction of smaller <br/>
	 * sub-patterns.  If sub-patterns are string literals then they can not contain commas.
	 * @param inString pattern string
	 * @return Array of each sub-pattern or null if inString is not a conjunction
	 */
	
	public static  String[] isConjunction(String inString)
	{
		String[] parts = splitPatternByCommas(inString);
		if (parts.length>1)
			return parts;
		else
			return null;
	}
	
	/**
	 * Splits a string representing a regular expression pattern by comma unless that comma <br/>
	 * is quoted in single quotes
	 * @param tokenizedString The pattern string
	 * @return Returns a string array of each trimmed component delimited by commas
	 */
	public static String[] splitPatternByCommas(String tokenizedString)
	{
		return splitPattern(tokenizedString,',',true);
	}
	
	/**
	 * Splits a string representing a regular expression pattern on a character unless that character <br/>
	 * is quoted in single quotes
	 * @param tokenizedString The pattern string
	 * @param slitChar character to split on
	 * @param trim if true, each component delimited by the splitChar is trimmed
	 * @return Returns a string array of each component delimited by the splitChar
	 */
	public static String[] splitPattern(String tokenizedString, char slitChar, boolean trim)
	{
		if (tokenizedString==null)
			return null;
		
		LinkedList<String> splitString = new LinkedList<String>();
		final int IN_PARENTHESIS=0;
		final int OUT_PARENTHESIS=1;
		final int IN_STRING=2;
		final int OUT_STRING=3;
		
		int outState=OUT_STRING;
		int level=0;
		int state = OUT_PARENTHESIS;
		char[] chars = tokenizedString.toCharArray();
		StringBuilder segment= new StringBuilder();
		
		int totalChars=chars.length;
		try
		{
			for (int i=0;i<totalChars;i++)
			{
				
				switch (state)
				{
					case IN_PARENTHESIS:
						if (chars[i] == ')')
						{
							if (level == 1)
								state = OUT_PARENTHESIS;
							else
								level--;
						}
						else
						{
							if (chars[i]=='(') 
								level++;
						}
						segment.append(chars[i]);
						break;
					case OUT_PARENTHESIS:
						if (chars[i] == '"')
						{
							state=IN_STRING;
							segment.append(chars[i]);
						}
						else
						{
							if (chars[i] == slitChar)
							{
								if (trim)
									splitString.add(segment.toString().trim());
								else
									splitString.add(segment.toString());
								segment = new StringBuilder();
							}
							else
							{
								if (chars[i] == '(')
								{
									state = IN_PARENTHESIS;
									level = 1;
								}
								segment.append(chars[i]);
							}
						}
						break;
					case IN_STRING:
						if (chars[i] == '"') 
						{
							state=OUT_PARENTHESIS;
						}
						segment.append(chars[i]);
						break;
					
						
				}
				
			}
		}
		catch (Exception e)
		{
			System.out.println(tokenizedString);
		}
		
		if (segment.length()>0)
		{
			if (trim)
				splitString.add(segment.toString().trim());
			else
				splitString.add(segment.toString());
		}
		return splitString.toArray(new String[0]);
		
	}
	
	/**
	 * Splits a string representing a regular expression pattern on a character unless that character <br/>
	 * is quoted in single quotes
	 * @param tokenizedString The pattern string
	 * @param slitChar character to split on
	 * @param trim if true, each component delimited by the splitChar is trimmed
	 * @return Returns a string array of each component delimited by the splitChar
	 */
	public static String[] splitPattern(String tokenizedString, String splitChar, boolean trim)
	{
		if (tokenizedString==null)
			return null;
		
		LinkedList<String> splitString = new LinkedList<String>();
		final int IN_PARENTHESIS=0;
		final int OUT_PARENTHESIS=1;
		final int IN_STRING=2;
		final int OUT_STRING=3;
		
		int outState=OUT_STRING;
		int level=0;
		int state = OUT_PARENTHESIS;
		char[] chars = tokenizedString.toCharArray();
		StringBuilder segment= new StringBuilder();
		
		int totalChars=chars.length;
		try
		{
			for (int i=0;i<totalChars;i++)
			{
				
				switch (state)
				{
					case IN_PARENTHESIS:
						if (chars[i] == ')')
						{
							if (level == 1)
								state = OUT_PARENTHESIS;
							else
								level--;
						}
						else
						{
							if (chars[i]=='(') 
								level++;
						}
						segment.append(chars[i]);
						break;
					case OUT_PARENTHESIS:
						if (chars[i] == '"')
						{
							state=IN_STRING;
							segment.append(chars[i]);
						}
						else
						{
							if (tokenizedString.substring(i, Math.min(i + splitChar.length(), tokenizedString.length())).equals(splitChar))
							{
								if (trim)
									splitString.add(segment.toString().trim());
								else
									splitString.add(segment.toString());
								segment = new StringBuilder();
								i+=splitChar.length()-1;
							}
							else
							{
								if (chars[i] == '(')
								{
									state = IN_PARENTHESIS;
									level = 1;
								}
								segment.append(chars[i]);
							}
						}
						break;
					case IN_STRING:
						if (chars[i] == '"') 
						{
							state=OUT_PARENTHESIS;
						}
						segment.append(chars[i]);
						break;
					
						
				}
				
			}
		}
		catch (Exception e)
		{
			System.out.println(tokenizedString);
		}
		
		if (segment.length()>0)
		{
			if (trim)
				splitString.add(segment.toString().trim());
			else
				splitString.add(segment.toString());
		}
		return splitString.toArray(new String[0]);
		
	}
	

	public static class QuantifierInfo
	{
		public Integer miniMatches;
		public Integer maxMatches;
		public String quantifier;
		public String grammar;
		
		public QuantifierInfo(Integer miniMatches, Integer maxMatches, String quantifier, String grammar)
		{
			this.miniMatches=miniMatches;
			this.maxMatches=maxMatches;
			this.quantifier=quantifier;
			this.grammar=grammar;
			
		}
		
	}
	
	
}
