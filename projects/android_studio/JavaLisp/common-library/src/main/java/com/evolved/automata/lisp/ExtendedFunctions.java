package com.evolved.automata.lisp;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;

import java.net.URL;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.Enumeration;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.NavigableSet;
import java.util.TreeSet;


import com.evolved.automata.AITools;
import com.evolved.automata.KeyValuePair;
import com.evolved.automata.PatternMap;

import com.evolved.automata.parser.CFGParser;
import com.evolved.automata.parser.general.CustomTerminalMatcher;
import com.evolved.automata.parser.general.GeneralizedCharacter;
import com.evolved.automata.parser.general.PatternParser;
import com.evolved.automata.parser.general.Sequence;
import com.evolved.automata.parser.general.TerminalMatcher;
import com.evolved.automata.parser.general.TextCharacter;
import com.evolved.automata.parser.math.Expression;
import com.evolved.automata.parser.math.ExpressionFactory;
import com.evolved.automata.parser.math.WordNumberExpressionPreProcessor;


public class ExtendedFunctions 
{
	public static Value _number_grammar;
	static CFGParser parser = null;
	private static MessageDigest _sha1digest = null;
	private static MessageDigest _md5digest = null;
	static Object _globalSynch = new Object();

	static Calendar _globalCalendar = Calendar.getInstance();
	
	static
	{
		InputStreamReader reader = null;
		BufferedReader breader =  null;
		try
		{

            ClassLoader loader = ExtendedFunctions.class.getClassLoader();
            // Note that resource name can't start with / when calling ClassLoader.getResourceAsStream(..) as
            // opposed to calling Class.getResourceAsStream(..)
			InputStream istream = loader.getResourceAsStream("com/evolved/automata/parser/math/basic_number_general_grammar.txt");
			
			reader = new InputStreamReader(istream);
			breader =  new BufferedReader(reader);
			String lineinput;
			LinkedList<String> list = new LinkedList<String>();
			
			while ((lineinput=breader.readLine())!=null)
			{
				if (lineinput.trim().length()>0)
					list.add(lineinput.trim());
			}
			
			Value[] v = new Value[list.size()];
			int i=0;
			for (String data:list)
			{
				v[i++] = NLispTools.makeValue(data);
			}
			_number_grammar = NLispTools.makeValue(v);
		}
		catch (IOException ie)
		{
			_number_grammar = null;
		}
		finally
		{
			if (null!=breader)
			{
				try
				{
					breader.close();
				}
				catch (Exception e)
				{
					
				}
			}
		}
		
		try
		{
			BufferedReader greader = new BufferedReader(new InputStreamReader( ExtendedFunctions.class.getResourceAsStream("/com/evolved/automata/parser/math/number_pattern.txt")));

			parser = new CFGParser(greader);
			
		}
		catch (IOException ie)
		{
			
		}
		
		try
		{
			_sha1digest = MessageDigest.getInstance("SHA-1");
			_md5digest = MessageDigest.getInstance("MD5");
		}
		catch (NoSuchAlgorithmException nsa){
			nsa.printStackTrace();
		}
		
	}
	
	
	public static void addExtendedFunctions(Environment env)
	{
		env.mapFunction("serialize", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				Lambda.resetRecursionLimit();
				if (evaluatedArgs.length>1 && !evaluatedArgs[1].isNull() && evaluatedArgs[0].isLambda())
				{
					return NLispTools.makeValue(((LambdaValue)evaluatedArgs[0]).serializedForm(true));
				}
				else
					return NLispTools.makeValue(evaluatedArgs[0].serializedForm());
				
			}
			
		}
		);


		
		env.mapFunction("vectorToValue-distribution", new SimpleFunctionTemplate()
		{

			/**
			 * First argument is a hashtable mapping a string or integer to a non-negative numeric weight
			 * or a key-value pair mapping any object to a numeric
			   Optional second argument is a boolean flag indicating whether to return null if
			   all weights are zero.  Otherwise all samples will be equally likely
			   Returns a random key in proportion to its weight
			 */
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				
				int numItems = 0, i = 0;
				double totalWeight= 0;
				double sample;
				double weight;
				boolean returnNullOnAllZeroWeightP = false;
				Value uniformSample = Environment.getNull();
				int uniformSampleIndex = 0;
				if (evaluatedArgs.length > 1)
				{
					returnNullOnAllZeroWeightP = !evaluatedArgs[1].isNull();
				}
				
				
				if (evaluatedArgs[0].isStringHashtable())
				{
					String lastString = null;
					HashMap<String, Value> map = evaluatedArgs[0].getStringHashtable();
					numItems = map.size();
					uniformSampleIndex = (int)(Math.random()*numItems);
					i = 0;
					for (Map.Entry<String, Value> entry:map.entrySet())
					{
						if (i == uniformSampleIndex)
							uniformSample = NLispTools.makeValue(entry.getKey());
						totalWeight+=entry.getValue().getFloatValue();
						i++;
					}

					if (totalWeight == 0)
					{
						if (returnNullOnAllZeroWeightP)
							return Environment.getNull();
						else
							return uniformSample;
					}

					sample = totalWeight*Math.random();
					totalWeight = 0;
					
					for (Map.Entry<String, Value> entry:map.entrySet())
					{
						lastString = entry.getKey();
						weight = entry.getValue().getFloatValue();
						if (totalWeight + weight>=sample)
							break;
						totalWeight+=weight;
					}
					return NLispTools.makeValue((String)lastString);
				}else if (evaluatedArgs[0].isList())
				{
					Value lastValue = Environment.getNull();
					numItems = evaluatedArgs[0].getList().length;
					uniformSampleIndex = (int)(Math.random()*numItems);
					i = 0;

					for (Value pair:evaluatedArgs[0].getList())
					{
						if (i == uniformSampleIndex)
							uniformSample = pair.getList()[0];

						if (pair.isList()&&pair.getList().length == 2)
						{
							totalWeight+=pair.getList()[1].getFloatValue();
							
						}
						else 
							throw new RuntimeException("Second argument to vectorToValue-distribution must be a hashtable or a key-value list");
						i++;
					}

					if (totalWeight == 0)
					{
						if (returnNullOnAllZeroWeightP)
							return Environment.getNull();
						else
							return uniformSample;
					}

					sample = totalWeight*Math.random();
					totalWeight = 0;
					
					for (Value pair:evaluatedArgs[0].getList())
					{
						if (pair.isList()&&pair.getList().length == 2)
						{
							weight = pair.getList()[1].getFloatValue();
							lastValue = pair.getList()[0];
							
							if (totalWeight + weight>=sample)
								break;
							totalWeight+=weight;
						}
						else 
							throw new RuntimeException("Second argument to vectorToValue-distribution must be a hashtable or a key-value list");
					}
					return lastValue;
					
				} else if (evaluatedArgs[0].isIntHashtable())
				{
					long lastKey = 0;
					HashMap<Long, Value> map = evaluatedArgs[0].getIntHashtable();
					numItems = map.size();
					uniformSampleIndex = (int)(Math.random()*numItems);
					i = 0;
					for (Map.Entry<Long, Value> entry:map.entrySet())
					{
						if (i == uniformSampleIndex)
							uniformSample = NLispTools.makeValue(entry.getKey());
						totalWeight+=entry.getValue().getFloatValue();
						i++;
					}

					if (totalWeight == 0)
					{
						if (returnNullOnAllZeroWeightP)
							return Environment.getNull();
						else
							return uniformSample;
					}

					sample = totalWeight*Math.random();
					totalWeight = 0;
					
					for (Map.Entry<Long, Value> entry:map.entrySet())
					{
						lastKey = entry.getKey();
						weight = entry.getValue().getFloatValue();
						if (totalWeight + weight>=sample)
							break;
						totalWeight+=weight;
					}
					return NLispTools.makeValue(lastKey);
				}
				else 
					throw new RuntimeException("Second argument to add-structure must be a hashtable or a key-value list");
				
			}
			
		}
		);
		
		env.mapFunction("make-pattern-map", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				
				return makeValue(new PatternMap());
			}
			
		});
		
		env.mapFunction("pmap-get", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, true, true);
				PatternMap map = (PatternMap)evaluatedArgs[0].getObjectValue();
				LinkedList<String> tokens = new LinkedList<String>();
				if (evaluatedArgs[1].isList())
				{
					for (Value v:evaluatedArgs[1].getList())
					{
						if (v.isString())
							tokens.add(v.getString());
						else
							tokens.add(v.toString());
					}
				}
				else
					throw new RuntimeException("Second argument to pmap-get must be a list of objects");
				int maxResults = 5;
				if (evaluatedArgs.length>2)
					maxResults = (int)evaluatedArgs[2].getIntValue();
				LinkedList<KeyValuePair<Object, Double>> result = map.get(tokens, maxResults);
				Value[] out = new Value[result.size()];
				int i = 0;
				for (KeyValuePair<Object, Double> match:result)
				{
					out[i++] = NLispTools.makeValue(new Value[]{(Value)(match.GetKey()), NLispTools.makeValue(match.GetValue().doubleValue())});
				}
				return NLispTools.makeValue(out);
			}
			
		});
		
		env.mapFunction("pmap-add", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(3, true, true);
				PatternMap map = (PatternMap)evaluatedArgs[0].getObjectValue();
				LinkedList<String> tokens = new LinkedList<String>();
				if (evaluatedArgs[1].isList())
				{
					for (Value v:evaluatedArgs[1].getList())
					{
						if (v.isString())
							tokens.add(v.getString());
						else
							tokens.add(v.toString());
					}
				}
				else
					throw new RuntimeException("Second argument to pmap-add must be a list of objects");
				
				boolean allowReplacement = (evaluatedArgs.length == 3 || (evaluatedArgs.length == 4 && evaluatedArgs[3].isNull()));
				
				if (map.put(tokens, evaluatedArgs[2], allowReplacement))
					return makeValue(map);
				else
					return NLispTools.makeValue(false);
			}
			
		});
		
		env.mapFunction("pmap-remove-all", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, true, false);
				PatternMap map = (PatternMap)evaluatedArgs[0].getObjectValue();
				LinkedList<String> tokens = new LinkedList<String>();
				if (evaluatedArgs[1].isList())
				{
					for (Value v:evaluatedArgs[1].getList())
					{
						if (v.isString())
							tokens.add(v.getString());
						else
							tokens.add(v.toString());
					}
				}
				else
					throw new RuntimeException("Second argument to pmap-get must be a list of objects");
				double threshold = 0.999;
				if (evaluatedArgs.length>2)
					threshold = evaluatedArgs[2].getFloatValue();
				LinkedList<Object> result = map.deleteAll(tokens, threshold);
				Value[] out = new Value[result.size()];
				int i = 0;
				for (Object match:result)
				{
					out[i++] = (Value)match;
				}
				return NLispTools.makeValue(out);
			}
			
		});
		
		env.mapFunction("pmap-remove-value", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, false);
				PatternMap map = (PatternMap)evaluatedArgs[0].getObjectValue();
				
				boolean result = map.deleteValue(evaluatedArgs[1]);
				
				return NLispTools.makeValue(result);
			}
			
		});
		
		env.mapFunction("make-general-parser", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				
				try
				{
					if (evaluatedArgs.length == 0)
					{
						return makeValue(new PatternParser(new String[0]));
					}

					return makeValue(new PatternParser(NLispTools.getStringArrayFromValue(evaluatedArgs[0])));
				}
				catch (Exception e)
				{
					throw new RuntimeException(e);
				}
				
			}
			
		}
		);
		
		env.mapFunction("load-grammar-into-parser", new SimpleFunctionTemplate()
		{
			// First argument is a pattern parser
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				
				try
				{
					PatternParser parser = (PatternParser)evaluatedArgs[0].getObjectValue();
					String[] grammar = NLispTools.getStringArrayFromValue(evaluatedArgs[1]);
					parser.reInit(grammar, true);
					return evaluatedArgs[0];
				}
				catch (Exception e)
				{
					throw new RuntimeException(e);
				}
				
			}
			
		}
		);
		
		
		env.mapFunction("general-number-grammar", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				
				return _number_grammar;
				
			}
			
		}
		);
		
		
		env.mapFunction("make-generalized-text-character", new SimpleFunctionTemplate()
		{

			// First argument is a string
			// Optional second argument is a boolean parameter indicating if string is a single 
			// character or not.  If second argument is absent or false then first argument is treated
			// as the entire token
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				String charValue = evaluatedArgs[0].getString();
				boolean isStringToken = (evaluatedArgs.length == 1 || evaluatedArgs[1].isNull());
				
				return makeValue(new TextCharacter(charValue, !isStringToken));
				
			}
			
		}
		);
		
		
		env.mapFunction("get-match-controller", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, false);
				PatternParser parser = (PatternParser)evaluatedArgs[0].getObjectValue();
				String grammar = evaluatedArgs[1].getString();
				PatternParser.MatcherController controller = parser.getIncrementalParser(grammar);
				return makeValue(controller);
				
			}
			
		}
		);
		
		env.mapFunction("add-custom-text-char-terminal", new SimpleFunctionTemplate()
		{
			// First argument is a Pattern Parser
			// Second argument is the name to assign to this custom terminal
			// Third argument is a lambda function that takes a string argument 
			// representing text token and is a predicate
			// Fourth argument is a lamba function that takes no arguments but which
			// generates a random GeneralizedCharacter in the domain of the predicate
			// given in argment 3
			@Override
			public Value evaluate(final Environment env, Value[] evaluatedArgs) {
				checkActualArguments(4, false, false);
				PatternParser parser = (PatternParser)evaluatedArgs[0].getObjectValue();
				String name = evaluatedArgs[1].getString();
				final FunctionTemplate matchLambda = ((LambdaValue)evaluatedArgs[2]).getLambda();
				final FunctionTemplate sampleLambda = ((LambdaValue)evaluatedArgs[3]).getLambda();
				
				CustomTerminalMatcher cm = new CustomTerminalMatcher()
				{

					@Override
					public boolean match(GeneralizedCharacter gchar) {
						if (gchar instanceof TextCharacter)
						{
							TextCharacter tchar = (TextCharacter)gchar;
							Value[] args = new Value[]{NLispTools.makeValue(tchar.getTextValue())};
							matchLambda.setActualParameters(args);
							try {
								return !matchLambda.evaluate(env, false).isNull();
							} catch (InstantiationException e) {
								throw new RuntimeException(e.getMessage());
							} catch (IllegalAccessException e) {
								throw new RuntimeException(e.getMessage());
							}
						}
						else
							return false;
						
					}

					@Override
					public GeneralizedCharacter sample() {
						try {
							Value result  = sampleLambda.evaluate(env, false);
							return (GeneralizedCharacter)result.getObjectValue();
						} catch (InstantiationException e) {
							throw new RuntimeException(e.getMessage());
						} catch (IllegalAccessException e) {
							throw new RuntimeException(e.getMessage());
						}
						
					}
					
				};
				parser.setCustomTerminalMatcher(name, cm);
				return evaluatedArgs[0];
			}
			
		}
		);
		
		
		env.mapFunction("general-extrude-pattern", new SimpleFunctionTemplate()
		{

			// First argument is a match controller
			// Optinal second argument is a specific pattern to parse, may as well be different
			// from the pattern used to create the match controller.  Can be null
			// Set optional third parameter to true if you want the result as generalized characters, otherwise
			// pattern will be extruded into a list of lists of string representations of the terminal characters
			// When third parameter is missing return value is a list of lists of strings
			// Returns an array of Strings or generalized characters which are the grammar definition
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				PatternParser.MatcherController controller = (PatternParser.MatcherController)evaluatedArgs[0].getObjectValue();
				
				LinkedList<LinkedList<TerminalMatcher>> terms = null;
				if (evaluatedArgs.length >= 2 && !evaluatedArgs[1].isNull())
				{
					String pattern = evaluatedArgs[1].getString();
					terms = controller.extrude(pattern);
				}
				else
					terms = controller.extrude();
				
				Value[] out = new Value[terms.size()];
				int i=0;
				for (LinkedList<TerminalMatcher> list:terms)
				{
					out[i++] = convertMatcherList(list, evaluatedArgs.length == 3 && !evaluatedArgs[2].isNull());
				}
				return NLispTools.makeValue(out);
				
			}
			
		}
		);
		
		env.mapFunction("get-character-tags", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				PatternParser.MatcherController controller = (PatternParser.MatcherController)evaluatedArgs[0].getObjectValue();
				Sequence seq = controller.getProcessedSequence();
				GeneralizedCharacter gchar = null;
				
				Value[] out = new Value[seq.size()], inner = null;
				HashSet<String> tags = null;
				int j = 0;
				for (int i = 0;i<seq.size();i++)
				{
					gchar = seq.get(i);
					if ((tags = gchar.getTagSet())!=null && tags.size()>0)
					{
						inner = new Value[tags.size()];
						j = 0;
						for (String tag:tags)
						{
							inner[j++] = NLispTools.makeValue(tag);
						}
						out[i] = NLispTools.makeValue(inner);
					}
					else
						out[i] = null;
					
				}
				
				return NLispTools.makeValue(out);
				
			}
			
		}
		);
		
		env.mapFunction("reset-controller", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				PatternParser.MatcherController controller = (PatternParser.MatcherController)evaluatedArgs[0].getObjectValue();
				controller.reset();
				
				return evaluatedArgs[0];
				
			}
			
		}
		);
		
		env.mapFunction("update-controller", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				PatternParser.MatcherController controller = (PatternParser.MatcherController)evaluatedArgs[0].getObjectValue();
				GeneralizedCharacter gchar = (GeneralizedCharacter)evaluatedArgs[1].getObjectValue();
				return NLispTools.makeValue(controller.update(gchar));
				
			}
			
		}
		);
		
		env.mapFunction("get-controller-matches", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				PatternParser.MatcherController controller = (PatternParser.MatcherController)evaluatedArgs[0].getObjectValue();
				HashSet<String> set = controller.getCurrentMatches();
				if (set.size()==0)
					return NLispTools.makeValue(new Value[0]);
				else
				{
					Value[] out = new Value[set.size()];
					int i=0;
					for (String key:set)
					{
						out[i++] =  NLispTools.makeValue(key);
					}
					return NLispTools.makeValue(out);
				}
				
			}
			
		}
		);
		
		env.mapFunction("get-controller-partial-matches", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				PatternParser.MatcherController controller = (PatternParser.MatcherController)evaluatedArgs[0].getObjectValue();
				
				HashSet<String> set = controller.getPartialMatches();
				if (set.size()==0)
					return NLispTools.makeValue(new Value[0]);
				else
				{
					Value[] out = new Value[set.size()];
					int i=0;
					for (String key:set)
					{
						out[i++] =  NLispTools.makeValue(key);
					}
					return NLispTools.makeValue(out);
				}
				
			}
			
		}
		);
		
		env.mapFunction("last-update-match-p", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				PatternParser.MatcherController controller = (PatternParser.MatcherController)evaluatedArgs[0].getObjectValue();
				
				if (controller.anyMatches())
					return evaluatedArgs[0];
				else
					return NLispTools.makeValue(false);
				
			}
			
		}
		);
		
		
		
		
		env.mapFunction("make-ascending-sorted-int-set", new SimpleFunctionTemplate()
		{
			// The only argument to this function is a lambda function or 
			// function spec that maps a Value to number
			// Returns a sorted set
			@Override
			public Value evaluate(final Environment env, Value[] evaluatedArgs) {
				NavigableSet<Long> sset = new TreeSet<Long>();
				
				
				return makeValue(sset);
				
			}
			
		}
		);
		
		
		env.mapFunction("make-descending-sorted-int-set", new SimpleFunctionTemplate()
		{
			// The only argument to this function is a lambda function or 
			// function spec that maps a Value to number
			// Returns a sorted set
			@Override
			public Value evaluate(final Environment env, Value[] evaluatedArgs) {
				NavigableSet<Long> sset = new TreeSet<Long>(new Comparator<Long>()
						{

							@Override
							public int compare(Long lhs, Long rhs) {
							
								return -lhs.compareTo(rhs);
							}
							 
						}
						);
				
				
				return makeValue(sset);
			}
			
		}
		);
		
		env.mapFunction("set-contains-key", new SimpleFunctionTemplate()
		{
			// The first argument is a TreeSet
			// The second argument to this function is a Value that should map
			// to an Integral value
			// 
			// Returns the input value if it is in the TreeSet
			@Override
			public Value evaluate(final Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, false);
				NavigableSet<Long> sset = (NavigableSet)evaluatedArgs[0].getObjectValue();
				Long key = evaluatedArgs[1].getIntValue();
				
				if (sset.contains(key))
					return evaluatedArgs[1];
				else
					return Environment.getNull();
			}
			
		});
		
		env.mapFunction("remove-key-from-set", new SimpleFunctionTemplate()
		{
			// The first argument is a TreeSet
			// The second argument to this function is a Value that should map
			// to an Integral value
			// 
			// Returns the input value if it was removed from the TreeSet
			@Override
			public Value evaluate(final Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, false);
				NavigableSet<Long> sset = (NavigableSet)evaluatedArgs[0].getObjectValue();
				Long key = evaluatedArgs[1].getIntValue();
				
				if (sset.contains(key))
				{
					sset.remove(key);
					return evaluatedArgs[1];
				}
				else
					return Environment.getNull();
			}
			
		});
		
		env.mapFunction("add-key-to-set", new SimpleFunctionTemplate()
		{
			// The first argument is a TreeSet
			// The second argument to this function is a Value that should map
			// to an Integral value
			// 
			// Returns the input value if it was added to the TreeSet
			// or false if the key was already present
			@Override
			public Value evaluate(final Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, false);
				NavigableSet<Long> sset = (NavigableSet)evaluatedArgs[0].getObjectValue();
				Long key = evaluatedArgs[1].getIntValue();
				
				if (sset.add(key))
				{
					return evaluatedArgs[1];
				}
				else
					return Environment.getNull();
			}
			
		});
		
		env.mapFunction("get-set-key-range", new SimpleFunctionTemplate()
		{
			// The first argument is a TreeSet
			// The second argument to this function is a Value that should map
			// to an Integral value of the minimum key according to this set's sort
			// order
			// The Third argument is the greater key
			// Optional Fourth and Fifth arguments indicate whether the start or
			// end keys SHOULD BE EXCLUSIVE in the range or not.  Missing or null
			// parameters indicate inclusivity
			// Returns a list of the keys between second argument and third according
			// to inclusivity rules
			@Override
			public Value evaluate(final Environment env, Value[] evaluatedArgs) {
				checkActualArguments(3, true, true);
				NavigableSet<Long> sset = (NavigableSet)evaluatedArgs[0].getObjectValue();
				Long startKey = evaluatedArgs[1].getIntValue();
				Long endKey = evaluatedArgs[2].getIntValue();
				boolean reverse = evaluatedArgs.length >5 && !evaluatedArgs[5].isNull();
				
				boolean inclusiveStart = evaluatedArgs.length <= 3 || evaluatedArgs[3].isNull();
				boolean inclusiveEnd = evaluatedArgs.length <= 4 || evaluatedArgs[4].isNull();
				
				return getSortedSetRange(sset, startKey, inclusiveStart, endKey, inclusiveEnd, reverse);
				
			}
			
		});
		
		env.mapFunction("get-smallest-set-key", new SimpleFunctionTemplate()
		{
			// The first argument is a TreeSet
			
			// Returns the minimum key in this set according to the comparator
			// 
			@Override
			public Value evaluate(final Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				NavigableSet<Long> sset = (NavigableSet)evaluatedArgs[0].getObjectValue();
				Long minimum = sset.first();
				if (minimum == null)
					return Environment.getNull();
				else
					return NLispTools.makeValue(minimum.longValue()); 
			}
			
		});
		
		env.mapFunction("get-largest-set-key", new SimpleFunctionTemplate()
		{
			// The first argument is a TreeSet
			
			// Returns the maximum key in this set according to the comparator
			// 
			@Override
			public Value evaluate(final Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				NavigableSet<Long> sset = (NavigableSet)evaluatedArgs[0].getObjectValue();
				Long maximum = sset.last();
				if (maximum == null)
					return Environment.getNull();
				else
					return NLispTools.makeValue(maximum.longValue()); 
			}
			
		});
		
		env.mapFunction("get-lower-set-keys", new SimpleFunctionTemplate()
		{
			// The first argument is a TreeSet
			// Second argument is an Integral key
			// Optional third argument indicates whether to include the given
			// key, defaults to false
			// Returns the set of keys less than (or if third argument true, equal to) second argument in reverse order, in the order
			// of largest (closest to given key)
			// 
			// 
			@Override
			public Value evaluate(final Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, true, true);
				NavigableSet<Long> sset = (NavigableSet)evaluatedArgs[0].getObjectValue();
				Long maximumKey = evaluatedArgs[1].getIntValue();
				boolean includeKey = evaluatedArgs.length>2 && !evaluatedArgs[2].isNull();
				
				NavigableSet<Long> outSet = sset.headSet(maximumKey, includeKey);
				
				int i = 0, length = outSet.size();
				Value[] outValues = new Value[length];
				for (Long keyInRange:outSet)
				{
					outValues[length - i - 1] = NLispTools.makeValue(keyInRange.longValue());
					i++;
				}
				
				return NLispTools.makeValue(outValues);
				
			}
			
		});
		
		env.mapFunction("get-higher-set-keys", new SimpleFunctionTemplate()
		{
			// The first argument is a TreeSet
			// Second argument is an Integral key
			// Optional third argument indicates whether to include the given
			// key, defaults to false
			// Returns the set of keys greater than (or if third argument true, equal to) second argument in the order
			// of smallest (closest to given key)
			// 
			// 
			@Override
			public Value evaluate(final Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, true, true);
				NavigableSet<Long> sset = (NavigableSet)evaluatedArgs[0].getObjectValue();
				Long maximumKey = evaluatedArgs[1].getIntValue();
				boolean includeKey = evaluatedArgs.length>2 && !evaluatedArgs[2].isNull();
				
				NavigableSet<Long> outSet = sset.tailSet(maximumKey, includeKey);
				
				int i = 0, length = outSet.size();
				Value[] outValues = new Value[length];
				for (Long keyInRange:outSet)
				{
					outValues[i] = NLispTools.makeValue(keyInRange.longValue());
					i++;
				}
				
				return NLispTools.makeValue(outValues);
				
			}
			
		});
		
		env.mapFunction("make-ascending-heap", new SimpleFunctionTemplate()
		{
			// The only argument to this function is a lambda function or 
			// function spec that maps a Value to number
			// Returns a heap with the property that it removes objects in the
			// order of the smallest element according to the function specified
			@Override
			public Value evaluate(final Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				Value functionSpec = evaluatedArgs[0];
				final FunctionTemplate f;
				if (!functionSpec.isLambda())
				{
					if (functionSpec.isString())
					{
						f = env.getFunction(functionSpec.getString());
						if (f == null)
							throw new RuntimeException("Undefined function name for apply: " + functionSpec.getString());
					}
					else
						throw new RuntimeException("First argument to apply must be a FunctionTemplate or the name of a function: " + functionSpec);
				}
				else
					f = functionSpec.getLambda();
				
				PriorityQueue<Value> scoreHeap = new PriorityQueue<Value>(1, new Comparator<Value>()
						{
							public int compare(Value left, Value right)
							{
								try
								{
									f.setActualParameters(new Value[]{left});
									
									Value lresult = f.evaluate(env, false);
									f.setActualParameters(new Value[]{right});
									Value rresult = f.evaluate(env, false);
									
									if (lresult.getFloatValue() < rresult.getFloatValue())
										return -1;
									else if (lresult.getFloatValue() == rresult.getFloatValue())
										return 0;
									else
										return 1;
								}
								catch (Exception e)
								{
									throw new RuntimeException(e.toString());
								}
								
							}
						});
				
				if (evaluatedArgs.length>1)
					if (evaluatedArgs[1].isList())
					{
						for (Value v:evaluatedArgs[1].getList())
							scoreHeap.add(v);
					}
					else
						scoreHeap.add(evaluatedArgs[1]);
				return makeValue(scoreHeap);
				
			}
			
		}
		);
		
		env.mapFunction("make-descending-heap", new SimpleFunctionTemplate()
		{
			// The only argument to this function is a lambda function or 
			// function spec that maps a Value to number
			// Returns a heap with the property that it removes objects in the
			// order of the largest element according to the function specified
			@Override
			public Value evaluate(final Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				Value functionSpec = evaluatedArgs[0];
				final FunctionTemplate f;
				if (!functionSpec.isLambda())
				{
					if (functionSpec.isString())
					{
						f = env.getFunction(functionSpec.getString());
						if (f == null)
							throw new RuntimeException("Undefined function name for apply: " + functionSpec.getString());
					}
					else
						throw new RuntimeException("First argument to apply must be a FunctionTemplate or the name of a function: " + functionSpec);
				}
				else
					f = functionSpec.getLambda();
				
				PriorityQueue<Value> scoreHeap = new PriorityQueue<Value>(1, new Comparator<Value>()
						{
							public int compare(Value left, Value right)
							{
								try
								{
									f.setActualParameters(new Value[]{left});
									
									Value lresult = f.evaluate(env, false);
									f.setActualParameters(new Value[]{right});
									Value rresult = f.evaluate(env, false);
									
									if (lresult.getFloatValue() > rresult.getFloatValue())
										return -1;
									else if (lresult.getFloatValue() == rresult.getFloatValue())
										return 0;
									else
										return 1;
								}
								catch (Exception e)
								{
									throw new RuntimeException(e.toString());
								}
								
							}
						});
				
				if (evaluatedArgs.length>1)
					if (evaluatedArgs[1].isList())
					{
						for (Value v:evaluatedArgs[1].getList())
							scoreHeap.add(v);
					}
					else
						scoreHeap.add(evaluatedArgs[1]);
				
				return makeValue(scoreHeap);
				
			}
			
		}
		);
		
		env.mapFunction("poll-heap", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				PriorityQueue<Value> scoreHeap = (PriorityQueue<Value>)evaluatedArgs[0].getObjectValue(); 
				
				if (scoreHeap.size()>0)
					return scoreHeap.poll().clone();
				else
					return Environment.getNull();
				
			}
			
		}
		);
		
		env.mapFunction("clear-heap", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				PriorityQueue<Value> scoreHeap = (PriorityQueue<Value>)evaluatedArgs[0].getObjectValue(); 
				scoreHeap.clear();
				return evaluatedArgs[0];
				
			}
			
		}
		);
		
		env.mapFunction("get-objects-in-heap", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				PriorityQueue<Value> scoreHeap = (PriorityQueue<Value>)evaluatedArgs[0].getObjectValue(); 
				
				return NLispTools.makeValue(scoreHeap.toArray(new Value[0]));
				
			}
			
		}
		);
		
		env.mapFunction("add-to-heap", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				PriorityQueue<Value> scoreHeap = (PriorityQueue<Value>)evaluatedArgs[0].getObjectValue(); 
				Value v = evaluatedArgs[1];
				scoreHeap.add(v);
					
				return evaluatedArgs[0];
				
			}
			
		}
		);
		
		env.mapFunction("remove-from-heap", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				PriorityQueue<Value> scoreHeap = (PriorityQueue<Value>)evaluatedArgs[0].getObjectValue(); 
				Value v = evaluatedArgs[1];
				scoreHeap.remove(v);
					
				return evaluatedArgs[0];
				
			}
			
		}
		);
		
		env.mapFunction("heap-contains-p", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				PriorityQueue<Value> scoreHeap = (PriorityQueue<Value>)evaluatedArgs[0].getObjectValue(); 
				Value v = evaluatedArgs[1];
				scoreHeap.remove(v);
				if (scoreHeap.contains(v))
					return v;
				else
					return NLispTools.makeValue(false); 
				
			}
			
		}
		);
		
		env.mapFunction("simple-keyset-compare", new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				if (evaluatedArgs[0].isStringHashtable() && evaluatedArgs[1].isStringHashtable())
				{
					
					Set<String> s1 = evaluatedArgs[0].getStringHashtable().keySet();
					Set<String> s2 = evaluatedArgs[1].getStringHashtable().keySet();
					return NLispTools.makeValue(compareSets(s1,s2));
				}
				else if (evaluatedArgs[0].isIntHashtable() && evaluatedArgs[1].isIntHashtable())
				{
					Set<Long> s1 = evaluatedArgs[0].getIntHashtable().keySet();
					Set<Long> s2 = evaluatedArgs[1].getIntHashtable().keySet();
					return NLispTools.makeValue(compareSets(s1,s2));
				}
				else throw new RuntimeException("Both arguments to 'simple-keyset-compare' must be hashtables of the same type");
				
			}
			
		}
		);
		
		env.mapFunction("evaluate-word-number", evaluate_word_number());
		
		env.mapFunction("simple-k-means", simple_k_means());
		
		env.mapFunction("to-sha1", to_sha1_sum());
		
		env.mapFunction("to-md5", to_md5_sum());
		
		env.mapFunction("synchronized", new FunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				checkActualArguments(0, true, true);
				if (!resume)
					resetFunctionTemplate();
				
				Value result = Environment.getNull();
				synchronized (_globalSynch)
				{
					for (;_instructionPointer<_actualParameters.length;_instructionPointer++)
					{
						if (resume && _lastFunctionReturn.getContinuingFunction() != null)
							result = _lastFunctionReturn = _lastFunctionReturn.getContinuingFunction().evaluate(env, resume);
						else
							result = _lastFunctionReturn = env.evaluate(_actualParameters[_instructionPointer], false);
						
						if (result.isContinuation())
							return continuationReturn(result);
						if (result.isBreak() || result.isReturn() || result.isSignal() || result.isSignalOut())
							return resetReturn(result);
					}
				}
				
				
				return resetReturn(result);
			}
		}
		);
		


		env.mapFunction("get-datetime-parts", getDateParts());

        env.mapFunction("parse-datetime-string", parseDateTime());


	}


	public static SimpleFunctionTemplate getDateParts()
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(0, true, true);

				Long time = System.currentTimeMillis();


				try
				{
                    if (evaluatedArgs.length > 0)
                        time = evaluatedArgs[0].getIntValue();

					Calendar calendar = _globalCalendar;
					calendar.setTimeInMillis(time);

					HashMap<String, Value> data = new HashMap<String, Value>();


					int year = calendar.get(Calendar.YEAR);
					int month = calendar.get(Calendar.MONTH);
					int dayOfMonth = calendar.get(Calendar.DAY_OF_MONTH);
					int dayOfYear = calendar.get(Calendar.DAY_OF_YEAR);
					int hourOfDay = calendar.get(Calendar.HOUR_OF_DAY);
					int hourAMPM = calendar.get(Calendar.HOUR);
					int am_pm = calendar.get(Calendar.AM_PM);
					String am_pm_label;
					if (am_pm == Calendar.AM)
						am_pm_label = "AM";
					else
						am_pm_label = "PM";
					int minute = calendar.get(Calendar.MINUTE);
					int second = calendar.get(Calendar.SECOND);
					int millisecond = calendar.get(Calendar.MILLISECOND);
					int weekdayIndex = calendar.get(Calendar.DAY_OF_WEEK) - 1; // Sunday is 1 (!) instead of 0
					String weekdayNameShort = AITools.DAY_OF_WEEK_SHORT_NAME_MAP.get(weekdayIndex);
					String weekdayNameLong = AITools.DAY_OF_WEEK_LONG_NAME_MAP.get(weekdayIndex);
					int weekInMonth = calendar.get(Calendar.WEEK_OF_MONTH);
					int weekInYear = calendar.get(Calendar.WEEK_OF_YEAR);
					String monthNameLong = AITools.MONTH_NAME_LONG_MAP.get(month);
					String monthNameShort= AITools.MONTH_NAME_SHORT_MAP.get(month);

					data.put("YEAR", NLispTools.makeValue(year));
					data.put("MONTH", NLispTools.makeValue(month + 1));
					data.put("MONTH_NAME_SHORT", NLispTools.makeValue(monthNameShort));
					data.put("MONTH_NAME_LONG", NLispTools.makeValue(monthNameLong));
					data.put("DAY_OF_MONTH", NLispTools.makeValue(dayOfMonth));
					data.put("DAY_OF_YEAR", NLispTools.makeValue(dayOfYear));
					data.put("HOUR_OF_DAY", NLispTools.makeValue(hourOfDay));


					data.put("HOUR", NLispTools.makeValue((hourAMPM == 0)?12:hourAMPM));

					data.put("AM_PM", NLispTools.makeValue(am_pm_label));

					data.put("MINUTE", NLispTools.makeValue(minute));
					data.put("SECOND", NLispTools.makeValue(second));
					data.put("MILLISECONDS", NLispTools.makeValue(millisecond));
					data.put("DAY_OF_WEEK_NAME_LONG", NLispTools.makeValue(weekdayNameLong));
					data.put("DAY_OF_WEEK_NAME_SHORT", NLispTools.makeValue(weekdayNameShort));
					data.put("DAY_OF_WEEK_INDEX", NLispTools.makeValue(weekdayIndex));
					data.put("WEEK_IN_MONTH_INDEX", NLispTools.makeValue(weekInMonth));
					data.put("WEEK_IN_YEAR_INDEX", NLispTools.makeValue(weekInYear));

					return new StringHashtableValue(data);
				}
				catch (Exception e)
				{
					throw new RuntimeException(e.getMessage());
				}
			}

		};
	}

	// parse date string to get epoch time using DateFormat.
    // See https://docs.oracle.com/javase/7/docs/api/index.html?javax/imageio/IIOImage.html

	public static SimpleFunctionTemplate parseDateTime()
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);

				try
				{
					String datetime = evaluatedArgs[0].getString();


					String format = "MM/dd/yyyy kk:mm" ;
                    if (evaluatedArgs.length > 1)
                        format = evaluatedArgs[1].getString();
                    SimpleDateFormat dateFormat = new SimpleDateFormat(format);

                    Date date = dateFormat.parse(datetime);
                    long time =date.getTime();
					return NLispTools.makeValue(time);
				}
				catch (Exception e)
				{
					throw new RuntimeException(e.getMessage());
				}
			}

		};
	}


	
	/* Taken from: http://docs.oracle.com/javase/6/docs/technotes/guides/security/crypto/CryptoSpec.html#AppA
     * Converts a byte to hex digit and writes to the supplied buffer
     */
    private static void byte2hex(byte b, StringBuffer buf) {
        char[] hexChars = { '0', '1', '2', '3', '4', '5', '6', '7', '8',
                            '9', 'A', 'B', 'C', 'D', 'E', 'F' };
        int high = ((b & 0xf0) >> 4);
        int low = (b & 0x0f);
        buf.append(hexChars[high]);
        buf.append(hexChars[low]);
    }

    /* Taken from: http://docs.oracle.com/javase/6/docs/technotes/guides/security/crypto/CryptoSpec.html#AppA
     * Converts a byte array to hex string
     */
    private static String toHexString(byte[] block) {
        StringBuffer buf = new StringBuffer();

        int len = block.length;

        for (int i = 0; i < len; i++) {
             byte2hex(block[i], buf);
             if (i < len-1) {
                 buf.append(":");
             }
        }
        return buf.toString();
    }
    
    // TODO - Optimize this
	public static String getSha1Sum(String input) throws UnsupportedEncodingException{
		
		
		
		byte[] raw = input.getBytes("UTF-8");
		return toHexString(_sha1digest.digest(raw));
	}
	
	 // TODO - Optimize this
	public static String getMd5Sum(String input) throws UnsupportedEncodingException{
		
		
		
		byte[] raw = input.getBytes("UTF-8");
		return toHexString(_md5digest.digest(raw));
	}
		
	public static SimpleFunctionTemplate to_md5_sum()
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, false, false);
				String stringForm = evaluatedArgs[0].serializedForm();
				
				try
				{
					return NLispTools.makeValue(getMd5Sum(stringForm));
				}
				catch (Exception e)
				{
					throw new RuntimeException(e.getMessage());
				}
			}
			
		};
	}
	
	public static SimpleFunctionTemplate to_sha1_sum()
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, false, false);
				String stringForm = evaluatedArgs[0].serializedForm();
				
				try
				{
					return NLispTools.makeValue(getSha1Sum(stringForm));
				}
				catch (Exception e)
				{
					throw new RuntimeException(e.getMessage());
				}
			}
			
		};
	}
	
	
	private static Value getSortedSetRange(NavigableSet<Long> set, Long startKey, boolean inclusiveStart, Long endKey, boolean inclusiveEnd, boolean reverseRange)
	{
		NavigableSet<Long> outSet = set.subSet(startKey, inclusiveStart, endKey, inclusiveEnd);
		
		int i = 0, length = outSet.size();
		Value[] outValues = new Value[length];
		for (Long keyInRange:outSet)
		{
			if (reverseRange)
			{
				outValues[length - i - 1] = NLispTools.makeValue(keyInRange.longValue());
			}
			else
				outValues[i] = NLispTools.makeValue(keyInRange.longValue());
			i++;
		}
		
		return NLispTools.makeValue(outValues);
	}
	
	
	public static SimpleFunctionTemplate evaluate_word_number()
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				String numberWords = evaluatedArgs[0].getString();
				try
				{
					WordNumberExpressionPreProcessor wprocessor = new WordNumberExpressionPreProcessor(parser);
					Expression expression = ExpressionFactory.parse(wprocessor, numberWords, null);
					double out = expression.getDoubleValue();
					return NLispTools.makeValue(out);
				}
				catch (Exception e)
				{
					throw new RuntimeException(e.getMessage());
				}
			}
			
		};
	}
	/**
	 * Performs 1 dimensional k-means clustering
	 * first argument is a list of numeric values
	 * second argument is the number of clusters to make
	 * optional third argument is distance between two successive estimations of a cluster
	 * centroid below which the centroid is considered to have converged to a stable point
	 * 
	 * @return Returns a list of the centroid/number points in centroid pairs, ((centroid_i num_points_in_centroid), ...)
	 */
	public static SimpleFunctionTemplate simple_k_means()
	{
		return new SimpleFunctionTemplate ()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, true, true);
				
				int numClasses = (int)evaluatedArgs[1].getIntValue();
				
				Value[] points = evaluatedArgs[0].getList();
					
				int length = points.length;
				
				
				double[] base = new double[length];
				
				double minValue=0, maxValue=0;
				
				for (int i=0;i<length; i++)
				{
					base[i] = points[i].getFloatValue();
					if (i == 0)
					{
						minValue = maxValue = base[i];
					}
					else if (minValue>=base[i])
						minValue = base[i];
					else if (maxValue<= base[i])
						maxValue = base[i];
				}
				
				double error = (evaluatedArgs.length>2)?evaluatedArgs[2].getFloatValue():(maxValue-minValue)*0.01;
				double[] means = new double[numClasses], newMeans = new double[numClasses];
				
				for (int i=0;i<numClasses;i++)
				{
					means[i] = minValue + (maxValue - minValue)/(numClasses + 1)*(i+1);
				}
				
				int[] centroidMass = new int[numClasses];
				
				
				boolean finish = false;
				double closestDistance=0, distance;
				int closestCentroid=0;
				do
				{
					for (int i=0;i<numClasses;i++)
					{
						centroidMass[i] = 0;
						newMeans[i] = 0;
					}
					for (int i=0;i<length;i++)
					{
						for (int j=0;j<numClasses;j++)
						{
							if (j == 0)
							{
								closestDistance = Math.abs(base[i] - means[j]);
								closestCentroid = j;
							}
							else if ((distance = Math.abs(base[i] - means[j])) < closestDistance)
							{
								closestDistance = distance;
								closestCentroid = j;
							}
						}
						
						newMeans[closestCentroid] = (newMeans[closestCentroid]*centroidMass[closestCentroid] + base[i])/(centroidMass[closestCentroid] + 1);
						centroidMass[closestCentroid] ++;
					}
					finish = true;
					for (int i = 0;i<numClasses && !finish;i++)
					{
						finish = finish && Math.abs(means[i] - newMeans[i])<error;
						
					}
					means = newMeans;
				}while (!finish);
				
				Value[] out = new Value[numClasses];
				for (int i=0;i<numClasses;i++)
				{
					out[i] = NLispTools.makeValue(new Value[]{NLispTools.makeValue(newMeans[i]), NLispTools.makeValue(Integer.valueOf(centroidMass[i]))});
					
				}
				
				return NLispTools.makeValue(out);
			}
			
		};
	}
	
	public static <T> double  compareSets(Set<T> s1, Set<T> s2)
	{
		if (s1.size() == 0 && s2.size() == 0)
			return 1;
		double intersectCount = 0;
		double unionCount = 0;
		for (T s:s1)
		{
			if (s2.contains(s))
				intersectCount++;
			unionCount++;
		}
		for (T t:s2)
		{
			if (!s1.contains(t))
				unionCount++;
		}
		
		return intersectCount/unionCount;
	}
	
	
	public static Value makeValue(Object obj)
	{
		if (obj == null)
			return Environment.getNull();
		return new UserObjectValue(obj);
	}
	
	public static Value makeValue(Object[] obj)
	{
		Value[] v = new Value[obj.length];
		for (int i=0;i<v.length;i++)
			v[i] = makeValue(obj[i]);
		return NLispTools.makeValue(v);
	}

	public static Value makeValue(Object[][] nested)
	{
		Value[] base = new Value[nested.length];
		for (int i = 0; i < nested.length;i++)
		{
			base[i] = makeValue(nested[i]);
		}
		return NLispTools.makeValue(base);
	}
	
	private static Value convertMatcherList(LinkedList<TerminalMatcher> terms, boolean getRaw)
	{
		if (!getRaw)
		{
			Value[] out = new Value[terms.size()];
			int i=0;
			for (TerminalMatcher tm:terms)
			{
				out[i++] = NLispTools.makeValue(tm.sample().toString());
				
			}
			return NLispTools.makeValue(out);
		}else
		{
			Object[] out = new Object[terms.size()];
			int i=0;
			for (TerminalMatcher tm:terms)
			{
				out[i++] = tm.sample();
			}
			return makeValue(out);
		}
		
	}
	
	private static Hashtable<String, String> buildHashtableFromList(Value v)
	{
		Hashtable<String, String> outMap = new Hashtable<String, String>();
		for (Value pair:v.getList())
		{
			if (pair.isList()&&pair.getList().length == 2)
			{
				if (pair.getList()[0].isString())
					outMap.put(pair.getList()[0].getString(), pair.getList()[1].toString());
				else
					outMap.put(pair.getList()[0].toString(), pair.getList()[1].toString());
			}
			else 
				return null;
		}
		return outMap;
	}
}
