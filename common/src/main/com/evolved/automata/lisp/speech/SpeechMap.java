package com.evolved.automata.lisp.speech;

import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.PriorityQueue;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;

import com.evolved.automata.lisp.speech.SpeechConfig.AMBIGUITY_NOTICATION_POLICY;
import com.evolved.automata.lisp.speech.SpeechConfig.BOUNDING_PATTERN_EVALUATION_STRATEGY;
import com.evolved.automata.lisp.speech.SpeechConfig.PRECEDENCE_ADHERENCE_POLICY;
import com.evolved.automata.parser.general.GeneralizedCharacter;
import com.evolved.automata.parser.general.PatternParser;
import com.evolved.automata.parser.general.TextCharacter;

public class SpeechMap {
	
	public enum FUNCTION_APPLICABILITY_STATUS
	{
		SUCCESS,
		FAILURE_TOO_FEW_TOKENS,
		PATTERN_TOKENS_MISSING,
		REJECTED
	}
	
	
	static class FunctionApplicabilityData
	{
		FUNCTION_APPLICABILITY_STATUS status;
		public double score;
		public double relativeScore;
		public HashMap<String, ScoredValue> argMap;
		public String functionName;
		public int initiallySkippedTokens = 0;
		public String[] pattern;
		public ScoredValue resultValue;
		public String cacheKey;
		public FunctionApplicabilityData(double s, HashMap<String, ScoredValue> map)
		{
			
			score = s;
			argMap = map;
		}
		
		public FunctionApplicabilityData copy()
		{
			FunctionApplicabilityData nData = new FunctionApplicabilityData(score, argMap);
			nData.functionName = functionName;
			nData.initiallySkippedTokens = 0;
			nData.pattern = pattern;
			nData.resultValue = resultValue;
			nData.status = status;
			return nData;
		}
		
		public FunctionApplicabilityData setStartingSkipCount(int initialSkips)
		{
			initiallySkippedTokens = initialSkips;
			return this;
		}
		
		public FunctionApplicabilityData setFunctionName(String function)
		{
			functionName = function;
			return this;
		}
		
		public FunctionApplicabilityData setSuccess()
		{
			status = FUNCTION_APPLICABILITY_STATUS.SUCCESS;
			return this;
		}
		
		public FunctionApplicabilityData setFailureTooFewTokens()
		{
			status = FUNCTION_APPLICABILITY_STATUS.FAILURE_TOO_FEW_TOKENS;
			return this;
		}
		
		public FunctionApplicabilityData setFailureRejected()
		{
			status = FUNCTION_APPLICABILITY_STATUS.REJECTED;
			return this;
		}
		
		
		public FunctionApplicabilityData setFailureMissingTokens()
		{
			status = FUNCTION_APPLICABILITY_STATUS.PATTERN_TOKENS_MISSING;
			return this;
		}
		
		public boolean failedDueToMissingTokens()
		{
			return status == FUNCTION_APPLICABILITY_STATUS.PATTERN_TOKENS_MISSING;
		}
		
		public boolean failedDueToInsufficientInput()
		{
			return status == FUNCTION_APPLICABILITY_STATUS.FAILURE_TOO_FEW_TOKENS;
		}
		
		public boolean failedRejected()
		{
			return status == FUNCTION_APPLICABILITY_STATUS.REJECTED;
		}
		
		
		public boolean isFailure()
		{
			return status != FUNCTION_APPLICABILITY_STATUS.SUCCESS;
		}
		
		public boolean isSuccess()
		{
			return status == FUNCTION_APPLICABILITY_STATUS.SUCCESS;
		}
		
		public FunctionApplicabilityData addArgument(String name, ScoredValue value)
		{
			if (argMap == null)
				argMap = new HashMap<String, ScoredValue>();
			argMap.put(name, value);
			return this;
		}
		
		public FunctionApplicabilityData setScore(double s)
		{
			score = s;
			return this;
		}
		
		public FunctionApplicabilityData addScore(double s)
		{
			score +=s;
			return this;
		}
	}
	

	static class ProcessStateInfo
	{
		public LinkedList<String> tokensSkippedFromPreviousStage = new LinkedList<String>();
		public String speechFunction = null;
		public ProcessStateInfo(String speechFunctionName)
		{
			speechFunction = speechFunctionName;
		}
	}
	
	final SpeechCache _cache;
	final SpeechConfig _speechConfig;
	final SpeechExternalInterface _evaluationExternalInterface;
	final Comparator<FunctionApplicabilityData> _descendingApplicabilityComparator = new Comparator<FunctionApplicabilityData>()
	{
		// TODO: Add an equality error to account for numerical rounding errors
		@Override
		public int compare(FunctionApplicabilityData left,
				FunctionApplicabilityData right) {
			if (left.relativeScore < right.relativeScore)
				return 1;
			else if (left.relativeScore == right.relativeScore)
				return 0;
			else
				return -1;
				
		}
	};
	
	final Comparator<ScoredValue> _descendingScoredValueComparator = new Comparator<ScoredValue>()
	{
		// TODO: Add an equality error to account for numerical rounding errors
		@Override
		public int compare(ScoredValue left,
				ScoredValue right) {
			if (left.score > right.score)
				return 1;
			else if (left.score == right.score)
				return 0;
			else
				return -1;
				
		}
	};
	
	
	public SpeechMap(SpeechExternalInterface econfig, SpeechConfig config)
	{
		_speechConfig = config;
		_evaluationExternalInterface = econfig; 
		_cache = new SpeechCache(config, econfig);
	}
	
	//  /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ 
	//					Begin Utility Functions
	//  /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\
	
 
	
	String[] slice(String[] data, int start)
	{
		return slice(data, start, data.length);
	}
	
	String[] slice(String[] data, int start, int end)
	{
		int stop = Math.min(data.length, end);
		String[] n = new String[Math.max(0,stop - start)];
		for (int i=start; i < stop;i++)
			n[i - start] = data[i];
		return n;
	}
	
	
	private String isGroup(String token)
	{
		if (token.startsWith("[") && token.endsWith("]"))
			return token.substring(0, token.length()-1).substring(1);
		else
			return null;
		
	}
	
	
	private String isNonTerminal(String token)
	{
		if (token.startsWith("<") && token.endsWith(">"))
			return token.substring(0, token.length()-1).substring(1);
		else
			return null;
		
	}
	
	private static enum SPECIAL_TOKEN_TYPE
	{
		GROUP, NONTERMINAL;
	}
	
	
	Triple<String, Integer,SPECIAL_TOKEN_TYPE> getNextNonTerminal(String[] tokenizedPattern, int start)
	{
		String token;
		
		for (int i = start;i < tokenizedPattern.length;i++)
		{
			token = isNonTerminal(tokenizedPattern[i]);
			if (token != null)
			{
				return Triple.of(token, i, SPECIAL_TOKEN_TYPE.NONTERMINAL); 
			}
		}
		return null;
	}
	
	Triple<String, Integer,SPECIAL_TOKEN_TYPE> getNextGroup(String[] tokenizedPattern, int start)
	{
		String token;
		
		for (int i = start;i < tokenizedPattern.length;i++)
		{
			token = isGroup(tokenizedPattern[i]);
			if (token != null)
			{
				return Triple.of(token, i, SPECIAL_TOKEN_TYPE.GROUP); 
			}
			
		}
		return null;
	}
	
	Triple<String, String[], SpeechConfig.PRECEDENCE_ADHERENCE_POLICY> getGroupSpec(String groupName)
	{
		Triple<String, String[], SpeechConfig.PRECEDENCE_ADHERENCE_POLICY> result = _cache.getGroupSpec(groupName);
		if (result != null)
			return result;
		String[] parts = StringUtils.split(groupName, '#');
		String baseName = parts[0];
		HashMap<String, LinkedList<String>> spec = _speechConfig.getPatternSpecMap();
		
		if (parts.length == 1 && spec.containsKey(baseName))
		{
			return _cache.setGroupSpec(groupName, Triple.of(baseName, new String[]{baseName}, _speechConfig.getPrecedencePolicy()));
		}
		
		HashMap<String, LinkedList<String>> typeMap = _speechConfig.getTypeMap();
		if (parts.length == 1 && typeMap.containsKey(baseName))
		{
			return _cache.setGroupSpec(groupName, Triple.of(baseName, typeMap.get(baseName).toArray(new String[0]), _speechConfig.getTypePrecedencePolicy(baseName)));
		}
		
		LinkedList<String> functionPrecList = new LinkedList<String>();
		String type;
		// Mixing functions from 
		
		SpeechConfig.PRECEDENCE_ADHERENCE_POLICY policy = _speechConfig.getPrecedencePolicy();
		for (int i = 1;i<parts.length;i++)
		{
			type = parts[i];
			if (spec.containsKey(type))
			{
				functionPrecList.add(type);
			}
			else if (typeMap.containsKey(type))
			{ 
				policy = _speechConfig.getTypePrecedencePolicy(type);
				functionPrecList.addAll(typeMap.get(type));
			}
			else
				throw new RuntimeException("Invalid pattern type spec: " + groupName);
		}
		
		if (functionPrecList.size() > 0)
		{
			return _cache.setGroupSpec(groupName, Triple.of(baseName, functionPrecList.toArray(new String[0]), policy));
		}
		else
			return _cache.setGroupSpec(groupName, Triple.of(baseName, (String[])null, policy));
		
	}
	
	boolean isUnstructuredTextGroup(Triple<String, String[], SpeechConfig.PRECEDENCE_ADHERENCE_POLICY> groupSpec)
	{
		return groupSpec.getMiddle() == null;
	}
	
	boolean isUnstructuredTextGroup(String groupName)
	{
		return isUnstructuredTextGroup(getGroupSpec(groupName));
	}
	
	int getMinimumTokensForMatchingGroup(String speechFunction, String groupName, int recursiveLimit)
	{
		if (isUnstructuredTextGroup(groupName) || recursiveLimit <= 0)
			return 1;
		int minValue = Integer.MAX_VALUE;
		
		Triple<String, String[], SpeechConfig.PRECEDENCE_ADHERENCE_POLICY> spec = getGroupSpec(groupName);
		HashMap<String, LinkedList<String>> patternSpec = _speechConfig.getPatternSpecMap();
		int value;
		for (String function: spec.getMiddle())
		{
			for (String rawPattern:patternSpec.get(function))
			{
				value = getMinimumTokensForMatchingPattern(speechFunction, _cache.getTokenizedPattern(rawPattern), recursiveLimit - 1);
				if (value < minValue)
					minValue = value;
			}
		}
		return minValue;
		
	}
	
	int getMinimumTokensForMatchingPattern(String speechFunction, String[] tokenizedPattern, int recursiveLimit)
	{
		Pair<Boolean, String> hasCached = _cache.hasCachedMinimumTokens(tokenizedPattern, recursiveLimit);
		
		if (hasCached.getLeft().booleanValue())
		{
			return _cache.getCachedMinimumTokens(hasCached.getRight());
		}
		
		int matchCount = 0;
		String group;
		boolean simpleTextCounted = false;
		for (String rawToken: tokenizedPattern)
		{
			group = isGroup(rawToken);
			if (group != null)
			{
				if (recursiveLimit > 0)
				{
					matchCount += getMinimumTokensForMatchingGroup(speechFunction, group, recursiveLimit-1);
				}
				else
					matchCount++;
				simpleTextCounted = false;
			}
			else
			{
				if (!simpleTextCounted)
				{
					matchCount++;
					simpleTextCounted = true;
				}
				_cache.addSpeechFunctionMarker(speechFunction, rawToken);
			}
		}
		
		
		return _cache.setCachedMinimumTokens(hasCached.getRight(), recursiveLimit, matchCount) ;
	}
	
	int index = 0;

	private String getCanonicalPhrase(String[] pattern, FunctionApplicabilityData argData)
	{
		
		StringBuilder sbuilder = new StringBuilder();
		
		HashMap<String, ScoredValue> argMap = argData.argMap;
		String key;
		boolean first = true;
		ScoredValue svalue = null;
		for (String token: pattern)
		{
			if (!first)
			{
				sbuilder.append(" ");
			}
			
			key = isGroup(token);
			
			if (key != null)
			{
				Triple<String, String[], SpeechConfig.PRECEDENCE_ADHERENCE_POLICY> groupSpec = getGroupSpec(key);
				ScoredValue v = argMap.get(groupSpec.getLeft());
				if (v.value != null)
				{
					sbuilder.append(_evaluationExternalInterface.toString(v));
				}
				else
				{
					sbuilder.append(v.toString());
				}
				
			}
			else 
			{
				key = isNonTerminal(token);
				if (key != null)
				{
					if (argMap.get(key) == null)
						return null;
					svalue = argMap.get(key);
					
					sbuilder.append(svalue.toString());
				}
				else
				{
					sbuilder.append(token);
				}
			}
			
			first = false;
		}
		
		return sbuilder.toString();
	}
	
	public static String serializeTokens(String[] tokens)
	{
		StringBuilder sbuilder = new StringBuilder();
		String delimiter = ":";
		int i = 0;
		for (i = 0;i<tokens.length;i++)
		{
			if (i > 0)
			{
				sbuilder.append(delimiter);
			}
			sbuilder.append(tokens[i]);
		}
		return sbuilder.toString();
	}
	
	public static String serializeTokens(LinkedList<String> tokens)
	{
		StringBuilder sbuilder = new StringBuilder();
		String delimiter = ":";
		boolean first = true;
		for (String token:tokens)
		{
			if (!first)
			{
				sbuilder.append(delimiter);
			}
			sbuilder.append(token);
			first = false;
		}
		return sbuilder.toString();
	}
	
	
	//  /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ 
	//					End Utility Functions
	//  /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\ /.:.\
	
	Pair<String[], HashMap<String, ScoredValue>> getTransformedInput(String[] tokenizedInput, String[] tokenizedPattern)
	{
		Pair<Boolean, String> hasCache = _cache.hasCachedTransformedValue(tokenizedInput, tokenizedPattern);
		
		if (hasCache.getLeft().booleanValue())
		{
			return _cache.getCachedTransformedValue(hasCache.getRight());
		}
		
		
		PatternParser.MatcherController controller;
		
		int inputIndex = 0, patternIndex = 0;
		int maxInput = tokenizedInput.length, maxPattern = tokenizedPattern.length;
		String patternToken, inputToken;
		Triple<String, Integer,SPECIAL_TOKEN_TYPE> nextSpecialTokenData;
		LinkedList<String> transformedInput = new LinkedList<String>();
		HashMap<String, ScoredValue> transformMap = new HashMap<String, ScoredValue>();
		int i;
		
		HashSet<String> failureSet;
		GeneralizedCharacter gchar;
		boolean reset, firstToken;
		LinkedList<String> capturedTokens = null;
		int loopAheadBaseIndex = 0;
		boolean matchingFinished;
		while (patternIndex < maxPattern && inputIndex < maxInput)
		{
			nextSpecialTokenData = getNextNonTerminal(tokenizedPattern, patternIndex);
			if (nextSpecialTokenData!=null)
			{
				patternToken = nextSpecialTokenData.getLeft();
				controller = _cache.getController(patternToken);
				patternIndex = nextSpecialTokenData.getMiddle().intValue();
				failureSet = _cache.getFailedStartTokens(patternToken);
				while (inputIndex < maxInput && failureSet.contains(tokenizedInput[inputIndex]))
				{
					transformedInput.add(tokenizedInput[inputIndex++]);
				}
				if (inputIndex >= maxInput)
					break;
				
				reset = true;
				
				firstToken = true;
				loopAheadBaseIndex = inputIndex;
				i = loopAheadBaseIndex;
				while (i < maxInput)
				{
					inputToken = tokenizedInput[i];
					gchar = new TextCharacter(inputToken,false);
					if (reset)
					{
						firstToken = true;
						reset = false;
						controller.reset();
						capturedTokens = new LinkedList<String>();
					}
					
					if (matchingFinished = controller.update(gchar))
					{ // parsing halted
						if (!controller.anyMatches())
						{
							if (firstToken)
							{
								failureSet.add(inputToken);
								transformedInput.add(inputToken);
								inputIndex++;
								reset = true;
								i = ++loopAheadBaseIndex;
								continue;
							}
							
							if (controller.getCurrentMatches().size()>0)
							{
								transformMap.put(patternToken, ScoredValue.from(capturedTokens));
								inputIndex = i;
								transformedInput.add(tokenizedPattern[patternIndex]); // raw token representation
								capturedTokens.clear();
								break;
							}
							else
							{
								reset = true;
								i = ++loopAheadBaseIndex;
							}
						}
						else
						{
							capturedTokens.add(inputToken);
							inputIndex = i + 1;
							transformMap.put(patternToken, ScoredValue.from(capturedTokens));
							transformedInput.add(tokenizedPattern[patternIndex]);
							capturedTokens.clear();
							break;
						}
					}
					else
					{
						i++;
						capturedTokens.add(inputToken);
					}
					
					firstToken = false;
				}
				// Test for breaking pattern at end of string
				if (i >= maxInput && capturedTokens.size()>0 && controller.anyMatches())
				{
					if (controller.getCurrentMatches().size()>0)
					{
						transformMap.put(patternToken, ScoredValue.from(capturedTokens));
						inputIndex = i;
						transformedInput.add(tokenizedPattern[patternIndex]); // raw token representation
						
					}
				}
				
				patternIndex++;
				
			}
			else
			{
				patternIndex = maxPattern;
			}
		}
		
		for (i = inputIndex;i < maxInput;i++)
			transformedInput.add(tokenizedInput[i]);
		
		return _cache.setCachedTransformedValue(hasCache.getRight(), Pair.of(transformedInput.toArray(new String[0]), transformMap));
	}
	
	
	
	
	
	
	public static class AssessingRemainingStructuredLiterals
	{
		HashSet<String> inputTokenSet = new HashSet<String>();
		int skipCount = 0;
		int lastMatchInputPosition = -1;
		double score;
	}
	
	private AssessingRemainingStructuredLiterals assessingRemainingStructuredLiterals(ProcessStateInfo pinfo, String[] tokenizedInput, HashSet<String> structuredLiteralTextSet)
	{
		AssessingRemainingStructuredLiterals out = new AssessingRemainingStructuredLiterals();
		int i = 0;
		String inputToken;
		int intersectionCount = 0;
		int unionCount = structuredLiteralTextSet.size();
		boolean foundIntersection = false;
		for (i = 0; i< tokenizedInput.length;i++)
		{
			inputToken = tokenizedInput[i];
			if (!out.inputTokenSet.contains(inputToken))
			{
				if (structuredLiteralTextSet.contains(inputToken))
				{
					intersectionCount++;
					foundIntersection = true;
					out.lastMatchInputPosition++;
				}
				else
				{
					unionCount++;
					if (!foundIntersection)
					{
						out.skipCount++;
					}
				}
				out.inputTokenSet.add(inputToken);
			}
			else if (!foundIntersection)
			{
				out.skipCount++;
			}
		}
		out.score = intersectionCount*1.0/unionCount;
		return out;
	}
	
	
	
	
	private FunctionApplicabilityData assessStructuredLiteralAfterUnstructuredGroup(ProcessStateInfo pinfo, String unstructuredGroupName, String[]  tokenizedInput, String structuredLiteral, String[] remaingPatternWithStructuredLiteral)
	{
		FunctionApplicabilityData finalOutput = null;
		// find first match in tokenized input
		LinkedList<String> potentialUnstructuredGroupText = new LinkedList<String>();
		int inputIndex = 0;
		String token;
		PriorityQueue<FunctionApplicabilityData> resultHeap = new PriorityQueue<SpeechMap.FunctionApplicabilityData>(1, _descendingApplicabilityComparator);
		
		while (inputIndex < tokenizedInput.length)
		{
			token = tokenizedInput[inputIndex];
			if (token.equals(structuredLiteral) && inputIndex > 0)
			{
				
				// try aligning the input with the remaining pattern
				finalOutput = assessPatternArguments(pinfo, slice(tokenizedInput, inputIndex), remaingPatternWithStructuredLiteral);
				if (finalOutput.isSuccess())
				{
					// Add score for value of capture group
					finalOutput.addArgument(unstructuredGroupName, ScoredValue.from(potentialUnstructuredGroupText)).addScore(1);
					if (_speechConfig.getBoundingPatternEvaluationStrategy() == BOUNDING_PATTERN_EVALUATION_STRATEGY.ACCEPT_FIRST_RESULT)
					{
						return finalOutput;
					}
					else
					{
						resultHeap.add(finalOutput);
					}
					
				}
				else if (finalOutput.failedDueToInsufficientInput())
				{
					break;
				}
				else
					potentialUnstructuredGroupText.add(token);
			}
			else
			{
				potentialUnstructuredGroupText.add(token);
			}
			inputIndex++;
		}
		
		if (resultHeap.size()>0)
			return finalOutput = resultHeap.poll();
		else if (finalOutput == null)
		{
			finalOutput = new FunctionApplicabilityData(0, null).setFailureMissingTokens();;
		}
		
		return finalOutput;
	}
	
	
	private FunctionApplicabilityData assessGroupAfterUnstructuredGroup(ProcessStateInfo pinfo, String unstructuredGroupName, String[]  tokenizedInput, String[] remaingPattern)
	{
		FunctionApplicabilityData finalOutput = null;
		// find first match in tokenized input
		LinkedList<String> potentialUnstructuredGroupText = new LinkedList<String>();
		int inputIndex = 0;
		String token;
		PriorityQueue<FunctionApplicabilityData> resultHeap = new PriorityQueue<SpeechMap.FunctionApplicabilityData>(1, _descendingApplicabilityComparator);
		
		while (inputIndex < tokenizedInput.length)
		{
			token = tokenizedInput[inputIndex];
			if (inputIndex > 0)
			{
				// try processing the remaining
				finalOutput = assessPatternArguments(pinfo, slice(tokenizedInput, inputIndex), remaingPattern);
				if (finalOutput.isSuccess())
				{
					finalOutput.addArgument(unstructuredGroupName, ScoredValue.from(potentialUnstructuredGroupText));
					if (_speechConfig.getBoundingPatternEvaluationStrategy() == BOUNDING_PATTERN_EVALUATION_STRATEGY.ACCEPT_FIRST_RESULT)
					{
						return finalOutput;
					}
					else
					{
						resultHeap.add(finalOutput);
					}
					
				}
				else if (finalOutput.failedDueToInsufficientInput())
				{
					break;
				}
				else
					potentialUnstructuredGroupText.add(token);
			}
			else
			{
				potentialUnstructuredGroupText.add(token);
			}
			inputIndex++;
		}
		
		if (resultHeap.size()>0)
			finalOutput = resultHeap.poll();
		else if (finalOutput == null)
		{
			finalOutput = new FunctionApplicabilityData(0, null);
		}
		
		return finalOutput.setFailureMissingTokens();
	}
	
	
	
	private FunctionApplicabilityData assessingPatternAfterUnstructuredGroup(ProcessStateInfo pinfo, String unstructuredGroupName, String[] tokenizedInput, String[] remainingPatternTokens)
	{
		FunctionApplicabilityData finalOutput = new FunctionApplicabilityData(0, null).setFailureMissingTokens();
		HashMap<String, ScoredValue> resultmap;
		if (remainingPatternTokens.length == 0)
		{
			resultmap = new HashMap<String, ScoredValue>();
			resultmap.put(unstructuredGroupName, ScoredValue.from(tokenizedInput));
			finalOutput = new FunctionApplicabilityData(1, resultmap).setSuccess();
		}
		else
		{
			FunctionApplicabilityData potentialResult;
			int boundingTokenIndex = 0;
			
			String token;
			pinfo.tokensSkippedFromPreviousStage = new LinkedList<String>();
			while (boundingTokenIndex < remainingPatternTokens.length)
			{
				token = remainingPatternTokens[boundingTokenIndex];
				if (isGroup(token) == null)
				{
					potentialResult = assessStructuredLiteralAfterUnstructuredGroup(pinfo, unstructuredGroupName, tokenizedInput,token, slice(remainingPatternTokens, boundingTokenIndex));
					if (potentialResult.isSuccess())
					{
						return potentialResult;
					}
				}
				else
				{
					return assessGroupAfterUnstructuredGroup(pinfo, unstructuredGroupName, tokenizedInput, slice(remainingPatternTokens, boundingTokenIndex));
				}
				pinfo.tokensSkippedFromPreviousStage.add(token);
				boundingTokenIndex++;
			}
		}
		return finalOutput;
	}
	
	private FunctionApplicabilityData assessingLeadingStructuredLiteral(ProcessStateInfo pinfo, HashSet<String> initialLiteralSet, String[] tokenizedInput, String[] remainingPatternTokens)
	{
		FunctionApplicabilityData finalOutput = new FunctionApplicabilityData(0, null).setFailureMissingTokens();
		
		HashSet<String> inputSet = new HashSet<String>();
		double score;
		int unionCount = initialLiteralSet.size();
		int intersectionCount = 0;
		int inputIndex = 0;
		int initialSkip = 0;
		boolean skipping = true;
		String token;
		int bestStartingIndex = 0;
		
		double bestScore = 0;
		boolean startingMatch = false;
		while (inputIndex < tokenizedInput.length)
		{
			token = tokenizedInput[inputIndex];
			if (!inputSet.contains(token))
			{
				inputSet.add(token);
				if (initialLiteralSet.contains(token))
				{
					startingMatch = true;
					skipping = false;
					intersectionCount++;
					
					score = intersectionCount/unionCount;
					
					if (score >= bestScore)
					{
						bestScore = score;
						bestStartingIndex = inputIndex;
					}
					else
						break;
					
					
				}
				else
				{
					if (skipping)
						initialSkip++;
					unionCount++;
				}
			}
			
			inputIndex++;
		}
		
		if (!startingMatch)
			return finalOutput;
		
		return  assessPatternArguments(pinfo, slice(tokenizedInput, bestStartingIndex+1), remainingPatternTokens).setStartingSkipCount(initialSkip).addScore(bestScore);
		
	}
	
	private boolean purelySemanticPattern(String[] tokenizedPattern)
	{
		return (tokenizedPattern.length == 1 && isUnstructuredTextGroup(tokenizedPattern[0]));
	}
	
	// TODO: Need to improve strategy for finding optimal input size for matching group
	private FunctionApplicabilityData assessingLeadingStructuredGroup(ProcessStateInfo pinfo, String structuredGroupName, String[] tokenizedInput, String[] remainingPatternTokens)
	{
		FunctionApplicabilityData resultRemaining = null, bestRemaining = null, finalOutput = new FunctionApplicabilityData(0, null).setFailureMissingTokens();
		
		boolean skipRemaining = remainingPatternTokens.length == 0;
		if (skipRemaining)
		{
			resultRemaining = new FunctionApplicabilityData(0, new HashMap<String, ScoredValue>()).setSuccess();
		}
		else
		{
			resultRemaining = new FunctionApplicabilityData(0, null).setFailureTooFewTokens();
		}
		Triple<String, String[], SpeechConfig.PRECEDENCE_ADHERENCE_POLICY> groupSpec = getGroupSpec(structuredGroupName);
		
		String parameterName = groupSpec.getLeft();
		String[] functionPrec = groupSpec.getMiddle();
		SpeechConfig.PRECEDENCE_ADHERENCE_POLICY precedencePolicy = groupSpec.getRight();
		ScoredValue initialResultValue, resultValue;
		
		
		
		int minimumPatternSize = getMinimumTokensForMatchingGroup(pinfo.speechFunction, structuredGroupName, 2);
		
		
		// TODO: handle the case better with shifting
		
		
		boolean cont = true;
		int currentInputOffset = 0;
		double score;
		
		double totalScore;
		
		int numTokensForMatchingCurrentGroup = minimumPatternSize;
		int maxTokensForMatchingCurrentGroup;
		int minTokensToMatchRemainingPattern = 0;
		if (remainingPatternTokens.length > 0)
		{
			minTokensToMatchRemainingPattern = getMinimumTokensForMatchingPattern(pinfo.speechFunction, remainingPatternTokens, 2);
			maxTokensForMatchingCurrentGroup = tokenizedInput.length - minTokensToMatchRemainingPattern;
		}
		else
			maxTokensForMatchingCurrentGroup = tokenizedInput.length;
		
		int bestAction = -1;
		double bestScore = -1;
		ScoredValue bestResult = null;
		int maxRecursiveAttempts = (maxTokensForMatchingCurrentGroup - minimumPatternSize)*2;
		
		
		
		
		
		
		switch (_speechConfig.getPatternWidthSearchPolicy())
		{
			case LINEAR:
				int leadingShiftLimit = 3;
				outer: for (currentInputOffset = 0;currentInputOffset < leadingShiftLimit;  currentInputOffset++)
				{
					inner: for (numTokensForMatchingCurrentGroup = minimumPatternSize; (tokenizedInput.length - (currentInputOffset + numTokensForMatchingCurrentGroup)) >= minTokensToMatchRemainingPattern;numTokensForMatchingCurrentGroup++)
					{
						String[] input = slice(tokenizedInput, currentInputOffset, currentInputOffset + numTokensForMatchingCurrentGroup);
						resultValue = evaluate(input, functionPrec, false, precedencePolicy);
						if (resultValue.score <= 0)
							continue;
						if (!skipRemaining)
							resultRemaining = assessPatternArguments(pinfo, slice(tokenizedInput, currentInputOffset + numTokensForMatchingCurrentGroup), remainingPatternTokens);
						
						if (resultValue.score > 0 && resultRemaining.isSuccess())
						{
							if (skipRemaining)
								resultValue.score*=numTokensForMatchingCurrentGroup*1.0/maxTokensForMatchingCurrentGroup;
							if (resultValue.score + resultRemaining.score >= bestScore)
							{
								bestResult = resultValue;
								bestScore = resultValue.score + resultRemaining.score;
								bestRemaining = resultRemaining;
							}
						}
						else
						{
							if (resultRemaining.failedDueToInsufficientInput())
							{
								// too few characters.  should shorten patternWidth but can't
								if (currentInputOffset == 0)
									return finalOutput.setFailureTooFewTokens();
								else
									break outer;
							}
							
						}
						
						
					}
					if (bestResult != null && _speechConfig.getBoundingPatternEvaluationStrategy() == BOUNDING_PATTERN_EVALUATION_STRATEGY.ACCEPT_FIRST_RESULT)
					{
						return bestRemaining.addArgument(parameterName, bestResult).addScore(bestResult.score);
					}
				}
				if (bestResult!=null)
					return bestRemaining.addArgument(parameterName, bestResult).addScore(bestResult.score * (numTokensForMatchingCurrentGroup - currentInputOffset)/(currentInputOffset + numTokensForMatchingCurrentGroup));
				else			
					return finalOutput;
			case HEURISTIC:
				final int actionInitialAssessment = 0;
				final int actionStretchingStopIndex = 1;
				final int actionShiftingStartingIndex = 2;
				final int actionGetResult = 3;
				
				int actionKey = actionInitialAssessment;
				int prevAction = actionInitialAssessment;
				
				int maxConsecutiveFailures = 3;
				int failureCount = maxConsecutiveFailures;
				int attemptLimit = maxRecursiveAttempts;
				int optimalWidth = minimumPatternSize;
				
				while (attemptLimit > 0 && failureCount > 0)
				{
					switch (actionKey)
					{
						case actionInitialAssessment:
							
							if (numTokensForMatchingCurrentGroup > maxTokensForMatchingCurrentGroup)
							{
								return finalOutput.setFailureTooFewTokens();
							}
							initialResultValue = evaluate(slice(tokenizedInput, currentInputOffset, currentInputOffset + numTokensForMatchingCurrentGroup), functionPrec, false, precedencePolicy);
							if (!skipRemaining)
								resultRemaining = assessPatternArguments(pinfo, slice(tokenizedInput, currentInputOffset + numTokensForMatchingCurrentGroup), remainingPatternTokens);
							if (initialResultValue.score > 0 && resultRemaining.isSuccess())
							{
								bestResult = initialResultValue;
								bestScore = initialResultValue.score + resultRemaining.score;
								bestRemaining = resultRemaining;
							}
							else
							{
								if (resultRemaining.failedDueToInsufficientInput())
								{
									// too few characters.  should shorten patternWidth but can't
									return finalOutput.setFailureTooFewTokens();
								}
								
							}
							// this is normally what you have to do
							actionKey = actionStretchingStopIndex;
							attemptLimit--;
							break;
						case actionShiftingStartingIndex:
							currentInputOffset++;
							actionKey = actionGetResult;
							break;
						case actionStretchingStopIndex:
							if (numTokensForMatchingCurrentGroup + 1 > maxTokensForMatchingCurrentGroup)
							{
								actionKey = actionShiftingStartingIndex;
								numTokensForMatchingCurrentGroup = optimalWidth;
								break;
							}
							else
							{
								actionKey = actionGetResult;
								numTokensForMatchingCurrentGroup++;
							}
							break;
						case actionGetResult:
							if (currentInputOffset + numTokensForMatchingCurrentGroup + minTokensToMatchRemainingPattern> tokenizedInput.length)
							{
								if (currentInputOffset > 0)
								{
									currentInputOffset--;
								}
								else if (numTokensForMatchingCurrentGroup > minimumPatternSize)
								{
									numTokensForMatchingCurrentGroup--;
								}
								attemptLimit--;
								break;
							}
							
							resultValue = evaluate(slice(tokenizedInput, currentInputOffset, currentInputOffset + numTokensForMatchingCurrentGroup), functionPrec, false, precedencePolicy);
							if (!skipRemaining)
								resultRemaining = assessPatternArguments(pinfo, slice(tokenizedInput, currentInputOffset + numTokensForMatchingCurrentGroup), remainingPatternTokens);
							
							if (resultValue.score > 0 && resultRemaining.isSuccess())
							{
								failureCount = maxConsecutiveFailures;
								if ( resultValue.score + resultRemaining.score >= bestScore)
								{
									bestScore = resultValue.score + resultRemaining.score;
									bestRemaining = resultRemaining;
									bestResult = resultValue;
									actionKey = actionStretchingStopIndex;
									optimalWidth = numTokensForMatchingCurrentGroup;
									if (prevAction == actionStretchingStopIndex || prevAction == actionInitialAssessment) // don't reduce attempt limit if making progress
										break;
								}
								else
								{
									actionKey = actionShiftingStartingIndex;
									numTokensForMatchingCurrentGroup = optimalWidth;
								}
								
							}
							else
							{
								failureCount--;
								if (bestResult == null && attemptLimit < 2 && currentInputOffset == 0)
								{ // try shifting the starting index if stretching the 
									actionKey = actionShiftingStartingIndex;
									numTokensForMatchingCurrentGroup = optimalWidth;
								}
							}
							attemptLimit--;
							break;	
						
					}
					prevAction = actionKey;
				}
				
				if (bestResult != null)
				{
					return bestRemaining.addArgument(parameterName, bestResult).addScore(bestResult.score);
				}
				else
					return finalOutput;
		}
		
		// this shouldn't get called but required by compiler
		return finalOutput;
		
		
		
	}
	
	
	
	FunctionApplicabilityData assessPatternArguments(ProcessStateInfo pinfo, String[] tokenizedInput, String[] tokenizedPattern)
	{
		int minimumTokensForMatch = getMinimumTokensForMatchingPattern(pinfo.speechFunction, tokenizedPattern, 2);
		if (tokenizedInput.length < minimumTokensForMatch)
			return new FunctionApplicabilityData(0, null).setFailureTooFewTokens();
		
		
		int groupPos = -1;
		String groupName = null;
		int patternLength = tokenizedPattern.length;
		HashSet<String> structuredLiteralTextSet = new HashSet<String>();
		if (pinfo.tokensSkippedFromPreviousStage!=null && pinfo.tokensSkippedFromPreviousStage.size()>0)
		{
			structuredLiteralTextSet.addAll(pinfo.tokensSkippedFromPreviousStage);
			pinfo.tokensSkippedFromPreviousStage.clear();
		}
		int i = 0;
		
		// ............................... 
		//			Find the first group
		// ............................... 
		while (i < patternLength)
		{
			groupName = isGroup(tokenizedPattern[i]);
			if (groupName!=null)
			{
				groupPos = i;
				break;
			}
			else
			{
				if (!structuredLiteralTextSet.contains(tokenizedPattern[i]))
				{
					structuredLiteralTextSet.add(tokenizedPattern[i]);
					
				}
			}
			i++;
		}
		
		// ............................... 
		//		Extra 
		
		
		if (groupName == null)
		{
			AssessingRemainingStructuredLiterals remainingStructuredLiteralAssessment = assessingRemainingStructuredLiterals(pinfo, tokenizedInput, structuredLiteralTextSet);
			return new FunctionApplicabilityData(remainingStructuredLiteralAssessment.score, null).setStartingSkipCount(remainingStructuredLiteralAssessment.skipCount).setSuccess();
		}
		else
		{
			if (groupPos == 0)
			{
				if (isUnstructuredTextGroup(groupName))
				{
					return assessingPatternAfterUnstructuredGroup(pinfo, groupName, tokenizedInput, slice(tokenizedPattern, 1));	
				}
				else
				{
					return assessingLeadingStructuredGroup(pinfo, groupName, tokenizedInput, slice(tokenizedPattern, groupPos+1));
				}
			}
			else
			{
				return assessingLeadingStructuredLiteral(pinfo, structuredLiteralTextSet, tokenizedInput, slice(tokenizedPattern, groupPos));
			}
		}
	}
	
	FunctionApplicabilityData assessPattern(ProcessStateInfo pinfo, String[] tokenizedInput, String[] tokenizedPattern)
	{
		Pair<Boolean, String> hasCache = _cache.hasCachedPatternAssessmentValue(tokenizedInput, tokenizedPattern);
		if (hasCache.getLeft().booleanValue())
		{
			return _cache.getCachedAssessmentValue(hasCache.getRight());
		}
		
		// TODO: fix handling of transformed input for recursively called patterns
		Pair<String[], HashMap<String, ScoredValue>> transformedData = getTransformedInput(tokenizedInput, tokenizedPattern);
		String[] transformedInput = transformedData.getKey();
		boolean purelySemanticPattern = purelySemanticPattern(tokenizedPattern);
		// Purely semantic patterns can't be assessed based on their grammatical structure; instead, they must be evaluated by a 
		// function
		if (_cache.isSpeechFunctionIndexed(pinfo.speechFunction) && !purelySemanticPattern && !_cache.getViableSpeechFunctions(transformedInput).contains(pinfo.speechFunction))
		{
			return new FunctionApplicabilityData(0, new HashMap<String, ScoredValue>()).setFailureMissingTokens();
		}
		
		FunctionApplicabilityData argumentSpec = assessPatternArguments(pinfo, transformedInput, tokenizedPattern);
		
		if (argumentSpec.isFailure())
			return _cache.setCachedAssessmentValue(hasCache.getRight(), argumentSpec);
		
		
		
		ScoredValue value;
		for (String key:transformedData.getRight().keySet())
		{
			value = transformedData.getRight().get(key);
			argumentSpec.addArgument(key, value);
		}
		
		
		double maxScore = getMinimumTokensForMatchingPattern(pinfo.speechFunction, tokenizedPattern, 0);
		double actualScore = argumentSpec.score/maxScore;
		
		return _cache.setCachedAssessmentValue(hasCache.getRight(), argumentSpec.setScore(actualScore));
		
	}
	
	private ScoredValue evaluate(String[] tokenizedInput, String[] functionPrecedence, boolean top, SpeechConfig.PRECEDENCE_ADHERENCE_POLICY precedencePolicy)
	{
		
		Pair<Boolean, String> hasCached = _cache.hasCachedPhraseValue(precedencePolicy, tokenizedInput, functionPrecedence);
		if (hasCached.getLeft().booleanValue())
		{
			return _cache.getCachedPhraseValue(hasCached.getRight());
		}
		Pair<Boolean, String> hasCachedFunctionValue = null;
		
		
		ScoredValue result;
		HashMap<String, LinkedList<String>> patternSpec = _speechConfig.getPatternSpecMap();
		LinkedList<String> patternVariations;
		
		
		double minResultScore = _speechConfig.getMinimumResultScoreThreshold();
		String[] tokenizedPattern;
		FunctionApplicabilityData functionApplicability;
		FunctionApplicabilityData cachedAppData, maxApplicability = null;
		String canonicalPhrase = null;
		PriorityQueue<FunctionApplicabilityData> maxVariationHeap = new PriorityQueue<FunctionApplicabilityData>(1, _descendingApplicabilityComparator);
		String functionName;
		int i = 0, numFunctions = functionPrecedence.length;
		double effectiveScore, maxScore=0;
		double minimumPatternScore = _speechConfig.getMinimumPatternScoreThreshold();
		
		
		
		for (i = 0;i <  numFunctions; i ++)
		{
			functionName = functionPrecedence[i];
			patternVariations = patternSpec.get(functionName);
			maxApplicability = null;
			
			hasCachedFunctionValue = _cache.hasCachedFunctionValue(tokenizedInput, functionName);
			
			if (hasCachedFunctionValue.getLeft().booleanValue())
			{
				cachedAppData = _cache.getCachedFunctionValue(hasCachedFunctionValue.getRight());

				if (cachedAppData.isSuccess())
				{
					cachedAppData.relativeScore = cachedAppData.score;
					switch (precedencePolicy)
					{
						case FULL:
							maxVariationHeap.add(cachedAppData);
							break;
						case NONE:
							if (maxApplicability == null || cachedAppData.score > maxApplicability.score)
							{
								maxApplicability = cachedAppData;
								
							}
							break;
						case PARTIAL:
							effectiveScore = cachedAppData.score * (numFunctions - i);
							if (maxApplicability == null || maxScore < effectiveScore)
							{
								maxScore = effectiveScore;
								maxApplicability = cachedAppData;
								maxApplicability.relativeScore = effectiveScore;
							}
							break;
					}
					
					
				}
				
				// At this point we have the processed results of the current speech input and the current function
			}
			else
			{
				for (String pattern:patternVariations)
				{
					ProcessStateInfo pinfo = new ProcessStateInfo(functionName);
					tokenizedPattern = _evaluationExternalInterface.tokenize(pattern);
					functionApplicability = assessPattern(pinfo, tokenizedInput, tokenizedPattern);
					functionApplicability.setFunctionName(functionName);
					functionApplicability.pattern = tokenizedPattern;
					
					if (functionApplicability.isSuccess() && functionApplicability.score >= minimumPatternScore)
					{
						functionApplicability.relativeScore = functionApplicability.score;
						switch (precedencePolicy)
						{
							case  FULL:
								maxVariationHeap.add(functionApplicability);
								break;
							case NONE:
								if (maxApplicability == null || functionApplicability.score > maxApplicability.score)
								{
									maxApplicability = functionApplicability;
									
								}
								break;
							case PARTIAL:
								effectiveScore = functionApplicability.score * (numFunctions - i);
								if (maxApplicability == null || maxScore < effectiveScore)
								{
									maxScore = effectiveScore;
									maxApplicability = functionApplicability;
									functionApplicability.relativeScore = effectiveScore;
								}
								break;
						}
					}
				}
				
				_cache.addSpeechFunctionToCompletionIndex(functionName);
				
				if (((precedencePolicy != PRECEDENCE_ADHERENCE_POLICY.FULL) && maxApplicability == null) || 
						((precedencePolicy == PRECEDENCE_ADHERENCE_POLICY.FULL) && maxVariationHeap.size() == 0))
				{
					_cache.setCachedFunctionValue(hasCachedFunctionValue.getRight(), new FunctionApplicabilityData(0, null).setFunctionName(functionName).setFailureMissingTokens() , new ScoredValue(null, 0));
				}
				
			}
			
			
			
			if (precedencePolicy == SpeechConfig.PRECEDENCE_ADHERENCE_POLICY.FULL)
			{
				while (maxVariationHeap.size()>0)
				{
					functionApplicability = maxVariationHeap.poll();
					
					if (functionApplicability.resultValue != null)
					{
						result = functionApplicability.resultValue;
					}
					else
					{
						canonicalPhrase = getCanonicalPhrase(functionApplicability.pattern, functionApplicability);
						
						if (canonicalPhrase == null)
							break;
						functionApplicability.addArgument(_speechConfig.getCanonicalPhraseArgumentKey(),  ScoredValue.from(canonicalPhrase));
						
						result = _cache.setCachedFunctionValue(hasCachedFunctionValue.getRight(), functionApplicability, _evaluationExternalInterface.evaluate(functionName, functionApplicability.argMap));
						result.score*=functionApplicability.score;
						
					}
					if (result.score > minResultScore)
					{
						
						return _cache.setCachedPhraseValue(hasCached.getRight(), result);
					}
					
				}
			}
			else if (maxApplicability != null)
			{
				maxVariationHeap.add(maxApplicability);
			}
			
		}
		
		// TODO: HANDLE the cases where  non-full precedence policy
		AMBIGUITY_NOTICATION_POLICY ambiguityPolicy = _speechConfig.getAmbiguityNotificationPolicy();
		double prevScore = -1;
		double ambiguityThreshold = _speechConfig.getAmbiguityThresholdFraction();
		LinkedList<FunctionApplicabilityData> intrinsicallyAmbiguousFunctions = new LinkedList<SpeechMap.FunctionApplicabilityData>(); 
		
		LinkedList<FunctionApplicabilityData> ambiguityList = new LinkedList<SpeechMap.FunctionApplicabilityData>();
		while (maxVariationHeap.size()>0)
		{
			functionApplicability = maxVariationHeap.poll();
			if (prevScore == -1 || (functionApplicability.relativeScore >= ( prevScore * ambiguityThreshold)))
			{
				functionName = functionApplicability.functionName;
				hasCachedFunctionValue = _cache.hasCachedFunctionValue(tokenizedInput, functionName);
				
				if (_speechConfig.isIntrinsicallyAmbiguousFunction(functionName))
				{
					if (!_speechConfig.functionWithSideEffectsP(functionName))
					{
							
						if (hasCachedFunctionValue.getLeft().booleanValue())
						{
							functionApplicability = _cache.getCachedFunctionValue(hasCachedFunctionValue.getRight());
							result = functionApplicability.resultValue;
						}
						else
						{
							canonicalPhrase = getCanonicalPhrase(functionApplicability.pattern, functionApplicability);
							
							if (canonicalPhrase == null)
								continue;
							functionApplicability.addArgument(_speechConfig.getCanonicalPhraseArgumentKey(),  ScoredValue.from(canonicalPhrase));
							
							result = _evaluationExternalInterface.evaluate(functionName, functionApplicability.argMap);
							
							if (result!=null)
							{
								result.score*=functionApplicability.score;
								_cache.setCachedFunctionValue(hasCachedFunctionValue.getRight(), functionApplicability, result);
							}
							else
								_cache.setCachedFunctionValue(hasCachedFunctionValue.getRight(), functionApplicability, new ScoredValue(null, 0));
						}
						
						if (result != null && result.score > 0)
						{
							functionApplicability.resultValue = result;
							intrinsicallyAmbiguousFunctions.add(functionApplicability);
						}
					}
					else
					{
						intrinsicallyAmbiguousFunctions.add(functionApplicability);
					}
				}
				else 
				{
					if (hasCachedFunctionValue.getLeft().booleanValue())
					{
						functionApplicability = _cache.getCachedFunctionValue(hasCachedFunctionValue.getRight());
						result = functionApplicability.resultValue;
					}
					else
					{
						canonicalPhrase = getCanonicalPhrase(functionApplicability.pattern, functionApplicability);
						
						if (canonicalPhrase == null)
							continue;
						functionApplicability.addArgument(_speechConfig.getCanonicalPhraseArgumentKey(),  ScoredValue.from(canonicalPhrase));
						
						result = _evaluationExternalInterface.evaluate(functionName, functionApplicability.argMap);
						
						if (result!=null)
						{
							result.score*=functionApplicability.score;
							_cache.setCachedFunctionValue(hasCachedFunctionValue.getRight(), functionApplicability, result);
						}
						else
							_cache.setCachedFunctionValue(hasCachedFunctionValue.getRight(), functionApplicability, new ScoredValue(null, 0));
					}
					

					if (result != null && result.score > 0)
					{
						functionApplicability.resultValue = result;
						prevScore = functionApplicability.relativeScore;
						if (ambiguityPolicy == AMBIGUITY_NOTICATION_POLICY.ALWAYS_NOTIFY || 
								(ambiguityPolicy == AMBIGUITY_NOTICATION_POLICY.NOTIFY_ONLY_AT_TOP_LEVEL && top))
						{
							
							ambiguityList.add(functionApplicability);
						}
						else 
						{
							return _cache.setCachedPhraseValue(hasCached.getRight(), result);
						}
					}
				}
			}
			else
				break;
			
		}
		
		ScoredValue finalResult = new ScoredValue(null, 0); 
		if (ambiguityList.size() == 1)
		{
			
			
			
			return _cache.setCachedPhraseValue(hasCached.getRight(), ambiguityList.getFirst().resultValue);
		}
		else if (ambiguityList.size() > 0)
		{
			FunctionApplicabilityData[] optionList = ambiguityList.toArray(new FunctionApplicabilityData[0]);
			if (ambiguityPolicy == AMBIGUITY_NOTICATION_POLICY.ALWAYS_NOTIFY || 
					(ambiguityPolicy == AMBIGUITY_NOTICATION_POLICY.NOTIFY_ONLY_AT_TOP_LEVEL && top))
			{
				return _evaluationExternalInterface.onAmbiguousResult(optionList, top).resultValue;
			}
		}
		
		if (intrinsicallyAmbiguousFunctions.size() == 1)
		{
			functionApplicability = intrinsicallyAmbiguousFunctions.getFirst();
			if (functionApplicability.resultValue != null)
				finalResult = functionApplicability.resultValue;
			else
			{
				finalResult = _evaluationExternalInterface.evaluate(functionApplicability.functionName, functionApplicability.argMap);
				
			}
		}
		
		return _cache.setCachedPhraseValue(hasCached.getRight(), finalResult);
	}
	
	
	// [-_-] (o.o) [-_-] (o.o) [-_-] (o.o) [-_-] (o.o) [-_-] (o.o) [-_-] (o.o) 
	//							Main Functions
	// [-_-] (o.o) [-_-] (o.o) [-_-] (o.o) [-_-] (o.o) [-_-] (o.o) [-_-] (o.o)
	
	
	public ScoredValue evaluate(String[] tokenizedInput, SpeechConfig.PRECEDENCE_ADHERENCE_POLICY precedencePolicy)
	{
		return evaluate(tokenizedInput, _speechConfig.getDefaultFunctionPrecedence(), true, precedencePolicy);
	}
	
	
	public ScoredValue evaluate(String[] tokenizedInput)
	{
		return evaluate(tokenizedInput, _speechConfig.getPrecedencePolicy());
	}
	
	
	//TODO: fix flex-evaluate to work properly with non-terminals
	/**
	 * This is a desperation function.  Call it as a last resort if you really need a response.  It doesn't work as well
	 * if you have a lot of grammars since it filters speech-funtions based on the raw speech text instead of the text 
	 * modified from context-free grammar patterns.  Any patterns that are reliant on CFGs may be excluded from this 
	 * function.
	 * 
	 * @param tokenizedInput
	 * @return
	 */

	public ScoredValue flexEvaluate(String[] tokenizedInput)
	{
		SpeechConfig.PRECEDENCE_ADHERENCE_POLICY policy = SpeechConfig.PRECEDENCE_ADHERENCE_POLICY.NONE;
		if (_cache.hasCompleteSpeechFunctionIndex())
		{
			HashSet<String> viableFunctions = _cache.getViableSpeechFunctions(tokenizedInput);
			if (viableFunctions.size() > 0)
			{
				String[] functionPrec = viableFunctions.toArray(new String[0]);
				return evaluate(tokenizedInput, functionPrec, true, policy);
			}
			else
				return new ScoredValue(null, 0);
		}
		else
		{
			return evaluate(tokenizedInput, _speechConfig.getPatternSpecMap().keySet().toArray(new String[0]), true, policy);
		}
		
	}
	
	public void clearSubCache(SpeechCache.SUB_CACHE cache)
	{
		_cache.clearCache(cache);
	}
	
	public void clearAllSubCaches()
	{
		
		for (SpeechCache.SUB_CACHE cache: SpeechCache.SUB_CACHE.values())
		{
			_cache.clearCache(cache);
		}
		
	}
	
	public SpeechCache getCache()
	{
		return _cache;
	}
	
}
