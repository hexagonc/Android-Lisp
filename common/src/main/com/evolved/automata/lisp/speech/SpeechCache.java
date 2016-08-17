package com.evolved.automata.lisp.speech;

import java.util.HashMap;
import java.util.HashSet;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import com.evolved.automata.lisp.speech.SpeechMap.FunctionApplicabilityData;
import com.evolved.automata.parser.general.PatternParser;

public class SpeechCache {
	
	
	HashMap<String, HashSet<String>> _failedInitialTokenMap;
	HashMap<String, PatternParser.MatcherController> _grammarControllerMap;
	SpeechConfig _config;
	SpeechExternalInterface _evaluationConfig;
	HashMap<String, Pair<String, String[]>> _groupSpecCacheMap;
	int _firstResultIsBestCount = 0;
	int _firstResultIsOnlyCount = 0;
	int _numResultSearches = 0;
	HashMap<String, Pair<String[], HashMap<String, ScoredValue>>> _transformedInputCacheMap;
	HashMap<String, FunctionApplicabilityData> _assessPatternMap;
	HashMap<String, Integer> _minimumPatternCache;
	
	public HashMap<String, ScoredValue> _phraseCache;
	HashMap<String, String[]> _tokenizedPatternMap;
	HashMap<String, FunctionApplicabilityData> _functionValueMap;
	
	public SpeechCache(SpeechConfig config, SpeechExternalInterface econfig)
	{
		_functionValueMap = new HashMap<String, FunctionApplicabilityData>();
		_phraseCache = new HashMap<String, ScoredValue>();
		_minimumPatternCache = new HashMap<String, Integer>();
		_assessPatternMap = new HashMap<String, SpeechMap.FunctionApplicabilityData>();
		_config = config;
		_evaluationConfig = econfig;
		_failedInitialTokenMap = new HashMap<String, HashSet<String>>();
		_grammarControllerMap = new HashMap<String, PatternParser.MatcherController>();
		_groupSpecCacheMap = new HashMap<String, Pair<String,String[]>>();
		_tokenizedPatternMap = new HashMap<String, String[]>();
		_transformedInputCacheMap = new HashMap<String, Pair<String[],HashMap<String,ScoredValue>>>();
	}
	
	Pair<Boolean, String> hasCachedFunctionValue(String[] tokenizedInput, String functionName)
	{
		if (_config.functionWithSideEffectsP(functionName))
			return Pair.of(Boolean.valueOf(false), null);
		StringBuilder key = new StringBuilder(SpeechMap.serializeTokens(tokenizedInput));
		key.append("+").append(functionName);
		String akey = key.toString();
		return Pair.of(_functionValueMap.containsKey(akey), akey);
	}
	
	FunctionApplicabilityData getCachedFunctionValue(String key)
	{
		FunctionApplicabilityData out = _functionValueMap.get(key);
		if (out == null)
			return out;
		return out.copy();
	}
	
	ScoredValue setCachedFunctionValue(String key, FunctionApplicabilityData data, ScoredValue value)
	{
		if (key != null)
		{
			data.resultValue = value;
			_functionValueMap.put(key, data);
		}
		return value.copy();
	}
	
	// l
	
	public Pair<Boolean, String> hasCachedMinimumTokens(String[] pattern, int recursiveLimit)
	{
		StringBuilder totalKey = new StringBuilder();
		totalKey.append(SpeechMap.serializeTokens(pattern)).append("+").append(recursiveLimit);
		
		String key = totalKey.toString();
		return Pair.of(_minimumPatternCache.containsKey(key), key);
	}
	
	public int getCachedMinimumTokens(String key)
	{
		return _minimumPatternCache.get(key).intValue();
	}
	
	public int setCachedMinimumTokens(String key, int recursiveLimit, int value)
	{
		_minimumPatternCache.put(key, Integer.valueOf(value));
		return value;
	}
	
	
	// l
	
	
	public Pair<Boolean, String> hasCachedPatternAssessmentValue(String[] input, String[] pattern)
	{
		StringBuilder totalKey = new StringBuilder();
		totalKey.append(SpeechMap.serializeTokens(input)).append("+").append(SpeechMap.serializeTokens(pattern));
		
		String key = totalKey.toString();
		return Pair.of(_assessPatternMap.containsKey(key), key);
	}
	
	public FunctionApplicabilityData getCachedAssessmentValue(String key)
	{
		FunctionApplicabilityData out = _assessPatternMap.get(key);
		if (out == null)
			return null;
		return _assessPatternMap.get(key);
	}
	
	public FunctionApplicabilityData setCachedAssessmentValue(String key, FunctionApplicabilityData value)
	{
		_assessPatternMap.put(key, value);
		return value;
	}
	
	
	public Pair<Boolean, String> hasCachedTransformedValue(String[] input, String[] pattern)
	{
		StringBuilder totalKey = new StringBuilder();
		totalKey.append(SpeechMap.serializeTokens(input)).append("+").append(SpeechMap.serializeTokens(pattern));
		
		String key = totalKey.toString();
		return Pair.of(_transformedInputCacheMap.containsKey(key), key);
	}
	
	public Pair<String[], HashMap<String, ScoredValue>> getCachedTransformedValue(String key)
	{
		return _transformedInputCacheMap.get(key);
	}
	
	public Pair<String[], HashMap<String, ScoredValue>> setCachedTransformedValue(String key, Pair<String[], HashMap<String, ScoredValue>> value)
	{
		_transformedInputCacheMap.put(key, value);
		return value;
	}
	
	public void notifySuccessfulFirstResultSearch()
	{
		_firstResultIsBestCount++;
		_numResultSearches++;
	}
	
	
	
	public String[] getTokenizedPattern(String rawPattern)
	{
		String[] value = _tokenizedPatternMap.get(rawPattern);
		if (value == null)
		{
			_tokenizedPatternMap.put(rawPattern, value = _evaluationConfig.tokenize(rawPattern));
		}
		return value;
	}
	
	public Pair<String, String[]> getGroupSpec(String patternName)
	{
		return _groupSpecCacheMap.get(patternName);
	}
	
	public Pair<String, String[]> setGroupSpec(String groupName, Pair<String, String[]> value)
	{
		_groupSpecCacheMap.put(groupName, value);
		return value;
	}
	
	/**
	 * Retrieves a match controller, creating a new one and mapping it if necessary
	 * @param nonTerminalName
	 * @return
	 */
	PatternParser.MatcherController getController(String nonTerminalName)
	{
		PatternParser.MatcherController controller = _grammarControllerMap.get(nonTerminalName);
		if (controller == null)
		{
			controller = _config.getGrammarParser().getIncrementalParser(nonTerminalName);
			_grammarControllerMap.put(nonTerminalName, controller);
		}
		controller.reset();
		return controller;
	}
	
	/**
	 * Always returns the set of start tokens that will fail the non-terminal, adding
	 * a new one to the failure map if necessary
	 * @param nonTerminalName
	 * @return
	 */
	HashSet<String> getFailedStartTokens(String nonTerminalName)
	{
		HashSet<String> failure = _failedInitialTokenMap.get(nonTerminalName);
		if (failure == null)
			_failedInitialTokenMap.put(nonTerminalName, failure = new HashSet<String>());
		return failure;
	}
	
	
	ScoredValue getCachedPhraseValue(String[] phrase, String[] functionPrec)
	{
		return getCachedPhraseValue(getPhraseCacheKey(phrase, functionPrec));
	}
	
	ScoredValue setCachedPhraseValue(String phraseKey, ScoredValue value)
	{
		if (phraseKey != null)
			_phraseCache.put(phraseKey, value);
		return value;
	}
	
	
	ScoredValue getCachedPhraseValue(String phraseKey)
	{
		ScoredValue s = _phraseCache.get(phraseKey);
		if (s!=null)
			return s.copy();
		return null;
	}
	
	
	
	Pair<Boolean, String> hasCachedPhraseValue(String[] phrase, String[] functionPrec)
	{
		for (String function: functionPrec)
		{
			if (_config.functionWithSideEffectsP(function))
				return Pair.of(Boolean.valueOf(false), null); // can't cache a function with side-effects
		}
		
		String key = getPhraseCacheKey(phrase, functionPrec);
		return Pair.of(Boolean.valueOf(_phraseCache.containsKey(key)), key);
	}
	
	private String getPhraseCacheKey(String[] phrase, String[] functionPrec)
	{
		StringBuilder sbuilder = new StringBuilder(StringUtils.join(phrase, ':'));
		sbuilder.append("+").append(StringUtils.join(functionPrec, ':'));
		return sbuilder.toString(); 
	}
	

}
