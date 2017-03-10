package com.evolved.automata.lisp.speech;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;

import com.evolved.automata.Metaphone;
import com.evolved.automata.parser.general.PatternParser;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

public class SpeechConfig {
	
	public static enum PATTERN_WIDTH_SEARCH_METHOD
	{
		LINEAR,
		HEURISTIC
	}
	
	
	public static enum AMBIGUITY_NOTICATION_POLICY
	{
		NEVER_NOTIFY,
		NOTIFY_ONLY_AT_TOP_LEVEL,
		ALWAYS_NOTIFY
	}
	
	public static enum PRECEDENCE_ADHERENCE_POLICY
	{
		FULL,
		PARTIAL,
		NONE;
		static HashMap<String, PRECEDENCE_ADHERENCE_POLICY> typeMap = new HashMap<String, PRECEDENCE_ADHERENCE_POLICY>(){
			{
				this.put("FULL", FULL);
				this.put("full", FULL);
				this.put("PARTIAL", PARTIAL);
				this.put("partial", PARTIAL);
				this.put("NONE", NONE);
				this.put("none", NONE);
			}
		};
		
		static PRECEDENCE_ADHERENCE_POLICY from(String name)
		{
			return typeMap.get(name);
		}
	}
	
	public static enum BOUNDING_PATTERN_EVALUATION_STRATEGY
	{
		USE_INITIAL_SKIPS,
		TEST_EACH_TOKEN,
		ACCEPT_FIRST_RESULT // only used
	}
	
	String[] _defaultFunctionPrecedence;
	PatternParser _simpleGrammarParser;
	HashMap<String, LinkedList<String>> _patternSpecificationMap;
	HashMap<String, LinkedList<String>> _patternTypeMap;
	HashMap<String, PRECEDENCE_ADHERENCE_POLICY> _defaultTypePrecedencePolicyMap;
	
	AMBIGUITY_NOTICATION_POLICY _ambiguityPolicy;
	HashSet<String> _intrinsicallyAmbiguousFunctions;
	HashSet<String> _functionsWithSideEffects;
	boolean _useFastLiteralMatching;
	PRECEDENCE_ADHERENCE_POLICY precedencePolicy;
	double _minScoreThreshold = 0.15;
	double _resultSignificanceThreshold = 0.01;
	private String _canonicalPhraseArgumentKey = "*canonical*";
	double _ambiguityThresholdFraction = 0.8;
    HashMap<String, LinkedList<String>> canonicalPatterns;
	PATTERN_WIDTH_SEARCH_METHOD _patternWidthSearchMethod = PATTERN_WIDTH_SEARCH_METHOD.LINEAR;
	
	BOUNDING_PATTERN_EVALUATION_STRATEGY _boundingPatternEvaluationStrategy = BOUNDING_PATTERN_EVALUATION_STRATEGY.ACCEPT_FIRST_RESULT;
	boolean _useMetaphoneP = false;
	
	public SpeechConfig(HashMap<String, LinkedList<String>> patternSpec, HashMap<String, LinkedList<String>> typeSpec, String[] defaultPrecedence)
	{
		_defaultTypePrecedencePolicyMap = new HashMap<String, SpeechConfig.PRECEDENCE_ADHERENCE_POLICY>();
		_patternSpecificationMap = patternSpec;
        canonicalPatterns = patternSpec;
		_patternTypeMap = typeSpec;
		_defaultFunctionPrecedence = defaultPrecedence;
		_functionsWithSideEffects = new HashSet<String>();
		_intrinsicallyAmbiguousFunctions = new HashSet<String>(); 
				 
	}

    private void convertPatternSpecToMetaPhone()
    {
        HashMap<String, LinkedList<String>> patternSpec = new HashMap<String, LinkedList<String>>();

        for (String functionName:_patternSpecificationMap.keySet())
        {
            LinkedList<String> converted = new LinkedList<String>(), original = _patternSpecificationMap.get(functionName);
            for (String pattern:original)
            {
                String[] tokens = StringUtils.split(pattern, ' ');
                StringBuilder recombined = new StringBuilder();
                for (String token: tokens)
                {
                    if (recombined.length() > 0)
                    {
                        recombined.append(" ");
                    }
                    Metaphone phone = new Metaphone(token);
                    Pair<String, String> meta = phone.getMetaphone();
                    String first = meta.getLeft();
                    if (first.length() == 0 || SpeechMap.isGroup(token) != null || SpeechMap.isNonTerminal(token) != null)
                    {
                        recombined.append(token);
                    }
                    else
                    {

                        recombined.append(first);
                    }
                }
                converted.add(recombined.toString());
            }
            patternSpec.put(functionName, converted);
        }
        canonicalPatterns = _patternSpecificationMap;
        _patternSpecificationMap = patternSpec;
    }



	public SpeechConfig setUseMetaphoneMatching(boolean useMetaphone)
	{

		_useMetaphoneP = useMetaphone;
        if (useMetaphone)
        {
            convertPatternSpecToMetaPhone();
            if (_simpleGrammarParser != null)
            {
                _simpleGrammarParser.convertToMetaphone();
            }
        }
		return this;
	}

	public boolean usesMetaphoneMatchingP()
	{
		return _useMetaphoneP;
	}

	public void setDefaultTypePrecedencePolicy(String typename, PRECEDENCE_ADHERENCE_POLICY policy)
	{
		_defaultTypePrecedencePolicyMap.put(typename, policy);
	}


	
	
	public PRECEDENCE_ADHERENCE_POLICY getTypePrecedencePolicy(String typename)
	{
		PRECEDENCE_ADHERENCE_POLICY policy = _defaultTypePrecedencePolicyMap.get(typename);
		if (policy == null)
		{
			return precedencePolicy;
		}
		else
			return policy;
	}
	
	public SpeechConfig addFunctionWithSideEffects(String functionName){
		_functionsWithSideEffects.add(functionName);
		return this;
	}
	
	
	
	
	public SpeechConfig addIntrinsicallyAmbiguousFunction(String functionName){
		_intrinsicallyAmbiguousFunctions.add(functionName);
		return this;
	}
	
	public boolean  isIntrinsicallyAmbiguousFunction(String functionName){
		
		return _intrinsicallyAmbiguousFunctions.contains(functionName);
	}
	
	public SpeechConfig setAmbiguityThresholdFraction(double threshold)
	{
		_ambiguityThresholdFraction = threshold;
		return this;
	}
	
	
	public double getAmbiguityThresholdFraction()
	{
		return _ambiguityThresholdFraction;
	}
	
	public boolean functionWithSideEffectsP(String functionName)
	{
		return _functionsWithSideEffects.contains(functionName);
	}
	
	public SpeechConfig setPatternWidthSearchPolicy(PATTERN_WIDTH_SEARCH_METHOD strategy)
	
	{
		_patternWidthSearchMethod = strategy;
		return this;
	}
	
	public PATTERN_WIDTH_SEARCH_METHOD getPatternWidthSearchPolicy()
	{
		return _patternWidthSearchMethod;
	}
	
	
	public BOUNDING_PATTERN_EVALUATION_STRATEGY getBoundingPatternEvaluationStrategy()
	{
		return _boundingPatternEvaluationStrategy;
	}
	
	public SpeechConfig setBoundingPatternEvaluationPolicy(BOUNDING_PATTERN_EVALUATION_STRATEGY strategy)
	{
		_boundingPatternEvaluationStrategy = strategy;
		return this;
	}
	
	public String getCanonicalPhraseArgKeyName()
	{
		return _canonicalPhraseArgumentKey;
	}
	
	public SpeechConfig setCanonicalPhraseArgKeyName(String key)
	{
		_canonicalPhraseArgumentKey = key;
		return this;
	}
	
	
	
	public SpeechConfig setTypeMap(HashMap<String, LinkedList<String>> map)
	{
		_patternTypeMap = map;
		return this;
	}
	
	public HashMap<String, LinkedList<String>> getTypeMap()
	{
		return _patternTypeMap;
	}
	
	public SpeechConfig setGrammarParser(PatternParser parser)
	{
		_simpleGrammarParser = parser;
		return this;
	}
	
	public PatternParser getGrammarParser()
	{
		return _simpleGrammarParser;
	}
	
	public SpeechConfig setCanonicalPhraseArgumentKey(double s)
	{
		_resultSignificanceThreshold = s;
		return this;
	}
	
	public double getMinimumResultScoreThreshold()
	{
		return _resultSignificanceThreshold;
	}
	
	public SpeechConfig setCanonicalPhraseArgumentKey(String name)
	{
		_canonicalPhraseArgumentKey = name;
		return this;
	}
	
	public String getCanonicalPhraseArgumentKey()
	{
		return _canonicalPhraseArgumentKey;
	}
	public SpeechConfig setMinimumPatternScoreThreshold(double min)
	{
		_minScoreThreshold = min;
		return this;
	}
	
	
	public double getMinimumPatternScoreThreshold()
	{
		return _minScoreThreshold;
	}
	
	public HashMap<String, LinkedList<String>> getPatternSpecMap()
	{
		return _patternSpecificationMap;
	}
	
	public SpeechConfig setPatternSpecMap(HashMap<String, LinkedList<String>> spec)
	{
		_patternSpecificationMap = spec;
		return this;
	}
	
	public String[] getDefaultFunctionPrecedence()
	{
		return _defaultFunctionPrecedence;
	}
	
	public SpeechConfig setDefaultFunctionPrecedence(String[] prec)
	{
		_defaultFunctionPrecedence = prec;
		return this;
	}
	
	public PRECEDENCE_ADHERENCE_POLICY getPrecedencePolicy()
	{
		return precedencePolicy;
	}
	
	public SpeechConfig setPrecedencePolicy(PRECEDENCE_ADHERENCE_POLICY policy)
	{
		precedencePolicy = policy;
		return this;
	}
	
	public AMBIGUITY_NOTICATION_POLICY getAmbiguityNotificationPolicy()
	{
		return _ambiguityPolicy;
	}
	
	public SpeechConfig setAmbiguityNotificationPolicy(AMBIGUITY_NOTICATION_POLICY policy)
	{
		_ambiguityPolicy = policy;
		return this;
	}
}
