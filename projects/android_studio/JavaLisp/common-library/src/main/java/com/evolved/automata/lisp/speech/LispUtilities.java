package com.evolved.automata.lisp.speech;

import java.util.HashMap;
import java.util.LinkedList;

import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.StringHashtableValue;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.lisp.speech.SpeechMap.FUNCTION_APPLICABILITY_STATUS;
import com.evolved.automata.lisp.speech.SpeechMap.FunctionApplicabilityData;
import com.evolved.automata.parser.general.PatternParser;

public class LispUtilities {

	public static HashMap<String, LinkedList<String>> convertSpeechMap(Value value)
	{
		HashMap<String, Value> baseMap = value.getStringHashtable();
		HashMap<String, LinkedList<String>> output = new HashMap<String, LinkedList<String>>();
		LinkedList<String> variants;
		Value[] x;
		
		for (String key:baseMap.keySet())
		{
			variants = new LinkedList<String>();
			x = (baseMap.get(key)).getList();
			for (Value v:x)
			{
				variants.add(v.getString());
			}
			output.put(key, variants);
		}
		return output;
	}
	
	public static String[] convertStringArray(Value value)
	{
		Value[] v = value.getList();
		String[] out = new String[v.length];
		int i = 0;
		for (i = 0;i<v.length;i++)
			out[i] = v[i].getString();
		return out;
	}
	
	public static Value convertStringArray(String[] value)
	{
		Value[] v = new Value[value.length];
		
		int i = 0;
		for (i = 0;i<v.length;i++)
			v[i] = NLispTools.makeValue(value[i]);
		return NLispTools.makeValue(v);
	}
	
	
	public static PatternParser convertParser(Value value)
	{
		return (PatternParser)value.getObjectValue();
	}
	
	public static SpeechConfig.PRECEDENCE_ADHERENCE_POLICY getFunctionPrecedenceAdherencePolicy(Value adherenceString)
	{
		return SpeechConfig.PRECEDENCE_ADHERENCE_POLICY.from(adherenceString.getString());
	}
	
	public static SpeechConfig.AMBIGUITY_NOTICATION_POLICY getAmbiguityNotificationPolicy(Value policyString)
	{
		return SpeechConfig.AMBIGUITY_NOTICATION_POLICY.valueOf(policyString.getString());
	}
	
	public static SpeechConfig.BOUNDING_PATTERN_EVALUATION_STRATEGY getBoundaryPatternEvaluationStrategy(Value strategyString)
	{
		return SpeechConfig.BOUNDING_PATTERN_EVALUATION_STRATEGY.valueOf(strategyString.getString());
	}
	
	//TODO: Need to really come up with a good way to recover the score.  Most of the time there will not be meta-data
	public static ScoredValue convertToScoredValue(Value value)
	{
		double score = 1;
		if (value.hasMetaData())
		{
			Value meta = value.getMetaData();
			score = meta.getFloatValue();
		}
		if (value.isString())
			return ScoredValue.from(value.getString()).setScore(score);
		else if (value.isNull())
			return new ScoredValue(null, 0);
		else
			return new ScoredValue(value, score);
	}
	
	/**
	 * Input is a list formated as: 
	 * (score, arg-map, function-name, pattern, scored-value, canonical-phrase, status)
	 * @param lispData
	 * @return
	 */
	public static FunctionApplicabilityData convertToFunctionApplicabilityData(SpeechConfig config, Value lispData)
	{
		Value[] arguments = lispData.getList();
		
		double score = arguments[0].getFloatValue();
		HashMap<String, Value> map = arguments[1].getStringHashtable();
		HashMap<String, ScoredValue> argMap = new HashMap<String, ScoredValue>();
		Value argValue;
		for (String key: map.keySet())
		{
			argValue = map.get(key);
			if (!argValue.isNull())
			{
				if (key.equals(config.getCanonicalPhraseArgKeyName()))
					argMap.put(key, ScoredValue.from(argValue.getString()));
				else
					argMap.put(key, new ScoredValue(argValue, 1));
				
			}
			else
				argMap.put(key, new ScoredValue(null, 0));
		}
		
		
		String functionName = arguments[2].getString();
		String[] pattern = LispUtilities.convertStringArray(arguments[3]);
		// TODO: handle restoring proper scores of scored values, although this is less important since this value was explicitly chosen by the user
		ScoredValue cachedResult = convertToScoredValue(arguments[4]);
		Value statusString = arguments[6];
		FunctionApplicabilityData data = new FunctionApplicabilityData(score, argMap);
		data.functionName = functionName;
		data.pattern = pattern;
		data.resultValue = cachedResult;
		data.status = FUNCTION_APPLICABILITY_STATUS.valueOf(statusString.getString());
		return data;
	}
	
	
}
