package com.evolved.automata.lisp.speech;

import java.util.HashMap;
import java.util.LinkedList;

import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;
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
	
}
