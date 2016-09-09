package com.evolved.automata.lisp.speech;

import java.util.HashMap;
import java.util.LinkedList;

import com.evolved.automata.lisp.Value;
import com.evolved.automata.parser.general.PatternParser;

public class SpeechConfigLispWrapper 
{
	
	SpeechConfig _internalConfig;
	public SpeechConfigLispWrapper(Value patternSpecMap, Value typeMap, Value defaultFunctionPrec, Value parser)
	{
		HashMap<String, LinkedList<String>> patternSpec = LispUtilities.convertSpeechMap(patternSpecMap);
		HashMap<String, LinkedList<String>> typeSpec = LispUtilities.convertSpeechMap(typeMap);
		_internalConfig = new SpeechConfig(patternSpec, typeSpec, LispUtilities.convertStringArray(defaultFunctionPrec));
		_internalConfig.setPrecedencePolicy(SpeechConfig.PRECEDENCE_ADHERENCE_POLICY.FULL);
		_internalConfig.setGrammarParser(LispUtilities.convertParser(parser));
	}
	
	public SpeechConfig getConfig()
	{
		return _internalConfig;
	}
}
