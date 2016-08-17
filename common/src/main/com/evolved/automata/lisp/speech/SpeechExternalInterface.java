package com.evolved.automata.lisp.speech;

import java.util.HashMap;

import com.evolved.automata.lisp.speech.SpeechMap.FunctionApplicabilityData;

public interface SpeechExternalInterface {
	public ScoredValue evaluate(String functionName, HashMap<String, ScoredValue> argMap);
	public String[] tokenize(String phrase);
	public String toString(ScoredValue value);
	public FunctionApplicabilityData onAmbiguousResult(FunctionApplicabilityData[] possibilities, boolean isTop);
}
