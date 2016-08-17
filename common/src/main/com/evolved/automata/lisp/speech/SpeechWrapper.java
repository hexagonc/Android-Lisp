package com.evolved.automata.lisp.speech;

import java.util.HashMap;
import java.util.LinkedList;

import org.apache.commons.lang3.StringUtils;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.StringHashtableValue;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.lisp.speech.SpeechConfig.AMBIGUITY_NOTICATION_POLICY;
import com.evolved.automata.lisp.speech.SpeechConfig.PRECEDENCE_ADHERENCE_POLICY;
import com.evolved.automata.lisp.speech.SpeechMap.FUNCTION_APPLICABILITY_STATUS;
import com.evolved.automata.lisp.speech.SpeechMap.FunctionApplicabilityData;

public class SpeechWrapper 
{
	Environment _environment;
	SpeechConfig _internalConfig;
	SpeechMap _sMap;
	String _speechValueToStringFunctionName;
	String _stringArrayScoredValueConversionFunctionName;
	String _tokenizeFunctionName;
	SpeechExternalInterface _speechEvaluator;
	FunctionTemplate _onAmbiguousResultHandler;
	
	final SpeechExternalInterface _topEvaluator = new SpeechExternalInterface()
	{

		@Override
		public ScoredValue evaluate(String name, HashMap<String, ScoredValue> argMap) {
			
			try
			{
				FunctionTemplate template = _environment.getFunction(name);
				Value[] args = new Value[]{convertArgument(argMap)};
				template.setActualParameters(args);
				Value result = template.evaluate(_environment, false);
				if (result.isNull())
					return new ScoredValue(result, 0);
				else
				{
					if (result.hasMetaData())
					{
						Value metaData = result.getMetaData();
						if (metaData.isFloat() && metaData.getFloatValue() <= 1 && metaData.getFloatValue()>=0)
							return new ScoredValue(result, metaData.getFloatValue());
					}
					return new ScoredValue(result, 1);
				}
			}
			catch (Exception e)
			{
				throw new RuntimeException(e);
			}
			
			
		}

		@Override
		public String[] tokenize(String phrase) {
			
			if (_tokenizeFunctionName != null)
			{
				try
				{
					FunctionTemplate template = _environment.getFunction(_tokenizeFunctionName);
					Value[] args = {NLispTools.makeValue(phrase)};
					template.setActualParameters(args);
					Value result = template.evaluate(_environment, false);
					return LispUtilities.convertStringArray(result);
				}
				catch (Exception e)
				{
					throw new RuntimeException(e);
				}
			}
			return StringUtils.split(phrase, ' ');
		}

		/**
		 * Converts a raw value to a string, to be used when converting a pattern and an array of
		 * arguments to its canonical form
		 */
		@Override
		public String toString(ScoredValue value) {
			if (_speechValueToStringFunctionName != null && value.value != null)
			{
				try
				{
					
					FunctionTemplate template = _environment.getFunction(_speechValueToStringFunctionName);
					Value[] args = {(Value)value.value};
					template.setActualParameters(args);
					Value result = template.evaluate(_environment, false);
					return result.getString();
				}
				catch (Exception e)
				{
					throw new RuntimeException(e);
				}
			}
			return convertScoredValue(value).toString();
			
		}

		@Override
		public FunctionApplicabilityData onAmbiguousResult(FunctionApplicabilityData[] possibilities, boolean isTop) {
			if (_onAmbiguousResultHandler != null)
			{
				Value[] options = new Value[possibilities.length];
				for (int i = 0;i<possibilities.length;i++)
				{
					options[i] = convertFunctionApplicabilityData(possibilities[i]);
				}
				
				Value[] args = {NLispTools.makeValue(options), NLispTools.makeValue(isTop)};
				_onAmbiguousResultHandler.setActualParameters(args);
				try
				{
					return convertToFunctionApplicabilityData(_onAmbiguousResultHandler.evaluate(_environment, false));
				}
				catch (Exception e)
				{
					throw new RuntimeException();
				}
			}
			else
				return new FunctionApplicabilityData(0, null).setFailureRejected();
		}
		
	};
	
	
	
	
	public SpeechWrapper(Environment env, Value patternSpecMap, Value typeMap, Value defaultFunctionPrecedence, Value parser, Value speechValueToStringFunctionName, Value stringScoredValueConversionFunctionName, Value patternTokenizeFunctionName, Value onAmbiguousResultCallbackLambda)
	{
		_environment = env;
		HashMap<String, LinkedList<String>> patternSpec = LispUtilities.convertSpeechMap(patternSpecMap);
		HashMap<String, LinkedList<String>> typeSpec = LispUtilities.convertSpeechMap(typeMap);
		_internalConfig = new SpeechConfig(patternSpec, typeSpec, LispUtilities.convertStringArray(defaultFunctionPrecedence));
		_internalConfig.setPrecedencePolicy(SpeechConfig.PRECEDENCE_ADHERENCE_POLICY.FULL);
		_internalConfig.setGrammarParser(LispUtilities.convertParser(parser));
		
		if (speechValueToStringFunctionName!=null)
			_speechValueToStringFunctionName = speechValueToStringFunctionName.getString();
		
		if (stringScoredValueConversionFunctionName != null)
			_stringArrayScoredValueConversionFunctionName = stringScoredValueConversionFunctionName.getString();
		
		if (patternTokenizeFunctionName != null)
			_tokenizeFunctionName = patternTokenizeFunctionName.getString();
		_sMap = new SpeechMap(_topEvaluator, _internalConfig);
	}
	
	
	/**
	 * Main function for evaluating a phrase
	 * @param tokenizedInput
	 * @return
	 */
	public Value evaluate(Value tokenizedInput)
	{
		String[] input = LispUtilities.convertStringArray(tokenizedInput);
		ScoredValue result = _sMap.evaluate(input);
		if (result != null && result.score > 0)
			return convertScoredValue(result);
		else
			return Environment.getNull();
	}
	
	// <)o(> <)o(> <)o(> <)o(> <)o(> <)o(> <)o(> <)o(> <)o(> <)o(> <)o(> <)o(> 
	//						Configuration Methods
	// <)o(> <)o(> <)o(> <)o(> <)o(> <)o(> <)o(> <)o(> <)o(> <)o(> <)o(> <)o(>
	
	
	/**
	 * 
	 * @param policy - a string
	 * @return
	 */
	public Value setAmbiguousPhraseNotificationPolicy(Value policyString)
	{
		String value = policyString.getString();
		AMBIGUITY_NOTICATION_POLICY policy = AMBIGUITY_NOTICATION_POLICY.valueOf(value);
		_internalConfig.setAmbiguityNotificationPolicy(policy);
		return ExtendedFunctions.makeValue(this);
	}
	
	public Value setDefaultFunctionPrecedencePolicy(Value policyString)
	{
		String value = policyString.getString();
		PRECEDENCE_ADHERENCE_POLICY policy = PRECEDENCE_ADHERENCE_POLICY.from(value);
		_internalConfig.setPrecedencePolicy(policy);
		return ExtendedFunctions.makeValue(this);
	}
	
	/**
	 * 
	 * @return
	 */
	public Value addFunctionsWithSideEffects(Value functionNameList)
	{
		String[] names = LispUtilities.convertStringArray(functionNameList);
		for (String name: names)
		{
			_internalConfig.addFunctionWithSideEffects(name);
		}
		return ExtendedFunctions.makeValue(this);
	}
	
	/**
	 * 
	 * @return
	 */
	public Value addIntrinsicallyAmbiguousFunctions(Value functionNameList)
	{
		String[] names = LispUtilities.convertStringArray(functionNameList);
		for (String name: names)
		{
			_internalConfig.addIntrinsicallyAmbiguousFunction(name);
		}
		return ExtendedFunctions.makeValue(this);
	}
	
	/**
	 * 
	 * @return
	 */
	public Value setAmbiguityThresholdFraction(Value fractionValue)
	{
		double fraction = fractionValue.getFloatValue();
		_internalConfig.setAmbiguityThresholdFraction(fraction);
		return ExtendedFunctions.makeValue(this);
	}
	
	
	// .:OO:..:OO:..:OO:..:OO:..:OO:..:OO:..:OO:..:OO:..:OO:..:OO:..:OO:..:OO:.
	//					Helper Functions
	// .:OO:..:OO:..:OO:..:OO:..:OO:..:OO:..:OO:..:OO:..:OO:..:OO:..:OO:..:OO:.
	
	Value convertArgument(HashMap<String, ScoredValue> argMap)
	{
		HashMap<String, Value> map = new HashMap<String, Value>();
		for (String key:argMap.keySet())
		{
			map.put(key, convertScoredValue(argMap.get(key)));
		}
		
		return new StringHashtableValue(map);
	}
	
	/**
	 * Simple implementation of converting a scored value to a lisp Value
	 * Currently ignores the score (may have to change that)
	 * @param value
	 * @return
	 */
	Value convertScoredValue(ScoredValue value)
	{
		if (value == null)
			return Environment.getNull();
		// This is the value that receives the canonical form of the speech function
		// this value will be mapped to 
		if (value.simpleString != null)
			return NLispTools.makeValue(value.simpleString);
		
		if (value.value != null)
		{
			return (Value)value.value;
		}
		else if (value.rawString!=null)
		{
			Value[] out = new Value[value.rawString.length];
			for (int i = 0;i<out.length;i++)
			{
				out[i] = NLispTools.makeValue(value.rawString[i]);
			}
			return convertRawStringTokens( NLispTools.makeValue(out));
		}
		else if (value.rawTokens != null)
		{
			Value[] out = new Value[value.rawTokens.size()];
			int i = 0;
			for (String token:value.rawTokens)
			{
				out[i++] = NLispTools.makeValue(token);
			}
			return convertRawStringTokens( NLispTools.makeValue(out));
		}
		else
			return Environment.getNull();
			
	}
	
	/**
	 * Converts the string array from a ScoredValue representing an unstructured group into a Value to used
 	 * for the speech functions
	 * @param string array from an unstructured text group
	 * @return
	 */
	Value convertRawStringTokens(Value stringArray)
	{
		try
		{
			if (_stringArrayScoredValueConversionFunctionName!= null)
			{
				FunctionTemplate converterFunction = _environment.getFunction(_stringArrayScoredValueConversionFunctionName);
				if (converterFunction != null)
				{
					converterFunction.setActualParameters(new Value[]{stringArray});
					return converterFunction.evaluate(_environment, false);
				}
			}
		}
		catch (Exception e)
		{
			throw new RuntimeException(e);
		}
		return stringArray;
		
	}
	
	
	/**
	 * FunctionApplicabilityData gets converted into a list:
	 * (score, arg-map, function-name, pattern, scored-value, canonical-phrase)
	 * @param data
	 * @return
	 */
	Value convertFunctionApplicabilityData(FunctionApplicabilityData data)
	{
		Value[] result = new Value[6];
		result[0] = NLispTools.makeValue(data.score);
		HashMap<String, Value> map = new HashMap<String, Value>();
		HashMap<String, ScoredValue> argMap = data.argMap;
		for (String key: argMap.keySet())
		{
			map.put(key, convertScoredValue(argMap.get(key)));
		}
		result[1] = new StringHashtableValue(map);
		result[2] = NLispTools.makeValue(data.functionName);
		result[3] = LispUtilities.convertStringArray(data.pattern);
		result[4] = convertScoredValue(data.resultValue);
		result[5] = map.get(_internalConfig.getCanonicalPhraseArgKeyName());
		result[6] = NLispTools.makeValue(data.status.toString());
		return NLispTools.makeValue(result);
	}
	
	
	/**
	 * Input is a list formated as: 
	 * (score, arg-map, function-name, pattern, scored-value, canonical-phrase, status)
	 * @param lispData
	 * @return
	 */
	FunctionApplicabilityData convertToFunctionApplicabilityData(Value lispData)
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
				argMap.put(key, new ScoredValue(argValue, 1));
			else
				argMap.put(key, new ScoredValue(null, 0));
		}
		
		
		String functionName = arguments[2].getString();
		String[] pattern = LispUtilities.convertStringArray(arguments[3]);
		// TODO: handle restoring proper scores of scored values
		ScoredValue cachedResult = (arguments[4].isNull())?null:(new ScoredValue(arguments[4], 1));
		Value statusString = arguments[6];
		FunctionApplicabilityData data = new FunctionApplicabilityData(score, argMap);
		data.functionName = functionName;
		data.pattern = pattern;
		data.resultValue = cachedResult;
		data.status = FUNCTION_APPLICABILITY_STATUS.valueOf(statusString.getString());
		return data;
	}
	
}
