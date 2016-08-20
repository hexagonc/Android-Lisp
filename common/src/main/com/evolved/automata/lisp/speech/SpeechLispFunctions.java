package com.evolved.automata.lisp.speech;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;

public class SpeechLispFunctions 
{
	public static Environment addSpeechFunctions(Environment env)
	{
		env.mapFunction("create-speech-wrapper", create_speech_wrapper());
		env.mapFunction("evaluate-speech-fast", evaluate_speech_fast());
		env.mapFunction("add-functions-with-side-effects", add_functions_with_side_effects());
		env.mapFunction("set-default-function-prec-policy", set_default_prec_policy());
		env.mapFunction("set-ambiguous-result-policy", set_ambiguous_result_policy());
		env.mapFunction("add-intrinsically-ambig-function", add_intrinsically_ambig_function());
		env.mapFunction("set-ambiguity-threshold", set_ambiguity_threshold());
		env.mapFunction("update-function-precedence", update_function_precedence());
		env.mapFunction("set-type-prec-policy", set_type_prec_policy()); 
		env.mapFunction("clear-speech-sub-cache", clear_speech_cache_subtype());
		env.mapFunction("flex-evaluate", flex_evaluate());
		return env;
	}
	
	private static SimpleFunctionTemplate create_speech_wrapper()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)create_speech_wrapper();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(4, true, true);
				Value speechMap = evaluatedArgs[0];
				Value typeMap = evaluatedArgs[1];
				Value defaultPrecedence = evaluatedArgs[2];
				Value parser = evaluatedArgs[3];
				Value speechFunctionResultToStringConverterFunctionName = null;
				Value stringArraySpeechArgumentConverterFunctionName = null;
				Value patternStringTokenizeFunctionName = null;
				Value ambiguityHandlerLambda = null;
				if (evaluatedArgs.length > 4)
				{
					speechFunctionResultToStringConverterFunctionName = evaluatedArgs[4];
					if (evaluatedArgs.length > 5 && !evaluatedArgs[5].isNull())
					{
						stringArraySpeechArgumentConverterFunctionName = evaluatedArgs[5];
					}
					if (evaluatedArgs.length > 6 && !evaluatedArgs[6].isNull())
					{
						patternStringTokenizeFunctionName = evaluatedArgs[6];
					}
					
					if (evaluatedArgs.length > 7 && !evaluatedArgs[7].isNull())
					{
						ambiguityHandlerLambda = evaluatedArgs[7];
					}
				}
				
				SpeechWrapper wrapper = new SpeechWrapper(
						env, 
						speechMap, 
						typeMap, 
						defaultPrecedence, 
						parser,
						speechFunctionResultToStringConverterFunctionName,
						stringArraySpeechArgumentConverterFunctionName,
						patternStringTokenizeFunctionName,
						ambiguityHandlerLambda
						);
				
				return ExtendedFunctions.makeValue(wrapper);
				
			}
		};
	}
	
	private static SimpleFunctionTemplate evaluate_speech_fast()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)evaluate_speech_fast();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
				return wrapper.evaluate(evaluatedArgs[1]);
				
				
			}
		};
	}
	
	
	private static SimpleFunctionTemplate clear_speech_cache_subtype()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			// first argument is the speech wrapper, second argument is the cache subtype name
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)clear_speech_cache_subtype();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
				return wrapper.clearCache(evaluatedArgs[1]);
				
				
			}
		};
	}
	
	private static SimpleFunctionTemplate flex_evaluate()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			// first argument is the speech wrapper
			// second argument is the tokenized input
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)flex_evaluate();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
				return wrapper.flexEvaluate(evaluatedArgs[1]);
				
				
			}
		};
	}
	
	
	private static SimpleFunctionTemplate add_functions_with_side_effects()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)add_functions_with_side_effects();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
				return wrapper.addFunctionsWithSideEffects(evaluatedArgs[1]);
				
				
			}
		};
	}
	
	
	private static SimpleFunctionTemplate update_function_precedence()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)update_function_precedence();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
				return wrapper.updateDefaultFunctionPrecedence(evaluatedArgs[1]);
				
				
			}
		};
	}
	
	
	private static SimpleFunctionTemplate add_intrinsically_ambig_function()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)add_intrinsically_ambig_function();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
				wrapper.addIntrinsicallyAmbiguousFunctions(evaluatedArgs[1]);
				return evaluatedArgs[0];
				
				
			}
		};
	}
	
	private static SimpleFunctionTemplate set_default_prec_policy()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)set_default_prec_policy();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
				return wrapper.setDefaultFunctionPrecedencePolicy(evaluatedArgs[1]);
				
				
			}
		};
	}
	
	private static SimpleFunctionTemplate set_type_prec_policy()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)set_type_prec_policy();
			}
			
			
			// First argument is the speech wrappert
			// Second argument is the type name
			// Third argument is the string form of the precedence policy
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(3, false, true);
				
				SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
				return wrapper.setTypePrecedencePolicy(evaluatedArgs[1], evaluatedArgs[2]);
				
				
			}
		};
	}
	
	
	private static SimpleFunctionTemplate set_ambiguous_result_policy()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)set_ambiguous_result_policy();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
				return wrapper.setAmbiguousPhraseNotificationPolicy(evaluatedArgs[1]);
				
				
			}
		};
	}
	
	
	private static SimpleFunctionTemplate set_ambiguity_threshold()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)set_ambiguity_threshold();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
				return wrapper.setAmbiguityThresholdFraction(evaluatedArgs[1]);
				
				
			}
		};
	}
	
}
