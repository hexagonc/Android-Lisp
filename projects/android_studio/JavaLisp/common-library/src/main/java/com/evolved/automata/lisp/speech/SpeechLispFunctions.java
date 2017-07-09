package com.evolved.automata.lisp.speech;

import java.util.HashMap;
import java.util.LinkedList;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.Lambda;
import com.evolved.automata.lisp.LambdaValue;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.StringHashtableValue;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.lisp.speech.SpeechMap.FunctionApplicabilityData;


public class SpeechLispFunctions 
{
	public static Environment addSpeechFunctions(Environment env)
	{
		env.mapFunction("create-speech-wrapper", create_speech_wrapper());
		env.mapFunction("evaluate-speech-fast", evaluate_speech_fast());
		env.mapFunction("add-functions-with-side-effects", add_functions_with_side_effects());
		env.mapFunction("set-default-function-prec-policy", set_default_prec_policy());
		env.mapFunction("set-ambiguous-result-policy", set_ambiguous_result_policy());
		env.mapFunction("add-intrinsically-ambig-functions", add_intrinsically_ambig_functions());
		env.mapFunction("set-ambiguity-threshold", set_ambiguity_threshold());
		env.mapFunction("update-function-precedence", update_function_precedence());
		env.mapFunction("set-type-prec-policy", set_type_prec_policy()); 
		env.mapFunction("clear-speech-sub-cache", clear_speech_cache_subtype());
		env.mapFunction("flex-evaluate", flex_evaluate());

        env.mapFunction("enable-experimental-optimizations", enable_experimental_optimizations());
        env.mapFunction("disable-experimental-optimizations", disable_experimental_optimizations());


		// 
		env.mapFunction("get-pattern-index-cache", get_pattern_index_cache());
		env.mapFunction("get-function-list-cache", get_function_list_cache());
		env.mapFunction("get-function-result-cache", get_function_result_cache());
		env.mapFunction("get-pattern-assessment-cache", get_pattern_assessment_cache());
		
		
		env.mapFunction("set-pattern-index-cache", set_pattern_index_cache());
		env.mapFunction("set-function-list-cache", set_function_list_cache());
		env.mapFunction("set-function-result-cache", set_function_result_cache());
		env.mapFunction("set-pattern-assessment-cache", set_pattern_assessment_cache());


        env.mapFunction("load-default-verb-metadata", load_default_verb_metadata());
        env.mapFunction("get-verb-conjugations", get_verb_conjugations());
        env.mapFunction("get-verb-tenses", get_verb_tenses());
		
		
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
				Environment targetEnv = env;
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

                    if (evaluatedArgs.length > 7 && evaluatedArgs[7].isLambda())
                    {
                        targetEnv = ((Lambda)(evaluatedArgs[7]).getLambda()).getInnerEnvironment();
                    }

					if (evaluatedArgs.length > 8 && !evaluatedArgs[8].isNull())
					{
						ambiguityHandlerLambda = evaluatedArgs[8];
					}


				}
				
				SpeechWrapper wrapper = new SpeechWrapper(
						targetEnv,
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

    private static SimpleFunctionTemplate load_default_verb_metadata()
    {
        return new SimpleFunctionTemplate()
        {

            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)load_default_verb_metadata();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                try
                {
                    WordMetadata.loadDefaultVerbConjugations();
                    return NLispTools.makeValue(1);
                }
                catch (Exception e)
                {
                    return Environment.getNull();
                }
            }
        };
    }


    private static SimpleFunctionTemplate get_verb_conjugations()
    {
        return new SimpleFunctionTemplate()
        {

            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)get_verb_conjugations();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, false, true);

                String verb = evaluatedArgs[0].getString();

                HashMap<WordMetadata.VerbTense, String> map = WordMetadata.getConjugations(verb);

                if (map != null)
                {
                    HashMap<String, Value> resultMap = new HashMap<String, Value>();
                    for (WordMetadata.VerbTense key:map.keySet())
                    {
                        String conjugation = map.get(key);
                        resultMap.put(key.name(), NLispTools.makeValue(conjugation));
                    }
                    return new StringHashtableValue(resultMap);
                }
                else
                    return Environment.getNull();

            }
        };
    }

    private static SimpleFunctionTemplate get_verb_tenses()
    {
        return new SimpleFunctionTemplate()
        {

            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)get_verb_tenses();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, false, true);

                String verb = evaluatedArgs[0].getString();

                LinkedList<WordMetadata.VerbTense> tenses = WordMetadata.getVerbTense(verb);

                HashMap<String, Value> resultSet = new HashMap<String, Value>();


                for (WordMetadata.VerbTense tense: tenses)
                {
                    resultSet.put(tense.name(), NLispTools.makeValue(1));

                }
                return new StringHashtableValue(resultSet);
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

    private static SimpleFunctionTemplate enable_experimental_optimizations()
    {
        return new SimpleFunctionTemplate()
        {

            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)enable_experimental_optimizations();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, false, true);

                SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
                wrapper.enableExperimentalFeatures(true);
                return evaluatedArgs[0];


            }
        };
    }

    private static SimpleFunctionTemplate disable_experimental_optimizations()
    {
        return new SimpleFunctionTemplate()
        {

            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)disable_experimental_optimizations();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, false, true);

                SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
                wrapper.enableExperimentalFeatures(false);
                return evaluatedArgs[0];


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
	
	
	private static SimpleFunctionTemplate add_intrinsically_ambig_functions()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)add_intrinsically_ambig_functions();
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
	
	
	
	
	private static SimpleFunctionTemplate get_pattern_assessment_cache()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)get_pattern_assessment_cache();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
				SpeechCache cache = wrapper.getSpeechCache();
				return wrapper.convertPatternAssessmentCache(cache.getPatternAssessmentCache());
				
				
			}
		};
	}
	
	private static SimpleFunctionTemplate get_function_result_cache()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)get_function_result_cache();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
				SpeechCache cache = wrapper.getSpeechCache();
				return wrapper.convertFunctionResultCache(cache.getFunctionResultCache());
				
				
			}
		};
	}
	
	private static SimpleFunctionTemplate get_function_list_cache()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)get_function_list_cache();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
				SpeechCache cache = wrapper.getSpeechCache();
				return wrapper.convertFunctionListCache(cache.getFunctionListResultCache());
				
				
			}
		};
	}
	
	
	private static SimpleFunctionTemplate get_pattern_index_cache()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)get_pattern_index_cache();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
				SpeechCache cache = wrapper.getSpeechCache();
				return wrapper.convertIndexedPatternCache(cache.getIndexedPatternCache());
				
				
			}
		};
	}
	
	
	// next
	
	private static SimpleFunctionTemplate set_pattern_assessment_cache()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)set_pattern_assessment_cache();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
				SpeechCache cache = wrapper.getSpeechCache();
				
				HashMap<String, FunctionApplicabilityData> subCache = wrapper.convertPatternAssessmentCache(evaluatedArgs[1]);
				cache.setPatternAssessmentCache(subCache);
				return evaluatedArgs[0];
			}
		};
	}
	
	private static SimpleFunctionTemplate set_function_result_cache()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)set_function_result_cache();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
				SpeechCache cache = wrapper.getSpeechCache();
				
				HashMap<String, FunctionApplicabilityData> subCache = wrapper.convertFunctionResultCache(evaluatedArgs[1]);
				cache.setFunctionResultCache(subCache);
				
				return evaluatedArgs[0];
			}
		};
	}
	
	private static SimpleFunctionTemplate set_function_list_cache()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)set_function_list_cache();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
				SpeechCache cache = wrapper.getSpeechCache();
				
				HashMap<String, ScoredValue> subCache = wrapper.convertFunctionListCache(evaluatedArgs[1]);
				cache.setFunctionListResultCache(subCache);
				return evaluatedArgs[0];
				
				
			}
		};
	}
	
	
	private static SimpleFunctionTemplate set_pattern_index_cache()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)set_pattern_index_cache();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, true);
				
				SpeechWrapper wrapper = (SpeechWrapper)evaluatedArgs[0].getObjectValue();
				SpeechCache cache = wrapper.getSpeechCache();
				cache.setIndexedPatternCache(wrapper.convertIndexedPatternCache(evaluatedArgs[1]));
				return evaluatedArgs[0];
				
			}
		};
	}
}
