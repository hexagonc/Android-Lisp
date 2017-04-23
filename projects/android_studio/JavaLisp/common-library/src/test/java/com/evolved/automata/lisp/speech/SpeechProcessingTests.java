package com.evolved.automata.lisp.speech;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;

import javax.swing.plaf.ListUI;

import org.junit.Assert;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.Before;
import org.junit.Test;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.StringHashtableValue;
import com.evolved.automata.lisp.TestHarnessBase;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.lisp.speech.SpeechMap.FUNCTION_APPLICABILITY_STATUS;
import com.evolved.automata.lisp.speech.SpeechMap.FunctionApplicabilityData;
import com.evolved.automata.lisp.speech.SpeechMap.ProcessStateInfo;
import com.evolved.automata.parser.general.PatternParser;

public class SpeechProcessingTests extends TestHarnessBase
{

	String[] grammar = new String[]{
			"seconds == [second] | [seconds];",
			"sequence_next == [then] | [next];",
			"conjunction == [and];",
			"alternative == [or];",
			"basic_noun == ([mount], [everest]) | ([your], [distance]) | [area] | [room] | [variable] | [region];",
			"modifier == [height] | [speed] | [length] | [width] | [distance] | [time] | [size];",

			"greater_than ==  [is]?, ([more] | [greater]), [than];",
			"equals == [is] | [equals];",
			"less_than == ([smaller] | [less]), [than];",
			"meters == [meter] | [meters];",
			"inches == [inch] | [inches];",
			"centimeters == [centimeters] | [centimeter];",
			"degrees == [degrees] | [degree];",
			"front_sonar ==  ([the] | [your])?, ([forward] | [front])?, ([sonar] | [ultrasonic] | [distance]);",
			"simple_move == [move] | [go];",
			"move_back == [back] | [backwards];",
			"move_forward == [forward] | [forwards] | [forth] | [fourth];",
			"move_left == [left];",
			"move_right == [right];",
			"say == [say] | [repeat];",
			"always == [always];",
			"if == [if] | [when] | [whenever];",
			"repeating-limit == [until];",
			"continuation == [while] | ([so], [long], [as]);",
			"plus == [plus] | [added to];",
			"times == [times] | [multiplied by];",
			"minus == [minus];",
			"divided == ([divided], [by]) | [over];",
			"subtract == [subtracted], [from];",
			"set_equals == [is] | [equals];",
			"to == [to] | [too] | [two] | [as];",
			"for == [for] | [four];",
			"what == [what];",
			"greeting == [hello] | [hi];",
			"user_past_speech == [said];",
			"sequence_delimiter == [next] | [then];",
			"limit == [unless] | [until];",
			"do == [do] | [execute];",
			"repeat == [repeat] | [do];",
			"rotate == [rotate] | [turn] | [spin];",
			"limit_prep == [for] | [by] | [buy] | [four];",
			"travel_distance == [your]?, [distance];",
			"frontal == [forward] | [front];",
			"or == [or];",
			"and == [and];",
			"predicate_not == [not];"};

	String[][] basePatternSpec = new String[][]{
			{"[decimal]"},
			{"[lvalue#number] <plus> [rvalue#number]"},
			{"[lvalue#number] <minus> [rvalue#number]", "[rvalue#number] <subtract> [lvalue#number]"},
			{"[lvalue#number] <times> [rvalue#number]"},
			{"[lvalue#number] <divided> [rvalue#number]"},
			{"[lvalue#number] <equals> [rvalue#number]"},
			{"[lvalue#number] <less_than> [rvalue#number]"},
			{"[lvalue#number] <greater_than> [rvalue#number]"},
			{"set [variable] to [number]", "[variable] is [number]" },
			{"[variable]"}};

	String[] baseFunctions = new String[]{
			"speech-number",
			"speech-add",
			"speech-subtract",
			"speech-times",
			"speech-divide",
			"speech-equals",
			"speech-less",
			"speech-greater",
			"speech-set",
			"speech-variable-get"};

	Value selectedOption;
	int ambiguousOptionCount = 0;

	SpeechConfig config;
	String[] baseTypeNames = new String[]{
			"number",
			"predicate"};


	String[][] baseTypeSpec = new String[][]{
			{ "speech-add", "speech-subtract", "speech-times", "speech-divide", "speech-number", "speech-set", "speech-variable-get"},
			{"speech-equals", "speech-less", "speech-greater" }};

	String ambiguity_handler_function_name = "on-ambiguity-handler";

	@SuppressWarnings("serial")
	HashMap<String, String> baseFunctionDefMap = new HashMap<String, String>(){
		{
			this.put("speech-number", "(defun speech-number (arg-map) 	(if (or (not (setq number 		  		   	   (gethash arg-map \"decimal\"))) 			(not (= 1 (length number)))) 		(return F))  	(signal-block (\"start\") 		((\"start\") (try (integer (first number)) (\"error\"))) 		((\"error\") (return F))))");
			this.put("speech-add", "(defun speech-add (arg-map) 	(multiple-bind (lvalue rvalue) 				   (list (gethash arg-map \"lvalue\") 				   		 (gethash arg-map \"rvalue\"))) 	(+ lvalue 	   rvalue))");
			this.put("speech-subtract", "(defun speech-subtract (arg-map) 	(multiple-bind (lvalue rvalue) 				   (list (gethash arg-map \"lvalue\") 				   		 (gethash arg-map \"rvalue\"))) 	(- lvalue 	   rvalue))");
			this.put("speech-times", "(defun speech-times (arg-map) 	(multiple-bind (lvalue rvalue) 				   (list (gethash arg-map \"lvalue\") 				   		 (gethash arg-map \"rvalue\"))) 	(* lvalue 	   rvalue))");
			this.put("speech-divide", "(defun speech-divide (arg-map) 	(multiple-bind (lvalue rvalue) 				   (list (gethash arg-map \"lvalue\") 				   		 (gethash arg-map \"rvalue\"))) 	(/ lvalue 	   rvalue))");
			this.put("speech-equals", "(defun speech-equals (arg-map) 	(multiple-bind (lvalue rvalue) 				   (list (gethash arg-map \"lvalue\") 				   		 (gethash arg-map \"rvalue\"))) 	(if (equals lvalue 	   			rvalue) 		1 		0))");
			this.put("speech-less", "(defun speech-less (arg-map) 	(multiple-bind (lvalue rvalue) 				   (list (gethash arg-map \"lvalue\") 				   		 (gethash arg-map \"rvalue\"))) 	(if (< lvalue 	   	   rvalue) 		1 		0))");
			this.put("speech-greater", "(defun speech-greater (arg-map) 	(multiple-bind (lvalue rvalue) 				   (list (gethash arg-map \"lvalue\") 				   		 (gethash arg-map \"rvalue\"))) 	(if (> lvalue 	   	   rvalue) 		1 		0)) ");
			this.put("speech-set", "(defun speech-set (arg-map) 	(if (or (not (setq number 		  		   	   (gethash arg-map \"number\"))) 			(not (setq tokenized-var-name 					   (gethash arg-map \"variable\")))) 		(return F))  	(defhash var-map  			 (join tokenized-var-name \" \") 			 number))");
			this.put("speech-variable-get", "(defun speech-variable-get (arg-map) 	(if (not (setq tokenized-var-name 				   (gethash arg-map \"variable\"))) 		(return F))  	(gethash var-map  			 (join tokenized-var-name \" \")))");
		}
	};


	public Value getLispPatternSpec(String[] keys, String[][] values, HashSet<String> filteredFunctions)
	{
		HashMap<String, Value> map = new HashMap<String, Value>();

		int i = 0, length;
		String key;
		Value[] strings;

		for (i = 0;i<keys.length;i++)
		{
			key = keys[i];

			if (!filteredFunctions.contains(key))
				continue;

			length = values[i].length;
			strings = new Value[length];
			for (int j = 0;j<length;j++)
			{
				strings[j] = NLispTools.makeValue(values[i][j]);
			}
			map.put(key, NLispTools.makeValue(strings));
		}

		return new StringHashtableValue(map);
	}

	public Value getLispTypeSpec(String[] keys, String[][] values, HashSet<String> filterFunctions)
	{
		HashMap<String, Value> map = new HashMap<String, Value>();

		int i = 0, length;
		String key, functionName;
		Value[] strings;
		LinkedList<String> filteredInput;
		String[] definedFunctions;

		for (i = 0;i<keys.length;i++)
		{

			key = keys[i];

			filteredInput = new LinkedList<String>();
			definedFunctions = values[i];

			for (int j = 0;j<definedFunctions.length;j++)
			{
				functionName = values[i][j];
				if (filterFunctions.contains(functionName))
					filteredInput.add(functionName);
			}

			if (filteredInput.size() == 0)
				continue;

			length = filteredInput.size();
			strings = new Value[length];
			int j = 0;
			for (String fname:filteredInput)
			{
				strings[j++] = NLispTools.makeValue(fname);
			}
			map.put(key, NLispTools.makeValue(strings));
		}

		return new StringHashtableValue(map);
	}


	HashSet<String> arrayToSet(String[] array)
	{
		HashSet<String> out = new HashSet<String>();
		for (String s:array)
			out.add(s);
		return out;
	}



	Environment env;
	@Before
	public void setupEnvironment() throws InstantiationException, IllegalAccessException
	{
		env = new Environment();
		NLispTools.addDefaultFunctionsAddMacros(env);
		ExtendedFunctions.addExtendedFunctions(env);
		env.mapFunction("create-speech-map", create_speech_map());
		env.mapFunction("create-speech-config", create_speech_config());
		env.mapFunction("evaluate-speech-fast", evaluate_speech_fast());
		env.mapFunction("add-function-wo-side-effects", add_function_wo_side_effects());
		env.mapFunction("on-ambiguity-handler", on_ambiguity_handler());
		PatternParser parser = new PatternParser(grammar );

		env.mapValue("parser", ExtendedFunctions.makeValue(parser));
		env.evaluate("(setq var-map (make-string-hashtable))", true);
		selectedOption = null;
		ambiguousOptionCount = 0;

		for (String function:baseFunctionDefMap.keySet())
		{
			String definition = baseFunctionDefMap.get(function);
			env.evaluate(definition, true);
		}


	}

	// Actual unit tests
	@Test
	public void testNumberEvaluation()
	{
		// Test number evaluation
		HashSet<String> availableFunctions = arrayToSet(new String[]{"speech-number"});
		String[] functionPrec = {"speech-number"};
		String utterance = "10";

		boolean success = false;
		try
		{
			success = false;
			env.mapValue("pattern-spec", getLispPatternSpec(baseFunctions, basePatternSpec, availableFunctions));
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue("Failed to bind lisp pattern-spec", success);


		try
		{
			success = false;
			env.mapValue("type-spec", getLispTypeSpec(baseTypeNames, baseTypeSpec, availableFunctions));
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue("Failed to bind lisp type-spec", success);


		try
		{
			success = false;
			env.mapValue("function-prec", LispUtilities.convertStringArray(functionPrec));
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue("Failed to bind lisp function-prec", success);


		try
		{
			success = false;
			env.mapValue("utterance", LispUtilities.convertStringArray(StringUtils.split(utterance, ' ')));
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue("Failed to bind lisp utterance", success);



		try
		{
			success = false;
			env.evaluate("(setq speech-config (create-speech-config pattern-spec type-spec function-prec parser))", true);
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue("Failed to bind lisp speech-config", success);


		try
		{
			success = false;
			env.evaluate("(setq speech-map (create-speech-map speech-config))", true);
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue("Failed to bind lisp speech-map", success);

		Value expectedResult = NLispTools.makeValue(10);

		Value actualResult = null;

		try
		{
			success = false;
			env.evaluate("(setq result (evaluate-speech-fast speech-map utterance))", true);
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue( "Failed to evaluate speech utterance: '" + utterance + "'", success);


		try
		{
			actualResult = env.getVariableValue("result");
			success = actualResult != null;
		}
		catch (Exception e)
		{
			e.printStackTrace();
			Assert.fail("Failed due to invalid result");
		}
		Assert.assertTrue( "Failed to get valid result for: '" + utterance + "'", success);

		Assert.assertTrue( "Failed to get valid type for: '" + utterance + "', expected integer, got: " + actualResult.getType(),  NLispTools.isNumericType(actualResult));
		Assert.assertTrue( "Incorrect result valued type for: '" + utterance + "', expected integer, got: " + actualResult.getType(), actualResult.getIntValue() == expectedResult.getIntValue());

	}


	private void setupSpeechMap(HashSet<String> availableFunctions, String[] functionPrec)
	{
		setupSpeechMap(availableFunctions, functionPrec, SpeechConfig.AMBIGUITY_NOTICATION_POLICY.NEVER_NOTIFY, SpeechConfig.PRECEDENCE_ADHERENCE_POLICY.FULL);
	}

	private void setupSpeechMap(HashSet<String> availableFunctions, String[] functionPrec, SpeechConfig.AMBIGUITY_NOTICATION_POLICY ambigPolicy, SpeechConfig.PRECEDENCE_ADHERENCE_POLICY precedencePolicy)
	{
		boolean success = false;
		try
		{
			success = false;
			env.mapValue("pattern-spec", getLispPatternSpec(baseFunctions, basePatternSpec, availableFunctions));
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue("Failed to bind lisp pattern-spec", success);


		try
		{
			success = false;
			env.mapValue("type-spec", getLispTypeSpec(baseTypeNames, baseTypeSpec, availableFunctions));
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue("Failed to bind lisp type-spec", success);


		try
		{
			success = false;
			env.mapValue("function-prec", LispUtilities.convertStringArray(functionPrec));
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue("Failed to bind lisp function-prec", success);



		try
		{
			success = false;
			env.evaluate("(setq speech-config (create-speech-config pattern-spec type-spec function-prec parser))", true);
			env.evaluate("(add-function-wo-side-effects speech-config \"speech-set\")", true);
			env.evaluate("(add-function-wo-side-effects speech-config \"speech-variable-get\")", true);
			Value configValue = env.getVariableValue("speech-config");
			config = ((SpeechConfigLispWrapper)configValue.getObjectValue()).getConfig();
			config.setAmbiguityNotificationPolicy(ambigPolicy);
			config.setPrecedencePolicy(precedencePolicy);
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue("Failed to bind lisp speech-config", success);

		try
		{
			success = false;
			env.evaluate("(setq speech-map (create-speech-map speech-config))", true);


			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue("Failed to bind lisp speech-map", success);
	}

	@SuppressWarnings("deprecation")
	@Test
	public void testTransformingPatterns()
	{
		HashSet<String> availableFunctions = arrayToSet(new String[]{"speech-number", "speech-add", "speech-times"});
		String[] functionPrec = { "speech-add", "speech-times", "speech-number"};

		setupSpeechMap(availableFunctions, functionPrec);

		String [] tokenizedPattern = {"<plus>", "10"};
		String [] tokenizedInput = {"<plus>", "10"};

		SpeechMap map = (SpeechMap)env.getVariableValue("speech-map").getObjectValue();

		Pair<String[], HashMap<String, ScoredValue>> out = map.getTransformedInput(map.getTokenizedInput(tokenizedInput), tokenizedPattern);




		Assert.assertEquals(out.getKey(), tokenizedInput);
		Assert.assertEquals(out.getRight().size(), 0);
	}


	@Test
	public void testScoringPatterns()
	{
		HashSet<String> availableFunctions = arrayToSet(new String[]{"speech-number", "speech-add"});
		String[] functionPrec = { "speech-add", "speech-number"};

		setupSpeechMap(availableFunctions, functionPrec);

		SpeechMap map = (SpeechMap)env.getVariableValue("speech-map").getObjectValue();


		// Test scoring simple explicit matches
		String [] tokenizedPattern = testTokenizer("[lvalue#number] <plus> [rvalue#number]");
		String [] tokenizedInput = testTokenizer("10 plus 10");


		FunctionApplicabilityData result = map.assessPattern(new ProcessStateInfo("speech-add"), map.getTokenizedInput(tokenizedInput), tokenizedPattern);
		double argumentScore = result.score;
		Assert.assertTrue("Score of perfectly matching pattern should be 1, was :" + argumentScore, argumentScore==1.0);

		// test hierarchical, indirect matches

		tokenizedInput = testTokenizer("10 plus 10 plus 20");
		result = map.assessPattern(new ProcessStateInfo("speech-add"), map.getTokenizedInput(tokenizedInput), tokenizedPattern);
		argumentScore = result.score;
		Assert.assertTrue("Score of hierachical pattern [" + "10 plus 10 plus 20" + "] should be 1, was :" + argumentScore, argumentScore==1.0);
	}


	@Test
	public void testSimpleArithmeticEvaluation()
	{
		// Test number evaluation

		HashSet<String> availableFunctions = arrayToSet(new String[]{"speech-number", "speech-add", "speech-subtract", "speech-times", "speech-divide"});
		String[] functionPrec = { "speech-add", "speech-subtract", "speech-times", "speech-divide", "speech-number"};

		setupSpeechMap(availableFunctions, functionPrec);


		int lvalue = 10;
		int rvalue = 20;

		String[] operators = {"plus", "minus", "times", "divide"};
		int[] results = {(lvalue + rvalue + rvalue), (lvalue - rvalue - rvalue), (lvalue * rvalue * rvalue), (lvalue / rvalue / rvalue)};



		String utterance = "" + lvalue + " plus " + rvalue + " plus " + rvalue;
		boolean success = true;




		int maxTestOperators = 1;
		for (int operatorIndex = 0;operatorIndex < maxTestOperators; operatorIndex++)
		{
			utterance = "" + lvalue + " " + operators[operatorIndex] + " " + rvalue + " " + operators[operatorIndex] + " " + rvalue;

			try
			{
				success = false;
				env.mapValue("utterance", LispUtilities.convertStringArray(StringUtils.split(utterance, ' ')));
				success = true;
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
			Assert.assertTrue("Failed to bind lisp utterance: '" + utterance+"'", success);

			Value expectedResult = NLispTools.makeValue(results[operatorIndex]);

			Value actualResult = null;

			try
			{
				success = false;
				env.evaluate("(setq result (evaluate-speech-fast speech-map utterance))", true);
				success = true;
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
			Assert.assertTrue( "Failed to evaluate speech utterance: '" + utterance + "'", success);


			try
			{
				actualResult = env.getVariableValue("result");
				success = actualResult != null;
			}
			catch (Exception e)
			{
				e.printStackTrace();
				Assert.fail("Failed due to invalid result");
			}
			Assert.assertTrue( "Failed to get valid result for: '" + utterance + "'", success);

			Assert.assertTrue( "Failed to get valid type for: '" + utterance + "', expected integer, got: " + actualResult.getType(), NLispTools.isNumericType(actualResult));
			Assert.assertTrue( "Incorrect result valued for: '" + utterance + "', expected integer, got: " + actualResult.getType(), actualResult.getIntValue() == expectedResult.getIntValue());
		}

		// Test base result still works

		utterance = "10";

		try
		{
			success = false;
			env.mapValue("utterance", LispUtilities.convertStringArray(StringUtils.split(utterance, ' ')));
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue("Failed to bind lisp utterance: '" + utterance+"'", success);

		Value expectedResult = NLispTools.makeValue(10);

		Value actualResult = null;

		try
		{
			success = false;
			env.evaluate("(setq result (evaluate-speech-fast speech-map utterance))", true);
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue( "Failed to evaluate speech utterance: '" + utterance + "'", success);


		try
		{
			actualResult = env.getVariableValue("result");
			success = actualResult != null;
		}
		catch (Exception e)
		{
			e.printStackTrace();
			Assert.fail("Failed due to invalid result");
		}
		Assert.assertTrue( "Failed to get valid result for: '" + utterance + "'", success);

		Assert.assertTrue( "Failed to get valid type for: '" + utterance + "', expected integer, got: " + actualResult.getType(), actualResult.isInteger());
		Assert.assertTrue( "Incorrect result valued type for: '" + utterance + "', expected integer, got: " + actualResult.getType(), actualResult.getIntValue() == expectedResult.getIntValue());

	}


	@Test
	public void testComplexArithmeticEvaluation()
	{
		// Test number evaluation

		HashSet<String> availableFunctions = arrayToSet(new String[]{"speech-times", "speech-number", "speech-add"});
		String[] functionPrec = { "speech-add", "speech-times", "speech-number"};

		setupSpeechMap(availableFunctions, functionPrec);


		String utterance = "20 times 10 plus 15 times 2";
		boolean success = true;

		int out = 20*10+15*2;

		try
		{
			success = false;
			env.mapValue("utterance", LispUtilities.convertStringArray(StringUtils.split(utterance, ' ')));
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue("Failed to bind lisp utterance: '" + utterance+"'", success);

		Value expectedResult = NLispTools.makeValue(out);

		Value actualResult = null;

		try
		{
			success = false;
			env.evaluate("(setq result (evaluate-speech-fast speech-map utterance))", true);
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue( "Failed to evaluate speech utterance: '" + utterance + "'", success);


		try
		{
			actualResult = env.getVariableValue("result");
			success = actualResult != null;
		}
		catch (Exception e)
		{
			e.printStackTrace();
			Assert.fail("Failed due to invalid result");
		}
		Assert.assertTrue( "Failed to get valid result for: '" + utterance + "'", success);

		Assert.assertTrue( "Failed to get valid type for: '" + utterance + "', expected integer, got: " + actualResult.getType(), NLispTools.isNumericType(actualResult));
		Assert.assertTrue( "Incorrect result valued for: '" + utterance + "', expected: " + expectedResult + ", got: " + actualResult, actualResult.getIntValue() == expectedResult.getIntValue());
	}


	@Test
	public void testAmbiguityEvaluation()
	{


		HashSet<String> availableFunctions = arrayToSet(new String[]{"speech-times", "speech-number", "speech-add"});
		String[] functionPrec = { "speech-add", "speech-times", "speech-number"};

		setupSpeechMap(availableFunctions, functionPrec, SpeechConfig.AMBIGUITY_NOTICATION_POLICY.NOTIFY_ONLY_AT_TOP_LEVEL, SpeechConfig.PRECEDENCE_ADHERENCE_POLICY.PARTIAL);


		String utterance = "20 times plus 2";
		boolean success = true;



		try
		{
			success = false;
			env.mapValue("utterance", LispUtilities.convertStringArray(StringUtils.split(utterance, ' ')));
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue("Failed to bind lisp utterance: '" + utterance+"'", success);

		Value expectedResult = NLispTools.makeValue(22);

		Value actualResult = null;

		try
		{
			success = false;
			env.evaluate("(setq result (evaluate-speech-fast speech-map utterance))", true);
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue( "Failed to evaluate speech utterance: '" + utterance + "'", success);


		try
		{
			actualResult = env.getVariableValue("result");
			success = actualResult != null && ambiguousOptionCount == 2;
		}
		catch (Exception e)
		{
			e.printStackTrace();
			Assert.fail("Failed due to invalid result");
		}
		Assert.assertTrue( "Failed to get valid result for: '" + utterance + "'", success);

		Assert.assertTrue( "Failed to get valid type for: '" + utterance + "', expected integer, got: " + actualResult.getType(), NLispTools.isNumericType(actualResult));
		Assert.assertTrue( "Incorrect result valued for: '" + utterance + "', expected: " + expectedResult + ", got: " + actualResult, actualResult.getIntValue() == expectedResult.getIntValue());
	}



	@Test
	public void testAssignment()
	{
		// Test number evaluation

		//HashSet<String> availableFunctions = arrayToSet(new String[]{"speech-times", "speech-number", "speech-add", "speech-set", "speech-variable-get"});
		//String[] functionPrec = { "speech-add", "speech-times", "speech-set", "speech-variable-get", "speech-number"};
		HashSet<String> availableFunctions = arrayToSet(new String[]{"speech-number", "speech-set", "speech-variable-get"});

		String[] functionPrec = {  "speech-set", "speech-variable-get", "speech-number"};

		setupSpeechMap(availableFunctions, functionPrec);

		String varName = "the upper x";
		String utterance = varName + " is 10";
		boolean success = true;

		int out = 2*10;

		try
		{
			success = false;
			env.mapValue("utterance", LispUtilities.convertStringArray(StringUtils.split(utterance, ' ')));
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue("Failed to bind lisp utterance: '" + utterance+"'", success);

		Value expectedResult = NLispTools.makeValue(out);

		Value varMap = null;

		try
		{
			success = false;
			env.evaluate("(setq result (evaluate-speech-fast speech-map utterance))", true);
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue( "Failed to evaluate speech utterance: '" + utterance + "'", success);


		try
		{
			success = false;
			varMap = env.getVariableValue("var-map");
			HashMap<String, Value> map = varMap.getStringHashtable();
			expectedResult = map.get(varName);
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
			Assert.fail("Failed due to invalid result and/or expected result");
		}

		Assert.assertTrue("Failed to save value", success && expectedResult.getIntValue() == 10);

		utterance = varName + " is 10";

	}


	@Test
	public void testAssignmentAndRetrieval()
	{
		// Test number evaluation

		HashSet<String> availableFunctions = arrayToSet(new String[]{"speech-times", "speech-number", "speech-set", "speech-variable-get"});
		String[] functionPrec = {  "speech-times", "speech-set", "speech-variable-get", "speech-number"};
		//HashSet<String> availableFunctions = arrayToSet(new String[]{"speech-number", "speech-set", "speech-variable-get"});

		//String[] functionPrec = {"speech-add",  "speech-set", "speech-variable-get", "speech-number"};

		setupSpeechMap(availableFunctions, functionPrec);

		String varName = "the upper x";
		String utterance = varName + " is 10";
		boolean success = true;



		try
		{
			success = false;
			env.mapValue("utterance", LispUtilities.convertStringArray(StringUtils.split(utterance, ' ')));
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue("Failed to bind lisp utterance: '" + utterance+"'", success);

		Value expectedResult = null;

		Value varMap = null;

		try
		{
			success = false;
			env.evaluate("(setq result (evaluate-speech-fast speech-map utterance))", true);
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue( "Failed to evaluate speech utterance: '" + utterance + "'", success);

		HashMap<String, Value> map = null;
		try
		{
			success = false;
			varMap = env.getVariableValue("var-map");
			map = varMap.getStringHashtable();
			expectedResult = map.get(varName);
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
			Assert.fail("Failed due to invalid result and/or expected result");
		}

		Assert.assertTrue("Failed to save value", success && expectedResult.getIntValue() == 10);



		int out = 10*10 + 3;
		utterance = varName + " times 10";

		// Redefine utterance
		try
		{
			success = false;
			env.mapValue("utterance", LispUtilities.convertStringArray(StringUtils.split(utterance, ' ')));
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue("Failed to rebind lisp utterance: '" + utterance+"'", success);

		try
		{
			success = false;
			env.evaluate("(setq result (evaluate-speech-fast speech-map utterance))", true);
			success = true;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		Assert.assertTrue( "Failed to evaluate speech utterance: '" + utterance + "'", success);

		Value actualResult;
		try
		{
			actualResult = env.getVariableValue("result");
			success = (actualResult != null && actualResult.getIntValue() == 100);


		}
		catch (Exception e)
		{
			e.printStackTrace();
			Assert.fail("Failed due to invalid result");
		}

		Assert.assertTrue("Incorrect result for: " + utterance + "", success);


	}


	// Unit Test-specific helpers

	// Common Lisp SpeechMap methods and fields

	String[] testTokenizer(String input)
	{
		return StringUtils.split(input, ' ');
	}


	String rawStringConverterFunctionName = null;

	final SpeechExternalInterface _topEvaluator = new SpeechExternalInterface()
	{

		@Override
		public ScoredValue evaluate(String name, HashMap<String, ScoredValue> argMap) {

			try
			{
				FunctionTemplate template = env.getFunction(name);
				Value[] args = new Value[]{convertArgument(argMap)};
				template.setActualParameters(args);
				Value result = template.evaluate(env, false);
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

			return StringUtils.split(phrase, ' ');
		}

		@Override
		public String toString(ScoredValue value) {
			return convertScoredValue(value).toString();
		}

		@Override
		public FunctionApplicabilityData onAmbiguousResult(
				FunctionApplicabilityData[] possibilities, boolean isTop) {

			try
			{
				FunctionTemplate template = env.getFunction(ambiguity_handler_function_name);
				Value[] args = new Value[possibilities.length];
				FunctionApplicabilityData data;
				for (int i = 0;i<args.length;i++)
				{
					data = possibilities[i];
					args[i] = convertFunctionApplicabilityData(config, data);
				}

				Value[] fargs = {NLispTools.makeValue(args), NLispTools.makeValue(isTop)};
				template.setActualParameters(fargs);
				Value returnValue = template.evaluate(env, false);
				return LispUtilities.convertToFunctionApplicabilityData(config, returnValue);

			}
			catch (Exception e)
			{
				e.printStackTrace();
				return new FunctionApplicabilityData(0, null).setFailureRejected();
			}
		}

	};



	Value convertArgument(HashMap<String, ScoredValue> argMap)
	{
		HashMap<String, Value> map = new HashMap<String, Value>();
		for (String key:argMap.keySet())
		{
			map.put(key, convertScoredValue(argMap.get(key)));
		}

		return new StringHashtableValue(map);
	}

	Value convertScoredValue(ScoredValue value)
	{
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
	 * FunctionApplicabilityData gets converted into a list:
	 * (score, arg-map, function-name, pattern, scored-value, canonical-phrase)
	 * @param data
	 * @return
	 */
	public Value  convertFunctionApplicabilityData(SpeechConfig config, FunctionApplicabilityData data)
	{
		Value[] result = new Value[7];
		result[0] = NLispTools.makeValue(data.score);
		HashMap<String, Value> map = new HashMap<String, Value>();
		HashMap<String, ScoredValue> argMap = data.argMap;
		if (argMap != null)
		{
			for (String key: argMap.keySet())
			{
				map.put(key, convertScoredValue(argMap.get(key)));
			}
		}
		else
			argMap = new HashMap<String, ScoredValue>();

		result[1] = new StringHashtableValue(map);
		result[2] = NLispTools.makeValue(data.functionName);
		result[3] = LispUtilities.convertStringArray(data.pattern);
		result[4] = convertScoredValue(data.resultValue);
		result[5] = map.get(config.getCanonicalPhraseArgKeyName());
		FUNCTION_APPLICABILITY_STATUS status = data.status;
		String s = status.name();
		result[6] = NLispTools.makeValue(s);
		return NLispTools.makeValue(result);
	}

	Value convertRawStringTokens(Value stringArray)
	{
		try
		{
			if (rawStringConverterFunctionName!= null)
			{
				FunctionTemplate converterFunction = env.getFunction(rawStringConverterFunctionName);
				if (converterFunction != null)
				{
					converterFunction.setActualParameters(new Value[]{stringArray});
					return converterFunction.evaluate(env, false);
				}
			}
		}
		catch (Exception e)
		{
			throw new RuntimeException(e);
		}
		return stringArray;

	}





	// FunctionTemplate methods

	public SimpleFunctionTemplate create_speech_config()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)create_speech_config();
			}

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(4, true, false);
				Value patternSpecMap = evaluatedArgs[0];
				Value typeSpecMap = evaluatedArgs[1];
				Value functionPrec = evaluatedArgs[2];
				Value parserValue = evaluatedArgs[3];

				SpeechConfigLispWrapper wrapper = new SpeechConfigLispWrapper(patternSpecMap, typeSpecMap, functionPrec, parserValue);
				return ExtendedFunctions.makeValue(wrapper);
			}



		};
	}

	public SimpleFunctionTemplate create_speech_map()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)create_speech_map();
			}

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, false, false);
				Value speechConfig = evaluatedArgs[0];
				SpeechConfigLispWrapper wrapper = (SpeechConfigLispWrapper)speechConfig.getObjectValue();
				SpeechMap smap = new SpeechMap(_topEvaluator, wrapper.getConfig());
				return ExtendedFunctions.makeValue(smap);
			}
		};
	}



	public SimpleFunctionTemplate on_ambiguity_handler()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)on_ambiguity_handler();
			}

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, false);
				Value[] options = evaluatedArgs[0].getList();
				if (options != null)
					ambiguousOptionCount = options.length;
				else
					ambiguousOptionCount = 0;
				return options[0];
			}
		};
	}



	public SimpleFunctionTemplate add_function_wo_side_effects()
	{
		return new SimpleFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)add_function_wo_side_effects();
			}

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(2, false, false);
				Value speechConfig = evaluatedArgs[0];
				Value functionName = evaluatedArgs[1];
				String fName = functionName.getString();
				SpeechConfigLispWrapper wrapper = (SpeechConfigLispWrapper)speechConfig.getObjectValue();
				wrapper.getConfig().addFunctionWithSideEffects(fName);
				SpeechMap smap = new SpeechMap(_topEvaluator, wrapper.getConfig());
				return ExtendedFunctions.makeValue(smap);
			}
		};
	}


	public SimpleFunctionTemplate evaluate_speech_fast()
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
				checkActualArguments(2, false, false);
				Value speechMap = evaluatedArgs[0];
				Value tokenizedInput = evaluatedArgs[1];

				SpeechMap smap = (SpeechMap)speechMap.getObjectValue();
				String[] tokenizedSpeech = LispUtilities.convertStringArray(tokenizedInput);

				ScoredValue result = smap.evaluate(tokenizedSpeech);
				if (result.score == 0 || result.value == null)
					return Environment.getNull();
				else
					return (Value)result.value;

			}
		};
	}

}
