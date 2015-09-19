package com.evolved.automata;

import java.io.IOException;
import java.util.*;


public class SpeechRecordBuilder 
{
	CFGParser parser;
	
	public static final String conjSymbol = "&";
	public SpeechRecordBuilder()
	{
		String[] speechGrammar = new String[]
		                                    {
				"alpha = ('@' | '_' | '#')+",
				"synonym = alpha",
				"word=alpha",
				"function=alpha",
				"synonyms = '$'*, '(', '$'*, synonym, ('$'+, synonym)*, '$'*, ')'",
				"segment = word, synonyms?",
				"phrase = '[', '~'+, ']'",
				"pattern = function, '$'+, phrase, '$'+, segment, ('$'+, segment)*"
		                                    };
		parser = new CFGParser(speechGrammar);
		
	}
	
	public SpeechRecordBuilder(String inputFileFullName)
	{
		
		String[] inputLines = null;                 
		try
		{
			inputLines = CFGParser.getCFGLines(inputFileFullName);
			parser = new CFGParser(inputLines);
		}
		catch (Exception e)
		{
			
		}
		
		
	}
	
	
	public LinkedList<Hashtable<String,String>> processSpeechInputPattern(String phraseDef)
	{
		Hashtable<String,String> baseMap = new Hashtable<String, String>(), additional;
		LinkedList<Hashtable<String,String>> output = new LinkedList<Hashtable<String,String>>();
		parser.addCaptureName("segment");
		parser.addCaptureName("function_id");
		parser.addCaptureName("word");
		parser.addCaptureName("synonym");
		parser.addCaptureName("phrase");
		parser.addCaptureName("phrase_type");
		setBaseSpeechKeys(baseMap,phraseDef);
		output.add(baseMap);
		String function = baseMap.get("function");
		String raw = baseMap.get("raw");
		
		LinkedList<String> segments = parser.getCapturedList("segment");
		for (String seg:segments)
		{
			additional = getSynonyms(function,raw,seg);
			if (additional!=null)
				output.add(additional);
		}
		
		return output;
	}
	
	public LinkedList<Hashtable<String,String>> processAllData(String inputFile) throws IOException
	{
		String[] dataLines = FileTools.getTextFileDataLines(inputFile);
		return processAllData(dataLines);
	}
	
	
	
	public LinkedList<Hashtable<String,String>> processAllData(String[] lineinput)
	{
		LinkedList<Hashtable<String,String>> output = new LinkedList<Hashtable<String,String>>();
		int[] match;
		String captured;
		for (String line:lineinput)
		{
			parser.addCaptureName("speech_pattern");
			parser.addCaptureName("behavior_pattern");
			parser.addCaptureName("action_pattern");
			match = parser.match(line, "speech_pattern|behavior_pattern|action_pattern");
			if (match!=null)
			{
				captured = parser.getFirstCapturedValue("speech_pattern");
				if (captured!=null)
				{
					output.addAll(processSpeechInputPattern(captured));
				}
				else
				{
					captured = parser.getFirstCapturedValue("behavior_pattern");
					if (captured!=null)
					{
						output.addAll(processBehaviorInputPattern(captured));
					}
					else
					{
						captured = parser.getFirstCapturedValue("action_pattern");
						output.addAll(processActionInputPattern(captured));
					}
				}
			}
		}
		return output;
	}
	
	
	public LinkedList<Hashtable<String,String>> processBehaviorInputPattern(String phraseDef)
	{
		Hashtable<String,String> baseMap = new Hashtable<String, String>(), additional;
		LinkedList<Hashtable<String,String>> output = new LinkedList<Hashtable<String,String>>();
		parser.addCaptureName("function_id");
		parser.addCaptureName("function_id2");
		parser.addCaptureName("gconj");
		int[] match=null;
		match=parser.match(phraseDef, "behavior_pattern");
		LinkedList<String> capturedSegmentConditions;
		capturedSegmentConditions = parser.getCapturedList("gconj");
		String[] expressionParts;
		String lvalue, rvalue;
		if (match!=null)
		{
			String funct = parser.getFirstCapturedValue("function_id");
			String triggeredAction = parser.getFirstCapturedValue("function_id2");
			
			if (capturedSegmentConditions!=null)
			{
				for (String condition:capturedSegmentConditions)
				{
					additional = new Hashtable<String, String>();
					additional.put("function", funct);
					additional.put("Enabled", "T");
					additional.put("behavior_defP", "T");
					additional.put("ref_action", triggeredAction);
					expressionParts = condition.split(conjSymbol);
					for (String sensorName:expressionParts)
					{
						
						parser.addCaptureName("lvalue");
						parser.addCaptureName("rvalue");
						parser.match(sensorName, "predicate");
						if (((lvalue=parser.getFirstCapturedValue("lvalue"))!=null)&& ((rvalue=parser.getFirstCapturedValue("rvalue"))!=null))
						{
							additional.put(lvalue, rvalue);
						}
						else
						{
							if (sensorName.trim().substring(0,1).equals("-"))
								additional.put(sensorName.substring(1,sensorName.length()), "-S");
							else
								additional.put(sensorName.trim(), "S");
						}
					}
					output.add(additional);
				}
				
			}
		}
		
		return output;
	}
	
	public LinkedList<Hashtable<String,String>> processActionInputPattern(String phraseDef)
	{
		Hashtable<String,String> baseMap = new Hashtable<String, String>(), additional;
		LinkedList<Hashtable<String,String>> output = new LinkedList<Hashtable<String,String>>();
		parser.addCaptureName("function_id");
		parser.addCaptureName("base_condition");
		parser.addCaptureName("action_def");
		
		
		baseMap.put("Enabled", "T");
		baseMap.put("action_defP", "T");
		int[] match;
		match=parser.match(phraseDef, "action_pattern");
		
		if (match!=null)
		{
			LinkedList<String> capturedActions, capturedSegmentConditions;
			
			String funct = parser.getFirstCapturedValue("function_id");
			if (funct!=null)
				baseMap.put("function", funct);
			
			capturedActions = parser.getCapturedList("action_def");
			String[] expressionParts;
			if (capturedActions!=null)
			{
				baseMap.put("step", "1");
				baseMap.put("total_steps", "" + capturedActions.size());
				output.add(baseMap);
				
				String base_condition = parser.getFirstCapturedValue("base_condition");
				if (base_condition!=null)
				{
					parser.addCaptureName("conj");
					parser.match(base_condition, "base_condition");
					
					for (String exitCondition:parser.getCapturedList("conj"))
					{
						additional = new Hashtable<String, String>();
						additional.put("Enabled", "T");
						additional.put("function", funct);
						additional.put("exitP", "T");
						expressionParts = exitCondition.split(conjSymbol);
						for (String sensorName:expressionParts)
						{
							if (sensorName.trim().substring(0,1).equals("-"))
								additional.put(sensorName.substring(1,sensorName.length()), "-S");
							else
								additional.put(sensorName.trim(), "S");
						}
						output.add(additional);
					}
				}
				
				// Add individual actions segments
				int stepCounter=1;
				String actionName;
				for (String actionSegment:capturedActions)
				{
					parser.addCaptureName("conj");
					parser.addCaptureName("action_name");
					parser.match(actionSegment, "action_def");
					actionName = parser.getFirstCapturedValue("action_name");
					capturedSegmentConditions = parser.getCapturedList("conj");
					
					additional = new Hashtable<String, String>();
					additional.put("Enabled", "T");
					additional.put("function", funct);
					additional.put("segment_defP", "T");
					additional.put("step", ""+stepCounter);
					additional.put("ref_action", actionName);
					output.add(additional);
					if (capturedSegmentConditions!=null)
					{
						for (String segmentCondition:capturedSegmentConditions)
						{
							additional = new Hashtable<String, String>();
							additional.put("Enabled", "T");
							additional.put("function", funct);
							additional.put("conditions_P", "T");
							additional.put("step", ""+stepCounter);
							expressionParts = segmentCondition.split(conjSymbol);
							for (String sensorName:expressionParts)
							{
								if (sensorName.trim().substring(0,1).equals("-"))
									additional.put(sensorName.substring(1,sensorName.length()), "-S");
								else
									additional.put(sensorName.trim(), "S");
							}
							output.add(additional);
						}
					}
					
					stepCounter++;
				}
				
			}
			else
			{
				// primitive action
				baseMap.put("step", "1");
				baseMap.put("total_steps", "1");
				output.add(baseMap);
			}
		}
		
		
		
		return output;
	}
	
	
	
	private void setBaseSpeechKeys(Hashtable<String,String> map, String def)
	{
		map.put("Enabled", "T");
		map.put("definitionP", "T");
		parser.match(def, "speech_pattern");
		LinkedList<String> capturedList;
		String funct = parser.getFirstCapturedValue("function_id");
		String phrase_type = parser.getFirstCapturedValue("phrase_type");
		if (phrase_type.equals("DEFINE PARAMETRIC PHRASE"))
			map.put("argsP", "T");
		if (funct!=null)
			map.put("function", funct);
		
		String phrase = parser.getFirstCapturedValue("phrase");
		if (funct!=null)
			map.put("raw", phrase);
		
		capturedList=parser.getCapturedList("word");
		if ((capturedList!=null)&&(capturedList.size()>0))
		{
			for (String v:capturedList)
			{
				map.put(v, "W");
				
			}
		}
		
	}
	
	private Hashtable<String,String> getSynonyms(String functionName, String raw, String segment)
	{
		Hashtable<String,String> map=null;
		parser.addCaptureName("word");
		parser.addCaptureName("synonym");
		parser.match(segment, "segment");
		String baseWord;
		LinkedList<String> synonyms, base;
		Hashtable<String, LinkedList<String>> set = parser.getCaptureSet();
		if ((set!=null)&&(set.size()>0))
		{
			if (set.containsKey("word"))
				baseWord = set.get("word").getFirst();
			else
				return null;
			if (set.containsKey("synonym"))
			{
				map = new Hashtable<String, String>();
				map.put("function", functionName);
				map.put("raw", raw);
				map.put("replacementmapP", "T");
				for (String syn:set.get("synonym"))
				{
					map.put("original_word", baseWord);
					map.put("substituted_word", syn);
				}
			}
		}
		
		return map;
	}
	
	
	
	
	
	
	
}
