package com.evolved.automata.inference;
import java.io.*;
import java.util.*;

import com.evolved.automata.AITools;
import com.evolved.automata.KeyValuePair;





public class InferenceTools 
{
	public static final String KEY_VALUE_SEPARATOR="z@@z";
	public static final String PAIR_SEPARATOR="!z_!!";
	public static final String WORLD_SEPARATOR="zz!@zz";
	
	
	
	public static void writeWorldsToCSVFile(StructureModel model, String outputFileFullname)
	{
		if (model==null)
			return;
		String[] keys = getAllKeyNames(model);
		String[] serializedWorlds =getAllWorlds(model);
		writeModelsToCSV(outputFileFullname, keys, serializedWorlds);
	}
	
	public static StructureModel createFromCSV(String inputFileFullName, int sizeMultiplier)
	{
		return createFromCSV(inputFileFullName,sizeMultiplier,false );
	}
	public static StructureModel createFromCSV(String inputFileFullName, int sizeMultiplier, boolean createIfNotExists)
	{
		if (createIfNotExists)
		{
			File f = new File(inputFileFullName);
			if (!f.exists())
			{
				try
				{
					f.createNewFile();
				}
				catch (IOException ie)
				{
					throw new RuntimeException(ie.toString());
				}
			}
		}
		String[] serializedWords=importFromCSV(inputFileFullName);
		if (serializedWords==null)
			return null;
		StructureModel model;
		if (serializedWords.length==0)
			model = new StructureModel();
		else
			model = new StructureModel(sizeMultiplier*serializedWords.length);
		KeyValuePair[] pairs = null;
		
		for (String serialized:serializedWords)
		{
			pairs = parseToKeyValuePairsFromString(serialized);
			model.AddObservation(pairs);
		}
		return model;
	}
	
	public static StructureModel createFromCSV(String inputFileFullName, boolean createIfNotExists, String delimiter)
	{
		if (createIfNotExists)
		{
			File f = new File(inputFileFullName);
			if (!f.exists())
			{
				try
				{
					f.createNewFile();
				}
				catch (IOException ie)
				{
					throw new RuntimeException(ie.toString());
				}
			}
		}
		
		String[] keyList=null;
		
		char[] delimiterArray = delimiter.toCharArray();
		StringBuilder s = new StringBuilder("\\,");
		for (char c:delimiterArray)
			s.append("\\").append(c);
		String splitPattern = s.toString();
		try
		{
			String input = com.evolved.automata.filetools.StandardTools.getDataFileLine(inputFileFullName);
			if (input.trim().length()==0)
				return new StructureModel();
			String[] parts = input.split(splitPattern);
			StructureModel model = new StructureModel(parts.length);
			boolean keyLine=true;
			String[] valueList;
			final KeyValuePair[] empty = new KeyValuePair[0];
			LinkedList<KeyValuePair> pairList = new LinkedList<KeyValuePair>();
			boolean trim=false;
			int totalKeys=0;
			
			for (String lineinput:parts)
			{
				if (keyLine)
				{
					keyLine=false;
					keyList = splitCommaDelimitedString(lineinput,',');
					totalKeys = keyList.length;
					
				}
				else
				{
					valueList=splitCommaDelimitedString(lineinput,',');
					// TODO: Figure out how to make this more efficient
					pairList = new LinkedList<KeyValuePair>();
					for (int i=0;i<totalKeys;i++)
					{
						if (valueList[i].length()>0)
						{
							
							if (trim)
								pairList.add(new KeyValuePair(keyList[i].trim(), valueList[i].trim()));
							else
								pairList.add(new KeyValuePair(keyList[i].trim(), valueList[i]));
						}
					}
					if (pairList.size()>0)
					{
						model.AddObservation(pairList.toArray(empty));
					}
					
				}
			}
			return model;
		}
		catch (Exception e)
		{
			return null;
		}
		
		
	}

	public static StructureModel createFromCSV(BufferedReader reader, int size)
	{
		
		StructureModel model = new StructureModel(size);
		
		String[] keyList=null;
		
		String lineInput;
		boolean keyLine=true;
		String[] valueList;
		final KeyValuePair[] empty = new KeyValuePair[0];
		LinkedList<KeyValuePair> pairList = new LinkedList<KeyValuePair>();
		boolean trim=false;
		int totalKeys=0;
		try
		{
			
			while ((lineInput=reader.readLine())!=null)
			{
				if (keyLine)
				{
					keyLine=false;
					keyList = splitCommaDelimitedString(lineInput,',');
					totalKeys = keyList.length;
					
				}
				else
				{
					valueList=splitCommaDelimitedString(lineInput,',');
					// TODO: Figure out how to make this more efficient
					pairList = new LinkedList<KeyValuePair>();
					for (int i=0;i<totalKeys;i++)
					{
						if (valueList[i].length()>0)
						{
							
							if (trim)
								pairList.add(new KeyValuePair(keyList[i].trim(), valueList[i].trim()));
							else
								pairList.add(new KeyValuePair(keyList[i].trim(), valueList[i]));
						}
					}
					if (pairList.size()>0)
					{
						model.AddObservation(pairList.toArray(empty));
					}
					
				}
			}
			return model;
		}
		catch (Exception e)
		{
			java.io.StringWriter traceText = new java.io.StringWriter();
			java.io.PrintWriter pWriter = new java.io.PrintWriter(traceText,true);
			e.printStackTrace(pWriter);
			pWriter.close();
			throw new RuntimeException(traceText.toString());
		}
		finally
		{
			if (reader!=null)
			{
				try
				{
					reader.close();
				}
				catch (Exception e2)
				{
					
				}
			}
		}
		
		
	}
	
	
	
	public static void serializeModelToRawFile(StructureModel model, String outputFileFullName) throws FileNotFoundException, IOException
	{
		ObjectOutputStream oStream = new ObjectOutputStream(new FileOutputStream(outputFileFullName));
		oStream.writeObject(model);
	}
	
	public static byte[] serializeModelToByteArray(StructureModel model) throws FileNotFoundException, IOException
	{
		ByteArrayOutputStream bostream = new ByteArrayOutputStream();
		ObjectOutputStream oStream = new ObjectOutputStream(bostream);
		oStream.writeObject(model);
		return bostream.toByteArray();
	}
	
	public static StructureModel loadModelFromRawBytes(byte[] input) throws IOException, ClassNotFoundException
	{
		ByteArrayInputStream bistream = new ByteArrayInputStream(input);
		ObjectInputStream iStream = new ObjectInputStream(bistream);
		Object output = iStream.readObject();
		if (output==null)
			return null;
		return (StructureModel)output;
	}
	
	public static StructureModel loadModelFromRawFile(String inputFileFullName) throws FileNotFoundException, IOException, ClassNotFoundException 
	{
		ObjectInputStream iStream = new ObjectInputStream(new FileInputStream(inputFileFullName));
		Object output = iStream.readObject();
		if (output==null)
			return null;
		return (StructureModel)output;
	}
	
	public static boolean storeKeyValueMap(StructureModel model, Map<String, String> keyValueMap)
	{
		
		if (keyValueMap.size()>0)
		{
			
			int out = addToModel(model, keyValueMap);
			return out>=0;
		}
		return false;
	}
	
	public static Integer storeKeyValueMapWithIndex(StructureModel model, Map<String, String> keyValueMap)
	{
		Integer outIndex=null;
		if (keyValueMap.size()>0)
		{
			
			outIndex=addToModel(model, keyValueMap);
			return outIndex;
		}
		return outIndex;
	}
	
	
	
	public static KeyValuePair[] getKVPairsFromArray(String[][] data)
	{
		String[] pair;
		if (data == null)
			return null;
		
		KeyValuePair[] kvPairs = new KeyValuePair[data.length];
		for (int i=0;i<data.length;i++)
		{
			pair =  data[i];
			kvPairs[i] = new KeyValuePair(pair[0], pair[1]);
		}
		
		return kvPairs;
	}
	
	public static void addArrayToModel(StructureModel model, String[][] data)
	{
		
		KeyValuePair<String, String>[] kvPairs = getKVPairsFromArray(data);
		if ((kvPairs==null)||(kvPairs.length==0))
			return;
			
		
		model.AddObservation(kvPairs);
	}
	
	
	
	public static Hashtable<String, String> parseToKeyValueMapFromKeyValuePairs(KeyValuePair[] pairs)
	{
		Hashtable<String, String> keyValueMap = new Hashtable<String, String>();
		
		if (pairs==null)
			return keyValueMap;


		for (KeyValuePair<String, String> kvPair:pairs)
		{
			keyValueMap.put(kvPair.GetKey(), kvPair.GetValue());
		}
		return keyValueMap;
	}
	
	public static KeyValuePair[] parseToKeyValuePairsFromMap(Map<String, String> keyValueMap)
	{
		KeyValuePair[] pairs;
		
		pairs = new KeyValuePair[keyValueMap.size()];
		int index=0;
		for (String keyName:keyValueMap.keySet().toArray(new String[0]))
		{
			pairs[index] = new KeyValuePair(keyName, keyValueMap.get(keyName));
			index++;
		}
		return pairs;
	}
	
	public static String serializeToString(Map<String, String> keyValueMap)
	{
		StringBuilder sBuilder = new StringBuilder("");

		String pairFormat="%1$s%2$s%3$s";
		boolean second=false;
		for (String keyName:keyValueMap.keySet().toArray(new String[0]))
		{
			if (second)
			{
				sBuilder.append(PAIR_SEPARATOR);
			}
			sBuilder.append(String.format(pairFormat, keyName,KEY_VALUE_SEPARATOR,keyValueMap.get(keyName)));
			second=true;
		}
		return sBuilder.toString();
	}
	
	public static String[] splitCommaDelimitedString(String tokenizedString, char splitChar)
	{
		char[] chars = tokenizedString.toCharArray();
		StringBuilder word= new StringBuilder();
		LinkedList<String> words = new LinkedList<String>();
		
		char currentChar;
		
		
		boolean ignoringSplitToken=false;
		boolean parsingDelimitedQuoteP=false;
		boolean canParseDelimitedQuoteP=false;
		boolean inQuote = false;
		
		int totalChars=chars.length;
		for (int i=0;i<totalChars;i++)
		{
			canParseDelimitedQuoteP=inQuote && i<totalChars-1;
			currentChar=chars[i];
			
			if (currentChar=='"' && !inQuote)
			{
				inQuote = true;
			}
			else if (currentChar=='"' && inQuote)
			{
				if (canParseDelimitedQuoteP && chars[i+1] == '"')
				{
					i++;
					word.append(currentChar);
				}
				else
				{
					inQuote = false;
				}
				
			}
			else if (currentChar==splitChar && !inQuote)
			{
				words.add(word.toString());
				word = new StringBuilder();
			}
			else if (currentChar==splitChar && inQuote)
			{
				
				word.append(currentChar);
			}
			else
				word.append(currentChar);
			
		}
		words.add(word.toString());
		return words.toArray(new String[0]);
	}
	
	public static String[] getAllKeys(StructureModel model)
	{
		if (model==null)
			return new String[0];
		else
			return model.GetKeys();
	}
	
	/**
	 * Use this function to extract a set of keys from a model based on a set of evidence when you know that 
	 * there will be only one consistent world.
	 * @param model
	 * @param evidence
	 * @param returnKeys
	 * @return null if there are no consistent worlds otherwise returns a string array representing
	 * the values of the keys that were requested
	 */
	public static String[] getScalarValue(StructureModel model, Map<String, String> evidence, String[] returnKeys)
	{
		KeyValuePair[] evidenceList = parseToKeyValuePairsFromMap(evidence);
		List<StructureSlice> slices = model.GetConsistentSlices(evidenceList, 1.0, 0.0);
		if (slices.size()>0)
		{
			if (returnKeys == null)
				return new String[0];
			StructureSlice slice = slices.get(0);
			Map<String, String> map = slice.getSliceMap();
			int index=0;
			String[] output = new String[returnKeys.length];
			for (String key:returnKeys)
			{
				if (map.containsKey(key))
					output[index]=map.get(key);
				index++;
			}
			return output;
		}
		else
			return null;
	}
	
	public static void updateValue(StructureModel model, Integer modelKey, String[] keyvaluepair)
	{
		List<Integer> list = new LinkedList<Integer>();
		list.add(modelKey);
		model.UpdateKeyValuesInAllConsistentWorlds(list, keyvaluepair[0], keyvaluepair[1]);
	}
	
	public static List<Integer> addKeyValuePairToWorld(StructureModel model, Integer modelKey, String[] keyvaluepair)
	{
		List<Integer> list = new LinkedList<Integer>();
		list.add(modelKey);
		return model.AddKeyValuesToAllConsistentWorlds(list, keyvaluepair[0], keyvaluepair[1]);
		
	}
	
	
	public static void updateValue(StructureModel model, Map<String, String> evidence, String[] keyvaluepair)
	{
		KeyValuePair[] evidenceList = parseToKeyValuePairsFromMap(evidence);
		model.UpdateKeyValuesInAllConsistentWorlds(evidenceList, keyvaluepair[0], keyvaluepair[1]);
	}
	
	public static List<StructureSlice> getConsistentWords(StructureModel model, Map<String, String> evidence)
	{
		KeyValuePair[] evidenceList = parseToKeyValuePairsFromMap(evidence);
		return model.GetConsistentSlices(evidenceList, 1.0, 0.0);
	}
	
	public static List<StructureSlice> getConsistentWords(StructureModel model, Map<String, String> evidence, boolean strictP)
	{
		KeyValuePair[] evidenceList = parseToKeyValuePairsFromMap(evidence);
		return model.GetConsistentSlices(evidenceList, 1.0, 0.0, strictP);
	}
	
	public static List<StructureSlice> getConsistentWords(StructureModel model, Map<String, String> evidence, String[] requiredKeys)
	{
		KeyValuePair[] evidenceList = parseToKeyValuePairsFromMap(evidence);
		return model.GetConsistentSlices(evidenceList, 1.0, 0.0, false, requiredKeys);
	}
	
	/**
	 * This overload of the function gets all consistent worlds where: <br/>
	 * (1) At least one key intersects the keys in evidence
	 * (2) If requiredKeys is not null then all worlds must contain these keys
	 * (3) If weightedKeys is not null then these keys are deemed more important in that worlds with
	 * these keys will only be returned if the evidence contains these keys 
	 * @param model
	 * @param evidence
	 * @param requiredKeys
	 * @param weightedKeys
	 * @return
	 */
	public static List<StructureSlice> getConsistentWords(StructureModel model, Map<String, String> evidence, String[] requiredKeys, String[] weightedKeys)
	{
		KeyValuePair[] evidenceList = parseToKeyValuePairsFromMap(evidence);
		return model.GetConsistentSlices(evidenceList, requiredKeys, weightedKeys);
	}
	
	
	
	/*
	public static List<StructureSlice> getConsistentWorlds(StructureModel model, Map<String, String> evidence, String[] requiredKeys, com.evolved.automata.alisp.Environment env, Hashtable<String, String > udEqualityFunctions)
	{
		if (evidence==null)
			return new LinkedList<StructureSlice>();
		KeyValuePair[] evidenceList = parseToKeyValuePairsFromMap(evidence);
		if (udEqualityFunctions!=null&&env!=null)
			return model.GetConsistentSlices(evidenceList, requiredKeys, env, udEqualityFunctions);
		else
			return model.GetConsistentSlices(evidenceList, 1.0, 0.0, false, requiredKeys);
	}
	*/
	
	
	public static List<Integer> getConsistentIndices(StructureModel model, Map<String, String> evidence)
	{
		if (evidence==null)
			return new LinkedList<Integer>();
		KeyValuePair[] evidenceList = parseToKeyValuePairsFromMap(evidence);
		return model.GetConsistentWorlds(evidenceList, 1.0, 0.0);
	}
	
	public static List<Integer> getConsistentIndices(StructureModel model, Map<String, String> evidence, String[] requiredKeys)
	{
		if (evidence==null)
			return new LinkedList<Integer>();
		KeyValuePair[] evidenceList = parseToKeyValuePairsFromMap(evidence);
		return model.GetConsistentWorlds(evidenceList, 1.0, 0.0, false, requiredKeys);
	}
	
	public static List<Integer> getConsistentIndices(StructureModel model, Map<String, String> evidence, boolean supersets)
	{
		if (evidence==null)
			return new LinkedList<Integer>();
		KeyValuePair[] evidenceList = parseToKeyValuePairsFromMap(evidence);
		if (supersets)
			return model.GetConsistentWorlds(evidenceList, 1.0, 0.0, false, evidence.keySet().toArray(new String[0]));
		else
			return model.GetConsistentWorlds(evidenceList, 1.0, 0.0, false, null);
	}
	
	
	// TODO: Make this work for user-defined match functions
	/*
	public static List<Integer> getConsistentIndices(StructureModel model, Map<String, String> evidence, String[] requiredKeys, com.evolved.automata.alisp.Environment env, Hashtable<String, String> customMatchFunctions)
	{
		if (evidence==null)
			return new LinkedList<Integer>();
		KeyValuePair[] evidenceList = parseToKeyValuePairsFromMap(evidence);
		return model.GetConsistentWorlds(evidenceList, requiredKeys, env, customMatchFunctions);
	}
	*/
	
	public static StructureSlice getWorldExplicit(StructureModel model, Integer index)
	{
		return model.GetSliceExplicit(index);
	}
	
	public static Hashtable<String, String> getWorldMapExplicit(StructureModel model, Integer index)
	{
		StructureSlice slice = model.GetSliceExplicit(index);
		if (slice==null)
		{
			return null;
		}
		Hashtable<String, String> o = new Hashtable<String, String>();
		Map<String, String> smap = slice.getSliceMap();
		for (String key:smap.keySet())
		{
			o.put(key, smap.get(key));
		}
		return o;
	}
	
	public static void deleteWorldExplicit(StructureModel model, Integer index)
	{
		model.DeleteWorldsExplicit(index);
	}
	
	public static void deleteConsistentWorlds(StructureModel model, Map<String, String> input)
	{
		if (input==null)
			return;
		KeyValuePair[] evidenceList = parseToKeyValuePairsFromMap(input);
		model.DeleteWorlds(evidenceList);
	}
	
	public static Hashtable<String, String> getShiftedOutput(Hashtable<String,String> baseHistory, Hashtable<String, String> newSlice, int shiftDepth)
	{
		LinkedList<String> keys = new LinkedList<String>(),values = new LinkedList<String>();
		
		if (newSlice==null)
		{
			AITools.shiftBackValues(baseHistory, baseHistory.keySet().toArray(new String[0]), null, shiftDepth);
		}
		else
		{
			for (String key:newSlice.keySet())
			{
				keys.add(key);
				values.add(newSlice.get(key));
			}
			AITools.shiftBackValues(baseHistory, keys.toArray(new String[0]), values.toArray(new String[0]), shiftDepth);
		}
		
		return baseHistory;
	}
	
	public static Hashtable<String, String> getShiftedOutput(Hashtable<String,String> baseHistory, String[] newSlice, int shiftDepth)
	{
		AITools.shiftBackValues(baseHistory, newSlice, null, shiftDepth);
		
		return baseHistory;
	}
	
	private static String[] getAllKeyNames(StructureModel model)
	{
		return getAllKeys(model);
	}
	
	private static String[] getAllWorlds(StructureModel model)
	{
		List<StructureSlice> results=model.GetAllWorlds();
		String[] serializedWorlds = new String[results.size()];
		
		int counter=0;
		for (StructureSlice slice:results)
		{
			serializedWorlds[counter++]=serializeToString(
					parseToKeyValueMapFromKeyValuePairs(slice.GetKeyValuePairs()));
		}
		return serializedWorlds;
	}
	
	
	
	
	private static String[] importFromCSV(String inputFileFullName)
	{
		return importFromCSV(inputFileFullName, false);
	}
	
	private static String[] importFromCSV(String inputFileFullName, boolean trim)
	{
		return importFromCSV(inputFileFullName, trim, true);
	}

	private static String[] importFromCSV(String inputFileFullName, boolean trim, boolean allowEmptyFieldsP)
	{
		String[] keyList=null;
		LinkedList<String> outputList = new LinkedList<String>();
		BufferedReader reader=null;
		String lineInput;
		boolean keyLine=true;
		String[] valueList;
		Hashtable<String,String> keyValuePairs;
		int totalKeys=0;
		try
		{
			reader = new BufferedReader(new FileReader(inputFileFullName));
			while ((lineInput=reader.readLine())!=null)
			{
				if (keyLine)
				{
					keyLine=false;
					keyList = splitCommaDelimitedString(lineInput,',');
					totalKeys = keyList.length;
				}
				else
				{
					valueList=splitCommaDelimitedString(lineInput,',');
					// TODO: Figure out how to make this more efficient
					keyValuePairs= new Hashtable<String, String>();
					for (int i=0;i<totalKeys;i++)
					{
						if (allowEmptyFieldsP || valueList[i].length()>0)
						{
							if (trim)
								keyValuePairs.put(keyList[i].trim(), valueList[i].trim());
							else
								keyValuePairs.put(keyList[i].trim(), valueList[i]);
						}
					}
					if (keyValuePairs.size()>0)
					{
						outputList.add(serializeToString(keyValuePairs));
					}
					
				}
			}
		}
		catch (Exception e)
		{
			java.io.StringWriter traceText = new java.io.StringWriter();
			java.io.PrintWriter pWriter = new java.io.PrintWriter(traceText,true);
			e.printStackTrace(pWriter);
			pWriter.close();
			throw new RuntimeException(traceText.toString());
		}
		finally
		{
			if (reader!=null)
			{
				try
				{
					reader.close();
				}
				catch (Exception e2)
				{
					
				}
			}
		}
		
		return outputList.toArray(new String[0]);
	}

	public static String[] importFromCSV(BufferedReader reader)
	{
		return importFromCSV(reader, false);
	}
	
	public static String[] importFromCSV(BufferedReader reader, boolean trim)
	{
		String[] keyList=null;
		LinkedList<String> outputList = new LinkedList<String>();
		
		String lineInput;
		boolean keyLine=true;
		String[] valueList;
		Hashtable<String,String> keyValuePairs;
		int totalKeys=0;
		try
		{
			
			while ((lineInput=reader.readLine())!=null)
			{
				if (keyLine)
				{
					keyLine=false;
					keyList = splitCommaDelimitedString(lineInput,',');
					totalKeys = keyList.length;
				}
				else
				{
					valueList=splitCommaDelimitedString(lineInput,',');
					// TODO: Figure out how to make this more efficient
					keyValuePairs= new Hashtable<String, String>();
					for (int i=0;i<totalKeys;i++)
					{
						if (valueList[i].length()>0)
						{
							if (trim)
								keyValuePairs.put(keyList[i].trim(), valueList[i].trim());
							else
								keyValuePairs.put(keyList[i].trim(), valueList[i]);
						}
					}
					if (keyValuePairs.size()>0)
					{
						outputList.add(serializeToString(keyValuePairs));
					}
					
				}
			}
		}
		catch (Exception e)
		{
			java.io.StringWriter traceText = new java.io.StringWriter();
			java.io.PrintWriter pWriter = new java.io.PrintWriter(traceText,true);
			e.printStackTrace(pWriter);
			pWriter.close();
			throw new RuntimeException(traceText.toString());
		}
		finally
		{
			if (reader!=null)
			{
				try
				{
					reader.close();
				}
				catch (Exception e2)
				{
					
				}
			}
		}
		
		return outputList.toArray(new String[0]);
	}
	
	
	private static void writeModelsToCSV(String outputFileFullName, String[] keys, String[] serializedWorlds)
	{
		BufferedWriter writer=null;
		try
		{
			File f = new File(outputFileFullName);
			if (f.exists())
				f.delete();
			
			writer = new BufferedWriter(new FileWriter(outputFileFullName));
			writeModelKeysToCSV(writer,keys);
			for (String serializedWorld:serializedWorlds)
			{
				writeModelStructureToCSV(writer,keys,serializedWorld);
			}
		}
		catch (Exception e)
		{
			java.io.StringWriter traceText = new java.io.StringWriter();
			java.io.PrintWriter pWriter = new java.io.PrintWriter(traceText,true);
			e.printStackTrace(pWriter);
			pWriter.close();
			throw new RuntimeException(traceText.toString());
		}
		finally
		{
			if (writer!=null)
			{
				try
				{
					writer.close();
				}
				catch (Exception e)
				{
					
				}
			}
		}
	}
	
	
	private static void writeModelKeysToCSV(BufferedWriter writer, String[] keys) throws IOException
	{
		StringBuilder sBuilder = new StringBuilder();
		int keyLength=keys.length;
		if (keyLength>0)
		{
			if (keys[0].length()>0)
			{
				sBuilder.append("\"");
				sBuilder.append(keys[0].replace((CharSequence)"\"",(CharSequence)"\"\""));
				sBuilder.append("\"");
			}
			
		}
		for (int i=1;i<keys.length;i++)
		{
			sBuilder.append(",");
			if (keys[i].length()>0)
			{
				sBuilder.append("\"");
				sBuilder.append(keys[i].replace((CharSequence)"\"",(CharSequence)"\"\""));
				sBuilder.append("\"");
			}
		}
		
		writer.write(sBuilder.toString());
		writer.newLine();
	}
	
	private static void writeModelStructureToCSV(BufferedWriter writer,String[] keys, String structureLine) throws IOException
	{
		Hashtable<String,String> table=parseToKeyValueMap(structureLine);
		
		StringBuilder sBuilder = new StringBuilder();
		int keyLength=keys.length;
		if (keyLength>0)
		{
			if (table.containsKey(keys[0]))
			{
				sBuilder.append("\"");
				sBuilder.append(table.get(keys[0]).replace((CharSequence)"\"",(CharSequence)"\"\""));
				sBuilder.append("\"");
			}
			
		}
		for (int i=1;i<keys.length;i++)
		{
			sBuilder.append(",");
			if (table.containsKey(keys[i]))
			{
				sBuilder.append("\"");
				sBuilder.append(table.get(keys[i]).replace((CharSequence)"\"",(CharSequence)"\"\""));
				sBuilder.append("\"");
			}
		}
		
		writer.write(sBuilder.toString());
		writer.newLine();
	}
	
	private static Hashtable<String,String> parseToKeyValueMap(String pairString)
	{
		Hashtable<String,String> out = new Hashtable<String, String>();
		if (pairString.length()>0)
		{
			KeyValuePair[] pairs = parseToKeyValuePairsFromString(pairString);
			for (KeyValuePair<String, String> kvPair:pairs)
			{
				out.put(kvPair.GetKey(), kvPair.GetValue());
			}
		}
		return out;
	}
	
	private static KeyValuePair[] parseToKeyValuePairsFromString(String pairString)
	{
		KeyValuePair[] pairs;
		String[] pairArray = pairString.split(PAIR_SEPARATOR);
		pairs = new KeyValuePair[pairArray.length];
		String[] kvPair;
		int index=0;
		for (String keyValue: pairArray)
		{
			kvPair=keyValue.split(KEY_VALUE_SEPARATOR);
			if (kvPair.length==1)
				pairs[index]=new KeyValuePair(kvPair[0], "");
			else
				pairs[index]=new KeyValuePair(kvPair[0], kvPair[1]);
			index++;
		}
		return pairs;
	}
	
	
	private static int addToModel(StructureModel model, String serializedMap)
	{
		Integer newKey=null;
		KeyValuePair[] pairs = parseToKeyValuePairsFromString(serializedMap);
		newKey=model.AddObservation(pairs);
		if (newKey==null)
			return -1;
		else
			return newKey.intValue();
	}
	
	private static int addToModel(StructureModel model, Map<String, String> inputMap)
	{
		Integer newKey=null;
		KeyValuePair[] pairs = new KeyValuePair[inputMap.size()];
		int i=0;
		for (String key:inputMap.keySet())
		{
			pairs[i++] = new KeyValuePair(key, inputMap.get(key));
		}
		newKey=model.AddObservation(pairs);
		if (newKey==null)
			return -1;
		else
			return newKey.intValue();
	}
}
