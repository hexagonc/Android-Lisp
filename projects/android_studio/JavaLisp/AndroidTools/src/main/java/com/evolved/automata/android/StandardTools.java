package com.evolved.automata.android;
import com.evolved.automata.*;
import java.util.*;
import java.io.*;

public class StandardTools 
{
	
	public static final Hashtable<Integer, String> MONTH_NUMBER_TO_NAME_MAP;
	public static final Hashtable<Integer, String> DAY_OF_WEEK_NUMBER_TO_NAME_MAP;
	public static final Hashtable<Integer, String> MONTH_NUMBER_TO_SEASON_NAME_MAP;
	

		
	
	static 
	{
		MONTH_NUMBER_TO_NAME_MAP = new Hashtable<Integer, String>();
		MONTH_NUMBER_TO_NAME_MAP.put(1, "January");
		MONTH_NUMBER_TO_NAME_MAP.put(2, "February");
		MONTH_NUMBER_TO_NAME_MAP.put(3, "March");
		MONTH_NUMBER_TO_NAME_MAP.put(4, "April");
		MONTH_NUMBER_TO_NAME_MAP.put(5, "May");
		MONTH_NUMBER_TO_NAME_MAP.put(6, "June");
		MONTH_NUMBER_TO_NAME_MAP.put(7, "July");
		MONTH_NUMBER_TO_NAME_MAP.put(8, "August");
		MONTH_NUMBER_TO_NAME_MAP.put(9, "September");
		MONTH_NUMBER_TO_NAME_MAP.put(10, "October");
		MONTH_NUMBER_TO_NAME_MAP.put(11, "November");
		MONTH_NUMBER_TO_NAME_MAP.put(12, "December");
		
		
		DAY_OF_WEEK_NUMBER_TO_NAME_MAP = new Hashtable<Integer, String>();
		DAY_OF_WEEK_NUMBER_TO_NAME_MAP.put(1,"Sunday");
		DAY_OF_WEEK_NUMBER_TO_NAME_MAP.put(2,"Monday");
		DAY_OF_WEEK_NUMBER_TO_NAME_MAP.put(3,"Tuesday");
		DAY_OF_WEEK_NUMBER_TO_NAME_MAP.put(4,"Wednesday");
		DAY_OF_WEEK_NUMBER_TO_NAME_MAP.put(5,"Thursday");
		DAY_OF_WEEK_NUMBER_TO_NAME_MAP.put(6,"Friday");
		DAY_OF_WEEK_NUMBER_TO_NAME_MAP.put(7,"Saturday");
		
		MONTH_NUMBER_TO_SEASON_NAME_MAP = new Hashtable<Integer, String>();
		MONTH_NUMBER_TO_SEASON_NAME_MAP.put(1,"Winter");
		MONTH_NUMBER_TO_SEASON_NAME_MAP.put(2,"Winter");
		MONTH_NUMBER_TO_SEASON_NAME_MAP.put(3,"Spring");
		MONTH_NUMBER_TO_SEASON_NAME_MAP.put(4,"Spring");
		MONTH_NUMBER_TO_SEASON_NAME_MAP.put(5,"Spring");
		MONTH_NUMBER_TO_SEASON_NAME_MAP.put(6,"Summer");
		MONTH_NUMBER_TO_SEASON_NAME_MAP.put(7,"Summer");
		MONTH_NUMBER_TO_SEASON_NAME_MAP.put(8,"Summer");
		MONTH_NUMBER_TO_SEASON_NAME_MAP.put(9,"Fall");
		MONTH_NUMBER_TO_SEASON_NAME_MAP.put(10,"Fall");
		MONTH_NUMBER_TO_SEASON_NAME_MAP.put(11,"Winter");
		MONTH_NUMBER_TO_SEASON_NAME_MAP.put(12,"Winter");
	}
	
	
	
	

	
	public static void mergeKeyValues(Hashtable<String,String> sourceMap, Hashtable<String,String> targetMap)
	{
		for (String sourceKey:sourceMap.keySet().toArray(new String[0]))
		{
			targetMap.put(sourceKey, sourceMap.get(sourceKey));
		}
	}

	public static void transferKeyValues(Hashtable<String,String> sourceMap, Hashtable<String,String> targetMap)
	{
		for (String keyName:sourceMap.keySet().toArray(new String[0]))
		{
			if (targetMap.containsKey(keyName))
				targetMap.put(keyName, sourceMap.get(keyName));
		}
	}
	
	// babyfish
	public static String[] extractQueryList(Hashtable<String, String> keyValueMap)
	{
		LinkedList<String> query = new LinkedList<String>();
		for (String keyname:keyValueMap.keySet().toArray(new String[0]))
		{
			if (keyValueMap.get(keyname).length()==0)
			{
				query.add(keyname);
				keyValueMap.remove(keyname);
			}
		}
		String[] queryList = new String[0];
		if (query.size()>0)
			queryList = query.toArray(new String[0]);
		
		return queryList;
	}
	
	
	

	
	
	public static Hashtable<String, String> parseToKeyValueMapFromKeyValuePairs(KeyValuePair<String, String>[] pairs)
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
	
	public static KeyValuePair<String, String>[] parseToKeyValuePairsFromString(Hashtable<String, String> keyValueMap)
	{
		KeyValuePair<String, String>[] pairs;
		
		pairs = new KeyValuePair[keyValueMap.size()];
		int index=0;
		for (String keyName:keyValueMap.keySet().toArray(new String[0]))
		{
			pairs[index] = new KeyValuePair<String, String>(keyName, keyValueMap.get(keyName));
			index++;
		}
		return pairs;
	}
	
	

	
	public static String mapDatepartToStringName(int datepart, int value)
	{
		switch (datepart)
		{
			case Calendar.DAY_OF_WEEK:
				if (DAY_OF_WEEK_NUMBER_TO_NAME_MAP.containsKey(value))
					return DAY_OF_WEEK_NUMBER_TO_NAME_MAP.get(value);
				else
					return "";
			case Calendar.MONTH:
				if (MONTH_NUMBER_TO_NAME_MAP.containsKey(value))
					return MONTH_NUMBER_TO_NAME_MAP.get(value);
				else
					return "";	
		}
		return "";
	}
	
	// TODO: Make this smarter

	

	
	public static String mapValueToKey(Hashtable<String,String> map, String value)
	{
		String out=null;
		for (String key:map.keySet().toArray(new String[0]))
		{
			if (map.get(key).equals(value))
				return key;
		}
		return out;
	}
	

	
	public static int doLogicalStringComparison(java.lang.String string, java.lang.String string2)
	{
		Double lNum=new Double(0),rNum= new Double(0);
		try
		{
			lNum=new Double(string);
			rNum = new Double(string2);
		}
		catch (Exception e)
		{
			return 0;
		}
		return lNum.compareTo(rNum);
	}

	
	
	
	
	
	public static void writeModelKeysToCSV(BufferedWriter writer, String[] keys) throws IOException
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
	
	
	
	public static String[] splitCommaDelimitedString(String tokenizedString, char splitChar)
	{
		char[] chars = tokenizedString.toCharArray();
		StringBuilder word= new StringBuilder();
		LinkedList<String> words = new LinkedList<String>();
		
		char currentChar;
		
		
		boolean ignoringSplitToken=false;
		boolean parsingDelimitedQuoteP=false;
		boolean canParseDelimitedQuoteP=false;
		
		int totalChars=chars.length;
		for (int i=0;i<totalChars;i++)
		{
			canParseDelimitedQuoteP=i<totalChars-1;
			currentChar=chars[i];
			
			if (currentChar=='"')
			{
				if (parsingDelimitedQuoteP)
				{
					word.append(currentChar);
					parsingDelimitedQuoteP=false;
				}
				else
				{
					if ((canParseDelimitedQuoteP)&&(chars[i+1]=='"'))
						parsingDelimitedQuoteP=true;
					else
					{
						ignoringSplitToken=!ignoringSplitToken;
					}
				}
			}
			else
			{
				if (currentChar==splitChar)
				{
					if (ignoringSplitToken)
					{
						word.append(currentChar);
					}
					else
					{
						words.add(word.toString());
						word = new StringBuilder();
					}
				}
				else
					word.append(currentChar);
			}
			
		}
		words.add(word.toString());
		return words.toArray(new String[0]);
	}
	
	public static boolean subSetP(Hashtable<String,String> superset, Hashtable<String,String> subSet, String matchIndicator)
	{
		if ((subSet==null)||(subSet.size()==0))
			return true;
		
		if ((superset==null)||(superset.size()==0))
			return false;
		boolean atLeastOne=false;
		for (String key:subSet.keySet())
		{
			if (subSet.get(key).equals(matchIndicator))
			{
				atLeastOne=true;
				if (!superset.containsKey(key))
					return false;
			}
		}
		
		return atLeastOne;
	}
	

	
	
	/**
	 * 
	 * Returns true if the Subset map is a subset of superset 
	 * @param superset
	 * @param subSet
	 * @return 
	 */
	public static boolean subSetP(Hashtable<String,String> superset, Hashtable<String,String> subSet)
	{
		if ((subSet==null)||(subSet.size()==0))
			return true;
		
		if ((superset==null)||(superset.size()==0))
			return false;
		
		for (String key:subSet.keySet())
		{
			if (!superset.containsKey(key))
				return false;
			
			if (!subSet.get(key).equals(superset.get(key)))
			{
				return false;
			}
		}
		
		return true;
	}
	
	public static String join(Iterable<String> parts, String delimiter )
	{
		StringBuffer buffer = new StringBuffer("");
		boolean first=true;
		for (String part:parts)
		{
			if (first)
			{
				buffer.append(part);
				first=false;
			}
			else
			{
				buffer.append(delimiter);
				buffer.append(part);
			}
			
		}
		return buffer.toString();
	}
	
	public static String join(String[] parts, String delimiter )
	{
		StringBuffer buffer = new StringBuffer("");
		boolean first=true;
		for (String part:parts)
		{
			if (first)
			{
				buffer.append(part);
				first=false;
			}
			else
			{
				buffer.append(delimiter);
				buffer.append(part);
			}
			
		}
		return buffer.toString();
	}
	
}
