package com.evolved.automata.inference;
import java.util.*;
import com.evolved.automata.parser.*;

public class TraceStructure {
	Hashtable<String, String> traceData;
	CFGParser baseIndexMatcher;
	String subIndexPattern = "%1$s.%2$s";
	Matcher compiledState = null;
	
	public TraceStructure()
	{
		traceData = new Hashtable<String, String>();
		String[] indexGrammar = new String[]{
				"index = '#'+",
				"base_key = ('#' | '@' | '_' | '-' | '+')+",
				"index_pattern = base_key, ('.', index)?, ' '"
				
		};
		baseIndexMatcher = new CFGParser(indexGrammar);
		compiledState = baseIndexMatcher.parse("index_pattern");
	}
	
	public Hashtable<String, String> getTrace()
	{
		return traceData;
	}
	
	public void clear()
	{
		traceData = new Hashtable<String, String>();
	}
	
	public void addSliceToTrace(Hashtable<String, String> slice, int maxDepth)
	{
		InferenceTools.getShiftedOutput(traceData, slice, maxDepth);
	}
	
	public void shiftTrace(String[] keys, int maxDepth)
	{
		InferenceTools.getShiftedOutput(traceData, keys, maxDepth);
	}
	
	public static String[] getProjectionKeys(String keyName, int startIndex, int endIndex)
	{
		String[] output = new String[endIndex-startIndex+1];
		for (int i=startIndex;i<=endIndex;i++)
		{
			if (i==0)
				output[i] = keyName;
			else
				output[i] = keyName+"."+i;
		}
		return output;
	}
	
	public void updateBaseKeyValues(Hashtable<String, String> slice)
	{
		for (String key:slice.keySet())
		{
			traceData.put(key, slice.get(key));
		}
			
	}
	
	
	public String[] projectKey(String baseKey, int maxDepth)
	{
		LinkedList<String> keyValues = new LinkedList<String>();
		String value=null;
		
		StringBuilder base = null;
		
		for (int i=0;i<=maxDepth;i++)
		{
			if (i==0 && traceData.containsKey(baseKey))
			{
				keyValues.add(traceData.get(baseKey));
			}
			else
			{
				base = new StringBuilder();
				base.append(baseKey).append(".").append(i);
				if (traceData.containsKey(value=base.toString()))
					keyValues.add(traceData.get(value));
			}
		}
		
		
		return keyValues.toArray(new String[0]);
	}
	
	public void joinTrace(String baseKey, String baseValue, int maxDepth)
	{
		String layerKey=null;
		for (int i=0;i<=maxDepth;i++)
		{
			
			String matchPattern = constructDepthGrammar(i);
			for (String key:traceData.keySet())
			{
				if (baseIndexMatcher.match(key+" ", matchPattern, null))
				{
					layerKey = constructLayerKey(baseKey, i);
					traceData.put(layerKey, baseValue);
				}
			}
		}
		
	}
	
	private String constructLayerKey(String baseName, int index)
	{
		if (index==0)
			return baseName;
		else
			return String.format(subIndexPattern, baseName, index);
	}
	
	public String[] projectKey(String baseKey)
	{
		LinkedList<String> keyValues = new LinkedList<String>();
		String value=null;
		String matchPattern = constructProjectGrammar(baseKey);
		
		for (String key:traceData.keySet())
		{
			if (baseIndexMatcher.match(key+" ", matchPattern, null))
			{
				
				value = traceData.get(key);
				keyValues.add(value);
			}
		}
		return keyValues.toArray(new String[0]);
	}
	
	private String constructDepthGrammar(Integer depth)
	{
		if (depth==0)
			return "base_key, ' '";
		else
			return String.format("base_key, '.', \"%1$s \"", depth);
	}
	
	private String constructProjectGrammar(String key)
	{
		return String.format("\"%1$s\", ('.', index)?, ' '", key);
	}
	
	public Hashtable<String, String> projectLayer(int layerDepth)
	{
		Hashtable<String, String> layer = new Hashtable<String, String>();
		String matchPattern = constructDepthGrammar(layerDepth);
		for (String key:traceData.keySet())
		{
			if (baseIndexMatcher.match(key+" ", matchPattern, null))
				layer.put(key, traceData.get(key));
		}
		return layer;
	}
	
	public Hashtable<String, String> getLayersBelow(int maxDepth)
	{
		Hashtable<String, String> out = new Hashtable<String, String>(), layer;
		
		for (int i=0;i<=maxDepth;i++)
		{
			layer = projectLayer(i);
			out.putAll(layer);
		}
		return out;
	}
}
