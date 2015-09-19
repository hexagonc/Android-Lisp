package com.evolved.automata.inference;
import java.util.*;
import java.io.*;

import com.evolved.automata.*;

public class StructureSlice implements Serializable 
{
	
	static final long serialVersionUID = 1;
	
	transient HashMap<String,String> sliceMap;
	KeyValuePair<String, String>[] i_keyValuepairs;

	transient int i_MaxBufferSize;
	transient int i_TopIndex;
	double paddingFraction=.25;
	
	public StructureSlice()
	{
		initialize();
	}
	
	public StructureSlice(int bufferMaxSize)
	{
		i_TopIndex=0;
		i_MaxBufferSize=bufferMaxSize;
		i_keyValuepairs=new KeyValuePair[bufferMaxSize];
		sliceMap = new HashMap<String, String>();
		
	}
	
	public StructureSlice(KeyValuePair<String, String>[] keyValuepairs)
	{
		initialize(keyValuepairs);
	}
	
	private void initialize(KeyValuePair<String, String>[] keyValuepairs)
	{
		if (keyValuepairs==null)
		{
			initialize();
			return;
		}
		i_MaxBufferSize=keyValuepairs.length;
		i_keyValuepairs=keyValuepairs;
		i_TopIndex=0;
		sliceMap = new HashMap<String, String>();
		for (KeyValuePair<String, String> kv:keyValuepairs)
		{
			if (kv!=null)
			{
				i_TopIndex++;
				sliceMap.put(kv.GetKey(), kv.GetValue());
			}
		}
	}
	
	private void initialize()
	{
		sliceMap = new HashMap<String, String>();
		i_TopIndex=i_MaxBufferSize=0;
	}
	
	public StructureSlice(KeyValuePair<String, String>[] keyValuepairs, int bufferMaxSize)
	{
		i_MaxBufferSize=keyValuepairs.length;
		i_keyValuepairs=keyValuepairs;
		sliceMap = new HashMap<String, String>();
		i_TopIndex=0;
		for (KeyValuePair<String, String> kv:keyValuepairs)
		{
			if (kv!=null)
			{
				sliceMap.put(kv.GetKey(), kv.GetValue());
				i_TopIndex++;
			}
			else
				break;
			
		}
	}
	
	public Map<String, String> getSliceMap()
	{
		return (Map<String, String>)sliceMap.clone();
	}
	
	public boolean AddKeyValuePair(String key, String value)
	{
		if (sliceMap.containsKey(key))
			return false;
		
		if (i_TopIndex>=i_MaxBufferSize)
			Reallocate();
		if (i_MaxBufferSize>i_TopIndex)
		{
			
			i_keyValuepairs[i_TopIndex]=new KeyValuePair<String, String>(key,value);
			i_TopIndex++;
			sliceMap.put(key, value);
			
			return true;
		}
		
		return false;
	}
	
	public String UpdateValue(String key, String value)
	{
		if (!sliceMap.containsKey(key))
			return null;
		String old;
		for (int i=0;i<i_TopIndex;i++)
		{
			if (i_keyValuepairs[i].GetKey().equals(key))
			{
				if (i_keyValuepairs[i].GetValue().equals(value))
					return value;
				old = i_keyValuepairs[i].GetValue();
				i_keyValuepairs[i]=new KeyValuePair<String, String>(key,value);
				sliceMap.put(key, value);
				return old;
			}
		}
		
		return null;
	}
	
	public String DeleteKey(String key)
	{
		if (!sliceMap.containsKey(key))
			return null;
		boolean found=false;
		String value=null;
		for (int i=0;i<i_TopIndex;i++)
		{
			if (!found)
			{
				
				if (i_keyValuepairs[i].GetKey().equals(key))
				{
					found=true;
					value = i_keyValuepairs[i].GetValue();
					i_keyValuepairs[i]=null;
					sliceMap.remove(key);
				}
			}
			else
			{
				i_keyValuepairs[i-1]=i_keyValuepairs[i];
			}
		}
		if (found)
		{
			i_TopIndex--;
			return value;
		}
		return null;
	}
	
	public String GetValue(String keyName)
	{
		return sliceMap.get(keyName);
	}
	
	public KeyValuePair<String, String>[] GetKeyValuePairs()
	{
		KeyValuePair<String, String>[] newKeys = new KeyValuePair[i_TopIndex];
		for (int i=0;i<i_TopIndex;i++)
			newKeys[i]=i_keyValuepairs[i];
		return newKeys;
	}
	
	public int Size()
	{
		return i_TopIndex;
	}

	private void Reallocate()
	{
		i_MaxBufferSize = Math.max(2, (int)(i_MaxBufferSize*paddingFraction))+i_MaxBufferSize;
		KeyValuePair<String, String>[] newPairs = new KeyValuePair[i_MaxBufferSize];
		for (int i=0;i<i_keyValuepairs.length;i++)
			newPairs[i]=i_keyValuepairs[i];
		i_keyValuepairs=newPairs;
	}
	
	private void readObject(ObjectInputStream s) throws IOException, ClassNotFoundException
	{
		s.defaultReadObject();
		initialize(i_keyValuepairs);
	}
}
