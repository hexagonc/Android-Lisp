package com.evolved.automata;

import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.PriorityQueue;
import java.util.Set;

import java.util.Vector;

public class PatternMap 
{
	HashMap<String, LinkedList<Integer>> _indexMap = new HashMap<String, LinkedList<Integer>>();
	HashMap<String, Integer> _incidenceMap = new HashMap<String, Integer>();
	Vector<KeyValuePair<Object, LinkedList<String>>> _index = new Vector<KeyValuePair<Object, LinkedList<String>>>();
	HashSet<Integer> _deletedSet = new HashSet<Integer>();
	
	public PatternMap()
	{
		
	}
	
	public boolean put(LinkedList<String> pattern, final Object key, boolean allowReplacement)
	{
		pattern = getNoDuplicateList(pattern);
		boolean removeDeletedP = false;
		int index = _index.size();
		if (_deletedSet.size()>0)
		{
			index = _deletedSet.iterator().next().intValue();
			removeDeletedP = true;
		}
		 
		LinkedList<Integer> indexReferences;
		int priorCount = 0;
		KeyValuePair<Object, LinkedList<String>> target;
		LinkedList<String> pdef;
		HashSet<String> keySet = new HashSet<String>();
		keySet.addAll(pattern);
		if (allowReplacement)
		{
			String token = pattern.getFirst();
			
			indexReferences = _indexMap.get(token);
			
			if (indexReferences!=null)
			{
				inner: for (Integer idx:indexReferences)
				{
					target = _index.get(idx.intValue());
					pdef = target.GetValue();
					if (pdef.size()!=pattern.size())
					{
						continue inner;
					}
					
					for (String t:pdef)
					{
						if (!keySet.contains(t))
							continue inner;
					}
					
					if (allowReplacement)
					{
						_index.set(idx.intValue(), new KeyValuePair<Object, LinkedList<String>>(key, pdef));
						
						return true;
					}
					else
					{
						
						return false;
					}
				}
			}
				
		}
		for (String token:keySet)
		{
			indexReferences = _indexMap.get(token);
			
			if (indexReferences==null)
			{
				_indexMap.put(token, indexReferences = new LinkedList<Integer>());
				priorCount = 0;
				_incidenceMap.put(token, Integer.valueOf(0));
			}
			else
				priorCount = _incidenceMap.get(token);
			
			_incidenceMap.put(token, Integer.valueOf(1 + priorCount));
			indexReferences.add(Integer.valueOf(index));
			
		}
		if (removeDeletedP)
		{
			_index.set(index, new KeyValuePair<Object, LinkedList<String>>(key, pattern));
			_deletedSet.remove(Integer.valueOf(index));
		}
		else
			_index.add(new KeyValuePair<Object, LinkedList<String>>(key, pattern));
		return true;
		
	}
	
	private void updateKeys(HashSet<String> keySet)
	{
		LinkedList<Integer> newList = null, oldList;
		for (String key:keySet)
		{
			oldList = _indexMap.get(key);
			newList = new LinkedList<Integer>();
			for (Integer idx:oldList)
			{
				if (!_deletedSet.contains(idx))
				{
					newList.add(idx);
				}
				else
				{
					_incidenceMap.put(key, Integer.valueOf(_incidenceMap.get(key).intValue() - 1));
				}
			}
			if (oldList.size()!=newList.size())
				_indexMap.put(key, newList);
			
		}
		
		
	}
	
	public boolean deleteValue(Object key)
	{
		HashSet<String> updateSet = new HashSet<String>();
		int i = 0;
		KeyValuePair<Object, LinkedList<String>> value;
		boolean deleted = false;
		for (i=0; i < _index.size();i++)
		{
			
			value = _index.get(i);
			if (value == null)
				continue;
			if (value.GetKey().equals(key))
			{
				deleted = true;
				_deletedSet.add(Integer.valueOf(i));
				updateSet.addAll(value.GetValue());
				_index.set(i, null);
			}
				
		}
		
		if (deleted)
		{
			updateKeys(updateSet);
			return deleted;
		}
		else
			return false;
		
	}
	
	private void deleteIndex(Integer index)
	{
		LinkedList<Integer> newList = null, oldList;
		for (String key:_index.get(index.intValue()).GetValue())
		{
			oldList = _indexMap.get(key);
			for (Integer idx:oldList)
			{
				if (idx.intValue() == index.intValue())
				{
					if (newList == null)
						newList = new LinkedList<Integer>();
					newList.add(idx);
				}
				else
				{
					_incidenceMap.put(key, Integer.valueOf(_incidenceMap.get(key).intValue() - 1));
				}
			}
			if (newList != null)
				_indexMap.put(key, newList);
			newList = null;
		}
	}
	
	public LinkedList<Object> deleteAll(LinkedList<String> pattern, double deleteThreshold)
	{
		pattern = getNoDuplicateList(pattern);
		double indexCount = _index.size() - _deletedSet.size();
		
		final HashSet<Integer> candidates = new HashSet<Integer>();
		
		LinkedList<Integer> indexReferences;
		HashSet<String> keySet = new HashSet<String>();
		for (String token:pattern)
		{
			indexReferences = _indexMap.get(token);
			keySet.add(token);
			if (indexReferences!=null)
			{
				for (Integer index:indexReferences)
				{
					candidates.add(index);
				}
			}
			
		}
		
		
		KeyValuePair<Object, LinkedList<String>> def;
		
		double unionCount = 0;
		double intersectCount = 0;
		
		HashSet<String> visitedSet;
		LinkedList<Object> out = new LinkedList<Object>();
		HashSet<String> updateSet = new HashSet<String>();
		
		for (Integer candidate:candidates)
		{
			unionCount = 0;
			intersectCount = 0;
			visitedSet = new HashSet<String>();
			def = _index.get(candidate.intValue());
			for (String patternToken:def.GetValue())
			{
				if (keySet.contains(patternToken))
				{
					intersectCount+= 1 - _incidenceMap.get(patternToken)/indexCount;
					visitedSet.add(patternToken);
					keySet.remove(patternToken);
				}
				unionCount+=1 - _incidenceMap.get(patternToken)/indexCount;
				
			}
			
			for (String remaining:keySet)
			{
				if (_incidenceMap.containsKey(remaining))
					unionCount+=1 - _incidenceMap.get(remaining)/indexCount;
				else
					unionCount+=1;
				
				visitedSet.add(remaining);
			}
			
			keySet = visitedSet;
			
			if (intersectCount/unionCount>=deleteThreshold)
			{
				_deletedSet.add(candidate);
				updateSet.addAll(def.GetValue());
				out.add(def.GetKey());
				_index.set(candidate, null);
			}
			
		}
		
		updateKeys(updateSet);
		
		return out;
	}
	
	private LinkedList<String> getNoDuplicateList(LinkedList<String> pattern)
	{
		HashSet<String> noDuplicateSet = new HashSet<String>();
		noDuplicateSet.addAll(pattern);
		pattern.clear();
		pattern.addAll(noDuplicateSet);
		return pattern;
	}
	
	public LinkedList<KeyValuePair<Object, Double>> get( LinkedList<String> pattern, int maxResults)
	{
		pattern = getNoDuplicateList(pattern);
		
		double indexCount = _index.size() - _deletedSet.size();
		
		if (indexCount == 0)
			return new LinkedList<KeyValuePair<Object,Double>>();
		
		final HashSet<Integer> candidates = new HashSet<Integer>();
		
		LinkedList<Integer> indexReferences;
		HashSet<String> keySet = new HashSet<String>();
		for (String token:pattern)
		{
			indexReferences = _indexMap.get(token);
			keySet.add(token);
			if (indexReferences!=null)
			{
				for (Integer index:indexReferences)
				{
					if (_index.get(index)!=null)
						candidates.add(index);
				}
			}
			
		}
		
		PriorityQueue<KeyValuePair<Object, Double>> scoreHeap = new PriorityQueue<KeyValuePair<Object,Double>>(1, new Comparator<KeyValuePair<Object, Double>>()
				{
					public int compare(KeyValuePair<Object,Double> left, KeyValuePair<Object,Double> right)
					{
						if (left.GetValue() > right.GetValue())
							return -1;
						else if (left.GetValue() == right.GetValue())
							return 0;
						else
							return 1;
					}
				});
		
		KeyValuePair<Object, LinkedList<String>> def;
		
		double unionCount = 0;
		double intersectCount = 0;
		
		HashSet<String> visitedSet;
		HashMap<Object, Double> scoreMap = new HashMap<Object, Double>();
		double score;
		Double oldScore;
		Object oldValue;
		for (Integer candidate:candidates)
		{
			unionCount = 0;
			intersectCount = 0;
			visitedSet = new HashSet<String>();
			def = _index.get(candidate.intValue());
			for (String patternToken:def.GetValue())
			{
				if (keySet.contains(patternToken))
				{
					if (indexCount == 1)
						intersectCount+=1;
					else
						intersectCount+= 1 - _incidenceMap.get(patternToken)/indexCount;
					visitedSet.add(patternToken);
					keySet.remove(patternToken);
				}
				if (indexCount == 1)
					unionCount++;
				else
					unionCount+=1 - _incidenceMap.get(patternToken)/indexCount;
				
			}
			
			for (String remaining:keySet)
			{
				if (_incidenceMap.containsKey(remaining))
					unionCount+=1 - _incidenceMap.get(remaining)/indexCount;
				else
					unionCount+=1;
				
				visitedSet.add(remaining);
			}
			
			keySet = visitedSet;
			score = intersectCount/unionCount;
			oldScore = scoreMap.get(def.GetKey());
			if (oldScore == null || oldScore.doubleValue() < score)
			{
				scoreHeap.add(new KeyValuePair<Object, Double>(def.GetKey(), score));
				scoreMap.put(def.GetKey(), Double.valueOf(score));
			}
			
				
		}
		
		LinkedList<KeyValuePair<Object, Double>> out = new LinkedList<KeyValuePair<Object,Double>>();
		KeyValuePair<Object, Double> result;
		
		int max = Math.min(maxResults, scoreHeap.size());
		for (int i=0;i<max;i++)
		{
			result = scoreHeap.poll();
			out.add(result);
		}
		return out;
	}
	
	public static double compareSets(Set<String> s1, Set<String> s2)
	{
		double intersectCount = 0;
		double unionCount = 0;
		for (String s:s1)
		{
			if (s2.contains(s))
				intersectCount++;
			unionCount++;
		}
		for (String t:s2)
		{
			if (!s1.contains(t))
				unionCount++;
		}
		
		return intersectCount/unionCount;
	}
	
}
