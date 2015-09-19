package com.evolved.automata.inference;



import java.util.*;
import java.io.*;

import com.evolved.automata.*;

public class StructureModel implements Serializable 
{
	static final long serialVersionUID = 1;
	
	StructureSlice[] i_WorldSlices;
	int[] i_PriorCounts;
	
	transient HashMap<String, HashMap<String, List<Integer>>> i_StructureMap;
	
	transient WorldAllocator i_Allocator;
	
	
	
	public StructureModel()
	{
		i_Allocator = new WorldAllocator(1000,this);
		i_StructureMap = new HashMap<String, HashMap<String,List<Integer>>>();
		i_WorldSlices = new StructureSlice[i_PriorCounts.length];
	}
	
	
	public StructureModel(int maxSize)
	{
		i_Allocator = new WorldAllocator(maxSize,this);
		i_StructureMap = new HashMap<String, HashMap<String,List<Integer>>>();
		i_WorldSlices = new StructureSlice[i_PriorCounts.length];
	}
	
	
	
	// This is called by the WorldAllocator
	public void SetPriorCounts(int[] priors)
	{
		i_PriorCounts=priors;
	}
	
	public String[] GetKeys()
	{
		String[] keys=new String[0];
		if (i_StructureMap==null)
			return keys;
		
		int index=0;
		keys = new String[i_StructureMap.size()];
		for (String keyName:i_StructureMap.keySet().toArray(new String[0]))
		{
			keys[index]=keyName;
			index++;
		}
		
		return keys;
	}
	
	public List<StructureSlice> GetAllWorlds()
	{
		LinkedList<StructureSlice> out = new LinkedList<StructureSlice>();
		for (int i=0;i<i_WorldSlices.length;i++)
		{
			if (!i_Allocator.IndexFreeP(i))
			{
				out.add(i_WorldSlices[i]);
			}
		}
		return out;
	}
	
	public String[] GetValuesForKey(String key)
	{
		LinkedList<String> values=new LinkedList<String>();
		
		HashMap<String, List<Integer>> valueNames;
		valueNames = i_StructureMap.get(key);
		
		if (valueNames==null)
			return new String[0];
		
		
		List worlds;
		for (String keyValue:valueNames.keySet().toArray(new String[0]))
		{
			worlds=valueNames.get(keyValue);
			if (worlds.size()>0)
			{
				values.add(keyValue);
			}
		}
		
		return values.toArray(new String[0]);
	}
	
	
	public boolean DeleteWorldsExplicit(int worldIndex)
	{
		return DeleteWorldsExplicit(new Integer[]{new Integer(worldIndex)});
	}
	
	
	/** This is the version of this method that is used when the key-value map has already been created from some
	 * other source.  It meant as a simple optimization to prevent duplicating effort
	 * 
	 * 
	 */
	private List<Integer> GetConsistentWorlds(HashMap<String, String> currentEvidenceMap, KeyValuePair[] evidenceList, double matchProb, double mismatchProb)
	{
		
		HashSet<Integer> testedWorlds = new HashSet<Integer>();
		HashMap<String, List<Integer>> evidenceValueSet;
		List<Integer> consistentWorldIndices;
		List<Integer> output = new LinkedList<Integer>();
		String consistentEvidenceValue;
		
		int evidenceEncounteredCount=0;
		double hypoProb;
		double minAffectiveProb=0.0;
		boolean cont;
		StructureSlice slice;
		
		for (KeyValuePair evidencePair:evidenceList)
		{
			evidenceValueSet = i_StructureMap.get(evidencePair.GetKey());
			// Allow for partial matches on the keys
			if (evidenceValueSet!=null)
			{
				consistentWorldIndices=evidenceValueSet.get(evidencePair.GetValue());
				if (consistentWorldIndices!=null)
				{
					for (Integer hypoIndex:consistentWorldIndices)
					{
						if (testedWorlds.contains(hypoIndex))
							continue;
						hypoProb=1.0;
						cont=true;
						slice = i_WorldSlices[hypoIndex.intValue()];
						evidenceEncounteredCount=0;
						for (KeyValuePair pair:slice.GetKeyValuePairs())
						{
							consistentEvidenceValue=currentEvidenceMap.get(pair.GetKey());
							if (consistentEvidenceValue!=null)
							{
								evidenceEncounteredCount++;
								if (hypoProb<minAffectiveProb)
								{
									cont=false;
									hypoProb=-1;
									break;
								}
								if (pair.GetValue().equals(consistentEvidenceValue))
									hypoProb*=matchProb;
								else
									hypoProb*=mismatchProb;
							}
							
						}
						if ((hypoProb>0)&&(evidenceEncounteredCount==evidenceList.length))
						{
							testedWorlds.add(hypoIndex);
							output.add(hypoIndex);
						}
						if (!cont)
							break;
					}
				}
			}
		}
		return output;
	}
	
	
	/** This is the workhorse function.  It returns all worlds consistent with a piece of evidence
	 * 
	 * 
	 * 
	 */
	public List<Integer> GetConsistentWorlds(KeyValuePair[] evidenceList, double matchProb, double mismatchProb)
	{
		HashMap<String, String> currentEvidenceMap = new HashMap<String, String>();
		HashSet<Integer> testedWorlds = new HashSet<Integer>();
		HashMap<String, List<Integer>> evidenceValueSet;
		List<Integer> consistentWorldIndices;
		List<Integer> output = new LinkedList<Integer>();
		String consistentEvidenceValue;
		
		int evidenceEncounteredCount=0;
		double hypoProb;
		double minAffectiveProb=0.0;
		boolean cont;
		StructureSlice slice;
		
		for (KeyValuePair<String, String> kvPair:evidenceList)
		{
			currentEvidenceMap.put(kvPair.GetKey(), kvPair.GetValue());
		}
		
		for (KeyValuePair<String, String> evidencePair:evidenceList)
		{
			evidenceValueSet = i_StructureMap.get(evidencePair.GetKey());
			// Allow for partial matches on the keys
			if (evidenceValueSet!=null)
			{
				consistentWorldIndices=evidenceValueSet.get(evidencePair.GetValue());
				if (consistentWorldIndices!=null)
				{
					for (Integer hypoIndex:consistentWorldIndices)
					{
						if (testedWorlds.contains(hypoIndex))
							continue;
						hypoProb=1.0;
						cont=true;
						slice = i_WorldSlices[hypoIndex.intValue()];
						evidenceEncounteredCount=0;
						for (KeyValuePair<String, String> pair:slice.GetKeyValuePairs())
						{
							consistentEvidenceValue=currentEvidenceMap.get(pair.GetKey());
							if (consistentEvidenceValue!=null)
							{
								evidenceEncounteredCount++;
								if (hypoProb<minAffectiveProb)
								{
									cont=false;
									hypoProb=-1;
									break;
								}
								if (pair.GetValue().equals(consistentEvidenceValue))
									hypoProb*=matchProb;
								else
									hypoProb*=mismatchProb;
							}
							
						}
						if ((hypoProb>0)&&(evidenceEncounteredCount==evidenceList.length))
						{
							testedWorlds.add(hypoIndex);
							output.add(hypoIndex);
						}
						if (!cont)
							break;
					}
				}
			}
		}
		return output;
	}
	
	/** This is a version of the method that is used for when evidence keys have already been converted into a 
	 *  HashMap by some other process.<br/>
	 * (1) if [requiredEvidenceList] is not null and not empty then any candidate slices must contain the listed
	 * 	   keys.  This is, in fact, a looser version of [strictP].  It allows you to specify which of the 
	 * 	   pairs in the [evidenceList] are required (instead of requiring all of them).<br/>
	 * @return Returns the allocation index of all consistent worlds 
	 * 
	 */
	private List<Integer> GetConsistentWorlds(HashMap<String, String> currentEvidenceMap, KeyValuePair[] evidenceList, String[] requiredEvidenceList)
	{
		double matchProb = 1.0;
		double mismatchProb = 0.0;
		
		HashSet<Integer> testedWorlds = new HashSet<Integer>();
		HashMap<String, List<Integer>> evidenceValueSet;
		List<Integer> consistentWorldIndices;
		List<Integer> output = new LinkedList<Integer>();
		HashSet<String> requiredEvidenceSet = new HashSet<String>();
		String consistentEvidenceValue;
		
		int evidenceEncounteredCount=0;
		double hypoProb;
		double minAffectiveProb=0.0;
		boolean cont;
		StructureSlice slice;
		
		
		int matchCount=0, matchTotal=0;
		boolean checkValue=false, extraKeysPresent=false;
		if ((requiredEvidenceList!=null)&&(requiredEvidenceList.length>0))
		{
			checkValue=true;
			for (String kvName:requiredEvidenceList)
			{
				requiredEvidenceSet.add(kvName);
				matchTotal++;
			}
		}
		
		
		
		for (KeyValuePair<String, String> evidencePair:evidenceList)
		{
			evidenceValueSet = i_StructureMap.get(evidencePair.GetKey());
			// Allow for partial matches on the keys
			if (evidenceValueSet!=null)
			{
				consistentWorldIndices=evidenceValueSet.get(evidencePair.GetValue());
				if (consistentWorldIndices!=null)
				{
					for (Integer hypoIndex:consistentWorldIndices)
					{
						matchCount=0;
						
						if (testedWorlds.contains(hypoIndex))
							continue;
						hypoProb=1.0;
						cont=true;
						slice = i_WorldSlices[hypoIndex.intValue()];
						evidenceEncounteredCount=0;
						for (KeyValuePair<String, String> pair:slice.GetKeyValuePairs())
						{
							consistentEvidenceValue=currentEvidenceMap.get(pair.GetKey());
							if (consistentEvidenceValue!=null)
							{
								evidenceEncounteredCount++;
								if (hypoProb<minAffectiveProb)
								{
									cont=false;
									hypoProb=-1;
									break;
								}
								if (pair.GetValue().equals(consistentEvidenceValue))
								{
									hypoProb*=matchProb;
								}
								else
									hypoProb*=mismatchProb;
							}
							if (checkValue)
							{
								if (requiredEvidenceSet.contains(pair.GetKey()))
								{
									matchCount++;
								}
								
							}
							
						}
						if ((hypoProb>0)&&(evidenceEncounteredCount==evidenceList.length)&&((matchCount==matchTotal&&checkValue) || !checkValue))
						{
							testedWorlds.add(hypoIndex);
							output.add(hypoIndex);
						}
						if (!cont)
							break;
					}
				}
			}
		}
		return output;
	}
	
	
	/** This is the workhorse method.  It returns the allocation indices of all StructureSlices where: <br/>
	 * (1) if strictP = true then it returns all slices whose key-value pairs are a subset of the key-value
	 * 	   pairs represented by [evidenceList]<br/> 
	 * (2) if [requiredEvidenceList] is not null and not empty then any candidate slices must contain the listed
	 * 	   keys.  This is, in fact, a looser version of [strictP].  It allows you to specify which of the 
	 * 	   pairs in the [evidenceList] are required (instead of requiring all of them).<br/>
	 * 
	 * 
	 */
	public List<Integer> GetConsistentWorlds(KeyValuePair[] evidenceList, double matchProb, double mismatchProb, boolean strictP, String[] requiredEvidenceList)
	{
		HashMap<String, String> currentEvidenceMap = new HashMap<String, String>();
		HashSet<Integer> testedWorlds = new HashSet<Integer>();
		HashMap<String, List<Integer>> evidenceValueSet;
		List<Integer> consistentWorldIndices;
		List<Integer> output = new LinkedList<Integer>();
		HashSet<String> requiredEvidenceSet = new HashSet<String>();
		String consistentEvidenceValue;
		
		int evidenceEncounteredCount=0;
		double hypoProb;
		double minAffectiveProb=0.0;
		boolean cont;
		StructureSlice slice;
		
		for (KeyValuePair<String, String> kvPair:evidenceList)
		{
			currentEvidenceMap.put(kvPair.GetKey(), kvPair.GetValue());
		}
		int matchCount=0, matchTotal=0;
		boolean checkValue=false, extraKeysPresent=false;
		if ((requiredEvidenceList!=null)&&(requiredEvidenceList.length>0))
		{
			checkValue=true;
			for (String kvName:requiredEvidenceList)
			{
				requiredEvidenceSet.add(kvName);
				matchTotal++;
			}
		}
		
		
		
		for (KeyValuePair<String, String> evidencePair:evidenceList)
		{
			evidenceValueSet = i_StructureMap.get(evidencePair.GetKey());
			// Allow for partial matches on the keys
			if (evidenceValueSet!=null)
			{
				consistentWorldIndices=evidenceValueSet.get(evidencePair.GetValue());
				if (consistentWorldIndices!=null)
				{
					for (Integer hypoIndex:consistentWorldIndices)
					{
						matchCount=0;
						extraKeysPresent=!checkValue||(checkValue&&matchTotal==evidenceList.length);
						if (testedWorlds.contains(hypoIndex))
							continue;
						hypoProb=1.0;
						cont=true;
						slice = i_WorldSlices[hypoIndex.intValue()];
						evidenceEncounteredCount=0;
						for (KeyValuePair pair:slice.GetKeyValuePairs())
						{
							consistentEvidenceValue=currentEvidenceMap.get(pair.GetKey());
							if (consistentEvidenceValue!=null)
							{
								evidenceEncounteredCount++;
								if (hypoProb<minAffectiveProb)
								{
									cont=false;
									hypoProb=-1;
									break;
								}
								if (pair.GetValue().equals(consistentEvidenceValue))
								{
									hypoProb*=matchProb;
									if (checkValue)
									{
										if (requiredEvidenceSet.contains(pair.GetKey()))
										{
											matchCount++;
										}
										else
											extraKeysPresent=true;
									}
								}
								else
									hypoProb*=mismatchProb;
							}
							
						}
						if ((hypoProb>0)&&((!strictP&&matchCount==matchTotal&&extraKeysPresent)||(evidenceEncounteredCount==evidenceList.length)))
						{
							testedWorlds.add(hypoIndex);
							output.add(hypoIndex);
						}
						if (!cont)
							break;
					}
				}
			}
		}
		return output;
	}
	
	
	/** This is the workhorse method.  It returns the allocation indices of all StructureSlices where: <br/>
	 * (1) if strictP = true then it returns all slices whose key-value pairs are a subset of the key-value
	 * 	   pairs represented by [evidenceList]<br/> 
	 * (2) if [requiredEvidenceList] is not null and not empty then any candidate slices must contain the listed
	 * 	   keys.  This is, in fact, a looser version of [strictP].  It allows you to specify which of the 
	 * 	   pairs in the [evidenceList] are required (instead of requiring all of them).<br/>
	 * 
	 * 
	 */
	/*
	public List<Integer> GetConsistentWorlds(KeyValuePair[] evidenceList, String[] requiredKeys, com.evolved.automata.alisp.Environment env, Hashtable<String, String> udCompKeys)
	{
		HashMap<String, String> currentEvidenceMap = new HashMap<String, String>(), totalEvidenceMap = new HashMap<String, String>();
		LinkedList<KeyValuePair> pList = new LinkedList<KeyValuePair>();
		for (KeyValuePair<String, String> kvPair:evidenceList)
		{
			if (udCompKeys==null || udCompKeys.size() == 0 || !udCompKeys.containsKey(kvPair.GetKey()))
			{
				pList.add(kvPair);
				currentEvidenceMap.put(kvPair.GetKey(), kvPair.GetValue());
			}
			totalEvidenceMap.put(kvPair.GetKey(), kvPair.GetValue());
		}
		List<Integer> finalOutput = new LinkedList<Integer>(), consistentIndices = GetConsistentWorlds(currentEvidenceMap, pList.toArray(new KeyValuePair[0]),  requiredKeys);
		if (consistentIndices==null||consistentIndices.size()==0)
		{
			return finalOutput;
		}
		else
		{
			StructureSlice slice=null;
			
			cont: for (Integer aIndex:consistentIndices)
			{
				slice = GetSliceExplicit(aIndex);
				for (KeyValuePair<String, String> kv:slice.GetKeyValuePairs())
				{
					if (udCompKeys.containsKey(kv.GetKey())&&totalEvidenceMap.containsKey(kv.GetKey()))
					{
						if (!evaluatePredicate(env, udCompKeys.get(kv.GetKey()), totalEvidenceMap.get(kv.GetKey()), kv.GetValue()))
						{
							continue cont;
						}
					}
				}
				finalOutput.add(aIndex);
			}
			return finalOutput;
			
		}
	}
	*/
	
	public StructureSlice GetSliceExplicit(Integer worldId)
	{
		if ((!i_Allocator.ValidIndexP(worldId.intValue()))||(i_Allocator.IndexFreeP(worldId.intValue())))
			return null;
		else
			return i_WorldSlices[worldId.intValue()];
		
	}
	
	/*
	public List<StructureSlice> GetConsistentSlices(KeyValuePair[] evidenceList, String[] requiredKeys, com.evolved.automata.alisp.Environment env, Hashtable<String, String> udCompKeys)
	{
		List<StructureSlice> slices = new LinkedList<StructureSlice>();
		List<Integer> worlds = GetConsistentWorlds(evidenceList, requiredKeys, env, udCompKeys);
		if (worlds!=null&&worlds.size()>0)
		{
			for (Integer index:worlds)
			{
				slices.add(GetSliceExplicit(index));
			}
			return slices;
		}
		else
			return slices;
	}
	*/
	
	/**
	 * Returns all slices that are a superset of the evidenceList.
	 * @param evidenceList Array of KeyValuePair representing evidence variables
	 * @param matchProb -- weight to assign structure slices that match the evidence list
	 * @param mismatchProb -- weight to assign structure slices that contradict the evidence list
	 * @return A list of StructureSlices.  This list will be non-null but empty if there are no consistent slices
	 */
	public List<StructureSlice> GetConsistentSlices(KeyValuePair[] evidenceList, double matchProb, double mismatchProb)
	{
		return GetConsistentSlices(evidenceList,matchProb,mismatchProb,true);
	}
	
	/**
	 * This is the workhorse function.  It returns all worlds consistent with a piece of evidence
	 * 
	 * @param evidenceList Array of evidence KeyValuePairs that represent what is known for certain about a structure
	 * @param matchProb weight to assign structure slices that are consistent with this evidence
	 * @param mismatchProb weight to assign structure slices that are not consistent with the evidence
	 * @param strictP If true, the only consistent structure slices are ones whose evidence are a super-set of @evidenceList if false then a 
	 * 		          a structure slice is counted so long as at least one evidence pair intersects @evidenceList and none contradict it
	 * @return LinkedList of StructureSlices.  Never null.  If there are no consistent slices then this returns an empty list
	 */
	public List<StructureSlice> GetConsistentSlices(KeyValuePair[] evidenceList, double matchProb, double mismatchProb, boolean strictP)
	{
		HashMap<String, String> currentEvidenceMap = new HashMap<String, String>();
		HashSet<Integer> testedWorlds = new HashSet<Integer>();
		HashMap<String, List<Integer>> evidenceValueSet;
		List<Integer> consistentWorldIndices;
		String consistentEvidenceValue;
		List<StructureSlice> outSlice = new LinkedList<StructureSlice>();
		
		double hypoProb;
		double minAffectiveProb=0.0;
		boolean cont;
		StructureSlice slice;
		int evidenceEncounteredCount=0;
		for (KeyValuePair<String, String> kvPair:evidenceList)
		{
			currentEvidenceMap.put(kvPair.GetKey(), kvPair.GetValue());
		}
		
		for (KeyValuePair evidencePair:evidenceList)
		{
			evidenceValueSet = i_StructureMap.get(evidencePair.GetKey());
			// Allow for partial matches on the keys
			if (evidenceValueSet!=null)
			{
				consistentWorldIndices=evidenceValueSet.get(evidencePair.GetValue());
				if (consistentWorldIndices!=null)
				{
					for (Integer hypoIndex:consistentWorldIndices)
					{
						if (testedWorlds.contains(hypoIndex))
							continue;
						hypoProb=1.0;
						cont=true;
						evidenceEncounteredCount=0;
						slice = i_WorldSlices[hypoIndex.intValue()];
						for (KeyValuePair pair:slice.GetKeyValuePairs())
						{
							consistentEvidenceValue=currentEvidenceMap.get(pair.GetKey());
							if (consistentEvidenceValue!=null)
							{
								evidenceEncounteredCount++;
								if (hypoProb<minAffectiveProb)
								{
									cont=false;
									hypoProb=-1;
									break;
								}
								if (pair.GetValue().equals(consistentEvidenceValue))
									hypoProb*=matchProb;
								else
									hypoProb*=mismatchProb;
							}
							
						}
						if ((hypoProb>0)&&((!strictP)||(evidenceEncounteredCount==evidenceList.length)))
						{
							testedWorlds.add(hypoIndex);
							outSlice.add(i_WorldSlices[hypoIndex]);
						}
						if (!cont)
							break;
					}
				}
			}
		}
		return outSlice;
	}
	
	/**
	 * Returns all structures with a non-empty intersection with the evidence list and
	 * that are a superset of the requiredEvidenceList
	 * @param evidenceList
	 * @param matchProb
	 * @param mismatchProb
	 * @param strictP
	 * @param requiredEvidenceList
	 * @return
	 */
	public List<StructureSlice> GetConsistentSlices(KeyValuePair<String, String>[] evidenceList, double matchProb, double mismatchProb, boolean strictP, String[] requiredEvidenceList)
	{
		HashMap<String, String> currentEvidenceMap = new HashMap<String, String>();
		HashSet<Integer> testedWorlds = new HashSet<Integer>();
		HashMap<String, List<Integer>> evidenceValueSet;
		HashSet<String> requiredEvidenceSet = new HashSet<String>();
		
		List<Integer> consistentWorldIndices;
		String consistentEvidenceValue;
		List<StructureSlice> outSlice = new LinkedList<StructureSlice>();
		
		double hypoProb;
		double minAffectiveProb=0.0;
		boolean cont;
		StructureSlice slice;
		int evidenceEncounteredCount=0;
		
		for (KeyValuePair<String, String> kvPair:evidenceList)
		{
			currentEvidenceMap.put(kvPair.GetKey(), kvPair.GetValue());
		}
		int matchCount=0, matchTotal=0;
		boolean checkValue=false, extraKeysPresent=false;
		if ((requiredEvidenceList!=null)&&(requiredEvidenceList.length>0))
		{
			checkValue=true;
			for (String kvName:requiredEvidenceList)
			{
				requiredEvidenceSet.add(kvName);
				matchTotal++;
			}
		}
		
		
		
		for (KeyValuePair<String, String> evidencePair:evidenceList)
		{
			evidenceValueSet = i_StructureMap.get(evidencePair.GetKey());
			// Allow for partial matches on the keys
			if (evidenceValueSet!=null)
			{
				consistentWorldIndices=evidenceValueSet.get(evidencePair.GetValue());
				if (consistentWorldIndices!=null)
				{
					for (Integer hypoIndex:consistentWorldIndices)
					{
						matchCount=0;
						extraKeysPresent=!checkValue||(checkValue&&matchTotal==evidenceList.length);
						if (testedWorlds.contains(hypoIndex))
							continue;
						hypoProb=1.0;
						cont=true;
						evidenceEncounteredCount=0;
						slice = i_WorldSlices[hypoIndex.intValue()];
						for (KeyValuePair<String, String> pair:slice.GetKeyValuePairs())
						{
							consistentEvidenceValue=currentEvidenceMap.get(pair.GetKey());
							if (consistentEvidenceValue!=null)
							{
								evidenceEncounteredCount++;
								if (hypoProb<=minAffectiveProb)
								{
									cont=false;
									hypoProb=-1;
									break;
								}
								if (pair.GetValue().equals(consistentEvidenceValue))
								{
									hypoProb*=matchProb;
									if (checkValue)
									{
										if (requiredEvidenceSet.contains(pair.GetKey()))
										{
											matchCount++;
										}
										else
											extraKeysPresent=true;
									}
								}
								else
									hypoProb*=mismatchProb;
							}
							
						}
						if ((hypoProb>0)&&((!strictP&&matchCount==matchTotal&&extraKeysPresent)||(evidenceEncounteredCount==evidenceList.length)))
						{
							testedWorlds.add(hypoIndex);
							outSlice.add(i_WorldSlices[hypoIndex]);
						}
						if (!cont)
							break;
					}
				}
			}
		}
		return outSlice;
	}
	
	/**
	 * Returns all structures with a non-empty intersection with the evidence list. <br/>
	 * If [requiredEvidenceList] is not null, then all worlds must have a key that is in this list <br/>
	 * whose value matches the key in the evidence list. <br/>
	 * In contrast, if [weightedKeyList] is not null, then worlds with these keys will only be included <br/>
	 * If the evidence contains these keys and have the same value
	 * @param evidenceList
	 * 
	 * @param requiredEvidenceList
	 * @param weightedKeyList 
	 * @return
	 */
	public List<StructureSlice> GetConsistentSlices(KeyValuePair<String, String>[] evidenceList, String[] requiredEvidenceList, String[] weightedKeyList)
	{
		double matchProb = 1;
		double mismatchProb = 0;
		boolean strictP = false;
		HashMap<String, String> currentEvidenceMap = new HashMap<String, String>();
		HashSet<Integer> testedWorlds = new HashSet<Integer>();
		HashMap<String, List<Integer>> evidenceValueSet;
		HashSet<String> requiredEvidenceSet = new HashSet<String>();
		HashSet<String> weightedKeys = new HashSet<String>();
		if (weightedKeyList!=null)
		{
			for (String v:weightedKeyList)
				weightedKeys.add(v);
		}
		List<Integer> consistentWorldIndices;
		String consistentEvidenceValue;
		List<StructureSlice> outSlice = new LinkedList<StructureSlice>();
		
		double hypoProb;
		double minAffectiveProb=0.0;
		boolean cont;
		StructureSlice slice;
		int evidenceEncounteredCount=0;
		
		for (KeyValuePair<String, String> kvPair:evidenceList)
		{
			currentEvidenceMap.put(kvPair.GetKey(), kvPair.GetValue());
		}
		int matchCount=0, matchTotal=0;
		boolean checkValue=false, extraKeysPresent=false;
		if ((requiredEvidenceList!=null)&&(requiredEvidenceList.length>0))
		{
			checkValue=true;
			for (String kvName:requiredEvidenceList)
			{
				requiredEvidenceSet.add(kvName);
				matchTotal++;
			}
		}
		
		
		
		for (KeyValuePair<String, String> evidencePair:evidenceList)
		{
			evidenceValueSet = i_StructureMap.get(evidencePair.GetKey());
			// Allow for partial matches on the keys
			if (evidenceValueSet!=null)
			{
				consistentWorldIndices=evidenceValueSet.get(evidencePair.GetValue());
				if (consistentWorldIndices!=null)
				{
					for (Integer hypoIndex:consistentWorldIndices)
					{
						matchCount=0;
						extraKeysPresent=!checkValue||(checkValue&&matchTotal==evidenceList.length);
						if (testedWorlds.contains(hypoIndex))
							continue;
						hypoProb=1.0;
						cont=true;
						evidenceEncounteredCount=0;
						slice = i_WorldSlices[hypoIndex.intValue()];
						for (KeyValuePair<String, String> pair:slice.GetKeyValuePairs())
						{
							consistentEvidenceValue=currentEvidenceMap.get(pair.GetKey());
							if (consistentEvidenceValue!=null)
							{
								evidenceEncounteredCount++;
								if (hypoProb<minAffectiveProb)
								{
									cont=false;
									hypoProb=-1;
									break;
								}
								if (pair.GetValue().equals(consistentEvidenceValue))
								{
									hypoProb*=matchProb;
									if (checkValue)
									{
										if (requiredEvidenceSet.contains(pair.GetKey()))
										{
											matchCount++;
										}
										else
											extraKeysPresent=true;
									}
								}
								else
									hypoProb*=mismatchProb;
							}
							else
							{
								if (weightedKeys.contains(pair.GetKey()))
								{
									cont=false;
									hypoProb=-1;
									break;
								}
							}
							
						}
						if ((hypoProb>0)&&((!strictP&&matchCount==matchTotal&&extraKeysPresent)||(evidenceEncounteredCount==evidenceList.length)))
						{
							testedWorlds.add(hypoIndex);
							outSlice.add(i_WorldSlices[hypoIndex]);
						}
						if (!cont)
							break;
					}
				}
			}
		}
		return outSlice;
	}
	
	public  HashMap<String, HashMap<String, Double>> OptimizedInference(KeyValuePair<String, String>[] evidenceList, String[] queryList, double matchProb, double mismatchProb)
	{
		HashMap<String, String> currentEvidenceMap = new HashMap<String, String>();
		
		double hypoProb;
		Double prob;
		StructureSlice slice;

	    HashMap<String, HashMap<String, Double>> inferredValueSet = new HashMap<String, HashMap<String,Double>>();
	    HashMap<String, Double> valueProbMap;
	    
	    /*  Begin of Modified area */
	    int evidenceLength=evidenceList.length;
	    int matchCount=0;
	    String expectedValue;
	    String currentEvidenceValue;
	    // Step 0 Add evidence to map
	    
	    for (KeyValuePair<String, String> kvPair:evidenceList)
		{
			currentEvidenceMap.put(kvPair.GetKey(), kvPair.GetValue());
		}
	    
	    // Step 1: Get all worlds that contain the query variable
	    HashMap<String, List<Integer>> queryValues;
	    List<Integer> queryWorlds;
	    
	    for (String queryKey:queryList)
	    {
	    	queryValues = i_StructureMap.get(queryKey);
	    	for (String queryValue:queryValues.keySet().toArray(new String[0]))
	    	{
	    		queryWorlds=queryValues.get(queryValue);
	    		for (Integer world:queryWorlds)
	    		{
	    			slice = i_WorldSlices[world.intValue()];
	    			hypoProb=1.0;
	    			matchCount=0;
	    			for (KeyValuePair<String, String> pair:slice.GetKeyValuePairs())
					{
						// Loop over all data in the slice and make sure that if that evidence key is present, then 
	    				// it is the same as in the slice
						
	    				currentEvidenceValue = currentEvidenceMap.get(pair.GetKey());
	    				expectedValue = pair.GetValue();
	    				if ((currentEvidenceValue!=null)&&(!currentEvidenceValue.equals(expectedValue)))
	    				{
	    					hypoProb=0.0;
	    					break;
	    				}
	    				else
	    				{
	    					if (currentEvidenceValue!=null)
	    					{
	    						matchCount++;
	    						if (matchCount==evidenceLength)
	    							break;
	    					}
	    					
	    				}
											
					}
	    			
	    			if (hypoProb>0)
					{
						valueProbMap=inferredValueSet.get(queryKey);
						if (valueProbMap==null)
						{
							valueProbMap = new HashMap<String, Double>();
							inferredValueSet.put(queryKey, valueProbMap);
						}
						prob = valueProbMap.get(queryValue);
						if (prob==null)
						{
							prob=new Double(hypoProb*i_PriorCounts[world.intValue()]);
							
						}
						else
						{
							prob=new Double(prob.doubleValue()+hypoProb*i_PriorCounts[world.intValue()]);
						}
						valueProbMap.put(queryValue, prob);
					}
					
	    		}
	    		
	    	}
	    }
	   
		return inferredValueSet;
		
	}

	
	public  HashMap<String, HashMap<String, Double>> OptimizedInference(KeyValuePair<String, String>[] evidenceList, String[] queryList, double matchProb, double mismatchProb, String[] requiredKeys)
	{
		HashMap<String, String> currentEvidenceMap = new HashMap<String, String>();
		
		double hypoProb;
		Double prob;
		StructureSlice slice;

	    HashMap<String, HashMap<String, Double>> inferredValueSet = new HashMap<String, HashMap<String,Double>>();
	    HashMap<String, Double> valueProbMap;
	    HashSet<String> requiredEvidenceSet = new HashSet<String>();
	    /*  Begin of Modified area */
	    int evidenceLength=evidenceList.length;
	    int matchCount=0, matchTotal=0;
	    String expectedValue;
	    String currentEvidenceValue;
	    // Step 0 Add evidence to map
	    
	    for (KeyValuePair<String, String> kvPair:evidenceList)
		{
			currentEvidenceMap.put(kvPair.GetKey(), kvPair.GetValue());
		}
	    
	    // Step 1: Get all worlds that contain the query variable
	    HashMap<String, List<Integer>> queryValues;
	    List<Integer> queryWorlds;
	    
	    boolean checkValue=false;
		if ((requiredKeys!=null)&&(requiredKeys.length>0))
		{
			checkValue=true;
			for (String kvName:requiredKeys)
			{
				requiredEvidenceSet.add(kvName);
				matchTotal++;
			}
		}
		
	    for (String queryKey:queryList)
	    {
	    	queryValues = i_StructureMap.get(queryKey);
	    	for (String queryValue:queryValues.keySet().toArray(new String[0]))
	    	{
	    		queryWorlds=queryValues.get(queryValue);
	    		for (Integer world:queryWorlds)
	    		{
	    			slice = i_WorldSlices[world.intValue()];
	    			hypoProb=1.0;
	    			matchCount=0;
	    			for (KeyValuePair<String, String> pair:slice.GetKeyValuePairs())
					{
						// Loop over all data in the slice and make sure that if that evidence key is present, then 
	    				// it is the same as in the slice
						
	    				currentEvidenceValue = currentEvidenceMap.get(pair.GetKey());
	    				expectedValue = pair.GetValue();
	    				if ((currentEvidenceValue!=null)&&(!currentEvidenceValue.equals(expectedValue)))
	    				{
	    					hypoProb=0.0;
	    					break;
	    				}
	    				else
	    				{
	    					if (currentEvidenceValue!=null)
	    					{
	    						if (checkValue)
								{
									if (requiredEvidenceSet.contains(pair.GetKey()))
									{
										matchCount++;
									}
									
								}
	    						else
	    							matchCount++;
	    						if (matchCount==evidenceLength)
	    							break;
	    					}
	    					
	    				}
											
					}
	    			
	    			if ((hypoProb>0)&&(!checkValue||(checkValue&&matchCount==matchTotal)))
					{
						valueProbMap=inferredValueSet.get(queryKey);
						if (valueProbMap==null)
						{
							valueProbMap = new HashMap<String, Double>();
							inferredValueSet.put(queryKey, valueProbMap);
						}
						prob = valueProbMap.get(queryValue);
						if (prob==null)
						{
							prob=new Double(hypoProb*i_PriorCounts[world.intValue()]);
							
						}
						else
						{
							prob=new Double(prob.doubleValue()+hypoProb*i_PriorCounts[world.intValue()]);
						}
						valueProbMap.put(queryValue, prob);
					}
					
	    		}
	    		
	    	}
	    }
	   
		return inferredValueSet;
		
	}
	
	public Integer AddObservation(KeyValuePair<String, String>[] evidenceList)
	{
		
		HashMap<String, String> currentEvidenceMap = new HashMap<String, String>();
		HashSet<Integer> testedWorlds = new HashSet<Integer>();
		HashMap<String, List<Integer>> evidenceValueSet;
		List<Integer> consistentWorldIndices;

		String consistentEvidenceValue;
		
		
		Integer newWorldIndex=null;
		StructureSlice slice;
		int valueMatchCount, varMatchCount, hypoLength, eviLength=evidenceList.length;
		
		for (KeyValuePair<String, String> kvPair:evidenceList)
		{
			currentEvidenceMap.put(kvPair.GetKey(), kvPair.GetValue());
		}
		boolean newHypothesis=false;
		if (evidenceList!=null && eviLength>0)
			newHypothesis=true;
		
		if (newHypothesis)
		{
			for (KeyValuePair<String, String> evidencePair:evidenceList)
			{
				evidenceValueSet=i_StructureMap.get(evidencePair.GetKey());
				if (evidenceValueSet!=null)
				{
					consistentWorldIndices=evidenceValueSet.get(evidencePair.GetValue());
					if (consistentWorldIndices!=null)
					{
					
						for (Integer hypoIndex:consistentWorldIndices)
						{
							if (testedWorlds.contains(hypoIndex))
								continue;
							slice=i_WorldSlices[hypoIndex.intValue()];
							valueMatchCount=0;
							varMatchCount=0;
							hypoLength=slice.Size();
							
							for (KeyValuePair<String, String> pair:slice.GetKeyValuePairs())
							{
								if (varMatchCount==eviLength)
									break;
								consistentEvidenceValue=currentEvidenceMap.get(pair.GetKey());
								if (consistentEvidenceValue!=null)
								{
									varMatchCount++;
									if (consistentEvidenceValue.equals(pair.GetValue()))
									{
										valueMatchCount++;
									}
									else
									{
										valueMatchCount=0;
										break;
									}
								}
							}
							
							testedWorlds.add(hypoIndex);
							// Check if this world is the same as the evidence
							if (valueMatchCount>0)
							{
								if ((hypoLength==valueMatchCount)&&(valueMatchCount==eviLength))
								{
									newHypothesis=false;
								}
								if (valueMatchCount==hypoLength) // evidence is a subset of this world
								{
									i_Allocator.UpdatePrior(hypoIndex);
								}
							}
						}
					}
				}
			}
			if (newHypothesis)
			{
				// Create new slice
				slice = new StructureSlice(evidenceList);
				int newIndex=i_Allocator.AllocateNewWorldGuarantee();
				i_WorldSlices[newIndex]=slice;
				i_Allocator.UpdatePrior(newIndex);
				
				// Add this evidence to the structure map
				for (KeyValuePair<String, String> evidencePair:evidenceList)
				{
					evidenceValueSet=i_StructureMap.get(evidencePair.GetKey());
					if (evidenceValueSet==null)
					{
						evidenceValueSet = new HashMap<String, List<Integer>>();
						i_StructureMap.put(evidencePair.GetKey(), evidenceValueSet);
					}
					// Create value to world map
					consistentWorldIndices=evidenceValueSet.get(evidencePair.GetValue());
					if (consistentWorldIndices==null)
					{
						consistentWorldIndices = new LinkedList<Integer>();
						evidenceValueSet.put(evidencePair.GetValue(), consistentWorldIndices);
					}
					newWorldIndex=new Integer(newIndex);
					consistentWorldIndices.add(newWorldIndex);
				}
			}
		}
		return newWorldIndex;
		
	}
	
	/*
	public Integer AddObservation(KeyValuePair<String, String>[] evidenceList, com.evolved.automata.alisp.Environment env, Hashtable<String, String> udCompKeys)
	{
		HashMap<String, String> currentEvidenceMap = new HashMap<String, String>(), totalEvidenceMap = new HashMap<String, String>();
		LinkedList<KeyValuePair<String, String>> pList = new LinkedList<KeyValuePair<String, String>>();
		for (KeyValuePair<String, String> kvPair:evidenceList)
		{
			if (udCompKeys==null || udCompKeys.size() == 0 || !udCompKeys.containsKey(kvPair.GetKey()))
			{
				pList.add(kvPair);
				currentEvidenceMap.put(kvPair.GetKey(), kvPair.GetValue());
			}
			totalEvidenceMap.put(kvPair.GetKey(), kvPair.GetValue());
		}
		List<Integer> consistentIndices = GetConsistentWorlds(currentEvidenceMap, pList.toArray(new KeyValuePair[0]), totalEvidenceMap.keySet().toArray(new String[0]) );
		if (consistentIndices==null||consistentIndices.size()==0)
		{
			return createStructureDirectly(evidenceList);
		}
		else
		{
			StructureSlice slice=null;
			
			cont: for (Integer aIndex:consistentIndices)
			{
				slice = GetSliceExplicit(aIndex);
				for (KeyValuePair<String, String> kv:slice.GetKeyValuePairs())
				{
					if (udCompKeys.containsKey(kv.GetKey())&&totalEvidenceMap.containsKey(kv.GetKey()))
					{
						if (!evaluatePredicate(env, udCompKeys.get(kv.GetKey()), totalEvidenceMap.get(kv.GetKey()), kv.GetValue()))
						{
							continue cont;
						}
					}
					
				}
				return null; // This means that the evidence-key-value list already exists according
				       		// to the criteria permitted by the predicate functions
			}
			return createStructureDirectly(evidenceList);
			
		}
		
		
	}
	
	
	private boolean evaluatePredicate(com.evolved.automata.alisp.Environment env, String name, String lvalue, String rvalue)
	{
		Object output = env.evaluateScalarFunction(name, lvalue, rvalue);
		return output!=null;
	}
	*/
	
	/**
	 * This method forces the creation of a structure slice even if a slice consistent with the key-value pairs
	 * already exists.
	 * 
	 * @param evidenceList
	 * @return The new allocation index for the newly created structure
	 */
	public Integer createStructureDirectly(KeyValuePair<String, String>[] evidenceList)
	{
		Integer newWorldIndex=null;
		HashMap<String, List<Integer>> evidenceValueSet=null;
		StructureSlice slice=null;
		List<Integer> consistentWorldIndices = null;
		slice = new StructureSlice(evidenceList);
		int newIndex=i_Allocator.AllocateNewWorldGuarantee();
		i_WorldSlices[newIndex]=slice;
		i_Allocator.UpdatePrior(newIndex);
		
		// Add this evidence to the structure map
		for (KeyValuePair<String, String> evidencePair:evidenceList)
		{
			evidenceValueSet=i_StructureMap.get(evidencePair.GetKey());
			if (evidenceValueSet==null)
			{
				evidenceValueSet = new HashMap<String, List<Integer>>();
				i_StructureMap.put(evidencePair.GetKey(), evidenceValueSet);
			}
			// Create value to world map
			consistentWorldIndices=evidenceValueSet.get(evidencePair.GetValue());
			if (consistentWorldIndices==null)
			{
				consistentWorldIndices = new LinkedList<Integer>();
				evidenceValueSet.put(evidencePair.GetValue(), consistentWorldIndices);
			}
			newWorldIndex=new Integer(newIndex);
			consistentWorldIndices.add(newWorldIndex);
		}
		return newWorldIndex;
	}
	
	
	public Integer AddObservation(KeyValuePair<String, String>[] evidenceList, int preferredNewIndex)
	{
		
		HashMap<String, String> currentEvidenceMap = new HashMap<String, String>();
		HashSet<Integer> testedWorlds = new HashSet<Integer>();
		HashMap<String, List<Integer>> evidenceValueSet;
		List<Integer> consistentWorldIndices;

		String consistentEvidenceValue;
		
		
		Integer newWorldIndex=null;
		StructureSlice slice;
		int valueMatchCount, varMatchCount, hypoLength, eviLength=evidenceList.length;
		
		for (KeyValuePair<String, String> kvPair:evidenceList)
		{
			currentEvidenceMap.put(kvPair.GetKey(), kvPair.GetValue());
		}
		boolean newHypothesis=false;
		if (evidenceList!=null && eviLength>0)
			newHypothesis=true;
		
		if (newHypothesis)
		{
			for (KeyValuePair<String, String> evidencePair:evidenceList)
			{
				evidenceValueSet=i_StructureMap.get(evidencePair.GetKey());
				if (evidenceValueSet!=null)
				{
					consistentWorldIndices=evidenceValueSet.get(evidencePair.GetValue());
					if (consistentWorldIndices!=null)
					{
					
						for (Integer hypoIndex:consistentWorldIndices)
						{
							if (testedWorlds.contains(hypoIndex))
								continue;
							slice=i_WorldSlices[hypoIndex.intValue()];
							valueMatchCount=0;
							varMatchCount=0;
							hypoLength=slice.Size();
							
							for (KeyValuePair<String, String> pair:slice.GetKeyValuePairs())
							{
								if (varMatchCount==eviLength)
									break;
								consistentEvidenceValue=currentEvidenceMap.get(pair.GetKey());
								if (consistentEvidenceValue!=null)
								{
									varMatchCount++;
									if (consistentEvidenceValue.equals(pair.GetValue()))
									{
										valueMatchCount++;
									}
									else
									{
										valueMatchCount=0;
										break;
									}
								}
							}
							
							testedWorlds.add(hypoIndex);
							// Check if this world is the same as the evidence
							if (valueMatchCount>0)
							{
								if ((hypoLength==valueMatchCount)&&(valueMatchCount==eviLength))
								{
									newHypothesis=false;
								}
								if (valueMatchCount==hypoLength) // evidence is a subset of this world
								{
									i_Allocator.UpdatePrior(hypoIndex);
								}
							}
						}
					}
				}
			}
			if (newHypothesis)
			{
				// Create new slice
				slice = new StructureSlice(evidenceList);
				if (i_Allocator.IndexFreeP(preferredNewIndex))
				{
					// TODO: Figure out what to do when max structures allocated!
					int newIndex=i_Allocator.AllocateNewWorldExplicit(preferredNewIndex);
					i_WorldSlices[newIndex]=slice;
					i_Allocator.UpdatePrior(newIndex);
					
					// Add this evidence to the structure map
					for (KeyValuePair<String, String> evidencePair:evidenceList)
					{
						evidenceValueSet=i_StructureMap.get(evidencePair.GetKey());
						if (evidenceValueSet==null)
						{
							evidenceValueSet = new HashMap<String, List<Integer>>();
							i_StructureMap.put(evidencePair.GetKey(), evidenceValueSet);
						}
						// Create value to world map
						consistentWorldIndices=evidenceValueSet.get(evidencePair.GetValue());
						if (consistentWorldIndices==null)
						{
							consistentWorldIndices = new LinkedList<Integer>();
							evidenceValueSet.put(evidencePair.GetValue(), consistentWorldIndices);
						}
						newWorldIndex=new Integer(newIndex);
						consistentWorldIndices.add(newWorldIndex);
					}
				}
			}
		}
		return newWorldIndex;
		
	}
	
	
	public List<Integer> UpdateKeyValuesInAllConsistentWorlds(KeyValuePair<String, String>[] evidenceList, String key, String value)
	{
		List<Integer> worlds = GetConsistentWorlds(evidenceList, 1.0,0.0), updatedWorlds = new LinkedList<Integer>();
		StructureSlice slice;
		String prior=null;
		HashMap<String, List<Integer>> values;
		List<Integer> priorWorlds;
		boolean found=false;
		for (Integer index:worlds)
		{
			slice = i_WorldSlices[index.intValue()];
			prior=slice.UpdateValue(key, value);
			if (prior!=null)
			{
				found=found||true;
				if (!prior.equals(value))
				{
					updatedWorlds.add(index);
					values = i_StructureMap.get(key);
					priorWorlds=values.get(prior);
					priorWorlds.remove(index);
					if (values.containsKey(value))
					{
						priorWorlds = values.get(value);
						priorWorlds.add(index);
					}
					else
					{
						priorWorlds = new LinkedList<Integer>();
						priorWorlds.add(index);
						values.put(value, priorWorlds);
					}
					
				}
			}
			
		}
		return updatedWorlds;
		
	}
	
	public List<Integer> UpdateKeyValuesInAllConsistentWorlds(List<Integer> worlds, String key, String value)
	{
		List<Integer> updatedWorlds = new LinkedList<Integer>();
		StructureSlice slice;
		String prior=null;
		HashMap<String, List<Integer>> values;
		List<Integer> priorWorlds;
		boolean found=false;
		for (Integer index:worlds)
		{
			slice = i_WorldSlices[index.intValue()];
			prior=slice.UpdateValue(key, value);
			if (prior!=null)
			{
				found=found||true;
				if (!prior.equals(value))
				{
					updatedWorlds.add(index);
					values = i_StructureMap.get(key);
					priorWorlds=values.get(prior);
					priorWorlds.remove(index);
					if (values.containsKey(value))
					{
						priorWorlds = values.get(value);
						priorWorlds.add(index);
					}
					else
					{
						priorWorlds = new LinkedList<Integer>();
						priorWorlds.add(index);
						values.put(value, priorWorlds);
					}
					
				}
			}
			
				
			
		}
		return updatedWorlds;
		
	}
	
	
	public List<Integer> AddKeyValuesToAllConsistentWorlds(KeyValuePair<String, String>[] evidenceList, String key, String value)
	{
		List<Integer> worlds = GetConsistentWorlds(evidenceList, 1.0,0.0), updatedWorlds = new LinkedList<Integer>();
		StructureSlice slice;
		HashMap<String, List<Integer>> values;
		List<Integer> priorWorlds;
		for (Integer index:worlds)
		{
			slice = i_WorldSlices[index.intValue()];
			if (slice.AddKeyValuePair(key, value))
			{
				updatedWorlds.add(index);
				if (i_StructureMap.containsKey(key))
				{
					values = i_StructureMap.get(key);
				}
				else
				{
					values = new HashMap<String, List<Integer>>();
					i_StructureMap.put(key, values);
				}
				
				if (values.containsKey(value))
				{
					priorWorlds = values.get(value);
					priorWorlds.add(index);
				}
				else
				{
					priorWorlds = new LinkedList<Integer>();
					priorWorlds.add(index);
					values.put(value, priorWorlds);
				}
			}
			
		}
		return updatedWorlds;
	}
	
	
	public List<Integer> AddKeyValuesToAllConsistentWorlds(List<Integer> worlds, String key, String value)
	{
		List<Integer> updatedWorlds = new LinkedList<Integer>();
		StructureSlice slice;
		HashMap<String, List<Integer>> values;
		List<Integer> priorWorlds;
		for (Integer index:worlds)
		{
			slice = i_WorldSlices[index.intValue()];
			if (slice.AddKeyValuePair(key, value))
			{
				updatedWorlds.add(index);
				if (i_StructureMap.containsKey(key))
				{
					values = i_StructureMap.get(key);
				}
				else
				{
					values = new HashMap<String, List<Integer>>();
					i_StructureMap.put(key, values);
				}
				
				if (values.containsKey(value))
				{
					priorWorlds = values.get(value);
					priorWorlds.add(index);
				}
				else
				{
					priorWorlds = new LinkedList<Integer>();
					priorWorlds.add(index);
					values.put(value, priorWorlds);
				}
			}
			
		}
		return updatedWorlds;
	}
	
	
	public List<Integer> RemoveKeyFromConsistentWorlds(KeyValuePair<String, String>[] evidenceList, String key)
	{
		List<Integer> worlds = GetConsistentWorlds(evidenceList, 1.0,0.0), updatedWorlds = new LinkedList<Integer>();
		StructureSlice slice;
		HashMap<String, List<Integer>> values;
		List<Integer> priorWorlds;
		String deletedValue=null;
		for (Integer index:worlds)
		{
			slice = i_WorldSlices[index.intValue()];
			if ((deletedValue=slice.DeleteKey(key))!=null)
			{
				values = i_StructureMap.get(key);
				priorWorlds = values.get(deletedValue);
				priorWorlds.remove(index);
			}
			
		}
		return updatedWorlds;
	}
	
	public List<Integer> RemoveKeyFromConsistentWorlds(List<Integer> worlds, String key)
	{
		List<Integer> updatedWorlds = new LinkedList<Integer>();
		StructureSlice slice;
		HashMap<String, List<Integer>> values;
		List<Integer> priorWorlds;
		String deletedValue=null;
		for (Integer index:worlds)
		{
			slice = i_WorldSlices[index.intValue()];
			if ((deletedValue=slice.DeleteKey(key))!=null)
			{
				values = i_StructureMap.get(key);
				priorWorlds = values.get(deletedValue);
				priorWorlds.remove(index);
			}
			
		}
		return updatedWorlds;
	}
	
	
	public boolean DeleteWorldsExplicit(Integer[] worldIndices)
	{
		boolean deleteOccurred=false;
		StructureSlice slice;
		List<Integer> priorMappedWorlds;
		String priorMapName;
		HashMap<String, List<Integer>> mappedValues;
		List<Integer> mappedWorlds;
		for (Integer worldIndex:worldIndices)
		{
			slice=i_WorldSlices[worldIndex.intValue()];
			for (KeyValuePair<String, String> kvPair:slice.GetKeyValuePairs())
			{
				mappedValues = i_StructureMap.get(kvPair.GetKey());
				if (mappedValues!=null)
				{
					mappedWorlds = mappedValues.get(kvPair.GetValue());
					if (mappedWorlds!=null)
					{
						deleteOccurred=true;
						mappedWorlds.remove(worldIndex);
						if (mappedWorlds.size()==0)
							mappedValues.remove(mappedWorlds);
					}
					
					if (mappedValues.size()==0)
						i_StructureMap.remove(kvPair.GetKey());
				}
			}
			
			i_Allocator.ForceFreeIndex(worldIndex);
			i_WorldSlices[worldIndex.intValue()]=null;
		}
		
		return deleteOccurred;
	}
	
	public boolean DeleteWorldsExplicit(List<Integer> worldIndices)
	{
		boolean deleteOccurred=false;
		StructureSlice slice;
		List<Integer> priorMappedWorlds;
		String priorMapName;
		HashMap<String, List<Integer>> mappedValues;
		List<Integer> mappedWorlds;
		for (Integer worldIndex:worldIndices)
		{
			slice=i_WorldSlices[worldIndex.intValue()];
			for (KeyValuePair<String, String> kvPair:slice.GetKeyValuePairs())
			{
				mappedValues = i_StructureMap.get(kvPair.GetKey());
				if (mappedValues!=null)
				{
					mappedWorlds = mappedValues.get(kvPair.GetValue());
					if (mappedWorlds!=null)
					{
						deleteOccurred=true;
						mappedWorlds.remove(worldIndex);
						if (mappedWorlds.size()==0)
							mappedValues.remove(mappedWorlds);
					}
					
					if (mappedValues.size()==0)
						i_StructureMap.remove(mappedValues);
				}
			}
			
			i_Allocator.ForceFreeIndex(worldIndex);
			i_WorldSlices[worldIndex.intValue()]=null;
		}
		
		return deleteOccurred;
	}
	
	public Integer[] DeleteWorlds(KeyValuePair<String, String>[] evidenceList)
	{
		List<Integer> worldsToDelete = GetConsistentWorlds(evidenceList,1.0,0.0);
		DeleteWorldsExplicit(worldsToDelete);
		
		return worldsToDelete.toArray(new Integer[0]);
			
	}

	private void readObject(ObjectInputStream s) throws IOException, ClassNotFoundException
	{
		s.defaultReadObject();
		Integer sCount = (Integer)s.readObject();
		i_StructureMap = (HashMap<String, HashMap<String, List<Integer>>>)readStructureMap(s);
		if (i_StructureMap==null)
			i_StructureMap = new HashMap<String, HashMap<String,List<Integer>>>();
		i_Allocator = (WorldAllocator)s.readObject();
		i_Allocator.setStructureData(this, i_WorldSlices);
	}
	
	// Output format is:
	// 	slice-count
	//  structure-map
	//  allocator
	private void writeObject(ObjectOutputStream s) throws IOException
	{
		s.defaultWriteObject();
		Integer sCount;
		if (i_WorldSlices==null)
			sCount = new Integer(0);
		else
			sCount = new Integer(i_WorldSlices.length);
		s.writeObject(sCount);
		writeStructureMap(i_StructureMap, s);
		s.writeObject(i_Allocator);
	}
	
	
	// Output format is:
	// 	slice-count, key-count, key-linkmap*
	//  key-linkmap = key-name, child-key-count, child-key-alloc*
	//  child-key-alloc = child-key-name, link-count, alloc-index*
	private void writeStructureMap(HashMap<String, HashMap<String, List<Integer>>> structureMap, ObjectOutputStream s) throws IOException
	{
		Integer count;
		if (structureMap == null)
		{
			count = 0;
			s.writeObject(count);
			return;
		}
		else
		{
			count = structureMap.size();
			s.writeObject(count);
		}
		
		HashMap<String, List<Integer>> substructure;
		
		for (String topKey:structureMap.keySet())
		{
			substructure = structureMap.get(topKey);
			writeKeyLinkMap(topKey, substructure, s);
		}
	}
	
	private HashMap<String, HashMap<String, List<Integer>>> readStructureMap(ObjectInputStream s) throws IOException, ClassNotFoundException
	{
		Integer count = (Integer)s.readObject();
		if (count.intValue() == 0)
			return null;
		HashMap<String, HashMap<String, List<Integer>>> structure = new HashMap<String, HashMap<String,List<Integer>>>();
		Object[] tMap;
		String topKey;
		HashMap<String, List<Integer>> linkMap;
		for (int i=0;i<count.intValue();i++)
		{
			tMap = readKeyLinkMap(s);
			topKey = (String)tMap[0];
			linkMap = (HashMap<String, List<Integer>>)tMap[1];
			structure.put(topKey, linkMap);
		}
		return structure;
	}
	
//  key-linkmap = key-name, child-key-count, child-key-alloc*
	private void writeKeyLinkMap(String topKey, HashMap<String, List<Integer>> linkMap, ObjectOutputStream s) throws IOException
	{
		s.writeObject(topKey);
		Integer childCount = linkMap.size();
		s.writeObject(childCount);
		List<Integer> links;
		for (String childKey:linkMap.keySet())
		{
			links = linkMap.get(childKey);
			writeChildKeyAlloc(childKey, links, s);
		}
	}
	
	//  key-linkmap = key-name, child-key-count, child-key-alloc*
	private Object[] readKeyLinkMap(ObjectInputStream s) throws IOException, ClassNotFoundException
	{
		Object[] oMap = new Object[2];
		HashMap<String, List<Integer>> allocationTable = new HashMap<String, List<Integer>>();
		String topKey = (String)s.readObject();
		Integer keyCount = (Integer)s.readObject();
		
		oMap[0] = topKey;
		oMap[1] = allocationTable;
		
		Object[] linkMap;
		String childKey;
		Integer linkCount;
		LinkedList<Integer> allocList;
		
		for (int i=0;i<keyCount.intValue();i++)
		{
			allocList = new LinkedList<Integer>();
			linkMap = readChildKeyAlloc(s);
			childKey = (String)linkMap[0];
			linkCount = (Integer)linkMap[1];
			for (int j=0;j<linkCount.intValue();j++)
				allocList.add((Integer)linkMap[j+2]);
			allocationTable.put(childKey, allocList);
		}
		return oMap;
	}
	
	
	//  child-key-alloc = child-key-name, link-count, alloc-index*
	private void writeChildKeyAlloc(String childKey, List<Integer> links, ObjectOutputStream s) throws IOException
	{
		s.writeObject(childKey);
		int len;
		if (links==null||links.size()==0)
			len = 0;
		else
			len = links.size();
		s.writeObject(new Integer(len));
		for (Integer i:links)
			s.writeObject(i);
	}
	
	//  child-key-alloc = child-key-name, link-count, alloc-index*
	private Object[] readChildKeyAlloc(ObjectInputStream s) throws IOException, ClassNotFoundException
	{
		String child_key_name = (String)s.readObject();
		Integer link_count = (Integer)s.readObject();
		Object[] output =  new Object[2 + link_count];
		output[0] = child_key_name;
		output[1] = link_count;
		for (int i=0;i<link_count.intValue();i++)
		{
			output[i+2] = s.readObject();
		}
		return output;
	}
	
}
