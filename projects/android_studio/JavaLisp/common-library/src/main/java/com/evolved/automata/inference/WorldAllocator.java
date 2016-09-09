package com.evolved.automata.inference;
import com.evolved.automata.*;

import java.io.Serializable;
import java.util.*;

public class WorldAllocator implements Serializable
{
	static final long serialVersionUID = 1;
	int[] i_PriorCounts;
	boolean[] i_HypoIndicesUsed;
	int i_TotalAvailable;
	int i_MaxAllocations;
	int i_TotalUpdates;
	
	transient StructureSlice[] i_Structures;
	transient StructureModel i_StructureModel;
	
	WorldAllocator()
	{
		
	}
	
	void setStructureData(StructureModel model, StructureSlice[] slices)
	{
		i_Structures = slices;
		i_StructureModel = model;
		i_StructureModel.SetPriorCounts(i_PriorCounts);
	}
	
	WorldAllocator(int maxAllocations, StructureModel structure)
	{
		i_TotalAvailable=maxAllocations;
		i_MaxAllocations=maxAllocations;
		i_Structures = new StructureSlice[i_MaxAllocations];
		i_PriorCounts = new int[i_MaxAllocations];
		i_HypoIndicesUsed = new boolean[i_MaxAllocations];
		for (int i=0;i<i_MaxAllocations;i++)
		{
			i_PriorCounts[i]=0;
			i_HypoIndicesUsed[i]=false;
		}
		i_TotalUpdates=0;
		i_StructureModel=structure;
		structure.SetPriorCounts(i_PriorCounts);
	}
	
	public Integer FindNewIndex()
	{
		for (int i=0;i<i_MaxAllocations;i++)
		{
			if (!i_HypoIndicesUsed[i])
				return new Integer(i);
		}
		
		return null;
	}
	
	public boolean IndexFreeP(int index)
	{
		if (index<0)
			return false;
		if (index>i_MaxAllocations)
			return false;
		return !i_HypoIndicesUsed[index];
	}
	
	
	public boolean ValidIndexP(int index)
	{
		if (index<0)
			return false;
		if (index>i_MaxAllocations)
			return false;
		return true;
	}
	// Only call this if it is already known
	private void MarkHypoIndexFree(int hypoIndex)
	{
		i_TotalAvailable++;
		i_HypoIndicesUsed[hypoIndex]=false;
	}
	
	private void MarkHypoIndexUsed(int hypoIndex)
	{
		i_TotalAvailable--;
		i_HypoIndicesUsed[hypoIndex]=true;
	}
	
	private int RecycleOldestIndices()
	{
		int highestPriorIndex=0;
		int highestPriorCount=i_PriorCounts[highestPriorIndex];
		int currentPriorCount;
		
		for (int i=0;i<i_MaxAllocations;i++)
		{
			if (i_HypoIndicesUsed[i])
			{
				currentPriorCount=i_PriorCounts[i];
				if (currentPriorCount>highestPriorCount)
				{
					highestPriorCount=currentPriorCount;
					highestPriorIndex=i;
				}
			}
		}
		
		i_TotalUpdates-=highestPriorCount;
		return highestPriorIndex;
	}
	
	public int AllocateNewWorldGuarantee()
	{
		Integer newWorldIndex=FindNewIndex();
		int newIndex;
		if (newWorldIndex==null)
		{
			newIndex = RecycleOldestIndices();
			i_StructureModel.DeleteWorldsExplicit(newIndex);
		}
		else
			newIndex = newWorldIndex.intValue();
		i_PriorCounts[newIndex]=0;
		MarkHypoIndexUsed(newIndex);
		return newIndex;
	}
	
	public int AllocateNewWorldExplicit(int newIndex)
	{
		
		if (!IndexFreeP(newIndex))
		{
			// TODO: Do something better than this
			ForceFreeIndex(newIndex);
			i_StructureModel.DeleteWorldsExplicit(newIndex);
		}
		
		i_PriorCounts[newIndex]=0;
		MarkHypoIndexUsed(newIndex);
		return newIndex;
	}
	
	
	public void ForceFreeIndex(int indexToFree)
	{
		i_TotalUpdates-=i_PriorCounts[indexToFree];
		i_PriorCounts[indexToFree]=0;
		MarkHypoIndexFree(indexToFree);
	}
	
	
	public void UpdatePrior(int worldIndex)
	{
		i_PriorCounts[worldIndex]++;
		i_TotalUpdates++;
	}
	
	
	
}
