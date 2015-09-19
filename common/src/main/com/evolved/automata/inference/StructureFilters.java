package com.evolved.automata.inference;
import java.util.*;

public class StructureFilters 
{
	public static List<Integer> sortStructuresNumericDescending(StructureModel model, List<Integer> input, String[] keys)
	{
		Integer[] arrayForm = input.toArray(new Integer[0]);
		Arrays.sort(arrayForm, makeNumericComparator(model, false, keys));
		LinkedList<Integer> out = new LinkedList<Integer>();
		for (int i=0;i<arrayForm.length;i++)
			out.add(arrayForm[i]);
		return out;
	}
	
	public static List<Integer> sortStructuresAlphabeticDescending(StructureModel model, List<Integer> input, String[] keys)
	{
		Integer[] arrayForm = input.toArray(new Integer[0]);
		Arrays.sort(arrayForm, makeStringComparator(model, false, keys));
		LinkedList<Integer> out = new LinkedList<Integer>();
		for (int i=0;i<arrayForm.length;i++)
			out.add(arrayForm[i]);
		return out;
	}
	
	public static List<StructureSlice> sortStructuresNumericDescending(List<StructureSlice> input, String[] keys)
	{
		StructureSlice[] arrayForm = input.toArray(new StructureSlice[0]);
		Arrays.sort(arrayForm, makeNumericComparator(false, keys));
		LinkedList<StructureSlice> out = new LinkedList<StructureSlice>();
		for (int i=0;i<arrayForm.length;i++)
			out.add(arrayForm[i]);
		return out;
	}
	
	public static List<StructureSlice> sortStructuresAlphabeticDescending(List<StructureSlice> input, String[] keys)
	{
		StructureSlice[] arrayForm = input.toArray(new StructureSlice[0]);
		Arrays.sort(arrayForm, makeStringComparator(false, keys));
		LinkedList<StructureSlice> out = new LinkedList<StructureSlice>();
		for (int i=0;i<arrayForm.length;i++)
			out.add(arrayForm[i]);
		return out;
	}
	
	
	
	
	public static List<Integer> sortStructuresNumericAscending(StructureModel model, List<Integer> input, String[] keys)
	{
		Integer[] arrayForm = input.toArray(new Integer[0]);
		Arrays.sort(arrayForm, makeNumericComparator(model, true, keys));
		LinkedList<Integer> out = new LinkedList<Integer>();
		for (int i=0;i<arrayForm.length;i++)
			out.add(arrayForm[i]);
		return out;
	}
	
	public static List<Integer> sortStructuresAlphabeticAscending(StructureModel model, List<Integer> input, String[] keys)
	{
		Integer[] arrayForm = input.toArray(new Integer[0]);
		Arrays.sort(arrayForm, makeStringComparator(model, true, keys));
		LinkedList<Integer> out = new LinkedList<Integer>();
		for (int i=0;i<arrayForm.length;i++)
			out.add(arrayForm[i]);
		return out;
	}
	
	public static List<StructureSlice> sortStructuresNumericAscending(List<StructureSlice> input, String[] keys)
	{
		StructureSlice[] arrayForm = input.toArray(new StructureSlice[0]);
		Arrays.sort(arrayForm, makeNumericComparator(true, keys));
		LinkedList<StructureSlice> out = new LinkedList<StructureSlice>();
		for (int i=0;i<arrayForm.length;i++)
			out.add(arrayForm[i]);
		return out;
	}
	
	public static List<StructureSlice> sortStructuresAlphabeticAscending(List<StructureSlice> input, String[] keys)
	{
		StructureSlice[] arrayForm = input.toArray(new StructureSlice[0]);
		Arrays.sort(arrayForm, makeStringComparator(true, keys));
		LinkedList<StructureSlice> out = new LinkedList<StructureSlice>();
		for (int i=0;i<arrayForm.length;i++)
			out.add(arrayForm[i]);
		return out;
	}
	
	
	public static Comparator<StructureSlice> makeNumericComparator(final boolean ascending, final String[] keys)
	{
		return new Comparator<StructureSlice>()
				{
					public int compare(StructureSlice slice_lower, StructureSlice slice_upper)
					{
						int multipler = (ascending)?1:-1, o;
						double lower, upper;
						String slower, supper;
						for (String key:keys)
						{
							slower = slice_lower.GetValue(key);
							supper = slice_upper.GetValue(key);
							
							if (slower!=null&&supper!=null)
							{
								lower = Double.parseDouble(slower.trim());
								upper = Double.parseDouble(supper.trim());
								if (lower == upper)
									continue;
								if (lower<upper)
									o = -1;
								else
									o = 1;
								return multipler*o;
							}
							else
								break; // leaves them in the order encountered
						}
						
						return 0;
					}
					
					
				};
	}
	
	public static Comparator<StructureSlice> makeStringComparator(final boolean ascending, final String[] keys)
	{
		return new Comparator<StructureSlice>()
				{
					public int compare(StructureSlice slice_lower, StructureSlice slice_upper)
					{
						int multipler = (ascending)?1:-1, o;
						
						String slower, supper;
						for (String key:keys)
						{
							slower = slice_lower.GetValue(key);
							supper = slice_upper.GetValue(key);
							
							if (slower!=null&&supper!=null)
							{
								return multipler*slower.compareTo(supper);
							}
							else
								break; // leaves them in the order encountered
						}
						
						return 0;
					}
					
					
				};
	}
	
	public static Comparator<Integer> makeNumericComparator(final StructureModel model, final boolean ascending, final String[] keys)
	{
		return new Comparator<Integer>()
				{
					public int compare(Integer lower_index, Integer upper_index)
					{
						StructureSlice slice_lower = model.GetSliceExplicit(lower_index), slice_upper = model.GetSliceExplicit(upper_index);
						if (slice_lower == null || slice_upper == null)
							return 0;
						int multipler = (ascending)?1:-1, o;
						double lower, upper;
						String slower, supper;
						for (String key:keys)
						{
							slower = slice_lower.GetValue(key);
							supper = slice_upper.GetValue(key);
							
							if (slower!=null&&supper!=null)
							{
								lower = Double.parseDouble(slower.trim());
								upper = Double.parseDouble(supper.trim());
								if (lower == upper)
									continue;
								if (lower<upper)
									o = -1;
								else
									o = 1;
								return multipler*o;
							}
							else
								break; // leaves them in the order encountered
						}
						
						return 0;
					}
					
					
				};
	}
	
	public static Comparator<Integer> makeStringComparator(final StructureModel model, final boolean ascending, final String[] keys)
	{
		return new Comparator<Integer>()
				{
					public int compare(Integer lower_index, Integer upper_index)
					{
						StructureSlice slice_lower = model.GetSliceExplicit(lower_index), slice_upper = model.GetSliceExplicit(upper_index);
						int multipler = (ascending)?1:-1;
						
						if (slice_lower == null || slice_upper == null)
							return 0;
						
						String slower, supper;
						for (String key:keys)
						{
							slower = slice_lower.GetValue(key);
							supper = slice_upper.GetValue(key);
							
							if (slower!=null&&supper!=null)
							{
								return multipler*slower.compareTo(supper);
							}
							else
								break; // leaves them in the order encountered
						}
						
						return 0;
					}
					
					
				};
	}
}
