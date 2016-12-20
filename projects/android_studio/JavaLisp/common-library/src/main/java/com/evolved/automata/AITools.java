package com.evolved.automata;
import java.lang.reflect.Array;
import java.util.regex.*;
import java.util.*;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.math3.random.AbstractRandomGenerator;
import org.apache.commons.math3.random.RandomDataGenerator;
import org.apache.commons.math3.random.RandomGenerator;


public class AITools {



	/**
	 * Maps the values of an array.  Returns a new array.  Mapped values
	 * can be different type of original input.

     *
	 * @param input
	 * @param mapper
	 * @param <I, O> the type of the input array and the output array
	 * @return
	 */
	public static <I, O> O[] mapValues(I[] input, IndexedValueMapper<I, O> mapper)
	{
		O[] buffer = Arrays.copyOf(mapper.getEmptyOutput(), input.length);

		for (int i =0;i<input.length;i++)
		{
			buffer[i] = mapper.map(input[i], i);
		}

		return buffer;
	}



    /**
     * Maps the values of an array.  Returns a new array
     * @param input
     * @param mapper
     * @param <T> the type of the array
     * @return
     */
	public static <T> T[] map(T[] input, ArrayMapper<T> mapper)
	{

		T[] out = Arrays.copyOf(input, input.length);
		for (int i =0;i<input.length;i++)
		{
			out[i] = mapper.map(input[i], i);
		}
		return out;
	}

    /**
     * Destructively maps the values of an array
     * @param input
     * @param mapper
     * @param <T>
     * @return
     */
    public static <T> T[] mapD(T[] input, ArrayMapper<T> mapper)
    {

        for (int i =0;i<input.length;i++)
        {
            input[i] = mapper.map(input[i], i);
        }
        return input;
    }

	/**
	 * Uniformly selects a random element of generic type V from a list of values
	 * 
	 * @param itemList
	 */
	public static <V> V ChooseRandom(List<V> itemList)
	{
		if ((itemList==null)||(itemList.size()<1))
			return null;
		else
		{
			int maxIndex=itemList.size();
			int chosenIndex=(int)Math.min(Math.random()*maxIndex, maxIndex-1);
			return itemList.get(chosenIndex);
		}
	}
	
	/**
	 * Uniformly selects a random element of generic type V from an array of values.
	 * 
	 * @param itemArray
	 */
	public static <V> V ChooseRandom(V[] itemArray)
	{
		if ((itemArray==null)||(itemArray.length<1))
			return null;
		else
		{
			int maxIndex=itemArray.length;
			int chosenIndex=(int)Math.min(Math.random()*maxIndex, maxIndex-1);
			return itemArray[chosenIndex];
		}
	}
	/**
	 * This function implements a slightly altered version of simulated sampling from
	 * a Bernoulli urn filled with colored balls.  With this function, colors with
	 * more balls are weighted slightly more than it would from a completely fair
	 * uniform sampling.  This function only works properly  when the weights are non-
	 * negative. TODO: Fix this so that the maximum and minimum weights are arbitrary
	 * and introduction an error and comparison scale as is done with
	 * ChooseWeightedRandomPartition
	 * 
	 * @param itemList
	 * @param favor_high    favor_high
	 */
	public static <V> WeightedValue<V> ChooseWeightedRandom(List<WeightedValue<V>> itemList, boolean favor_high)
	{
		if ((itemList==null)||(itemList.size()<1))
			return null;
		else
		{
			
			double maxValue=0;
			double weightFactor=(favor_high)?1:-1;
			int randomPrec=10000;
			int cutoff;
			for (WeightedValue<V> wValue: itemList)
			{
				if (wValue.GetWeight()>=maxValue)
					maxValue=wValue.GetWeight();
			}
			cutoff =  (int)(maxValue*Math.random()*randomPrec);
			List<WeightedValue<V>> choiceList = new LinkedList<WeightedValue<V>>();
			for (WeightedValue<V> potential:itemList)
			{
				if (weightFactor*potential.GetWeight()*randomPrec>=weightFactor*cutoff)
					choiceList.add(potential);
			}
			return ChooseRandom(choiceList);
		}
	}
	
	
	/**
	 * This function simulates sampling from a Bernoulli urn filled with colored balls.
	 * Each element of 'itemList' represents a 'color' and the number of balls is
	 * modeled as a double precision float.  The actual thing that is selected is the
	 * generic parameter V.  This only works with non-negative double precision
	 * weights. If sum of all weights are less than minTotal then returns null.
	 * TODO: Introduce an error and comparison scale as is done with
	 * ChooseWeightedRandomPartition
	 * 
	 * @param itemList    itemList
	 */
	public static <V> WeightedValue<V> ChooseWeightedRandomFair(List<WeightedValue<V>> itemList)
	{
		if ((itemList==null)||(itemList.size()<1))
			return null;
		else
		{
			
			double cutoff;
			double totalWeight=0;
			double minTotal = 0.000001D;
			for (WeightedValue<V> wValue: itemList)
			{
				totalWeight+=wValue.GetWeight();
			}
			if (totalWeight<minTotal)
				return null;
			cutoff =  totalWeight*Math.random();
			double pastRange=0,weight;
			
			WeightedValue<V> chosen=null;
			
			for (WeightedValue<V> wValue:itemList)
			{
				
				weight=wValue.GetWeight();
				
				if ((weight+pastRange)>=cutoff)
				{
					chosen= wValue;
					break;
				}
				pastRange+=weight;
			}
			return chosen;
			
		}
	}
	
	/**
	 * This function implements a slightly altered version of simulated sampling from
	 * a Bernoulli urn filled with colored balls.  With this function, colors with
	 * more balls are weighted slightly more than it would from a completely fair
	 * uniform sampling.  This function only works properly  when the weights are non-
	 * negative. TODO: Fix this so that the maximum and minimum weights are arbitrary
	 * and introduction an error and comparison scale as is done with
	 * ChooseWeightedRandomPartition
	 * 
	 * @param stringDistribution
	 * @param favor_high    favor_high
	 */
	public static  String ChooseWeightedRandomString(Hashtable<String, Integer> stringDistribution, boolean favor_high)
	{
		if ((stringDistribution==null)||(stringDistribution.size()<1))
			return null;
		else
		{
			
			double cutoff;
			double totalWeight=0;
			Integer value;
			for (String wValue: stringDistribution.keySet())
			{
				value = stringDistribution.get(wValue);
				totalWeight+=value.intValue();
			}
			cutoff =  totalWeight*Math.random();
			double pastRange=0,weight;
			
			for (String wValue:stringDistribution.keySet())
			{
				
				value = stringDistribution.get(wValue);
				
				if ((value.intValue()+pastRange)>=cutoff)
				{
					return wValue;
				}
				pastRange+=value.intValue();
			}
			return null;
			
		}
	}
	
	public static void shiftBackValues(Map<String,String> map, String[] baseKeyNames, String[] newBaseValues,int shiftDepth)
	{
		if (newBaseValues!=null)
		{
			for (int i=0;i<baseKeyNames.length;i++)
				shiftBackValue(map,baseKeyNames[i],newBaseValues[i],shiftDepth);
		}
		else
		{
			for (int i=0;i<baseKeyNames.length;i++)
				shiftBackValue(map,baseKeyNames[i], null,shiftDepth);
		}
		
		
	}
	
	public static void shiftBackValue(Map<String,String> map, String baseKeyName, String newBaseValue,int shiftDepth)
	{
		String moreRecentValue,moreRecentKey, currentKey;
		String separator=".";
		String keyPattern = "%1$s%2$s%3$s";
		for (int i=shiftDepth;i>=1;i--)
		{
			currentKey=String.format(keyPattern, baseKeyName,separator,i);
			if (i>1)
			{
				moreRecentKey=String.format(keyPattern, baseKeyName,separator,i-1);
			}
			else
				moreRecentKey=baseKeyName;
			if (map.containsKey(moreRecentKey))
				map.put(currentKey, map.get(moreRecentKey));
			else
				map.remove(currentKey);
		}
		if (newBaseValue!=null)
			map.put(baseKeyName, newBaseValue);
		else
			map.remove(baseKeyName);
	}

	public static String stripPrefixDelimitedComments(String input, String commentDelimiter)
	{
		String newLine = System.getProperty("line.separator");
		String[] parts = StringUtils.split(input, newLine);
		StringBuilder sbuilder = new StringBuilder();
		boolean replaceNewLines =false;
		for (int i=0;i<parts.length;i++)
		{
			if (!parts[i].trim().startsWith(commentDelimiter))
			{
				if (replaceNewLines)
					sbuilder.append(newLine);
				sbuilder.append(parts[i]);
				replaceNewLines = true;
			}
		}
		return sbuilder.toString();
	}
	
	
	/**
	 * This function creates a virtual rotor for the Enigma machine.  A rotor must perform both a 
	 * forward as well as a reverse mapping between letters of an alphabet to encrypted or decrypted
	 * letters.  This same function can also be used to create the plugboard

	 * @return
	 */
	static KeyValuePair<ArrayList<String>, ArrayList<String>> createReversibleMapping(RandomDataGenerator rdg, List<String> baseDictionary, HashMap<String, Integer> charIndexMap)
	{
		
		int i = 0, dictionarySize = baseDictionary.size();
		Object[] forwardRotorDef = rdg.nextSample(baseDictionary, dictionarySize);
		
		ArrayList<String> forwardRotorPermutation = new ArrayList<String>(dictionarySize), reverseRotorPermutation = new ArrayList<String>(dictionarySize);
		// Do this to initialize all positions in arraylist up to capacity.
		reverseRotorPermutation.addAll(baseDictionary);
		String linkedChar;
		for (String referenceChar:baseDictionary)
		{
			linkedChar = (String)forwardRotorDef[i];
			forwardRotorPermutation.add(linkedChar);
			reverseRotorPermutation.set(charIndexMap.get(linkedChar), referenceChar);
			i++;
		}
		
		return new KeyValuePair<ArrayList<String>, ArrayList<String>>(forwardRotorPermutation, reverseRotorPermutation);
	}

	static ArrayList<String> createRandomPermutation(RandomDataGenerator rdg, List<String> baseDictionary, HashMap<String, Integer> charIndexMap)
	{
		
		int dictionarySize = baseDictionary.size();
		Object[] forwardRotorDef = rdg.nextSample(baseDictionary, dictionarySize);
		
		ArrayList<String> forwardRotorPermutation = new ArrayList<String>(dictionarySize);

		String linkedChar;
		for (int i=0;i< dictionarySize;i++)
		{
			linkedChar = (String)forwardRotorDef[i];
			forwardRotorPermutation.add(linkedChar);
		}
		
		return forwardRotorPermutation;
	}
	
	
	/**
	 * The first rotor steps by one for each character input in the message,
	 * The remaining rotors steps by one every full revolution of the previous
	 * rotor
	 * @param messageCharIndex
	 * @param numRotorsInUse
	 * @param dictionarySize
	 * @return
	 */
	private static int[] getRotorStepOffset(int messageCharIndex, int numRotorsInUse, int dictionarySize)
	{
		int[] out = new int[numRotorsInUse];
		for (int j=0;j<numRotorsInUse;j++)
		{
			out[j] = ((int)(messageCharIndex/Math.pow(dictionarySize, j))) % ((int)Math.pow(dictionarySize, j + 1)); 
		}
		return out;
	}
	
	
	/**
	 * Simulates an enigma machine
	 * @param message
	 * @param dictionary - string with each of the letters that will be decrypted or encrypted. For example, a dictionary with
	 * 					  just lowercase letters is "abcdefghijklmnopqrstuvwxyz"
	 * @param numRotors - total number of possible rotors
	 * @param rotorsInUse - the number of rotors in use in this particular machine
	 * @param key -	 a string describing the configuration of the machine.  The first [rotorsInUse] characters are the indices
	 * 				 of the actual rotors being used in the machine.  The last [rotorsInUse] characters represent the letters 
	 * 				 the initial offset of the rotors.  Example, 012rab means that rotors 0, 1, and 2 are in used and the initial
	 * 				 letters dialed onto them are 'r', 'a' and 'b' respectively 
	 * @param seed - a random seed used to generate a plugboard and the reflector for the enigma.  Any combination of key and 
	 * 				 seed will produce the same enigma machine
	 * @return
	 */
	public static String encodeDecode(String message, String dictionary, int numRotors, int rotorsInUse, String key, long seed)
	{
		RandomDataGenerator rdg = new RandomDataGenerator();
		rdg.reSeed(seed);
		
		LinkedList<String> alpha = new LinkedList<String>();
		
		HashMap<String, Integer> mainCharIndexMap = new HashMap<String, Integer>();
		
		String[] dictionaryList = new String[dictionary.length()];
		int i=0;
		for (char c:dictionary.toCharArray())
		{
			dictionaryList[i] = "" + c;
			alpha.add(dictionaryList[i]);
			mainCharIndexMap.put(dictionaryList[i], Integer.valueOf(i));
			i++;
		}
		
		// Set the rotor base definitions.  This defines the 
		
		HashMap<String, ArrayList<String>> rotorBaseMap = new HashMap<String, ArrayList<String>>();
		
		// Create a new random set of rotors
		for (i=0;i<numRotors;i++)
		{
			String rotorKey = "" + i;
			rotorBaseMap.put(rotorKey, createRandomPermutation(rdg, alpha, mainCharIndexMap));
		}
		
		// Set the rotor offsets based on the rotor dial settings
		
		int[] rotorDialOffsets = new int[rotorsInUse];
		String[] rotorTypes = new String[rotorsInUse];
		for (i=0;i<rotorsInUse;i++)
		{
			rotorTypes[i]=key.substring(i, i+1);
			rotorDialOffsets[i] = mainCharIndexMap.get(key.substring(i + rotorsInUse, i + rotorsInUse + 1));
		}
		
		KeyValuePair<ArrayList<String>, ArrayList<String>> plugboard = createReversibleMapping(rdg, alpha, mainCharIndexMap);
		
		
		HashMap<String, String> reflector = new HashMap<String, String>();
		Object[] reflectorBase = rdg.nextSample(alpha, alpha.size());
		
		int mid = (int)(dictionaryList.length/2); // mid will be half of the largest even number less than 
		reflector.put((String)reflectorBase[dictionaryList.length-1] , (String)reflectorBase[dictionaryList.length-1]);
		for (i=0;i<mid;i++)
		{
			reflector.put((String)reflectorBase[i] , (String)reflectorBase[i + mid]);
			reflector.put((String)reflectorBase[i + mid] , (String)reflectorBase[i]);
		}
		
		int[] rotorStepOffset = new int[rotorsInUse];
		
		ArrayList< KeyValuePair<HashMap<String, String>, HashMap<String, String>>> actualRotors = new ArrayList< KeyValuePair<HashMap<String, String>, HashMap<String, String>>>();
		
		
		String schar = null;
		String outchar = null;
		
		StringBuilder encrypedOutput = new StringBuilder();
		ArrayList<String>  reflectedPlugboard = plugboard.GetValue();
		
		
		
		for (i=0;i<message.length();i++)
		{
			schar = message.substring(i, i + 1);
			// Check if character is in dictionary, if not, do not encrypt
			
			if (!mainCharIndexMap.containsKey(schar))
			{
				encrypedOutput.append(schar);
				continue;
			}
			
			
			// Get the rotor offsets for this character (this is only a function of the number of remaining 
			// characters in the message
			rotorStepOffset = getRotorStepOffset(i, rotorsInUse, dictionaryList.length);
			
			
			// Get the state of rotors for this 
			actualRotors = new ArrayList< KeyValuePair<HashMap<String, String>, HashMap<String, String>>>();
			HashMap<String, String> forwardMap, reflectedMap ;
			
			for (int j=0;j<rotorsInUse;j++)
			{
				forwardMap = new HashMap<String, String>();
				reflectedMap = new HashMap<String, String>();
				
				ArrayList<String> baseRotorDef = rotorBaseMap.get(rotorTypes[j]);
				for (String inputChar:dictionaryList)
				{
					String mappedChar = baseRotorDef.get((mainCharIndexMap.get(inputChar) + rotorStepOffset[j] + rotorDialOffsets[j]) % dictionaryList.length);
					forwardMap.put(inputChar, mappedChar);
					reflectedMap.put(mappedChar, inputChar);
					
				}
				actualRotors.add(new KeyValuePair<HashMap<String, String>, HashMap<String, String>>(forwardMap,reflectedMap ));
			}
			
			
			// Propagate char through machine
			// Run the character through the plugboard
			outchar = plugboard.GetKey().get(mainCharIndexMap.get(schar));
			
			// run the character forward through the rotors
			for (int j=0;j<rotorsInUse;j++)
			{
				
				forwardMap = actualRotors.get(j).GetKey();
				outchar = forwardMap.get(outchar);
			}
			
			// Reflect the character
			outchar = reflector.get(outchar);
			
			// Run the reflected character back through the rotors in reverse
			for (int j=rotorsInUse-1;j >=0;j--)
			{
				reflectedMap = actualRotors.get(j).GetValue();
				
				outchar = reflectedMap.get(outchar);
			}
			
			// Run the character through the plugboard in reverse
			outchar = reflectedPlugboard.get(mainCharIndexMap.get(outchar));
			encrypedOutput.append(outchar);
		}
		
		return encrypedOutput.toString();
	}
	
	
}
