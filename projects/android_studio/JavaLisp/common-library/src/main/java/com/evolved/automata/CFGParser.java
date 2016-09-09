package com.evolved.automata;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class CFGParser 
{
	public final String wildcard = "~";
	public final String whitespace = "$";
	public final String numeric = "#";
	public final String letter = "@";
	public final String commentChar=";";
	
	
	public final String matchOne="?";
	public final String kleene="*";
	public final String onePlus="+";
	public final String negation = "`";
	
	private HashSet<String> captureGroupNames = null;
	private Hashtable<String, LinkedList<String>> captureMap;
	
	public final String groupName = "group";
	private Hashtable<String,String> namedComponents;
	Hashtable<String, LinkedList<Integer>> matchStartingPoints;
	
	public CFGParser(String[] definitionComponent)
	{
		String[] defParts;
		namedComponents = new Hashtable<String, String>();
		
		for (String definition:definitionComponent)
		{
			if (definition.length()>0)
			{
				if (!definition.substring(0, 1).equals(commentChar))
				{
					defParts = splitDelimitedStringWithEscapes(definition, '=', true);
					namedComponents.put(defParts[0], stringToTerminalSequence(defParts[1]));
				}
			}
		}
	}
	
	public CFGParser(String definition)
	{
		String[] defParts;
		namedComponents = new Hashtable<String, String>();
		
		if (definition.length()>0)
		{
			if (!definition.substring(0, 1).equals(commentChar))
			{
				defParts = splitDelimitedStringWithEscapes(definition, '=', true);
				namedComponents.put(defParts[0], stringToTerminalSequence(defParts[1]));
			}
		}
	}
	
	public CFGParser(Hashtable<String, String> definitionMap)
	{
		
		namedComponents = definitionMap;
		
	}
	
	
	
	
	
	public static String[] getCFGLines(String inputFileFulName) throws IOException
	{
		LinkedList<String> outLine = new LinkedList<String>();
		String lineInput="";
		File f = new File(inputFileFulName);
		BufferedReader reader=null;
		try
		{
			if (f.exists())
			{
				reader = new BufferedReader(new FileReader(f));
				while ((lineInput=reader.readLine())!=null)
				{
					if (lineInput.trim().length()>0)
						outLine.add(lineInput.trim());
				}
				return outLine.toArray(new String[0]);
			}
			else
				return null;
		}
		finally
		{
			if (reader!=null)
			{
				
				try {
					reader.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
	}
	
	public static LinkedList<String> getCFGListLines(String inputFileFulName) throws IOException
	{
		LinkedList<String> outLine = new LinkedList<String>();
		String lineInput="";
		File f = new File(inputFileFulName);
		BufferedReader reader=null;
		try
		{
			if (f.exists())
			{
				reader = new BufferedReader(new FileReader(f));
				while ((lineInput=reader.readLine())!=null)
				{
					if (lineInput.trim().length()>0)
						outLine.add(lineInput.trim());
				}
				return outLine;
			}
			else
				return null;
		}
		finally
		{
			if (reader!=null)
			{
				
				try {
					reader.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
	}
	
	
	public static String[] getCFGLines(java.io.InputStream fistream) throws IOException
	{
		LinkedList<String> outLine = new LinkedList<String>();
		String lineInput="";
		
		BufferedReader reader=null;
		try
		{
			
			reader = new BufferedReader(new java.io.InputStreamReader(fistream));
			while ((lineInput=reader.readLine())!=null)
			{
				if (lineInput.trim().length()>0)
					outLine.add(lineInput.trim());
			}
			return outLine.toArray(new String[0]);
			
		}
		finally
		{
			if (reader!=null)
			{
				
				try {
					reader.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
	}
	
	
	public void addCaptureName(String name)
	{
		if (captureGroupNames == null)
		{
			captureGroupNames = new HashSet<String>();
			matchStartingPoints=null;
		}
		captureGroupNames.add(name);
		captureMap = new Hashtable<String, LinkedList<String>>();
	}
	
	public LinkedList<String> getCapturedList(String name)
	{
		if ((captureMap!=null)&&(captureMap.containsKey(name)))
		{
			return captureMap.get(name);
		}
		else
			return null;
	}
	
	
	public String getFirstCapturedValue(String name)
	{
		LinkedList<String> v = getCapturedList(name);
		if ((v!=null)&&(v.size()>0))
			return v.getFirst();
		else
			return null;
			
	}
	
	
	public Hashtable<String, LinkedList<String>> getCaptureSet()
	{
		return captureMap;
		
	}
	
	private int[] getEndPoints(HashSet<Integer> set)
	{
		int size=0,counter=0;
		if ((set!=null)&&((size=set.size())>0))
		{
			int[] out = new int[size];
			for (Integer i:set)
			{
				out[counter++]=i;
			}
			return out;
		}
		return null;
	}
	
	private void addMatchStartingPoint(String capturename, Integer startingPoint)
	{
		if (matchStartingPoints==null)
			matchStartingPoints = new Hashtable<String, LinkedList<Integer>>();
		LinkedList<Integer> points;
		if (!matchStartingPoints.containsKey(capturename))
		{
			points = new LinkedList<Integer>();
			matchStartingPoints.put(capturename, points);
		}
		else
			points = matchStartingPoints.get(capturename);
		
		if (!points.contains(startingPoint))
			points.add(startingPoint);
	}
	
	private boolean checkContainsStartingPoint(String capturename, Integer startingPoint)
	{
		if (matchStartingPoints==null)
			return false;
		if (!matchStartingPoints.containsKey(capturename))
			return false;
		LinkedList<Integer> points;
		points = matchStartingPoints.get(capturename);
		return points.contains(startingPoint);
		
	}
	
	public String findPattern(String input, String pattern)
	{
		return findPattern(input, pattern, false);
	}
	
	
	
	public String findPattern(String input, String pattern,boolean firstOnly)
	{
		StringBuffer s = new StringBuffer();
		String subString=input;
		boolean first=true;
		int max=0;
		int[] result;
		LinkedList<String> matches = new LinkedList<String>();
		
		while (subString.length()>0)
		{
			
			result = match(subString,pattern, new String[]{pattern});
			if (result==null)
			{
				s.append(subString.charAt(0) );
				subString=subString.substring(1);
			}
			else
			{
				first=true;
				for (int i=0;i<result.length;i++)
				{
					if (first||result[i]>max)
					{
						first=false;
						max=result[i];
					}
				}
				matches.add(subString.substring(0, max));
				subString=subString.substring(max);
				if (firstOnly)
				{
					captureMap = new Hashtable<String, LinkedList<String>>();
					captureMap.put(pattern, matches);
					return s.append(subString).toString();
				}
				
			}
				
		}
		if (matches.size()>0)
		{
			captureMap = new Hashtable<String, LinkedList<String>>();
			captureMap.put(pattern, matches);
			
		}
		else
			captureMap=null;
		return s.toString();
		
	}
	
	
	
	public String matchCapture(String input, String parentPattern, String targetPattern)
	{
		
		int[] m = match(input, parentPattern, new String[]{targetPattern});
		if (m!=null)
			return getFirstCapturedValue(targetPattern);
		else
			 return null;
	}
	
	
	public int[] match(String inputString, String grammarComponent)
	{
		return match(null, inputString, grammarComponent);
	}
	
	public int[] match(String inputString, String grammarComponent, String[] captureNames)
	{
		if (captureNames!=null)
		{
			for (String name:captureNames)
			{
				addCaptureName(name);
			}
		}
		return match(null, inputString, grammarComponent);
	}
	
	
	public int[] match(int[] initialIndices, String inputString, String grammarComponent)
	{
		LinkedList<String> captureList=null;
		HashSet<Integer> indexMap= new HashSet<Integer>();
		boolean top=false;
		if (initialIndices == null)
		{
			top=true;
			initialIndices = new int[]{0};
		}
		
		int[] startIndices=null;
		int[] endingIndices=null;
		
		boolean cont;
		int index;
		int maxIndex=0;
		boolean first=true;
		
		
		/**************************************************************
		 *  Check if current grammarSegment is a terminal symbol
		 *  
		 *  state variables: 
		 *  grammarComponent (global)
		 *  indexMap (global)
		 *  index (global)
		 *  terminalSymbol (local)
		 *  
		 *  
		 **************************************************************/
		
	    String terminalSymbol = isTerminal(grammarComponent);
	    if (terminalSymbol!=null)
	    {
	    	for (int i=0;i<initialIndices.length;i++)
	    	{
	    		index = initialIndices[i];
	    		//TODO: refactor to use inputString.charAt
	    		if (index!=inputString.length()&&matchTerminal(terminalSymbol, inputString.substring(index, index+1)))
	    		{
	    			indexMap.add(new Integer(index+1));
	    		}
	    	}
	    	
	    	return getEndPoints(indexMap);
	    		
	    }
		
		
		
		/**************************************************************
		 *  Check if current grammarSegment is a nonterminal symbol,
		 *  parenthetical group, or conjunction of grammar elements
		 *  state variables:
		 *  grammarComponent (global)
		 *  indexMap (global)
		 *  index (global)
		 *  first (global)
		 *  maxIndex (global)
		 *  captureList (global)
		 *  cont (global)
		 *  groupGrammar (local)
		 *  labelGrammar (local)
		 *  grammarSegments (local)
		 *  
		 **************************************************************/

	    String groupGrammar=null, labelGrammar=null;
	    String[] grammarSegments=null;
	    String capturedName;
	    groupGrammar=isGroup(grammarComponent);
	    labelGrammar=isLabel(grammarComponent);
	    grammarSegments = isConjunction(grammarComponent);
	    
	    if ((groupGrammar!=null)||(labelGrammar!=null)||(grammarSegments!=null))
	    {
	    	if (grammarSegments==null)
	    	{
	    		if (groupGrammar!=null)
	    			grammarSegments=segmentGroup(groupGrammar);
	    		else
	    			grammarSegments=segmentGroup(labelGrammar);
	    	}
	    	
	    	if (grammarSegments.length==1)
	    	{
	    		for (int i=0;i<initialIndices.length;i++)
	    		{
	    			index = initialIndices[i];
	    			if (index<inputString.length())
	    			{
	    				endingIndices = match(new int[]{index}, inputString, grammarSegments[0]);
	    				if (endingIndices!=null)
	    				{
	    					/** Putting end indices into indexMap and capturing if necessary
		    				*/
	    					
	    					first=true;
	    					for (int j=0;j<endingIndices.length;j++)
	    					{
	    						indexMap.add(new Integer(endingIndices[j]));
	    						if ((captureGroupNames!=null)&&captureGroupNames.contains(grammarComponent))
	    						{
	    							if (first || endingIndices[j]>maxIndex)
	    							{
	    								maxIndex=endingIndices[j];
	    								first=false;
	    							}
	    						}
	    					}
	    					
	    					if ((captureGroupNames!=null)&&captureGroupNames.contains(grammarComponent)&&(maxIndex-index)>0)
	    					{
	    						capturedName = inputString.substring(index, maxIndex);
	    						if (!checkContainsStartingPoint(grammarComponent, index))
	    						{
	    							addMatchStartingPoint(grammarComponent, index);
	    							if (!captureMap.containsKey(grammarComponent))
		    						{
		    							captureList = new LinkedList<String>();
		    							captureMap.put(grammarComponent, captureList);
		    						}
		    						else
		    							captureList=captureMap.get(grammarComponent);
	    							captureList.add(capturedName);
	    						}
	    						
	    						
	    					}
	    				}
	    			}
	    		}
	    	}
	    	else
	    	{
	    		//startIndices = initialIndices;
	    		for (Integer sindex:initialIndices)
	    		{
	    			startIndices = new int[]{sindex.intValue()};
	    			cont=true;
	    			for (int gIndex = 0;gIndex<grammarSegments.length;gIndex++)
	    			{
	    				endingIndices = match(startIndices, inputString, grammarSegments[gIndex]);
	    				if (endingIndices == null)
	    				{
	    					cont=false;
	    					break;
	    				}
	    				startIndices=endingIndices;
	    			}
	    			
	    			if (cont)
	    			{
	    				/** Putting end indices into indexMap and capturing if necessary
	    				*/
	    				first=true;
	    				for (int j=0;j<endingIndices.length;j++)
    					{
    						indexMap.add(new Integer(endingIndices[j]));
    						if ((captureGroupNames!=null)&&captureGroupNames.contains(grammarComponent))
    						{
    							if (first || endingIndices[j]>maxIndex)
    							{
    								maxIndex=endingIndices[j];
    								first=false;
    							}
    						}
    					}
	    				
	    				if ((captureGroupNames!=null)&&captureGroupNames.contains(grammarComponent)&&(maxIndex-sindex)>0)
    					{
	    					if (!checkContainsStartingPoint(grammarComponent, sindex))
	    					{
	    						capturedName = inputString.substring(sindex.intValue(), maxIndex);
	    						addMatchStartingPoint(grammarComponent, sindex);
	    						if (!captureMap.containsKey(grammarComponent))
	    						{
	    							captureList = new LinkedList<String>();
	    							captureMap.put(grammarComponent, captureList);
	    						}
	    						else
	    							captureList=captureMap.get(grammarComponent);
	    						captureList.add(capturedName);
	    					}
	    					
    					}
	    				
	    				
	    			}
	    			
	    		}
	    		
	    	}
	    	if (top)
	    	{
	    		if (captureGroupNames==null)
	    		{
	    			captureMap=null;
	    			matchStartingPoints=null;
	    		}
	    		else
	    			captureGroupNames=null;
	    		
	    	}
	    	return getEndPoints(indexMap);
	    	
	    }
	    
		/**************************************************************
		 *  Check if current grammarSegment is an alternation of grammar
		 *  elements
		 **************************************************************/

	    grammarSegments=isAlternation(grammarComponent);
	    if (grammarSegments!=null)
	    {
	    	for (int i=0;i<grammarSegments.length;i++)
	    	{
	    		endingIndices = match(initialIndices, inputString, grammarSegments[i]);
	    		if (endingIndices!=null)
	    		{
	    			for (int j=0;j<endingIndices.length;j++)
	    			{
	    				indexMap.add(new Integer(endingIndices[j]));
	    			}
	    		}
	    	}
	    	
	    	return getEndPoints(indexMap);
	    }
	    
		/**************************************************************
		 *  Check if current grammarSegment is a quantifier over grammar
		 *  elements
		 **************************************************************/
		
	    String[] segments = isQuantifier(grammarComponent);
	    if (segments!=null)
	    {
	    	endingIndices=initialIndices;
	    	while (endingIndices!=null)
	    	{
	    		endingIndices=match(endingIndices, inputString,segments[1]);
	    		
	    		if (endingIndices!=null)
	    		{
	    			if (segments[0].equals(negation))
	    				return null;
	    			else
	    			{
	    				for (int j=0;j<endingIndices.length;j++)
		    			{
		    				indexMap.add(new Integer(endingIndices[j]));
		    			}
	    			}
	    			
	    		}
	    		else
	    		{
	    			if (segments[0].equals(negation))
	    				return initialIndices;
	    		}
	    			
	    		if (segments[0].equals(matchOne))
	    			break;
	    	}
	    	
	    	
	    	if (segments[0].equals(matchOne)||segments[0].equals(kleene))
	    	{
	    		for (int j=0;j<initialIndices.length;j++)
    			{
    				indexMap.add(new Integer(initialIndices[j]));
    			} 
	    	}
	    	
	    	return getEndPoints(indexMap);
	    	
	    }
	    
	    return null;
		
	}
	

	
	
	private boolean matchTerminal(String terminal, String input)
	{
		if (terminal.length()>1)
			return terminal.substring(1, 2).equals(input);
		else
		{
			char raw = input.charAt(0);
			if (terminal.substring(0,1).equals(wildcard))
				return true;
			else if (terminal.substring(0,1).equals(whitespace))
				return Character.isWhitespace(raw);
			else if (terminal.substring(0,1).equals(numeric))
				return Character.isDigit(raw);
			else if (terminal.substring(0,1).equals(letter))
				return Character.isLetter(raw);
			else
				return terminal.substring(0, 1).equals(input);
		}
	}
	
	private String[] segmentGroup(String groupString)
	{
		return splitCommaDelimitedString(groupString);
	}
	
	private String isTerminal(String inString)
	{
		if (inString==null)
			return null;
		
		boolean len3 = inString.length() == 3;
		boolean len4 = inString.length() == 4;
		boolean firstCharDelimiter = inString.length()>0 && "'".equals(inString.substring(0, 1));
		boolean thirdCharDelimiter = inString.length()>2 && "'".equals(inString.substring(2, 3));
		boolean fourthCharDelimiter = inString.length()>3 && "'".equals(inString.substring(3, 4));
		boolean secondCharEscape = inString.length()>1 && "\\".equals(inString.substring(1, 2));
		
		if (len3 && firstCharDelimiter && thirdCharDelimiter)
			return inString.substring(1,2);
		if (len4 && firstCharDelimiter && secondCharEscape && fourthCharDelimiter)
			return inString.substring(1,3);
		return null;
		
	}
	
	private String isLabel(String inString)
	{
		if (namedComponents.containsKey(inString.trim()))
			return namedComponents.get(inString.trim());
		return null;
	}
	
	private String[] isAlternation(String inString)
	{
		String[] parts = splitDelimitedStringWithEscapes(inString, '|', true);
		if ((parts!=null)&&(parts.length>1))
			return parts;
		else
			return null;
	}
	
	private String isGroup(String inString)
	{
		char[] values = inString.toCharArray();
		StringBuffer sBuffer = null;
		
		int pcounter = 0;
		for (int i=0;i<values.length;i++)
		{
			if ((i==0)&&(values[i]=='('))
			{
				pcounter++;
				sBuffer = new StringBuffer();
			}
			else
			{
				if ((values[i]=='(')&&!isEscaped(values, i))
					pcounter++;
				if ((values[i]==')')&&!isEscaped(values, i))
					pcounter--;
				if (pcounter==0)
				{
					if ((i!=values.length-1)||(i==0))
						return null;
					else
						return sBuffer.toString();
				}
				sBuffer.append(values[i]);
			}
		}
		return null;
	}
	
	private String[] isQuantifier(String inString)
	{
		int len = inString.length();
		String lastChar;
		if (len>1)
		{
			lastChar = inString.substring(len - 1, len);
			if (lastChar.equals("?") || lastChar.equals("+") || lastChar.equals("*") || lastChar.equals(negation))
				return new String[]{lastChar, inString.substring(0, len - 1)};
		}
		return null;
	}
	
	private String[] isConjunction(String inString)
	{
		String[] parts = splitCommaDelimitedString(inString);
		if (parts.length>1)
			return parts;
		else
			return null;
	}
	
	public static boolean isEscaped(char[] array, int pos)
	{
		return (pos >0)&&(pos < array.length-1)&&(array[pos+1]=='\'') && ((array[pos - 1]=='\'')||((pos - 2 >=0)&&(array[pos-2]=='\'')&&(array[pos - 1]=='\\')));
	}
	
	
	public static String[] splitCommaDelimitedString(String tokenizedString)
	{
		return splitDelimitedStringWithEscapes(tokenizedString,',',true);
	}
	
	
	
	public static String[] splitDelimitedStringWithEscapes(String tokenizedString, char slitChar, boolean trim)
	{
		if (tokenizedString==null)
			return null;
		
		LinkedList<String> splitString = new LinkedList<String>();
		final int IN_PARENTHESIS=0;
		final int OUT_PARENTHESIS=1;
		int level=0;
		int state = OUT_PARENTHESIS;
		char[] chars = tokenizedString.toCharArray();
		StringBuilder segment= new StringBuilder();
		
		int totalChars=chars.length;
		for (int i=0;i<totalChars;i++)
		{
			
			switch (state)
			{
				case IN_PARENTHESIS:
					if ((chars[i] == ')')&&!isEscaped(chars, i))
					{
						if (level == 1)
							state = OUT_PARENTHESIS;
						else
							level--;
					}
					else
					{
						if ((chars[i]=='(') && !isEscaped(chars, i))
							level++;
					}
					segment.append(chars[i]);
					break;
				case OUT_PARENTHESIS:
					if ((chars[i] == slitChar) && !isEscaped(chars, i))
					{
						if (trim)
							splitString.add(segment.toString().trim());
						else
							splitString.add(segment.toString());
						segment = new StringBuilder();
					}
					else
					{
						if ((chars[i] == '(') && !isEscaped(chars, i))
						{
							state = IN_PARENTHESIS;
							level = 1;
						}
						segment.append(chars[i]);
					}
					break;
			}
			
		}
		
		if (segment.length()>0)
		{
			if (trim)
				splitString.add(segment.toString().trim());
			else
				splitString.add(segment.toString());
		}
		return splitString.toArray(new String[0]);
		
	}
	

	
	public static String stringToTerminalSequence(String input) {
		final int INITIAL = 0;
		final int IN_QUOTE = 1;
		final int OUT_QUOTE = 2;
		final int ESCAPE_SEEN_OUT_QUOTE = 3;
		final int ESCAPE_SEEN_IN_QUOTE = 4;
		final int ESCAPE_CHAR = 0;
		final int QUOTE = 1;
		final int NEITHER = 2;
		int state = INITIAL;
		int event;
		char[] chars = input.toCharArray();
		StringBuffer currentQuoted = null;
		StringBuffer overallOutput = new StringBuffer();
		for (int i = 0; i < chars.length; i++) {
			switch (chars[i]) {
			case '\"':
				event = QUOTE;
				break;
			case '\\':
				event = ESCAPE_CHAR;
				break;
			default:
				event = NEITHER;
			}
			switch (state) {
			case INITIAL:
				switch (event) {
				case NEITHER:
					overallOutput.append(chars[i]);
					state = OUT_QUOTE;
					break;
				case ESCAPE_CHAR:
					overallOutput.append(chars[i]);
					state = ESCAPE_SEEN_OUT_QUOTE;
					break;
				case QUOTE:
					currentQuoted = new StringBuffer();
					state = IN_QUOTE;
					break;
				}
				break;
			case OUT_QUOTE:
				switch (event) {
				case NEITHER:
					overallOutput.append(chars[i]);
					state = OUT_QUOTE;
					break;
				case ESCAPE_CHAR:
					overallOutput.append(chars[i]);
					state = ESCAPE_SEEN_OUT_QUOTE;
					break;
				case QUOTE:
					currentQuoted = new StringBuffer();
					state = IN_QUOTE;
					break;
				}
				break;
			case IN_QUOTE:
				switch (event) {
				case NEITHER:
					currentQuoted.append(chars[i]);
					state = IN_QUOTE;
					break;
				case ESCAPE_CHAR:
					currentQuoted.append(chars[i]);
					state = ESCAPE_SEEN_IN_QUOTE;
					break;
				case QUOTE:
					overallOutput.append(convertToTerminal(currentQuoted
							.toString()));
					state = OUT_QUOTE;
					break;
				}
				break;
			case ESCAPE_SEEN_OUT_QUOTE:
				switch (event) {
				case NEITHER:
					overallOutput.append(chars[i]);
					state = OUT_QUOTE;
					break;
				case ESCAPE_CHAR:
					overallOutput.append(chars[i]);
					state = ESCAPE_SEEN_OUT_QUOTE;
					break;
				case QUOTE:
					overallOutput.append(chars[i]);
					state = OUT_QUOTE;
					break;
				}
				break;
			case ESCAPE_SEEN_IN_QUOTE:
				switch (event) {
				case NEITHER:
					currentQuoted.append(chars[i]);
					state = IN_QUOTE;
					break;
				case ESCAPE_CHAR:
					currentQuoted.append(chars[i]);
					state = ESCAPE_SEEN_IN_QUOTE;
					break;
				case QUOTE:
					currentQuoted.append(chars[i]);
					state = IN_QUOTE;
					break;
				}
				break;
			}
		}
		// finishing
		switch (state) {
		case INITIAL:
			return "";
		case OUT_QUOTE:
		case ESCAPE_SEEN_OUT_QUOTE:
			return overallOutput.toString();
		case IN_QUOTE: // this is an invalid state
		case ESCAPE_SEEN_IN_QUOTE:
		default:
			return null;
		}
	}

	
	
	private static String convertToTerminal(String input)
	{
		if ((input==null)||(input.length()==0))
			return "";
		char[] raw = input.toCharArray();
		StringBuilder sBuilder = new StringBuilder();
		sBuilder.append("'");
		sBuilder.append("\\");
		sBuilder.append(raw[0]);
		sBuilder.append("'");
		for (int i=1;i<raw.length;i++)
		{
			sBuilder.append(",");
			sBuilder.append(" ");
			sBuilder.append("'");
			sBuilder.append("\\");
			sBuilder.append(raw[i]);
			sBuilder.append("'");
		}
		return sBuilder.toString();
	}
}
