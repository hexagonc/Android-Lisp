package com.evolved.automata;

import org.junit.Test;

import com.evolved.automata.lisp.TestHarnessBase.AssertEvaluate;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertEquals;
public class AIToolsTest {
	
	@Test
	public void testOneCharCommentStripping()
	{
		String commentPattern = ";";
		String newLine = System.getProperty("line.separator");
		String[][] inputResponsePairs = new String[][]{
				new String[]{"(+ x 1)", "(+ x 1)"},
				new String[]{ String.format("%1$sThis should be commented out", commentPattern), ""},
				new String[]{String.format("%1$sThis should be commented out%2$s this should remain", commentPattern, newLine), " this should remain"},
				new String[]{String.format("%1$sFirst comment%2$s%1$sSecond comment%2$sRemaining", commentPattern, newLine), "Remaining"},
				new String[]{String.format("First non-commented stuff%2$s%1$sFirst comment%2$s%1$sSecond comment%2$sRemaining", commentPattern, newLine), String.format("First non-commented stuff%1$sRemaining", newLine)},
				new String[]{String.format("Multi-line%1$sCompletely uncommented stuff%1$sDone", newLine), String.format("Multi-line%1$sCompletely uncommented stuff%1$sDone", newLine)},
				
		};
		
		for (int i =0;i<inputResponsePairs.length;i++)
		{
			assertEquals("Comparing: " + inputResponsePairs[i][0] + " to " + inputResponsePairs[i][1],
					AITools.stripPrefixDelimitedComments(inputResponsePairs[i][0], commentPattern),
					inputResponsePairs[i][1]);
		}
	}
	
	@Test
	public void testEncryption()
	{
		String key = "0b";
		long seed = 0;
		
		String input = "h";
		String dictionary = "abcdefghijklmnopqrstuvwxyz";
		String encypted = AITools.encodeDecode(input, dictionary,  3, key.length()/2, key, seed);
		org.junit.Assert.assertEquals(input, AITools.encodeDecode(encypted, dictionary, 3,key.length()/2, key, seed));

	}
	
	@Test
	public void testEncryptionWithExtraCharacters()
	{
		String key = "0b";
		long seed = 0;
		
		String input = "hello world";
		String dictionary = "abcdefghijklmnopqrstuvwxyz";
		String encypted = AITools.encodeDecode(input, dictionary,  3, key.length()/2, key, seed);
		org.junit.Assert.assertEquals(input, AITools.encodeDecode(encypted, dictionary, 3,key.length()/2, key, seed));

	}
	
	@Test
	public void testEncryptionWithMultipleRotors()
	{
		String key = "012zat";
		long seed = 0;
		
		String input = "hello world";
		String dictionary = "abcdefghijklmnopqrstuvwxyz";
		String encypted = AITools.encodeDecode(input, dictionary,  3, key.length()/2, key, seed);
		org.junit.Assert.assertEquals(input, AITools.encodeDecode(encypted, dictionary, 3,key.length()/2, key, seed));

	}
	
	
	
}
