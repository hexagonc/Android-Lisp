package com.evolved.automata;

import java.util.ArrayList;

import junit.framework.Assert;

import org.apache.commons.math3.random.RandomDataGenerator;
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
	
	private int randomIndex(int min, int max)
	{
		RandomDataGenerator rdg = new RandomDataGenerator();
		rdg.reSeed(System.currentTimeMillis());
		
		return rdg.nextInt(min, max);
	}
	
	@Test
	public void testKDTree()
	{
		RandomDataGenerator rdg = new RandomDataGenerator();
		long seed = System.currentTimeMillis();
		rdg.reSeed(seed);
		
		
		
		ArrayList<TestPoint> points = new ArrayList<TestPoint>();
		
		KDTree<TestPoint> tree = new KDTree<TestPoint>();
		String timeArrayPattern = "Time to add %1$s points to ArrayList: %2$s";
		String timeTreePattern = "Time to add %1$s points to KD-Tree: %2$s";
		
		int minx = 0, miny = 0, maxx = 1020 , maxy = 720;
		int numPoints = (maxx - minx - 1)*(maxy - miny - 1);
		TestPoint tp;
		
		
		long startTime = System.currentTimeMillis();
		
		
		int[] splitX = new int[]{100, 200, 800};
		
		int[] splitY = new int[]{100, 200, 800};
		
		double[] maxRange, minRange;
		
		ArrayList<TestPoint> out;
		
		
		try
		{
			for (int i=0;i<numPoints;i++)
			{
				tp = TestPoint.getPoint(rdg.nextInt(minx, maxx - 1), rdg.nextInt(miny, maxy - 1));
				points.add(tp);
			}
			System.out.println(String.format(timeArrayPattern,  numPoints, (System.currentTimeMillis() - startTime)));
			startTime = System.currentTimeMillis();
			tree.addAllPoints(points);
			System.out.println(String.format(timeTreePattern,  numPoints, (System.currentTimeMillis() - startTime)));
			int[] deleteCounts = new int[splitY.length], addCount = new int[splitY.length], expectedRemaining = new int[splitY.length];
			
			for (int i=0;i<splitY.length;i++)
			{
				startTime = System.currentTimeMillis();
				maxRange = new double[]{splitX[i], splitY[i]};
				minRange = new double[]{0, 0};
				
				out = tree.getRange(maxRange, minRange);
				addCount[i] = out.size();
				System.out.println(String.format("Search range (%1$s, %2$s) - (%3$s, %4$s) : %5$s points and %6$s ms", minRange[0], minRange[1], maxRange[1], maxRange[1],  addCount[i], (System.currentTimeMillis() - startTime)));
				tree.cleanup();
				out = tree.getRange(maxRange, minRange);
				Assert.assertTrue("Failure to verify cleanup.  Expected: " + out.size() + " = " + addCount[i], out.size() == addCount[i]);
			}
			int previouslyDeleted = 0;
			for (int i=0;i<splitY.length;i++)
			{
				startTime = System.currentTimeMillis();
				maxRange = new double[]{splitX[i], splitY[i]};
				minRange = new double[]{0, 0};
				
				out = tree.deleteRange(maxRange, minRange);
				deleteCounts[i] = out.size();
				
				System.out.println(String.format("delete range (%1$s, %2$s) - (%3$s, %4$s) : %5$s points and %6$s ms", minRange[0], minRange[1], maxRange[1], maxRange[1],  deleteCounts[i], (System.currentTimeMillis() - startTime)));
				//tree.cleanup();
				out = tree.getRange(maxRange, minRange);
				Assert.assertTrue("Assert failure: points not deleted.  Expected 0 found " + out.size(), out.size() == 0);
				Assert.assertTrue(String.format("Assert failure: deleted in range not equal added in range: %1$s added , %2$s deleted", addCount[i], deleteCounts[i]), deleteCounts[i] == (addCount[i] - previouslyDeleted));
				previouslyDeleted +=deleteCounts[i];
			}
			
			startTime = System.currentTimeMillis();
			tree.cleanup();
			System.out.println(String.format("Time to cleanup KD-tree %1$s ms", (System.currentTimeMillis() - startTime)));
			startTime = System.currentTimeMillis();
			maxRange = new double[]{splitX[splitY.length- 1], splitY[splitY.length - 1]};
			minRange = new double[]{0, 0};
			
			out = tree.getRange(maxRange, minRange);
			System.out.println(String.format("updated remaining vaues (%1$s, %2$s) - (%3$s, %4$s) : %5$s points and %6$s ms", minRange[0], minRange[1], maxRange[1], maxRange[1],  out.size(), (System.currentTimeMillis() - startTime)));
			
			Assert.assertTrue(String.format("Assert failure: Remaining points after cleanup: %1$s remaining , expected %2$s", out.size(), addCount[splitY.length- 1] - previouslyDeleted ), out.size() == (addCount[splitY.length- 1] - previouslyDeleted));
			
		}
		catch (Exception e)
		{
			Assert.assertTrue( "General error: " + e.toString(), false);
		}
		
	}
	
	
	private static class TestPoint implements GeneralizedPoint
	{
		int x;
		int y;
		
		private TestPoint(int x, int y)
		{
			this.x = x;
			this.y = y;
		}
		
		public static TestPoint getPoint(int x, int y)
		{
			return new TestPoint(x,y);
		}
		
		public double[] getCoordinates()
		{
			return new double[]{(int)x, (int)y};
		}
		
		@Override
		public String toString()
		{
			return "(" + x + ", " + y + ")";
		}
	}
	
}
