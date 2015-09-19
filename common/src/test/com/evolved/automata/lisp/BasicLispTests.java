package com.evolved.automata.lisp;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class BasicLispTests 
{
	@Test
	public void testNumericCalculations()
	{
		try
		{
			Environment env = new Environment();
			NLispTools.addDefaultFunctionsAddMacros(env);
			
			Value out = env.evaluate("(+ 23 56)", true);
			assertTrue(NLispTools.isNumericType(out));
			assertTrue(out.getIntValue() == (23 + 56));
			
		}
		catch (Exception e)
		{
			assertTrue(false);
		}
	}
}
