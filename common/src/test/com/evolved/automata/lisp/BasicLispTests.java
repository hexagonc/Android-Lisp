package com.evolved.automata.lisp;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class BasicLispTests extends TestHarnessBase
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
	
	@Test
	public void testKeyValueParameters()
	{
		try
		{
			Environment env = new Environment();
			NLispTools.addDefaultFunctionsAddMacros(env);
			addAssertFunction(env);
			env.evaluate("(multiple-bind (x y z) (10 100 1000))", true);
			env.evaluate("(defmacro assert-key-args-present () '(assert-true (> (length key-map) 0)))", true);
			env.evaluate("(defmacro assert-contains-keys (...) `(assert-true (all key ,... (gethash key-map key))))", true);
			env.evaluate("(defmacro key-value (name) `(gethash key-map ,name))", true);
			env.evaluate("(defun check-only-key-parameters (...) (assert-key-args-present) (assert-contains-keys :x :y) (assert-true (= (key-value :x) x)) (assert-true (= (key-value :y) y)))", true);
			env.evaluate("(defun check-mixed-parameter-types (normal-parameter ...) (assert-key-args-present) (assert-contains-keys :x :y) (assert-true (= normal-parameter z)) (assert-true (= (key-value :x) x)) (assert-true (= (key-value :y) y)))", true);
			env.evaluate("(check-only-key-parameters :x x :y y)", true);
			env.evaluate("(check-mixed-parameter-types z :y y :x x)", true);
		}
		catch (IllegalAccessException ia)
		{
			assertTrue(false);
		}
		catch (InstantiationException ie)
		{
			assertTrue(false);
		}
		
	}
	
	
}
