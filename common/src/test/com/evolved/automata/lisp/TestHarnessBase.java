package com.evolved.automata.lisp;

import static org.junit.Assert.assertTrue;

public class TestHarnessBase 
{
	
	public class AssertEvaluate extends SimpleFunctionTemplate
	{
		public AssertEvaluate()
		{
			super();
			_name = "assert-true";
		}
		@Override
		public Value evaluate(Environment env, Value[] evaluatedArgs) {
			checkActualArguments(1, false, true);
			assertTrue(!evaluatedArgs[0].isNull());
			return evaluatedArgs[0];
		}
		
		@Override
		public Object clone()
		{
			return new AssertEvaluate();
			
		}
	}
	
	
	
	protected void addAssertFunction(Environment env)
	{
		env.mapFunction("assert-true", new AssertEvaluate());
	}
}
