package com.evolved.automata.lisp;

import static org.junit.Assert.assertTrue;

public class TestHarnessBase 
{
	protected static void addAssertFunction(Environment env)
	{
		env.mapFunction("assert-true", new SimpleFunctionTemplate() {
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				assertTrue(!evaluatedArgs[0].isNull());
				return evaluatedArgs[0];
			}
		});
	}
}
