package com.evolved.automata.lisp;

import static org.junit.Assert.assertTrue;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.LinkedBlockingQueue;

import com.evolved.automata.lisp.LispInterpreter.LispResponseListener;

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
	
	/**
	 * Gets a standard Response listener with errors not allowed
	 * @param queue
	 * @return
	 */
	public LispInterpreter.LispResponseListener getStandardResponseListener(final LinkedBlockingQueue<Value> queue)
	{
		return new LispResponseListener()
		{
			
			@Override
			public void onOutput(Value out) {
				try
				{
					queue.put(out);
				}
				catch (Exception e)
				{
					
				}
			}
			
			@Override
			public void onIncompleteInputException(String message) {
				assertTrue(false);
			}
			
			@Override
			public void onGeneralException(Exception e) {
				assertTrue(false);
			}
		};
	}
	
	public void step(CountDownLatch latch, String description)
	{
		latch.countDown();
		System.out.println(description);
	}
}
