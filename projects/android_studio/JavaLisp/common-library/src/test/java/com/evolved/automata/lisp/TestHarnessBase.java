package com.evolved.automata.lisp;

import static org.junit.Assert.assertTrue;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.LinkedBlockingQueue;

import com.evolved.automata.lisp.LispInterpreter.LispInputListener;
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
		
		/**
		 * First argument is a value to be tested as true. <br/>
		 * Optional second argument is a string label 
		 */
		@Override
		public Value evaluate(Environment env, Value[] evaluatedArgs) {
			checkActualArguments(1, true, true);
			if (evaluatedArgs.length>1)
				assertTrue(evaluatedArgs[1].getString(), !evaluatedArgs[0].isNull());
			else
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
	
	protected void addCountdownFunction(Environment env, final CountDownLatch latch)
	{
		env.mapFunction("latch-count-down", count_down(latch));
	}
	
	protected void addPrintFunction(Environment env)
	{
		env.mapFunction("print", console_print());
	}
	
	protected void addEvaluateBackgroundFunction(Environment env, LispInputListener inputListener)
	{
		env.mapFunction("evaluate-background", evaluate_background(inputListener));
	}
	
	
	private SimpleFunctionTemplate console_print()
	{
		return new SimpleFunctionTemplate ()
		{
			@Override
			public Object clone()
			{
				return console_print();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				StringBuilder out = new StringBuilder();
				for (Value v:evaluatedArgs)
				{
					if (v.isString())
						out.append(v.getString());
					else
						out.append(v.toString());
				}
				
				System.out.println(out.toString());
				return evaluatedArgs[0];
			}
		};
	}
	
	private FunctionTemplate evaluate_background(final LispInputListener inputListener)
	{
		return new FunctionTemplate ()
		{

			public Object clone()
			{
				return evaluate_background(inputListener);
			}
			
			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				inputListener.evaluateValue(_actualParameters[0], env);
				return Environment.getNull();
			}
			
		};
	}
	
	
	private SimpleFunctionTemplate count_down(final CountDownLatch latch)
	{
		return new SimpleFunctionTemplate()
		{
			
			@Override
			public Object clone()
			{
				return count_down(latch);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				latch.countDown();
				return NLispTools.makeValue(latch.getCount());
			}
			
		};
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
			public void onGeneralException(Throwable e) {
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
