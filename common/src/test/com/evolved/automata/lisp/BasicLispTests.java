package com.evolved.automata.lisp;

import static org.junit.Assert.assertTrue;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.junit.Test;

import com.evolved.automata.lisp.LispInterpreter.LispResponseListener;

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
	
	@Test
	public void testAsynchLispInterpreterStartupAndShutdown()
	{
		try
		{
			// .:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_
			//			Start of Initial Setup
			// .:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_
			
			final Environment env = new Environment();
			NLispTools.addDefaultFunctionsAddMacros(env);
			addAssertFunction(env);
			final LinkedBlockingQueue<Value> queue = new LinkedBlockingQueue<Value>();
			
			final LispInterpreter.LispResponseListener responseListener = new LispResponseListener()
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
					
				}
				
				@Override
				public void onGeneralException(Exception e) {
					
				}
			};
			
			//						End of Setup
			// -<(O)>-^-<(O)>-^-<(O)>-^-<(O)>-^-<(O)>-^-<(O)>-^-<(O)>-^-<(O)>-^-<(O)>- 
			
			int testSteps = 3;
			final CountDownLatch interlock = new CountDownLatch(testSteps);
			Thread testThread = new Thread()
			{
				public void run()
				{
					try
					{
						LispInterpreter interpreter = new LispInterpreter(env);
						LispInterpreter.LispInputListener controlListener = interpreter.start(responseListener, true);
						
						// Step one: Started up interpreter
						interlock.countDown();
						controlListener.evaluateExpression("1");
						Value first;
						
						first = queue.take();
						assertTrue(first.getIntValue() == 1);
						// Step two: properly evaluated expression
						interlock.countDown();
						assertTrue(interpreter.stop(true, 200));
						// Step three: shut down with time limit
						interlock.countDown();
						
					}
					catch (InterruptedException e) {
						
					}
					
					
				}
			};
			testThread.setDaemon(true);
			testThread.start();
			
			// Overall test completion
			interlock.await(500, TimeUnit.MILLISECONDS); 
			
		}
		catch (IllegalAccessException ia)
		{
			assertTrue(false);
		}
		catch (InstantiationException ie)
		{
			assertTrue(false);
		} catch (InterruptedException e) {
			assertTrue(false);
		}
		
	}
	
	
	
	
	@Test
	public void testAsynchLispInterpreterContinuation1()
	{
		try
		{
			// .:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_
			//			Start of Initial Setup
			// .:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_
			
			final Environment env = new Environment();
			NLispTools.addDefaultFunctionsAddMacros(env);
			addAssertFunction(env);
			final LinkedBlockingQueue<Value> queue = new LinkedBlockingQueue<Value>();
			
			
			
			//						End of Setup
			// -<(O)>-^-<(O)>-^-<(O)>-^-<(O)>-^-<(O)>-^-<(O)>-^-<(O)>-^-<(O)>-^-<(O)>- 
			
			int testSteps = 3;
			final CountDownLatch interlock = new CountDownLatch(testSteps);
			final long waitSeconds = 4;
			Thread testThread = new Thread()
			{
				public void run()
				{
					LispInterpreter interpreter = new LispInterpreter(env);
					try
					{
						
						LispInterpreter.LispInputListener controlListener = interpreter.start(getStandardResponseListener(queue), true);
						
						// Step one: Started up interpreter
						step(interlock, "Step one: Started up interpreter");
						
						controlListener.evaluateExpression(String.format("(progn (setq stop (+ (time) (* 1000 %1$s))) (unless (> (time) stop) (sleep-milli 20)) \"finish\")", waitSeconds));
						Value result;
						
						result = queue.take();
						assertTrue(result.isString() && "finish".equals(result.getString()));
						// Step two: properly evaluated expression
						step(interlock, "Step two: properly evaluated expression");
						assertTrue(interpreter.stop(true, 200));
						// Step three: shut down with time limit
						step(interlock, "Step three: shut down within time limit");
						interlock.countDown();
						
					}
					catch (InterruptedException e) {
						
					}
					finally
					{
						if (interpreter.isRunning())
							interpreter.stop(false, -1);
					}
					
				}
			};
			testThread.setDaemon(true);
			testThread.start();
			
			// Overall test completion
			// should be false since test is expected to take longer than 2 seconds
			assertTrue(!interlock.await(2000, TimeUnit.MILLISECONDS)); 
			
		}
		catch (IllegalAccessException ia)
		{
			assertTrue(false);
		}
		catch (InstantiationException ie)
		{
			assertTrue(false);
		} catch (InterruptedException e) {
			assertTrue(false);
		}
		
	}
	
	
	@Test
	public void testAsynchLispInterpreterContinuation2()
	{
		try
		{
			// .:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_
			//			Start of Initial Setup
			// .:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_.:/`o_
			
			final Environment env = new Environment();
			NLispTools.addDefaultFunctionsAddMacros(env);
			addAssertFunction(env);
			final LinkedBlockingQueue<Value> queue = new LinkedBlockingQueue<Value>();
			
			
			
			//						End of Setup
			// -<(O)>-^-<(O)>-^-<(O)>-^-<(O)>-^-<(O)>-^-<(O)>-^-<(O)>-^-<(O)>-^-<(O)>- 
			
			int testSteps = 3;
			final CountDownLatch interlock = new CountDownLatch(testSteps);
			final long waitSeconds = 4;
			Thread testThread = new Thread()
			{
				public void run()
				{
					LispInterpreter interpreter = new LispInterpreter(env);
					try
					{
						
						LispInterpreter.LispInputListener controlListener = interpreter.start(getStandardResponseListener(queue), true);
						
						// Step one: Started up interpreter
						step(interlock, "Step one: Started up interpreter");
						
						controlListener.evaluateExpression(String.format("(progn (setq stop (+ (time) (* 1000 %1$s))) (unless (> (time) stop) (sleep-milli 20)) \"finish\")", waitSeconds));
						Value result;
						
						result = queue.take();
						assertTrue(result.isString() && "finish".equals(result.getString()));
						// Step two: properly evaluated expression
						step(interlock, "Step two: properly evaluated expression");
						assertTrue(interpreter.stop(true, 200));
						// Step three: shut down with time limit
						step(interlock, "Step three: shut down within time limit");
						interlock.countDown();
						
					}
					catch (InterruptedException e) {
						
					}
					finally
					{
						if (interpreter.isRunning())
							interpreter.stop(false, -1);
					}
					
				}
			};
			testThread.setDaemon(true);
			testThread.start();
			
			// Overall test completion
			// should should be done no more than 4.1 seconds after start
			assertTrue(interlock.await(4100, TimeUnit.MILLISECONDS));
			//interlock.await();
			
		}
		catch (IllegalAccessException ia)
		{
			assertTrue(false);
		}
		catch (InstantiationException ie)
		{
			assertTrue(false);
		} catch (InterruptedException e) {
			assertTrue(false);
		}
		
	}
}