package com.evolved.automata.lisp;

import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import junit.framework.Assert;

import org.apache.commons.math3.random.RandomDataGenerator;
import org.junit.Test;

import com.evolved.automata.lisp.LispInterpreter.LispInputListener;
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
	public void testStringInString()
	{
		String testString = "\"\\\"expression\\\"\"";
		try
		{
			LinkedList<Value> processed = Environment.parse(testString, true);
			assertTrue(processed.size()==1);
			Value result = processed.getFirst();
			assertTrue(result.isString());
			String rawString = result.getString();
			assertTrue(rawString.length()>0);
			int doubleQuoteCount = 0;
			int backSlashCount = 0;
			for (char c:rawString.toCharArray())
			{
				if (c == '\"')
					doubleQuoteCount++;
				if (c == '\\')
					backSlashCount++;
			}
			assertTrue(backSlashCount == 0 && doubleQuoteCount == 2);
			assertTrue(rawString.startsWith("\"") && rawString.endsWith("\""));
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
				public void onGeneralException(Throwable e) {
					
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
	public void testAsynchEarlyContinuationBreak()
	{
		Environment env = new Environment();
		Thread t = null;
		try
		{
			final CountDownLatch finishLatch = new CountDownLatch(1);
			NLispTools.addDefaultFunctionsAddMacros(env);
			int timeoutMilli = 4000;
			final int breakMilli = 1000;
			int failureTimeout = timeoutMilli + 1000;
			env.mapValue("timeout", NLispTools.makeValue(timeoutMilli));
			addAssertFunction(env);
			long startTime = System.currentTimeMillis();
			LispInterpreter interpreter = new LispInterpreter(env);
			
			final LispInputListener inputListener = interpreter.start(new LispResponseListener()
			{

				@Override
				public void onOutput(Value out) {
					if (finishLatch.getCount()>0)
						finishLatch.countDown();
				}

				@Override
				public void onIncompleteInputException(String message) {
					Assert.assertTrue("Incomplete input exception", false);
					if (finishLatch.getCount()>0)
						finishLatch.countDown();
				}

				@Override
				public void onGeneralException(Throwable e) {
					Assert.assertTrue("Lisp Exception: " + e.toString(), false);
					if (finishLatch.getCount()>0)
						finishLatch.countDown();
				}
				
			}, true);
			
			inputListener.evaluateExpression("(progn (setq stop-time (+ timeout (time))) (unless (> (time) stop-time)) (assert-true F \"break failure\"))");
			t = new Thread()
			{
				public void run()
				{
					try
					{
						Thread.sleep(breakMilli);
						inputListener.breakExecution();
						if (finishLatch.getCount()>0)
							finishLatch.countDown();
					}
					catch (InterruptedException ie)
					{
						
					}
				}
						
			};
			t.start();
			Assert.assertTrue("Exception due to general failure", finishLatch.await(failureTimeout, TimeUnit.MILLISECONDS));
			Assert.assertTrue((System.currentTimeMillis() - startTime) < (timeoutMilli + breakMilli)/2 );
			interpreter.stop(false, 1000);
		}
		catch (Exception e)
		{
			Assert.assertTrue("Exception: " + e.toString(), false);
			if (t!=null && t.isAlive())
				t.interrupt();
		}
		
	}
	
	
	@Test
	public void testSimulataneousTimers()
	{
		int processCount = 4;
		
		Environment env = new Environment();
		
		env.mapValue("process-count", NLispTools.makeValue(processCount));
		LispInterpreter interpreter = new LispInterpreter(env);
		CountDownLatch completionLatch = new CountDownLatch(processCount);
		final LispInputListener inputListener = interpreter.start(new LispResponseListener()
		{

			@Override
			public void onOutput(Value out) {
				
			}

			@Override
			public void onIncompleteInputException(String message) {
				Assert.assertTrue("Incomplete input exception", false);
				
			}

			@Override
			public void onGeneralException(Throwable e) {
				Assert.assertTrue("Lisp Exception: " + e.toString(), false);
				
			}
			
		}, true);
		
		addEvaluateBackgroundFunction(env, inputListener);
		
		addCountdownFunction(env, completionLatch);
		addPrintFunction(env);
		int failureTimeout = processCount * 1000*2;
		long start = System.currentTimeMillis();
		long lowerBound = start + 1000*processCount, upperBound = start + 1500*processCount;
		try
		{
			NLispTools.addDefaultFunctionsAddMacros(env);
			inputListener.evaluateExpression("(defun countdown (count) (evaluate-background (for i count (latch-count-down) (progn (print \"Countdown: \" count \": \" i) (setq stop (+ (time) 1000)) (unless (> (time) stop)))))) (for i process-count process-count (countdown (+ i 1)))");
			Assert.assertTrue("await timeout failure: ",completionLatch.await(failureTimeout, TimeUnit.MILLISECONDS));
			long stop = System.currentTimeMillis();
			Assert.assertTrue("Process completion failure", stop >=lowerBound && stop <= upperBound);
		}
		catch (Exception e)
		{
			Assert.assertTrue("General test error: " + e.toString(), false);
		}
		finally
		{
			interpreter.stop(false, 1000);
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
	
	@Test
	public void testKMeans()
	{
		int numClusters = 3;
		int numSamplesPerCluster = 30;
		
		double[] sample = new double[numClusters*numSamplesPerCluster];
		
		
		int k=0;
		double min = 0, max = 100;
		
		double step = (max - min)/(numClusters + 1);
		
		double[] expectedCentroid = new double[numClusters];
		
		RandomDataGenerator rgenerator = new RandomDataGenerator();
		for (int i=0;i<numSamplesPerCluster+1;i++)
		{
			for (int j=1;j<=numClusters;j++)
			{
				if (i == 0)
				{
					expectedCentroid[j - 1] = min + j*step;
				}
				else
				{
					sample[k] = min + j*step + rgenerator.nextUniform(-step/2, step/2); 
					k++;
				}
			}
		}
		
		Environment env = new Environment();
		ExtendedFunctions.addExtendedFunctions(env);
		
		double errorF = step/4;
		Value input = NLispTools.makeValue(sample);
		Value numCentroids = NLispTools.makeValue(numClusters);
		Value error = NLispTools.makeValue(errorF);
		double calculatedCentroid;
		try {
			FunctionTemplate ft = env.getFunction("simple-k-means");
			ft.setActualParameters(new Value[]{input, numCentroids, error});
			Value out = ft.evaluate(env, false);
			for (int i=0;i<numClusters;i++)
			{
				calculatedCentroid = out.getList()[i].getList()[0].getFloatValue();
				assertTrue(Math.abs(calculatedCentroid - expectedCentroid[i]) <= errorF);
			}
			
			
		} catch (InstantiationException e) {
			assertTrue(e.toString(), false);
		} catch (IllegalAccessException e) {
			assertTrue(e.toString(), false);
		}
	}
	
	@Test
	public void testDestructiveAppend()
	{
		Environment env = new Environment();
		try
		{
			NLispTools.addDefaultFunctionsAddMacros(env);
			addAssertFunction(env);
			String oldTestExpression = "(setq old-method ()) (setq start (time)) (for i 10000 (- (time) start) (set old-method (append old-method i)))";
			String newTestExpression = "(setq new-method ()) (setq start (time)) (for i 10000 (- (time) start) (destructive-append new-method i))";
			String compareExpression = "(assert-true (equals old-method new-method))";
			
			
			Value oldTimeV = env.evaluate(oldTestExpression, true);
			Value newTimeV = env.evaluate(newTestExpression, true);
			System.out.println("Old time: " + oldTimeV + " new method time: " + newTimeV);
			assertTrue(oldTimeV.getIntValue()>newTimeV.getIntValue());
			Value comp = env.evaluate(compareExpression, true);
			
		}
		catch (Exception e)
		{
			assertTrue(e.toString(), false);
		}
		
	}
	

	
	public void testCode()
	{
		double late = 37.7940865;
		double lone = -122.4115304;
		
		double lats = 40.7481018;
		double lons = -73.9848219;
		
		double dlon = Math.PI/180*(lone - lons);
		double dlat = Math.PI/180*(late = lats);
		
		
	}

}
