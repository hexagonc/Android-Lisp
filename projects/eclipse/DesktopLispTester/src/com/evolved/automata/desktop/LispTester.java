package com.evolved.automata.desktop;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.IncompleteLispExpressionException;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.LinkedList;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadPoolExecutor;

public class LispTester {

	public static ScheduledThreadPoolExecutor _executor = new ScheduledThreadPoolExecutor(1);
	
	public static Runnable getWorkItem(final AIInfoDisplay display, final Environment top, final Value input, final LinkedList<Value> remaining)
	{
		return new Runnable(){
			public void run()
			{
				try {
					Value result = top.evaluate(input, false);
					
					if (result.isContinuation())
					{
						display.setTitle("New Lisp, continuing...");
						_executor.submit(getWorkItem(display, top, result.getContinuingFunction(), remaining));
					}
					else {
						if (remaining!= null && remaining.size() > 0)
						{
							_executor.submit(getWorkItem(display, top, remaining.removeFirst(), remaining));
						}
						else
						{
							displayValue(display, result);
						}
					}
					
				} catch (Exception e){
					System.out.println(e.toString());
				}
			}
		};
	}
	
	
	
	
//	public static Runnable getWorkItem(final AIInfoDisplay display, final Environment top, final FunctionTemplate function)
//	{
//		return new Runnable(){
//			public void run()
//			{
//				try {
//					Value result = function.evaluate(top, true);
//					
//					if (result.isContinuation())
//					{
//						getWorkItem(display, top, result.getContinuingFunction(), null);
//					}
//					else {
//						displayValue(display, result);
//					}
//					
//				} catch (Exception e){
//					System.out.println(e.toString());
//				}
//			}
//		};
//	}
	
	public static Runnable getWorkItem(final AIInfoDisplay display, final Environment top, final FunctionTemplate function, final LinkedList<Value> remaining)
	{
		return new Runnable(){
			public void run()
			{
				try {
					Value result = function.evaluate(top, true);
					
					if (result.isContinuation())
					{
						_executor.submit(getWorkItem(display, top, result.getContinuingFunction(), remaining));
					}
					else {
						if (remaining!= null && remaining.size() > 0)
						{
							_executor.submit(getWorkItem(display, top, remaining.removeFirst(), remaining));
						}
						else
						{
							if (_executor.getQueue().size() == 0)
								display.setTitle("New Lisp");
							displayValue(display, result);
						}
					}
					
				} catch (Exception e){
					displayError(display, e);
				}
			}
		};
	}
	
	private static void displayValue(AIInfoDisplay display, Value result)
	{
		if (result!=null)
			display.DisplayMessageLine(result.toString(), true);
	}
	
	private static void displayError(AIInfoDisplay display, Exception result)
	{
		if (result!=null)
			display.DisplayMessageLine(result.toString(), true);
	}
	
	static SimpleFunctionTemplate  getPrintln(final AIInfoDisplay display)
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)getPrintln(display);
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
			
				StringBuilder sBuilder = new StringBuilder();
				for (int i = 0;i<evaluatedArgs.length;i++)
				{
					sBuilder.append((evaluatedArgs[i].isString())?evaluatedArgs[i].getString():evaluatedArgs[i].toString());
				}
				String out = sBuilder.toString();
				display.DisplayMessageLine(out, true);
				
				return NLispTools.makeValue(out);
			}
		};
	}
	
	/**
	 * @param args
	 */
	
	public static void main(String[] args) 
	{
		
		
		final Environment top;
		boolean skipCont = false;
		try
		{
			String initialTitle = "New Lisp";
			String continuationTitle = "Total";
			AIInfoDisplay display = new AIInfoDisplay(initialTitle);
			top = new Environment();
			NLispTools.addDefaultFunctionsAddMacros(top);
			ExtendedFunctions.addExtendedFunctions(top);
			top.mapFunction("println",getPrintln(display));
			
			
			
			BufferedReader b = new BufferedReader(new InputStreamReader(System.in));
			String lineinput;
			LinkedList<Value> parsedResult = null;
			Value result = null;
			String currentTitle = initialTitle;
			display.DisplayMessageLine("Type \"quit\" to exit program.  Type \"clear\" to clear a multiline input buffer.", true);
			StringBuilder command = new StringBuilder();
			while ((lineinput = b.readLine())!=null && !"quit".equalsIgnoreCase(lineinput.trim()))
			{
				if (lineinput.trim().length()==0 || lineinput.trim().startsWith(";"))
					continue;
				command.append(lineinput);
				
				if (lineinput.equals("clear"))
				{
					command = new StringBuilder();
					continue;
				}
				else if (lineinput.equals("break"))
				{
					_executor.shutdown();
					_executor = new ScheduledThreadPoolExecutor(1);
					display.setTitle("New Lisp");
					command = new StringBuilder();
					continue;
				}
				try
				{
					parsedResult = Environment.parse(command.toString(), true);
				}
				catch (IncompleteLispExpressionException icl)
				{
					continue;
				}
				command = new StringBuilder();
				if (parsedResult!=null && parsedResult.size()>0)
					_executor.submit(getWorkItem(display, top, parsedResult.removeFirst(), parsedResult));
			}
			
		}
		catch (Exception e)
		{
			System.out.println(e.toString());
		}
		finally
		{
			_executor.shutdownNow();
		}
		System.exit(0);
	}
	
	
//	public static void main(String[] args) 
//	{
//		
//		
//		final Environment top;
//		boolean skipCont = false;
//		try
//		{
//			String initialTitle = "New Lisp";
//			String continuationTitle = "Total";
//			AIInfoDisplay display = new AIInfoDisplay(initialTitle);
//			top = new Environment();
//			NLispTools.addDefaultFunctionsAddMacros(top);
//			ExtendedFunctions.addExtendedFunctions(top);
//			top.mapFunction("println",getPrintln(display));
//			
//			
//			
//			BufferedReader b = new BufferedReader(new InputStreamReader(System.in));
//			String lineinput;
//			LinkedList<Value> parsedResult = null;
//			Value result = null;
//			String currentTitle = initialTitle;
//			display.DisplayMessageLine("Type \"quit\" to exit program.  Type \"clear\" to clear a multiline input buffer.", true);
//			StringBuilder command = new StringBuilder();
//			while ((lineinput = b.readLine())!=null && !"quit".equalsIgnoreCase(lineinput.trim()))
//			{
//				if (lineinput.trim().length()==0 || lineinput.trim().startsWith(";"))
//					continue;
//				command.append(lineinput);
//				
//				if (lineinput.equals("clear"))
//				{
//					command = new StringBuilder();
//					continue;
//				}
//				try
//				{
//					parsedResult = Environment.parse(command.toString(), true);
//				}
//				catch (IncompleteLispExpressionException icl)
//				{
//					continue;
//				}
//				command = new StringBuilder();
//				try
//				{
//					for (Value comp:parsedResult)
//					{
//						result = top.evaluate(comp, false);
//						if (!skipCont)
//						{
//							while (result.isContinuation())
//							{
//								if (!continuationTitle.equals(currentTitle))
//								{
//									display.setTitle(continuationTitle);
//									currentTitle = continuationTitle;
//								}
//								result = result.getContinuingFunction().evaluate(top, true);
//							}
//						}
//						
//					}
//					
//					if (!initialTitle.equals(currentTitle))
//					{
//						display.setTitle(initialTitle);
//						currentTitle = initialTitle;
//					}
//					
//					if (result!=null)
//						display.DisplayMessageLine(result.toString(), true);
//				}
//				catch (Exception e)
//				{
//					System.out.println(e.toString());
//				}
//			}
//			
//		}
//		catch (Exception e)
//		{
//			System.out.println(e.toString());
//		}
//		System.exit(0);
//	}

}
