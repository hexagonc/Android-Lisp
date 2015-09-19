package com.evolved.automata.desktop;
import com.evolved.automata.nlisp.Environment;
import com.evolved.automata.nlisp.ExtendedFunctions;
import com.evolved.automata.nlisp.IncompleteLispExpressionException;
import com.evolved.automata.nlisp.NLispTools;
import com.evolved.automata.nlisp.SimpleFunctionTemplate;
import com.evolved.automata.nlisp.Value;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.LinkedList;

public class LispTester {

	
	
	/**
	 * @param args
	 */
	public static void main(String[] args) 
	{
		
		
		final Environment top;
		boolean skipCont = false;
		try
		{
			top = new Environment();
			NLispTools.addDefaultFunctionsAddMacros(top);
			ExtendedFunctions.addExtendedFunctions(top);
			top.mapFunction("println", new SimpleFunctionTemplate()
			{

				@Override
				public Value evaluate(Environment env,Value[] evaluatedArgs) {
					checkActualArguments(1, true, true);
				
					StringBuilder sBuilder = new StringBuilder();
					for (int i = 0;i<evaluatedArgs.length;i++)
					{
						sBuilder.append((evaluatedArgs[i].isString())?evaluatedArgs[i].getString():evaluatedArgs[i].toString());
					}
					String out = sBuilder.toString();
					System.out.println(out);
					return NLispTools.makeValue(out);
				}
				
			}
			);
			String initialTitle = "New Lisp";
			String continuationTitle = "Total";
			AIInfoDisplay display = new AIInfoDisplay(initialTitle);
			
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
				try
				{
					parsedResult = Environment.parse(command.toString(), true);
				}
				catch (IncompleteLispExpressionException icl)
				{
					continue;
				}
				command = new StringBuilder();
				try
				{
					for (Value comp:parsedResult)
					{
						result = top.evaluate(comp, false);
						if (!skipCont)
						{
							while (result.isContinuation())
							{
								if (!continuationTitle.equals(currentTitle))
								{
									display.setTitle(continuationTitle);
									currentTitle = continuationTitle;
								}
								result = result.getContinuingFunction().evaluate(top, true);
							}
						}
						
					}
					
					if (!initialTitle.equals(currentTitle))
					{
						display.setTitle(initialTitle);
						currentTitle = initialTitle;
					}
					
					if (result!=null)
						display.DisplayMessageLine(result.toString(), true);
				}
				catch (Exception e)
				{
					System.out.println(e.toString());
				}
			}
			
		}
		catch (Exception e)
		{
			System.out.println(e.toString());
		}
		System.exit(0);
	}

}
