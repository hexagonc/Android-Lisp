package com.evolved.automata.android.lisp.guibuilder;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.lisp.editor.ParseContext;
import com.evolved.automata.lisp.editor.ParseNode;
import com.evolved.automata.lisp.editor.TopParseNode;

import org.junit.Assert;
import org.junit.Test;

import java.io.BufferedReader;
import java.io.StringReader;
import java.util.HashSet;

/**
 * Created by Evolved8 on 4/26/17.
 */

public class AndroidCodeEditorTests {

    @Test
    public void testSimpleParenthesisError()
    {
        String errorMessage = "Failed to test for errors";
        try
        {
            String errorInput = ")";
            TopParseNode topNode = new TopParseNode();
            errorMessage = "Failed to create simple parse context";
            ParseContext simpleContext = new LispCodeEditorParseContext();
            topNode.setContext(simpleContext);
            errorMessage = "Failed to process all input";
            topNode.processAll(errorInput);
            String result = topNode.getValue();

            errorMessage = "Failed to get correct input: expected [" + errorInput + "] but found: [" + result + "]";

            Assert.assertTrue(errorMessage, result.equals(errorInput));

            HashSet<ParseNode> errorNodes = simpleContext.getErrorNodes();
            int errorCount = errorNodes.size();
            int expectedCount = 1;
            errorMessage = "Incorrect error count: expected [" + expectedCount + "] but found [" + errorCount + "]";
            Assert.assertTrue(errorMessage, expectedCount == errorCount);

        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testSimpleParenthesisErrorWithValidInput()
    {
        String errorMessage = "Failed to test for errors";
        try
        {
            String errorInput = "12 ) xyz (+ 12 89) )abc";
            TopParseNode topNode = new TopParseNode();
            errorMessage = "Failed to create simple parse context";
            ParseContext simpleContext = new LispCodeEditorParseContext();
            topNode.setContext(simpleContext);
            errorMessage = "Failed to process all input";
            topNode.processAll(errorInput);
            String result = topNode.getValue();

            errorMessage = "Failed to get correct input: expected [" + errorInput + "] but found: [" + result + "]";

            Assert.assertTrue(errorMessage, result.equals(errorInput));

            HashSet<ParseNode> errorNodes = simpleContext.getErrorNodes();
            System.out.println("Error tokens: " + errorNodes);
            int errorCount = errorNodes.size();
            int expectedCount = 2;
            errorMessage = "Incorrect error count: expected [" + expectedCount + "] but found [" + errorCount + "]";
            Assert.assertTrue(errorMessage, expectedCount == errorCount);
            System.out.println("All children: " + topNode.getTokenChildren());
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    public String stripComments(String expr)
    {
        BufferedReader breader = new BufferedReader(new StringReader(expr ));
        StringBuilder builder = new StringBuilder();
        String line = null;
        try
        {
            while ((line = breader.readLine())!=null)
            {
                if (!line.trim().startsWith(";"))
                    builder.append(line).append(' ');
            }
        }
        catch (Exception e)
        {
            throw new RuntimeException(e.toString());
        }
        return builder.toString();
    }


    @Test
    public void testStripComments()
    {
        String expression = "(setq x 12) \n ; (setq x 20)\n(* x 10)";

        Environment env = new Environment();

        try
        {
            NLispTools.addDefaultFunctionsAddMacros(env);
            String stripped = stripComments(expression);
            Value result = env.evaluate(stripped, false);
            Assert.assertTrue("Failed to strip properly", result.getIntValue() == 120);
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }
}
