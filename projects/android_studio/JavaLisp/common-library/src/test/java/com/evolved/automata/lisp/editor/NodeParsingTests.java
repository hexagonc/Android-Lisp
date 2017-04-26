package com.evolved.automata.lisp.editor;

import com.evolved.automata.lisp.editor.ListNode;
import com.evolved.automata.lisp.editor.ParseContext;
import com.evolved.automata.lisp.editor.ParseNode;
import com.evolved.automata.lisp.editor.StringNode;
import com.evolved.automata.lisp.editor.TopParseNode;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.HashSet;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 4/22/17.
 */

public class NodeParsingTests {


    @Test
    public void testParsingWhiteSpace()
    {
        String errorMessage = "Failed to create TopParse node";

        try
        {

            String input = "     ";
            TopParseNode node = new TopParseNode();
            errorMessage = "Failed to obtain proper parse state";
            ParseNode.ParseStatus status;
            for (char c:input.toCharArray())
            {
                status = node.appendChar(c);
                Assert.assertTrue(errorMessage, status == ParseNode.ParseStatus.FINISHED);
            }
            String result = node.getValue();
            errorMessage = "Failed to match inputs";
            Assert.assertTrue(errorMessage, result.equals(input));
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testNumberParsing()
    {
        String errorMessage = "Failed to create TopParse node";

        try
        {

            String[] testNumbers = new String[]{"7", "10", "-10", "10.90", "0.89", "-0.98", "-.78"};
            for (String num:testNumbers)
            {
                testNumber(num);
            }
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    private void testNumber(String number)
    {
        String expectedResult = number;
        TopParseNode node = new TopParseNode();
        String errorMessage = "Failed to obtain proper parse state";
        ParseNode.ParseStatus status = null, expectedStatus = ParseNode.ParseStatus.FINISHED;
        for (char c:number.toCharArray())
        {
            status = node.appendChar(c);
            /*
            if (c == '.' || c == '-')
            {
                expectedStatus = ParseNode.ParseStatus.BUILDING;

            }
            else
            {
                expectedStatus = ParseNode.ParseStatus.FINISHED;
            }


            */
        }
        errorMessage =  "With " + number + ".  Failed to obtain proper parse state.  Expected: " + expectedStatus.toString() + " found: " + status.toString();
        Assert.assertTrue(errorMessage, status == expectedStatus);
        String result = node.getValue();
        errorMessage = "Failed to match inputs: Expected [" + expectedResult + "] found: [" + result + "]";
        Assert.assertTrue(errorMessage, result.equals(expectedResult));
    }

    @Test
    public void testParsingStrings()
    {
        try
        {
            String[] testStrings = new String[]{"\"\"", "\" \"", "\"12.89\"", "\"Hello my baby hello my honey\"", "\"Embedded quotes \\\"embedded\\\"\""};
            for (String testString:testStrings)
            {
                testString(testString, ParseNode.ParseStatus.COMPLETE_ABSORB);
                testTopString(testString);
            }
        }
        catch (Exception e)
        {
            e.printStackTrace();

        }
    }

    private void testTopString(String string)
    {
        String expectedResult = string;
        TopParseNode node = new TopParseNode();
        String errorMessage = "Failed to obtain proper parse state";
        ParseNode.ParseStatus status, expectedStatus;
        for (char c:string.toCharArray())
        {
            status = node.appendChar(c);

        }
        String result = node.getValue();
        errorMessage = "Failed to match inputs: Expected [" + expectedResult + "] found: [" + result + "]";

        Assert.assertTrue(errorMessage, result.equals(expectedResult));
    }

    private void testString(String string, ParseNode.ParseStatus finalStatus)
    {
        String expectedResult = string;//StringUtils.replace(string, "\\\"", "\"");
        StringNode node = new StringNode(null);
        String errorMessage = "Failed to obtain proper parse state";
        ParseNode.ParseStatus status, expectedStatus;
        for (char c:string.toCharArray())
        {
            status = node.appendChar(c);
            expectedStatus = ParseNode.ParseStatus.BUILDING;
            errorMessage =  "With " + string + " and input: '" + c + "'.  Failed to obtain proper parse state.  Expected: " + expectedStatus.toString() + " found: " + status.toString();
            //Assert.assertTrue(errorMessage, status == expectedStatus);
        }
        String result = node.getValue();
        errorMessage = "Failed to match inputs: Expected [" + expectedResult + "] found: [" + result + "]";

        Assert.assertTrue(errorMessage, result.equals(expectedResult));
        Assert.assertTrue(errorMessage, node.getStatus() == finalStatus);
    }

    @Test
    public void testParsingVariableNames()
    {
        try
        {
            String[] testStrings = new String[]{"a", "alpha", "A1", "a1+a2", "+", "-X", "t'"};
            for (String testString:testStrings)
            {
                testTopVariablleName(testString);

            }
        }
        catch (Exception e)
        {
            e.printStackTrace();

        }
    }

    private void testTopVariablleName(String string)
    {
        String expectedResult = string;
        TopParseNode node = new TopParseNode();
        String errorMessage = "Failed to obtain proper parse state";
        ParseNode.ParseStatus status, expectedStatus;
        for (char c:string.toCharArray())
        {
            status = node.appendChar(c);

        }
        String result = node.getValue();
        errorMessage = "Failed to match inputs: Expected [" + expectedResult + "] found: [" + result + "]";

        Assert.assertTrue(errorMessage, result.equals(expectedResult));
    }


    @Test
    public void testNumberSequences()
    {
        String errorMessage = "Failed testNumberSequences";
        try
        {
            String input = "10 23";
            TopParseNode node = new TopParseNode();
            ParseNode.ParseStatus status = null, expectedFinalStatus = ParseNode.ParseStatus.FINISHED;
            for (char c:input.toCharArray())
            {
                status = node.appendChar(c);

            }
            String result = node.getValue();
            String expectedResult = input;
            errorMessage = "Failed to match inputs: Expected [" + expectedResult + "] found: [" + result + "]";

            Assert.assertTrue(errorMessage, result.equals(expectedResult));

            errorMessage = "Failed to match final status: Expected: " + expectedFinalStatus.toString() + " but found: " + status.toString();
            Assert.assertTrue(errorMessage, expectedFinalStatus == status);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testSequences()
    {
        String errorMessage = "Failed testSequences";
        try
        {
            String input = "10 ; this is a comment\n23 \"up\" -12.89 \"   \" + rotate-left-90 (upside-down)";
            int numChildCount = 0, expectedNumChildren = 16, expectedTokens = 9, numTokens ;
            TopParseNode node = new TopParseNode();
            ParseNode.ParseStatus status = null, expectedFinalStatus = ParseNode.ParseStatus.FINISHED;
            for (char c:input.toCharArray())
            {
                status = node.appendChar(c);

            }
            String result = node.getValue();
            String expectedResult = input;

            LinkedList<ParseNode> children = node.getChildren();
            numChildCount = children.size();
            errorMessage = "Failed to match inputs: Expected [" + expectedResult + "] found: [" + result + "]";

            Assert.assertTrue(errorMessage, result.equals(expectedResult));

            errorMessage = "Failed to match final status: Expected: " + expectedFinalStatus.toString() + " but found: " + status.toString();
            Assert.assertTrue(errorMessage, expectedFinalStatus == status);

            errorMessage = "Found incorrect num children.  Expected: " + expectedNumChildren + " but found: " + numChildCount;
            Assert.assertTrue(errorMessage, expectedNumChildren == numChildCount);

            numTokens = node.getTokenChildren().size();

            errorMessage = "Found incorrect num tokens.  Expected: " + expectedTokens + " but found: " + numTokens;
            Assert.assertTrue(errorMessage, expectedTokens == numTokens);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testListNodes()
    {
        String errorMessage = "";
        try
        {
            String[] inputs = new String[]{"(+ 12 34)", "()", "(rotate-left \"40 degrees\")"};
            int[][] expectedListChildren = new int[][]{{5,3}, {0,0}, {3, 2}};

            for (int i = 0; i < inputs.length;i++)
            {
                testList(inputs[i], expectedListChildren[i][0], expectedListChildren[i][1]);
            }

        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    private void testList(String listInput, int expectedNumChildren, int expectedNumTokenChildren)
    {
        System.out.println("Testing: " + listInput);
        String errorMessage = "Failed to create top node";
        TopParseNode node = new TopParseNode();
        errorMessage = "Failed to add characters to top node";
        ParseNode.ParseStatus status = null, expectedFinalStatus = ParseNode.ParseStatus.COMPLETE_ABSORB;
        for (char c:listInput.toCharArray())
        {
            status = node.appendChar(c);

        }
        String result = node.getValue();
        String expectedResult = listInput;
        errorMessage = "Failed to match inputs: Expected [" + expectedResult + "] found: [" + result + "]";

        Assert.assertTrue(errorMessage, result.equals(expectedResult));


        LinkedList<ParseNode> children = node.getChildren();

        errorMessage = "Failed to create correct number of top level children";

        int numChildCount = children.size();

        Assert.assertTrue(errorMessage, numChildCount == 1);

        ParseNode child = children.getFirst();

        errorMessage = "Failed to get correct child type.  Expected " + ParseNode.TYPE.LIST.toString() + " but found: " + child.getType().toString();

        Assert.assertTrue(errorMessage, ParseNode.TYPE.LIST == child.getType());

        ListNode listChild = (ListNode)child;

        int numListChildren = listChild.getChildren().size();
        int numListTokenChildren = listChild.getTokenChildren().size();
        errorMessage = "Failed to construct proper number of list children.  Expected: " + expectedNumChildren + " but found: " +  numListChildren;


        Assert.assertTrue(errorMessage, numListChildren == expectedNumChildren);

        errorMessage = "Found incorrect num list token children.  Expected: " + expectedNumTokenChildren + " but found: " + numListTokenChildren;
        Assert.assertTrue(errorMessage, expectedNumTokenChildren == numListTokenChildren);

        status = listChild.getStatus();
        errorMessage = "Failed to match final status: Expected: " + expectedFinalStatus.toString() + " but found: " + status.toString();
        Assert.assertTrue(errorMessage, expectedFinalStatus == status);


    }




    @Test
    public void testSimpleNestedLists()
    {
        String errorMessage = "Failed to create TopParse node";

        try
        {

            String input = "  12   (left (of 12) 89)";
            TopParseNode node = new TopParseNode();
            errorMessage = "Failed to obtain proper parse state";
            ParseNode.ParseStatus status;
            for (char c:input.toCharArray())
            {
                status = node.appendChar(c);
                //Assert.assertTrue(errorMessage, status == ParseNode.ParseStatus.FINISHED);
            }
            String result = node.getValue();
            errorMessage = "Failed to match inputs.  Expected: [" + input + "] but found: [" + result + "]";
            Assert.assertTrue(errorMessage, result.equals(input));

            LinkedList<ParseNode> children = node.getChildren();
            System.out.println("Childre are: " + children);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testSelectingNodes()
    {
        String errorMessage = "Failed to create TopParse node";

        try
        {

            String input = "  12   (left (of 12) 89)";
            TopParseNode node = new TopParseNode();
            errorMessage = "Failed to obtain proper parse state";
            ParseNode.ParseStatus status;
            for (char c:input.toCharArray())
            {
                status = node.appendChar(c);

            }
            String result = node.getValue();
            errorMessage = "Failed to match inputs.  Expected: [" + input + "] but found: [" + result + "]";
            Assert.assertTrue(errorMessage, result.equals(input));

            LinkedList<ParseNode> children = node.getChildren();
            System.out.println("Children are: " + children);


            // Scan
            for (int i = 0;i < input.length();i++)
            {
                ParseNode selectedNode = node.findNode(i);
                System.out.println("Scanning position: " + insertText(input, "|", i) + " found [" + selectedNode + "]");
            }
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    private String insertText(String input, String text, int pos)
    {
        if (pos == 0)
        {
            return text + input;
        }
        else if (pos == input.length())
        {
            return input + text;
        }
        else
            return input.substring(0, pos) + text + input.substring(pos);
    }

    @Test
    public void testBackwardSiblingNavigation()
    {
        String errorMessage = "Failed to create TopParse node";

        try
        {
            boolean wrap = false;
            String input = "12 3.141592 ;Move left please\n (left (of 12) 89) \"over\" \"there\" (map x (list \"x\" \"\\\"over\\\"\") (println x))";
            String expectedResult = input; //StringUtils.replace(input, "\\\"", "\"");
            TopParseNode node = new TopParseNode();
            errorMessage = "Failed to obtain proper parse state";
            ParseNode.ParseStatus status;
            for (char c:input.toCharArray())
            {
                status = node.appendChar(c);

            }
            String result = node.getValue();
            errorMessage = "Failed to match inputs.  Expected: [" + expectedResult + "] but found: [" + result + "]";
            Assert.assertTrue(errorMessage, result.equals(expectedResult));

            LinkedList<ParseNode> children = node.getChildren();
            System.out.println("Children are: " + children);

            ParseNode current = node.getFirstChild(), next;
            System.out.println("Testing backward from start");
            System.out.println("Initial Selection: " + printNodeSelection(input, current));
            next = current.getPreviousTokenSibling(wrap);
            errorMessage = "Expected null previous node, found: " + next + " instead";
            Assert.assertTrue(errorMessage, next == null);

            current = node.getLastChild();
            System.out.println("Testing backward from end");
            System.out.println("Initial Selection: " + printNodeSelection(input, current));

            int selectionCount = node.getTokenChildren().size();
            for (int i = 0;i < selectionCount;i++)
            {
                next = current.getPreviousTokenSibling(wrap);
                System.out.println("Previous Selection: " + printNodeSelection(input, next));
                if (i != selectionCount - 1)
                {
                    errorMessage = "Expected non-null final node";
                    Assert.assertTrue(errorMessage, next != null);
                    current = next;
                }
                else
                {
                    errorMessage = "Expected null final node, found: " + next + " instead";
                    Assert.assertTrue(errorMessage, next == null);
                }

            }

        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testWrappingBackwardSiblingNavigation()
    {
        String errorMessage = "Failed to create TopParse node";

        try
        {
            boolean wrap = true;
            String input = "12 3.141592  (left (of 12) 89) \"over\" \"there\" (map x (list \"x\" \"\\\"over\\\"\") (println x))";
            String expectedResult = input; //StringUtils.replace(input, "\\\"", "\"");
            TopParseNode node = new TopParseNode();
            errorMessage = "Failed to obtain proper parse state";
            ParseNode.ParseStatus status;
            for (char c:input.toCharArray())
            {
                status = node.appendChar(c);

            }
            String result = node.getValue();
            errorMessage = "Failed to match inputs.  Expected: [" + expectedResult + "] but found: [" + result + "]";
            Assert.assertTrue(errorMessage, result.equals(expectedResult));

            LinkedList<ParseNode> children = node.getChildren();
            System.out.println("Children are: " + children);

            ParseNode current = node.getFirstChild(), next, first;
            first = current;
            System.out.println("Testing backward from start");
            System.out.println("Initial Selection: " + printNodeSelection(input, current));

            int step = 0, stepMax = node.getTokenChildren().size();
            while ((next = current.getPreviousTokenSibling(wrap)) != first)
            {
                errorMessage = "Expected non-null final node";
                Assert.assertTrue(errorMessage, next != null);
                System.out.println("Previous Selection: " + printNodeSelection(input, next));
                step++;
                current = next;
            }
            errorMessage = "Expected wrap to beginning after " + (stepMax - 1) + " steps";
            Assert.assertTrue(errorMessage, step == stepMax - 1);



        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testForwardSiblingNavigation()
    {
        String errorMessage = "Failed to create TopParse node";

        try
        {
            boolean wrap = false;
            String input = "12 3.141592  (left (of 12) 89) \"over\" \"there\" (map x (list \"x\" \"\\\"over\\\"\") (println x))";
            String expectedResult = input; //StringUtils.replace(input, "\\\"", "\"");
            TopParseNode node = new TopParseNode();
            errorMessage = "Failed to obtain proper parse state";
            ParseNode.ParseStatus status;
            for (char c:input.toCharArray())
            {
                status = node.appendChar(c);

            }
            String result = node.getValue();
            errorMessage = "Failed to match inputs.  Expected: [" + expectedResult + "] but found: [" + result + "]";
            Assert.assertTrue(errorMessage, result.equals(expectedResult));

            LinkedList<ParseNode> children = node.getChildren();
            System.out.println("Children are: " + children);

            ParseNode current = node.getLastChild(), next;
            System.out.println("Testing forward from end");
            System.out.println("Initial Selection: " + printNodeSelection(input, current));
            next = current.getNextTokenSibling(wrap);
            errorMessage = "Expected null next node, found: " + next + " instead";
            Assert.assertTrue(errorMessage, next == null);

            current = node.getFirstChild();
            System.out.println("Testing forward navigation from start");
            System.out.println("Initial Selection: " + printNodeSelection(input, current));

            int selectionCount = node.getTokenChildren().size();
            for (int i = 0;i < selectionCount;i++)
            {
                next = current.getNextTokenSibling(wrap);
                System.out.println("Next Selection: " + printNodeSelection(input, next));
                if (i != selectionCount - 1)
                {
                    errorMessage = "Expected non-null final node";
                    Assert.assertTrue(errorMessage, next != null);
                    current = next;
                }
                else
                {
                    errorMessage = "Expected null final node, found: " + next + " instead";
                    Assert.assertTrue(errorMessage, next == null);
                }

            }

        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testWrappingForwardSiblingNavigation()
    {
        String errorMessage = "Failed to create TopParse node";

        try
        {
            boolean wrap = true;
            String input = "12 3.141592  (left (of 12) 89) \"over\" \"there\" (map x (list \"x\" \"\\\"over\\\"\") (println x))";
            String expectedResult = input; //StringUtils.replace(input, "\\\"", "\"");
            TopParseNode node = new TopParseNode();
            errorMessage = "Failed to obtain proper parse state";
            ParseNode.ParseStatus status;
            for (char c:input.toCharArray())
            {
                status = node.appendChar(c);

            }
            String result = node.getValue();
            errorMessage = "Failed to match inputs.  Expected: [" + expectedResult + "] but found: [" + result + "]";
            Assert.assertTrue(errorMessage, result.equals(expectedResult));

            LinkedList<ParseNode> children = node.getChildren();
            System.out.println("Children are: " + children);

            ParseNode current = node.getLastChild(), next, first;
            first = current;
            System.out.println("Testing forward from end");
            System.out.println("Initial Selection: " + printNodeSelection(input, current));

            int step = 0, stepMax = node.getTokenChildren().size();
            while ((next = current.getNextTokenSibling(wrap)) != first)
            {
                errorMessage = "Expected non-null final node";
                Assert.assertTrue(errorMessage, next != null);
                System.out.println("Next Selection: " + printNodeSelection(input, next));
                step++;
                current = next;
            }
            errorMessage = "Expected wrap to end after " + (stepMax - 1) + " steps";
            Assert.assertTrue(errorMessage, step == stepMax - 1);

        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testTopComment()
    {
        String string = ";  this is garabage\n";
        String expectedResult = string;
        TopParseNode node = new TopParseNode();
        String errorMessage = "Failed to obtain proper parse state";
        ParseNode.ParseStatus status, expectedStatus;
        for (char c:string.toCharArray())
        {
            status = node.appendChar(c);

        }
        String result = node.getValue();
        errorMessage = "Failed to match inputs: Expected [" + expectedResult + "] found: [" + result + "]";

        Assert.assertTrue(errorMessage, result.equals(expectedResult));
    }

    @Test
    public void testTopCommentSequence()
    {
        String string = ";  this is garabage\n; 89 + 23\n12";
        String expectedResult = string;
        TopParseNode node = new TopParseNode();
        String errorMessage = "Failed to obtain proper parse state";
        ParseNode.ParseStatus status, expectedStatus;
        for (char c:string.toCharArray())
        {
            status = node.appendChar(c);

        }
        String result = node.getValue();
        errorMessage = "Failed to match inputs: Expected [" + expectedResult + "] found: [" + result + "]";

        Assert.assertTrue(errorMessage, result.equals(expectedResult));
    }

    @Test
    public void testMinusHandlingList()
    {
        String string = "(- 89 90)";
        String expectedResult = string;
        TopParseNode node = new TopParseNode();
        String errorMessage = "Failed to obtain proper parse state";
        ParseNode.ParseStatus status, expectedStatus;
        for (char c:string.toCharArray())
        {
            status = node.appendChar(c);

        }
        String result = node.getValue();
        errorMessage = "Failed to match inputs: Expected [" + expectedResult + "] found: [" + result + "]";

        Assert.assertTrue(errorMessage, result.equals(expectedResult));
    }

    @Test
    public void testBackTickListHandling()
    {
        String string = "`(- 89 90)";
        String expectedResult = string;
        String errorMessage = "Failed to create top node";
        try
        {

            TopParseNode node = new TopParseNode();

            ParseNode.ParseStatus status;
            for (char c:string.toCharArray())
            {
                status = node.appendChar(c);

            }
            String result = node.getValue();
            errorMessage = "Failed to match inputs: Expected [" + expectedResult + "] found: [" + result + "]";

            Assert.assertTrue(errorMessage, result.equals(expectedResult));
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }

    }

    @Test
    public void testTopSplitList()
    {
        String string = "(89 89\n90 89)";
        String expectedResult = string;
        TopParseNode node = new TopParseNode();
        String errorMessage = "Failed to obtain proper parse state";
        ParseNode.ParseStatus status, expectedStatus;
        for (char c:string.toCharArray())
        {
            status = node.appendChar(c);

        }
        String result = node.getValue();
        errorMessage = "Failed to match inputs: Expected [" + expectedResult + "] found: [" + result + "]";

        Assert.assertTrue(errorMessage, result.equals(expectedResult));
    }

    @Test
    public void testComplexNestedSplitList()
    {
        String string = "(89 (x (y)))";
        String expectedResult = string;
        TopParseNode node = new TopParseNode();
        String errorMessage = "Failed to obtain proper parse state";
        ParseNode.ParseStatus status, expectedStatus;
        for (char c:string.toCharArray())
        {
            status = node.appendChar(c);

        }
        String result = node.getValue();
        errorMessage = "Failed to match inputs: Expected [" + expectedResult + "] found: [" + result + "]";

        Assert.assertTrue(errorMessage, result.equals(expectedResult));
    }

    @Test
    public void testTopSplitListComplex()
    {

        String string = "(setq mark-display-map\n" +
                "\t  (make-int-hashtable (list (list X-MARK-ID \"X\")\n" +
                "\t  \t\t\t\t\t\t\t(list O-MARK-ID \"O\")\n" +
                "\t  \t\t\t\t\t\t\t(list EMPTY-MARK-ID \" \"))))";
        System.out.println("Input:\n" + string);
        String expectedResult = string;
        TopParseNode node = new TopParseNode();
        String errorMessage = "Failed to obtain proper parse state";
        try
        {
            ParseNode.ParseStatus status, expectedStatus;
            for (char c:string.toCharArray())
            {
                status = node.appendChar(c);

            }

            String result = node.getValue();
            errorMessage = "Failed to match inputs: Expected [" + expectedResult + "] found: [" + result + "]";

            Assert.assertTrue(errorMessage, result.equals(expectedResult));
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }

    }



    @Ignore("Need to dork around with gradle to get the test resource to be readable")
    @Test
    public void largeTest()
    {
        String largeFileName = "/com/evolved/automata/lisp/editor/tic-tac-toe-game.lisp";
        String errorMessage = "Failed to open large test file";
        InputStreamReader reader = null;
        InputStream istream = null;
        try
        {
            TopParseNode topNode = new TopParseNode();
            istream = this.getClass().getResourceAsStream(largeFileName);
            reader = new InputStreamReader(istream, Charset.forName("UTF-8"));
            StringBuilder input = new StringBuilder();
            char currentChar;
            int charValue;
            long start = System.currentTimeMillis();
            while ((charValue = reader.read()) != -1)
            {
                currentChar = (char)charValue;
                input.appendCodePoint(charValue);
                topNode.appendChar(currentChar);
            }
            long duration = System.currentTimeMillis() - start;
            System.out.println("Took " + duration + " ms to process input.");

            String result = topNode.getValue();
            errorMessage = "Failed to match parsed result to input.  Result is: " + result;
            Assert.assertTrue(errorMessage, result.equals(input.toString()));
        }
        catch (Exception e)
        {

            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
        finally
        {
            if (reader != null)
            {
                try
                {
                    reader.close();
                }
                catch (Exception e2)
                {
                    e2.printStackTrace();
                }
            }
        }
    }

    private String printNodeSelection(String baseInput, ParseNode selection)
    {
        if (selection == null)
            return baseInput;
        int startIndex = selection.getStartIndex(), length = selection.getLength();
        String startDelimited = insertText(baseInput, "|", startIndex);
        return insertText(startDelimited, "|", startIndex + length + 1);
    }

}
