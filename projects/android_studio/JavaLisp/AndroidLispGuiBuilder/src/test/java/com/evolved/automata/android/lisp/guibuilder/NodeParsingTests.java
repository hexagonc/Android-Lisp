package com.evolved.automata.android.lisp.guibuilder;

import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;

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
        String expectedResult = StringUtils.replace(string, "\\\"", "\"");
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
        String expectedResult = StringUtils.replace(string, "\\\"", "\"");
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
            String input = "10 23 \"up\" -12.89 \"   \" + rotate-left-90 (upside-down)";
            int numChildCount = 0, expectedNumChildren = 15, expectedTokens = 8, numTokens ;
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
            Assert.assertTrue(errorMessage, expectedNumChildren == numChildCount);
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

}
