package com.evolved.automata.lisp.editor;

import org.junit.Assert;
import org.junit.Test;

import java.util.HashSet;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 4/25/17.
 */

public class CodeCompletionTests {

    static String[] testFunctionNames = new String[]{"mapcar", "map-filter", "setq" , "defun", "switch", "for", "equals", "while", "unless", "break", "signal", "signal-block", "return", "set", "with", "all", "some", "if", "and", "not", "or", "lambda", "integer", "string", "make-int-hashtable", "make-string-hashtable", "get-hash-keys", "funcall", "apply", "defhash", "gethash", "minimal-value-map", "maximum-value-map", "sin", "cos", "tan", "arg-max", "max", "min", "find", "fill-list", "make-range"};

    @Test
    public void testWordCompletionStorage()
    {
        String errorMessage = "Failed to create WordCompletor";
        try
        {
            WordCompletor completer = new WordCompletor();
            errorMessage = "Failed to add all words";

            HashSet<String> wordMap = new HashSet<String>();

            for (String name:testFunctionNames)
            {
                completer.addWord(name);
                wordMap.add(name);
            }

            errorMessage = "Failed to get all completions";

            LinkedList<String> completions = completer.getAllWords();

            Assert.assertTrue(errorMessage, completions != null && completions.size() > 0);


            errorMessage = "Mismatching data size.  Expected " + testFunctionNames.length + " found: " + completions.size();
            Assert.assertTrue(errorMessage, testFunctionNames.length == completions.size());
            errorMessage = "Failed to verify words";

            for (String name:completions)
            {
                Assert.assertTrue(errorMessage, wordMap.contains(name));
            }



        }
        catch (Exception e)
        {
            e.printStackTrace();;
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testWordCompletion()
    {
        String errorMessage = "Failed to create WordCompletor";
        try
        {
            WordCompletor completer = new WordCompletor();
            errorMessage = "Failed to add all words";

            HashSet<String> wordMap = new HashSet<String>();

            for (String name:testFunctionNames)
            {
                completer.addWord(name);
                wordMap.add(name);
            }

            errorMessage = "Failed to get all completions";

            LinkedList<String> completions = completer.getAllWords();

            Assert.assertTrue(errorMessage, completions != null && completions.size() > 0);


            errorMessage = "Mismatching data size.  Expected " + testFunctionNames.length + " found: " + completions.size();
            Assert.assertTrue(errorMessage, testFunctionNames.length == completions.size());
            errorMessage = "Failed to verify words";

            for (String name:completions)
            {
                Assert.assertTrue(errorMessage, wordMap.contains(name));
            }

            String testword = "make-range";
            int expectedFinalCompletions = 1;


            Character ch = null;
            boolean first = true;
            for (char c:testword.toCharArray())
            {
                ch = Character.valueOf(c);
                if (first)
                {
                    completions = completer.resetCompletionIterator(ch);
                }
                else
                    completions = completer.continueCompletion(ch);
                first = false;
                Assert.assertTrue("Completions cannot be null or empty at this point", completions != null && completions.size()>0);
                System.out.println("Current completions: " + completions);
            }

            errorMessage = "Incorrect final number of completions: Expected " + expectedFinalCompletions + " but found " + completions.size();
            Assert.assertTrue(errorMessage, completions.size() == expectedFinalCompletions);


        }
        catch (Exception e)
        {
            e.printStackTrace();;
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testWordRemoval()
    {
        String errorMessage = "Failed to create WordCompletor";
        try
        {
            WordCompletor completer = new WordCompletor();
            errorMessage = "Failed to add all words";

            HashSet<String> wordMap = new HashSet<String>();

            for (String name:testFunctionNames)
            {
                completer.addWord(name);
                wordMap.add(name);
            }

            errorMessage = "Failed to get all completions";

            LinkedList<String> completions = completer.getAllWords();

            Assert.assertTrue(errorMessage, completions != null && completions.size() > 0);


            errorMessage = "Mismatching data size.  Expected " + testFunctionNames.length + " found: " + completions.size();
            Assert.assertTrue(errorMessage, testFunctionNames.length == completions.size());


            String wordToRemove = "set";
            errorMessage = "Failed to attempt to remove word";
            completer.removeWord(wordToRemove);

            completions = completer.getAllWords();



            errorMessage = "Failed to verify word removal";
            for (String name:completions)
            {
                errorMessage = "Failed to verify word removal";
                Assert.assertTrue(errorMessage, !name.equals(wordToRemove));
                errorMessage = "Failed to verify other words";
                Assert.assertTrue(errorMessage, wordMap.contains(name));
            }

            wordToRemove = "setq";

            completer.removeWord(wordToRemove);

            completions = completer.getAllWords();



            errorMessage = "Failed to verify word removal";
            for (String name:completions)
            {
                errorMessage = "Failed to verify word removal";
                Assert.assertTrue(errorMessage, !name.equals(wordToRemove));
                errorMessage = "Failed to verify other words";
                Assert.assertTrue(errorMessage, wordMap.contains(name));
            }

        }
        catch (Exception e)
        {
            e.printStackTrace();;
            Assert.assertTrue(errorMessage, false);
        }
    }
}
