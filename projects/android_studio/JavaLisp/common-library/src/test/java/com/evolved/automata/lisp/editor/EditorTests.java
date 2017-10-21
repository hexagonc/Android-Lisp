package com.evolved.automata.lisp.editor;

/**
 * Created by Evolved8 on 8/26/17.
 */
import com.evolved.automata.editor.TextSearchIndex;
import com.evolved.automata.editor.TextSearchResult;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import java.util.Iterator;
import java.util.LinkedList;


public class EditorTests {

    @Test
    public void testAddingCharsToEditor()
    {
        String errorMessage = "Failed to create editor";
        try
        {
            String addedText = "(+ x 12)";
            SimpleTextEditor editor = new SimpleTextEditor("");
            for (char c:addedText.toCharArray())
            {
                errorMessage = "Failed to add character";
                editor.applyTextInsertTransaction("" + c);
            }

            errorMessage = "Failed to get editor text";
            String editorText = editor.getText();

            Assert.assertTrue(errorMessage, editorText != null);

            errorMessage = String.format("Failed to match editor text with initial text, expected \"%1$s\" but found \"%2$s\"", addedText, editorText);

            Assert.assertTrue(errorMessage, editorText.equals(addedText));
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testUndoingRedoingAddedCharsToEditor()
    {
        String errorMessage = "Failed to create editor";
        try
        {
            String initialText = "(println 12)";
            String addedText = "(+ x 12)";
            String updatedText = initialText + addedText;
            SimpleTextEditor editor = new SimpleTextEditor(initialText, initialText.length());
            for (char c:addedText.toCharArray())
            {
                errorMessage = "Failed to add character";
                editor.applyTextInsertTransaction("" + c);
            }

            errorMessage = "Failed to get editor text";
            String editorText = editor.getText();

            Assert.assertTrue(errorMessage, editorText != null);

            errorMessage = String.format("Failed to match editor text with initial text, expected \"%1$s\" but found \"%2$s\"", addedText, editorText);

            Assert.assertTrue(errorMessage, editorText.equals(updatedText));

            for (char c:addedText.toCharArray())
            {
                errorMessage = "Failed to undo character";
                editor.applyUndoTransaction();
            }

            String restoredText = editor.getText();

            errorMessage = "Failed to get text after undo";

            Assert.assertTrue(errorMessage, restoredText != null);

            errorMessage = String.format("Failed to undo added characters, expected \"%1$s\" but found \"%2$s\"", restoredText, initialText);

            Assert.assertTrue(errorMessage, restoredText.equals(initialText));

            errorMessage = "Failed to redo undid changes";
            while (editor.redoUndidTransaction())
            {

            }

            errorMessage = "Failed to get redid changes";

            editorText = editor.getText();
            Assert.assertTrue(errorMessage, editorText!=null);
            errorMessage = String.format("Failed to get redid changes, expected \"%1$s\" but found \"%2$s\"",updatedText, editorText);
            Assert.assertTrue(errorMessage, editorText.equals(updatedText));

        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testUndoingDeletedCharsToEditor()
    {
        String errorMessage = "Failed to create editor";
        try
        {
            String initialText = "(println 12)";
            String addedText = "(+ x 12)";
            String updatedText = initialText + addedText;
            SimpleTextEditor editor = new SimpleTextEditor(initialText, initialText.length());
            for (char c:addedText.toCharArray())
            {
                errorMessage = "Failed to add character";
                editor.applyTextInsertTransaction("" + c);
            }

            errorMessage = "Failed to get editor text";
            String editorText = editor.getText();

            Assert.assertTrue(errorMessage, editorText != null);

            errorMessage = String.format("Failed to match editor text with initial text, expected \"%1$s\" but found \"%2$s\"", addedText, editorText);

            Assert.assertTrue(errorMessage, editorText.equals(updatedText));

            for (char c:addedText.toCharArray())
            {
                errorMessage = "Failed to apply backspace";
                editor.applyBackspaceTransaction(1);
            }

            String textAfterDelete = editor.getText();

            errorMessage = "Failed to get text after backspace";

            Assert.assertTrue(errorMessage, textAfterDelete != null);

            errorMessage = String.format("Failed to delete added characters, expected \"%1$s\" but found \"%2$s\"", initialText, textAfterDelete);

            Assert.assertTrue(errorMessage, textAfterDelete.equals(initialText));

            errorMessage = "Failed to undo deleted changes";
            for (char c:addedText.toCharArray())
            {

                editor.applyUndoTransaction();
            }

            errorMessage = "Failed to get undid deletions";

            editorText = editor.getText();
            Assert.assertTrue(errorMessage, editorText!=null);
            errorMessage = String.format("Failed to get undo changes, expected \"%1$s\" but found \"%2$s\"",updatedText, editorText);
            Assert.assertTrue(errorMessage, editorText.equals(updatedText));

        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testCreateSearchIndex()
    {
        String errorMessage = "Failed to create TextSearchIndex";
        try
        {
            String input = "(defun heapify-up (i)\n(if (<= i 1)\n(return F))\n(setq parent-index (integer (/ i 2)))\n(setq parent (nth weight-heap parent-index))\n(setq v (nth weight-heap i))\n(if (< v parent)\n(progn\n(set-nth weight-heap i parent)\n(set-nth weight-heap parent-index v)\n(heapify-up parent-index))))";
            TextSearchIndex searchIndex = new TextSearchIndex(input);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testTextSearch()
    {
        String errorMessage = "Failed to create TextSearchIndex";
        try
        {
            String input = "(defun heapify-up (i)\n(if (<= i 1)\n(return F))\n(setq parent-index (integer (/ i 2)))\n(setq parent (nth weight-heap parent-index))\n(setq v (nth weight-heap i))\n(if (< v parent)\n(progn\n(set-nth weight-heap i parent)\n(set-nth weight-heap parent-index v)\n(heapify-up parent-index))))";
            int expectedNumResults = 3;
            String searchText = "setq";

            TextSearchIndex searchIndex = new TextSearchIndex(input);

            errorMessage = "Failed to execute search";
            LinkedList<TextSearchIndex.IndexItem> results = searchIndex.findString(searchText);
            errorMessage = "Failed to get search results";
            Assert.assertTrue(errorMessage, results != null || results.size() == 0);


            errorMessage = "Wrong number of results: expected: [" + expectedNumResults + "] but found [" + results.size() + "]";
            Assert.assertTrue(errorMessage, results.size() == expectedNumResults);


            if (results == null)
            {
                for (TextSearchIndex.IndexItem result:results)
                {
                    String searchResult = input.substring(result.getPosition(), result.getPosition() + searchText.length());
                    errorMessage = "Found wrong item: expected [" + searchText + " but found: [" + searchResult + "]";
                    System.out.println("Found " + searchText + " on line: " + result.getLineNumber());
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
    public void testSearchIterator()
    {
        String errorMessage = "Failure creating search index";

        try
        {
            String input = "(defun heapify-up (i)\n(if (<= i 1)\n(return F))\n(setq parent-index (integer (/ i 2)))\n(setq parent (nth weight-heap parent-index))\n(setq v (nth weight-heap i))\n(if (< v parent)\n(progn\n(set-nth weight-heap i parent)\n(set-nth weight-heap parent-index v)\n(heapify-up parent-index))))";

            String searchText = "setq";

            TextSearchIndex searchIndex = new TextSearchIndex(input);

            Iterator<TextSearchResult> results = searchIndex.getSearchIterator(searchText);

            while (results.hasNext())
            {
                TextSearchResult result = results.next();
                System.out.println("<><><><><><><><><><><><>");
                System.out.println("Line: " + result.getLineNumber() + " context:" + result.getContextualText(5, 5));
            }


        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }
}
