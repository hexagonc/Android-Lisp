package com.evolved.automata.lisp.editor;

/**
 * Created by Evolved8 on 8/26/17.
 */
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;


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
}
