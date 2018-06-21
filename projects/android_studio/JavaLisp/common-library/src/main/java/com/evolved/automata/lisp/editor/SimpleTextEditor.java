package com.evolved.automata.lisp.editor;

import java.util.Deque;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 8/26/17.
 */

public class SimpleTextEditor {
    int mCursorPosition = 0;
    String mText = "";

    LinkedList<EditorTransaction> mHistory = new LinkedList<EditorTransaction>();

    LinkedList<EditorTransaction> redoList = new LinkedList<EditorTransaction>();

    public static final int DEFAULT_HISTORY_LENGTH = 20;
    int mHistoryLength = DEFAULT_HISTORY_LENGTH;

    public SimpleTextEditor(String text, int cursor)
    {
        this(text, cursor, DEFAULT_HISTORY_LENGTH);
    }

    public SimpleTextEditor(String text)
    {
        this(text, 0, DEFAULT_HISTORY_LENGTH);
    }

    public SimpleTextEditor(String text, int cursor, int historyLength)
    {
        mCursorPosition = cursor;
        mText = text;
        mHistoryLength = historyLength;
    }


    public SimpleTextEditor resetText(String text, int cursor)
    {
        mCursorPosition = cursor;
        mText = text;
        return this;
    }

    public SimpleTextEditor resetText(String text)
    {
        return resetText(text, 0);
    }


    public SimpleTextEditor updateTextOnly(String text)
    {
        mText = text;
        return this;
    }

    public SimpleTextEditor applyTransaction(EditorTransaction trans, boolean updateHistory)
    {
        int initialCursorPos = (trans.getInitialCursorPos()>=0?trans.getInitialCursorPos():mCursorPosition);
        int insertCursorPos = (trans.getInsertCursorPos()>=0?trans.getInsertCursorPos():initialCursorPos);
        mCursorPosition = (trans.getFinalCursorPos()>=0?trans.getFinalCursorPos():insertCursorPos + trans.getCharactersToInsert().length());
        String afterDelete = mText.substring(0, initialCursorPos) + mText.substring(initialCursorPos + trans.getCharactersToDelete().length());
        mText = afterDelete.substring(0, insertCursorPos) + trans.getCharactersToInsert() + afterDelete.substring(insertCursorPos);

        if (updateHistory)
        {
            mHistory.addLast(trans);
            while (mHistory.size() > mHistoryLength)
            {
                mHistory.removeFirst();
            }
        }

        return this;
    }

    public String getText()
    {
        return mText;
    }

    public int getCursorPosition()
    {
        return mCursorPosition;
    }

    public EditorTransaction getUndoTransaction(EditorTransaction trans)
    {
        return new EditorTransaction(trans.getInsertCursorPos(), trans.getCharactersToInsert(), trans.getInitialCursorPos(), trans.getCharactersToDelete(), -1);
    }

    public boolean applyUndoTransaction()
    {
        if (mHistory.size() > 0)
        {
            EditorTransaction last =  mHistory.pollLast();
            redoList.addFirst(last);
            EditorTransaction undoTrans = getUndoTransaction(last);
            applyTransaction(undoTrans, false);
            return true;
        }
        return false;
    }

    public boolean redoUndidTransaction()
    {
        if (redoList.size()>0)
        {
            EditorTransaction trans = redoList.removeFirst();
            applyTransaction(trans, true);
            return true;
        }
        return false;
    }

    public EditorTransaction getTextInsertTransaction(String textToInsert, int position)
    {
        return new EditorTransaction(mCursorPosition, "", position,textToInsert, mCursorPosition + textToInsert.length());
    }

    public SimpleTextEditor applyTextInsertTransaction(String textToInsert, int position)
    {
        EditorTransaction trans = getTextInsertTransaction(textToInsert,position );
        redoList.clear();
        return applyTransaction(trans, true);
    }

    public SimpleTextEditor applyTextInsertTransaction(String textToInsert)
    {
        EditorTransaction trans = getTextInsertTransaction(textToInsert );
        redoList.clear();
        return applyTransaction(trans, true);
    }

    public EditorTransaction getTextInsertTransaction(String textToInsert)
    {
        return getTextInsertTransaction(textToInsert, mCursorPosition);
    }

    public EditorTransaction getTextDeleteTransaction(String textToDelete, int cursor)
    {
        return new EditorTransaction(cursor, textToDelete, cursor, "", cursor);
    }

    public SimpleTextEditor applyTextDeleteTransaction(String textToDelete, int cursor)
    {
        EditorTransaction trans = getTextDeleteTransaction(textToDelete, cursor );
        redoList.clear();
        return applyTransaction(trans, true);
    }

    public EditorTransaction getTextDeleteTransaction(String textToDelete)
    {
        return getTextDeleteTransaction(textToDelete, mCursorPosition);
    }

    public SimpleTextEditor applyTextDeleteTransaction(String textToDelete)
    {
        return applyTextDeleteTransaction(textToDelete, mCursorPosition);
    }

    public EditorTransaction getMoveCursorTransaction(int newPosition)
    {
        return new EditorTransaction(mCursorPosition, "", mCursorPosition, "", newPosition);
    }

    public SimpleTextEditor applyMoveCursorTransaction(int newPosition)
    {
        EditorTransaction trans = getMoveCursorTransaction(newPosition);
        redoList.clear();
        return applyTransaction(trans, true);
    }

    public EditorTransaction getBackspaceTransaction(int count)
    {
        int deletePos = Math.max(0, mCursorPosition - count);
        return new EditorTransaction(deletePos , mText.substring(deletePos, mCursorPosition), deletePos, "", deletePos);
    }

    public SimpleTextEditor applyBackspaceTransaction(int count)
    {
        EditorTransaction trans = getBackspaceTransaction(count);
        redoList.clear();
        return applyTransaction(trans, true);
    }

    public EditorTransaction getReplaceAllTextTransaction(String newText)
    {
        return new EditorTransaction(0, mText, 0, newText, newText.length());
    }

    public SimpleTextEditor applyReplaceAllTextTransaction(String newText)
    {
        EditorTransaction trans = getReplaceAllTextTransaction(newText);
        redoList.clear();
        return applyTransaction(trans, true);
    }
}
