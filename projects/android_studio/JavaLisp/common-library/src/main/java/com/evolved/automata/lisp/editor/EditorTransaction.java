package com.evolved.automata.lisp.editor;

/**
 * Created by Evolved8 on 8/26/17.
 */

public class EditorTransaction {
    int mInitialCursorPos = 0;
    String mCharactersToDelete;
    int mInsertCursorPosition = 0;
    String mCharactersToInsert;
    int mFinalCursorPosition = 0;

    public EditorTransaction(int initialPosition, String textToDelete, int insertPosition, String textToInsert, int finalCursorPos)
    {
        mInitialCursorPos = initialPosition;
        mCharactersToDelete = textToDelete;
        mInsertCursorPosition = insertPosition;
        mCharactersToInsert = textToInsert;
        mFinalCursorPosition = finalCursorPos;
    }

    public int getInitialCursorPos()
    {
        return mInitialCursorPos;
    }

    public int getInsertCursorPos()
    {
        return mInsertCursorPosition;
    }

    public int getFinalCursorPos()
    {
        return mFinalCursorPosition;
    }

    public String getCharactersToDelete()
    {
        if (mCharactersToDelete == null)
            return "";
        else
            return mCharactersToDelete;
    }

    public String getCharactersToInsert()
    {
        if (mCharactersToInsert == null)
            return "";
        else
            return mCharactersToInsert;
    }



}
