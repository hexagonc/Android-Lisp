package com.evolved.automata.android.lisp.guibuilder;

import org.junit.runners.ParentRunner;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 4/23/17.
 */

public class ParseContext {



    public interface OnVariableAppendListener
    {
        public void onAppend(StringBuilder currentVariableName, char nextCharacter);
    }

    public interface OnFunctionAppendListener
    {
        public void onAppend(StringBuilder currentFunctionName, char nextCharacter);
    }

    HashMap<String, Integer> mVariableReferences;
    HashMap<String, Integer> mFunctionReferences;
    HashMap<String, HashSet<ParseNode>> mIncompleteQuoteReferences;
    HashMap<String, HashSet<ParseNode>> mIncompleteLeftParenReferences;
    HashMap<String, HashSet<ParseNode>> mIncompleteRightParenReferences;

    OnFunctionAppendListener mFunctionAppendListener;
    OnVariableAppendListener mVariableAppendListener;

    public ParseContext()
    {
        mFunctionReferences = new HashMap<String, Integer>();
        mVariableReferences = new HashMap<String, Integer>();
        mIncompleteQuoteReferences = new HashMap<String, HashSet<ParseNode>>();
        mIncompleteLeftParenReferences = new HashMap<String, HashSet<ParseNode>>();
        mIncompleteRightParenReferences = new HashMap<String, HashSet<ParseNode>>();
    }


    /**
     * This is mostly to facilitate suggested completions.  This is called by a
     * ParseNode as a variable is being created
     * @param wholeName
     * @param nextCharacter
     * @return
     */
    public ParseContext onVariableCharacterAppend(StringBuilder wholeName, char nextCharacter)
    {
        if (mVariableAppendListener != null)
            mVariableAppendListener.onAppend(wholeName, nextCharacter);
        return this;
    }

    /**
     * This is mostly to facilitate suggested completions.  This is called by a
     * ParseNode as a function name is being created
     * @param wholeName
     * @param nextCharacter
     * @return
     */
    public ParseContext onFunctionCharacterAppend(StringBuilder wholeName, char nextCharacter)
    {
        if (mFunctionAppendListener != null)
            mFunctionAppendListener.onAppend(wholeName, nextCharacter);
        return this;
    }

    /**
     * Called by the Parse
     * @param name
     * @return
     */
    public ParseContext onVariableNameComplete(String name)
    {
        Integer priorCount = mVariableReferences.get(name);
        if (priorCount == null)
        {
            priorCount = Integer.valueOf(0);

        }
        mVariableReferences.put(name, Integer.valueOf(priorCount + 1));
        return this;
    }

    public ParseContext onFunctionNameComplete(String name)
    {
        Integer priorCount = mFunctionReferences.get(name);
        if (priorCount == null)
        {
            priorCount = Integer.valueOf(0);

        }
        mFunctionReferences.put(name, Integer.valueOf(priorCount + 1));
        return this;
    }

}
