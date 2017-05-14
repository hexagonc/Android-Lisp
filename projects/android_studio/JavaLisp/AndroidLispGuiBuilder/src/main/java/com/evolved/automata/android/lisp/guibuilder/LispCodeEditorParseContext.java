package com.evolved.automata.android.lisp.guibuilder;

import com.evolved.automata.lisp.editor.ParseContext;
import com.evolved.automata.lisp.editor.ParseNode;

import java.util.HashMap;
import java.util.HashSet;

/**
 * Created by Evolved8 on 4/25/17.
 */

public class LispCodeEditorParseContext implements ParseContext {


    HashMap<String, Integer> mVariableReferences;
    HashMap<String, Integer> mFunctionReferences;


    OnFunctionAppendListener mFunctionAppendListener;
    OnVariableAppendListener mVariableAppendListener;

    HashSet<ParseNode> mErrorNodes;
    HashSet<ParseNode> mIncompleteNodes;

    public LispCodeEditorParseContext()
    {
        mFunctionReferences = new HashMap<String, Integer>();
        mVariableReferences = new HashMap<String, Integer>();

        mErrorNodes = new HashSet<ParseNode>();
        mIncompleteNodes = new HashSet<ParseNode>();
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


    @Override
    public ParseContext setErrorNode(ParseNode errorNode)
    {
        mErrorNodes.add(errorNode);
        return this;
    }

    @Override
    public ParseContext removeErrorNode(ParseNode errorNode)
    {
        mErrorNodes.remove(errorNode);
        return this;
    }

    @Override
    public HashSet<ParseNode> getIncompleteNodes()
    {
        return mIncompleteNodes;
    }

    @Override
    public ParseContext addIncompleteNode(ParseNode node)
    {
        mIncompleteNodes.add(node);
        return this;
    }

    @Override
    public boolean hasIncompleteNodes()
    {
        return mIncompleteNodes.size() > 0;
    }

    @Override
    public ParseContext removeIncompleteNode(ParseNode node)
    {
        mIncompleteNodes.remove(node);
        return this;
    }


    @Override
    public HashSet<ParseNode> getErrorNodes()
    {
        return mErrorNodes;
    }

    @Override
    public boolean hasErrors()
    {
        return mErrorNodes.size() > 0;
    }


}
