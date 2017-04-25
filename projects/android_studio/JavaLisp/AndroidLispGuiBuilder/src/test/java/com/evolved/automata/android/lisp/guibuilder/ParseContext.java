package com.evolved.automata.android.lisp.guibuilder;

import org.junit.runners.ParentRunner;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 4/23/17.
 */

public interface ParseContext {



    public interface OnVariableAppendListener
    {
        public void onAppend(StringBuilder currentVariableName, char nextCharacter);
    }

    public interface OnFunctionAppendListener
    {
        public void onAppend(StringBuilder currentFunctionName, char nextCharacter);
    }



    /**
     * This is mostly to facilitate suggested completions.  This is called by a
     * ParseNode as a variable is being created
     * @param wholeName
     * @param nextCharacter
     * @return
     */
    public ParseContext onVariableCharacterAppend(StringBuilder wholeName, char nextCharacter);


    /**
     * This is mostly to facilitate suggested completions.  This is called by a
     * ParseNode as a function name is being created
     * @param wholeName
     * @param nextCharacter
     * @return
     */
    public ParseContext onFunctionCharacterAppend(StringBuilder wholeName, char nextCharacter);

    /**
     * Called by the Parse
     * @param name
     * @return
     */
    public ParseContext onVariableNameComplete(String name);


    public ParseContext onFunctionNameComplete(String name);

    ParseContext setErrorNode(ParseNode errorNode);

    ParseContext removeErrorNode(ParseNode errorNode);


    HashSet<ParseNode> getErrorNodes();

    boolean hasErrors();

    HashSet<ParseNode> getIncompleteNodes();

    ParseContext addIncompleteNode(ParseNode node);

    boolean hasIncompleteNodes();



    ParseContext removeIncompleteNode(ParseNode node);


}
