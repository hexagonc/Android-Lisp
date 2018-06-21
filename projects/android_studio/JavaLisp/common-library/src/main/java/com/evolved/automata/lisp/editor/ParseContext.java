package com.evolved.automata.lisp.editor;



import java.util.HashSet;

/**
 * Created by Evolved8 on 4/23/17.
 */

public interface ParseContext {



     interface OnVariableAppendListener
    {
         void onAppend(StringBuilder currentVariableName, char nextCharacter);
    }

     interface OnFunctionAppendListener
    {
         void onAppend(StringBuilder currentFunctionName, char nextCharacter);
    }



    /**
     * This is mostly to facilitate suggested completions.  This is called by a
     * ParseNode as a variable is being created
     * @param wholeName
     * @param nextCharacter
     * @return
     */
     ParseContext onVariableCharacterAppend(StringBuilder wholeName, char nextCharacter);


    /**
     * This is mostly to facilitate suggested completions.  This is called by a
     * ParseNode as a function name is being created
     * @param wholeName
     * @param nextCharacter
     * @return
     */
     ParseContext onFunctionCharacterAppend(StringBuilder wholeName, char nextCharacter);

    /**
     * Called by the Parse
     * @param name
     * @return
     */
     ParseContext onVariableNameComplete(String name);


     ParseContext onFunctionNameComplete(String name);

    ParseContext setErrorNode(ParseNode errorNode);

    ParseContext removeErrorNode(ParseNode errorNode);


    HashSet<ParseNode> getErrorNodes();

    boolean hasErrors();



    HashSet<ParseNode> getIncompleteNodes();

    ParseContext addIncompleteNode(ParseNode node);

    boolean hasIncompleteNodes();



    ParseContext removeIncompleteNode(ParseNode node);


}
