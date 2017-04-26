package com.evolved.automata.lisp.editor;

/**
 * Created by Evolved8 on 4/23/17.
 */

public class VarNameNode extends AtomNode {

    boolean startsWithHyphenP = false;
    boolean previousHyphenP = false;
    public VarNameNode(ParseNode parent)
    {
        super(parent, TYPE.VAR_NAME);
    }


    @Override
    public boolean possibleFirstCharP(char firstChar)
    {
        return !Character.isDigit(firstChar) &&
                !Character.isWhitespace(firstChar) &&
                firstChar != '(' &&
                firstChar != '\"' &&
                firstChar != ')' &&
                firstChar != '\'' &&
                firstChar != ';';

    }


    boolean isBoundaryCharacter(char value)
    {
        return Character.isWhitespace(value) || value == ')' || value == ';';
    }

    @Override
    public ParseStatus appendChar(char value)
    {
        if (mStatus == ParseStatus.INITIAL)
        {
            if (possibleFirstCharP(value))
            {
                startsWithHyphenP = previousHyphenP = value == '-';
                mValue.append(value);
                setStatus(ParseStatus.FINISHED);
            }
        }
        else if (mStatus == ParseStatus.FINISHED &&
                isBoundaryCharacter(value))
        {
            setStatus(ParseStatus.COMPLETE_BOUNDARY);
        }
        else if ( mStatus == ParseStatus.FINISHED && value == '\'')
        {

            mValue.append(value);
            setStatus(ParseStatus.COMPLETE_ABSORB);

        }
        else if (mStatus == ParseStatus.FINISHED && (Character.isDigit(value) || value == '.') && startsWithHyphenP && previousHyphenP)
        {
            setStatus(ParseStatus.ERROR);
        }
        else if (mStatus == ParseStatus.FINISHED &&
                value != '\"' &&
                value != ')')
        {
            previousHyphenP = false;
            mValue.append(value);
            setStatus(ParseStatus.FINISHED);
        }
        else
        {
            setStatus(ParseStatus.ERROR);
        }

        return mStatus;
    }
}
