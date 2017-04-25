package com.evolved.automata.android.lisp.guibuilder;

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
                mStatus = ParseStatus.FINISHED;
            }
        }
        else if (mStatus == ParseStatus.FINISHED &&
                isBoundaryCharacter(value))
        {
            mStatus = ParseStatus.COMPLETE_BOUNDARY;
        }
        else if ( mStatus == ParseStatus.FINISHED && value == '\'')
        {

            mValue.append(value);
            mStatus = ParseStatus.COMPLETE_ABSORB;
        }
        else if (mStatus == ParseStatus.FINISHED && (Character.isDigit(value) || value == '.') && startsWithHyphenP && previousHyphenP)
        {
            mStatus = ParseStatus.ERROR;
        }
        else if (mStatus == ParseStatus.FINISHED &&
                value != '\"' &&
                value != ')')
        {
            previousHyphenP = false;
            mValue.append(value);
            mStatus = ParseStatus.FINISHED;
        }
        else
        {
            mStatus = ParseStatus.ERROR;
        }

        return mStatus;
    }
}
