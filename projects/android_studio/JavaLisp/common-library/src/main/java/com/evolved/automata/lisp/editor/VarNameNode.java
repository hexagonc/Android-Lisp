package com.evolved.automata.lisp.editor;

import java.util.HashMap;

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

    public VarNameNode()
    {
        super(TYPE.VAR_NAME);
    }

    @Override
    protected void fill(String[] fields, HashMap<Integer, ParseNode> inverseMap)
    {
        super.fill(fields, inverseMap);
        startsWithHyphenP = toBoolean(fields[BASE_EXTRA_DATA_INDEX]);
        previousHyphenP = toBoolean(fields[BASE_EXTRA_DATA_INDEX+1]);
    }

    @Override
    protected String serialize(HashMap<ParseNode, Integer> nodeIndex)
    {
        StringBuilder baseSerialize = new StringBuilder(super.serialize(nodeIndex));
        baseSerialize.append(STANDARD_FIELD_SEPARATOR).append(serialize(startsWithHyphenP))
                .append(STANDARD_FIELD_SEPARATOR).append(serialize(previousHyphenP));

        return baseSerialize.toString();

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
