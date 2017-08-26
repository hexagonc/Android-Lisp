package com.evolved.automata.lisp.editor;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

/**
 * Created by Evolved8 on 4/23/17.
 */

public class TopParseNode extends CompositeNode {

    public TopParseNode()
    {
        super(null, TYPE.TOP);
        acceptWorkingChildStatusP = true;
    }

    public boolean possibleFirstCharP(char firstChar)
    {
        return true;
    }

    @Override
    public int getChildOffset()
    {
        return 0;
    }


    @Override
    protected void fill(String[] fields, HashMap<Integer, ParseNode> inverseMap)
    {
        super.fill(fields, inverseMap);
        parseNumWorkingChildren(fields[BASE_EXTRA_DATA_INDEX]);

        parseAcceptWorkingChildStatus(fields[BASE_EXTRA_DATA_INDEX + 1]);
        parsePossibleNextChild(fields[BASE_EXTRA_DATA_INDEX + 2], inverseMap);
    }

    protected String serializeNumWorkingChildren()
    {
        return "" + mNumWorkingChildren;
    }


    protected String serializeAcceptWorkingChildStatus()
    {
        return serialize(acceptWorkingChildStatusP);
    }

    protected String serializePossibleNextChild(HashMap<ParseNode, Integer> nodeIndex)
    {
        StringBuilder builder = new StringBuilder();
        if (mPossibleNextChild != null)
        {
            for (ParseNode p:mPossibleNextChild)
            {
                if (builder.length() > 0)
                    builder.append(CHILD_NODE_SEPARATOR);
                builder.append(nodeIndex.get(p));
            }
            return builder.toString();
        }
        else
            return "";
    }

    protected ArrayList<String> serializeFields(HashMap<ParseNode, Integer> nodeIndex)
    {

        ArrayList<String> list = super.serializeFields(nodeIndex);
        list.add(serializeNumWorkingChildren());
        list.add(serializeAcceptWorkingChildStatus());
        list.add(serializePossibleNextChild(nodeIndex));
        return list;
    }


    protected void parseNumWorkingChildren(String value)
    {
        mNumWorkingChildren = Integer.parseInt(value);
    }


    protected void parseAcceptWorkingChildStatus(String value)
    {
        acceptWorkingChildStatusP = toBoolean(value);
    }

    protected void parsePossibleNextChild(String value, HashMap< Integer, ParseNode> inverseIndex)
    {
        if (mPossibleNextChild == null)
            mPossibleNextChild = new HashSet<>();
        else
            mPossibleNextChild.clear();
        if (value.length() > 0)
        {
            for (String possibleChildId: StringUtils.splitByWholeSeparatorPreserveAllTokens(value, CHILD_NODE_SEPARATOR))
            {
                Integer index = Integer.valueOf(Integer.parseInt(possibleChildId));
                ParseNode possibleChild = inverseIndex.get(index);
                possibleChild.setParent(this);
                mPossibleNextChild.add(possibleChild);
            }
        }
    }




}
