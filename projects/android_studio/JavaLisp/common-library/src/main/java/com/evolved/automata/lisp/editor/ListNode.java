package com.evolved.automata.lisp.editor;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

/**
 * Created by Evolved8 on 4/24/17.
 */

public class ListNode extends CompositeNode {




    String prefix = "";
    public ListNode(ParseNode parent)
    {
        super(parent, TYPE.LIST);
        acceptWorkingChildStatusP = false;
    }

    @Override
    protected void fill(String[] fields, HashMap<Integer, ParseNode> inverseMap)
    {
        super.fill(fields, inverseMap);
        parseNumWorkingChildren(fields[BASE_EXTRA_DATA_INDEX]);
        parsePrefix(fields[BASE_EXTRA_DATA_INDEX + 1]);
        parseAcceptWorkingChildStatus(fields[BASE_EXTRA_DATA_INDEX + 2]);
        parsePossibleNextChild(fields[BASE_EXTRA_DATA_INDEX + 3], inverseMap);
    }

    protected String serializeNumWorkingChildren()
    {
        return "" + mNumWorkingChildren;
    }

    protected String serializePrefix()
    {
        return prefix;
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
        }

        return builder.toString();
    }

    protected ArrayList<String> serializeFields(HashMap<ParseNode, Integer> nodeIndex)
    {

        ArrayList<String> list = super.serializeFields(nodeIndex);
        list.add(serializeNumWorkingChildren());
        list.add(serializePrefix());
        list.add(serializeAcceptWorkingChildStatus());
        list.add(serializePossibleNextChild(nodeIndex));
        return list;
    }


    protected void parseNumWorkingChildren(String value)
    {
        mNumWorkingChildren = Integer.parseInt(value);
    }

    protected void parsePrefix(String value)
    {
        prefix = value;
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
            for (String possibleChildId: StringUtils.splitByWholeSeparator(value, CHILD_NODE_SEPARATOR))
            {
                Integer index = Integer.valueOf(Integer.parseInt(possibleChildId));
                ParseNode possibleChild = inverseIndex.get(index);
                possibleChild.setParent(this);
                mPossibleNextChild.add(possibleChild);
            }
        }
    }



    public ListNode()
    {
        super(TYPE.LIST);
        acceptWorkingChildStatusP = false;
    }

    @Override
    public boolean possibleFirstCharP(char firstChar)
    {
        return firstChar == '(' || firstChar == '`' || firstChar == ',';
    }

    @Override
    public int getChildOffset()
    {
        return prefix.length();
    }

    @Override
    public String getValue()
    {
        switch (mStatus)
        {
            case INITIAL:
                return prefix;
            case BUILDING:
            case ERROR:

                return prefix + super.getValue();
            case COMPLETE_ABSORB:
                return prefix + super.getValue() + ")";

        }

        throw new IllegalStateException(mStatus + " is not a valid state for a List");
    }



    @Override
    public ParseStatus appendChar(char value)
    {
        if (mStatus == ParseStatus.INITIAL)
        {
            if (value == '(')
            {
                prefix= prefix + '(';
                return setStatus(ParseStatus.BUILDING);
            }
            else if ((value == ',' || value == '`' || value == '@'))
            {
                prefix+=value;
                return mStatus;
            }
            else
                return setStatus(ParseStatus.ERROR);
        }
        else if (mStatus == ParseStatus.BUILDING)
        {
            ParseStatus lastStatus, updatedStatus = null;
            if (value == ')')
            {
                switch (mNumWorkingChildren)
                {
                    case 1:
                        lastStatus = mLastChildLink.node.getStatus();
                        switch (lastStatus)
                        {
                            case FINISHED:
                            case BUILDING:
                                updatedStatus = mLastChildLink.node.appendChar(value);
                                if (updatedStatus.consumedInputP())
                                    return mStatus;
                                break;
                        }
                        break;
                    default:
                        // this is the case where there are multiple or no possible children.  If there are multiple
                        // then the right parenthesis would be an error so ignore the error at this level and complete
                        // the list as is.  If there are no working children then this is an empty list so we still absorb
                        // the right parenthesis
                }



                return setStatus(ParseStatus.COMPLETE_ABSORB);
            }
            else
                return super.appendChar(value);
        }
        else if (mStatus == ParseStatus.COMPLETE_ABSORB)
        {
            return ParseStatus.COMPLETE_BOUNDARY;
        }
        else
            return setStatus(ParseStatus.ERROR);

    }

    @Override
    public int getLength()
    {
        switch (mStatus)
        {
            case INITIAL:
                return prefix.length();
            case BUILDING:
            case ERROR:
                return prefix.length() + super.getLength();
            case COMPLETE_ABSORB:
                return prefix.length() + super.getLength() + 1;

        }
        // TODO: remove this after testing or replace with assert
        throw new IllegalStateException(mStatus + " is not allowed.");
    }

    /**
     *
     * @param absPosition Character position whose containing node you want
     * @return
     */
    @Override
    public ParseNode findNode(int absPosition)
    {
        int start = getStartIndex();
        int len = getLength();
        if (absPosition == start + len - 1 && mStatus == ParseStatus.COMPLETE_ABSORB) // right parenthesis
        {
            return this;
        }
        else if (start == absPosition) // left parenthesis
        {
            return this;
        }
        else if (start + len <= absPosition) // after this node
        {
            if (getNextSibling() != null)
                return getNextSibling().findNode(absPosition);
            else
                return null;
        }
        else if (absPosition < start) // prior to this node
        {
            return null;
        }
        else
        {
            if (mFirstChildLink != null) // there will never be both a non-null mFirstChildLink and mValue.length > 0
            {
                return mFirstChildLink.node.findNode(absPosition);
            }
            else
                return this;
        }
    }

}
