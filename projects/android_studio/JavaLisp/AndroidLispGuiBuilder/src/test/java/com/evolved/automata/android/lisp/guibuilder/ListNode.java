package com.evolved.automata.android.lisp.guibuilder;

/**
 * Created by Evolved8 on 4/24/17.
 */

public class ListNode extends CompositeNode {


    String prefix = "";
    public ListNode(ParseNode parent)
    {
        super(parent, TYPE.LIST);
        acceptWorkingChildP = false;
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

        assert false;
        return null;
    }

    @Override
    public ParseStatus appendChar(char value)
    {
        if (mStatus == ParseStatus.INITIAL)
        {
            if (value == '(')
            {
                prefix= prefix + '(';
                return mStatus = ParseStatus.BUILDING;
            }
            else if ((value == ',' || value == '`' || value == '@'))
            {
                prefix+=value;
                return mStatus;
            }
            else
                return mStatus = ParseStatus.ERROR;
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

                mStatus = ParseStatus.COMPLETE_ABSORB;
                return mStatus;
            }
            else
                return super.appendChar(value);
        }
        else if (mStatus == ParseStatus.COMPLETE_ABSORB)
        { // this shouldn't happen.  You wouldn't try to append to a list that is already complete
            assert false;
            return ParseStatus.COMPLETE_BOUNDARY;
        }
        else
            return ParseStatus.ERROR;

    }

    @Override
    public int getLength()
    {
        switch (mStatus)
        {
            case INITIAL:
                return 0;
            case BUILDING:
            case ERROR:
                return 1 + super.getLength();
            case COMPLETE_ABSORB:
                return 2 + super.getLength();

        }
        assert false;
        return 0;
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
            if (mChildLinks != null) // there will never be both a non-null mChildLinks and mValue.length > 0
            {
                return mChildLinks.node.findNode(absPosition);
            }
            else
                return this;
        }
    }

}
