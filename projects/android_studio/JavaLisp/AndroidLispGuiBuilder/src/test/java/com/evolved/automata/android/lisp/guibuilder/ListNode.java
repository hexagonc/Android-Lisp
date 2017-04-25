package com.evolved.automata.android.lisp.guibuilder;

/**
 * Created by Evolved8 on 4/24/17.
 */

public class ListNode extends CompositeNode {

    public ListNode(ParseNode parent)
    {
        super(parent, TYPE.LIST);
        acceptWorkingChildP = false;
    }

    @Override
    public boolean possibleFirstCharP(char firstChar)
    {
        return firstChar == '(';
    }

    @Override
    public int getChildOffset()
    {
        return 1;
    }

    @Override
    public String getValue()
    {
        switch (mStatus)
        {
            case INITIAL:
                return "";
            case BUILDING:
            case IN_COMPLETE:
                return "(" + super.getValue();
            case COMPLETE_ABSORB:
                return "(" + super.getValue() + ")";

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
                return mStatus = ParseStatus.BUILDING;
            else
                return mStatus = ParseStatus.IN_COMPLETE;
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
                                if (updatedStatus == ParseStatus.COMPLETE_ABSORB)
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
            return ParseStatus.IN_COMPLETE;

    }

    @Override
    public int getLength()
    {
        switch (mStatus)
        {
            case INITIAL:
                return 0;
            case BUILDING:
            case IN_COMPLETE:
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
