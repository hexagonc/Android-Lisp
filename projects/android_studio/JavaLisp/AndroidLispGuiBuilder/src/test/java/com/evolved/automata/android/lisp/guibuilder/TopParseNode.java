package com.evolved.automata.android.lisp.guibuilder;

import java.util.HashSet;

/**
 * Created by Evolved8 on 4/23/17.
 */

public class TopParseNode extends CompositeNode {

    public TopParseNode()
    {
        super(null, TYPE.TOP);
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


    public ParseStatus appendCharOld(char value)
    {
        HashSet<ParseNode> newPossible;
        if (mLastChildLink == null)
        {

            if (mPossibleNextChild != null && mPossibleNextChild.size()>0)
            {
                newPossible = getUpdatedPossibleChildren(mPossibleNextChild, value);
            }
            else
            {
                newPossible = getPossibleChildren(value);
            }

            if (newPossible.size() == 1)
            {
                Link newLink = new Link(newPossible.iterator().next());
                if (newLink.node.getStatus() != ParseStatus.IN_COMPLETE)
                {
                    mLastChildLink = mChildLinks = newLink;
                    return mStatus = newLink.node.getStatus();
                }

            }else if (newPossible.size() > 1)
            {
                mPossibleNextChild = newPossible;
                mStatus = ParseStatus.BUILDING;
            }
            else
            {
                mPossibleNextChild = null;
                mStatus = ParseStatus.IN_COMPLETE;
            }
        }
        else
        {
            ParseStatus status = mLastChildLink.node.appendChar(value);
            if (status != ParseStatus.IN_COMPLETE)
            {
                return mStatus = status;
            }
            else
                mStatus = ParseStatus.IN_COMPLETE;
        }
        return mStatus;
    }





    @Override
    public String getValue()
    {
        StringBuilder builder = new StringBuilder();
        Link link = mChildLinks;
        while (link != null)
        {
            builder.append(link.node.getValue());
            link = link.nextChild;
        }
        return builder.toString();
    }






}
