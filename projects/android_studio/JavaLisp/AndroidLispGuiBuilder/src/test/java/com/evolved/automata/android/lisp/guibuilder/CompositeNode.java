package com.evolved.automata.android.lisp.guibuilder;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashSet;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 4/23/17.
 */

public abstract class CompositeNode extends ParseNode {

    public enum WORKING_CHILD_STATE
    {
        NONE, SINGLE
    }


    int mNumWorkingChildren = 0;
    protected boolean acceptWorkingChildP = false;
    static LinkedList<Class<? extends ParseNode>> mPossibleChildTypes;

    static {
        mPossibleChildTypes = new LinkedList<Class<? extends ParseNode>>();
        mPossibleChildTypes.add(WhiteSpaceNode.class);
        mPossibleChildTypes.add(NumberNode.class);
        mPossibleChildTypes.add(StringNode.class);
        mPossibleChildTypes.add(VarNameNode.class);
        mPossibleChildTypes.add(ListNode.class);
        mPossibleChildTypes.add(CommentNode.class);

    }

    public CompositeNode(ParseNode parent, ParseNode.TYPE type)
    {
        super(parent, type);

    }



    protected HashSet<ParseNode> getPossibleChildren(char firstChar)
    {
        mNumWorkingChildren = 0;
        try
        {
            HashSet<ParseNode> possible = new HashSet<ParseNode>();
            ParseNode node;
            ParseStatus status;

            for (Class<? extends ParseNode> nodeClass: mPossibleChildTypes)
            {
                Constructor<? extends ParseNode> constructor = nodeClass.getConstructor(ParseNode.class);
                node = constructor.newInstance(this);
                status = node.appendChar(firstChar);
                if (status.isProgressP())
                {
                    mNumWorkingChildren++;
                    possible.add(node);
                }

            }
            return possible;
        }
        catch (InstantiationException e)
        {
            throw new RuntimeException(e); // this should never happen
        }
        catch (InvocationTargetException e)
        {
            throw new RuntimeException(e); // this should never happen
        }
        catch (IllegalAccessException e)
        {
            throw new RuntimeException(e); // this should never happen
        }
        catch (NoSuchMethodException e)
        {
            throw new RuntimeException(e); // this should never happen
        }

    }

    protected HashSet<ParseNode> getUpdatedPossibleChildren(HashSet<ParseNode> possible, char value)
    {
        HashSet<ParseNode> newPossible = new HashSet<ParseNode>();
        ParseStatus status;
        mNumWorkingChildren = 0;
        for (ParseNode node:mPossibleNextChild)
        {
            status = node.appendChar(value);
            if (status.isProgressP())
            {
                mNumWorkingChildren++;
                newPossible.add(node);
            }
        }
        return newPossible;
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

    @Override
    public int getLength()
    {
        int total = 0;
        Link child = mChildLinks;
        while (child != null)
        {
            total+=child.node.getLength();
            child = child.nextChild;
        }
        return total;
    }


    public LinkedList<ParseNode> getChildren()
    {
        LinkedList<ParseNode> nodes = new LinkedList<ParseNode>();
        Link child = mChildLinks;
        while (child != null)
        {
            nodes.add(child.node);
            child = child.nextChild;
        }
        return nodes;
    }

    public LinkedList<ParseNode> getTokenChildren()
    {
        LinkedList<ParseNode> nodes = new LinkedList<ParseNode>();
        Link child = mChildLinks;
        while (child != null)
        {
            if (child.node.getType() != TYPE.WHITE_SPACE)
            {
                nodes.add(child.node);
            }
            child = child.nextChild;
        }
        return nodes;
    }

    public ParseStatus appendChar(char value)
    {

        ParseStatus returnStatus = mStatus, status;
        HashSet<ParseNode> newPossible;
        boolean continueProcessingP = false;

        do
        {
            switch (mNumWorkingChildren)
            {
                case 0: // process value as start of new token or child
                    continueProcessingP = false;
                    newPossible = getPossibleChildren(value);
                    mPossibleNextChild = newPossible;
                    switch (newPossible.size())
                    {
                        case 0:
                            returnStatus = mStatus = ParseStatus.ERROR;
                            break;
                        case 1:
                            status = finalizePossibleChildren(value);
                            switch (status)
                            {
                                case COMPLETE_ABSORB:
                                    mNumWorkingChildren = 0;
                                    break;
                                case ERROR:
                                    returnStatus = status;
                                    if (acceptWorkingChildP)
                                        mStatus = returnStatus;
                            }
                            if (acceptWorkingChildP)
                                mStatus = returnStatus = status;
                            break;
                        default:
                            if (acceptWorkingChildP)
                                mStatus = returnStatus = ParseStatus.BUILDING;
                    }
                    break;
                case 1: // process value as continuation of existing token or child
                    status = mLastChildLink.node.appendChar(value);
                    if (acceptWorkingChildP)
                        returnStatus = mStatus = status;
                    switch (status)
                    {
                        case COMPLETE_ABSORB:
                            mNumWorkingChildren = 0;
                            if (acceptWorkingChildP) // override in this case
                                returnStatus = mStatus = ParseStatus.FINISHED;
                            return returnStatus;
                        case COMPLETE_BOUNDARY:
                        case ERROR:
                            mNumWorkingChildren = 0;
                            continueProcessingP = true;
                            break;
                    }
                    break;
                default: // process value as continuation of multiple possible children.
                    continueProcessingP = false;
                    newPossible = getUpdatedPossibleChildren(mPossibleNextChild, value);
                    mPossibleNextChild = newPossible;
                    switch (newPossible.size())
                    {
                        case 0:
                            return mStatus = ParseStatus.ERROR;
                        case 1:
                            status = finalizePossibleChildren(value);
                            if (acceptWorkingChildP)
                                returnStatus = mStatus = status;
                            switch (status)
                            {
                                case COMPLETE_ABSORB:
                                    mNumWorkingChildren = 0;
                                    break;
                                case ERROR:
                                    returnStatus = status;
                                    if (acceptWorkingChildP)
                                        mStatus = returnStatus;
                            }
                            break;
                        default:
                            if (acceptWorkingChildP)
                                mStatus = returnStatus = ParseStatus.BUILDING;

                    }

            }

        }while (continueProcessingP);


        return returnStatus;
    }

    private ParseStatus finalizePossibleChildren(char value)
    {
        Link newLink = new Link(mPossibleNextChild.iterator().next());


        if (mLastChildLink == null)
        {
            mChildLinks = mLastChildLink = newLink;
        }
        else
        {
            mLastChildLink.nextChild = newLink;
            newLink.prevChild = mLastChildLink;
            mLastChildLink = newLink;
        }

        return newLink.node.getStatus();
    }


    /*
    protected ParseStatus appendUnconsummedChar(char value)
    {
        mPossibleNextChild = getPossibleChildren(value);

        if (mPossibleNextChild.size() == 1)
        {

            return finalizePossibleChildren(value, false);

        }else if (mPossibleNextChild.size() > 1)
        {
            mStatus = ParseStatus.BUILDING;
        }
        else
        {
            mPossibleNextChild = null;
            mStatus = ParseStatus.ERROR;
        }
        return mStatus;
    }
    */

}