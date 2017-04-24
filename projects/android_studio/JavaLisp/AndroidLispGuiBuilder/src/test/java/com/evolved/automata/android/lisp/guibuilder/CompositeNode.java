package com.evolved.automata.android.lisp.guibuilder;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashSet;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 4/23/17.
 */

public abstract class CompositeNode extends ParseNode {



    public enum State
    {
        NO_CHILDREN, TESTING_POSSIBLE_CHILDREN, BUILDING_CHILD, FINDING_POSSIBLE_CHILDREN
    }


    State mAppendState = State.NO_CHILDREN;

    static LinkedList<Class<? extends ParseNode>> mPossibleChildTypes;

    static {
        mPossibleChildTypes = new LinkedList<Class<? extends ParseNode>>();
        mPossibleChildTypes.add(WhiteSpaceNode.class);
        mPossibleChildTypes.add(NumberNode.class);
        mPossibleChildTypes.add(StringNode.class);
        mPossibleChildTypes.add(VarNameNode.class);


    }

    public CompositeNode(ParseNode parent, ParseNode.TYPE type)
    {
        super(parent, type);

    }



    protected HashSet<ParseNode> getPossibleChildren(char firstChar)
    {
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
        for (ParseNode node:mPossibleNextChild)
        {
            status = node.appendChar(value);
            if (status.isProgressP())
                newPossible.add(node);
        }
        return newPossible;
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
        boolean firstrunner= true;
        if (mLastChildLink != null)
        {
            if (mLastChildLink.node.getStatus() == ParseStatus.BUILDING ||
                    mLastChildLink.node.getStatus() == ParseStatus.FINISHED)
            {
                ParseStatus status = mLastChildLink.node.appendChar(value);
                firstrunner = false;
                if (status.reprocessInputP())
                {
                    mPossibleNextChild = null;
                    mStatus = status;
                }
                else if (status == ParseStatus.COMPLETE_ABSORB)
                {
                    mPossibleNextChild = null;
                    mStatus = ParseStatus.FINISHED;
                    return mStatus;
                }
                else
                {
                    mStatus = status;
                    return mStatus;
                }
            }

        }

        HashSet<ParseNode> newPossible;
        if (mPossibleNextChild != null && mPossibleNextChild.size()>0)
        {
            newPossible = getUpdatedPossibleChildren(mPossibleNextChild, value);
        }
        else
        {
            newPossible = getPossibleChildren(value);
        }

        mPossibleNextChild = newPossible;
        if (newPossible.size() == 1)
        {
            return mStatus = finalizePossibleChildren(value, firstrunner);

        }else if (newPossible.size() > 1)
        {
            mStatus = ParseStatus.BUILDING;
        }
        else
        { // this shouldn't happen
            assert false;
            mPossibleNextChild = null;
            mStatus = ParseStatus.IN_COMPLETE;
        }

        return mStatus;
    }

    private ParseStatus finalizePossibleChildren(char value, boolean first)
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


        if (newLink.node.getStatus() == ParseStatus.COMPLETE_BOUNDARY && first) // this probably never happens
        {

            return appendCharRoundTwo(value);
        }
        else
            return newLink.node.getStatus();
    }


    private ParseStatus appendCharRoundTwo(char value)
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
            mStatus = ParseStatus.IN_COMPLETE;
        }
        return mStatus;
    }

}
