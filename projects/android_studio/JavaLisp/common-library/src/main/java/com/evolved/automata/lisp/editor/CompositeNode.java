package com.evolved.automata.lisp.editor;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashSet;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 4/23/17.
 */

public abstract class CompositeNode extends ParseNode {



    int mNumWorkingChildren = 0;
    protected boolean acceptWorkingChildStatusP = false;
    static LinkedList<Class<? extends ParseNode>> mPossibleChildTypes;
    HashSet<ParseNode> mPossibleNextChild = null;

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

    public CompositeNode reset()
    {
        mNumWorkingChildren = 0;
        mFirstChildLink = null;
        mLastChildLink = null;
        if (mPossibleNextChild != null)
            mPossibleNextChild.clear();
        return this;
    }

    public ParseStatus processAll(String input)
    {
        reset();
        for (char c:input.toCharArray())
        {
            appendChar(c);
        }
        return getStatus();
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

                if (node.possibleFirstCharP(firstChar))
                {
                    status = node.appendChar(firstChar);
                    node.setContext(getParseContext());
                    if (status.isProgressP() || (node.getType() == TYPE.LIST && status == ParseStatus.INITIAL))
                    {
                        mNumWorkingChildren++;
                        possible.add(node);
                    }
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
            if (status.isProgressP() || (node.getType() == TYPE.LIST && status == ParseStatus.INITIAL))
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
        Link link = mFirstChildLink;
        ParseNode node;
        while (link != null)
        {
            node = link.node;
            builder.append(node.getValue());
            link = link.nextChild;
        }
        if (mPossibleNextChild != null && mPossibleNextChild.size() > 0)
        {
            node = mPossibleNextChild.iterator().next();
            builder.append(node.getValue());
        }
        return builder.toString();
    }

    @Override
    public int getLength()
    {
        int total = 0;
        Link child = mFirstChildLink;
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
        Link child = mFirstChildLink;
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
        Link child = mFirstChildLink;
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

        ParseStatus returnStatus = getStatus(), status;
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
                            // invalid character

                            ErrorTextNode errorChild = new ErrorTextNode(this);
                            errorChild.appendChar(value);
                            appendChild(errorChild);
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
                                    if (acceptWorkingChildStatusP)
                                        setStatus(returnStatus);

                            }
                            if (acceptWorkingChildStatusP)
                                setStatus(returnStatus = status);

                            break;
                        default:
                            if (acceptWorkingChildStatusP)
                                setStatus(returnStatus = ParseStatus.BUILDING);

                    }
                    break;
                case 1: // process value as continuation of existing token or child
                    status = mLastChildLink.node.appendChar(value);
                    if (acceptWorkingChildStatusP)
                        setStatus(returnStatus = status);
                    switch (status)
                    {
                        case COMPLETE_ABSORB:
                            mNumWorkingChildren = 0;
                            if (acceptWorkingChildStatusP) // override in this case since, in general, composite codes are never completed (can always append another child)
                                setStatus(returnStatus = ParseStatus.FINISHED);
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
                            ErrorTextNode errorChild = new ErrorTextNode(this);
                            errorChild.appendChar(value);
                            appendChild(errorChild);
                            break;
                        case 1:
                            status = finalizePossibleChildren(value);
                            if (acceptWorkingChildStatusP)
                                setStatus(returnStatus = status);

                            switch (status)
                            {
                                case COMPLETE_ABSORB:
                                    mNumWorkingChildren = 0;
                                    break;
                                case COMPLETE_BOUNDARY:
                                case ERROR:
                                    mNumWorkingChildren = 0;
                                    continueProcessingP = true;
                                    break;

                            }
                            break;
                        default:
                            if (acceptWorkingChildStatusP)
                                setStatus(returnStatus = ParseStatus.BUILDING);


                    }

            }

        }while (continueProcessingP);


        return returnStatus;
    }

    protected ParseStatus finalizePossibleChildren(char value)
    {
        Link newLink = new Link(mPossibleNextChild.iterator().next());

        appendChild(newLink);

        return newLink.node.getStatus();
    }

    protected ParseNode appendChild(Link newLink)
    {
        mPossibleNextChild = null;
        if (mLastChildLink == null)
        {
            mFirstChildLink = mLastChildLink = newLink;
        }
        else
        {
            mLastChildLink.nextChild = newLink;
            newLink.prevChild = mLastChildLink;
            mLastChildLink = newLink;
        }

        return this;
    }

    protected ParseNode appendChild(ParseNode node)
    {
        return appendChild(new Link(node));
    }


}
