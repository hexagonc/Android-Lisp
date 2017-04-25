package com.evolved.automata.android.lisp.guibuilder;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.Stack;

/**
 * Created by Evolved8 on 4/22/17.
 */

public abstract class ParseNode {

    public enum TYPE
    {
        LIST, NUMBER, VAR_NAME, TOP, WHITE_SPACE, STRING, LEFT_PAREN, RIGHT_PAREN, QUOTE
    }


    public enum ParseStatus
    {
        INITIAL(false, true), BUILDING(true, true), IN_COMPLETE(false, false), FINISHED(true, true), COMPLETE_BOUNDARY(true, false), COMPLETE_ABSORB(true, true);

        boolean isProgressStatus = false;
        boolean consumedInputCharP = false;
        ParseStatus(boolean progressStatus, boolean consumed)
        {
            isProgressStatus = progressStatus;
            consumedInputCharP = consumed;
        }

        public boolean isProgressP()
        {
            return isProgressStatus;
        }

        public boolean consumedInputP()
        {
            return consumedInputCharP;
        }

    }

    protected static class Link
    {
        public ParseNode node;
        public Link nextChild;
        public Link prevChild;
        public Link(ParseNode node)
        {
            this.node = node;
            node.myLink = this;

        }
    }

    StringBuilder mValue;

    Link mChildLinks = null;
    Link mLastChildLink = null;
    HashSet<ParseNode> mPossibleNextChild;

    ParseNode mParent;
    Link myLink;
    ParseContext mContext;

    TYPE mType;
    ParseStatus mStatus;

    public ParseNode(ParseNode parent, TYPE type)
    {
        mParent = parent;
        mValue = new StringBuilder();
        this.mType = type;
        mChildLinks = null;
        mStatus = ParseStatus.INITIAL;
    }




    public ParseNode setContext(ParseContext context)
    {
        mContext = context;
        return this;
    }

    public TYPE getType()
    {
        return mType;
    }

    public ParseStatus getStatus()
    {
        return mStatus;
    }

    public ParseNode replaceSelf(LinkedList<ParseNode> newNodes)
    {
        ParseNode prevSibling = getPrevSibling();
        ParseNode nextSibling = getNextSibling();
        boolean first = true;


        for (ParseNode node:newNodes)
        {
            node.mParent = mParent;
            if (first)
            {
                myLink.node = node;
                node.myLink = myLink;
                first = false;
            }
            else
            {
                Link nextContainer = new Link(node);
                if (prevSibling !=  null)
                {
                    nextContainer.prevChild = prevSibling.myLink;
                    prevSibling.myLink.nextChild = nextContainer;
                }


            }
            prevSibling = node;
        }

        if (nextSibling != null)
        {
            prevSibling.myLink.nextChild = nextSibling.myLink;
            nextSibling.myLink.prevChild = prevSibling.myLink;
        }
        return myLink.node;
    }

    public int getStartIndex()
    {
        if (getPrevSibling() != null)
            return getPrevSibling().getStartIndex() + getPrevSibling().getLength();
        else if (mParent != null)
            return mParent.getStartIndex() + mParent.getChildOffset();
        else
            return 0;
    }

    public abstract boolean possibleFirstCharP(char firstChar);

    public abstract int getChildOffset();


    public int getLength()
    {
        int total = mValue.length();
        Link child = mChildLinks;
        while (child != null)
        {
            total+=child.node.getLength();
            child = child.nextChild;
        }
        return total;
    }

    public ParseNode getParent()
    {
        return mParent;
    }

    public ParseNode getNextSibling()
    {
        if (myLink != null && myLink.nextChild != null)
            return myLink.nextChild.node;
        else
            return null;
    }

    public ParseNode getPrevSibling()
    {
        if (myLink != null && myLink.prevChild != null)
            return myLink.prevChild.node;
        else
            return null;
    }

    public ParseNode getFirstChild()
    {
        if (mChildLinks != null)
            return mChildLinks.node;
        else
            return null;
    }

    public ParseNode getLastChild()
    {
        if (mLastChildLink != null)
            return mLastChildLink.node;
        else
            return null;
    }

    @Override
    public String toString()
    {
        return getValue();
    }


    public abstract ParseStatus appendChar(char value);

    // TODO: Readd this when finished handling appendChar
    //public abstract ParseNode insertChar(char value);

    // TODO: Readd this when finished handling appendChar
    //public abstract ParseNode deleteChar(int absPosition);

    public ParseNode deleteSelf()
    {
        ParseNode prevSibling = getPrevSibling();
        ParseNode nextSiblign = getNextSibling();

        if (prevSibling != null)
        {
            if (nextSiblign != null)
            {
                prevSibling.myLink.nextChild = nextSiblign.myLink;
                nextSiblign.myLink.prevChild = prevSibling.myLink;
            }
            else
            {
                prevSibling.myLink.nextChild = null;
            }
        }
        else if (nextSiblign != null)
        {
            nextSiblign.myLink.prevChild = null;
        }

        if (mParent.mChildLinks == myLink)
        {
            if (myLink.nextChild != null)
                mParent.mChildLinks = myLink.nextChild;
            else
                mParent.mChildLinks = null;
        }

        if (prevSibling != null)
            return prevSibling;
        else
            return nextSiblign;
    }


    public ParseNode addChild(ParseNode node)
    {
        node.mParent = this;
        Link child = new Link(node);
        if (mChildLinks == null)
        {
            mChildLinks = child;

        }
        else
        {
            ParseNode prevSibling = mChildLinks.node;
            while (prevSibling.myLink.nextChild != null)
            {
                prevSibling = myLink.nextChild.node;
            }
            child.prevChild = prevSibling.myLink;
            prevSibling.myLink.nextChild = child;

        }
        return child.node;
    }

    public ParseNode addChild(Link child)
    {
        child.node.mParent = this;

        if (mChildLinks == null)
        {
            mChildLinks = child;

        }
        else
        {
            ParseNode prevSibling = mChildLinks.node;
            while (prevSibling.myLink.nextChild != null)
            {
                prevSibling = myLink.nextChild.node;
            }
            child.prevChild = prevSibling.myLink;
            prevSibling.myLink.nextChild = child;

        }
        return child.node;
    }

    public LinkedList<Link> getOlderSiblings()
    {
        LinkedList<Link> header = new LinkedList<Link>();
        Stack<Link> olderSiblings = new Stack<Link>();
        Link older = myLink.prevChild;
        boolean first = true;
        while (older != null)
        {
            if (first)
            {
                first = false;
                older.nextChild= null;
            }
            olderSiblings.push(older);
            older = older.prevChild;
        }

        while (olderSiblings.size() > 0)
        {
            header.push(olderSiblings.pop());
        }
        return header;
    }

    public LinkedList<Link> getYoungerSiblings()
    {
        LinkedList<Link> youngerSiblings = new LinkedList<Link>();

        Link younger = myLink.nextChild;
        boolean first = true;
        while (younger != null)
        {
            if (first)
            {
                first = false;
                younger.prevChild= null;
            }
            youngerSiblings.add(younger);
            younger = younger.nextChild;
        }


        return youngerSiblings;
    }

    public LinkedList<Link> trimOlderSiblings()
    {
        LinkedList<Link> out = getOlderSiblings();
        if (myLink.prevChild != null)
        {
            myLink.prevChild.nextChild = null;
            myLink.prevChild = null;
        }

        mParent.mChildLinks = myLink;

        return out;
    }

    public LinkedList<Link> trimYoungerSiblings()
    {
        LinkedList<Link> out = getYoungerSiblings();
        myLink.nextChild = null;
        if (out.size()>0)
        {
            Link first = out.getFirst();
            first.prevChild = null;
        }
        return out;
    }


    public ParseNode findNode(int absPosition)
    {
        int start = getStartIndex();
        int len = getLength();

        if (start + len <= absPosition)
        {
            if (getNextSibling() != null)
                return getNextSibling().findNode(absPosition);
            else
                return null;
        }
        else if (absPosition < start)
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

    /**
     * Finds the most applicable surrounding expression.  If absPosition is in a number, or var_name or
     * string then it returns the ParseNode.  If it is in a whitespace node then it returns the surrounding
     * container node, (normally either the top node or a list node).  If absPosition is at ends of a list
     * then the whole list is returned
     * @param absPosition
     * @return
     */
    public ParseNode selectSurroundingExpr(int absPosition)
    {
        ParseNode selected = findNode(absPosition);
        if (selected != null && selected.getType() == TYPE.WHITE_SPACE)
            return selected.getParent();
        else
            return selected;
    }

    /**
     * Returns the previous non-whitespace sibling node.
     * @param wrap When true, starts testing nodes from the end of the containing node, up to the current node
     * @return
     */
    public ParseNode getPreviousTokenSibling(boolean wrap)
    {
        if (getParent() == null)
        {
            if (wrap)
                return this;
            else
                return null;
        }

        Link testLink = myLink.prevChild;
        Link stopLink;
        if (wrap)
            stopLink = myLink;
        else
            stopLink = null;

        if (testLink == null && wrap)
            testLink = getParent().getLastChild().myLink;

        while (testLink != stopLink && testLink.node.getType() == TYPE.WHITE_SPACE)
        {
            testLink = testLink.prevChild;
            if (testLink == null && wrap)
                testLink = getParent().getLastChild().myLink;
        }
        return (testLink != null)?testLink.node : null;
    }

    /**
     * Returns the next non-whitespace sibling node.
     * @param wrap When true, starts testing nodes from the start of the containing node, up to the current node
     * @return
     */
    public ParseNode getNextTokenSibling(boolean wrap)
    {
        if (getParent() == null)
        {
            if (wrap)
                return this;
            else
                return null;
        }

        Link testLink = myLink.nextChild;
        Link stopLink;
        if (wrap)
            stopLink = myLink;
        else
            stopLink = null;

        if (testLink == null && wrap)
        {
            testLink = getParent().getFirstChild().myLink;
        }

        while (testLink != stopLink && testLink.node.getType() == TYPE.WHITE_SPACE)
        {

            testLink = testLink.nextChild;
            if (testLink == null && wrap)
                testLink = getParent().getFirstChild().myLink;
        }
        return (testLink != null)?testLink.node : null;
    }

    public abstract String getValue();


}
