package com.evolved.automata.lisp.editor;



import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Stack;

/**
 * Created by Evolved8 on 4/22/17.
 */

public abstract class ParseNode {

    protected static final String SERIALIZED_ITEM_SEPARATOR ="æ";
    protected static final String STANDARD_FIELD_SEPARATOR = "å";
    protected static final String CHILD_NODE_SEPARATOR = "Â";

    protected static final int BASE_SERIALIZED_TYPE_INDEX = 0;
    protected static final int BASE_SERIALIZED_STATUS_INDEX = BASE_SERIALIZED_TYPE_INDEX + 1;
    protected static final int BASE_SERIALIZED_CHAR_DATA_INDEX = BASE_SERIALIZED_STATUS_INDEX + 1;
    protected static final int BASE_SERIALIZED_CHILDREN_INDEX = BASE_SERIALIZED_CHAR_DATA_INDEX + 1;
    protected static final int BASE_EXTRA_DATA_INDEX = BASE_SERIALIZED_CHILDREN_INDEX + 1;

    public enum TYPE
    {
        LIST, NUMBER, VAR_NAME, TOP, WHITE_SPACE, STRING, COMMENT, INVALID_CHARS
    }


    public enum ParseStatus
    {
        INITIAL(false, true),
        BUILDING(true, true),
        ERROR(false, false),
        FINISHED(true, true),
        COMPLETE_BOUNDARY(true, false),
        COMPLETE_ABSORB(true, true);

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

    Link mFirstChildLink = null;
    Link mLastChildLink = null;


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
        mFirstChildLink = null;
        mStatus = ParseStatus.INITIAL;
    }

    protected ParseNode(TYPE type)
    {
        mParent = null;
        mValue = new StringBuilder();
        this.mType = type;
        mFirstChildLink = null;
        mStatus = ParseStatus.INITIAL;
    }


    public ParseNode setParent(ParseNode parent)
    {
        mParent = parent;
        if (mParent != null)
            mContext = parent.getParseContext();
        return this;
    }



    public ParseContext getParseContext()
    {
        return mContext;
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

    protected ParseStatus setStatus(ParseStatus newStatus)
    {
        if (newStatus == mStatus)
            return mStatus;

        mStatus = newStatus;

        if (mContext != null)
        {
            switch (mStatus)
            {
                case ERROR:
                    mContext.setErrorNode(this);
                    break;
                case COMPLETE_ABSORB:
                case COMPLETE_BOUNDARY:
                case FINISHED:
                    mContext.removeIncompleteNode(this);
                    mContext.removeErrorNode(this);
                    break;
                case INITIAL:
                case BUILDING:
                    mContext.removeErrorNode(this);
                    mContext.addIncompleteNode(this);
                    break;

            }
        }
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
        Link child = mFirstChildLink;
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
        if (mFirstChildLink != null)
            return mFirstChildLink.node;
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

        if (mParent.mFirstChildLink == myLink)
        {
            if (myLink.nextChild != null)
                mParent.mFirstChildLink = myLink.nextChild;
            else
                mParent.mFirstChildLink = null;
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
        if (mFirstChildLink == null)
        {
            mFirstChildLink = child;

        }
        else
        {
            ParseNode prevSibling = mFirstChildLink.node;
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

        if (mFirstChildLink == null)
        {
            mFirstChildLink = child;

        }
        else
        {
            ParseNode prevSibling = mFirstChildLink.node;
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

        mParent.mFirstChildLink = myLink;

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
            if (mFirstChildLink != null) // there will never be both a non-null mFirstChildLink and mValue.length > 0
            {
                return mFirstChildLink.node.findNode(absPosition);
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

    protected String serialize(boolean bool)
    {
        if (bool)
            return "1";
        else
            return "0";
    }

    protected void fill(String[] fields, HashMap<Integer, ParseNode> inverseMap)
    {
        mStatus = getStatus(fields);
        mValue = new StringBuilder(getValue(fields));
        String[] serializedChildren = getSerializedChildIndices(fields);
        mFirstChildLink = mLastChildLink = null;
        Link prevLink = null;
        for (String serializedChildIndex:serializedChildren)
        {
            Integer index = Integer.valueOf(Integer.parseInt(serializedChildIndex));
            ParseNode node = inverseMap.get(index);
            node.setParent(this);
            if (prevLink == null)
            {
                mLastChildLink = mFirstChildLink = prevLink = new Link(node);
            }
            else
            {
                prevLink = mLastChildLink;
                mLastChildLink = new Link(node);
                prevLink.nextChild = mLastChildLink;
                mLastChildLink.prevChild = prevLink;
            }
        }

    }

    protected String serialize(HashMap<ParseNode, Integer> nodeIndex)
    {
        ArrayList<String> serializedFields =  serializeFields(nodeIndex);
        StringBuilder builder = new StringBuilder();
        for (String field:serializedFields)
        {
            if (builder.length() > 0 )
                builder.append(STANDARD_FIELD_SEPARATOR);
            builder.append(field);
        }
        return builder.toString();
    }

    protected ArrayList<String> serializeFields(HashMap<ParseNode, Integer> nodeIndex)
    {

        ArrayList<String> list = new ArrayList<String>();
        list.add(mType.name());
        list.add(mStatus.name());
        list.add((mValue == null)?"":mValue.toString());
        list.add(serializedChildren(nodeIndex));

        return list;

    }

    protected String serializedChildren(HashMap<ParseNode, Integer> nodeIndex)
    {
        StringBuilder builder = new StringBuilder();
        Link currentChild = mFirstChildLink;
        while (currentChild != null)
        {
            if (currentChild != mFirstChildLink)
                builder.append(CHILD_NODE_SEPARATOR);
            builder.append(nodeIndex.get(currentChild.node));
            currentChild = currentChild.nextChild;
        }
        return builder.toString();
    }

    protected boolean toBoolean(String serialized)
    {
        return ("1".equalsIgnoreCase(serialized));

    }

    protected static String[] getSerializedFields(String serialized)
    {
        return StringUtils.splitByWholeSeparatorPreserveAllTokens(serialized, STANDARD_FIELD_SEPARATOR);
    }



    protected static TYPE getType(String[] serializedFields)
    {
        String typeValue = serializedFields[BASE_SERIALIZED_TYPE_INDEX];
        TYPE type = TYPE.valueOf(typeValue);
        return type;
    }

    protected static ParseStatus getStatus(String[] serializedFields)
    {
        String statusValue = serializedFields[BASE_SERIALIZED_STATUS_INDEX];
        ParseStatus status = ParseStatus.valueOf(statusValue);
        return status;
    }


    protected String[] getSerializedChildIndices(String[] serializedFields)
    {
        String children = serializedFields[BASE_SERIALIZED_CHILDREN_INDEX];
        if (children.length()>0)
            return StringUtils.splitByWholeSeparatorPreserveAllTokens(children, CHILD_NODE_SEPARATOR);
        else
            return new String[0];
    }

    protected String getValue(String[] serializedFields)
    {
        return serializedFields[BASE_SERIALIZED_CHAR_DATA_INDEX];
    }

    protected static ParseNode getBaseInstance(TYPE type)
    {
        switch (type)
        {
            case INVALID_CHARS:
                return new ErrorTextNode();
            case NUMBER:
                return new NumberNode();
            case VAR_NAME:
                return new VarNameNode();
            case LIST:
                return new ListNode();
            case TOP:
                return new TopParseNode();
            case WHITE_SPACE:
                return new WhiteSpaceNode();
            case STRING:
                return new StringNode();
            case COMMENT:
                return new CommentNode();

        }
        return null;
    }


    public void sort(HashMap<ParseNode, Integer> nodeIndex, HashMap<Integer, ParseNode> inverseIndex)
    {
        Integer length = nodeIndex.size();
        nodeIndex.put(this, length);
        inverseIndex.put(length, this);
    }




    public static ParseNode deserialize(String serialized, HashMap<Integer, ParseNode> inverseMap)
    {
        String[] fields = getSerializedFields(serialized);
        TYPE baseType = getType(fields);
        ParseNode baseNode = getBaseInstance(baseType);
        baseNode.fill(fields, inverseMap);

        int myIndex = inverseMap.size();
        return baseNode;
    }

    public String serialize()
    {
        HashMap<ParseNode, Integer> nodeIndex = new HashMap<ParseNode, Integer>();
        HashMap<Integer, ParseNode> inverseIndex = new HashMap<Integer, ParseNode>();

        sort(nodeIndex, inverseIndex);
        StringBuilder result = new StringBuilder();
        Integer serializeOrderIndex;
        ParseNode targetNode;
        String serializedNode;
        for (int i = 0;i < nodeIndex.size();i++)
        {
            serializeOrderIndex = Integer.valueOf(i);
            targetNode = inverseIndex.get(serializeOrderIndex);
            if (i > 0)
            {
                result.append(ParseNode.SERIALIZED_ITEM_SEPARATOR);
            }
            serializedNode = targetNode.serialize(nodeIndex);
            result.append(serializedNode);
        }
        return result.toString();
    }

    public static ParseNode deserialize(String serializedTree)
    {
        String[] serializedNodes = StringUtils.splitByWholeSeparatorPreserveAllTokens(serializedTree,SERIALIZED_ITEM_SEPARATOR);
        HashMap<Integer, ParseNode> inverseIndex = new HashMap<Integer, ParseNode>();
        Integer index;
        int topIndex = serializedNodes.length - 1;
        ParseNode node;
        String serializedNode;

        for (int i = 0;i < serializedNodes.length;i++)
        {
            index = Integer.valueOf(i);
            serializedNode = serializedNodes[i];
            node = deserialize(serializedNode, inverseIndex);

            inverseIndex.put(index, node);
        }

        return inverseIndex.get(topIndex);
    }

}
