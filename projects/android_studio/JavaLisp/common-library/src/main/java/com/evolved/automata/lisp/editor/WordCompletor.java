package com.evolved.automata.lisp.editor;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Stack;

/**
 * Created by Evolved8 on 4/25/17.
 */

public class WordCompletor
{
    private class Node
    {
        HashMap<Character, Node> suffix;
        String data;
        public Node()
        {
            suffix = new HashMap<Character, Node>();
        }

        public Node addCharacter(Character c)
        {
            Node out = suffix.get(c);
            if (out == null)
            {
                out = new Node();
                suffix.put(c, out);
            }
            return out;
        }

        public Node addCharacter(Character c, String d)
        {
            Node out = suffix.get(c);
            if (out == null)
            {
                out = new Node();

                suffix.put(c, out);
            }
            out.setData(d);
            return out;
        }

        public Node setData(String d)
        {
            data = d;
            return this;
        }

        public String getData()
        {
            return data;
        }

        public Node navigateTo(Character c)
        {
            return suffix.get(c);
        }

        public LinkedList<String> getAllCompletions()
        {
            LinkedList<String> completions = new LinkedList<String>();
            for (Character key:suffix.keySet())
            {
                Node subtree = suffix.get(key);
                completions.addAll(subtree.getAllCompletions());
            }
            if (data != null)
                completions.add(data);
            return completions;
        }

        public Node remove(Character c)
        {
            Node prior = suffix.get(c);
            if (prior != null)
                suffix.remove(c);
            return prior;
        }

    }

    Node mBase = null, completionIterator;

    public WordCompletor()
    {

    }

    public LinkedList<String> resetCompletionIterator(Character c)
    {
        completionIterator = mBase;
        return continueCompletion(c);

    }

    public LinkedList<String> getAllWords()
    {
        completionIterator = mBase;
        if (mBase != null)
            return mBase.getAllCompletions();
        else
            return null;

    }

    public LinkedList<String> continueCompletion(Character c)
    {

        if (completionIterator != null)
        {
            completionIterator = completionIterator.navigateTo(c);
            return completionIterator.getAllCompletions();
        }
        else
            return null;

    }



    public Node addWord(String word)
    {
        if (mBase == null)
            mBase = new Node();
        Node cursor = mBase;
        char c;
        int i = 0, length = word.length();

        for (i = 0; i < length;i++)
        {
            c = word.charAt(i);
            if (i == length - 1)
                cursor = cursor.addCharacter(c, word);

            else
                cursor = cursor.addCharacter(c);

        }

        return mBase;
    }

    public Node navigatePrefix(String prefix)
    {
        Node cursor = mBase;

        for (char c:prefix.toCharArray())
        {
            if ((cursor = cursor.navigateTo(c))==null)
            {
                break;
            }
        }
        return cursor;
    }

    public Node removeWord(String word)
    {
        Stack<Node> stack = new Stack<Node>();

        Node next, cursor = mBase;
        Character last = null;
        for (char c:word.toCharArray())
        {
            last = Character.valueOf(c);
            if ((next = cursor.navigateTo(last))==null)
            {
                return null;
            }
            stack.push(next);
            cursor = next;
        }

        while (stack.size() > 0)
        {
            cursor = stack.pop();
            if (word.equals(cursor.getData()))
            {
                cursor.setData(null);
                next = stack.pop();
                if (next.getAllCompletions().size() ==0)
                    next.remove(last);
                return next;
            }
        }
        return null;
    }

}
