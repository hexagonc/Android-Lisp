package com.evolved.automata.nn.grammar;

/**
 * Created by Evolved8 on 2/16/17.
 */

public class CharacterNode implements GrammarNode {
    String token;
    boolean hasNext = true;
    private CharacterNode()
    {

    }



    public static CharacterNode make(String token)
    {
        CharacterNode n = new CharacterNode();
        n.token = token;
        return n;
    }

    public void reset()
    {
        hasNext = true;
    }
    public boolean hasNext()
    {
        return hasNext;
    }

    public String next()
    {
        if (hasNext())
        {
            hasNext = false;
            return token;

        }
        else
            return null;
    }


}
