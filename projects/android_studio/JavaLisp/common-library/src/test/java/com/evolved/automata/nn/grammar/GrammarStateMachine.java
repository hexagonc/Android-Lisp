package com.evolved.automata.nn.grammar;

import com.evolved.automata.nn.FastLSTMNetwork;

/**
 * Created by Evolved8 on 2/16/17.
 */

public class GrammarStateMachine implements GrammarNode {

    int[][] transitionMatrix;
    GrammarNode[] states;
    int currentState = 0;
    int initialState;

    private GrammarStateMachine()
    {

    }

    public static GrammarStateMachine make(int initialState, GrammarNode[] states, int[][] transitionMatrix)
    {
        GrammarStateMachine g = new GrammarStateMachine();
        g.transitionMatrix = transitionMatrix;
        g.states = states;
        g.currentState = g.initialState = initialState;
        return g;
    }

    public String next()
    {
        String next;
        while (!pastEndState())
        {
            next = states[currentState].next();
            if (next != null)
                return next;
            else
            {
                states[currentState].reset(); // so it can be used again
                int[] successorStates = transitionMatrix[currentState];
                double random = FastLSTMNetwork.randomLCG();
                int index = (int)(successorStates.length * random);
                currentState = successorStates[index];
            }

        }

        return null;

    }

    public void reset()
    {
        currentState = initialState;

    }

    public boolean pastEndState()
    {
        return currentState >= states.length ;
    }

}
