package com.evolved.automata;
import java.util.*;

public class StateMachine 
{
	
	
	private State priorState=null;
	
	public State run(State startState)
	{
		
		State currentState=startState, nextState;
		
		do
		{
			if (currentState!=null)
			{
				nextState=currentState.processState(priorState);
				priorState=currentState;
				currentState=nextState;
			}
		}while ((currentState!=null)&&(!currentState.isTerminalStateP()));
		
		return currentState;
	}
	
	
}
