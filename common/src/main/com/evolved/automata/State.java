package com.evolved.automata;

public interface State {
	public void setData(Object data);
	public Object getData();
	public State processState(State priorState);
	public boolean isTerminalStateP();
	
}
