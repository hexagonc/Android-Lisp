package com.evolved.automata.android.lisp.guibuilder.events;

public interface ActivityLifeCycleEventListener 
{
	public void onCreate(Object obj);
	public void onDestroy(Object obj);
	public void onPause(Object obj);
	public void onResume(Object obj);
	public void onStart(Object obj);
	public void onStop(Object obj);
	
}
