package com.evolved.automata.android.lisp.guibuilder.events;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;

import android.content.Context;

public class EventManager 
{
	Context _context;
	
	HashSet<ActivityLifeCycleEventListener> _lifeCycleEventSet = new HashSet<ActivityLifeCycleEventListener >();
	HashSet<ToolAreaEventListener> _toolAreaEventSet = new HashSet<ToolAreaEventListener>();
	HashSet<ProjectEventListener> _projectEventSet = new HashSet<ProjectEventListener>();
	
	
	
	private static EventManager _event;
	
	private EventManager(Context con)
	{
		_context = con;
	}
	
	public static EventManager getInstance()
	{
		return _event;
	}
	
	public static EventManager create(Context con)
	{
		return _event = new EventManager(con);
	}
	
	public void setToolAreaEventListener(ToolAreaEventListener listener)
	{
		_toolAreaEventSet.add(listener);
	}
	
	public ToolAreaEventListener getToolAreaEventNotifier()
	{
		return new ToolAreaEventListener()
		{

			@Override
			public void localStorageToolsSelected() {
				for (ToolAreaEventListener listener:_toolAreaEventSet)
				{
					listener.localStorageToolsSelected();
				}
			}

			@Override
			public void dropboxToolsSelected() {
				for (ToolAreaEventListener listener:_toolAreaEventSet)
				{
					listener.dropboxToolsSelected();
				}
			}

			@Override
			public void codeTemplateToolsSelected() {
				for (ToolAreaEventListener listener:_toolAreaEventSet)
				{
					listener.codeTemplateToolsSelected();
				}
			}
		};
	}
	
	
	public void setProjectEventListener(ProjectEventListener listener)
	{
		_projectEventSet.add(listener);
	}
	
	public ProjectEventListener getProjectEventNotifier()
	{
		return new ProjectEventListener()
		{

			@Override
			public void currentCodePageDeleted(String newPageTitle, String newPageText, boolean hasPrevious, boolean hasNext){
				for (ProjectEventListener listener:_projectEventSet)
				{
					listener.currentCodePageDeleted(newPageTitle, newPageText, hasPrevious, hasNext);
				}
				
			}
			
		};
	}
	
	/**
	 * Sets the objects that will be notified when the events occurs
	 * 
	 */
	public void setActivityLifeCycleEventListener(ActivityLifeCycleEventListener listener)
	{
		_lifeCycleEventSet.add(listener);
	}
	
	/**
	 * Gets a temporary object that will be used to notify the activity life cycle listeners.  Call
	 * the appropriate methods indicating the event that occurred
	 * 
	 * @return
	 */
	public ActivityLifeCycleEventListener getLifeCycleEventNotifier()
	{
		return new ActivityLifeCycleEventListener()
		{

			@Override
			public void onCreate(Object obj) {
				for (ActivityLifeCycleEventListener listener:_lifeCycleEventSet)
				{
					listener.onCreate(obj);
				}
			}

			@Override
			public void onDestroy(Object obj) {
				for (ActivityLifeCycleEventListener listener:_lifeCycleEventSet)
				{
					listener.onDestroy(obj);
				}
			}

			@Override
			public void onPause(Object obj) {
				for (ActivityLifeCycleEventListener listener:_lifeCycleEventSet)
				{
					listener.onPause(obj);
				}
			}

			@Override
			public void onResume(Object obj) {
				for (ActivityLifeCycleEventListener listener:_lifeCycleEventSet)
				{
					listener.onResume(obj);
				}
			}

			@Override
			public void onStart(Object obj) {
				for (ActivityLifeCycleEventListener listener:_lifeCycleEventSet)
				{
					listener.onStart(obj);
				}
			}

			@Override
			public void onStop(Object obj) {
				for (ActivityLifeCycleEventListener listener:_lifeCycleEventSet)
				{
					listener.onStop(obj);
				}
			}

			@Override
			public void onResetEnvironmentRequested(Object obj) {
				for (ActivityLifeCycleEventListener listener:_lifeCycleEventSet)
				{
					listener.onResetEnvironmentRequested(obj);
				}
			}
			
		};
	}
}
