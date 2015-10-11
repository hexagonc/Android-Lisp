package com.evolved.automata.android;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;

import android.content.Context;
import android.util.Log;

public class AppStateManager 
{
	public static interface AppStateListener
	{
		public void onError(String source, Exception ex);
		public void onEvent(String key, HashMap<String, String> data);
	}
	
	private static AppStateManager _sManager = null;
	Context _context = null;
	HashSet<WeakReference<AppStateListener>> _listeners = new HashSet<WeakReference<AppStateListener>>();
	HashMap<String, HashMap<String, String>> _eventHistory = new HashMap<String, HashMap<String,String>>();
	
	public AppStateManager(Context context)
	{
		_context = context;
	}
	
	public static AppStateManager getInstance()
	{
		return _sManager;
	}
	
	
	public static AppStateManager create(Context context)
	{
		if (_sManager == null)
			_sManager = new AppStateManager(context);
		return _sManager;
	}
	
	
	public synchronized void onError(String source, Exception ex)
	{
		Log.e(source, ex.toString());
		LinkedList<WeakReference<AppStateListener>> forremoval = new LinkedList<WeakReference<AppStateListener>>();
		for (WeakReference<AppStateListener> listener:_listeners)
		{
			AppStateListener alistener = listener.get();
			if (alistener != null)
				alistener.onError(source, ex);
			else
				forremoval.add(listener);
		}
		
		for (WeakReference<AppStateListener> a:forremoval)
			_listeners.remove(a);
	}
	
	public synchronized void onEvent(String key, HashMap<String, String> data)
	{
		Log.i(key, (data!=null)?data.toString():null);
		_eventHistory.put(key, data);
		LinkedList<WeakReference<AppStateListener>> forremoval = new LinkedList<WeakReference<AppStateListener>>();
		for (WeakReference<AppStateListener> listener:_listeners)
		{
			AppStateListener alistener = listener.get();
			if (alistener != null)
				alistener.onEvent(key, data);
			else
				forremoval.add(listener);
		}
		
		for (WeakReference<AppStateListener> a:forremoval)
			_listeners.remove(a);
	}
	
	public synchronized void simpleMessage(String tag, String message)
	{
		Log.i(tag, message);
		
	}
	
	
	public synchronized void onEvent(String key, String ...keyValuePairs )
	{
		HashMap<String, String> map = new HashMap<String, String>();
		
		
		String dataKey = null;
		
		for (int i=0;i<keyValuePairs.length;i++)
		{
			if (i % 2 == 0)
				dataKey = keyValuePairs[i];
			else
				map.put(dataKey, keyValuePairs[i]);
		}
		onEvent(key, map);
	}
	
}
