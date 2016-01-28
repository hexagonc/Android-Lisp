package com.evolved.automata.android.lisp.guibuilder;

import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.android.AppStateManager;
import com.evolved.automata.android.DeviceInfo;
import com.evolved.automata.android.lisp.AndroidLispInterpreter;
import com.evolved.automata.android.lisp.guibuilder.events.EventManager;
import com.evolved.automata.android.mindstorms.NXTBluetoothManager;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.LispInterpreter;

import android.app.Application;

public class GuiBuilderApplication extends Application
{

	
	GlobalInterface _data = null;
	
	
	@Override
	public void onCreate() {
		
		super.onCreate();
		
		try
		{
			EventManager.create(this);
			AndroidTools.initialize(null, this, null);
			AppStateManager.create(this);
			NXTBluetoothManager.create(this);
			DeviceInfo.create(this);
			GuiBuilderConfiguration.create(this);
			_data = new GlobalInterface(this);
			MenuManager.create(this);
			CodeManager.create(this);
			
		}
		catch (Exception e)
		{
			throw new RuntimeException(e);
		}
		
		
	}
	
	public GlobalInterface getGlobalData()
	{
		return _data;
	}
}
