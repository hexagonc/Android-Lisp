package com.evolved.automata.android.lisp.guibuilder;

import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.android.AppStateManager;
import com.evolved.automata.android.DeviceInfo;

import com.evolved.automata.android.mindstorms.NXTBluetoothManager;

import android.app.Application;

public class ALGBApplication extends Application
{


	
	
	@Override
	public void onCreate() {
		
		super.onCreate();
		
		
		try
		{

			AndroidTools.initialize(null, this, null);
			AppStateManager.create(this);
			NXTBluetoothManager.create(this);
			DeviceInfo.create(this);


			DropboxManager.create(getApplicationContext());


		}
		catch (Exception e)
		{
			throw new RuntimeException(e);
		}
		
		
	}
	

	
}
