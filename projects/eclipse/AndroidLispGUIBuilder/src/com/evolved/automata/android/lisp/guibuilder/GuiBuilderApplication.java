package com.evolved.automata.android.lisp.guibuilder;

import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.android.AppStateManager;
import com.evolved.automata.android.mindstorms.NXTBluetoothManager;

import android.app.Application;

public class GuiBuilderApplication extends Application
{

	@Override
	public void onCreate() {
		
		super.onCreate();
		AndroidTools.initialize(null, this, null);
		AppStateManager.create(this);
		NXTBluetoothManager.create(this);
	}
	
}
