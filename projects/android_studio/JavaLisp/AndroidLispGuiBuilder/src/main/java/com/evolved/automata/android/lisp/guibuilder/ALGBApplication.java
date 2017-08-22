package com.evolved.automata.android.lisp.guibuilder;

import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.android.AppStateManager;
import com.evolved.automata.android.DeviceInfo;

import com.evolved.automata.android.mindstorms.NXTBluetoothManager;

import android.app.Application;
import android.content.SharedPreferences;

public class ALGBApplication extends Application
{


	ALGB mApplication= null;
	static final String _USER_ACCESS_TOKEN_PREF_KEY = "ACCESS-TOKEN-PREF-KEY";
	
	@Override
	public void onCreate() {
		
		super.onCreate();
		try
		{

			mApplication = new ALGB(getApplicationContext());
			AndroidTools.initialize(null, this, null);
			AppStateManager.create(this);
			NXTBluetoothManager.create(this);
			DeviceInfo.create(this);
			DropboxManager.create(getApplicationContext());

			EventLog.create(mApplication);

		}
		catch (Exception e)
		{
			throw new RuntimeException(e);
		}

	}

	public static String getAccessToken()
	{
		return AndroidTools.getStringPreferenceSetting(_USER_ACCESS_TOKEN_PREF_KEY, null);
	}

	public static void saveAccessToken(String token)
	{
		AndroidTools.setStringPreferenceSetting(_USER_ACCESS_TOKEN_PREF_KEY, token);
	}
	

	public ALGB getALGB()
	{
		return mApplication;
	}

	
}
