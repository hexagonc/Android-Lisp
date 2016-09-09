package com.evolved.automata.android;

import android.os.Build;

public class DeviceBehaviorOverrides 
{
	
	public static final int _SDK_LEVEL = Build.VERSION.SDK_INT;
	
	public static boolean radioPaddingBugP()
	{
		return DeviceInfo.thisDevice().isHTC();
	}
	
	
	
}
