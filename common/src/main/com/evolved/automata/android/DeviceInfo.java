package com.evolved.automata.android;

import android.content.Context;
import android.os.Build;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.WindowManager;

public class DeviceInfo 
{
	public final String _DEVICE_BRAND;
	public final String _MANUFACTURER ;
	public final String _MODEL ;
	
	public final int _SCREEN_PIXEL_WIDTH;
	public final int _SCREEN_PIXEL_HEIGHT;
	
	public final float _LOGICAL_DPI;
	//public final float _WIDTH_INCHES;
	//	public final float _HEIGHT_INCHES;
	
	public final float _HORIZONTAL_DPI; // may eventually need to change this definition
										// if landscape support is added
	public final float _VERTICAL_DPI;  // may eventually need to change this definition
									   // if landscape support is added
	
	private static DeviceInfo _thisDevice = null;
	Context _context;
	DisplayMetrics _dm = null;
	
	private DeviceInfo(Context context)
	{
		_DEVICE_BRAND = Build.BRAND;
		_MANUFACTURER = Build.MANUFACTURER;
		_MODEL = Build.MODEL;
		_context = context;
		_dm = new DisplayMetrics();
		WindowManager wm = (WindowManager)_context.getSystemService(Context.WINDOW_SERVICE);
		wm.getDefaultDisplay().getMetrics(_dm);
		_SCREEN_PIXEL_WIDTH = _dm.widthPixels;
		_SCREEN_PIXEL_HEIGHT = _dm.heightPixels;
		
		_LOGICAL_DPI = _dm.density;
		
		_HORIZONTAL_DPI = _dm.xdpi;
		_VERTICAL_DPI = _dm.ydpi;
		
	}
	
	public static DeviceInfo create(Context context)
	{
		if (_thisDevice == null)
		{
			_thisDevice = new DeviceInfo(context);
			
			Log.i("DeviceInfo", _thisDevice.toString());
			return _thisDevice;
		}
		else
			return _thisDevice;
	}
	
	public static DeviceInfo thisDevice()
	{
		return _thisDevice;
	}
	
	public boolean isHTC()
	{
		return _MANUFACTURER.indexOf("HTC")>-1;
	}
	
	public String toString()
	{
		StringBuilder s = new StringBuilder("Device - ");
		s.append("Manufacturer: ");
		s.append(_MANUFACTURER);
		s.append(", Brand: ");
		s.append(_DEVICE_BRAND);
		s.append(", Model: ");
		s.append(_MODEL);
		s.append(", Product: ");
		s.append(Build.PRODUCT);
		// TODO: decide whether to include product in the deviceinfo
		return s.toString();
	}
}
