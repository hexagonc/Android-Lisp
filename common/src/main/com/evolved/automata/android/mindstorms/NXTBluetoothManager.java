package com.evolved.automata.android.mindstorms;

import java.util.LinkedList;
import java.util.Set;

import com.evolved.automata.android.AppStateManager;

import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.content.Context;
import android.content.Intent;

public class NXTBluetoothManager 
{
	Context _context;
	private static  NXTBluetoothManager _manager = null;
	BluetoothAdapter _bAdapter = null;
	boolean _bluetoothAvailableP = false;
	
	public static boolean _debugMode = true;
	
	public static NXTBluetoothManager getInstance()
	{
		
		return _manager;
	}
	
	public static NXTBluetoothManager create(Context context)
	{
		if (_manager == null)
			_manager = new NXTBluetoothManager(context);
		return _manager;
	}
	
	
	private NXTBluetoothManager(Context con)
	{
		_context = con;
		_bAdapter = BluetoothAdapter.getDefaultAdapter();
		_bluetoothAvailableP = _bAdapter != null;
	}
	
	public LinkedList<NXTBluetoothInterface> getPairedDevices() throws BluetoothUnavailableException
	{
		LinkedList<NXTBluetoothInterface> interfaces = new LinkedList<NXTBluetoothInterface>();
		if (_bAdapter == null)
		{
			AppStateManager.getInstance().onError("NXTBluetoothManager", new BluetoothUnavailableException(""));
			return interfaces; 
		}
		
		Set<BluetoothDevice> pairedDevices =  _bAdapter.getBondedDevices();
		
		NXTBluetoothInterface nInterface = null;
		for (BluetoothDevice d:pairedDevices)
		{
			nInterface = new NXTBluetoothInterface(_context, d);
			interfaces.add(nInterface);
		}
		return interfaces;
	}
	
	public boolean startNXTBluetoothService()
	{
		if (_bAdapter!=null)
		{
			Intent intent = new Intent(_context, NXTBluetoothService.class);
			_context.startService(intent);
			return true;
		}
		else
			return false;
		
	}
	
	public boolean stopNXTBluetoothService()
	{
		if (_bAdapter!=null)
		{
			Intent intent = new Intent(_context, NXTBluetoothService.class);
			_context.stopService(intent);
			return true;
		}
		else
			return false;
	}
	
	/**
	 * Returns true if an attempt was made to enable bluetooth
	 * @return
	 */
	public boolean requestEnableBluetoothAdapter()
	{
		if (_bAdapter!=null)
		{
			Intent intent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
			
			_context.startActivity(intent);
			return true;
		}
		else
			return false;
	}
	
}
