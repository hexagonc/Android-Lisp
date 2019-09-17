package com.evolved.automata.android.mindstorms;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map.Entry;
import java.util.Set;

import com.evolved.automata.android.AppStateManager;
import com.evolved.automata.android.mindstorms.NXTBluetoothService.BluetoothStatusListener;
import com.evolved.automata.android.tools.R;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.Service;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.support.v4.app.NotificationCompat;

import static com.evolved.automata.android.mindstorms.NXTBluetoothService._EXTRA_NOTIFICATION_CONTENT;
import static com.evolved.automata.android.mindstorms.NXTBluetoothService._EXTRA_NOTIFICATION_ICON;
import static com.evolved.automata.android.mindstorms.NXTBluetoothService._EXTRA_NOTIFICATION_TITLE;

public class NXTBluetoothManager implements BluetoothStatusListener
{
	Context _context;
	private static  NXTBluetoothManager _manager = null;
	BluetoothAdapter _bAdapter = null;
	boolean _bluetoothAvailableP = false;
	boolean _bluetoothEnabledP = false;
	HashSet<String> _connectedNXTMap = new HashSet<String>();
	public static boolean _debugMode = true;
	boolean _serviceStartedP = false;
	HashMap<String, NXTBluetoothInterface> _nxtInterfaceMap = new HashMap<String, NXTBluetoothInterface>();
	
	
	public static final String _NOTIFICATION_TAG= NXTBluetoothManager.class.getPackage().getName();
	public static final int _NOTIFICATION_ID = 31415;
	public static final String _EXTRA_NOTIFICATION_CHANNEL_ID = "NOTIFICATION_CHANNEL_ID"; // Unique ID
	public static final String _EXTRA_NOTIFICATION_CHANNEL_NAME = "NOTIFICATION_CHANNEL_NAME"; // user facing name
	public static final String _EXTRA_NOTIFICATION_CHANNEL_DESCRIPTION = "NOTIFICATION_CHANNEL_DESCRIPTION";
	public static final String _EXTRA_NOTIFICATION_ID = "NOTIFICATION_ID";



	public static final String sNotificationChannelId = "Android_Mindstorms_SERVICE"; // Unique ID
	public static final String sNotificationChannelName = "Android Lego Mindstorms connection"; // user facing name
	public static final String sNotificationChannelDesc = "Android Lego Mindstorms connectivity channel";



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
		if (_bluetoothAvailableP)
		{
			NXTBluetoothService.addBluetoothStatusListener(this);
			//startNXTBluetoothService();
		}
	}
	
	public boolean bluetoothAdapterPresentP()
	{
		return _bluetoothAvailableP;
	}
	
	public boolean bluetoothAdapterEnabledP()
	{
		return _bluetoothEnabledP;
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
			if (_nxtInterfaceMap.containsKey(d.getName()))
			{
				nInterface = _nxtInterfaceMap.get(d.getName());
			}
			else
			{
				nInterface = new NXTBluetoothInterface(_context, d);
				NXTBluetoothService.addBluetoothStatusListener(nInterface);
				_nxtInterfaceMap.put(d.getName(), nInterface);
			}
			interfaces.add(nInterface);
		}
		return interfaces;
	}
	
	public void disconnectAllNXT() throws Exception
	{
		NXTBluetoothInterface ninterface;
		for (Entry<String, NXTBluetoothInterface> entry :_nxtInterfaceMap.entrySet())
		{
			ninterface = entry.getValue();
			if (ninterface.isConnected())
				ninterface.disconnect();
		}
	}
	
	public boolean startNXTBluetoothService()
	{
		// TODO: This needs to take input parameters for the notification id and title and content and icon
		if (_bAdapter!=null)
		{
			Intent intent = new Intent(_context, NXTBluetoothService.class);
			intent.putExtra(_EXTRA_NOTIFICATION_CHANNEL_NAME, sNotificationChannelName);
			intent.putExtra(_EXTRA_NOTIFICATION_CHANNEL_DESCRIPTION, sNotificationChannelDesc);
			intent.putExtra(_EXTRA_NOTIFICATION_CHANNEL_ID, sNotificationChannelId);

			// These parameters aren't used at the moment because updateBluetoothNotification
			// doesn't accept notification configuration parameters
			intent.putExtra(_EXTRA_NOTIFICATION_ID, _NOTIFICATION_ID);
			intent.putExtra(_EXTRA_NOTIFICATION_CONTENT, "NXT Bluetooth Connectivity service");
			intent.putExtra(_EXTRA_NOTIFICATION_TITLE, "NXT Bluetooth Service");

			intent.putExtra(_EXTRA_NOTIFICATION_ICON, R.drawable.ic_small_nxt_disconnected);
			if (Build.VERSION.SDK_INT >= 26 )
				_context.startForegroundService(intent);
			else
				_context.startService(intent);
			return true;
		}
		else
			return false;
		
	}
	
	public boolean stopNXTBluetoothService()
	{
		// TODO: make notification id a parameter
		if (_bAdapter!=null)
		{
			Intent intent = new Intent(_context, NXTBluetoothService.class);
			intent.putExtra(_EXTRA_NOTIFICATION_ID, _NOTIFICATION_ID);
			_context.stopService(intent);
			return true;
		}
		else
			return false;
	}
	
	public boolean setBluetoothDebugMode(boolean enable)
	{
		return _debugMode = enable;
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
	
	public Notification updateBluetoothNotification(boolean serviceRunning, boolean connected, String nxtName)
	{
		if (connected)
		{
			_connectedNXTMap.add(nxtName);
		}
		else
			_connectedNXTMap.remove(nxtName);
		
		return updateBluetoothNotification(serviceRunning, true);
	}





	public Notification updateBluetoothNotification(boolean serviceRunning, boolean startIfNecessary)
	{
		NotificationManager manager = (NotificationManager)_context.getSystemService(Context.NOTIFICATION_SERVICE);
		if (!serviceRunning)
		{
			manager.cancel(_NOTIFICATION_TAG, _NOTIFICATION_ID);
			_serviceStartedP = false;
			return null;
		}


		String ticker = null;
		String title = null;
		String content = null;
		int small_icon = 0;
		if (_connectedNXTMap.size()>0)
		{
			title = "Connected to NXT" + ((_connectedNXTMap.size()>1)?"s":"");
			small_icon = com.evolved.automata.android.tools.R.drawable.ic_small_nxt_connected;
			content = _connectedNXTMap.toString();
		}
		else
		{
			title = "Disconnected from NXTs";
			content = "";
			small_icon = com.evolved.automata.android.tools.R.drawable.ic_small_nxt_disconnected;
		}

		if (serviceRunning && !_serviceStartedP)
		{
			ticker = "NXT Bluetooth Service starting";
			_serviceStartedP = true;
		}

		// TODO: Figure out if there should be a pending intent for this
		Notification notification = null;
		NotificationCompat.Builder builder = new NotificationCompat.Builder(_context, sNotificationChannelId);

		builder.setContentText(content);
		builder.setContentTitle(title);
		builder.setSmallIcon(small_icon);
		builder.setOngoing(true);

		if (ticker!=null)
			builder.setTicker(ticker);

		notification = builder.build();

		if (startIfNecessary)
			manager.notify(_NOTIFICATION_TAG, _NOTIFICATION_ID, notification);

		return notification;
	}

	@Override
	public void onStatusChange(int state) {
		_bluetoothEnabledP = state == BluetoothAdapter.STATE_ON;
	}

	@Override
	public void onConnectionChange(int connectionState) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void onServiceStatusChanged(int status) {
		// TODO Auto-generated method stub
		
	}
	
}
