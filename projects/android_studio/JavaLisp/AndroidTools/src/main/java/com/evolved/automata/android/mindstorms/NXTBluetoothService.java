package com.evolved.automata.android.mindstorms;

import java.util.LinkedList;

import android.annotation.SuppressLint;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.Service;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothSocket;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Build;
import android.os.IBinder;
import android.os.PowerManager;
import android.support.v4.app.NotificationCompat;

import com.evolved.automata.android.tools.R;

import static com.evolved.automata.android.mindstorms.NXTBluetoothManager._NOTIFICATION_ID;

public class NXTBluetoothService extends Service implements NXTServiceInterface
{
	
	
	static boolean sConfiguredChannelP = false;

	public interface BluetoothStatusListener
	{
		public void onStatusChange(int state);
		public void onConnectionChange(int connectionState);
		public void onServiceStatusChanged(int status);
	}
	
	private static NXTServiceInterface _serviceInteface = null;
	
	
	public static final int _STARTED = 1;
	public static final int _STOPPED = 0;
	public static final int _UPDATE_ALL = 0;
	public static final int _UPDATE_CONNECTION_STATE_LISTENERS = 1;
	public static final int _UPDATE_ADAPTER_STATUS_LISTENERS = 2;
	public static final int _UPDATE_SERVICE_STATUS_LISTENERS = 3;
	
	int _connectionState = BluetoothAdapter.STATE_DISCONNECTED;
	int _adapterStatus = BluetoothAdapter.STATE_OFF;
	static int _serviceStatus = _STOPPED;
	
	static LinkedList<BluetoothStatusListener> _statusListeners = new LinkedList<NXTBluetoothService.BluetoothStatusListener>();
	
	BroadcastReceiver _bluetoothStateReceiver = null; 
	
	BroadcastReceiver _bluetoothConnectionReceiver = null;
	
	PowerManager.WakeLock _serviceLock;
	BluetoothStatusListener _statusListener;
	
	static Object _interfaceMutex = new Object();
	
	private static NXTBluetoothService _instance = null;

	public static final String _EXTRA_NOTIFICATION_TITLE = "NXT_SERVICE_TITLE";
	public static final String _EXTRA_NOTIFICATION_CONTENT = "NXT_SERVICE_CONTENT";
	public static final String _EXTRA_NOTIFICATION_ICON = "NXT_SERVICE_ICON";
	public static final String _EXTRA_NOTIFICATION_CHANNEL = "NXT_SERVICE_CHANNEL";
	
	@Override
	public void onCreate()
	{

		PowerManager pmanager = (PowerManager)getSystemService(Context.POWER_SERVICE);
		_serviceLock = pmanager.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "mindstorms");
		_serviceLock.acquire();
		
		_bluetoothConnectionReceiver = getConnectionReceiver();
		_bluetoothStateReceiver = getStatusReceiver();
		
		IntentFilter bluetoothStateFilter = new IntentFilter(BluetoothAdapter.ACTION_STATE_CHANGED);
		BluetoothAdapter adapter = BluetoothAdapter.getDefaultAdapter();
		if (adapter != null)
		{
			_adapterStatus = (adapter.isEnabled())?BluetoothAdapter.STATE_ON:BluetoothAdapter.STATE_OFF;
		}
		
		registerReceiver(_bluetoothStateReceiver, bluetoothStateFilter);
		
		IntentFilter bluetoothConnectionFilter = new IntentFilter(BluetoothAdapter.ACTION_CONNECTION_STATE_CHANGED);
		
		registerReceiver(_bluetoothConnectionReceiver, bluetoothConnectionFilter);
		
		_serviceStatus = _STARTED;
		
		notifyListeners(_UPDATE_ALL);
		_serviceInteface = this;
		_instance = this;

	}
	
	
	@Override
	public IBinder onBind(Intent intent) {

		return null;
	}


	@SuppressLint("NewApi")
	@Override
	public int onStartCommand(Intent intent, int flags, int startId) {
		String title = intent.getStringExtra(_EXTRA_NOTIFICATION_TITLE);
		String content = intent.getStringExtra(_EXTRA_NOTIFICATION_CONTENT);

		// Notification must exist before starting
		if (!sConfiguredChannelP){
			if (Build.VERSION.SDK_INT>=26){
				int importance = NotificationManager.IMPORTANCE_LOW;
				String channelName = intent.getStringExtra(NXTBluetoothManager._EXTRA_NOTIFICATION_CHANNEL_NAME);
				String channelId = intent.getStringExtra(NXTBluetoothManager._EXTRA_NOTIFICATION_CHANNEL_ID);

				NotificationChannel channel = new NotificationChannel(channelId, channelName, importance);
				channel.setDescription(NXTBluetoothManager._EXTRA_NOTIFICATION_CHANNEL_DESCRIPTION);
				NotificationManager manager = (NotificationManager)getSystemService(Context.NOTIFICATION_SERVICE);
				manager.createNotificationChannel(channel);
			}
			sConfiguredChannelP = true;
		}

		Notification notification = NXTBluetoothManager.getInstance().updateBluetoothNotification(true, false);

		startForeground(_NOTIFICATION_ID, notification);
		return Service.START_NOT_STICKY;
	}


	@Override
	public void onDestroy() {
		_serviceLock.release();
		_serviceStatus = _STOPPED;
		
		notifyListeners(_UPDATE_ALL);
		clearServiceInterface();
		NXTBluetoothManager.getInstance().updateBluetoothNotification(false, false);
		
		try
		{
			if (_bluetoothConnectionReceiver != null)
				unregisterReceiver(_bluetoothConnectionReceiver);
		}
		catch (IllegalStateException ie)
		{
			
		}
		
		try
		{
			if (_bluetoothStateReceiver != null)
				unregisterReceiver(_bluetoothStateReceiver);
		}
		catch (IllegalStateException ie)
		{
			
		}
		
		_instance = null;
		super.onDestroy();
	}
	
	public static void addBluetoothStatusListener(BluetoothStatusListener listener)
	{
		if (!_statusListeners.contains(listener))
		{
			_statusListeners.add(listener);
			if (_instance !=null)
				_instance.notifyListeners(listener);
		}
		
	}
	
	public BroadcastReceiver getStatusReceiver()
	{
		return new BroadcastReceiver()
		{

			@Override
			public void onReceive(Context arg0, Intent intent) {
				if (intent.getAction().equals(BluetoothAdapter.ACTION_STATE_CHANGED))
				{
					int state = intent.getExtras().getInt(BluetoothAdapter.EXTRA_STATE);
					synchronized (_interfaceMutex)
					{
						_adapterStatus = state;
					}
					notifyListeners(_UPDATE_ADAPTER_STATUS_LISTENERS);
					
					
				}
				
			}
			
		};
	}
	
	public BroadcastReceiver getConnectionReceiver()
	{
		return new BroadcastReceiver()
		{

			@Override
			public void onReceive(Context arg0, Intent intent) {
				if (intent.getAction().equals(BluetoothAdapter.ACTION_CONNECTION_STATE_CHANGED))
				{
					int state = intent.getExtras().getInt(BluetoothAdapter.EXTRA_CONNECTION_STATE);
					synchronized (_interfaceMutex)
					{
						_connectionState = state;
					}
					notifyListeners(_UPDATE_CONNECTION_STATE_LISTENERS);
					
				}
				
			}
			
		};
	}

	private void notifyListeners(int updateType)
	{
		if (_statusListeners.size()>0)
		{
			for (BluetoothStatusListener listener:_statusListeners)
			{
				if (updateType == _UPDATE_ALL || updateType == _UPDATE_CONNECTION_STATE_LISTENERS)
					listener.onConnectionChange(_connectionState);
				if (updateType == _UPDATE_ALL || updateType == _UPDATE_SERVICE_STATUS_LISTENERS)
					listener.onServiceStatusChanged(_serviceStatus);
				if (updateType == _UPDATE_ALL || updateType == _UPDATE_ADAPTER_STATUS_LISTENERS)
					listener.onStatusChange(_adapterStatus);
			}
		}
	}
	
	private void notifyListeners(BluetoothStatusListener listener)
	{
		listener.onServiceStatusChanged(_serviceStatus);
		listener.onStatusChange(_adapterStatus);
		listener.onConnectionChange(_connectionState);
	}


	@Override
	public boolean serviceRunningP() {
		synchronized (_interfaceMutex)
		{
			return _serviceStatus == _STARTED;
		}
		
	}


	@Override
	public boolean bluetoothAdapterOnP() {
		synchronized (_interfaceMutex)
		{
			return _adapterStatus == BluetoothAdapter.STATE_ON;
		}
	}
	
	public static NXTServiceInterface getServiceInterface()
	{
		synchronized (_interfaceMutex)
		{
			return _serviceInteface;
		}
	}
	
	private void clearServiceInterface()
	{
		synchronized (_interfaceMutex)
		{
			_serviceInteface = null;
		}
	}
	
}
