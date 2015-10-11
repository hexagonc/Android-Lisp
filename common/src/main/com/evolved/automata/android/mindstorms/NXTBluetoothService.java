package com.evolved.automata.android.mindstorms;

import java.util.LinkedList;

import android.app.Service;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothSocket;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.IBinder;
import android.os.PowerManager;

public class NXTBluetoothService extends Service implements NXTServiceInterface
{
	
	
	
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
	
	@Override
	public void onCreate()
	{
		super.onCreate();
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
		NXTBluetoothManager.getInstance().showBluetooNotification(true);
		_instance = this;
	}
	
	
	@Override
	public IBinder onBind(Intent intent) {

		return null;
	}


	@Override
	public int onStartCommand(Intent intent, int flags, int startId) {
		
		return super.onStartCommand(intent, flags, startId);
	}


	@Override
	public void onDestroy() {
		_serviceLock.release();
		_serviceStatus = _STOPPED;
		
		notifyListeners(_UPDATE_ALL);
		clearServiceInterface();
		NXTBluetoothManager.getInstance().showBluetooNotification(false);
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
