package com.evolved.automata.android.mindstorms;

import com.evolved.automata.android.mindstorms.NewBTCommunicator.BluetoothRequestInterface;
import com.evolved.automata.android.mindstorms.NewBTCommunicator.BluetoothResponseInterface;


import android.app.Notification;
import android.app.Service;
import android.bluetooth.BluetoothAdapter;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Handler;
import android.os.IBinder;

public class MindstormBluetoothService extends Service
{
	public enum CONNECTION_STATE
	{
		SERVICE_STOPPED,
		SERVICE_STARTED_NOT_CONNECTED_NOT_INITIALIZED,
		SERVICE_STARTED_NOT_CONNECTED_INITIALIZED,
		SERVICE_STARTED_CONNECTED
		
	}
	
	BroadcastReceiver _bluetoothStateReceiver = new BroadcastReceiver()
	{

		@Override
		public void onReceive(Context arg0, Intent intent) {
			if (intent.getAction().equals(BluetoothAdapter.ACTION_STATE_CHANGED))
			{
				int state = intent.getExtras().getInt(BluetoothAdapter.EXTRA_STATE);
				switch (state)
				{
					case BluetoothAdapter.STATE_ON:
						
						break;
					case BluetoothAdapter.STATE_TURNING_ON:
						
						break;
					case BluetoothAdapter.STATE_OFF:
						
						break;
					case BluetoothAdapter.STATE_TURNING_OFF:
						
						break;
				}
				
			}
			
		}
		
	};
	
	BroadcastReceiver _bluetoothConnectionReceiver = new BroadcastReceiver()
	{

		@Override
		public void onReceive(Context arg0, Intent intent) {
			if (intent.getAction().equals(BluetoothAdapter.ACTION_CONNECTION_STATE_CHANGED))
			{
				int state = intent.getExtras().getInt(BluetoothAdapter.EXTRA_CONNECTION_STATE);
				
					
					switch (state)
					{
						case BluetoothAdapter.STATE_CONNECTED:
							
							break;
						case BluetoothAdapter.STATE_CONNECTING:
							
							break;
						case BluetoothAdapter.STATE_DISCONNECTED:
							
							break;
						case BluetoothAdapter.STATE_DISCONNECTING:
							
							break;
					}
				
				
			}
			
		}
		
	};
	
	
	NewBTCommunicator _btInterface;
	static MindstormBluetoothService _this = null;
	public static final int SERVICE_STARTED = 0;
	public static final int SERVICE_STOPPED = 1;
	public static String SERVICE_STATE = "SERVICE_STATE";
	BluetoothResponseInterface _responseInterface = null;
	
	static Object _stateSynch = new Object();
	
	
	public static String MindstormsServiceIntentAction = MindstormBluetoothService.class.getName(); 
	
	@Override
	public IBinder onBind(Intent arg0) {
		
		return null;
	}

	@Override
	public void onCreate() {
		
		super.onCreate();
		synchronized (_stateSynch)
		{
			_this = this;
			_btInterface = new NewBTCommunicator();
			
			IntentFilter bluetoothStateFilter = new IntentFilter(BluetoothAdapter.ACTION_STATE_CHANGED);
			registerReceiver(_bluetoothStateReceiver, bluetoothStateFilter);
			
			IntentFilter bluetoothConnectionFilter = new IntentFilter(BluetoothAdapter.ACTION_CONNECTION_STATE_CHANGED);
			registerReceiver(_bluetoothConnectionReceiver, bluetoothConnectionFilter);
			
			Intent startedIntent = new Intent(MindstormsServiceIntentAction);
			startedIntent.putExtra(SERVICE_STATE, SERVICE_STARTED);
			sendBroadcast(startedIntent);
			setupNotification();
		}
		
	}
	
	protected void setupNotification()
	{
		
	}

	public static BluetoothRequestInterface initialize(BluetoothResponseInterface responseHandler)
	{
		synchronized (_stateSynch)
		{
			if (_this!=null)
			{
				return _this.initializeThis(responseHandler);
			}
		}
		
		return null;
	}
	
	
	public static CONNECTION_STATE getConnectionState()
	{
		synchronized (_stateSynch)
		{
			if (_this == null)
				return CONNECTION_STATE.SERVICE_STOPPED;
			else if (_this._responseInterface!=null && _this._btInterface.isConnected())
				return CONNECTION_STATE.SERVICE_STARTED_CONNECTED;
			else if (_this._responseInterface!=null && !_this._btInterface.isConnected())
				return CONNECTION_STATE.SERVICE_STARTED_NOT_CONNECTED_INITIALIZED;
			else
				return CONNECTION_STATE.SERVICE_STARTED_NOT_CONNECTED_NOT_INITIALIZED;
		}
		
			
	}
	
	private BluetoothRequestInterface initializeThis(BluetoothResponseInterface responseHandler)
	{
		_responseInterface = responseHandler;
		return _btInterface.initialize(responseHandler);
	}
	
	
	@Override
	public void onDestroy() {
		synchronized (_stateSynch)
		{
			_this = null;
			_btInterface.disconnectFromNXT();
			_btInterface = null;
			unregisterReceiver(_bluetoothConnectionReceiver);
			unregisterReceiver(_bluetoothStateReceiver);
			_responseInterface = null;
			Intent stoppedIntent = new Intent(MindstormsServiceIntentAction);
			stoppedIntent.putExtra(SERVICE_STATE, SERVICE_STOPPED);
			sendBroadcast(stoppedIntent);
		}
		
		super.onDestroy();
	}

	@Override
	public int onStartCommand(Intent intent, int flags, int startId) {
		
		return super.onStartCommand(intent, flags, startId);
	}

}
