package com.evolved.automata.android.mindstorms.lisp;

import java.util.LinkedList;

import com.evolved.automata.android.AppStateManager;
import com.evolved.automata.android.mindstorms.BluetoothUnavailableException;
import com.evolved.automata.android.mindstorms.NXTMessage;
import com.evolved.automata.android.mindstorms.NXTBluetoothInterface;
import com.evolved.automata.android.mindstorms.NXTBluetoothManager;
import com.evolved.automata.android.mindstorms.NXTBluetoothService;
import com.evolved.automata.android.mindstorms.NXTServiceInterface;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;

public class NXTLispFunctions 
{
	public static void addFunctions(final Environment env, final NXTBluetoothManager manager)
	{
		env.mapFunction("get-paired-devices", get_paired_devices(manager));
		env.mapFunction("get-device-name", get_device_name(manager));
		env.mapFunction("get-device-mac-address", get_device_mac_address(manager));
		env.mapFunction("nxt-service-running-p", nxt_service_running_p(manager));
		env.mapFunction("bluetooth-adapter-on-p", bluetooth_adapter_on_p(manager));
		env.mapFunction("request-bluetooth-adapter_enable", request_bluetooth_adatper_enable(manager));
		env.mapFunction("start-nxt-service", start_nxt_service(manager));
		env.mapFunction("stop-nxt-service", stop_nxt_service(manager));
		env.mapFunction("connect-to-device", connect_to_device(manager));
		env.mapFunction("disconnect-from-device", disconnect_from_device(manager));
		env.mapFunction("set-motor-power", set_motor_power(manager));
		env.mapFunction("configure-sensor-port", configure_sensor_port(manager));
		env.mapFunction("get-raw-sensor-value", get_raw_sensor_value(manager));
		env.mapFunction("get-boolean-sensor-value", get_boolean_sensor_value(manager));
		env.mapFunction("get-motor-tach", get_motor_tach(manager));
		env.mapFunction("reset-motor-tach", reset_motor_tach(manager));
		env.mapFunction("keep-nxt-alive", keep_nxt_alive(manager));
		
		env.mapFunction("get-battery-millivolts", get_battery_millivolts(manager));
	}
	
	private static SimpleFunctionTemplate get_battery_millivolts(final NXTBluetoothManager manager)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) 
			{
				try
				{
					NXTBluetoothInterface binterface = (NXTBluetoothInterface)evaluatedArgs[0].getObjectValue();
					
					return NLispTools.makeValue(binterface.getBatteryLevelMilliVolts());
				}
				catch (Exception e)
				{
					AppStateManager.getInstance().onError("lisp:get-battery-millivolts", e);
				}
				
				return NLispTools.makeValue(false);
				
			}
			
		};
	}
	
	
	private static SimpleFunctionTemplate keep_nxt_alive(final NXTBluetoothManager manager)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) 
			{
				try
				{
					NXTBluetoothInterface binterface = (NXTBluetoothInterface)evaluatedArgs[0].getObjectValue();
					
					return NLispTools.makeValue(binterface.keepAlive());
				}
				catch (Exception e)
				{
					AppStateManager.getInstance().onError("lisp:keep-nxt-alive", e);
				}
				
				return NLispTools.makeValue(false);
				
			}
			
		};
	}
	
	
	private static SimpleFunctionTemplate reset_motor_tach(final NXTBluetoothManager manager)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) 
			{
				try
				{
					NXTBluetoothInterface binterface = (NXTBluetoothInterface)evaluatedArgs[0].getObjectValue();
					int port = (int)evaluatedArgs[1].getIntValue();
					NXTMessage.TachRelation relation = NXTMessage.TachRelation.ABSOLUTE;
					
					if (evaluatedArgs.length>2)
					{
						int value = (int)evaluatedArgs[2].getIntValue();
						if (value == 1)
							relation = NXTMessage.TachRelation.RELATIVE_TO_LAST_MOVEMENT;
						else if (value == 0)
							relation = NXTMessage.TachRelation.ABSOLUTE;
					}
					
					return NLispTools.makeValue(binterface.resetMotorTach(port, relation));
				}
				catch (Exception e)
				{
					AppStateManager.getInstance().onError("lisp:reset-motor-tach", e);
				}
				
				return NLispTools.makeValue(false);
				
			}
			
		};
	}
	
	private static SimpleFunctionTemplate get_motor_tach(final NXTBluetoothManager manager)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) 
			{
				try
				{
					NXTBluetoothInterface binterface = (NXTBluetoothInterface)evaluatedArgs[0].getObjectValue();
					int port = (int)evaluatedArgs[1].getIntValue();
					
					return NLispTools.makeValue(binterface.getMotorTach(port));
				}
				catch (Exception e)
				{
					AppStateManager.getInstance().onError("lisp:get-motor-tach", e);
				}
				
				return NLispTools.makeValue(false);
				
			}
			
		};
	}
	
	
	private static SimpleFunctionTemplate get_boolean_sensor_value(final NXTBluetoothManager manager)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) 
			{
				try
				{
					NXTBluetoothInterface binterface = (NXTBluetoothInterface)evaluatedArgs[0].getObjectValue();
					int port = (int)evaluatedArgs[1].getIntValue();
					
					return NLispTools.makeValue(binterface.getBooleanSensorValue(port));
				}
				catch (Exception e)
				{
					AppStateManager.getInstance().onError("lisp:get-boolean-sensor-value", e);
				}
				
				return NLispTools.makeValue(false);
				
			}
			
		};
	}
	
	
	
	private static SimpleFunctionTemplate get_raw_sensor_value(final NXTBluetoothManager manager)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) 
			{
				try
				{
					NXTBluetoothInterface binterface = (NXTBluetoothInterface)evaluatedArgs[0].getObjectValue();
					int port = (int)evaluatedArgs[1].getIntValue();
					
					return NLispTools.makeValue(binterface.getSensorRawValue(port));
				}
				catch (Exception e)
				{
					AppStateManager.getInstance().onError("lisp:get-sensor-raw-value", e);
				}
				
				return NLispTools.makeValue(false);
				
			}
			
		};
	}
	
	private static SimpleFunctionTemplate configure_sensor_port(final NXTBluetoothManager manager)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) 
			{
				try
				{
					NXTBluetoothInterface binterface = (NXTBluetoothInterface)evaluatedArgs[0].getObjectValue();
					int port = (int)evaluatedArgs[1].getIntValue();
					int type = (int)evaluatedArgs[2].getIntValue();
					
					return NLispTools.makeValue(binterface.configureSensorPort(port, type));
				}
				catch (Exception e)
				{
					AppStateManager.getInstance().onError("lisp:configure-sensor-port", e);
				}
				
				return NLispTools.makeValue(false);
				
			}
			
		};
	}
	
	
	private static SimpleFunctionTemplate set_motor_power(final NXTBluetoothManager manager)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) 
			{
				try
				{
					NXTBluetoothInterface binterface = (NXTBluetoothInterface)evaluatedArgs[0].getObjectValue();
					int port = (int)evaluatedArgs[1].getIntValue();
					int power = (int)evaluatedArgs[2].getIntValue();
					
					return NLispTools.makeValue(binterface.setMotorPower(port, Math.max(-100, Math.min(100, power))));
				}
				catch (Exception e)
				{
					AppStateManager.getInstance().onError("lisp:set-motor-power", e);
				}
				
				return NLispTools.makeValue(false);
				
			}
			
		};
	}
	
	
	
	private static SimpleFunctionTemplate disconnect_from_device(final NXTBluetoothManager manager)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) 
			{
				try
				{
					NXTBluetoothInterface binterface = (NXTBluetoothInterface)evaluatedArgs[0].getObjectValue();
					return NLispTools.makeValue(binterface.disconnect());
				}
				catch (Exception e)
				{
					AppStateManager.getInstance().onError("lisp:disconnect", e);
				}
				
				return NLispTools.makeValue(false);
				
			}
			
		};
	}
	
	
	private static SimpleFunctionTemplate connect_to_device(final NXTBluetoothManager manager)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) 
			{
				try
				{
					NXTBluetoothInterface binterface = (NXTBluetoothInterface)evaluatedArgs[0].getObjectValue();
					return NLispTools.makeValue(binterface.connect());
				}
				catch (Exception e)
				{
					AppStateManager.getInstance().onError("lisp:connect", e);
				}
				
				return NLispTools.makeValue(false);
				
			}
			
		};
	}
	
	
	private static SimpleFunctionTemplate start_nxt_service(final NXTBluetoothManager manager)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) 
			{
				return NLispTools.makeValue(manager.startNXTBluetoothService());
			}
			
		};
	}
	
	private static SimpleFunctionTemplate stop_nxt_service(final NXTBluetoothManager manager)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) 
			{
				return NLispTools.makeValue(manager.startNXTBluetoothService());
			}
			
		};
	}
	
	
	private static SimpleFunctionTemplate request_bluetooth_adatper_enable(final NXTBluetoothManager manager)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) 
			{
				return NLispTools.makeValue(manager.requestEnableBluetoothAdapter());
			}
			
		};
	}
	
	private static SimpleFunctionTemplate bluetooth_adapter_on_p(final NXTBluetoothManager manager)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) 
			{
				NXTServiceInterface sinterface = NXTBluetoothService.getServiceInterface();
				if (sinterface != null)
					return NLispTools.makeValue(sinterface.bluetoothAdapterOnP());
				else
					return NLispTools.makeValue(false);
			}
			
		};
	}
	
	
	private static SimpleFunctionTemplate nxt_service_running_p(final NXTBluetoothManager manager)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) 
			{
				NXTServiceInterface sinterface = NXTBluetoothService.getServiceInterface();
				if (sinterface != null)
					return NLispTools.makeValue(sinterface.serviceRunningP());
				else
					return NLispTools.makeValue(false);
			}
			
		};
	}
	
	
	private static SimpleFunctionTemplate get_paired_devices(final NXTBluetoothManager manager)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) 
			{
				try
				{
					LinkedList<NXTBluetoothInterface> devices = manager.getPairedDevices();
					Value[] dlist = new Value[devices.size()];
					int i=0;
					for (NXTBluetoothInterface device:devices)
					{
						dlist[i] = ExtendedFunctions.makeValue(device);
						i++;
					}
					return NLispTools.makeValue(dlist);
				}
				catch (BluetoothUnavailableException bue)
				{
					
				}
				
				return null;
			}
			
		};
	}
	
	private static SimpleFunctionTemplate get_device_name(final NXTBluetoothManager manager)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) 
			{
				NXTBluetoothInterface ninterface = (NXTBluetoothInterface)evaluatedArgs[0].getObjectValue();
				
				return NLispTools.makeValue(ninterface.getDeviceName());
			}
			
		};
	}
	
	private static SimpleFunctionTemplate get_device_mac_address(final NXTBluetoothManager manager)
	{
		return new SimpleFunctionTemplate()
		{

			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) 
			{
				NXTBluetoothInterface ninterface = (NXTBluetoothInterface)evaluatedArgs[0].getObjectValue();
				
				return NLispTools.makeValue(ninterface.getMacAddress());
			}
			
		};
	}
	
}