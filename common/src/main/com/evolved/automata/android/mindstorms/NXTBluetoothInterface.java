package com.evolved.automata.android.mindstorms;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.UUID;

import com.evolved.automata.android.AppStateManager;

import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothSocket;
import android.content.Context;
import android.util.Log;

public class NXTBluetoothInterface implements NXTBluetoothService.BluetoothStatusListener 
{
	
	private class State
	{
		Exception missingHandlerException;
		
		public State()
		{
			
		}
		
		public State(Exception onMissingHandlerException)
		{
			
			missingHandlerException = onMissingHandlerException;
		}
		
		public boolean connect() throws Exception
		{
			throw missingHandlerException;
			
		}
		
		public boolean disconnect() throws Exception
		{
			throw missingHandlerException;
		}
		
		
		public boolean keepNXTAlive() throws Exception
		{
			throw missingHandlerException;
		}

		public int getSensorRawValue(int port) throws Exception
		{
			throw missingHandlerException;
		}
		
		public boolean getBooleanSensorValue(int port) throws Exception
		{
			throw missingHandlerException;
		}
		
		public boolean setMotorPower(int port, int speed) throws Exception
		{
			throw missingHandlerException;
		}
		
		public int getBatteryLevelMilliVolts() throws Exception
		{
			throw missingHandlerException;
		}
		
		public int getMotorTach(int port) throws Exception
		{
			throw missingHandlerException;
		}
		
		public boolean configureSensorPort(int port, int type) throws Exception
		{
			throw missingHandlerException;
		}
		
		public boolean resetMotorTach(int port, NXTMessage.TachRelation relation) throws Exception
		{
			throw missingHandlerException;
		}
		
	}
	
	
	public abstract class Sensor {

		int _port = 0;

		public Sensor(int port) {
			_port = port;
		}

		public abstract boolean configurePort();

		public abstract boolean getBooleanValue() throws IOException;

		public abstract int getRawValue() throws IOException;

		public int getPort() {
			return _port;
		}

	}
	
	public class Switch extends Sensor {
		private final int ON_THRESHOLD = 200;

		Switch(int port) {
			super(port);
		}

		public boolean getBooleanValue() throws IOException {
			int rawValue = getRawValue();
			return rawValue >= ON_THRESHOLD;
		}

		public boolean configurePort() {
			try {
				sendBTMessage(NXTMessage.getSwitchPortConfigureCommand(_port,
						false));
				if (NXTBluetoothManager._debugMode)
				{
					byte[] response = readBTMessage();
					logResponse(response);
					return responseStatusOkay(response);
				}
				return true;
			} catch (IOException ie) {
				AppStateManager.getInstance().onError(
						"NXTBluetoothInterface:" + _deviceName
								+ ":Switch.configurePort", ie);
				return false;
			}
		}

		@Override
		public int getRawValue() throws IOException {
			return getRawSensorValue(_port);
		}
	}

	public class Sonar extends Sensor {
		private final int CLOSE_THRESHOLD = 200;

		Sonar(int port) {
			super(port);
		}

		public boolean getBooleanValue() throws IOException {
			int rawValue = getRawValue();
			return rawValue <= CLOSE_THRESHOLD;
		}

		public boolean configurePort() {
			try {
				sendBTMessage(NXTMessage.getSwitchPortConfigureCommand(_port,
						false));
				if (NXTBluetoothManager._debugMode)
				{
					byte[] response = readBTMessage();
					logResponse(response);
					return responseStatusOkay(response);
				}
				
				return true;
			} catch (IOException ie) {
				AppStateManager.getInstance().onError(
						"NXTBluetoothInterface:" + _deviceName
								+ ":Sonar.configurePort", ie);
				return false;
			}
		}

		@Override
		public int getRawValue() throws IOException {
			return getRawSensorValue(_port);
		}
	}
	
	
	public static final int _SENSOR_TYPE_SWITCH = 0;
	public static final int _SENSOR_TYPE_SONAR = 1;

	public String _macAddress;
	public String _deviceName;
	BluetoothSocket _socket;
	BluetoothDevice _device;
	Context _context;

	private static UUID _defaultBluetoothSerialDeviceUUID = UUID.fromString("00001101-0000-1000-8000-00805F9B34FB");
	
	InputStream _istream = null;
	OutputStream _ostream = null;
	HashMap<Integer, Sensor> _sensorMap = new HashMap<Integer, Sensor>();
	State _currentState = null;
	

	public NXTBluetoothInterface(Context con, BluetoothDevice device) {
		_device = device;
		_context = con;
		_deviceName = _device.getName();
		_macAddress = _device.getAddress();
		_currentState = getBluetoothServiceStoppedState();
		
	}

	

	public String getDeviceName() {
		return _deviceName;
	}

	public String getMacAddress() {
		return _macAddress;
	}

	@Override
	public void onStatusChange(int adapterStatus) {
		switch (adapterStatus) {
		case BluetoothAdapter.STATE_ON:
			_currentState = getBluetoothEnabledState();
			break;
		case BluetoothAdapter.STATE_TURNING_ON:

			break;
		case BluetoothAdapter.STATE_OFF:
			_currentState = getBluetoothDisabledState();
			break;
		case BluetoothAdapter.STATE_TURNING_OFF:

			break;
		}
	}

	@Override
	public void onConnectionChange(int connectionState) {
		switch (connectionState) {
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

	@Override
	public void onServiceStatusChanged(int status) {
		switch (status) {
		case NXTBluetoothService._STOPPED:
			_currentState = getBluetoothServiceStoppedState();
			break;
		case NXTBluetoothService._STARTED:
			_currentState = getBluetoothServiceStartedState();
			break;
		}
	}

	// -+] [+- -+] [+- -+] [+- -+] [+- -+] [+- -+] [+- -+] [+- -+] [+- -+] [+- 
	//						Interface Functions
	// -+] [+- -+] [+- -+] [+- -+] [+- -+] [+- -+] [+- -+] [+- -+] [+- -+] [+-
	
	public boolean connect() throws Exception
	{
		return _currentState.connect();
	}
	
	public boolean disconnect() throws Exception
	{
		
		return _currentState.disconnect();
		
	}
	
	public boolean configureSensorPort(int port, int sensorType) throws Exception
	{
		return _currentState.configureSensorPort(port, sensorType);
	}
	
	public int getSensorRawValue(int port) throws Exception
	{
		return _currentState.getSensorRawValue(port);
	}
	
	public boolean getBooleanSensorValue(int port) throws Exception
	{
		return _currentState.getBooleanSensorValue(port);
		
	}
	
	
	public int getMotorTach(int port) throws Exception
	{
		return _currentState.getMotorTach(port);
	}
	
	public int getBatteryLevelMilliVolts() throws Exception
	{
		return _currentState.getBatteryLevelMilliVolts();
	}
	
	public boolean setMotorPower(int port, int speed) throws Exception
	{
		return _currentState.setMotorPower(port, speed);
	}
	
	public boolean keepAlive() throws Exception
	{
		return _currentState.keepNXTAlive();
	}
	
	public boolean resetMotorTach(int port, NXTMessage.TachRelation relation) throws Exception
	{
		return _currentState.resetMotorTach(port, relation);
	}
	
	// *~=[o_o]=~* *~=[o_o]=~* *~=[o_o]=~* *~=[o_o]=~* *~=[o_o]=~* *~=[o_o]=~* *~=[o_o]=~* 
	//						State Methods
	// *~=[o_o]=~* *~=[o_o]=~* *~=[o_o]=~* *~=[o_o]=~* *~=[o_o]=~* *~=[o_o]=~* *~=[o_o]=~*
	
	private State getBluetoothEnabledState()
	{
		return new State(new NotConnectedToNXTException("Connect to:" + _deviceName));
	}
	
	private State getBluetoothDisabledState()
	{
		return new State(new BluetoothDisabledException("Start bluetooth adapter"));
	}
	
	private State getBluetoothServiceStoppedState()
	{
		return new State(new BluetoothServiceNotRunningException("Start bluetooth service"));
	}
	
	private State getBluetoothServiceStartedState()
	{
		return new State(new NotConnectedToNXTException("Connect to:" + _deviceName));
	}
	
	
	
	private State getConnectedToDeviceHandler(){
		return new State(){
	
		@Override
		public boolean connect() {
			return true;
		}

		@Override
		public boolean disconnect() {
			try {
				_istream.close();
				_ostream.close();
				_socket.close();
				return true;

			} catch (Exception e) {
				AppStateManager.getInstance().onError(
						"NXTBluetoothInterface:" + _deviceName, e);
			}
			finally
			{
				_currentState = getDisconnectedToDeviceHandler(); 
			}
			return false;
		}

		@Override
		public boolean configureSensorPort(int port, int sensorType) {
			Sensor sensor = null;
			boolean out;
			switch (sensorType) {
			case _SENSOR_TYPE_SWITCH:
				sensor = new Switch(port);
				out = sensor.configurePort();
				if (out)
					_sensorMap.put(Integer.valueOf(sensorType), sensor);
				return out;
			case _SENSOR_TYPE_SONAR:
				sensor = new Sonar(port);
				out = sensor.configurePort();
				if (out)
					_sensorMap.put(Integer.valueOf(sensorType), sensor);
				return out;
			}
			AppStateManager.getInstance().onEvent("UNSUPPORTED_SENSOR_TYPE",
					"port", "" + port, "sensorType", "" + sensorType);
			return false;
		}

		@Override
		public int getSensorRawValue(int port) throws IOException {
			Sensor sensor = _sensorMap.get(Integer.valueOf(port));
			return sensor.getRawValue();

		}

		@Override
		public boolean getBooleanSensorValue(int port) throws IOException {
			Sensor sensor = _sensorMap.get(Integer.valueOf(port));
			return sensor.getBooleanValue();

		}

		@Override
		public int getMotorTach(int port) throws IOException {
			byte[] command = NXTMessage.getOutputStateMessage(port);
			byte[] response = null;
			try {
				sendBTMessage(command);
				response = readBTMessage();
				if (NXTBluetoothManager._debugMode)
				{
					logResponse(response);
					
				}
				
				int position = NXTMessage.byteToInt(response[21])
						+ (NXTMessage.byteToInt(response[22]) << 8)
						+ (NXTMessage.byteToInt(response[23]) << 16)
						+ (NXTMessage.byteToInt(response[24]) << 24);
				return position;
			} catch (IOException ie) {
				AppStateManager.getInstance().onError(
						"NXTBluetoothInterface:" + _deviceName
								+ ":getMotorTach", ie);
				throw ie;
			}

		}

		@Override
		public int getBatteryLevelMilliVolts() throws IOException {
			byte[] command = NXTMessage.getBatteryLevelMessage();
			byte[] response = null;
			try {
				sendBTMessage(command);
				response = readBTMessage();
				if (NXTBluetoothManager._debugMode)
				{
					logResponse(response);
					
				}
				return NXTMessage.convertUnsignedByteArrayToInt(response,
						NXTMessage._IDX_GET_BATTERY_LEVEL_MILLIVOLTS_LSB);
			} catch (IOException ie) {
				AppStateManager.getInstance().onError(
						"NXTBluetoothInterface:" + _deviceName
								+ ":getBatteryLevelMilliVolts", ie);
				throw ie;
			}
		}

		@Override
		public boolean setMotorPower(int port, int speed) {
			
			byte[] command = NXTMessage.getMotorMessage(NXTBluetoothManager._debugMode, port, speed);
			try {
				sendBTMessage(command);
				
				if (NXTBluetoothManager._debugMode)
				{
					byte[] response = readBTMessage();
					logResponse(response);
					return responseStatusOkay(response);
				}
				
				return true;
			} catch (IOException ie) {
				AppStateManager.getInstance().onError(
						"NXTBluetoothInterface:" + _deviceName
								+ ":setMotorPower", ie);
				return false;
			}
		}

		@Override
		public boolean keepNXTAlive() {
			byte[] command = NXTMessage.getKeepAliveMessage(NXTBluetoothManager._debugMode);
			try {
				sendBTMessage(command);
				if (NXTBluetoothManager._debugMode)
				{
					byte[] response = readBTMessage();
					logResponse(response);
					return responseStatusOkay(response);
				}
				return true;
			} catch (IOException ie) {
				AppStateManager.getInstance().onError(
						"NXTBluetoothInterface:" + _deviceName + ":keepAlive",
						ie);
				return false;
			}
		}
		
		public boolean resetMotorTach(int port, NXTMessage.TachRelation relation) throws Exception
		{
			byte[] command = NXTMessage.getResetMotorTachMessage(NXTBluetoothManager._debugMode, port, relation);
			sendBTMessage(command);
			if (NXTBluetoothManager._debugMode)
			{
				byte[] response = readBTMessage();
				logResponse(response);
				return responseStatusOkay(response);
			}
			
			return true;
		}};
	}

	private State getDisconnectedToDeviceHandler()
	{
		return new State(){
		
		@Override
		public boolean connect() {
			try {
				_socket = _device
						.createRfcommSocketToServiceRecord(_defaultBluetoothSerialDeviceUUID);
				_socket.connect();
				_istream = _socket.getInputStream();
				_ostream = _socket.getOutputStream();
				_currentState = getConnectedToDeviceHandler();
				return true;
			} catch (IOException e) {
				AppStateManager.getInstance().onError(
						"NXTBluetoothInterface:" + _deviceName, e);

				return false;
			}

		}
		};
	}
	
	
	
	// <(o+--- ---+o)> <(o+--- ---+o)> <(o+--- ---+o)> <(o+--- ---+o)> <(o+--- ---+o)> 
	//									Helper Methods
	// <(o+--- ---+o)> <(o+--- ---+o)> <(o+--- ---+o)> <(o+--- ---+o)> <(o+--- ---+o)>
	
	/**
	 * Sends a message on the opened OutputStream
	 * 
	 * @param message
	 *            , the message as a byte array
	 */
	private void sendBTMessage(byte[] message) throws IOException {

		// send message length
		int messageLength = message.length;
		_ostream.write(messageLength);
		_ostream.write(messageLength >> 8);
		_ostream.write(message, 0, message.length);
	}

	/**
	 * Receives a message on the opened InputStream
	 * 
	 * @return the message
	 */
	private byte[] readBTMessage() throws IOException {

		int length = _istream.read();
		length = (_istream.read() << 8) + length;
		byte[] returnMessage = new byte[length];
		_istream.read(returnMessage);
		return returnMessage;
	}

	private void logResponse(byte[] response)
	{
		String statusDesc = NXTMessage._STATUS_DESC_MAP.get(Byte.valueOf(response[NXTMessage._IDX_STATUS_GENERAL]));
		String commandTypeDesc = NXTMessage._COMMAND_DESC_MAP.get(Byte.valueOf(response[NXTMessage._IDX_COMMAND_TYPE_GENERAL]));
		
		AppStateManager.getInstance().onEvent("DIRECT-COMMAND", "command_type", commandTypeDesc, "response_status", statusDesc);
		
	}
	

	private int getRawSensorValue(int port) throws IOException {
		byte[] command = new byte[] { NXTMessage.DIRECT_COMMAND_REPLY,
				NXTMessage.GET_INPUT_VALUES, (byte) port };
		sendBTMessage(command);
		byte[] response = readBTMessage();
		if (NXTBluetoothManager._debugMode)
		{
			logResponse(response);
			
		}
		return getRawSensorValue(response);
	}

	private int getRawSensorValue(byte[] response) throws IOException {
		return NXTMessage.convertUnsignedByteArrayToInt(response,
				NXTMessage._IDX_GET_INPUT_VALUES_VALUE_LSB);
	}

	private int getNormalizedSensorValue(int port) throws IOException {
		byte[] command = new byte[] { NXTMessage.DIRECT_COMMAND_REPLY,
				NXTMessage.GET_INPUT_VALUES, (byte) port };
		sendBTMessage(command);
		byte[] response = readBTMessage();
		if (NXTBluetoothManager._debugMode)
		{
			logResponse(response);
			
		}
		return getNormalizedSensorValue(response);
	}

	private int getNormalizedSensorValue(byte[] response) throws IOException {
		return NXTMessage.convertUnsignedByteArrayToInt(response,
				NXTMessage._IDX_GET_INPUT_VALUES_NORMALIZED_VALUE_LSB);
	}

	private int getScaledSensorValue(int port) throws IOException {
		byte[] command = new byte[] { NXTMessage.DIRECT_COMMAND_REPLY,
				NXTMessage.GET_INPUT_VALUES, (byte) port };
		sendBTMessage(command);
		byte[] response = readBTMessage();
		if (NXTBluetoothManager._debugMode)
		{
			logResponse(response);
			
		}
		return getScaledSensorValue(response);
	}

	private int getScaledSensorValue(byte[] response) throws IOException {

		return NXTMessage.convertUnsignedByteArrayToInt(response,
				NXTMessage._IDX_GET_INPUT_VALUES_SCALED_VALUE_LSB);
	}

	private int getCalibratedSensorValue(int port) throws IOException {
		byte[] command = new byte[] { NXTMessage.DIRECT_COMMAND_REPLY,
				NXTMessage.GET_INPUT_VALUES, (byte) port };
		sendBTMessage(command);
		byte[] response = readBTMessage();
		if (NXTBluetoothManager._debugMode)
		{
			logResponse(response);
			
		}
		return getCalibratedSensorValue(response);
	}

	private int getCalibratedSensorValue(byte[] response) throws IOException {
		return NXTMessage.convertUnsignedByteArrayToInt(response,
				NXTMessage._IDX_GET_INPUT_VALUES_CALIBRATED_VALUE_LSB);
	}

	

	/*
	 * Over-ride standard get function to ensure correct inter-command timing
	 * when using the ultrasonic sensor. The Lego Ultrasonic sensor uses a
	 * "bit-banged" i2c interface and seems to require a minimum delay between
	 * commands otherwise the commands fail.
	 */
	// 1.2
	private int getData(int register, byte[] buf, int len, byte port) {

		waitSomeTime(NXTMessage._COMMAND_DELAY_MILLI);

		byte[] txData = { NXTMessage._I2C_ADDRESS, (byte) register };
		try {
			LSWrite(port, txData, (byte) len);
		} catch (IOException ioe) {
			throw new RuntimeException(ioe.toString());
		}

		byte[] status = null;
		do {
			try {
				status = LSGetStatus(port);
			} catch (IOException ioe) {
				return -1;
			}
		} while (status[0] == NXTMessage.PENDING_COMMUNICATION_TRANSACTION_IN_PROGRESS
				| status[0] == NXTMessage.SPECIFIED_CHANNEL_CONNECTION_NOT_CONFIGURED_OR_BUSY);

		try {
			byte[] ret = LSRead(port);
			if (ret != null)
				System.arraycopy(ret, 0, buf, 0, ret.length);
		} catch (IOException ioe) {
			return -1;
		}

		return status[0];
	}

	/**
	 * Reads data from an Inter-Integrated Circuit (I2C) sensor (the ultrasound
	 * sensor) via the Low Speed (LS) data port. The port must first be
	 * configured to type LOWSPEED or LOWSPEED_9V. Data lengths are limited to
	 * 16 bytes per command. The response will also contain 16 bytes, with
	 * invalid data padded with zeros.
	 * 
	 * @param port
	 * @return the response
	 */
	// 1.2.1.3
	private byte[] LSRead(byte port) throws IOException {
		byte[] request = { NXTMessage.DIRECT_COMMAND_REPLY, NXTMessage.LS_READ,
				port };
		byte[] reply = null;
		sendBTMessage(request);

		reply = readBTMessage();

		byte rxLength = reply[3];
		if (reply[2] == 0 && rxLength >= 0) {
			byte[] rxData = new byte[rxLength];
			System.arraycopy(reply, 4, rxData, 0, rxLength);
			return rxData;
		} else {
			return null;
		}
	}

	/**
	 * Used to request data from an Inter-Integrated Circuit (I2C) sensor (the
	 * ultrasound sensor) via the Low Speed (LS) data port. The port must first
	 * be configured to type LOWSPEED or LOWSPEED_9V. Data lengths are limited
	 * to 16 bytes per command. Rx (receive) Data Length MUST be specified in
	 * the write command since reading from the device is done on a master-slave
	 * basis.
	 * 
	 * @param txData
	 *            Transmitted data.
	 * @param rxDataLength
	 *            Receive data length.
	 * @param port
	 *            0-3
	 * @return the status (0 = success)
	 */
	// 1.2.1.1
	private byte LSWrite(byte port, byte[] txData, byte rxDataLength)
			throws IOException {
		byte[] request = { NXTMessage.DIRECT_COMMAND_NOREPLY,
				NXTMessage.LS_WRITE, port, (byte) txData.length, rxDataLength };
		request = NXTMessage.appendBytes(request, txData);
		// Use Mindroid version of sendRequest
		sendBTMessage(request);
		return 1;
	}

	/**
	 * Returns the status for an Inter-Integrated Circuit (I2C) sensor (the
	 * ultrasound sensor) via the Low Speed (LS) data port. The port must first
	 * be configured to type LOWSPEED or LOWSPEED_9V.
	 * 
	 * @param port
	 *            0-3
	 * @return byte[0] = status, byte[1] = Bytes Ready (count of available bytes
	 *         to read)
	 */
	// 1.2.1.2
	private byte[] LSGetStatus(byte port) throws IOException {
		byte[] request = { NXTMessage.DIRECT_COMMAND_REPLY,
				NXTMessage.LS_GET_STATUS, port };
		sendBTMessage(request);

		byte[] reply = readBTMessage();
		byte[] returnData = { reply[2], reply[3] };
		return returnData;
	}

	private void waitSomeTime(int millis) {
		try {
			Thread.sleep(millis);

		} catch (InterruptedException e) {
		}
	}
	
	private boolean responseStatusOkay(byte[] response)
	{
		return response[NXTMessage._IDX_STATUS_GENERAL] == NXTMessage.SUCCESS;
	}

}
