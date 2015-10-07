package com.evolved.automata.android.mindstorms;

import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothSocket;
import android.content.res.Resources;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.util.Log;
import android.util.Pair;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.util.*;
import java.util.concurrent.*;



/**
 * This class is for talking to a LEGO NXT robot via bluetooth.
 * The communciation to the robot is done via LCP (LEGO communication protocol).
 * Objects of this class can either be run as standalone thread or controlled
 * by the owners, i.e. calling the send/recive methods by themselves.
 */
public class NewBTCommunicator 
{
	int mRobotType;
    int motorLeft;
    private int directionLeft=1; // +/- 1
    int motorRight;
    private int directionRight=1; // +/- 1
    private int motorAction;
    private int directionAction; // +/- 1
    private List<String> programList;
    private static final int MAX_PROGRAMS = 20;
    private String programToStart;
    private boolean collision = false;
    private byte[] rawSensorValue = null;
    private int distance=0;
    
    public static final int MOTOR_A = 0;
    public static final int MOTOR_B = 1;
    public static final int MOTOR_C = 2;
    public static final int S1 = 0;
	public static final int S2 = 1;
	public static final int S3 = 2;
	public static final int S4 = 3;
    public static final int MOTOR_B_ACTION = 40;
    public static final int MOTOR_A_ACTION = 41;
    public static final int MOTOR_C_ACTION = 42;
    public static final int MOTOR_RESET = 10;
    public static final int DO_BEEP = 51;
    public static final int DO_ACTION = 52;    
    public static final int READ_MOTOR_STATE = 60;
    public static final int GET_FIRMWARE_VERSION = 70;
    public static final int DISCONNECT = 99;

    public static final int DISPLAY_TOAST = 1000;
    public static final int STATE_CONNECTED = 1001;
    public static final int STATE_CONNECTERROR = 1002;
    public static final int STATE_CONNECTERROR_PAIRING = 1022;
    public static final int MOTOR_STATE = 1003;
    public static final int STATE_RECEIVEERROR = 1004;
    public static final int STATE_SENDERROR = 1005;
    public static final int FIRMWARE_VERSION = 1006;
    public static final int FIND_FILES = 1007;
    public static final int START_PROGRAM = 1008;
    public static final int STOP_PROGRAM = 1009;
    public static final int GET_PROGRAM_NAME = 1010;
    public static final int PROGRAM_NAME = 1011;
    public static final int SAY_TEXT = 1030;
    public static final int VIBRATE_PHONE = 1031;
    public static final int GET_ULTRASONIC = 1032;
    public static final int SLEEP = 1033;
    public static final int SET_S1_SENSOR_MODE = 1034;
    public static final int SET_S2_SENSOR_MODE = 1035;
    public static final int SET_S3_SENSOR_MODE = 1036;
    public static final int SET_S4_SENSOR_MODE = 1037;
    public static final int GET_RAW_SENSOR_VALUE=1038; // non-i2c
    public static final int UPDATE_CENTER=2038; 
	
    public static final int NO_DELAY = 0;

    private static final UUID SERIAL_PORT_SERVICE_CLASS_UUID = UUID.fromString("00001101-0000-1000-8000-00805F9B34FB");
    // this is the only OUI registered by LEGO, see http://standards.ieee.org/regauth/oui/index.shtml
    public static final String OUI_LEGO = "00:16:53";

    
    private BluetoothAdapter btAdapter;
    private BluetoothSocket nxtBTsocket = null;
    private OutputStream nxtOutputStream = null;
    private InputStream nxtInputStream = null;
    private boolean connected = false;

    //private Handler uiHandler;
    private String mMACaddress;
    private boolean sensorsConfigured=false;

    private byte[] returnMessage;
    private Thread bluetoothReceiverThread;
    private Thread messageThread;
    Handler myHandler;
    LinkedBlockingQueue<Integer> responseQueue;
    BluetoothResponseInterface _responseInterface;
    Hashtable<Integer,Integer> portToTypeMap;
    
    public static interface BluetoothResponseInterface
    {
    	public void onConnectionSuccess();
    	public void onNoBluetoothAdapter();
    	public void onBluetoothNotEnabled();
    	public void onConnectionError(String message);
    	public void onDisplayToast(String message);
    	public void onFirmwareVersion(int version);
    	public void onMotorState(int state);
    	public void onCommunicationError(String message);
    	public void onDestroyError(String message);
    	public void onDisconnect(boolean success);
    }
    
    public interface BluetoothRequestInterface
    {
    	public int leftMotorPosition();
    	public int rightMotorPosition();
    	public ArrayList<Pair<String, String>> getPairedDevices();
    	public void connectToDevice(String deviceAddress);
    	public boolean setMotorPower(int leftMotor, int rightMotor);
    	public int getUltrasonicData();
    	public int getBatteryMilli();
    	public int getTouch();
    	public int getLeftTouch();
    	public int getRightTouch();
    	public void disconnect();
    	public void resetLeftTach();
    	public void resetRightTach();
    	public void moveLeftMotorToTach(int speed, int targetTach);
    	public void moveRightMotorToTach(int speed, int targetTach);
    	
    }
    
    public class TimeoutRunnable implements Runnable
    {
    	int timeout;
    	int timeoutValue;
    	public TimeoutRunnable(int durationMilli, int tValue)
    	{
    		timeout=durationMilli;
    		timeoutValue=tValue;
    	}
    	
    	public void run()
    	{
    		try
    		{
    			Thread.sleep(timeout);
    			responseQueue.put(timeoutValue);
    		}
    		catch (Exception e)
    		{
    			
    		}
    	}
    }
    
    
    public NewBTCommunicator() 
    {
        this.btAdapter = BluetoothAdapter.getDefaultAdapter();
        setUpByType();
        responseQueue = new LinkedBlockingQueue<Integer>();
        portToTypeMap = new Hashtable<Integer, Integer>();
    }
    
    public BluetoothRequestInterface initialize(final BluetoothResponseInterface resp)
    {
    	_responseInterface = resp;
    	if (btAdapter == null)
    	{
    		Handler h = new Handler(Looper.getMainLooper());
    		h.post(new Runnable()
	    		{
	    			public void run()
	    			{
	    				resp.onNoBluetoothAdapter();
	    			}
	    		}
    		);
    	}
    	return new BluetoothRequestInterface()
    	{
    		public int leftMotorPosition()
        	{
        		return getLeftMotorPosition();
        	}
        	
        	public int rightMotorPosition()
        	{
        		return getRightMotorPosition();
        	}
        	
        	public ArrayList<Pair<String, String>> getPairedDevices()
        	{
        		ArrayList<Pair<String, String>> devices = new ArrayList<Pair<String, String>>();
        		Set<BluetoothDevice> paired = btAdapter.getBondedDevices();
        		for (BluetoothDevice d:paired)
        		{
        			devices.add(Pair.create(d.getName(), d.getAddress()));
        		}
        		return devices;
        	}
        	
        	public void connectToDevice(String deviceAddress)
        	{
        		connectToNXT(deviceAddress);
        	}
        	
        	public boolean setMotorPower(int leftMotor, int rightMotor)
        	{
        		if (connected)
        		{
        			updateMotorControl(leftMotor, rightMotor);
        			return true;
        		}
        		else
        			return false;
        	}
        	
        	public int getUltrasonicData()
        	{
        		return getDistanceValue();
        	}
        	
        	public int getBatteryMilli()
        	{
        		return getBatteryValue();
        	}
        	
        	public int getTouch()
        	{
        		return getTouchSensorValue();
        	}
        	
        	public void disconnect()
        	{
        		disconnectFromNXT();
        	}

			@Override
			public int getLeftTouch() {
				return getLeftTouchSensorValue();
			}

			@Override
			public int getRightTouch() {
				return getRightTouchSensorValue();
			}

			@Override
			public void resetLeftTach() {
				NewBTCommunicator.this.resetLeftTach();
			}

			@Override
			public void resetRightTach() {
				NewBTCommunicator.this.resetRightTach();
				
			}

			@Override
			public void moveLeftMotorToTach(int speed, int targetTach) {
				setLeftMotorTargetTach(speed, targetTach);
				
			}

			@Override
			public void moveRightMotorToTach(int speed, int targetTach) {
				setRightMotorTargetTach(speed, targetTach);
			}
    	};
    }
    
    
    public void connectToNXT(String macAddress)
    {
    	disconnectFromNXT(true);
    	
    	this.mMACaddress = macAddress;
    	connected=createNXTconnection();
    	
    	
    	messageThread = new Thread()
    	{
    		public void run()
    		{
    			Looper.prepare();
    			myHandler = new Handler(myCallback);
    			Looper.loop();
    			
    		}
    	};
    	
    	if (connected)
    	{
    		messageThread.start();
    		_responseInterface.onConnectionSuccess();
    		setSensorConfiguration();
    	}
    	
    	
    }
    
    public void disconnectFromNXT()
    {
    	boolean success = true;
    	if (connected)
    	{
    		updateMotorControl(0, 0);
    		success = destroyNXTconnection();
    	}
    	if ((bluetoothReceiverThread!=null)&&(bluetoothReceiverThread.isAlive()))
    		bluetoothReceiverThread.interrupt();
    	if ((messageThread!=null)&&(messageThread.isAlive()))
    		messageThread.interrupt();
    	myHandler=null;
    	if (_responseInterface!=null)
    		_responseInterface.onDisconnect(success);
    	
    }
    
    public void disconnectFromNXT(boolean suppressUpdate)
    {
    	boolean success = true;
    	if (connected)
    	{
    		updateMotorControl(0, 0);
    		success = destroyNXTconnection();
    	}
    	if ((bluetoothReceiverThread!=null)&&(bluetoothReceiverThread.isAlive()))
    		bluetoothReceiverThread.interrupt();
    	if ((messageThread!=null)&&(messageThread.isAlive()))
    		messageThread.interrupt();
    	myHandler=null;
    	if (_responseInterface!=null && !suppressUpdate)
    		_responseInterface.onDisconnect(success);
    	
    }
    
    /**
     * Create a bluetooth connection with SerialPortServiceClass_UUID
     * @see <a href=
     *      "http://lejos.sourceforge.net/forum/viewtopic.php?t=1991&highlight=android"
     *      />
     * On error the method either sends a message to it's owner or creates an exception in the
     * case of no message handler.
     */
    // this must be called externally
    public boolean createNXTconnection() 
    {
    	boolean isConnected=false;
        try 
        {
            BluetoothSocket nxtBTSocketTemporary;
            BluetoothDevice nxtDevice = null;
            nxtDevice = btAdapter.getRemoteDevice(mMACaddress);
            
            nxtBTSocketTemporary = nxtDevice.createRfcommSocketToServiceRecord(SERIAL_PORT_SERVICE_CLASS_UUID);
            try {
                nxtBTSocketTemporary.connect();
            }
            catch (IOException e) 
            {  
                
            	
                // try another method for connection, this should work on the HTC desire, credits to Michael Biermann
                try 
                {
                    Method mMethod = nxtDevice.getClass().getMethod("createRfcommSocket", new Class[] { int.class });
                    nxtBTSocketTemporary = (BluetoothSocket) mMethod.invoke(nxtDevice, Integer.valueOf(1));            
                    nxtBTSocketTemporary.connect();
                }
                catch (Exception e1)
                {
                    _responseInterface.onConnectionError(e1.toString());
                    return isConnected;
                }
            }
            nxtBTsocket = nxtBTSocketTemporary;
            nxtInputStream = nxtBTsocket.getInputStream();
            nxtOutputStream = nxtBTsocket.getOutputStream();
            isConnected = true;
        } 
        catch (IOException e) 
        {
        	_responseInterface.onConnectionError(e.toString());
            return isConnected;
        }
        // everything was OK
        return isConnected;
    }
    
    // receive messages from the UI
    /* All of the code below results in writes to NXT.  The response to these data requests are in the 
     * dispatchMessage() method
     * 
     */
    final Handler.Callback myCallback= new Handler.Callback() {
        @Override
        public boolean handleMessage(Message myMessage) {
        	int port;
            int message;
            try
            {
            	if (connected)
            	{
		            switch (message = myMessage.getData().getInt("message")) 
		            {
		                case MOTOR_A:
		                case MOTOR_B:
		                case MOTOR_C:
		                    changeMotorSpeed(message, myMessage.getData().getInt("value1"));
		                    break;
		                case MOTOR_B_ACTION:
		                    rotateTo(MOTOR_B, myMessage.getData().getInt("value1"), myMessage.getData().getInt("value2"));
		                    break;
		                case MOTOR_A_ACTION:
		                    rotateTo(MOTOR_A, myMessage.getData().getInt("value1"), myMessage.getData().getInt("value2"));
		                    break;
		                case MOTOR_C_ACTION:
		                    rotateTo(MOTOR_C, myMessage.getData().getInt("value1"), myMessage.getData().getInt("value2"));
		                    break;
		                case MOTOR_RESET:
		                    reset(myMessage.getData().getInt("value1"));
		                    break;
		                case START_PROGRAM:
		                    startProgram(myMessage.getData().getString("name"));
		                    break;
		                case STOP_PROGRAM:
		                    stopProgram();
		                    break;
		                case GET_PROGRAM_NAME:
		                    getProgramName();
		                    break;    
		                case DO_BEEP:
		                    doBeep(myMessage.getData().getInt("value1"), myMessage.getData().getInt("value2"));
		                    break;
		                case DO_ACTION:
		                    doAction(0);
		                    break;
		                case READ_MOTOR_STATE:
		                	returnMessage=readMotorState(myMessage.getData().getInt("value1"));
		                	int position = byteToInt(returnMessage[21]) + (byteToInt(returnMessage[22]) << 8) + (byteToInt(returnMessage[23]) << 16)
                            + (byteToInt(returnMessage[24]) << 24);
		                	responseQueue.put(position);
		                    break;
		                case GET_FIRMWARE_VERSION:
		                    getFirmwareVersion();
		                    break;
		                case FIND_FILES:
		                    findFiles(myMessage.getData().getInt("value1") == 0, myMessage.getData().getInt("value2"));
		                    break;
		                case DISCONNECT:
		                    // send stop messages before closing
		                    changeMotorSpeed(MOTOR_A, 0);
		                    changeMotorSpeed(MOTOR_B, 0);
		                    changeMotorSpeed(MOTOR_C, 0);
		                    waitSomeTime(500);
		                    destroyNXTconnection();
		                    
		                    
		                    break;
		                case GET_ULTRASONIC:
		                	//TODO: Redone
		                	port = myMessage.getData().getInt("value1");
		                	int response =getDistance((byte)port);
		                	responseQueue.put(response);
		                	
		                	break;
		                case SLEEP:
		                	waitSomeTime(myMessage.getData().getInt("value1"));
		                	break;
		                case SET_S1_SENSOR_MODE:
		                	setInputState((byte)S1, (byte)myMessage.getData().getInt("value1"), (byte)myMessage.getData().getInt("value2"));
		                	break;
		                case SET_S2_SENSOR_MODE:
		                	setInputState((byte)S2, (byte)myMessage.getData().getInt("value1"), (byte)myMessage.getData().getInt("value2"));
		                	break;
		                case SET_S3_SENSOR_MODE:
		                	setInputState((byte)S3, (byte)myMessage.getData().getInt("value1"), (byte)myMessage.getData().getInt("value2"));
		                	break;
		                case SET_S4_SENSOR_MODE:
		                	setInputState((byte)S4, (byte)myMessage.getData().getInt("value1"), (byte)myMessage.getData().getInt("value2"));
		                	break;
		                case GET_RAW_SENSOR_VALUE:
		                	port = myMessage.getData().getInt("value1");
		                	returnMessage = getRawSensoryValues(port);
//		                	byte mode=returnMessage[7];
//		                	byte type=returnMessage[6];
//		                	
//		                	int sensoryType= portToTypeMap.get(port);
		                	Bundle b = new Bundle();
		                	int responseInt = convertUnsignedByteArrayToInt(returnMessage,10);
                			b.putInt("message", GET_RAW_SENSOR_VALUE);
                			b.putInt("response", responseInt);
                			responseQueue.put(responseInt);
//		                	switch (sensoryType)
//		                	{
//		                		case LIGHT_INACTIVE:
//		                			int responseInt=0;
//		                			// set highest order bit
//		                			responseInt = returnMessage[9] & 0xff;
//		                			responseInt = responseInt+ returnMessage[8] & 0xff;
//		                			b.putInt("message", GET_RAW_SENSOR_VALUE);
//		                			b.putInt("response", responseInt);
//		                			responseQueue.put(responseInt);
//		                			//sendBundle(b);
//		                			break;
//		                			
//		                	}
		                	
		                	//sendState(GET_RAW_SENSOR_VALUE);
		                	break;
		                case GET_BATTERY_LEVEL:
		                	int returnV=getBatteryMilli();
		                	responseQueue.put(returnV);
		                	break;
		                		
		                	
		            }
            	}
            }
            catch (Exception e)
            {
	        	 _responseInterface.onCommunicationError(e.toString());
            }
            return true;
        }
    };

    public Handler getHandler() {
        return myHandler;
    }

    public byte[] getReturnMessage() {
        return returnMessage;
    }


    public static int convertUnsignedByteArrayToInt(byte[] data, int start)
    {
    	int out;
    	byte lsb=data[start];
    	byte msb=data[start+1];
    	return (lsb & 0xFF) | ((msb & 0xFF) << 8);
    }

    /**
     * @return The current status of the connection
     */            
    public boolean isConnected() {
        return connected;
    }


    private int byteToInt(byte byteValue) {
        int intValue = (byteValue & (byte) 0x7f);

        if ((byteValue & (byte) 0x80) != 0)
            intValue |= 0x80;

        return intValue;
    }

    /**
     * Closes the bluetooth connection. On error the method either sends a message
     * to it's owner or creates an exception in the case of no message handler.
     */
    private boolean destroyNXTconnection() 
    {
        try 
        {
            if (nxtBTsocket != null) 
            {
                connected = false;
                nxtBTsocket.close();
                nxtBTsocket = null;
            }

            nxtInputStream = null;
            nxtOutputStream = null;
            return true;
        } 
        catch (IOException e) 
        {
        	if (_responseInterface!=null)
        		_responseInterface.onDestroyError(e.toString());
        	return false;
        }
    }

    private void setUpByType() {
        
        // default
        motorLeft = NewBTCommunicator.MOTOR_A;
        motorRight = NewBTCommunicator.MOTOR_C;
        motorAction = NewBTCommunicator.MOTOR_B;

    }
    
    /**
     * Sends the motor control values to the communcation thread.
     * @param left The power of the left motor from 0 to 100.
     * @param rigth The power of the right motor from 0 to 100.
     */   
    public void updateMotorControl(int left, int right) 
    {

        // send messages via the handler
        sendBTCmessage(NewBTCommunicator.NO_DELAY, motorLeft, left * directionLeft, 0);
        sendBTCmessage(NewBTCommunicator.NO_DELAY, motorRight, right * directionRight, 0);
        
    }
    
    private void setUltraSonicDistance()
    {
    	sendBTCmessage(NewBTCommunicator.NO_DELAY, NewBTCommunicator.GET_ULTRASONIC, S1, 0);
        
    }
    
	public int getDistanceValue()
	{
		
		setUltraSonicDistance();
		currentDistance=getResponseWithTimeout(1000,255);
		return currentDistance;
	}
	
	
    public void sendSleepMessage(int sleepTimeMilli)
    {
    	sendBTCmessage(NewBTCommunicator.NO_DELAY,NewBTCommunicator.SLEEP, sleepTimeMilli, 0);
        
    }

    public void setSensorConfiguration()
    {
    	if (!sensorsConfigured)
    	{
    		//this is the ultrasonic ranger
    		sendBTCmessage(NewBTCommunicator.NO_DELAY,NewBTCommunicator.SET_S1_SENSOR_MODE, NewBTCommunicator.LOWSPEED_9V, NewBTCommunicator.RAWMODE);
    		// Left switch switch
    		sendBTCmessage(NewBTCommunicator.NO_DELAY,NewBTCommunicator.SET_S3_SENSOR_MODE, NewBTCommunicator.SWITCH, NewBTCommunicator.BOOLEANMODE);
    		
    		// Right switch switch
    		sendBTCmessage(NewBTCommunicator.NO_DELAY,NewBTCommunicator.SET_S2_SENSOR_MODE, NewBTCommunicator.SWITCH, NewBTCommunicator.BOOLEANMODE);

    		portToTypeMap.put(S2, (int)NewBTCommunicator.SWITCH);
    		portToTypeMap.put(S3, (int)NewBTCommunicator.SWITCH);
    		portToTypeMap.put(S1, (int)NewBTCommunicator.LOWSPEED_9V);
    		sensorsConfigured=true;
    	}
        
    }
    
    public boolean sensorsConfiguredP()
    {
    	return sensorsConfigured;
    }
    
    public int getGyroSensorValue()
    {
    	return getRawSensorValue(0);
    }
    

    
    public int getRawSensorValue(int port)
    {
    	sendBTCmessage(NewBTCommunicator.NO_DELAY, NewBTCommunicator.GET_RAW_SENSOR_VALUE, port,RAWMODE);
    	return getResponseWithTimeout(1000,-99999);
    }
    
    public int getTouchSensorValue()
    {
    	int L, R;
    	sendBTCmessage(NewBTCommunicator.NO_DELAY, NewBTCommunicator.GET_RAW_SENSOR_VALUE, S3,RAWMODE);
    	L=getResponseWithTimeout(1000,-99999);
    	
    	sendBTCmessage(NewBTCommunicator.NO_DELAY, NewBTCommunicator.GET_RAW_SENSOR_VALUE, S2,RAWMODE);
    	R=getResponseWithTimeout(1000,-99999);
    	

    	return L;
    }
    
    public int getLeftTouchSensorValue()
    {
    	int L, R;
    	sendBTCmessage(NewBTCommunicator.NO_DELAY, NewBTCommunicator.GET_RAW_SENSOR_VALUE, S2,RAWMODE);
    	R=getResponseWithTimeout(1000,-99999);
    	
    	return R;
    }
    
    public int getRightTouchSensorValue()
    {
    	int L, R;
    	sendBTCmessage(NewBTCommunicator.NO_DELAY, NewBTCommunicator.GET_RAW_SENSOR_VALUE, S3,RAWMODE);
    	L=getResponseWithTimeout(1000,-99999);
    	
    	return L;
    }
    
    public int getBatteryValue()
    {
    	sendBTCmessage(NewBTCommunicator.NO_DELAY, NewBTCommunicator.GET_BATTERY_LEVEL, 0,0);
    	return getResponseWithTimeout(1000,-99999);
    }
    
    public void setLeftMotorTargetTach(int speed, int tach)
    {
    	sendBTCmessage(NewBTCommunicator.NO_DELAY, NewBTCommunicator.MOTOR_A_ACTION, speed,tach);
    	
    }
    
    public void setRightMotorTargetTach(int speed, int tach)
    {
    	sendBTCmessage(NewBTCommunicator.NO_DELAY, NewBTCommunicator.MOTOR_C_ACTION, speed,tach);
    	
    }
    
    public int getRightMotorPosition()
    {
    	return getMotorPosition(MOTOR_C);
    }
    
    public int getLeftMotorPosition()
    {
    	return getMotorPosition(MOTOR_A);
    }
    
    public void resetLeftTach()
    {
    	sendBTCmessage(NewBTCommunicator.NO_DELAY, NewBTCommunicator.RESET_MOTOR_POSITION, MOTOR_A,0);
    }
    
    public void resetRightTach()
    {
    	sendBTCmessage(NewBTCommunicator.NO_DELAY, NewBTCommunicator.RESET_MOTOR_POSITION, MOTOR_C,0);
    }
    
    private int getMotorPosition(int motorPort)
    {
    	sendBTCmessage(NewBTCommunicator.NO_DELAY, NewBTCommunicator.READ_MOTOR_STATE, motorPort,0);
    	return getResponseWithTimeout(1000,-99999);
    }
    
    
    private int getResponseWithTimeout(int commandTimeout, int timeoutResponse)
    {
    	Thread timeoutThread = new Thread(new TimeoutRunnable(commandTimeout,timeoutResponse));
    	int desiredResponse=0;
    	try
    	{
    		timeoutThread.start();
    		desiredResponse=responseQueue.take();
    		if (timeoutThread.isAlive())
    			timeoutThread.interrupt();
    	}
    	catch (Exception e)
    	{
    		
    	}
    	return desiredResponse;
    }
    
    
    /**
     * Sends the message to the robot.
     * @param delay time to wait before sending the message.
     * @param message the message type (as defined in BTCommucator)
     * @param value1 first parameter
     * @param value2 second parameter
     */   
    void sendBTCmessage(int delay, int message, int value1, int value2) {
        Bundle myBundle = new Bundle();
        myBundle.putInt("message", message);
        myBundle.putInt("value1", value1);
        myBundle.putInt("value2", value2);
        Message myMessage = myHandler.obtainMessage();
        myMessage.setData(myBundle);

        if (delay == 0)
        	myHandler.sendMessage(myMessage);

        else
        	myHandler.sendMessageDelayed(myMessage, delay);
    }

    /**
     * Sends the message via the BTCommuncator to the robot.
     * @param delay time to wait before sending the message.
     * @param message the message type (as defined in BTCommucator)
     * @param String a String parameter
     */       
    void sendBTCmessage(int delay, int message, String name) {
        Bundle myBundle = new Bundle();
        myBundle.putInt("message", message);
        myBundle.putString("name", name);
        Message myMessage = myHandler.obtainMessage();
        myMessage.setData(myBundle);

        if (delay == 0)
        	myHandler.sendMessage(myMessage);
        else
        	myHandler.sendMessageDelayed(myMessage, delay);
    }
    
    /**
     * Sends a message on the opened OutputStream
     * @param message, the message as a byte array
     */
    private void sendBTMessage(byte[] message) throws IOException 
    {
        if (nxtOutputStream == null)
            throw new IOException();

        // send message length
        int messageLength = message.length;
        nxtOutputStream.write(messageLength);
        nxtOutputStream.write(messageLength >> 8);
        nxtOutputStream.write(message, 0, message.length);
    }  

    /**
     * Receives a message on the opened InputStream
     * @return the message
     */                
    private byte[] receiveMessage() throws IOException 
    {
        if (nxtInputStream == null)
            throw new IOException();

        int length = nxtInputStream.read();
        length = (nxtInputStream.read() << 8) + length;
        byte[] returnMessage = new byte[length];
        nxtInputStream.read(returnMessage);
        return returnMessage;
    }    

    /**
     * Sends a message on the opened OutputStream. In case of 
     * an error the state is sent to the handler.
     * @param message, the message as a byte array
     */
    private void sendMessageAndState(byte[] message) 
    {
        if (nxtOutputStream == null)
            return;

        try 
        {
            sendBTMessage(message);
        }
        catch (IOException e) 
        {
            _responseInterface.onCommunicationError(e.toString());
        }
    }

    /* Processes return messages from NXT and directs replies back to uiHandler.  With direct commands, the NXT will echo the original 
     * command that was sent.  This is how the code below knows what command it is responding to
     */
    private void dispatchMessage(byte[] message) 
    {
        switch (message[1]) 
        {
            case LCPMessage.GET_OUTPUT_STATE:
                if (message.length >= 25)
                	_responseInterface.onMotorState(MOTOR_STATE);
                break;
            case LCPMessage.GET_FIRMWARE_VERSION:
                if (message.length >= 7)
                    _responseInterface.onFirmwareVersion(FIRMWARE_VERSION);
                break;
            case LCPMessage.FIND_FIRST:
            case LCPMessage.FIND_NEXT:

                if (message.length >= 28) {
                    // Success
                    //if (message[2] == 0)
                        //sendState(FIND_FILES);
                }
                break;
        }
    }

    private void doBeep(int frequency, int duration) {
        byte[] message = LCPMessage.getBeepMessage(frequency, duration);
        sendMessageAndState(message);
        waitSomeTime(20);
    }
    
    private void doAction(int actionNr) {
        byte[] message = LCPMessage.getActionMessage(actionNr);
        sendMessageAndState(message);
    }

    private void startProgram(String programName) {
        byte[] message = LCPMessage.getStartProgramMessage(programName);
        sendMessageAndState(message);
    }

    private void stopProgram() 
    {
        byte[] message = LCPMessage.getStopProgramMessage();
        sendMessageAndState(message);
    }
    
    private void getProgramName() throws IOException
    {
        byte[] message = LCPMessage.getProgramNameMessage();
        sendMessageAndState(message);
        returnMessage = receiveMessage();
        dispatchMessage(returnMessage);
    }
    
    private void changeMotorSpeed(int motor, int speed) {
        if (speed > 100)
            speed = 100;

        else if (speed < -100)
            speed = -100;

        byte[] message = LCPMessage.getMotorMessage(motor, speed);
        sendMessageAndState(message);
    }

    private void changeMotorSpeed(int motor, int speed, boolean regulation) {
        if (speed > 100)
            speed = 100;

        else if (speed < -100)
            speed = -100;

        byte[] message = LCPMessage.getMotorMessage(motor, speed);
        sendMessageAndState(message);
    }
    
    private void rotateTo(int motor, int speed, int end) {
        byte[] message = LCPMessage.getMotorMessage(motor, speed, end);
        sendMessageAndState(message);
    }

    private void reset(int motor) {
        byte[] message = LCPMessage.getResetMessage(motor);
        sendMessageAndState(message);
    }

    private byte[] readMotorState(int motor) throws IOException
    {
        byte[] message = LCPMessage.getOutputStateMessage(motor);
        sendMessageAndState(message);
        return receiveMessage();
    }

    private void getFirmwareVersion() throws IOException
    {
        byte[] message = LCPMessage.getFirmwareVersionMessage();
        sendMessageAndState(message);
        returnMessage = receiveMessage();
        dispatchMessage(returnMessage);
    }

    private void findFiles(boolean findFirst, int handle) throws IOException
    {
        byte[] message = LCPMessage.getFindFilesMessage(findFirst, handle, "*.*");
        sendMessageAndState(message);
        returnMessage = receiveMessage();
        dispatchMessage(returnMessage);
    }

    public static void waitSomeTime(int millis) 
    {
        try {
            Thread.sleep(millis);

        } catch (InterruptedException e) {
        }
    }

    

//    private void sendState(int message) 
//    {
//        Bundle myBundle = new Bundle();
//        myBundle.putInt("message", message);
//        sendBundle(myBundle);
//    }

    

    
    
    
	/* Device control locations */

	private static final byte DISTANCE = 0x42;

	/* Device modes */

	/* Device timing */
	private static final int DELAY_CMD = 0x5;

	
	private byte[] buf = new byte[1];
	private int currentDistance;

	protected byte address = 0x02; // the default I2C address for a port. You can change address of compass sensor (see docs) and then communicate with multiple sensors on same physical port.
	protected static byte STOP = 0x00; // Commands don't seem to use this?
	protected static String BLANK = "       ";
	
	// Port information (constants)
	
	/**
	 * Returns the version number of the sensor. e.g. "V1.0" Reply length = 8.
	 */
	protected static byte VERSION = 0x00;
	/**
	 * Returns the product ID of the sensor.  e.g. "LEGO" Reply length = 8.
	 */
	protected static byte PRODUCT_ID = 0x08;
	/**
	 * Returns the sensor type. e.g. "Sonar" Reply length = 8.
	 */
	protected static byte SENSOR_TYPE = 0x10;
	/**
	 * Returns the "zero position" set at the factory for this sensor. e.g. 0 Reply length = 1.
	 *
	 */
	
	
	/*******************************************
	 * ******  Begin of NXTProtocal Constants 
	 ******************************************** 
	 */
	
	// Command types constants. Indicates type of packet being sent or received.
	
	public static byte SYSTEM_COMMAND_REPLY = 0x01;
	public static byte REPLY_COMMAND = 0x02;
	//public static byte LCP DIRECT_COMMAND_NOREPLY = (byte)0x80; // Avoids ~100ms latency
	public static byte SYSTEM_COMMAND_NOREPLY = (byte)0x81; // Avoids ~100ms latency
	
	// System Commands:
	public static byte OPEN_READ = (byte)0x80;
	public static byte OPEN_WRITE = (byte)0x81;
	public static byte READ = (byte)0x82;
	public static byte WRITE = (byte)0x83;
	public static byte CLOSE = (byte)0x84;
	public static byte DELETE = (byte)0x85;
	public static byte FIND_FIRST = (byte)0x86;
	public static byte FIND_NEXT = (byte)0x87;
	
	public static byte OPEN_WRITE_LINEAR = (byte)0x89;
	public static byte OPEN_READ_LINEAR = (byte)0x8A;
	public static byte OPEN_WRITE_DATA = (byte)0x8B;
	public static byte OPEN_APPEND_DATA = (byte)0x8C;
	// Many commands could be hidden between 0x8D and 0x96!
	public static byte BOOT = (byte)0x97;
	public static byte SET_BRICK_NAME = (byte)0x98;
	// public static byte MYSTERY_COMMAND = (byte)0x99;
	// public static byte MYSTERY_COMMAND = (byte)0x9A;
	public static byte GET_DEVICE_INFO = (byte)0x9B;
	// commands could be hidden here...
	public static byte DELETE_USER_FLASH = (byte)0xA0;
	public static byte POLL_LENGTH = (byte)0xA1;
	public static byte POLL = (byte)0xA2;
	
	public static byte NXJ_FIND_FIRST = (byte)0xB6;
	public static byte NXJ_FIND_NEXT = (byte)0xB7;
    public static byte NXJ_PACKET_MODE = (byte)0xff;
	
	// Poll constants:
	public static byte POLL_BUFFER = (byte)0x00;
	public static byte HIGH_SPEED_BUFFER = (byte)0x01;
		
	// Direct Commands

	public static byte PLAY_SOUND_FILE = 0x02;
	public static byte PLAY_TONE = 0x03;
	public static byte SET_OUTPUT_STATE = 0x04;
	public static byte SET_INPUT_MODE = 0x05;
	public static byte GET_OUTPUT_STATE = 0x06;
	public static byte GET_INPUT_VALUES = 0x07;
	public static byte RESET_SCALED_INPUT_VALUE = 0x08;
	public static byte MESSAGE_WRITE = 0x09;
	public static byte RESET_MOTOR_POSITION = 0x0A;	
	public static final byte GET_BATTERY_LEVEL = 0x0B;
	public static byte STOP_SOUND_PLAYBACK = 0x0C;
	public static byte KEEP_ALIVE = 0x0D;

	public static byte LS_READ = 0x10;
	public static byte GET_CURRENT_PROGRAM_NAME = 0x11;
	// public static byte MYSTERY_OPCODE = 0x12; // ????
	public static byte MESSAGE_READ = 0x13;
	// public static byte POSSIBLY_MORE_HIDDEN = 0x14; // ????
	
	// NXJ additions
	public static byte NXJ_DISCONNECT = 0x20; 
	public static byte NXJ_DEFRAG = 0x21;
	
	// Output state constants 
	// "Mode":
	/** Turn on the specified motor */
	public static byte MOTORON = 0x01;
	/** Use run/brake instead of run/float in PWM */
	public static byte BRAKE = 0x02;
	/** Turns on the regulation */
	public static byte REGULATED = 0x04; 

	// "Regulation Mode":
	/** No regulation will be enabled */
	public static byte REGULATION_MODE_IDLE = 0x00;
	/** Power control will be enabled on specified output */
	public static byte REGULATION_MODE_MOTOR_SPEED = 0x01;
	/** Synchronization will be enabled (Needs enabled on two output) */
	public static byte REGULATION_MODE_MOTOR_SYNC = 0x02; 

	// "RunState":
	/** Output will be idle */
	public static byte MOTOR_RUN_STATE_IDLE = 0x00;
	/** Output will ramp-up */
	public static byte MOTOR_RUN_STATE_RAMPUP = 0x10;	
	/** Output will be running */
	public static byte MOTOR_RUN_STATE_RUNNING = 0x20; 
	/** Output will ramp-down */
	public static byte MOTOR_RUN_STATE_RAMPDOWN = 0x40;
	
	// Input Mode Constants
	// "Port Type":
	/**  */
	public static byte NO_SENSOR = 0x00;
	/**  */
	public static byte SWITCH = 0x01;
	/**  */
	public static byte TEMPERATURE = 0x02;
	/**  */
	public static byte REFLECTION = 0x03;
	/**  */
	public static byte ANGLE = 0x04;
	/**  */
	public static byte LIGHT_ACTIVE = 0x05;
	/**  */
	public static final byte LIGHT_INACTIVE = 0x06;
	/**  */
	public static byte SOUND_DB = 0x07;
	/**  */
	public static byte SOUND_DBA = 0x08;
	/**  */
	public static byte CUSTOM = 0x09;
	/**  */
	public static byte LOWSPEED = 0x0A;
	/**  */
	public static byte LOWSPEED_9V = 0x0B;
	/**  */
	public static byte NO_OF_SENSOR_TYPES = 0x0C;

	// "Port Mode":
	/**  */
	public static byte RAWMODE = 0x00;
	/**  */
	public static byte BOOLEANMODE = 0x20;
	/**  */
	public static byte TRANSITIONCNTMODE = 0x40;
	/**  */
	public static byte PERIODCOUNTERMODE = 0x60;
	/**  */
	public static byte PCTFULLSCALEMODE = (byte)0x80;
	/**  */
	public static byte CELSIUSMODE = (byte)0xA0;
	/**  */
	public static byte FAHRENHEITMODE = (byte)0xC0;
	/**  */
	public static byte ANGLESTEPSMODE = (byte)0xE0;
	/**  */
	public static byte SLOPEMASK = 0x1F;
	/**  */
	public static byte MODEMASK = (byte)0xE0;
	
	/*******************************************
	 * ******  End of NXTProtocal Constants 
	 ******************************************** 
	 */
	
	// TODO: Do not hardcode this!
	byte port = S3;
	
    
    /**
	 * Return distance to an object. To ensure that the data returned is valid
	 * this method may have to wait a short while for the distance data to
	 * become available.
	 *
	 * @return distance or 255 if no object in range
	 */
	
	// 1
	private int getDistance(byte port)
	{
		
		int ret;
		for (int i=0;i<2;i++)
		{
			ret= getData(DISTANCE, buf, 1,port);
			currentDistance = (ret == 0 ? (buf[0] & 0xff) : 255);
		}

		// Make a note of when new data should be available.
		
		return currentDistance;
	}
	

	private int getBatteryMilli() throws IOException 
	{
		byte[] message = new byte[]{LCPMessage.DIRECT_COMMAND_REPLY, LCPMessage.GET_BATTERY_LEVEL};
		sendBTMessage(message);
		byte[] returnMessage = receiveMessage();
		return convertUnsignedByteArrayToInt(returnMessage,3);
	}
	
	
	private byte[] getRawSensoryValues(int port) throws IOException 
	{
		byte[] message = new byte[]{LCPMessage.DIRECT_COMMAND_REPLY, LCPMessage.GET_INPUT_VALUES, (byte)port};
		sendBTMessage(message);
		return receiveMessage();
	}
	
	private void setInputState(byte port, byte sensoryType, byte sensorMode) throws IOException 
	{
		byte[] command = new byte[]{LCPMessage.DIRECT_COMMAND_NOREPLY, LCPMessage.SET_INPUT_MODE, port,sensoryType, sensorMode};
		
		sendBTMessage(command);
	}


	
	/*
	 * Over-ride standard get function to ensure correct inter-command timing
	 * when using the ultrasonic sensor. The Lego Ultrasonic sensor uses a
	 * "bit-banged" i2c interface and seems to require a minimum delay between
	 * commands otherwise the commands fail.
	 */
	// 1.2
	public int getData(int register, byte [] buf, int len, byte port)
	{
		
		waitSomeTime(DELAY_CMD);

		
		byte [] txData = {address, (byte) register};
		try 
		{
			LSWrite(port, txData, (byte)len);
		} 
		catch (IOException ioe) 
		{
			throw new RuntimeException(ioe.toString());
		}
		
		byte [] status = null;
		do 
		{
			try 
			{
				status = LSGetStatus(port);
			} 
			catch (IOException ioe) 
			{
				return -1;
			}
		} 
		while(status[0] == ErrorMessages.PENDING_COMMUNICATION_TRANSACTION_IN_PROGRESS|status[0] == ErrorMessages.SPECIFIED_CHANNEL_CONNECTION_NOT_CONFIGURED_OR_BUSY);
	
		try 
		{
			byte [] ret = LSRead(port);
			if (ret != null) 
				System.arraycopy(ret, 0,buf, 0, ret.length);
		} 
		catch (IOException ioe) 
		{
			return -1;
		}
		
		return status[0];
	}
	
	/**
	 * Reads data from an Inter-Integrated Circuit (I2C) sensor (the 
	 * ultrasound sensor) via the Low Speed (LS) data port. The port must 
	 * first be configured to type LOWSPEED or LOWSPEED_9V.
	 * Data lengths are limited to 16 bytes per command. The response will
	 * also contain 16 bytes, with invalid data padded with zeros.
	 * @param port
	 * @return the response
	 */
	// 1.2.1.3
	public byte [] LSRead(byte port) throws IOException 
	{
		byte [] request = {LCPMessage.DIRECT_COMMAND_REPLY, LS_READ, port};
		byte [] reply=null;
		sendBTMessage(request);
		
		reply= receiveMessage();
		
		byte rxLength = reply[3];
		if(reply[2] == 0 && rxLength >= 0) 
		{
            byte [] rxData = new byte[rxLength];
			System.arraycopy(reply, 4, rxData, 0, rxLength);
            return rxData;
		} else {
			return null;
		}
	}
	
	
	
	
	/**
	 * Used to request data from an Inter-Integrated Circuit (I2C) sensor (the 
	 * ultrasound sensor) via the Low Speed (LS) data port. The port must first 
	 * be configured to type  LOWSPEED or LOWSPEED_9V.
	 * Data lengths are limited to 16 bytes per command.
	 * Rx (receive) Data Length MUST be specified in the write
	 * command since reading from the device is done on a 
	 * master-slave basis.
	 * @param txData Transmitted data.
	 * @param rxDataLength Receive data length.
	 * @param port 0-3
	 * @return the status (0 = success)
	 */
	// 1.2.1.1
	public byte LSWrite(byte port, byte [] txData, byte rxDataLength) throws IOException 
	{
		byte [] request = {LCPMessage.DIRECT_COMMAND_NOREPLY, LCPMessage.LS_WRITE, port, (byte)txData.length, rxDataLength};
		request = appendBytes(request, txData);
		// Use Mindroid version of sendRequest
		sendBTMessage(request);
		return 1;
	}
	
	
	/**
	 * Returns the status for an Inter-Integrated Circuit (I2C) sensor (the 
	 * ultrasound sensor) via the Low Speed (LS) data port. The port must first 
	 * be configured to type LOWSPEED or LOWSPEED_9V.
	 * @param port 0-3
	 * @return byte[0] = status, byte[1] = Bytes Ready (count of available bytes to read)
	 */
	// 1.2.1.2
	public byte [] LSGetStatus(byte port) throws IOException
	{
		byte [] request = {LCPMessage.DIRECT_COMMAND_REPLY, LCPMessage.LS_GET_STATUS, port};
		sendBTMessage(request);
		
		byte [] reply = receiveMessage();
		byte [] returnData = {reply[2], reply[3]}; 
		return returnData;
	}
	
	
	/**
	 * Helper method to concatenate two byte arrays
	 * @param array1 the first array (e.g. a request)
	 * @param array2 the second array (e.g. an extra parameter)
	 * 
	 * @return the concatenated array
	 */
	// 1.2.1.1.1
	private byte[] appendBytes(byte[] array1, byte[] array2) {
		byte[] array = new byte[array1.length + array2.length];
		System.arraycopy(array1, 0, array, 0, array1.length);
		System.arraycopy(array2, 0, array, array1.length, array2.length);
		return array;
	}

	
}
