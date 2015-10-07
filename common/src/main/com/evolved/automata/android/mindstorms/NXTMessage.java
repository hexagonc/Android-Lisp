/**
 *   Copyright 2010, 2011 Guenther Hoelzl, Shawn Brown
 *
 *   This file is part of MINDdroid.
 *
 *   MINDdroid is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   MINDdroid is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with MINDdroid.  If not, see <http://www.gnu.org/licenses/>.
**/

package com.evolved.automata.android.mindstorms;

import java.util.HashMap;


/**
 * Class for composing the proper messages for simple
 * communication over bluetooth
 */
public class NXTMessage {

	public static enum TachRelation
	{
		RELATIVE_TO_LAST_MOVEMENT, ABSOLUTE
	}
    // the folowing constants were taken from the leJOS project (http://www.lejos.org) 
    
    // Command types constants. Indicates type of packet being sent or received.
    public static byte DIRECT_COMMAND_REPLY = 0x00;
    public static byte SYSTEM_COMMAND_REPLY = 0x01;
    public static byte REPLY_COMMAND = 0x02;
    public static byte DIRECT_COMMAND_NOREPLY = (byte)0x80; // Avoids ~100ms latency
    public static byte SYSTEM_COMMAND_NOREPLY = (byte)0x81; // Avoids ~100ms latency

    // Direct Commands
    public static final byte START_PROGRAM = 0x00;
    public static final byte STOP_PROGRAM = 0x01;
    public static final byte PLAY_SOUND_FILE = 0x02;
    public static final byte PLAY_TONE = 0x03;
    public static final byte SET_OUTPUT_STATE = 0x04;
    public static final byte SET_INPUT_MODE = 0x05;
    public static final byte GET_OUTPUT_STATE = 0x06;
    public static final byte GET_INPUT_VALUES = 0x07;
    public static final byte RESET_SCALED_INPUT_VALUE = 0x08;
    public static final byte MESSAGE_WRITE = 0x09;
    public static final byte RESET_MOTOR_POSITION = 0x0A;   
    public static final byte GET_BATTERY_LEVEL = 0x0B;
    public static final byte STOP_SOUND_PLAYBACK = 0x0C;
    public static final byte KEEP_ALIVE = 0x0D;
    public static final byte LS_GET_STATUS = 0x0E;
    public static final byte LS_WRITE = 0x0F;
    public static final byte LS_READ = 0x10;
    public static final byte GET_CURRENT_PROGRAM_NAME = 0x11;
    public static final byte MESSAGE_READ = 0x13;
        
    // System Commands:
    public static final byte OPEN_READ = (byte)0x80;
    public static final byte OPEN_WRITE = (byte)0x81;
    public static final byte READ = (byte)0x82;
    public static final byte WRITE = (byte)0x83;
    public static final byte CLOSE = (byte)0x84;
    public static final byte DELETE = (byte)0x85;        
    public static final byte FIND_FIRST = (byte)0x86;
    public static final byte FIND_NEXT = (byte)0x87;
    public static final byte GET_FIRMWARE_VERSION = (byte)0x88;
    public static final byte OPEN_WRITE_LINEAR = (byte)0x89;
    public static final byte OPEN_READ_LINEAR = (byte)0x8A;
    public static final byte OPEN_WRITE_DATA = (byte)0x8B;
    public static final byte OPEN_APPEND_DATA = (byte)0x8C;
    public static final byte BOOT = (byte)0x97;
    public static final byte SET_BRICK_NAME = (byte)0x98;
    public static final byte GET_DEVICE_INFO = (byte)0x9B;
    public static final byte DELETE_USER_FLASH = (byte)0xA0;
    public static final byte POLL_LENGTH = (byte)0xA1;
    public static final byte POLL = (byte)0xA2;
   
    // Sensor Type
    public static final byte NO_SENSOR = 0x0;
    public static final byte SWITCH = 0x1;
    public static final byte TEMPERATURE = 0x2;
    public static final byte REFLECTION = 0x3;
    public static final byte ANGLE = 0x4;
    public static final byte LIGHT_ACTIVE = 0x5;
    public static final byte LIGHT_INACTIVE = 0x6;
    public static final byte SOUND_DB = 0x7;
    public static final byte SOUND_DBA = 0x8;
    public static final byte CUSTOM = 0x9;
    public static final byte LOWSPEED = 0xA;
    public static final byte LOWSPEED_9V = 0xB;
    public static final byte NO_OF_SENSOR_TYPES = 0xC;
    
    // Sensor Modes
    
    public static final byte RAW_MODE = 0x0;
    public static final byte BOOLEANMODE = 0x20;
    public static final byte TRANSITIONCNTMODE = 0x40;
    public static final byte PERIODCOUNTERMODE = 0x60;
    public static final byte PCTFULLSCALEMODE = (byte)0x80;
    public static final byte CELSIUSMODE = (byte)0xA0;
    public static final byte FAHRENHEITMODE = (byte)0xC0;
    public static final byte ANGLESTEPMODE = (byte)0xE0;
    public static final byte SLOPEMASK = (byte)0x1F;
    public static final byte MODEMASK = (byte)0xE0;
    
   
    
    // Message Data Indices
    public static final int _IDX_COMMAND_TYPE_GENERAL = 1;
    public static final int _IDX_STATUS_GENERAL = 2;
    
    
    // Response packet
    public static final int _IDX_GET_INPUT_VALUES_STATUS = 2;
    public static final int _IDX_GET_INPUT_VALUES_PORT = 3;
    public static final int _IDX_GET_INPUT_VALUES_VALID = 4;
    public static final int _IDX_GET_INPUT_VALUES_CALIBRATED = 5;
    public static final int _IDX_GET_INPUT_VALUES_SENSOR_TYPE = 6;
    public static final int _IDX_GET_INPUT_VALUES_SENSOR_MODE = 7;
    public static final int _IDX_GET_INPUT_VALUES_VALUE_LSB = 8;
    public static final int _IDX_GET_INPUT_VALUES_VALUE_MSB = 9;
    public static final int _IDX_GET_INPUT_VALUES_NORMALIZED_VALUE_LSB = 10;
    public static final int _IDX_GET_INPUT_VALUES_NORMALIZED_VALUE_MSB = 11;
    public static final int _IDX_GET_INPUT_VALUES_SCALED_VALUE_LSB = 12;
    public static final int _IDX_GET_INPUT_VALUES_SCALED_VALUE_MSB = 13;
    public static final int _IDX_GET_INPUT_VALUES_CALIBRATED_VALUE_LSB = 14;
    public static final int _IDX_GET_INPUT_VALUES_CALIBRATED_VALUE_MSB = 15;
    
    // Command packet
    public static final int _IDX_SET_INPUT_MODE_PORT = 2;
    public static final int _IDX_SET_INPUT_MODE_TYPE = 3;
    
    
    // Command paket
    public static final int _IDX_SET_OUTPUT_STATE_PORT = 2;
    public static final int _IDX_SET_OUTPUT_STATE_POWER_SET = 3;
    public static final int _IDX_SET_OUTPUT_STATE_MODE = 4;
    public static final int _IDX_SET_OUTPUT_STATE_REGULATION_MODE = 5;
    public static final int _IDX_SET_OUTPUT_STATE_TURN_RATIO = 6;
    public static final int _IDX_SET_OUTPUT_STATE_RUN_STATE = 7;
    public static final int _IDX_SET_OUTPUT_STATE_TACHO_LIMIT_1 = 8;
    public static final int _IDX_SET_OUTPUT_STATE_TACHO_LIMIT_2 = 9;
    public static final int _IDX_SET_OUTPUT_STATE_TACHO_LIMIT_3 = 10;
    public static final int _IDX_SET_OUTPUT_STATE_TACHO_LIMIT_4 = 11;
    
    
    // Response packet
    
    public static final int _IDX_GET_OUTPUT_STATE_STATUS = 2;
    public static final int _IDX_GET_OUTPUT_STATE_PORT = 3;
    public static final int _IDX_GET_OUTPUT_STATE_POWER_SET = 4;
    public static final int _IDX_GET_OUTPUT_STATE_MODE = 5;
    public static final int _IDX_GET_OUTPUT_STATE_REGULATION_MODE = 6;
    public static final int _IDX_GET_OUTPUT_STATE_TURN_RATIO = 7;
    public static final int _IDX_GET_OUTPUT_STATE_RUN_STATE = 8;
    public static final int _IDX_GET_OUTPUT_STATE_TACHO_LIMIT_BIT1 = 9;
    public static final int _IDX_GET_OUTPUT_STATE_TACHO_LIMIT_BIT2 = 10;
    public static final int _IDX_GET_OUTPUT_STATE_TACHO_LIMIT_BIT3 = 11;
    public static final int _IDX_GET_OUTPUT_STATE_TACHO_LIMIT_BIT4 = 12;
    public static final int _IDX_GET_OUTPUT_STATE_TACHO_COUNT_BIT1 = 13;
    public static final int _IDX_GET_OUTPUT_STATE_TACHO_COUNT_BIT2 = 14;
    public static final int _IDX_GET_OUTPUT_STATE_TACHO_COUNT_BIT3 = 15;
    public static final int _IDX_GET_OUTPUT_STATE_TACHO_COUNT_BIT4 = 16;
    public static final int _IDX_GET_OUTPUT_STATE_BLOCK_TACHO_COUNT_BIT1 = 17;
    public static final int _IDX_GET_OUTPUT_STATE_BLOCK_TACHO_COUNT_BIT2 = 18;
    public static final int _IDX_GET_OUTPUT_STATE_BLOCK_TACHO_COUNT_BIT3 = 18;
    public static final int _IDX_GET_OUTPUT_STATE_BLOCK_TACHO_COUNT_BIT4 = 19;
    public static final int _IDX_GET_OUTPUT_STATE_ROTATION_COUNT_BIT1 = 20;
    public static final int _IDX_GET_OUTPUT_STATE_ROTATION_COUNT_BIT2 = 21;
    public static final int _IDX_GET_OUTPUT_STATE_ROTATION_COUNT_BIT3 = 22;
    public static final int _IDX_GET_OUTPUT_STATE_ROTATION_COUNT_BIT4 = 23;
    
    // 
    public static final int _IDX_GET_BATTERY_LEVEL_STATUS = 2;
    public static final int _IDX_GET_BATTERY_LEVEL_MILLIVOLTS_LSB = 3;
    public static final int _IDX_GET_BATTERY_LEVEL_MILLIVOLTS_MSB = 4;
    
    
    public static final byte _I2C_ADDRESS = 0x02; // the default I2C address for a port. You can change address of compass sensor (see docs) and then communicate with multiple sensors on same physical port.
    /* Device timing */
    public static final int _COMMAND_DELAY_MILLI = 5;
    
    // Status Messages
 // Direct communication errors:
 	public static final byte PENDING_COMMUNICATION_TRANSACTION_IN_PROGRESS = 0x20;
 	public static final byte SPECIFIED_MAILBOX_QUEUE_IS_EMPTY = 0x40;
 	/** Request failed (i.e. specified file not found) */
 	public static final byte REQUEST_FAILED = (byte)0xBD;
 	public static final byte UNKNOWN_COMMAND_OPCODE = (byte)0xBE;
 	public static final byte INSANE_PACKET = (byte)0xBF;
 	public static final byte DATA_CONTAINS_OUT_OF_RANGE_VALUES = (byte)0xC0;
 	public static final byte COMMUNICATION_BUS_ERROR = (byte)0xDD;
 	public static final byte NO_FREE_MEMORY_IN_COMMUNICATION_BUFFER = (byte)0xDE;
 	/** Specified channel/connection is not valid */
 	public static final byte SPECIFIED_CHANNEL_CONNECTION_IS_NOT_VALID = (byte)0xDF;
 	/** Specified channel/connection not configured or busy */
 	public static final byte SPECIFIED_CHANNEL_CONNECTION_NOT_CONFIGURED_OR_BUSY = (byte)0xE0;
 	public static final byte NO_ACTIVE_PROGRAM = (byte)0xEC;
 	public static final byte ILLEGAL_SIZE_SPECIFIED = (byte)0xED;
 	public static final byte ILLEGAL_MAILBOX_QUEUE_ID_SPECIFIED = (byte)0xEE;
 	public static final byte ATTEMPTED_TO_ACCESS_INVALID_FIELD_OF_A_STRUCTURE = (byte)0xEF;
 	public static final byte BAD_INPUT_OR_OUTPUT_SPECIFIED = (byte)0xF0;
 	public static final byte INSUFFICIENT_MEMORY_AVAILABLE = (byte)0xFB;
 	public static final byte BAD_ARGUMENTS = (byte)0xFF;
 	
 	// Communication protocol errors:
 	public static final byte SUCCESS = 0x00;
 	public static final byte NO_MORE_HANDLES = (byte)0x81;
 	public static final byte NO_SPACE = (byte)0x82;
 	public static final byte NO_MORE_FILES = (byte)0x83;
 	public static final byte END_OF_FILE_EXPECTED = (byte)0x84;
 	public static final byte END_OF_FILE = (byte)0x85;
 	public static final byte NOT_A_LINEAR_FILE = (byte)0x86;
 	public static final byte FILE_NOT_FOUND = (byte)0x87;
 	public static final byte HANDLE_ALREADY_CLOSED = (byte)0x88;
 	public static final byte NO_LINEAR_SPACE = (byte)0x89;
 	public static final byte UNDEFINED_ERROR = (byte)0x8A;
 	public static final byte FILE_IS_BUSY = (byte)0x8B;
 	public static final byte NO_WRITE_BUFFERS = (byte)0x8C;
 	public static final byte APPEND_NOT_POSSIBLE = (byte)0x8D;
 	public static final byte FILE_IS_FULL = (byte)0x8E;
 	public static final byte FILE_EXISTS = (byte)0x8F;
 	public static final byte MODULE_NOT_FOUND = (byte)0x90;
 	public static final byte OUT_OF_BOUNDARY = (byte)0x91;
 	public static final byte ILLEGAL_FILE_NAME = (byte)0x92;
 	public static final byte ILLEGAL_HANDLE = (byte)0x93;
 	
 	
 	public static final HashMap<Byte, String> _STATUS_DESC_MAP =  new HashMap<Byte, String>();
 	public static final HashMap<Byte, String> _COMMAND_DESC_MAP = new HashMap<Byte, String>();
 	public static final HashMap<Byte, String> _SENSOR_MODE_DESC_MAP = new HashMap<Byte, String>();
 	public static final HashMap<Byte, String> _SENSOR_TYPE_MAP = new HashMap<Byte, String>();
 	
 	static{
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x20),"PENDING_COMMUNICATION_TRANSACTION_IN_PROGRESS");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x40),"SPECIFIED_MAILBOX_QUEUE_IS_EMPTY");
 		/** Request failed (i.e. specified file not found) */
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0xBD),"REQUEST_FAILED");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0xBE),"UNKNOWN_COMMAND_OPCODE");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0xBF),"INSANE_PACKET");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0xC0),"DATA_CONTAINS_OUT_OF_RANGE_VALUES");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0xDD),"COMMUNICATION_BUS_ERROR");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0xDE),"NO_FREE_MEMORY_IN_COMMUNICATION_BUFFER");
 		/** Specified channel/connection is not valid */
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0xDF),"SPECIFIED_CHANNEL_CONNECTION_IS_NOT_VALID");
 		/** Specified channel/connection not configured or busy */
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0xE0),"SPECIFIED_CHANNEL_CONNECTION_NOT_CONFIGURED_OR_BUSY");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0xEC),"NO_ACTIVE_PROGRAM");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0xED),"ILLEGAL_SIZE_SPECIFIED");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0xEE),"ILLEGAL_MAILBOX_QUEUE_ID_SPECIFIED");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0xEF),"ATTEMPTED_TO_ACCESS_INVALID_FIELD_OF_A_STRUCTURE");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0xF0),"BAD_INPUT_OR_OUTPUT_SPECIFIED");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0xFB),"INSUFFICIENT_MEMORY_AVAILABLE");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0xFF),"BAD_ARGUMENTS");
 		
 		// Communication protocol errors:
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x00),"SUCCESS");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x81),"NO_MORE_HANDLES");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x82),"NO_SPACE");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x83),"NO_MORE_FILES");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x84),"END_OF_FILE_EXPECTED");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x85),"END_OF_FILE");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x86),"NOT_A_LINEAR_FILE");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x87),"FILE_NOT_FOUND");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x88),"HANDLE_ALREADY_CLOSED");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x89),"NO_LINEAR_SPACE");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x8A),"UNDEFINED_ERROR");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x8B),"FILE_IS_BUSY");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x8C),"NO_WRITE_BUFFERS");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x8D),"APPEND_NOT_POSSIBLE");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x8E),"FILE_IS_FULL");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x8F),"FILE_EXISTS");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x90),"MODULE_NOT_FOUND");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x91),"OUT_OF_BOUNDARY");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x92),"ILLEGAL_FILE_NAME");
 		_STATUS_DESC_MAP.put(Byte.valueOf((byte)0x93),"ILLEGAL_HANDLE");
 		
 		
 		_COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x00), "START_PROGRAM");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x01), "STOP_PROGRAM");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x02), "PLAY_SOUND_FILE");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x03), "PLAY_TONE");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x04), "SET_OUTPUT_STATE");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x05), "SET_INPUT_MODE");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x06), "GET_OUTPUT_STATE");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x07), "GET_INPUT_VALUES");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x08), "RESET_SCALED_INPUT_VALUE");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x09), "MESSAGE_WRITE");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x0A), "RESET_MOTOR_POSITION");   
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x0B), "GET_BATTERY_LEVEL");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x0C), "STOP_SOUND_PLAYBACK");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x0D), "KEEP_ALIVE");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x0E), "LS_GET_STATUS");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x0F), "LS_WRITE");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x10), "LS_READ");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x11), "GET_CURRENT_PROGRAM_NAME");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x13), "MESSAGE_READ");
 	        
 	    // System Commands:
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x80), "OPEN_READ");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x81), "OPEN_WRITE");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x82), "READ");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x83), "WRITE");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x84), "CLOSE");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x85), "DELETE");        
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x86), "FIND_FIRST");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x87), "FIND_NEXT");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x88), "GET_FIRMWARE_VERSION");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x89), "OPEN_WRITE_LINEAR");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x8A), "OPEN_READ_LINEAR");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x8B), "OPEN_WRITE_DATA");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x8C), "OPEN_APPEND_DATA");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x97), "BOOT");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x98), "SET_BRICK_NAME");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0x9B), "GET_DEVICE_INFO");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0xA0), "DELETE_USER_FLASH");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0xA1), "POLL_LENGTH");
 	    _COMMAND_DESC_MAP.put(Byte.valueOf((byte)0xA2), "POLL");
 	    
 	    
 	    _SENSOR_TYPE_MAP.put(Byte.valueOf((byte)0x0), "NO_SENSOR");
 	    _SENSOR_TYPE_MAP.put(Byte.valueOf((byte)0x1), "SWITCH");
 	    _SENSOR_TYPE_MAP.put(Byte.valueOf((byte)0x2), "TEMPERATURE");
 	    _SENSOR_TYPE_MAP.put(Byte.valueOf((byte)0x3), "REFLECTION");
 	    _SENSOR_TYPE_MAP.put(Byte.valueOf((byte)0x4), "ANGLE");
 	    _SENSOR_TYPE_MAP.put(Byte.valueOf((byte)0x5), "LIGHT_ACTIVE");
 	    _SENSOR_TYPE_MAP.put(Byte.valueOf((byte)0x6), "LIGHT_INACTIVE");
 	    _SENSOR_TYPE_MAP.put(Byte.valueOf((byte)0x7), "SOUND_DB");
 	    _SENSOR_TYPE_MAP.put(Byte.valueOf((byte)0x8), "SOUND_DBA");
 	    _SENSOR_TYPE_MAP.put(Byte.valueOf((byte)0x9), "CUSTOM");
 	    _SENSOR_TYPE_MAP.put(Byte.valueOf((byte)0xA), "LOWSPEED");
 	    _SENSOR_TYPE_MAP.put(Byte.valueOf((byte)0xB), "LOWSPEED_9V");
 	    _SENSOR_TYPE_MAP.put(Byte.valueOf((byte)0xC), "NO_OF_SENSOR_TYPES");
 	    
 	    _SENSOR_MODE_DESC_MAP.put(Byte.valueOf((byte)0x0), "RAW_MODE");
 	    _SENSOR_MODE_DESC_MAP.put(Byte.valueOf((byte)0x20), "BOOLEANMODE");
 	    _SENSOR_MODE_DESC_MAP.put(Byte.valueOf((byte)0x40), "TRANSITIONCNTMODE");
 	    _SENSOR_MODE_DESC_MAP.put(Byte.valueOf((byte)0x60), "PERIODCOUNTERMODE");
 	    _SENSOR_MODE_DESC_MAP.put(Byte.valueOf((byte)0x80), "PCTFULLSCALEMODE");
 	    _SENSOR_MODE_DESC_MAP.put(Byte.valueOf((byte)0xA0), "CELSIUSMODE");
 	    _SENSOR_MODE_DESC_MAP.put(Byte.valueOf((byte)0xC0), "FAHRENHEITMODE");
 	    _SENSOR_MODE_DESC_MAP.put(Byte.valueOf((byte)0xE0), "ANGLESTEPMODE");
 	    _SENSOR_MODE_DESC_MAP.put(Byte.valueOf((byte)0x1F), "SLOPEMASK");
 	    _SENSOR_MODE_DESC_MAP.put(Byte.valueOf((byte)0xE0), "MODEMASK");
 	    
 	}
    
    
    
    
    

    public static byte[] getBeepMessage(boolean replyRequired, int frequency, int duration) {
        byte[] message = new byte[6];

        message[0] = getDirectCommandTypePrefix(replyRequired);
        message[1] = PLAY_TONE;
        // Frequency for the tone, Hz (UWORD); Range: 200-14000 Hz
        message[2] = (byte) frequency;
        message[3] = (byte) (frequency >> 8);
        // Duration of the tone, ms (UWORD)
        message[4] = (byte) duration;
        message[5] = (byte) (duration >> 8);

        return message;
    }
    


    public static byte[] getMotorMessage(boolean requireRequired, int motor, int speed) {
        byte[] message = new byte[12];

        message[0] = getDirectCommandTypePrefix(requireRequired);
        message[1] = SET_OUTPUT_STATE;
        // Output port
        message[2] = (byte) motor;

        if (speed == 0) {
            message[3] = 0;
            message[4] = 0;
            message[5] = 0;
            message[6] = 0;
            message[7] = 0;

        } else {
            // Power set option (Range: -100 - 100)
            message[3] = (byte) speed;
            // Mode byte (Bit-field): MOTORON + BREAK
            message[4] = 0x03;
            // Regulation mode: REGULATION_MODE_MOTOR_SPEED set back to 0x01 when finished testing
            message[5] = 0x02; 
            // Turn Ratio (SBYTE; -100 - 100)
            message[6] = 0x00;
            // RunState: MOTOR_RUN_STATE_RUNNING
            message[7] = 0x20;
        }

        // TachoLimit: run forever
        message[8] = 0;
        message[9] = 0;
        message[10] = 0;
        message[11] = 0;

        return message;

    }

    public static byte[] getMotorMessage(boolean replyRequired, int motor, int speed, int end) {
        byte[] message = getMotorMessage(replyRequired, motor, speed);

        // TachoLimit
        message[8] = (byte) end;
        message[9] = (byte) (end >> 8);
        message[10] = (byte) (end >> 16);
        message[11] = (byte) (end >> 24);

        return message;
    }

    public static byte[] getResetMessage(int motor) {
        byte[] message = new byte[4];

        message[0] = DIRECT_COMMAND_NOREPLY;
        message[1] = RESET_MOTOR_POSITION;
        // Output port
        message[2] = (byte) motor;
        // absolute position
        message[3] = 0;

        return message;
    }

    public static byte[] getStartProgramMessage(boolean replyRequired, String programName) {
        byte[] message = new byte[22];

        message[0] = getDirectCommandTypePrefix(replyRequired);
        message[1] = START_PROGRAM;

        // copy programName and end with 0 delimiter
        for (int pos=0; pos<programName.length(); pos++)
            message[2+pos] = (byte) programName.charAt(pos);

        message[programName.length()+2] = 0;

        return message;
    }

    public static byte[] getStopProgramMessage(boolean replyRequired) 
    {
        byte[] message = new byte[2];

        message[0] = getDirectCommandTypePrefix(replyRequired);
        message[1] = STOP_PROGRAM;

        return message;
    }
    
    public static byte[] getProgramNameMessage(boolean replyRequired) 
    {
        byte[] message = new byte[2];

        message[0] = getDirectCommandTypePrefix(replyRequired);
        message[1] = GET_CURRENT_PROGRAM_NAME;

        return message;
    }
    
    public static byte[] getBatteryLevelMessage()
    {
    	return new byte[]{DIRECT_COMMAND_REPLY,
    					  GET_BATTERY_LEVEL};
    }

    public static byte[] getOutputStateMessage(int motor) {
        byte[] message = new byte[3];

        message[0] = DIRECT_COMMAND_REPLY;
        message[1] = GET_OUTPUT_STATE;
        // Output port
        message[2] = (byte) motor;

        return message;
    }
    
    public static byte[] getResetMotorTachMessage(boolean replyRequired, int port, TachRelation relation )
    {
    	return new byte[]
    			{
    				getDirectCommandTypePrefix(replyRequired),
    				RESET_MOTOR_POSITION,
    				(relation == TachRelation.RELATIVE_TO_LAST_MOVEMENT)?(byte)1:(byte)0
    			};
    }

    public static byte[] getFirmwareVersionMessage() {
        byte[] message = new byte[2];

        message[0] = SYSTEM_COMMAND_REPLY;
        message[1] = GET_FIRMWARE_VERSION;

        return message;
    }

    public static byte[] getKeepAliveMessage(boolean replyRequired) {
        byte[] message = new byte[2];

        message[0] = getDirectCommandTypePrefix(replyRequired);
        message[1] = KEEP_ALIVE;

        return message;
    }
    
    public static byte[] getFindFilesMessage(boolean findFirst, int handle, String searchString) {
        byte[] message;

        if (findFirst)
            message = new byte[22];

        else
            message = new byte[3];

        message[0] = SYSTEM_COMMAND_REPLY;

        if (findFirst) {
            message[1] = FIND_FIRST;

            // copy searchString and end with 0 delimiter
            for (int pos=0; pos<searchString.length(); pos++)
                message[2+pos] = (byte) searchString.charAt(pos);

            message[searchString.length()+2] = 0;

        } else {
            message[1] = FIND_NEXT;
            message[2] = (byte) handle;
        }

        return message;
    }

    public static byte[] getOpenWriteMessage(String fileName, int fileLength) {
        byte[] message = new byte[26];

        message[0] = SYSTEM_COMMAND_REPLY;
        message[1] = OPEN_WRITE;
        
        // copy programName and end with 0 delimiter
        for (int pos=0; pos<fileName.length(); pos++)
            message[2+pos] = (byte) fileName.charAt(pos);

        message[fileName.length()+2] = 0;
        // copy file size
        message[22] = (byte) fileLength;
        message[23] = (byte) (fileLength >>> 8);
        message[24] = (byte) (fileLength >>> 16);
        message[25] = (byte) (fileLength >>> 24);        
        return message;
    }

    public static byte[] getWriteMessage(int handle, byte[] data, int dataLength) {
        byte[] message = new byte[dataLength + 3];

        message[0] = SYSTEM_COMMAND_REPLY;
        message[1] = WRITE;
        
        // copy handle
        message[2] = (byte) handle;
        // copy data
        System.arraycopy(data, 0, message, 3, dataLength);

        return message;
    }
    
    public static byte[] getCloseMessage(boolean replyRequired, int handle) {
        byte[] message = new byte[3];

        message[0] = getDirectCommandTypePrefix(replyRequired);
        message[1] = CLOSE;
        
        // copy handle
        message[2] = (byte) handle;

        return message;
    }
    
    public static byte getDirectCommandTypePrefix(boolean replyRequired)
    {
    	if (replyRequired)
    		return DIRECT_COMMAND_REPLY;
    	else
    		return DIRECT_COMMAND_NOREPLY;
    }
    
    public static int convertUnsignedByteArrayToInt(byte[] data, int start) {
		byte lsb = data[start];
		byte msb = data[start + 1];
		return (lsb & 0xFF) | ((msb & 0xFF) << 8);
	}

	public static int byteToInt(byte byteValue) {
		int intValue = (byteValue & (byte) 0x7f);

		if ((byteValue & (byte) 0x80) != 0)
			intValue |= 0x80;

		return intValue;
	}
    
	/**
	 * Helper method to concatenate two byte arrays
	 * 
	 * @param array1
	 *            the first array (e.g. a request)
	 * @param array2
	 *            the second array (e.g. an extra parameter)
	 * 
	 * @return the concatenated array
	 */
	// 1.2.1.1.1
	public static byte[] appendBytes(byte[] array1, byte[] array2) {
		byte[] array = new byte[array1.length + array2.length];
		System.arraycopy(array1, 0, array, 0, array1.length);
		System.arraycopy(array2, 0, array, array1.length, array2.length);
		return array;
	}
	
    public static byte[] getSwitchPortConfigureCommand(int port, boolean replyRequired)
    {
    	return new byte[]{getDirectCommandTypePrefix(replyRequired),
    								SET_INPUT_MODE,
    								(byte)port,
    								SWITCH,
    								BOOLEANMODE};
    }
    
    public static byte[] getSonarPortConfigureCommand(int port, boolean replyRequired)
    {
    	return new byte[]{getDirectCommandTypePrefix(replyRequired),
    								SET_INPUT_MODE,
    								(byte)port,
    								LOWSPEED_9V,
    								RAW_MODE};
    }
    
    
}
