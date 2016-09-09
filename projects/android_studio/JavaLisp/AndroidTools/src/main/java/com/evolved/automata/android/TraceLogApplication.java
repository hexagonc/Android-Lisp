package com.evolved.automata.android;

import java.util.Calendar;
import java.util.Hashtable;

import android.app.Application;
import android.content.ContentValues;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;



public class TraceLogApplication extends Application 
{
	public Handler _processHandler;
	Thread _queue;
	protected SQLiteOpenHelper _databaseHelper;
	
	static final String LOG_DATABASE_NAME = "trace-db-2";
	static final String LOG_TABLE = "EA_LOG_MAIN";
	static final String LOG_TABLE_CREATE_SQL = String.format("create table if not exists %1$s (_ID INTEGER PRIMARY KEY AUTOINCREMENT, UPDATE_DATE INT, EVENT_SOURCE TEXT, EVENT_TYPE TEXT, TAG TEXT, EXTRA_DATA TEXT )", LOG_TABLE);
	static TraceLogApplication _instance;
	
	public TraceLogApplication()
	{
		super();
		_instance = this;
		_queue = new Thread()
		{
			public void run()
			{
				Looper.prepare();
				_processHandler = new Handler();
				
				Looper.loop();
			}
		};
		_queue.setDaemon(true);
		_queue.start();
	}
	
	static Handler getProcessHandler()
	{
		if (_instance!=null)
			return _instance._processHandler;
		else
			return null;
	}
	
	public void onCreate()
	{
		super.onCreate();
		setupDB();
	}

	public static void logEvent(final String tag, final String event_source, final String event_type, final String extra)
    {
		if (_instance == null)
			return;
		
        Runnable ljob = new Runnable() {
            
            @Override
            public void run()
            {
                SQLiteDatabase db = null;
                try
                {
                    db = _instance._databaseHelper.getWritableDatabase();
                    ContentValues values = new ContentValues();
                    values.put("EVENT_TYPE", event_type);
                    values.put("UPDATE_DATE", (long)Calendar.getInstance().getTimeInMillis());
                    values.put("EVENT_SOURCE", event_source);
                    values.put("TAG", tag);
                    values.put("EXTRA_DATA", extra);
                    db.insert(LOG_TABLE, null, values);
                    Log.e("TraceLogApplication", String.format(":::SQL Inserted row: - [%1$s] [%2$s] [%3$s] [%4$s]", tag, event_source, event_type, extra));
                }
                catch (SQLException se)
                {
                    Log.e("TraceLogApplication", ":::SQL Logging exception: - " + se.toString());
                }
                finally
                {
                    if (db!=null)
                    {
                        db.close();
                    }
                }
            }
        };
        _instance._processHandler.post(ljob);
        
        
    }
    
    
    
    private void setupDB()
    {
      
        _databaseHelper = new SQLiteOpenHelper(this, LOG_DATABASE_NAME, null, 1)
        {

            @Override
            public void onCreate(SQLiteDatabase db)
            {
            	initializeDB(db);
                
            }

            @Override
            public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion)
            {
                
            }
            
        };
    }
    
    private void initializeDB(final SQLiteDatabase db)
    {
    	try
        {
            db.beginTransaction();
            db.execSQL(LOG_TABLE_CREATE_SQL);
            
            db.setTransactionSuccessful();
            db.endTransaction();
            
        }
        catch (SQLException se)
        {
            Log.e("TraceLogApplication", "SQL ERROR::: " + se.toString());
        }
    }
	
    
}
