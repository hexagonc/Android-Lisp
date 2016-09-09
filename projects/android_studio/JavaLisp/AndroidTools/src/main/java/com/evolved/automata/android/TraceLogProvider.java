package com.evolved.automata.android;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;


import android.content.ContentProvider;
import android.content.ContentValues;
import android.content.UriMatcher;
import android.content.res.Resources;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.net.Uri;
import android.util.Log;

public class TraceLogProvider extends ContentProvider 
{
	SQLiteOpenHelper _databaseHelper;
    Resources _resources;
    final int defaultMaxReturn = 300;
    final int RETURN_ALL_ROWS = 0;
    final int RETURN_TOP_N = 1;
    final int CLEAR_LOG = 2;
    UriMatcher _matcher;
    public String TRACE_AUTHORITY = "com.evolved.automata.trace";
    
    
    
    public TraceLogProvider()
    {
    	
    }
    
    @Override
    public boolean onCreate()
    {
    	TRACE_AUTHORITY = getContext().getPackageName() + ".provider.trace";
        _resources = getContext().getResources();
        _databaseHelper = new SQLiteOpenHelper(getContext(), TraceLogApplication.LOG_DATABASE_NAME, null, 1)
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
        
        _matcher = new UriMatcher(UriMatcher.NO_MATCH);
        _matcher.addURI(TRACE_AUTHORITY, "log", RETURN_ALL_ROWS);
        _matcher.addURI(TRACE_AUTHORITY, "log/#", RETURN_TOP_N);
        _matcher.addURI(TRACE_AUTHORITY, "clear", CLEAR_LOG);
        return true;
    }

    private void initializeDB(final SQLiteDatabase db)
    {
        Runnable gjob = new Runnable() {
            
            @Override
            public void run()
            {
                try
                {
                    db.beginTransaction();
                    db.execSQL(TraceLogApplication.LOG_TABLE_CREATE_SQL);
                    
                    db.setTransactionSuccessful();
                    db.endTransaction();
                }
                catch (SQLException se)
                {
                    
                    Log.e("TraceLogProvider", "SQL ERROR::: " + se.toString());
                }
                
            }
        };
        TraceLogApplication.getProcessHandler().post(gjob);
    }
    
    
    @Override
    public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs,
                        String sortOrder)
    {
    	
        StringBuilder columnBuilder = new StringBuilder();
        boolean first = true;
        String project = "*";
        if (projection!=null && projection.length>0)
        {
            for (String columnname:projection)
            {
                if (first)
                {
                    first = false;
                    columnBuilder.append(columnname);
                }
                else
                    columnBuilder.append(", ").append(columnname);
            }
            project = columnBuilder.toString();
        }
        int limit = defaultMaxReturn;
        
        switch (_matcher.match(uri))
        {
            case RETURN_ALL_ROWS:
                break;
            case RETURN_TOP_N:
                List<String> parts = uri.getPathSegments();
                String count = parts.get(parts.size()-1);
                limit = Integer.parseInt(count);
                break;
            
            default:
                throw new IllegalArgumentException("Invalid provider URI: " + uri.toString());
        }
        String baseStatement = "select %1$s from " + TraceLogApplication.LOG_TABLE + " order by UPDATE_DATE desc limit %2$s";
        String fullStatement = String.format(baseStatement, project, limit);
        SQLiteDatabase db = _databaseHelper.getReadableDatabase();
        
        return db.rawQuery(fullStatement, null);
    }

    @Override
    public String getType(Uri uri)
    {
        
        return null;
    }

    @Override
    public Uri insert(Uri uri, ContentValues values)
    {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public int delete(Uri uri, String selection, String[] selectionArgs)
    {
    	if (_matcher.match(uri) == CLEAR_LOG)
    	{
    		SQLiteDatabase db = _databaseHelper.getReadableDatabase();
        	try
        	{
        		db.beginTransaction();
            	db.delete(TraceLogApplication.LOG_TABLE, null, null);
            	db.setTransactionSuccessful();
            	db.endTransaction();
            	return 0;
        	}
        	catch (Exception e)
        	{
        		Log.e("TraceLogProvider", "Exception::: " + e.toString());
        	}
    	}
    	return -1;
    }

    @Override
    public int update(Uri uri, ContentValues values, String selection, String[] selectionArgs)
    {
        // TODO Auto-generated method stub
        return 0;
    }

    
    
    
}
