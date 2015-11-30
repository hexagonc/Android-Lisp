package com.evolved.automata.android;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Calendar;
import java.util.LinkedList;
import java.util.Locale;
import java.io.*;



import android.net.Uri;
import android.os.*;
import android.provider.MediaStore;
import android.text.SpannableString;
import android.text.Spanned;
import android.text.TextUtils;
import android.util.DisplayMetrics;
import android.util.TypedValue;
import android.view.WindowManager;
import android.widget.MultiAutoCompleteTextView;
import android.widget.Toast;
import android.content.*;
import android.content.res.*;
import android.app.*;
import android.database.*;
import android.database.sqlite.*;


public class AndroidTools 
{
	public static final String OLD_PHONE_RINGER_URI_STRING= "content://media/internal/audio/media/24";
	public static final String HAPPY_RINGER_URI_STRING= "content://media/internal/audio/media/25";
	public static final String STANDARD_RINGER_URI_STRING= "content://media/internal/audio/media/28";

	public static final int INTENT_CAMERA=0;
	public static final int INTENT_SPEECH_TO_TEXT=1;
	
	public static String baseLogFileLabel="base";
	public static String baseLogFilePath="/sdcard/";
	
	public static Context appContext;
	public static Activity currentActivity;
	public static Handler currentHandler;
	public static String preferenceName="default";
	public static String databaseName;
	
	public static SQLiteDatabase appDB;
	public static final String STRING_ARRAY_SEPARATOR="@z_X_Z@@";
	public static LinkedList<ActivityResultHandler> activityHandlers;
	public static String camera_media_path;
	
	
	public static void initialize(String baseLogPath, Context appContext, String databaseName)
	{
		AndroidTools.databaseName=databaseName;
		AndroidTools.appContext=appContext;
		if (baseLogPath!=null)
			createIfNotExists(baseLogPath);
		AndroidTools.baseLogFilePath=baseLogPath;
		if (databaseName!=null)
			appDB=getDataBase(databaseName);
		else
			appDB=null;
		
	}
	
	
	
	
	
	public static int convertDPtoPX(Context context, int dp)
	{
		DisplayMetrics dm = new DisplayMetrics();
		WindowManager m = (WindowManager)context.getSystemService(Context.WINDOW_SERVICE);
		m.getDefaultDisplay().getMetrics(dm);
		return (int)TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, dp, dm);
	}
	
	public static void initializeRequiredPaths(String[] paths)
	{
		for (String path:paths)
		{
			createIfNotExists(path);
		}
	}
	
	public static void createIfNotExists(String directoryName)
	{
		File f = new File(directoryName);
		if (!f.exists())
		{
			f.mkdirs();
		}
	}
	
	
	
	public static void updateActivity(Activity currentActivity, Handler currentHandler, String logLabel)
	{
		AndroidTools.currentHandler=currentHandler;
		AndroidTools.currentActivity=currentActivity;
		AndroidTools.baseLogFileLabel=logLabel;
		
		activityHandlers = new LinkedList<ActivityResultHandler>();
	}
	
	public static void addActivityHandler(ActivityResultHandler handler)
	{
		activityHandlers.add(handler);
	}
	
	public static void processActivityResults(int requestCode, int resultCode, Intent data)
	{
		for (ActivityResultHandler handler: activityHandlers)
		{
			if (handler.onActivityResult(requestCode, resultCode, data))
				break;
		}
	}
	
	
	
	public static void sendMessageToCurrentHandler(Bundle messageBundle)
	{
		if (currentHandler!=null)
		{
			Message requestMessage = currentHandler.obtainMessage();
			requestMessage.setData(messageBundle);
			currentHandler.sendMessage(requestMessage);
		}
	}
	
	
	public static void executeNonQuery(SQLiteDatabase database, String query, String...params)
	{
		try
		{
//			String[] escaped = new String[params.length];
//			for (int i=0;i<params.length;i++)
//				escaped[i]=escapeDBString(params[i]);
			
			database.execSQL(query, params);
		}
		catch (Exception e)
		{
			String detailedException = getDetailedExceptionText(e);
			logMessage(detailedException);
			throw  new RuntimeException(detailedException);
		}
	}
	
	public static void executeNonQuery(String query,String...params)
	{
		try
		{
//			String[] escaped = new String[params.length];
//			for (int i=0;i<params.length;i++)
//				escaped[i]=escapeDBString(params[i]);
			appDB.execSQL(query,params);
		}
		catch (Exception e)
		{
			String detailedException = getDetailedExceptionText(e);
			logMessage(detailedException);
			throw  new RuntimeException(detailedException);
		}
	}
	
	public static Bundle[] executeQuery(SQLiteDatabase database, String sql, String...params)
	{
		SQLiteCursor cursor=null;
		Bundle[] rows=new Bundle[0];
		int rowCount, counter=0;
		try
		{
//			String[] escaped = new String[params.length];
//			for (int i=0;i<params.length;i++)
//				escaped[i]=escapeDBString(params[i]);
			
			cursor = (SQLiteCursor)database.rawQuery(sql, params);
			rowCount = cursor.getCount();
			String[] columnNames;
			if (rowCount>0)
			{
				rows = new Bundle[rowCount];
				columnNames = cursor.getColumnNames();
				while (cursor.moveToNext())
				{
					rows[counter]= new Bundle();
					for (int i=0;i<columnNames.length;i++)
					{
						rows[counter].putString(columnNames[i], cursor.getString(i));
					}
					counter++;
				}
			}
		}
		catch (Exception e)
		{
			String detailedException = getDetailedExceptionText(e);
			logMessage(detailedException);
			
		}
		finally
		{
			if (cursor!=null)
				cursor.close();
		}
		return rows;
	}
	
	
	
	public static Bundle[] executeQuery(String sql, String...params)
	{
		SQLiteCursor cursor=null;
		Bundle[] rows=new Bundle[0];
		int rowCount, counter=0;
		try
		{
//			String[] escaped = new String[params.length];
//			for (int i=0;i<params.length;i++)
//				escaped[i]=escapeDBString(params[i]);
			cursor = (SQLiteCursor)appDB.rawQuery(sql, params);
			rowCount = cursor.getCount();
			String[] columnNames;
			if (rowCount>0)
			{
				rows = new Bundle[rowCount];
				columnNames = cursor.getColumnNames();
				while (cursor.moveToNext())
				{
					rows[counter]= new Bundle();
					for (int i=0;i<columnNames.length;i++)
					{
						rows[counter].putString(columnNames[i], cursor.getString(i));
					}
					counter++;
				}
			}
		}
		catch (Exception e)
		{
			String detailedException = getDetailedExceptionText(e);
			logMessage(detailedException);
			throw  new RuntimeException(detailedException);
		}
		finally
		{
			if (cursor!=null)
				cursor.close();
		}
		return rows;
	}
	
	
	public static String isNull(String data, String effectiveValue)
	{
		return (data==null)?effectiveValue:data;
	}
	
	public static String isNull(String data,String alternativenull, String effectiveValue)
	{
		return (data==null||data.equals(alternativenull))?effectiveValue:data;
	}
	
	public static int castInt(String data)
	{
		return Integer.parseInt(isNull(data,"","0"));
	}
	
	public static int getCurrentDateInt()
	{
		Calendar calendar = Calendar.getInstance();
		int yearMultiplier=10^4;
		int monthMultiplier=10^2;
		int year = calendar.get(calendar.YEAR);
		int day = calendar.get(calendar.DAY_OF_MONTH)+1;
		int month = calendar.get(calendar.MONTH);
		int totalDate = year*yearMultiplier+month*monthMultiplier+day;
		return totalDate;
		
	}
	
	
	public static String getCurrentDateString()
	{
		Calendar calendar = Calendar.getInstance();
		int yearMultiplier=10000;
		int monthMultiplier=100;
		int year = calendar.get(calendar.YEAR);
		int day = calendar.get(calendar.DAY_OF_MONTH);
		int month = calendar.get(calendar.MONTH)+1;
		int totalDate = year*yearMultiplier+month*monthMultiplier+day;
		return ""+totalDate;
		
	}
	
	public static String getCurrentDateString(int dateField, int offset)
	{
		Calendar calendar = Calendar.getInstance();
		calendar.add(dateField, offset);
		int yearMultiplier=10000;
		int monthMultiplier=100;
		int year = calendar.get(calendar.YEAR);
		int day = calendar.get(calendar.DAY_OF_MONTH);
		int month = calendar.get(calendar.MONTH)+1;
		int totalDate = year*yearMultiplier+month*monthMultiplier+day;
		return ""+totalDate;
		
	}
	
	public static String getCurrentDateTimeString()
	{
		Calendar calendar = Calendar.getInstance();
		
		int yearMultiplier=10000;
		int monthMultiplier=100;
		int year = calendar.get(calendar.YEAR);
		int day = calendar.get(calendar.DAY_OF_MONTH);
		int month = calendar.get(calendar.MONTH)+1;
		int hour = calendar.get(calendar.HOUR_OF_DAY);
		int minute = calendar.get(calendar.MINUTE);
		int second = calendar.get(calendar.SECOND);
		
		
		return String.format("%1$s/%2$s/%3$s %4$s:%5$s:%6$s", month,day, year,hour, minute,second);
		
	}
	
	public static SQLiteDatabase getDataBase(String dbName)
	{
		SQLiteDatabase dataBase;
		dataBase = appContext.openOrCreateDatabase(dbName,Context.MODE_PRIVATE,null);
		dataBase.setLocale(Locale.getDefault());
		dataBase.setLockingEnabled(true);
		dataBase.setVersion(1);
		return dataBase;
	}
	
	public static SQLiteDatabase getDataBase()
	{
		SQLiteDatabase dataBase;
		dataBase = appContext.openOrCreateDatabase(databaseName,Context.MODE_PRIVATE,null);
		dataBase.setLocale(Locale.getDefault());
		dataBase.setLockingEnabled(true);
		dataBase.setVersion(1);
		return dataBase;
	}
	
	public static String getStringResource(int resourceId)
	{
		return appContext.getResources().getString(resourceId);
	}
	
	public static String[] getStringArrayPreferenceSetting(String settingName)
    {
    	SharedPreferences preferences = appContext.getSharedPreferences(preferenceName, Context.MODE_PRIVATE);
    	String baseValue = preferences.getString(settingName, "");
    	if (baseValue.length()==0)
    		return new String[0];
    	else
    		return baseValue.split(STRING_ARRAY_SEPARATOR);
    }
	
	public static String getStringPreferenceSetting(String settingName, String defaultIfNotDefined)
    {
    	SharedPreferences preferences = appContext.getSharedPreferences(preferenceName, Context.MODE_PRIVATE);
    	return preferences.getString(settingName, defaultIfNotDefined);
    }
    
    public static boolean getBooleanPreferenceSetting(String settingName, boolean defaultIfNotDefined)
    {
    	SharedPreferences preferences = appContext.getSharedPreferences(preferenceName, Context.MODE_PRIVATE);
    	return preferences.getBoolean(settingName, defaultIfNotDefined);
    }
    
    public static int getIntPreferenceSetting(String settingName, int defaultIfNotDefined)
    {
    	SharedPreferences preferences = appContext.getSharedPreferences(preferenceName, Context.MODE_PRIVATE);
    	return preferences.getInt(settingName, defaultIfNotDefined);
    }
    
    public static void setStringPreferenceSetting(String settingName, String newValue)
    {
    	SharedPreferences preferences = appContext.getSharedPreferences(preferenceName, Context.MODE_PRIVATE);
    	SharedPreferences.Editor edit = preferences.edit();
    	edit.putString(settingName, newValue);
    	edit.commit();
    }
    
    public static void setStringArrayPreferenceSetting(String settingName, String[] newValue)
    {
    	SharedPreferences preferences = appContext.getSharedPreferences(preferenceName, Context.MODE_PRIVATE);
    	SharedPreferences.Editor edit = preferences.edit();
    	edit.putString(settingName, StandardTools.join(newValue, STRING_ARRAY_SEPARATOR));
    	edit.commit();
    }
    
    public static void setBooleanPreferenceSetting(String settingName, boolean newValue)
    {
    	SharedPreferences preferences = appContext.getSharedPreferences(preferenceName, Context.MODE_PRIVATE);
    	SharedPreferences.Editor edit = preferences.edit();
    	edit.putBoolean(settingName, newValue);
    	edit.commit();
    }
    
    public static void setIntPreferenceSetting(String settingName, int newValue)
    {
    	SharedPreferences preferences = appContext.getSharedPreferences(preferenceName, Context.MODE_PRIVATE);
    	SharedPreferences.Editor edit = preferences.edit();
    	edit.putInt(settingName, newValue);
    	edit.commit();
    }
    
    
    
    public static InputStream getFileAsset(String assetPath) throws IOException
    {
    	return appContext.getAssets().open(assetPath);
    }
    
    public static InputStream getFileAsset(String assetPath, Context context) throws IOException
    {
    	return context.getAssets().open(assetPath);
    }
    
    public static String getFileAssetAsString(String assetPath, Context context) throws IOException
    {
    	BufferedReader breader = null;
		StringBuilder sbuilder = new StringBuilder();
		try
		{
			breader = new BufferedReader(new InputStreamReader(getFileAsset(assetPath)));
			String line;
			while ((line = breader.readLine())!=null)
				sbuilder.append(line).append("\n");
			return sbuilder.toString();
		}
		finally
		{
			if (breader!=null)
				breader.close();
		}
    }
    
    public static void writeAssetToLocalStorage(Context context, String assetPath, String filePath) throws IOException
    {
    	InputStream istream = getFileAsset(assetPath, context);
    	File f = new File(filePath);
    	if (!f.exists())
    	{
    		f.createNewFile();
    	}
    	FileOutputStream finputStream = null;
    	try
    	{
    		finputStream = new FileOutputStream(f);
    		int data;
        	while ((data = istream.read())!=-1)
        	{
        		finputStream.write(data);
        	}
    	}
    	finally
    	{
    		if (finputStream!=null)
    			finputStream.close();
    		istream.close();
    	}
    	
    	
    }
    
    public static boolean fileAssetExists(String assetFileFullName)
    {
    	try
    	{
    		InputStream f = getFileAsset(assetFileFullName);
    		f.close();
    	}
    	catch (IOException ioe)
    	{
    		if (ioe instanceof FileNotFoundException)
    			return false;
    		else 
    			throw new RuntimeException(getDetailedExceptionText(ioe));
    	}
    	return true;
    }
    
    
    public static void writeToFile(String logFileFullName, String text)
	{
		writeToFile(logFileFullName,text,false);
	}

	
	public static void writeToFile(String logFileFullName, String text,boolean throwException)
	{
		BufferedWriter writer=null;
		try
		{
			writer= new BufferedWriter(new FileWriter(logFileFullName,true));
			writer.write(text);
			writer.newLine();
		}
		catch (Exception e)
		{
			if (throwException)
			{
				throw new RuntimeException(getDetailedExceptionText(e));
			}
		}
		finally
		{
			if (writer!=null)
			{
				try
				{
					writer.close();
				}
				catch (Exception e)
				{
					
				}
			}
		}
	}
	
	
	public static void logMessage(String messageText)
	{
		logMessage(messageText,baseLogFileLabel);
	}
	
	public static void logMessage(String messageText, String logLabel)
	{
		String logFileFullName = String.format("%1$s%2$s_%3$s.txt", baseLogFilePath,logLabel,getCurrentDateString() );
		
		writeToFile(logFileFullName,messageText);
	}
	
	public static String getDetailedExceptionText(Exception e)
	{
		java.io.StringWriter traceText = new java.io.StringWriter();
		java.io.PrintWriter pWriter = new java.io.PrintWriter(traceText,true);
		e.printStackTrace(pWriter);
		pWriter.close();
		return traceText.toString();
	}
    
	public static void logError(Exception e)
	{
		logMessage(getDetailedExceptionText(e));
	}
	
	public static String escapeDBString(String data)
	{
		if (data==null)
			return "";
		else
			return data.replace((CharSequence)"'", (CharSequence)"''");
	}
	
	public static Intent makeCameraImageIntent(String outputfile)
	{
		String actionName = MediaStore.ACTION_IMAGE_CAPTURE;
		File file = new File( outputfile );
		Uri outputFileUri = Uri.fromFile( file );
		camera_media_path=outputfile;
		Intent intent = new Intent(actionName);
		intent.putExtra( MediaStore.EXTRA_OUTPUT, outputFileUri );
	    return intent;
	}
	
	
	public static Intent makeCameraVideoIntent(String outputfile)
	{
		String actionName = MediaStore.ACTION_VIDEO_CAPTURE;
		File file = new File( outputfile );
		Uri outputFileUri = Uri.fromFile( file );
		camera_media_path=outputfile;
		Intent intent = new Intent(actionName);
		intent.putExtra( MediaStore.EXTRA_OUTPUT, outputFileUri );
	    return intent;
	}
	
	public static String getSdCardPath() {
        return Environment.getExternalStorageDirectory().getPath() + "/";
    }
	
	public static String getCameraIntentOutputPath(Intent cameraResultIntent)
	{
//		Uri resultData = cameraResultIntent.getData();
//		return resultData.getPath();
		return camera_media_path;
	}
	
	public static void showStatusBarNotification(Context context, int notificationId, 
			String statusBarRollingMessage, String title, 
			String text, int iconResourceId,  Intent returnIntent)
	{
		NotificationManager nmanager = (NotificationManager)context.getSystemService(Context.NOTIFICATION_SERVICE);
		Notification notification = new Notification(iconResourceId, statusBarRollingMessage,0);
		
		PendingIntent pIntent = PendingIntent.getActivity(context, 0, null, 0);
		notification.setLatestEventInfo(context, title, text, pIntent);
		notification.flags|=Notification.FLAG_AUTO_CANCEL;
		notification.defaults|=Notification.DEFAULT_LIGHTS;
		nmanager.notify(notificationId, notification);
	}
	
	public static String makeSQLiteDateString(Calendar cal)
	{

		String formatted = String.format("%1$s-%2$s-%3$s %4$s:%5$s", 
				cal.get(Calendar.YEAR), 
				zeroPad(cal.get(Calendar.MONTH)+1,2),
				zeroPad(cal.get(Calendar.DAY_OF_MONTH),2),
				zeroPad(cal.get(Calendar.HOUR_OF_DAY),2),
				zeroPad(cal.get(Calendar.MINUTE),2));
		return formatted;
		
		
	}
	
	public static String makeDateString(Calendar cal)
	{

		String formatted = String.format("%1$s/%2$s/%3$s %4$s:%5$s",
				zeroPad(cal.get(Calendar.MONTH)+1,2),
				zeroPad(cal.get(Calendar.DAY_OF_MONTH),2),
				cal.get(Calendar.YEAR), 
				zeroPad(cal.get(Calendar.HOUR_OF_DAY),2),
				zeroPad(cal.get(Calendar.MINUTE),2));
		return formatted;
		
		
	}
	
	public static  void showMessageToast(String message, Context con)
	{
		Toast t = Toast.makeText(con, message, Toast.LENGTH_LONG);
		t.show();

	}
	
	public static void showshortMessageToast(String message, Context con)
	{
		Toast t = Toast.makeText(con, message, Toast.LENGTH_SHORT);
		t.show();

	}
	
	public static String zeroPad(int wholeNumber, int width)
	{
		StringBuilder sBuilder = new StringBuilder();
		int digits=0;
		if (wholeNumber==0)
			digits=1;
		else
			digits = 1+ ((int)Math.log10(Math.abs(wholeNumber)));
		
		for (int i=0;i<width-digits;i++)
		{
			sBuilder.append("0");
		}
		
		sBuilder.append(""+wholeNumber);
		return sBuilder.toString();
	}
	
	public static void sendEmail(Activity con, String[] recipient, String subject, String body, String attachmentFile )
	{
		Intent i = new Intent(Intent.ACTION_SEND);
		i.setType("text/plain");
		i.putExtra(Intent.EXTRA_EMAIL  , recipient);
		i.putExtra(Intent.EXTRA_SUBJECT, subject);
		i.putExtra(Intent.EXTRA_TEXT   , body);
		Uri uri = Uri.fromFile(new File(attachmentFile));
		i.putExtra(android.content.Intent.EXTRA_STREAM, uri);
		
		
		try {
		    con.startActivity(Intent.createChooser(i, "Send mail..."));
		} catch (android.content.ActivityNotFoundException ex) {
		    Toast.makeText(con, "There are no email clients installed.", Toast.LENGTH_SHORT).show();
		}

		
	}
	
	
	
	public static  MultiAutoCompleteTextView.Tokenizer getSpaceTokenizer()
	{
		return new MultiAutoCompleteTextView.Tokenizer()
		{


			public int findTokenStart(CharSequence text, int cursor) 
			{
			int i = cursor;
			
			while (i > 0 && text.charAt(i - 1) != ' ') {
			    i--;
			}
			while (i < cursor && text.charAt(i) == ' ') {
			    i++;
			}
			
			return i;
			}
			
			public int findTokenEnd(CharSequence text, int cursor) {
			int i = cursor;
			int len = text.length();
			
			while (i < len) {
			    if (text.charAt(i) == ' ') {
			        return i;
			    } else {
			        i++;
			    }
			}
			
			return len;
			}
			
			public CharSequence terminateToken(CharSequence text) {
			int i = text.length();
			
			while (i > 0 && text.charAt(i - 1) == ' ') {
			    i--;
			}
			
			if (i > 0 && text.charAt(i - 1) == ' ') {
			    return text;
			} else {
			    if (text instanceof Spanned) {
			        SpannableString sp = new SpannableString(text + " ");
			        TextUtils.copySpansFrom((Spanned) text, 0, text.length(),
			                Object.class, sp, 0);
			        return sp;
			    } else {
			        return text + " ";
			    }
			}
			}

		};
	}
}
