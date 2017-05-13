package com.evolved.automata.android.lisp.guibuilder;

import java.sql.Connection;
import java.sql.DriverManager;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.LinkedList;
import java.util.Locale;

import com.dropbox.core.v2.DbxClientV2;
import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.android.AppStateManager;
import com.evolved.automata.android.DeviceInfo;
import com.evolved.automata.android.lisp.AndroidLispInterpreter;
import com.evolved.automata.android.lisp.guibuilder.events.EventManager;
import com.evolved.automata.android.lisp.guibuilder.v2.*;
import com.evolved.automata.android.lisp.guibuilder.v2.DropboxManager;
import com.evolved.automata.android.mindstorms.NXTBluetoothManager;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.LispInterpreter;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;

import android.app.Application;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteStatement;

public class ALGBApplication extends Application
{


	
	
	@Override
	public void onCreate() {
		
		super.onCreate();
		
		
		try
		{

			EventManager.create(this);
			AndroidTools.initialize(null, this, null);
			AppStateManager.create(this);
			NXTBluetoothManager.create(this);
			DeviceInfo.create(this);
			GuiBuilderConfiguration.create(this);

			MenuManager.create(this);
			CodeManager.create(this);

			com.evolved.automata.android.lisp.guibuilder.v2.DropboxManager.create(getApplicationContext());


		}
		catch (Exception e)
		{
			throw new RuntimeException(e);
		}
		
		
	}
	

	
}
