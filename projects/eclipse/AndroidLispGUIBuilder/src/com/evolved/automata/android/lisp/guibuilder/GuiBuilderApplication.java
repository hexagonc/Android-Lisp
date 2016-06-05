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

import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.android.AppStateManager;
import com.evolved.automata.android.DeviceInfo;
import com.evolved.automata.android.lisp.AndroidLispInterpreter;
import com.evolved.automata.android.lisp.guibuilder.events.EventManager;
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

public class GuiBuilderApplication extends Application
{

	
	GlobalInterface _data = null;
	
	static String _DEFAULT_CONTEXT = "default";
	static String _INSERT_KEY = "insert into objects (name, context, value, last_access) values (?,?, ?, ?)"; // 1
	
	static String _TEST_KEY = "select rowid from objects where name = ? and context = ?";
	static String _SELECT_DATA_KEY = "select value, rowid from objects where name = ? and context = ?";
	static String _SELECT_SPECIFIC_DATA_KEY = "select value from objects where rowid = ?";
	static String _UPDATE_SPECIFIC_DATA_KEY = "UPDATE OBJECTS SET value = ?, last_access =  coalesce(?, last_access) where rowid = ?";
	static String _UPDATE_SPECIFIC_ACCESS_DATA_KEY = "UPDATE OBJECTS SET last_access = ? where rowid = ?";
	
	
	static String _SELECT_ALL_AGED_KEY = "select name, value from objects where context=? and last_access<?";
	
	
	static String _DELETE_DATA_KEY = "delete from objects where name = ? and context=?";
	static String _DELETE_ALL_KEY = "delete from objects";
	static String _DELETE_ALL_CONTEXT_KEY = "delete from objects where context=?";
	static String _SELECT_ALL_NAMES_KEY = "select name from objects where context=?";
	
	static String _DELETE_ALL_AGED_KEY = "delete from objects where context=? and last_access < ?";
	
	
	static String _STANDARD_CREATE_MAIN_TABLE = "create table if not exists objects (name text primary key, context varchar(500),  value text, value_bin blob, last_access integer, primary key (name, context))";
	
	
	boolean rebuildTable = true;
	public SQLiteDatabase appDB;
	
	SQLiteStatement insertStatement = null;
	SQLiteStatement testStatement = null;
	
	
	SQLiteStatement deleteStatement =  null;
	SQLiteStatement deleteValueStatement = null;
	SQLiteStatement updateSpecificStatement = null;
	SQLiteStatement updateSpecificAccessStatement = null;
	SQLiteStatement deleteOldDataStatement = null;
	
	
	SQLiteStatement statement = null;
	
	
	@Override
	public void onCreate() {
		
		super.onCreate();
		
		
		try
		{
			appDB  = openOrCreateDatabase("object-map",Context.MODE_PRIVATE,null);
			appDB.setLocale(Locale.getDefault());
			appDB.setLockingEnabled(true);
			appDB.setVersion(1);
			
			
			if (rebuildTable){
				SQLiteStatement dropStatement = appDB.compileStatement("drop table if exists objects");
				dropStatement.execute();
				
			}
			statement = appDB.compileStatement(_STANDARD_CREATE_MAIN_TABLE);
			statement.execute();
			
			insertStatement = appDB.compileStatement(_INSERT_KEY);
			testStatement = appDB.compileStatement(_TEST_KEY);
			//selectStatement = appDB.compileStatement(_SELECT_DATA_KEY);
			deleteStatement = appDB.compileStatement(_DELETE_ALL_KEY);
			deleteValueStatement = appDB.compileStatement(_DELETE_DATA_KEY);
			
			updateSpecificStatement = appDB.compileStatement(_UPDATE_SPECIFIC_DATA_KEY);
			
			updateSpecificAccessStatement = appDB.compileStatement(_UPDATE_SPECIFIC_ACCESS_DATA_KEY);
			deleteOldDataStatement = appDB.compileStatement(_DELETE_ALL_AGED_KEY);
			
			//selectOldDataStatement = appDB.compileStatement(_SELECT_ALL_AGED_KEY);
			//selectAllDataKeysStatement = appDB.compileStatement(_SELECT_ALL_NAMES_KEY);
			
			
			EventManager.create(this);
			AndroidTools.initialize(null, this, null);
			AppStateManager.create(this);
			NXTBluetoothManager.create(this);
			DeviceInfo.create(this);
			GuiBuilderConfiguration.create(this);
			_data = new GlobalInterface(this);
			
			//_data.getEnvironment().mapFunction("set-data-value", setObjectDataValue(testStatement, insertStatement, updateStatement));
			_data.getEnvironment().mapFunction("set-data-value", setObjectDataValue(appDB, insertStatement, updateSpecificStatement));
			_data.getEnvironment().mapFunction("get-data-value", getObjectDataValue(appDB, updateSpecificAccessStatement));
			_data.getEnvironment().mapFunction("delete-all-data", deleteAllData(deleteStatement));
			_data.getEnvironment().mapFunction("delete-data-value", deleteDataValue(deleteValueStatement));
			_data.getEnvironment().mapFunction("check-data-exists", testDataExists(appDB));
			_data.getEnvironment().mapFunction("delete-old-data", deleteOldData(deleteOldDataStatement));
			_data.getEnvironment().mapFunction("select-old-data-names", selectOldData(appDB));
			_data.getEnvironment().mapFunction("get-all-names", selectAllDataKeys(appDB));
			MenuManager.create(this);
			CodeManager.create(this);
			
		}
		catch (Exception e)
		{
			throw new RuntimeException(e);
		}
		
		
	}
	
	public GlobalInterface getGlobalData()
	{
		return _data;
	}
	
	static SimpleFunctionTemplate testDataExists(final SQLiteDatabase db)
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)testDataExists(db);
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				
				String name = evaluatedArgs[0].getString();
				String context = _DEFAULT_CONTEXT;
				if (evaluatedArgs.length>1)
					context = evaluatedArgs[1].getString();
				Integer rowId;
				
				try
				{
					rowId = testValueExists(db, name, context);
					
					if (rowId != null)
						return NLispTools.makeValue(rowId.longValue());
					else
						return Environment.getNull();
				}
				catch (Exception e)
				{
					throw new RuntimeException(e);
				}
			}
		};
	}
	
	
	static SimpleFunctionTemplate getObjectDataValue(final SQLiteDatabase selectDB, final SQLiteStatement updateSpecificAccessStatement)
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)getObjectDataValue(selectDB, updateSpecificAccessStatement);
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, true, true);
			
				String name = evaluatedArgs[0].getString();
				String context = _DEFAULT_CONTEXT;
				if (evaluatedArgs.length>1)
					context = evaluatedArgs[1].getString();
				Long lastAccess = null;
				if (evaluatedArgs.length > 2 && !evaluatedArgs[2].isNull())
				{
					lastAccess = Long.valueOf(System.currentTimeMillis());
				}
				
				
				
				try
				{
					
					return selectValue(env, selectDB, updateSpecificAccessStatement, name, context, lastAccess);
				}
				catch (Exception e)
				{
					throw new RuntimeException(e);
				}
			}
		};
	}
	
	static SimpleFunctionTemplate deleteDataValue(final SQLiteStatement delete)
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)deleteDataValue(delete);
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
				
				String name = evaluatedArgs[0].getString();
				String context = _DEFAULT_CONTEXT;
				if (evaluatedArgs.length>1)
					context = evaluatedArgs[1].getString();
				
				try
				{
					return NLispTools.makeValue(deleteeValue(delete, name, context));
				}
				catch (Exception e)
				{
					throw new RuntimeException(e);
				}
			}
		};
	}
	
	static SimpleFunctionTemplate deleteAllData(final SQLiteStatement delete)
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)deleteAllData(delete);
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				//checkActualArguments(1, false, true);
			
				//String name = evaluatedArgs[0].getString();
				
				
				try
				{
					return NLispTools.makeValue(deleteAll(delete));
				}
				catch (Exception e)
				{
					throw new RuntimeException(e);
				}
			}
		};
	}
	
	
	static SimpleFunctionTemplate setObjectDataValue(final SQLiteDatabase queryDatabase, final SQLiteStatement insert, final SQLiteStatement update)
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)setObjectDataValue(queryDatabase, insert, update);
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(2, true, true);
			
				String name = evaluatedArgs[0].getString();
				String value = evaluatedArgs[1].serializedForm();
				String context = _DEFAULT_CONTEXT;
				if (evaluatedArgs.length>2)
					context = evaluatedArgs[2].getString();
				
				
				Long timeStamp = System.currentTimeMillis();
				
				Integer rowId;
				int modifyCount = 0;
				
				try
				{
					rowId = testValueExists(queryDatabase, name, context);
					if (rowId != null)
					{
						modifyCount = (int)updateValue(update, value, timeStamp, rowId);
					}
					else
					{
						modifyCount = (int)insertUpdateValue(insert, name, context, value, timeStamp);
					}
					
					return NLispTools.makeValue(modifyCount);
				}
				catch (Exception e)
				{
					throw new RuntimeException(e);
				}
			}
		};
	}
	
	
	static Integer testValueExists(SQLiteDatabase queryDatabase, String name, String context) throws SQLException
	{
		
		Cursor cursor = queryDatabase.rawQuery(_TEST_KEY, new String[]{name, context});
		try
		{
			if (cursor.moveToNext())
			{
				return Integer.valueOf(cursor.getInt(0));
			}
			else
				return null;
		}
		finally
		{
			cursor.close();
		}
		
	}
	
	
	static long insertUpdateValue(SQLiteStatement insert,  String name, String context, String value, long timeStamp) throws SQLException
	{
		insert.bindString(1, name);
		insert.bindString(2, context);
		insert.bindString(3, value);
		insert.bindLong(4, timeStamp);
		
		
		
		return insert.executeInsert();
	}
	
	
	static long updateValue(SQLiteStatement update, String value, Long lastAccess, long rowid) throws SQLException
	{
		update.bindString(1, value);
		
		if (lastAccess == null)
			update.bindNull(2);
		else
			update.bindLong(2, lastAccess.longValue());
		update.bindLong(3, rowid);
		
		
		return update.executeUpdateDelete();
	}
	
	static Value selectAllDataNames(SQLiteDatabase db, String context) throws SQLException
	{
		
		LinkedList<Value> value = new LinkedList<Value>();
		Cursor rs = db.rawQuery(_SELECT_ALL_NAMES_KEY, new String[]{context});
		try
		{
			while (rs.moveToNext())
			{
				value.add(NLispTools.makeValue(rs.getString(0)));
			}
			return NLispTools.makeValue(value.toArray(new Value[0]));
		}
		finally
		{
			rs.close();
		}
		
	}
	
	static SimpleFunctionTemplate selectAllDataKeys(final SQLiteDatabase db)
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)selectAllDataKeys(db);
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
					
				String context = _DEFAULT_CONTEXT;
				if (evaluatedArgs.length>0)
					context = evaluatedArgs[0].getString();
				
				try
				{
					return selectAllDataNames(db, context);
				}
				catch (Exception e)
				{
					throw new RuntimeException(e);
				}
			}
		};
	}
	
	static SimpleFunctionTemplate selectOldData(final SQLiteDatabase db)
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)selectOldData(db);
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
			
				Long age = evaluatedArgs[0].getIntValue();
				String context = _DEFAULT_CONTEXT;
				if (evaluatedArgs.length>1)
					context = evaluatedArgs[1].getString();
				
				try
				{
					return selectOldDataNames(db, context, age);
				}
				catch (Exception e)
				{
					throw new RuntimeException(e);
				}
			}
		};
	}
	
	static boolean deleteAll(SQLiteStatement delete) throws SQLException
	{
		
		
		return delete.executeUpdateDelete() > 0;
	}
	
	static SimpleFunctionTemplate deleteOldData(final SQLiteStatement delete)
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)deleteOldData(delete);
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
			
				Long age = evaluatedArgs[0].getIntValue();
				String context = _DEFAULT_CONTEXT;
				if (evaluatedArgs.length>1)
					context = evaluatedArgs[1].getString();
				
				try
				{
					return NLispTools.makeValue(deleteOldData(delete, context, age));
				}
				catch (Exception e)
				{
					throw new RuntimeException(e);
				}
			}
		};
	}
	

	static int deleteOldData(SQLiteStatement delete, String context, long age) throws SQLException
	{
		
		delete.bindString(1, context);
		delete.bindLong(2, age);
		return delete.executeUpdateDelete();
	}
	
	
	
	static boolean deleteeValue(SQLiteStatement delete, String name, String context) throws SQLException
	{
		
		delete.bindString(1, name);
		delete.bindString(2, context);
		return delete.executeUpdateDelete() > 0;
	}
	
	
	static Value selectOldDataNames(SQLiteDatabase db, String context, long age) throws SQLException
	{
		
		
		
		Cursor rs = db.rawQuery(_SELECT_ALL_AGED_KEY, new String[]{context, "" + age});
		
		LinkedList<Value> out = new LinkedList<Value>();
		
		try
		{
			while (rs.moveToNext())
			{
				
				out.add(NLispTools.makeValue(rs.getString(0)));
				
			}
			return NLispTools.makeValue(out.toArray(new Value[0]));
		}
		finally{
			rs.close();
		}
		
		
	}
	
	static Value selectValue(Environment env, SQLiteDatabase db, SQLiteStatement update, String name, String context, Long lastAccess) throws SQLException, InstantiationException, IllegalAccessException
	{
		
		Cursor rs = db.rawQuery(_SELECT_DATA_KEY, new String[]{name, context});
		
		try
		{
			if (rs.moveToNext())
			{
				String result = rs.getString(0);
				
				if (lastAccess != null)
				{
					Long rowid = rs.getLong(2);
					update.bindLong(1, lastAccess.longValue());
					update.bindLong(2, rowid.longValue());
					update.executeUpdateDelete();
				}
				
				LinkedList<Value> out = env.parse(result, true);
				if (out.size() == 0)
					return Environment.getNull();
				return env.evaluate(out.getFirst());
			}
			else
				return Environment.getNull();
		}
		finally
		{
			rs.close();
		}
		
	}
	
}
