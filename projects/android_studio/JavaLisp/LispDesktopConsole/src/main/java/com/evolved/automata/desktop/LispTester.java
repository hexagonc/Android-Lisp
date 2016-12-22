package com.evolved.automata.desktop;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FileFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.IncompleteLispExpressionException;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.lisp.nn.NeuralNetLispInterface;
import com.evolved.automata.lisp.speech.SpeechLispFunctions;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;

import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.LinkedList;
import java.util.concurrent.ScheduledThreadPoolExecutor;


public class LispTester {

	public static ScheduledThreadPoolExecutor _executor = new ScheduledThreadPoolExecutor(1);
	
	public static Runnable getWorkItem(final AIInfoDisplay display, final Environment top, final Value input, final LinkedList<Value> remaining)
	{
		return new Runnable(){
			public void run()
			{
				try {
					Value result = top.evaluate(input, false);
					
					if (result.isContinuation())
					{
						display.setTitle("New Lisp, continuing...");
						_executor.submit(getWorkItem(display, top, result.getContinuingFunction(), remaining));
					}
					else {
						if (remaining!= null && remaining.size() > 0)
						{
							_executor.submit(getWorkItem(display, top, remaining.removeFirst(), remaining));
						}
						else
						{
							displayValue(display, result);
						}
					}
					
				} catch (Exception e){
					System.out.println(e.toString());
				}
			}
		};
	}
	
	
	
	
//	public static Runnable getWorkItem(final AIInfoDisplay display, final Environment top, final FunctionTemplate function)
//	{
//		return new Runnable(){
//			public void run()
//			{
//				try {
//					Value result = function.evaluate(top, true);
//					
//					if (result.isContinuation())
//					{
//						getWorkItem(display, top, result.getContinuingFunction(), null);
//					}
//					else {
//						displayValue(display, result);
//					}
//					
//				} catch (Exception e){
//					System.out.println(e.toString());
//				}
//			}
//		};
//	}
	
	public static Runnable getWorkItem(final AIInfoDisplay display, final Environment top, final FunctionTemplate function, final LinkedList<Value> remaining)
	{
		return new Runnable(){
			public void run()
			{
				try {
					Value result = function.evaluate(top, true);
					
					if (result.isContinuation())
					{
						_executor.submit(getWorkItem(display, top, result.getContinuingFunction(), remaining));
					}
					else {
						if (remaining!= null && remaining.size() > 0)
						{
							_executor.submit(getWorkItem(display, top, remaining.removeFirst(), remaining));
						}
						else
						{
							if (_executor.getQueue().size() == 0)
								display.setTitle("New Lisp");
							displayValue(display, result);
						}
					}
					
				} catch (Exception e){
					displayError(display, e);
				}
			}
		};
	}
	
	private static void displayValue(AIInfoDisplay display, Value result)
	{
		if (result!=null)
			display.DisplayMessageLine(result.toString(), true);
	}
	
	private static void displayError(AIInfoDisplay display, Exception result)
	{
		if (result!=null)
			display.DisplayMessageLine(result.toString(), true);
	}
	
	static SimpleFunctionTemplate  getPrintln(final AIInfoDisplay display)
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)getPrintln(display);
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
			
				StringBuilder sBuilder = new StringBuilder();
				for (int i = 0;i<evaluatedArgs.length;i++)
				{
					sBuilder.append((evaluatedArgs[i].isString())?evaluatedArgs[i].getString():evaluatedArgs[i].toString());
				}
				String out = sBuilder.toString();
				display.DisplayMessageLine(out, true);
				
				return NLispTools.makeValue(out);
			}
		};
	}
	
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
	
	
	static String _STANDARD_CREATE_MAIN_TABLE = "create table if not exists objects (name text, context varchar(500),  value text, value_bin blob, last_access integer, primary key (name, context))";
	
	
	
	
	static SimpleFunctionTemplate getObjectDataValue(final PreparedStatement selectState, final PreparedStatement updateSpecificAccessStatement)
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)getObjectDataValue(selectState, updateSpecificAccessStatement);
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, true, true);
			
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
					return selectValue(env, selectState, updateSpecificAccessStatement, name, context, lastAccess);
				}
				catch (Exception e)
				{
					throw new RuntimeException(e);
				}
			}
		};
	}
	
	static SimpleFunctionTemplate deleteDataValue(final PreparedStatement delete)
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
	
	
	static SimpleFunctionTemplate deleteOldData(final PreparedStatement delete)
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
	
	static SimpleFunctionTemplate selectOldData(final PreparedStatement select)
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)selectOldData(select);
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
					return selectOldDataNames(select, context, age);
				}
				catch (Exception e)
				{
					throw new RuntimeException(e);
				}
			}
		};
	}
	
	static SimpleFunctionTemplate selectAllDataKeys(final PreparedStatement select)
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)selectAllDataKeys(select);
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
					
				String context = _DEFAULT_CONTEXT;
				if (evaluatedArgs.length>0)
					context = evaluatedArgs[0].getString();
				
				try
				{
					return selectAllDataNames(select, context);
				}
				catch (Exception e)
				{
					throw new RuntimeException(e);
				}
			}
		};
	}
	
	
	static SimpleFunctionTemplate deleteAllData(final PreparedStatement delete)
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
	
	static SimpleFunctionTemplate testDataExists(final PreparedStatement testState)
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)testDataExists(testState);
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
					rowId = testValueExists(testState, name, context);
					
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
	
	
	static SimpleFunctionTemplate setObjectDataValue(final PreparedStatement testState, final PreparedStatement insert, final PreparedStatement update)
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)setObjectDataValue(testState, insert, update);
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
					rowId = testValueExists(testState, name, context);
					if (rowId != null)
					{
						modifyCount = updateSpecificValue(update, value, timeStamp, rowId);
					}
					else
					{
						modifyCount = insertDataValue(insert, name, context, value, timeStamp);
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
	
	static Integer testValueExists(PreparedStatement testState, String name, String context) throws SQLException
	{
		testState.setString(1, name);
		testState.setString(2, context);
		ResultSet rs = testState.executeQuery();
		try
		{
			if (rs.next())
			{
				return Integer.valueOf(rs.getInt(1));
			}
			else
				return null;
		}
		finally
		{
			rs.close();
		}
		
	}
	
	
	static int updateSpecificValue(PreparedStatement update, String value, Long lastAccess, long rowid) throws SQLException
	{
		update.setString(1, value);
		
		if (lastAccess == null)
			update.setNull(2, Types.INTEGER);
		else
			update.setLong(2, lastAccess.longValue());
		update.setLong(3, rowid);
		
		return update.executeUpdate();
	}
	
	
	static int insertDataValue(PreparedStatement insert, String name, String context, String value, long timeStamp) throws SQLException
	{
		insert.setString(1, name);
		insert.setString(2, context);
		insert.setString(3, value);
		insert.setLong(4, timeStamp);
		
		return insert.executeUpdate();
	}
	
	static boolean deleteAll(PreparedStatement delete) throws SQLException
	{
		
		
		return delete.executeUpdate() > 0;
	}
	
	static Value selectAllDataNames(PreparedStatement select, String context) throws SQLException
	{
		select.setString(1, context);
		
		LinkedList<Value> value = new LinkedList<Value>();
		ResultSet rs = select.executeQuery();
		while (rs.next())
		{
			value.add(NLispTools.makeValue(rs.getString(1)));
		}
		return NLispTools.makeValue(value.toArray(new Value[0]));
	}
	
	
	static boolean deleteeValue(PreparedStatement delete, String name, String context) throws SQLException
	{
		
		delete.setString(1, name);
		delete.setString(2, context);
		return delete.executeUpdate() > 0;
	}
	
	static int deleteOldData(PreparedStatement delete, String context, long age) throws SQLException
	{
		
		delete.setString(1, context);
		delete.setLong(2, age);
		return delete.executeUpdate();
	}
	
	static Value selectOldDataNames(PreparedStatement select, String context, long age) throws SQLException
	{
		
		select.setString(1, context);
		select.setLong(2, age);
		
		ResultSet rs = select.executeQuery();
		
		LinkedList<Value> out = new LinkedList<Value>();
		
		while (rs.next())
		{
			
			out.add(NLispTools.makeValue(rs.getString(1)));
			
		}
		return NLispTools.makeValue(out.toArray(new Value[0]));
		
	}
	
	
	
	static Value selectValue(Environment env, PreparedStatement selectStatement, PreparedStatement updateSpecificAccessStatement, String name, String context, Long lastAccess) throws SQLException, InstantiationException, IllegalAccessException
	{
		selectStatement.setString(1, name);
		selectStatement.setString(2, context);
		ResultSet rs = selectStatement.executeQuery();
		try
		{
			if (rs.next())
			{
				String result = rs.getString("value");
				
				if (lastAccess != null)
				{
					Long rowid = rs.getLong(2);
					updateSpecificAccessStatement.setLong(1, lastAccess.longValue());
					updateSpecificAccessStatement.setLong(2, rowid.longValue());
					updateSpecificAccessStatement.executeUpdate();
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
	
	
	
	/**
	 * @param args
	 */
	
	public static void main(String[] args) 
	{
		
		
		final Environment top;
		boolean skipCont = false;
		String databasePath = ":memory:";
		if (args != null && args.length > 0)
			databasePath = args[0];
		PreparedStatement insertStatement = null;
		PreparedStatement testStatement = null;
		PreparedStatement selectStatement = null;
		
		PreparedStatement deleteStatement =  null;
		PreparedStatement deleteValueStatement = null;
		PreparedStatement updateSpecificStatement = null;
		PreparedStatement updateSpecificAccessStatement = null;
		PreparedStatement deleteOldDataStatement = null;
		PreparedStatement selectOldDataStatement = null;
		PreparedStatement selectAllDataKeysStatement = null;
		
		LinkedList<PreparedStatement> toCloseList = new LinkedList<PreparedStatement>();
		Connection connection = null;
		Statement statement = null;
		boolean drop = false;
		try
		{
			connection = DriverManager.getConnection("jdbc:sqlite:" + databasePath);
			statement = connection.createStatement();
			statement.setQueryTimeout(30);
			if (drop){
				int count = statement.executeUpdate("drop table if exists objects");
				System.out.println("Dropped table");
			}
			statement.executeUpdate(_STANDARD_CREATE_MAIN_TABLE);
			
			insertStatement = connection.prepareStatement(_INSERT_KEY);
			toCloseList.add(insertStatement);
			testStatement = connection.prepareStatement(_TEST_KEY);
			toCloseList.add(testStatement);
			selectStatement = connection.prepareStatement(_SELECT_DATA_KEY); 
			toCloseList.add(selectStatement);
			updateSpecificStatement = connection.prepareStatement(_UPDATE_SPECIFIC_DATA_KEY);
			toCloseList.add(updateSpecificStatement);
			
			updateSpecificAccessStatement = connection.prepareStatement(_UPDATE_SPECIFIC_ACCESS_DATA_KEY);
			toCloseList.add(updateSpecificAccessStatement);
			toCloseList.add(insertStatement);
			deleteStatement = connection.prepareStatement(_DELETE_ALL_KEY);
			toCloseList.add(deleteStatement);
			deleteValueStatement = connection.prepareStatement(_DELETE_DATA_KEY);
			toCloseList.add(deleteValueStatement);
			deleteOldDataStatement = connection.prepareStatement(_DELETE_ALL_AGED_KEY);
			toCloseList.add(deleteOldDataStatement);
			selectOldDataStatement = connection.prepareStatement(_SELECT_ALL_AGED_KEY);
			toCloseList.add(selectOldDataStatement);
			selectAllDataKeysStatement = connection.prepareStatement(_SELECT_ALL_NAMES_KEY);
			toCloseList.add(selectAllDataKeysStatement);
			
			String initialTitle = "New Lisp";
			String continuationTitle = "Total";
			AIInfoDisplay display = new AIInfoDisplay(initialTitle);
			top = new Environment();
			NLispTools.addDefaultFunctionsAddMacros(top);
			ExtendedFunctions.addExtendedFunctions(top);
			SpeechLispFunctions.addSpeechFunctions(top);
			NeuralNetLispInterface.addNeuralNetFunctions(top);
			FileFunctions.addFunctions(top);
			top.mapFunction("println",getPrintln(display));
			top.mapFunction("set-data-value", setObjectDataValue(testStatement, insertStatement, updateSpecificStatement));
			top.mapFunction("get-data-value", getObjectDataValue(selectStatement, updateSpecificAccessStatement));
			top.mapFunction("delete-all-data", deleteAllData(deleteStatement));
			top.mapFunction("delete-data-value", deleteDataValue(deleteValueStatement));
			top.mapFunction("check-data-exists", testDataExists(testStatement));
			top.mapFunction("delete-old-data", deleteOldData(deleteOldDataStatement));
			top.mapFunction("select-old-data-names", selectOldData(selectOldDataStatement));
			top.mapFunction("get-all-names", selectAllDataKeys(selectAllDataKeysStatement));
			
			BufferedReader b = new BufferedReader(new InputStreamReader(System.in));
			String lineinput;
			LinkedList<Value> parsedResult = null;
			Value result = null;
			String currentTitle = initialTitle;
			display.DisplayMessageLine("Type \"quit\" to exit program.  Type \"clear\" to clear a multiline input buffer.", true);
			StringBuilder command = new StringBuilder();
			while ((lineinput = b.readLine())!=null && !"quit".equalsIgnoreCase(lineinput.trim()))
			{
				if (lineinput.trim().length()==0 || lineinput.trim().startsWith(";"))
					continue;
				command.append(lineinput);
				
				if (lineinput.equals("clear"))
				{
					command = new StringBuilder();
					continue;
				}
				else if (lineinput.equals("break"))
				{
					_executor.shutdown();
					_executor = new ScheduledThreadPoolExecutor(1);
					display.setTitle("New Lisp");
					command = new StringBuilder();
					continue;
				}
				try
				{
					parsedResult = Environment.parse(command.toString(), true);
				}
				catch (IncompleteLispExpressionException icl)
				{
					continue;
				}
				command = new StringBuilder();
				if (parsedResult!=null && parsedResult.size()>0)
					_executor.submit(getWorkItem(display, top, parsedResult.removeFirst(), parsedResult));
			}
			
		}
		catch (Exception e)
		{
			System.out.println(e.toString());
		}
		finally
		{
			_executor.shutdownNow();
			
			try
			{
				for (PreparedStatement ps: toCloseList)
				{
					if (ps != null)
						ps.close();
				}
				
				
				if (statement != null)
					statement.close();
				if (connection != null)
					connection.close();
				
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
			
			
			
		}
		System.exit(0);
	}
	

}
