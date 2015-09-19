package com.evolved.automata.inference;
import java.util.*;
public class SQLTester {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		String note_data_file = "/Users/Evolved8/Documents/notes.csv";
		SQLExtension ext = null;
		Hashtable<String, String> files = new Hashtable<String, String>(), data, out;
		
		LinkedList<Hashtable<String, String>> resultSet = null;
		try
		{
			files.put("notes", note_data_file);
			ext = new SQLExtension(files);
			
			ext.saveModels();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}

	}
	
	private static void printResultSet(LinkedList<Hashtable<String, String>> resultSet, String operation)
	{
		if (resultSet!=null)
		{
			for (Hashtable<String, String> result:resultSet)
				System.out.println(result);
			System.out.println("");
			System.out.println(resultSet.size() + " Results Returned for " + operation);
		}
		else
			System.out.println("Undefined table for " + operation);
	}
	
//	public static LinkedList<Hashtable<String, String>> testDeletes(SQLExtension database)
//	{
//		String deleteStatement = "delete from notes where id < {id}";
//		Hashtable<String, String> data = new Hashtable<String, String>();
//		data.put("id", "6");
//		LinkedList<Hashtable<String, String>> resultset = (LinkedList<Hashtable<String, String>>)database.executeSQL(deleteStatement, data);
//		return resultset;
//	}
//	
//	public static LinkedList<Hashtable<String, String>> testInserts(SQLExtension database)
//	{
//		String insertStatement = "insert into notes (id, date, value, desc, data) values ({id#}, {time}, {text}, {desc}, {data})";
//		LinkedList<Hashtable<String, String>> resultSet = null;
//		Hashtable<String, String>  data;
//		for (int i=0;i<3;i++)
//		{
//			data = new Hashtable<String, String>();
//			data.put("time", "" + System.currentTimeMillis());
//			data.put("text", "Hello SQL world!");
//			data.put("desc", "hello message");
//			data.put("data", "t");
//			resultSet = (LinkedList<Hashtable<String, String>>)database.executeSQL(insertStatement, data);
//			if (resultSet!=null)
//				System.out.println(resultSet.getFirst());
//		}
//		
//		resultSet = (LinkedList<Hashtable<String, String>>)database.executeSQL("insert into notes (date, value, desc) values ({\"333\"}, {\"Goodbye world\"}, {\"100\"}, {id#})", null);
//		return resultSet;
//	}
//	
//
//	public static LinkedList<Hashtable<String, String>> testSelects(SQLExtension database, String selectSQL)
//	{
//		
//		LinkedList<Hashtable<String, String>> resultSet = null;
//		Hashtable<String, String> data = new Hashtable<String, String>();
//		data.put("data", "t");
//		resultSet = (LinkedList<Hashtable<String, String>>)database.executeSQL(selectSQL, data);
//		
//		return resultSet;
//	}
//	
//	public static LinkedList<Hashtable<String, String>> testUpdates(SQLExtension database, String updateSQL)
//	{
//		
//		LinkedList<Hashtable<String, String>> resultSet = null, r2;
//		Hashtable<String, String> data = new Hashtable<String, String>();
//		
//		data.put("id", "6");
//		data.put("date", "1");
//		data.put("data", "t");
//		resultSet = (LinkedList<Hashtable<String, String>>)database.executeSQL(updateSQL, data);
//		r2 = (LinkedList<Hashtable<String, String>>)database.executeSQL("update notes set date-={\"1360114641245\"} where id = {\"39\"}", data);
//		resultSet.addAll(r2);
//		return resultSet;
//	}
	
}
