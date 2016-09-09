package com.evolved.automata;
import java.util.*;
import java.io.*;




public class FileTools 
{
	public static void writeModelsToCSV(String outputFileFullName, LinkedList<Hashtable<String,String>> input)
	{
		HashSet<String> keys = extractAllKeys(input);
		writeModelsToCSV(outputFileFullName,keys.toArray(new String[0]),input);
	}
	
	private static HashSet<String> extractAllKeys(LinkedList<Hashtable<String,String>> serializedWorlds)
	{
		HashSet<String> outerMap = new HashSet<String>();
		
		
		for (Hashtable<String,String> serializedWorld:serializedWorlds)
		{
			
			for (String key:serializedWorld.keySet().toArray(new String[0]))
			{
				if (!outerMap.contains(key))
					outerMap.add(key);
			}
		}
		return outerMap;
	}
	
	
	
	private static void writeModelsToCSV(String outputFileFullName, String[] keys, LinkedList<Hashtable<String,String>> serializedWorlds)
	{
		BufferedWriter writer=null;
		try
		{
			writer = new BufferedWriter(new FileWriter(outputFileFullName));
			writeModelKeysToCSV(writer,keys);
			for (Hashtable<String,String> serializedWorld:serializedWorlds)
			{
				writeModelStructureToCSV(writer,keys,serializedWorld);
			}
		}
		catch (Exception e)
		{
			java.io.StringWriter traceText = new java.io.StringWriter();
			java.io.PrintWriter pWriter = new java.io.PrintWriter(traceText,true);
			e.printStackTrace(pWriter);
			pWriter.close();
			throw new RuntimeException(traceText.toString());
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
	
	
	private static void writeModelStructureToCSV(BufferedWriter writer,String[] keys, Hashtable<String,String> table) throws IOException
	{
		StringBuilder sBuilder = new StringBuilder();
		int keyLength=keys.length;
		if (keyLength>0)
		{
			if (table.containsKey(keys[0]))
			{
				sBuilder.append("\"");
				sBuilder.append(table.get(keys[0]).replace((CharSequence)"\"",(CharSequence)"\"\""));
				sBuilder.append("\"");
			}
			
		}
		for (int i=1;i<keys.length;i++)
		{
			sBuilder.append(",");
			if (table.containsKey(keys[i]))
			{
				sBuilder.append("\"");
				sBuilder.append(table.get(keys[i]).replace((CharSequence)"\"",(CharSequence)"\"\""));
				sBuilder.append("\"");
			}
		}
		
		writer.write(sBuilder.toString());
		writer.newLine();
	}
	
	private static void writeModelKeysToCSV(BufferedWriter writer, String[] keys) throws IOException
	{
		StringBuilder sBuilder = new StringBuilder();
		int keyLength=keys.length;
		if (keyLength>0)
		{
			if (keys[0].length()>0)
			{
				sBuilder.append("\"");
				sBuilder.append(keys[0].replace((CharSequence)"\"",(CharSequence)"\"\""));
				sBuilder.append("\"");
			}
			
		}
		for (int i=1;i<keys.length;i++)
		{
			sBuilder.append(",");
			if (keys[i].length()>0)
			{
				sBuilder.append("\"");
				sBuilder.append(keys[i].replace((CharSequence)"\"",(CharSequence)"\"\""));
				sBuilder.append("\"");
			}
		}
		
		writer.write(sBuilder.toString());
		writer.newLine();
	}
	
	public static String[] getTextFileDataLines(String inputFileFulName) throws IOException
	{
		LinkedList<String> outLine = new LinkedList<String>();
		String lineInput="";
		File f = new File(inputFileFulName);
		BufferedReader reader=null;
		try
		{
			if (f.exists())
			{
				reader = new BufferedReader(new FileReader(f));
				while ((lineInput=reader.readLine())!=null)
				{
					if (lineInput.trim().length()>0)
						outLine.add(lineInput.trim());
				}
				return outLine.toArray(new String[0]);
			}
			else
				return null;
		}
		finally
		{
			if (reader!=null)
			{
				
				try {
					reader.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
	}
	
	

	public static String loadAllFile(String inputFileFullname) throws Exception
	{
		StringBuffer buffer = new StringBuffer();
		
		String lineInput="";
		File f = new File(inputFileFullname);
		BufferedReader reader=null;
		try
		{
			if (f.exists())
			{
				reader = new BufferedReader(new FileReader(f));
				while ((lineInput=reader.readLine())!=null)
				{
					buffer.append(lineInput);
					buffer.append(System.getProperty("line.separator"));
				}
				return buffer.toString();
			}
			else
				return null;
		}
		finally
		{
			if (reader!=null)
			{
				
				try {
					reader.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
	}
}
