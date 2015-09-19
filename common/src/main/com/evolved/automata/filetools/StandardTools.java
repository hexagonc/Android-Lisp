package com.evolved.automata.filetools;
import java.io.*;
import java.nio.channels.FileChannel;
import java.util.*;

import com.evolved.automata.parser.CFGParser;
public class StandardTools 
{
	public static String join(String[] parts, String delimiter)
	{
		if (parts == null)
			return null;
		if (parts.length == 0)
			return "";
		
		StringBuilder sBuilder = new StringBuilder(parts[0]);
		for (int i=1;i<parts.length;i++)
		{
			sBuilder.append(delimiter);
			sBuilder.append(parts[i]);
		}
		return sBuilder.toString();
	}
	
	public static String join(String[] parts, char delimiter)
	{
		if (parts == null)
			return null;
		if (parts.length == 0)
			return "";
		
		StringBuilder sBuilder = new StringBuilder(parts[0]);
		for (int i=1;i<parts.length;i++)
		{
			sBuilder.append(delimiter);
			sBuilder.append(parts[i]);
		}
		return sBuilder.toString();
	}
	
	public static void initializeRequiredPaths(String[] paths)
	{
		for (String path:paths)
		{
			createFolderIfNotExists(path);
		}
	}
	
	public static boolean createFolderIfNotExists(String directoryName)
	{
		File f = new File(directoryName);
		if (!f.exists())
		{
			return f.mkdirs();
		}
		return false;
	}
	
	public static String correctFilePath(String directoryName)
	{
		if (directoryName!=null && directoryName.length()>0 && !directoryName.endsWith("/"))
		{
			return directoryName + "/";
		}
		else
			return directoryName;
	}
	
	public static String[] partitionFilePath(String fullPath)
	{
		String[] grammar = new String[]
				{"separator = '/'",
				"path_char = '@' | '#' | ' ' | '_' | '.' | '-'",
				"pre_extension_char = '.'`, path_char",
				"pre_extension = pre_extension_char+",
				"path_component = path_char+",
				"directory = (separator, path_component)+",
				"ext_part = ('@' | '#' | '_')+",
				"extension = '.', ext_part",
				"filename_short = pre_extension*, pre_extension",
				"filename = filename_short, extension?",
				"path = (directory, separator, filename?) | (directory, (separator, filename)?)"
				};
		CFGParser parser = new CFGParser(grammar);
		Hashtable<String, LinkedList<String>> out = null;
		if ((out = parser.matchPathExtrude(fullPath, "path, '^'", new String[]{"directory", "filename_short", "ext_part"}))!=null)
		{
			String directory = parser.getFirstCapturedValue("directory");
			String filename_short = parser.getFirstCapturedValue("filename_short");
			String extension = parser.getFirstCapturedValue("ext_part");
			return new String[]{directory, filename_short, extension};
		}
		return null;
	}
	
	public static boolean moveFile(String oldFileName, String newFileName, boolean overwrite)
	{
		File oldFile = new File(oldFileName);
		
		File newFile = new File(newFileName);
		if (newFile.exists() && !overwrite)
			return false;
		if (newFile.exists())
			newFile.delete();
		return oldFile.renameTo(newFile);
	}
	
	
	
	public static boolean moveFiles(String[] oldFileNameNames, String targetPath, boolean overwrite)
	{
		File f;
		String name;
		boolean out = true;
		targetPath = preparePath(targetPath);
		for (String old:oldFileNameNames)
		{
			f = new File(old);
			name = f.getName();
			out = out &&  moveFile(old, targetPath + name, overwrite);
		}
		return out;
	}
	
	public static boolean copyFile(String sourceFile, String destFile)
	{
		try
		{
			copyFile(new File(sourceFile), new File(destFile));
			return true;
		}
		catch (IOException ie)
		{
			return false;
		}
		
	}
	public static void copyFile(File sourceFile, File destFile) throws IOException {
	    if(!destFile.exists()) {
	        destFile.createNewFile();
	    }

	    FileChannel source = null;
	    FileChannel destination = null;

	    try {
	        source = new FileInputStream(sourceFile).getChannel();
	        destination = new FileOutputStream(destFile).getChannel();
	        destination.transferFrom(source, 0, source.size());
	    }
	    finally {
	        if(source != null) {
	            source.close();
	        }
	        if(destination != null) {
	            destination.close();
	        }
	    }
	}
	
	public static boolean copyFiles(String[] oldFileNameNames, String targetPath)
	{
		File f;
		String name;
		boolean out = true;
		targetPath = preparePath(targetPath);
		for (String old:oldFileNameNames)
		{
			f = new File(old);
			name = f.getName();
			out = out &&  copyFile(old, targetPath + name);
		}
		return out;
	}
	
	public static boolean copyFiles(String sourcePath, String extension, String targetPath)
	{
		File f = new File(sourcePath);
		String name;
		boolean out = true;
		targetPath = preparePath(targetPath);
		File[] listed = f.listFiles();
		for (File old:listed)
		{
			
			name = old.getName();
			if (name.endsWith(extension))
				out = out &&  copyFile(old.getAbsolutePath(), targetPath + name);
		}
		return out;
	}
	
	public static boolean copyFiles(String sourcePath, String extension[], String targetPath)
	{
		File f = new File(sourcePath);
		String name;
		boolean out = true;
		targetPath = preparePath(targetPath);
		File[] listed = f.listFiles();
		for (File old:listed)
		{
			
			name = old.getName();
			for (String ext:extension)
			{
				if (name.endsWith(ext))
				{
					out = out &&  copyFile(old.getAbsolutePath(), targetPath + name);
					break;
				}
			}
			
		}
		return out;
	}
	
	public static String[] listFiles(String basepath, String extension)
	{
		File f = new File(preparePath(basepath));
		File[] listed = f.listFiles();
		LinkedList<String> filtered = new LinkedList<String>();
		String name;
		for (File old:listed)
		{
			
			name = old.getName();
			if (name.endsWith(extension))
				filtered.add( old.getAbsolutePath());
		}
		return filtered.toArray(new String[0]);
	}
	
	public static String[] listFiles(String basepath, String[] extension)
	{
		File f = new File(preparePath(basepath));
		File[] listed = f.listFiles();
		LinkedList<String> filtered = new LinkedList<String>();
		String name;
		for (File old:listed)
		{
			
			name = old.getName();
			for (String ext:extension)
			{
				if (name.endsWith(ext))
				{
					filtered.add( old.getAbsolutePath());
					break;
				}
			}
			
		}
		return filtered.toArray(new String[0]);
	}
	
	public static String[] listFiles(String basepath)
	{
		File f = new File(preparePath(basepath));
		File[] listed = f.listFiles();
		LinkedList<String> filtered = new LinkedList<String>();
		for (File old:listed)
		{
			filtered.add( old.getAbsolutePath());
		}
		return filtered.toArray(new String[0]);
	}
	
	// Returns a list of directories in a path.  The first element of which is
	// the parent directory or null if there is no parent folder (basepath is root)
	public static String[] listDirectories(String basepath, boolean nameOnlyP)
	{
		File f = new File(preparePath(basepath));
		File[] listed = f.listFiles();
		LinkedList<String> filtered = new LinkedList<String>();
		File parent = f.getParentFile();
		if (parent!=null)
			filtered.add(preparePath(parent.getAbsolutePath()));
		else
			filtered.add(null);
		
		for (File old:listed)
		{
			if (old.isDirectory())
				if (nameOnlyP)
					filtered.add(old.getName());
				else
					filtered.add(preparePath(old.getAbsolutePath()));
		}
		return filtered.toArray(new String[0]);
	}
	
	// Returns a list of directories in a path.  The first element of which is
    // the parent directory or null if there is no parent folder (basepath is root)
	public static File[] listDirectoryFiles(String basepath)
	{
		File f = new File(preparePath(basepath));
		File[] listed = f.listFiles();
		LinkedList<File> filtered = new LinkedList<File>();
		File parent = f.getParentFile();
		if (parent!=null)
			filtered.add(parent);
		else
			filtered.add(null);
		
		for (File old:listed)
		{
			if (old.isDirectory())
				filtered.add(old);
		}
		return filtered.toArray(new File[0]);
	}
	
	// Returns a list of directories in a path.  The first element of which is
    // the parent directory or null if there is no parent folder (basepath is root)
	public static File[] listDirectoryFiles(String basepath, boolean includeFiles)
	{
		File f = new File(preparePath(basepath));
		File[] listed = f.listFiles();
		LinkedList<File> filtered = new LinkedList<File>();
		File parent = f.getParentFile();
		if (parent!=null)
			filtered.add(parent);
		else
			filtered.add(null);
		
		for (File old:listed)
		{
			if (old.isDirectory() || includeFiles)
				filtered.add(old);
		}
		return filtered.toArray(new File[0]);
	}
	
	
	public static String preparePath(String path)
	{
		if (!path.endsWith("/"))
			return path + "/";
		else
			return path;
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
	
	public static String getDateTimeString(long timeMilli)
	{
		Calendar calendar = Calendar.getInstance();
		calendar.setTimeInMillis(timeMilli);
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
	
	public static String newLine()
	{
		return System.getProperty("line.separator");
	}
	
	public static BufferedReader  getReaderFromPackageResource(String resource)
	{
		InputStream istream = StandardTools.class.getResourceAsStream(resource);
		if (istream==null)
			return null;
		InputStreamReader reader = new InputStreamReader(istream);
		return new BufferedReader(reader);
	}
	
	public static BufferedReader[]  getReaderFromPackageResource(String[] resources)
	{
		BufferedReader[] array = new BufferedReader[resources.length];
		for (int i=0;i<resources.length;i++)
			array[i] = getReaderFromPackageResource(resources[i]);
		return array;
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
	
	
	
	public static String getDetailedExceptionText(Exception e)
	{
		java.io.StringWriter traceText = new java.io.StringWriter();
		java.io.PrintWriter pWriter = new java.io.PrintWriter(traceText,true);
		e.printStackTrace(pWriter);
		pWriter.close();
		return traceText.toString();
	}
    
	public static void writeLinesToFile(String outputFile, String[] dataLines, boolean appendP) throws IOException
	{
		BufferedWriter bWriter=null;
		try
		{
			bWriter = new BufferedWriter(new FileWriter(outputFile,appendP));
			for (String lineoutput:dataLines)
			{
				bWriter.write(lineoutput);
				bWriter.newLine();
			}
			
			
		}
		finally
		{
			if (bWriter!=null)
				try {
					bWriter.close();
				} catch (IOException e) {
					throw new RuntimeException(e.toString());
				}
		}
	}
	
	public static void WriteTraceLog(String tracefilefullname,String message)
	{
		GregorianCalendar now = new GregorianCalendar();
		String logMessage=String.format(
				"[%1$s/%2$s/%3$s]%4$s:%5$s:%6$s  %7$s\r\n",
				now.get(Calendar.MONTH)+1,
				now.get(Calendar.DAY_OF_MONTH),
				now.get(Calendar.YEAR),
				now.get(Calendar.HOUR),
				now.get(Calendar.MINUTE),
				now.get(Calendar.SECOND),
				message);
		AppendAllText(tracefilefullname,logMessage);
	}
	public static void AppendAllText(String inputFileFullName, String text) throws RuntimeException
	{
		//input.spl
		BufferedWriter bWriter=null;
		
		try
		{
			bWriter = new BufferedWriter(new FileWriter(inputFileFullName,true));
			bWriter.write(text);
			
		}
		catch (Exception e)
		{
			
		}
		finally
		{
			if (bWriter!=null)
				try {
					bWriter.close();
				} catch (IOException e) {
					throw new RuntimeException(e.toString());
				}
		}
		
	}
	
	public static Boolean FileExists(String fileFullName)
	{
		File f = new File(fileFullName);
		return f.exists();
	}
	
	public static String[] getListSubDirectory(File f)
	{
		String[] out = null;
		if (f.isDirectory())
		{
			File[] dList = f.listFiles();
			out = new String[dList.length];
			for (int i=0;i<dList.length;i++)
				out[i] = dList[i].getAbsolutePath() + File.pathSeparator + dList[i].getName();
		}
		return out;
	}
	
	public static String[] getListSubDirectory(String f)
	{
		
		return getListSubDirectory(new File(f));
	}
	
	public static void DeleteFile(String fileFullName) throws RuntimeException
	{
		File f = new File(fileFullName);
		try
		{
			f.delete();
		}
		catch (Exception e)
		{
			java.io.StringWriter traceText = new StringWriter();
			java.io.PrintWriter pWriter = new PrintWriter(traceText,true);
			e.printStackTrace(pWriter);
			pWriter.close();
			throw new RuntimeException(traceText.toString());
		}
	}
	
	public static void ImportCSVFile(String csvFileFullName, CSVRowImporter importer, boolean includeCellPositionP)
	{
		String inputFileFullName=csvFileFullName;
		BufferedReader bfReader=null;
		String lineInput="";
		String[] lineSplit;
		int i=0;
		try
		{
			bfReader = new BufferedReader(new FileReader(inputFileFullName));
			while ((lineInput=bfReader.readLine())!=null)
			{
				lineSplit=lineInput.split(",");
				
				for (int j=0;j<lineSplit.length;j++)
				{
					if (includeCellPositionP)
						importer.ImportCellData(lineSplit[j].trim(),i,j);
					else
						importer.ImportCellData(lineSplit[j].trim());
				}
				i++;
			}
		}
		catch (Exception e)
		{
			java.io.StringWriter traceText = new StringWriter();
			java.io.PrintWriter pWriter = new PrintWriter(traceText,true);
			e.printStackTrace(pWriter);
			pWriter.close();
			throw new RuntimeException(traceText.toString());
		}
	}
	
	public static void ProcessFile(String inputFileFullName, LineInputProcessor lineProcessor) 
	{
		
		BufferedReader bfReader=null;
		String lineInput="";
		
		try
		{
			bfReader = new BufferedReader(new FileReader(inputFileFullName));
			while ((lineInput=bfReader.readLine())!=null)
			{
				lineProcessor.ProcessLine(lineInput);
			}
		}
		catch (Exception e)
		{
			java.io.StringWriter traceText = new StringWriter();
			java.io.PrintWriter pWriter = new PrintWriter(traceText,true);
			e.printStackTrace(pWriter);
			pWriter.close();
			throw new RuntimeException(traceText.toString());
		}
	}
	
	/**
	 * Reads a file 
	 * @param inputFileFulName
	 * @return returns an array of strings for each line in the file
	 * @throws IOException
	 */
	public static String[] getDataFileLines(String inputFileFulName) throws IOException
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
					
					e.printStackTrace();
				}
			}
		}
	}
	
	
	
	/**
	 * Reads a file 
	 * @param inputFileFulName
	 * @return returns an array of strings for each line in the file
	 * @throws IOException
	 */
	public static String[] getDataFileLines(BufferedReader reader) throws IOException
	{
		LinkedList<String> outLine = new LinkedList<String>();
		String lineInput;
		try
		{
			if (reader!=null)
			{
				
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
	
	
	/**
	 * Reads a text file into a single String line
	 * @param inputFileFulName
	 * @return File contents as a single string
	 * @throws IOException
	 */
	public static String getDataFileLine(String inputFileFulName) throws IOException
	{
		
		String lineInput="";
		StringBuffer sBuffer = new StringBuffer();
		File f = new File(inputFileFulName);
		BufferedReader reader=null;
		try
		{
			if (f.exists())
			{
				reader = new BufferedReader(new FileReader(f));
				while ((lineInput=reader.readLine())!=null)
				{
					sBuffer.append(lineInput);
				}
				return sBuffer.toString();
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
	
	
	/**
	 * Reads a text package resource into a single String line
	 * @param fully qualified package resource name
	 * @return File contents as a single string or null if there was an error
	 */
	public static String getPackageDataFileLine(String resourceName)
	{
		
		String lineInput="";
		StringBuffer sBuffer = new StringBuffer();
		
		BufferedReader reader=null;
		try
		{
			reader = getReaderFromPackageResource(resourceName);
			if (reader!=null)
			{
				
				while ((lineInput=reader.readLine())!=null)
				{
					sBuffer.append(lineInput);
				}
				return sBuffer.toString();
			}
			else
				return null;
		}
		catch (Exception e)
		{
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
	
	/**
	 * Reads a file 
	 * @param inputFileFulName
	 * @return returns an array of strings for each line in the file
	 * @throws IOException
	 */
	public static String[] getPackageDataFileLines(String resourceName)
	{
		LinkedList<String> outLine = new LinkedList<String>();
		String lineInput="";
		
		BufferedReader reader=null;
		try
		{
			reader = getReaderFromPackageResource(resourceName);
			if (reader!=null)
			{
				
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
		catch (Exception e)
		{
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
