package com.evolved.automata.inference;
import com.evolved.automata.KeyValuePair;
import com.evolved.automata.parser.*;
import java.util.*;

import com.evolved.automata.inference.*;
import java.io.*;

public class SQLExtension 
{
	private static class UpdateSpec
	{
		public boolean incrementP;
		public String targetKey;
		public String targetValue;
		public long incrementValue;
		public String[] pair;
		
		
		public UpdateSpec(String updateDescription, Hashtable<String, String> inputMap)
		{
			String[] updateKVPair = null;
			
			if (updateDescription.contains("+="))
			{
				incrementP = true;
				updateKVPair = updateDescription.split("\\+\\=");
				targetKey = updateKVPair[0].trim();
				
				targetValue = updateKVPair[1].trim().replace("{", "").replace("}", "");
				if (targetValue.contains("\""))
				{
					targetValue = targetValue.replace("\"", "");
					incrementValue = Long.parseLong(targetValue);
				}
				else
				{
					if (inputMap!=null && inputMap.containsKey(targetValue))
					{
						targetValue = inputMap.get(targetValue);
						incrementValue = Long.parseLong(targetValue);
					}
					else
						throw new RuntimeException("Update error: Column [" + targetValue + "] not defined in inputMap: " + inputMap);
					
				}
			}
			else if (updateDescription.contains("-="))
			{
				incrementP = true;
				updateKVPair = updateDescription.split("\\-\\=");
				targetKey = updateKVPair[0].trim();
				
				targetValue = updateKVPair[1].trim().replace("{", "").replace("}", "");
				if (targetValue.contains("\""))
				{
					targetValue = targetValue.replace("\"", "");
					incrementValue = -Long.parseLong(targetValue);
				}
				else
				{
					if (inputMap!=null && inputMap.containsKey(targetValue))
					{
						targetValue = inputMap.get(targetValue);
						incrementValue = -Long.parseLong(targetValue);
					}
					else
						throw new RuntimeException("Update error: Column [" + targetValue + "] not defined in inputMap: " + inputMap);
					
				}
			}
			else
			{
				updateKVPair = updateDescription.split("=");
				targetKey = updateKVPair[0].trim();
				
				targetValue = updateKVPair[1].trim();
				targetValue = targetValue.replace("{", "").replace("}", "");
				if (targetValue.contains("\""))
				{
					targetValue = targetValue.replace("\"", "");
				}
				else
				{
					if (inputMap!=null && inputMap.containsKey(targetValue))
					{
						targetValue = inputMap.get(targetValue);
					}
					else if (targetValue.equals(EMPTY_DESIGNATOR))
					{
						targetValue = "";
					}
					else
						throw new RuntimeException("Update error: Column [" + targetValue + "] not defined in inputMap: " + inputMap);
					
				}
			}
			
			pair = new String[]{targetKey, targetValue};
		}
	}
	
	
	private abstract class SubQuery
	{
		public abstract LinkedList<Hashtable<String, String>> executeSQLComponent(Hashtable<String, LinkedList<String>> sqlParameters, Hashtable<String, String> inputMap);
		public abstract LinkedList<Hashtable<String, String>> executeSQLComponent(Hashtable<String, LinkedList<String>> sqlParameters);
		
		public LinkedList<Hashtable<String, String>> execMultipleQuery(Hashtable<String, LinkedList<String>> sqlComponents, LinkedList<Hashtable<String, String>> inputMap)
		{
			LinkedList<Hashtable<String, String>> tout, mapOutput = new LinkedList<Hashtable<String, String>>();
			if (inputMap==null)
			{
				
				return executeSQLComponent(sqlComponents, null);
			}
			
			for (Hashtable<String, String> input:inputMap)
			{
				tout = executeSQLComponent(sqlComponents, input);
				mapOutput.addAll(tout);
			}
			return mapOutput;
		}
		
	}
	
	public class InsertSubQuery extends SubQuery
	{
		public LinkedList<Hashtable<String, String>> executeSQLComponent(Hashtable<String, LinkedList<String>> sqlParameters, Hashtable<String, String> inputMap)
		{
			return executeInsertSQL(sqlParameters, inputMap);
		}
		public LinkedList<Hashtable<String, String>> executeSQLComponent(Hashtable<String, LinkedList<String>> sqlParameters)
		{
			return executeInsertSQL(sqlParameters, null, null);
		}

	}
	
	public class SelectSubQuery extends SubQuery
	{
		public LinkedList<Hashtable<String, String>> executeSQLComponent(Hashtable<String, LinkedList<String>> sqlParameters, Hashtable<String, String> inputMap)
		{
			return processSelectResult(sqlParameters, inputMap);
		}
		public LinkedList<Hashtable<String, String>> executeSQLComponent(Hashtable<String, LinkedList<String>> sqlParameters)
		{
			return processSelectResult(sqlParameters, null);
		}

	}
	
	public class UpdateSubQuery extends SubQuery
	{
		public LinkedList<Hashtable<String, String>> executeSQLComponent(Hashtable<String, LinkedList<String>> sqlParameters, Hashtable<String, String> inputMap)
		{
			return processUpdateCommand(sqlParameters, inputMap);
		}
		public LinkedList<Hashtable<String, String>> executeSQLComponent(Hashtable<String, LinkedList<String>> sqlParameters)
		{
			return processUpdateCommand(sqlParameters, null);
		}

	}
	
	public class DeleteSubQuery extends SubQuery
	{
		public LinkedList<Hashtable<String, String>> executeSQLComponent(Hashtable<String, LinkedList<String>> sqlParameters, Hashtable<String, String> inputMap)
		{
			return processDeleteCommand(sqlParameters, inputMap);
		}
		public LinkedList<Hashtable<String, String>> executeSQLComponent(Hashtable<String, LinkedList<String>> sqlParameters)
		{
			return processDeleteCommand(sqlParameters, null);
		}

	}
	
	private static final String EMPTY_DESIGNATOR = "empty";
	public static final String HIDDEN_KEY = "*";
	public static final String HIDDEN_VALUE = "*t*";
	String[] grammar = new String[]{
			"return_all = \"*\"",
			"output_column_name = ('@' | '_' | '-'), ('@' | '_' | '-' | '#')*",
			"column_spec = output_column_name, ('$'*, ',' , '$'*, output_column_name)*",
			"output_columns = return_all | column_spec",
			"table_name = ('@' | '_' | '-'), ('@' | '_' | '-' | '#')*",
			"table_spec = '$'+, \"from\", '$'+, table_name",
			"column_name = ('@' | '_' | '-'), ('@' | '_' | '-' | '#')*",
			"term_char = '@' | '_' | '-' | '#' | '\"'",
			"equality_spec = column_name, '$'*, '=', '$'*, '{', term_char+, '}'",
			"greater_equal_spec = column_name, '$'*, \">=\", '$'*, '{', term_char+, '}'",
			"greater_spec = column_name, '$'*, '>', '$'*, '{', term_char+, '}'",
			"less_equal_spec = column_name, '$'*, \"<=\", '$'*, '{', term_char+, '}'",
			"less_spec = column_name, '$'*, '<', '$'*, '{', term_char+, '}'",
			"isNull_spec = \"isNull(\", column_name, \")\"",
			"isBlank_spec = \"isBlank(\", column_name, \")\"",
			"notBlank_spec = \"is\", (' ' | '_' | '-')?, \"not\",  (' ' | '_' | '-')?, ('b' | 'B'), \"lank(\", column_name, \")\"",
			"term = equality_spec | greater_equal_spec | greater_spec | less_equal_spec | less_spec | isNull_spec | isBlank_spec | notBlank_spec",
			"condition = '$'+, \"where\", '$'+, term, ('$'+, \"and\", '$'+, term)*",
			"ascending = \"asc\" | \"ascending\" | \"dsc\"" ,
			"descending = \"desc\" | \"descending\"",
			"direction = ascending | descending",
			"order_column = ('@' | '_' | '-'), ('@' | '_' | '-' | '#')*",
			"order_spec = '$'+, \"order\", '$'+, \"by\", '$'+, order_column, (',', order_column)*, ('$'+, direction)?",
			"top_count = '#'+",
			"add = \"add\"",
			"set = \"set\"",
			"update_type = add | set",
			"top = '$'+, \"top\", '$'+, top_count",
			"input_column = '{', ('@' | '_' | '\"' | '-' | ' ' | '#' | '\\#')+, '}'",
			"input_column_spec = input_column, ('$'*, ',', '$'*, input_column)*",
			"update_spec = column_name, '$'*, ('+' | '-')?, '=', '$'*, '{', ('@' | '_' | '-' | '\"' | '#')+, '}'",
			"insert_command = \"insert\", '$'+, \"into\", '$'+, table_name, '$'*, '(', '$'*, column_spec, '$'*, ')', '$'*, \"values\", '$'*, '(', '$'*, input_column_spec, '$'*, ')'",
			"select_command = \"select\", top?, '$'+, output_columns, table_spec, condition?, order_spec?",
			"update_command = \"update\", '$'+, table_name, '$'+, update_type, (','?, '$'+, update_spec)+, condition?",
			"delete_command = \"delete\", table_spec, condition?",
			"sql_command = update_command | select_command | insert_command | delete_command"};
	
	Hashtable<String, StructureModel> _tables;
	Hashtable<String, String> _tableFiles;
	CFGParser _sqlParser;
	final String idColumn = "id#";
	String idTableName = "id_table";
	String idColumnName = "id";
	String dataColumnName= "data";
	
	
	public SQLExtension(Hashtable<String, StructureModel> tables, Hashtable<String, String> fileNames)
	{
		_tables = tables;
		_sqlParser = new CFGParser(grammar);
		_tableFiles = fileNames;
	}
	
	public SQLExtension()
	{
		_tables = new Hashtable<String, StructureModel>();
		_sqlParser = new CFGParser(grammar);
		_tableFiles = new Hashtable<String, String>();
	}
	
	public SQLExtension(Hashtable<String, String> fileNames) throws ClassNotFoundException, IOException
	{
		_tables = new Hashtable<String, StructureModel>();
		_sqlParser = new CFGParser(grammar);
		_tableFiles = fileNames;
		File f;
		StructureModel model = null;
		String filename = null;
		for (String key:fileNames.keySet())
		{
			filename = fileNames.get(key);
			f = new File(filename);
			if (f.exists())
			{
				String[] comps = com.evolved.automata.filetools.StandardTools.partitionFilePath(filename);
				if (comps[2].equalsIgnoreCase("csv"))
				{
					 model = InferenceTools.createFromCSV(filename, 10);
				}
				else
					model = InferenceTools.loadModelFromRawFile(filename);
			}
			else
				model = new StructureModel();
			_tables.put(key, model);
		}
		
	}
	
	public void saveModels() throws IOException
	{
		for (String tableName:_tableFiles.keySet())
		{
			saveTableModel(tableName);
		}
	}
	
	public void removeTable(String name)
	{
		if (_tableFiles.containsKey(name))
		{
			removeTable(name);
			_tableFiles.remove(name);
			_tables.remove(name);
		}
		
	}
	
	
	public StructureModel getTable(String name)
	{
		if (_tableFiles.containsKey(name))
			return _tables.get(name);
		else
			return null;
	}
	
	public LinkedList<Hashtable<String, String>> exec(String sql, LinkedList<Hashtable<String, String>> inputMap)
	{
		Hashtable<String, LinkedList<String>> out = null;
		out = _sqlParser.matchPathExtrude(
				sql, 
				"sql_command, '^'", 
				new String[]{"sql_command", "return_all", "order_column", "top_count", "output_column_name", "return_all", "table_name", "direction", "equality_spec", "greater_equal_spec", "greater_spec", "less_equal_spec", "less_spec", "input_column", "update_spec", "update_type", "isNull_spec", "isBlank_spec", "notBlank_spec"},
				new String[]{"sql_command", "direction", "update_type"});
		
		if (out == null)
			throw new RuntimeException("Syntax error with sql statement: " + sql);
		String type = getFirst(out, "sql_command");
		SubQuery query = null;
		
		if (type!=null && type.equals("select_command"))
		{
			query = new SelectSubQuery();
			return query.execMultipleQuery(out, inputMap);
		}
		else if (type!=null && type.equals("insert_command"))
		{
			query = new InsertSubQuery();
			return query.execMultipleQuery(out, inputMap);
		}
		else if (type!=null && type.equals("update_command"))
		{
			query = new UpdateSubQuery();
			return query.execMultipleQuery(out, inputMap);
		}
			
		else if (type!=null && type.equals("delete_command"))
		{
			query = new DeleteSubQuery();
			return query.execMultipleQuery(out, inputMap);
		}
		else
			return null;
	}
	
	public LinkedList<Hashtable<String, String>> exec(String sql)
	{
		return exec(sql, (LinkedList<Hashtable<String, String>>)null);
	}
	
	public LinkedList<Hashtable<String, String>> exec(String sql, Hashtable<String, String> inputMap)
	{
		LinkedList<Hashtable<String, String>> o = new LinkedList<Hashtable<String,String>>();
		o.add(inputMap);
		return exec(sql, o);
	}
	
	public Hashtable<String, String> execScalar(String sql, LinkedList<Hashtable<String, String>> inputMap)
	{
		LinkedList<Hashtable<String, String>> o = exec(sql, inputMap);
		if (o.size()>0)
			return o.getFirst();
		else
			return new Hashtable<String, String>(); 
	}
	
	
	public Hashtable<String, String> execScalar(String sql)
	{
		return execScalar(sql, null);
	}
	
	public void addTable(String tableName, StructureModel model)
	{
		_tables.put(tableName, model);
	}
	
	public void addTable(String tableName, String filename, boolean createAutomatically) throws FileNotFoundException, IOException, ClassNotFoundException
	{
		if (_tables.containsKey(tableName))
		{
			saveTableModel(tableName);
		}
		
		StructureModel model = null;
		
		File f = new File(filename);
		
		if (f.exists())
		{
			String[] comps = com.evolved.automata.filetools.StandardTools.partitionFilePath(filename);
			if (comps[2].equalsIgnoreCase("csv"))
			{
				 model = InferenceTools.createFromCSV(filename, 10);
			}
			else
			{
				model = InferenceTools.loadModelFromRawFile(filename);
				
			}
		}
		else if (createAutomatically)
			model = new StructureModel();
		_tables.put(tableName, model);
		
		_tableFiles.put(tableName, filename);
	}
	
	public void addTable(String tableName, String filename, boolean createAutomatically, String customdelimiter) throws FileNotFoundException, IOException, ClassNotFoundException
	{
		if (_tables.containsKey(tableName))
		{
			saveTableModel(tableName);
		}
		
		StructureModel model = null;
		
		
		File f = new File(filename);
		
		if (f.exists())
		{
			String[] comps = com.evolved.automata.filetools.StandardTools.partitionFilePath(filename);
			if (comps[2].equalsIgnoreCase("csv"))
			{
				 model = InferenceTools.createFromCSV(filename, true, customdelimiter);
			}
			else
			{
				model = InferenceTools.loadModelFromRawFile(filename);
				
			}
		}
		else if (createAutomatically)
			model = new StructureModel();
		_tables.put(tableName, model);
		
		_tableFiles.put(tableName, filename);
	}
	
	public LinkedList<Hashtable<String, String>> join(LinkedList<Hashtable<String, String>> leftSide, LinkedList<Hashtable<String, String>> rightSide)
	{
		LinkedList<Hashtable<String, String>> total = new LinkedList<Hashtable<String,String>>();
		Hashtable<String, String> join;
		if (leftSide == null || rightSide == null || leftSide.size()==0 || rightSide.size() == 0)
		{
			return total;
		}
		
		for (Hashtable<String, String> left:leftSide)
		{
			for (Hashtable<String, String> right:rightSide)
			{
				join = new Hashtable<String, String>();
				join.putAll(left);
				join.putAll(right);
				total.add(join);
			}
		}
		return total;
	}
	
	public LinkedList<Hashtable<String, String>> join(Hashtable<String, String> leftSide, LinkedList<Hashtable<String, String>> rightSide)
	{
		LinkedList<Hashtable<String, String>> actualLeft, actualRight, total = new LinkedList<Hashtable<String,String>>();
		if (leftSide == null)
		{
			return total;
		}
		
		actualLeft = new LinkedList<Hashtable<String,String>>();
		actualLeft.add(leftSide);
		return join(actualLeft, rightSide);
	}
	
	public LinkedList<Hashtable<String, String>> join(LinkedList<Hashtable<String, String>> leftSide,  Hashtable<String, String> rightSide)
	{
		LinkedList<Hashtable<String, String>> actualRight, total = new LinkedList<Hashtable<String,String>>();
		
		if (rightSide == null)
		{
			return total;
		}
		
		actualRight = new LinkedList<Hashtable<String,String>>();
		actualRight.add(rightSide);
		
		return join(leftSide, actualRight);
	}
	
	public LinkedList<Hashtable<String, String>> join(Hashtable<String, String> leftSide,  Hashtable<String, String> rightSide)
	{
		LinkedList<Hashtable<String, String>> total = new LinkedList<Hashtable<String,String>>();
		
		if (rightSide == null || leftSide == null)
		{
			return total;
		}
		Hashtable<String, String> join = new Hashtable<String, String>();
		join.putAll(leftSide);
		join.putAll(rightSide);
		total.add(join);
		return total;
	}
	
	private LinkedList<Hashtable<String, String>> processDeleteCommand(Hashtable<String, LinkedList<String>> sqlParameters, Hashtable<String, String> inputMap)
	{
		LinkedList<Hashtable<String, String>> updatedOutput = new LinkedList<Hashtable<String, String>>();
		String tableName = getFirst(sqlParameters, "table_name");
		if (!_tables.containsKey(tableName))
			return new LinkedList<Hashtable<String, String>>();
		StructureModel tableModel = _tables.get(tableName);
		Hashtable<String, String> queryMap = getQueryMapFromEqualitySpec(sqlParameters, inputMap);
				
		List<Integer> baseWorlds = InferenceTools.getConsistentIndices(tableModel, queryMap, true);
		StructureSlice slice;

		for (Integer worldIndex:baseWorlds)
		{
			slice = InferenceTools.getWorldExplicit(tableModel, worldIndex);
			if (!inequalityTermsMatch(sqlParameters, inputMap, slice))
				continue;
			InferenceTools.deleteConsistentWorlds(tableModel, slice.getSliceMap());
			updatedOutput.add(sliceToHashtable(slice));
		}
		return updatedOutput;
	}
	
	private LinkedList<Hashtable<String, String>> processSelectResult(Hashtable<String, LinkedList<String>> sqlComponents, Hashtable<String, String> inputMap)
	{
		
		LinkedList<String> outputColumns = getAll(sqlComponents, "output_column_name");
		boolean returnAllCols = getFirst(sqlComponents, "return_all")!=null;
		
		HashSet<String> oSet = new HashSet<String>();
		if (!returnAllCols)
		{
			for (String ocol:outputColumns)
				oSet.add(ocol);
		}
		
		
		String tableName = getFirst(sqlComponents, "table_name");
		// Do nothing for now, might need to change this in the future
		// TODO: Come up with a different way of handling the no
		if (!_tables.containsKey(tableName))
		{
			if (errorOnUndefinedTableP())
				throw new RuntimeException("Select error: Undefined table -- " + tableName);
		}
		StructureModel tableModel = _tables.get(tableName);
		LinkedList<StructureSlice> baseOutput = preFilterResults(tableModel, sqlComponents, inputMap), finalOut = new LinkedList<StructureSlice>();
		String order = getFirst(sqlComponents, "direction");
		List<String> orderCol = getAll(sqlComponents, "order_column");
		List<StructureSlice>  sortedOutput;
		if (orderCol!=null)
		{
			boolean dsc = "descending".equals(order);
			
			String[] okeys = orderCol.toArray(new String[0]);
			if (!dsc)
				sortedOutput = StructureFilters.sortStructuresNumericAscending(baseOutput, okeys);
			else
				sortedOutput = StructureFilters.sortStructuresNumericDescending(baseOutput, okeys);
			baseOutput = (LinkedList<StructureSlice>)sortedOutput;
		}
		int returnCount = -1;
		String rvalue;
		rvalue = getFirst(sqlComponents, "top_count");
		if (rvalue!=null)
			returnCount = Integer.parseInt(rvalue);
		else
			returnCount = baseOutput.size();
		int i=0;
		LinkedList<Hashtable<String, String>> mapOutput = new LinkedList<Hashtable<String, String>>();
		String c;
		Hashtable<String, String> mappedValue;
		for (StructureSlice s:baseOutput)
		{
			mappedValue = new Hashtable<String, String>();
			if (returnAllCols)
			{
				for (KeyValuePair<String, String> pair:s.GetKeyValuePairs())
				{
					if (!isHiddenKey(pair.GetKey()))
					{
						c = s.GetValue(pair.GetKey());
						mappedValue.put(pair.GetKey(), c);	
					}
					
				}
			}
			else
			{
				for (String ocol:outputColumns)
				{
					c = s.GetValue(ocol);
					if (c!=null)
						mappedValue.put(ocol, c);
					else
						if (inputMap!=null && inputMap.containsKey(ocol))
							mappedValue.put(ocol, inputMap.get(ocol));
				}
			}
			
			mapOutput.add(mappedValue);
			i++;
			if (i>=returnCount)
				break;
		}
		return mapOutput;
	}
	
	private LinkedList<Hashtable<String, String>> executeInsertSQL(Hashtable<String, LinkedList<String>> sqlParameters, Hashtable<String, String> inputMap)
	{
		return executeInsertSQL(sqlParameters, inputMap, null);
		
	}
	
	private LinkedList<Hashtable<String, String>> executeInsertSQL(Hashtable<String, LinkedList<String>> sqlParameters, Hashtable<String, String> inputMap, String[] identity)
	{
		String tableName = getFirst(sqlParameters, "table_name");
		if (!_tables.containsKey(tableName))
			return new LinkedList<Hashtable<String, String>>();
		StructureModel tableModel = _tables.get(tableName);
		
		Hashtable<String, String> insertMap = new Hashtable<String, String>();
		LinkedList<String> outputColumns = getAll(sqlParameters, "output_column_name");
		String[] array_output = outputColumns.toArray(new String[0]);
		LinkedList<String> inputData = getAll(sqlParameters, "input_column");
		String[] array_input = inputData.toArray(new String[0]);
		
		String output;
		String input;
		int id;
		String value;
		for (int i=0;i<array_output.length;i++)
		{
			output = array_output[i];
			input = array_input[i].replace("{", "").replace("}", "");
			if (input.equalsIgnoreCase(idColumn))
			{
				id = getNewId(tableModel);
				value = ""+id;
				if (identity!=null && identity.length==1)
					identity[0]=value;
			}
			else
			{
				if (input.contains("\""))
				{
					input.replace("\"", "");
					value = input;
				}
				else
				{
					if (inputMap!=null && inputMap.containsKey(input))
					{
						value = inputMap.get(input);
					}
					else if (EMPTY_DESIGNATOR.equalsIgnoreCase(input))
						value="";
					else
						return new LinkedList<Hashtable<String, String>>();
				}
				
			}
			insertMap.put(output, value);
		}
		addHiddenKey(insertMap);
		boolean success = InferenceTools.storeKeyValueMap(tableModel, insertMap);
		if (success)
		{
			LinkedList<Hashtable<String, String>> list =  new LinkedList<Hashtable<String, String>>();
			list.add(insertMap);
			return list;
		}
		else
			return new LinkedList<Hashtable<String, String>>();
		
	}
	
	private LinkedList<Hashtable<String, String>> processUpdateCommand(Hashtable<String, LinkedList<String>> sqlParameters, Hashtable<String, String> inputMap)
	{
		LinkedList<Hashtable<String, String>> updatedOutput = new LinkedList<Hashtable<String, String>>();
		String tableName = getFirst(sqlParameters, "table_name");
		if (!_tables.containsKey(tableName))
			return new LinkedList<Hashtable<String, String>>();
		StructureModel tableModel = _tables.get(tableName);
		
		
		boolean setColumnP = "set".equals(getFirst(sqlParameters, "update_type"));
		LinkedList<UpdateSpec> uspec = new LinkedList<SQLExtension.UpdateSpec>();
		
		for (String updateSpec:getAll(sqlParameters, "update_spec"))
		{
			uspec.add(new UpdateSpec(updateSpec, inputMap));
		}
		
		Hashtable<String, String> queryMap = getQueryMapFromEqualitySpec(sqlParameters, inputMap);
		
		List<Integer> o, baseWorlds = InferenceTools.getConsistentIndices(tableModel, queryMap, true);
		
		String ivalue;
		long updatedValue;
		StructureSlice slice;
		
		for (Integer worldIndex:baseWorlds)
		{
			slice = InferenceTools.getWorldExplicit(tableModel, worldIndex);
			if (!inequalityTermsMatch(sqlParameters, inputMap, slice))
				continue;
			
			
			for (UpdateSpec us:uspec)
			{
				if (us.incrementP)
				{
					ivalue = slice.GetValue(us.targetKey);
					updatedValue = us.incrementValue + Long.parseLong(ivalue);
					InferenceTools.updateValue(tableModel, worldIndex, new String[]{us.targetKey, "" + updatedValue});
				}
				else
				{
					if (setColumnP)
						InferenceTools.updateValue(tableModel, worldIndex, us.pair);
					else
					{
						o = InferenceTools.addKeyValuePairToWorld(tableModel, worldIndex, us.pair);
						if (o.size()!=1)
							throw new RuntimeException("Update-Add Error: Attempting to add columns to table twice! " + us.pair);
					}
				}
			}
			
			updatedOutput.add(sliceToHashtable(slice));
		}
		return updatedOutput;
	}
	
	private boolean errorOnUndefinedTableP()
	{
		return true;
	}
	private Double getCompValue(String input, Hashtable<String, String> inputMap)
	{
		
		if (input.contains("\""))
		{
			input = input.replace("\"", "");
			return new Double(input);
		}
		else
		{
			if (inputMap.containsKey(input))
			{
				return new Double(inputMap.get(input));
			}
			else
				return null;
		}
	}
	
	private String getEffectiveValue(String input, Hashtable<String, String> inputMap)
	{
		
		if (input.contains("\""))
		{
			input = input.replace("\"", "");
			return input;
		}
		else
		{
			if (inputMap.containsKey(input))
			{
				return inputMap.get(input);
			}
			else if (input.equals(EMPTY_DESIGNATOR))
				return "";
			else
				return null;
		}
	}
	
	

	
	private Hashtable<String, String> getQueryMapFromEqualitySpec(Hashtable<String, LinkedList<String>> sqlParameters, Hashtable<String, String> inputMap)
	{
		LinkedList<String> equalityList = getAll(sqlParameters, "equality_spec");
		Hashtable<String, String> queryMap = new Hashtable<String, String>();
		String[] kvPair;
		String evalue;
		if (equalityList != null)
		{
			for (String equality:equalityList)
			{
				kvPair = extractConditionSpecData(equality);
				if (kvPair!=null)
				{
					evalue = getEffectiveValue(kvPair[1], inputMap);
					if (evalue !=null)
						queryMap.put(kvPair[0], evalue);
					else
						throw new RuntimeException("Column " + kvPair[1] + "does not exist in input set");
				}
			}
		}
		
		
		addHiddenKey(queryMap);
		return queryMap;
	}
	
	private boolean inequalityTermsMatch(Hashtable<String, LinkedList<String>> sqlParameters, Hashtable<String, String> inputMap, StructureSlice slice)
	{
//		if (inputMap==null)
//			return true;
		String[] kvPair;
		LinkedList<String> greaterList = getAll(sqlParameters, "greater_spec");
		LinkedList<String> lowerList = getAll(sqlParameters, "less_spec");
		LinkedList<String> greaterEqualList = getAll(sqlParameters, "greater_equal_spec");
		LinkedList<String> lowerEqualList = getAll(sqlParameters, "less_equal_spec");
		LinkedList<String> isNullEqualList = getAll(sqlParameters, "isNull_spec");
		LinkedList<String> isBlankEqualList = getAll(sqlParameters, "isBlank_spec");
		LinkedList<String> notBlankEqualList = getAll(sqlParameters, "notBlank_spec");
		double sliceValue, compValue;
		Double value;
		String svalue;
		
		if (notBlankEqualList!=null)
		{
			for (String notblank:notBlankEqualList)
			{
				kvPair = extractConditionSpecData(notblank);
				svalue = slice.GetValue(kvPair[0]);
				 if (svalue==null || svalue.length()==0)
					return false;	
			}
		}
		
		if (isNullEqualList!=null)
		{
			for (String isnullSpec:isNullEqualList)
			{
				kvPair = extractConditionSpecData(isnullSpec);
				svalue = slice.GetValue(kvPair[0]);
				 if (svalue!=null)
					return false;	
			}
		}
		
		if (isBlankEqualList!=null)
		{
			for (String isblankSpec:isBlankEqualList)
			{
				kvPair = extractConditionSpecData(isblankSpec);
				svalue = slice.GetValue(kvPair[0]);
				 if (svalue==null || svalue.length()>0)
					return false;	
			}
		}
		
		
		if (greaterList!=null)
		{
			for (String greater:greaterList)
			{
				kvPair = extractConditionSpecData(greater);
				svalue = slice.GetValue(kvPair[0]);
				if (svalue==null)
					continue;
				sliceValue = Double.parseDouble(svalue);
				value = getCompValue(kvPair[1], inputMap);
				if (value == null)
					continue;
				compValue = value.doubleValue();
				if (sliceValue <= compValue)
					return false;
			}
		}
		
		if (lowerList!=null)
		{
			for (String lower:lowerList)
			{
				kvPair = extractConditionSpecData(lower);
				svalue = slice.GetValue(kvPair[0]);
				if (svalue==null)
					continue;
				sliceValue = Double.parseDouble(svalue);
				value = getCompValue(kvPair[1], inputMap);
				if (value == null)
					continue;
				compValue = value.doubleValue();
				if (sliceValue >= compValue)
					return false;
			}
		}
		
		if (greaterEqualList!=null)
		{
			for (String greaterEqual:greaterEqualList)
			{
				kvPair = extractConditionSpecData(greaterEqual);
				svalue = slice.GetValue(kvPair[0]);
				if (svalue==null)
					continue;
				sliceValue = Double.parseDouble(svalue);
				value = getCompValue(kvPair[1], inputMap);
				if (value == null)
					continue;
				compValue = value.doubleValue();
				if (sliceValue < compValue)
					return false;
			}
		}
		
		if (greaterEqualList!=null)
		{
			for (String greaterEqual:greaterEqualList)
			{
				kvPair = extractConditionSpecData(greaterEqual);
				svalue = slice.GetValue(kvPair[0]);
				if (svalue==null)
					continue;
				sliceValue = Double.parseDouble(svalue);
				value = getCompValue(kvPair[1], inputMap);
				if (value == null)
					continue;
				compValue = value.doubleValue();
				if (sliceValue < compValue)
					return false;
			}
		}
		
		if (lowerEqualList!=null)
		{
			for (String lowerEqual:lowerEqualList)
			{
				kvPair = extractConditionSpecData(lowerEqual);
				svalue = slice.GetValue(kvPair[0]);
				if (svalue==null)
					continue;
				sliceValue = Double.parseDouble(svalue);
				value = getCompValue(kvPair[1], inputMap);
				if (value == null)
					continue;
				compValue = value.doubleValue();
				if (sliceValue > compValue)
					return false;
			}
		}
		
		return true;
	}
	

	
	private Hashtable<String, String> sliceToHashtable(StructureSlice slice)
	{
		Hashtable<String, String> map = new Hashtable<String, String>();
		for (KeyValuePair<String, String> kv:slice.GetKeyValuePairs())
		{
			map.put(kv.GetKey(), kv.GetValue());
		}
		return map;
				
	}
	
	
	

	
	
	private void addHiddenKey(Hashtable<String, String> map)
	{
		map.put(HIDDEN_KEY, HIDDEN_VALUE);
	}
	
	private boolean isHiddenKey(String v)
	{
		return HIDDEN_KEY.equals(v);
				
	}
	private int getNewId(StructureModel model)
	{
		Hashtable<String, String> inputMap = new Hashtable<String, String>();
		inputMap.put(dataColumnName, "T");
		String[] values = InferenceTools.getScalarValue(model, inputMap, new String[]{idColumnName});
		int newValue = 1;
		if (values == null)
		{
			inputMap.put(idColumnName, "1");
			InferenceTools.storeKeyValueMap(model, inputMap);
			return newValue;
		}
		else
		{
			
			newValue = Integer.parseInt(values[0])+1;
			InferenceTools.updateValue(model, inputMap, new String[]{idColumnName, ""+newValue});
			
		}
		
		return newValue;
	}
	

	
	
	

	
	private String getFirst(Hashtable<String, LinkedList<String>> sqlComponents, String key)
	{
		if (sqlComponents.containsKey(key))
		{
			LinkedList<String> o = sqlComponents.get(key);
			String out = o.getFirst();
			return out;
		}
		else
			return null;
	}
	
	private LinkedList<String> getAll(Hashtable<String, LinkedList<String>> sqlComponents, String key)
	{
		if (sqlComponents.containsKey(key))
		{
			return sqlComponents.get(key);
		}
		else
			return null;
	}
	
	private LinkedList<StructureSlice> preFilterResults(StructureModel model, Hashtable<String, LinkedList<String>> sqlParameters, Hashtable<String, String> inputMap)
	{
		
		LinkedList<StructureSlice> updatedOutput = new LinkedList<StructureSlice>();
		
		Hashtable<String, String> queryMap = getQueryMapFromEqualitySpec(sqlParameters, inputMap);
		
		List<Integer> baseWorlds = InferenceTools.getConsistentIndices(model, queryMap, true);
		StructureSlice slice;
		

		for (Integer worldIndex:baseWorlds)
		{
			slice = InferenceTools.getWorldExplicit(model, worldIndex);
			if (!inequalityTermsMatch(sqlParameters, inputMap, slice))
				continue;
			updatedOutput.add(slice);
			
		}
		return updatedOutput;
	}
	
	private String[] extractConditionSpecData(String conditionSpec)
	{
		String[] parts;
		String[] operators = {"<=", ">=", "=", "<", ">"};
		for (String op:operators)
		{
			parts = conditionSpec.split(op);
			if (parts!=null && parts.length == 2)
			{
				parts[1] = parts[1].replace("{", "").replace("}", "");
				parts[0]=parts[0].trim();
				parts[1]=parts[1].trim();
				return parts;
			}
		}
		int start, end;
		if (((start = conditionSpec.indexOf("("))>-1) && ((end = conditionSpec.indexOf(")"))>start))
		{
			return new String[]{conditionSpec.substring(start+1, end)};
		}
		
		return null;
	}
	
	private void saveTableModel(String table) throws IOException
	{
		StructureModel model = null;
		File f;
		model = _tables.get(table);
		String filename = _tableFiles.get(table);
		f = new File(filename);
		if (f.exists())
		{
			f.delete();
		}
		
		String[] comps = com.evolved.automata.filetools.StandardTools.partitionFilePath(filename);
		if (comps[2].equalsIgnoreCase("csv"))
		{
			 InferenceTools.writeWorldsToCSVFile(model, filename);
		}
		else
			InferenceTools.serializeModelToRawFile(model, filename);
	}
}
