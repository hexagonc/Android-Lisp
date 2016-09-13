package com.evolved.automata.android.lisp.guibuilder;

import java.util.ArrayList;
import com.evolved.automata.android.guibuilder.R;
import com.evolved.automata.CorrectableException;
import com.evolved.automata.android.lisp.guibuilder.workspace.Workspace;
import com.evolved.automata.android.widgets.ShadowButton;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.EditText;
import android.widget.FrameLayout;
import android.widget.ListView;

public class ProjectManagementView extends FrameLayout
{
	
	public interface ProjectSelectedListener
	{
		public void onSelected(String projectName);
		public void onCancelled();
	}
	Context _con;
	
	ShadowButton _createProjectButton;
	ShadowButton _selectProjectButton;
	ShadowButton _renameProjectButton;
	ShadowButton _deleteProjectButton;
	ShadowButton _cancelButton;
	ListView _allProjectsListView;
	ArrayList<String> _projectNameList = new ArrayList<>();
	ArrayAdapter<String> _projectNameAdapter;
	ProjectSelectedListener _projectSelectedListener;
	EditText _selectedProjectEdit;
	ViewGroup _rootView = null;
	public ProjectManagementView(Context context)
	{
		super(context);
		_con = context;
		
	}
	
	public ProjectManagementView(Context context, AttributeSet attrs)
	{
		super(context, attrs);
		_con = context;
		
		
	}
	
	public void setManagementListener(ProjectSelectedListener listener)
	{
		_projectSelectedListener = listener;
	}
	
	
	
	private void configure(Context context)
	{
		LayoutInflater inflater = (LayoutInflater)context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		_rootView = (ViewGroup)inflater.inflate(R.layout.project_management, this, false);
		addView(_rootView);
		String tag;
		
		_createProjectButton  = (ShadowButton)findViewById(R.id.sdw_but_project_create);
		_createProjectButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				String projectName;
				if ((projectName = checkProjectNameDefined())!=null)
				{
					createProject(projectName);
				}
			}
		});
		tag = (String)_createProjectButton.getTag();
		_selectProjectButton  = (ShadowButton)findViewById(R.id.sdw_but_project_select);
		_selectProjectButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				String projectName;
				if ((projectName = checkProjectNameDefined("Project name is undefined"))!=null)
				{
					_projectSelectedListener.onSelected(projectName);
				}
			}
		});
		tag = (String)_selectProjectButton.getTag();
		_renameProjectButton  = (ShadowButton)findViewById(R.id.sdw_but_project_rename);
		_renameProjectButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				String projectName;
				if ((projectName = checkProjectNameDefined())!=null)
				{
					renameProject(projectName);
				}
			}
		});
		tag = (String)_renameProjectButton.getTag();
		_deleteProjectButton  = (ShadowButton)findViewById(R.id.sdw_but_project_delete);
		_deleteProjectButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				String projectName;
				if ((projectName = checkProjectNameDefined())!=null)
				{
					deleteProject(projectName);
				}
			}
		});
		tag = (String)_deleteProjectButton.getTag();
		_cancelButton  = (ShadowButton)findViewById(R.id.sdw_but_project_cancel);
		_cancelButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				_projectSelectedListener.onCancelled();
			}
		});
		tag = (String)_cancelButton.getTag();
		_selectedProjectEdit = (EditText)findViewById(R.id.edit_project_name);
		_allProjectsListView = (ListView)findViewById(R.id.lst_project_names);
		
		_projectNameAdapter = new ArrayAdapter<String>(context, R.layout.project_name_list_item, R.id.txt_project_name_list_item, _projectNameList);
		Workspace workspace = CodeManager.get().getWorkspace();
		_projectNameList.addAll(workspace.getProjectMap().keySet());
		_allProjectsListView.setAdapter(_projectNameAdapter);
		_allProjectsListView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

			@Override
			public void onItemClick(AdapterView<?> parent, View view,
					int position, long id) {
				String proj = _projectNameList.get(position);
				_selectedProjectEdit.setText(proj);
			}
		});
		
		
	}
	
	private String checkProjectNameDefined(String errorMessage)
	{
		String name = _selectedProjectEdit.getText().toString().trim();
		if (name.length()>0)
			return name;
		else
		{
			showErrorDialog("Error", (errorMessage != null)?errorMessage:"Project name can't be blank");
			return null;
		}
	}
	
	private String checkProjectNameDefined()
	{
		return checkProjectNameDefined(null);
	}
	
	private void showErrorDialog(String title, String errorMessage)
	{
		AlertDialog.Builder builder =  new AlertDialog.Builder(_con);
		builder.setTitle(title);
		builder.setMessage(errorMessage);
		builder.setPositiveButton("Okay", new DialogInterface.OnClickListener()
		{

			@Override
			public void onClick(DialogInterface dialog, int which) {
				dialog.dismiss();
			}
			
		});
		builder.create().show();
		
	}
	

	private void showConfirmationDialog(String title, String message, final Runnable onAcceptRunnable)
	{
		AlertDialog.Builder builder =  new AlertDialog.Builder(_con);
		builder.setTitle(title);
		builder.setMessage(message);
		builder.setPositiveButton("Okay", new DialogInterface.OnClickListener()
		{

			@Override
			public void onClick(DialogInterface dialog, int which) {
				dialog.dismiss();
				onAcceptRunnable.run();
			}
			
		});
		
		builder.setNegativeButton("Cancel", new DialogInterface.OnClickListener() {
			
			@Override
			public void onClick(DialogInterface dialog, int which) {
				dialog.dismiss();
			}
		});
		builder.create().show();
		
	}
	
	private void createProject(final String name)
	{
		final Workspace workspace = CodeManager.get().getWorkspace();
		if (workspace.hasProject(name))
		{
			showConfirmationDialog(
					"Overwrite Existing Project?", 
					"Creating " + name + " will clear all code pages.",
					new Runnable()
					{
						public void run()
						{
							workspace.deleteProject(name, CodeManager.get().getDefaultProjectName());
							workspace.createNewProject(name, false);
						}
					});
		}
		else
		{
			workspace.createNewProject(name, false);
			_projectNameList.add(name);
			_projectNameAdapter.notifyDataSetChanged();
		}
	}
	
	private void deleteProject(final String name)
	{
		final Workspace workspace = CodeManager.get().getWorkspace();
		
		if (workspace.hasProject(name))
		{
			showConfirmationDialog(
					"Delete Project?", 
					"Are you sure you want to delete \"" + name + "\"?   This action is irreversible.",
					new Runnable()
					{
						public void run()
						{
							workspace.deleteProject(name, CodeManager.get().getDefaultProjectName());
							_projectNameList.remove(name);
							_projectNameAdapter.notifyDataSetChanged();
							_selectedProjectEdit.setText("");
						}
					});
		}
		else
		{
			showErrorDialog("Error", "No project exists by the nane: " + name);
		}
	}
	
	private void renameProject(final String oldName)
	{
		
		SimpleTextInputDialog inputDialog = new SimpleTextInputDialog(
				_con,
				new SimpleTextInputDialog.DialogButtonListener() {
					
					@Override
					public void onCancel() {
						
					}
					
					@Override
					public void onAccept(String inputText) {
						handleRename(oldName, inputText);
					}
				},
				"Rename Project",
				"Enter name of new project",
				SimpleTextInputDialog.ButtonType.SHADOW
				);
		inputDialog.show();
	}
	
	private void handleRename(final String oldName, final String newName)
	{
		final Workspace workspace = CodeManager.get().getWorkspace();
		try
		{
			workspace.renameProject(oldName, newName);
			_projectNameList.remove(oldName);
			_projectNameList.remove(newName);
			_projectNameAdapter.notifyDataSetChanged();
		}
		catch (final CorrectableException ce) {
			showConfirmationDialog("Error", ce.getMessage(), new Runnable()
			{
				public void run()
				{
					ce.fix();
					_projectNameList.remove(oldName);
					_projectNameList.remove(newName);
					_projectNameAdapter.notifyDataSetChanged();
				}
			});
		}
	}

	@Override
	protected void onFinishInflate() {
		 
		super.onFinishInflate();
		configure(_con);
	}
	
}
