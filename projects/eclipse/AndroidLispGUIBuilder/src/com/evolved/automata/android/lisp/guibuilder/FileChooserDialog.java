package com.evolved.automata.android.lisp.guibuilder;

import java.util.ArrayList;

import android.app.Activity;
import android.app.Dialog;
import android.app.ProgressDialog;
import android.content.Context;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;
import android.view.ViewGroup;

public class FileChooserDialog extends Dialog implements OnChildFilesRequestedListener
{
	
	
	ArrayList<FileChooserItem> _children = new ArrayList<FileChooserItem>();
	
	
	FileChooserItem _currentFolder = null;
	
	Activity _parent;
	ListView _fileList;
	TextView _upButton;
	
	Button _createFolderButton;
	Button _selectFileButton;
	Button _cancelButton;
	EditText _selectedFileEdit;
	
	ArrayAdapter<FileChooserItem> _fileAdapter = null;
	
	TextView _pathView = null;
	
	boolean _nothingToDo = false;
	
	ProgressDialog _progressDialog = null;
	final String PREVIOUS_DIRECTORY_NAME = "...";
	boolean _showProgressRequestedP = false;
	FileChooserItem _selectedFile = null;
	
	String _title;
	
	Activity _activity;
	public FileChooserDialog(Activity activity, String title, FileChooserItem baseFolder)
	{
		super(activity);
		_activity = activity;
		_title = title;
		_parent = activity;
		_currentFolder = baseFolder;
		if (!_currentFolder.hasChildren())
		{
			_currentFolder.onClickListener(null);
			_nothingToDo = true;
		}
	}

	private boolean childExists(String name)
	{
		for (FileChooserItem item:_children)
		{
			if (name.equals(item.getFileNameShort()))
				return true;
		}
		return false;
	}
	
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		
		super.onCreate(savedInstanceState);
		setContentView(R.layout.filechooser_layout);
		_selectedFileEdit = (EditText)findViewById(R.id.edit_filename);
		_createFolderButton = (Button)findViewById(R.id.but_create_folder);
		final FileChooserItem.OnCreateChildFileListener folderListener = new FileChooserItem.OnCreateChildFileListener()
		{
			
			@Override
			public void onSuccess(FileChooserItem item) {
				startProgressDialog();
				_currentFolder.getChildren(FileChooserDialog.this);
			}
			
			@Override
			public void onError(String message) {
				stopProgressDialog();
				showError(message);
			}
		}; 
		
		_createFolderButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				String folderName = _selectedFileEdit.getText().toString().trim();
				if (folderName.length()>0 && !childExists(folderName))
				{
					boolean showProgress = _currentFolder.onCreateChildFolder(folderName, folderListener);
					if (showProgress)
						startProgressDialog();
				}
				else
					showError("Enter the name of the folder you want to create");
			}
		});
		
		
		_selectFileButton = (Button)findViewById(R.id.but_ok);
		_selectFileButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				
				if (_selectedFile == null)
				{
					String fileShort = _selectedFileEdit.getText().toString().trim();
					if (fileShort.length() == 0)
						showError("You must select a file!");
					else
					{
						_currentFolder.onCreateChildFile(fileShort, getOnCreateNewFileListener());
					}
					
				}
				else
				{
					dismiss();
					_selectedFile.onClickListener(FileChooserDialog.this);
					
				}
			}
		});
		
		_cancelButton = (Button)findViewById(R.id.but_cancel);
		_cancelButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				dismiss();
			}
		});
		
		_upButton = new TextView(_parent);
		_upButton.setText(PREVIOUS_DIRECTORY_NAME);
		_upButton.setTextSize(30);
		
		_upButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				
				if (_currentFolder.getParent()!=null)
				{
					startProgressDialog();
					_currentFolder.getParent().getChildren(FileChooserDialog.this);
				}
			}
		});
		
		_upButton.setLayoutParams(new AbsListView.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT));
		
		
		setTitle(_title);
		
		_pathView = (TextView)findViewById(R.id.txt_parent_path_view);
		_fileList = (ListView)findViewById(R.id.lst_files);
		_fileList.addHeaderView(_upButton);
		_fileAdapter = new ArrayAdapter<FileChooserItem>(_parent, 0, _children)
				{
					@Override
					public View getView(int position, View cached, ViewGroup parent)
					{
						FileChooserItem f = _children.get(position);
						if (cached!=null)
						{
							f.configureView(cached);
							return cached;
						}
						else
						{
							LayoutInflater inflater = (LayoutInflater)_parent.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
							View v = inflater.inflate(f.getViewResource(), parent, false);
							f.configureView(v);
							return v;
						}
					}
				};
		_fileList.setAdapter(_fileAdapter);
		
		_fileList.setOnItemClickListener(new AdapterView.OnItemClickListener() {

			@Override
			public void onItemClick(AdapterView<?> parent, View view,
					int position, long id) {
				position -=1; // decrement by one due to the presence of the header item w
				FileChooserItem f = _children.get(position);
				
				if (f.hasChildren())
				{
					_selectedFile = null;
					
					startProgressDialog();
					f.getChildren( FileChooserDialog.this);
				}
				else
				{
					_selectedFile = f; 
					_selectedFileEdit.setText(f.getFileNameShort());
				}
			}
		});
	}
	
	
	private FileChooserItem.OnCreateChildFileListener getOnCreateNewFileListener()
	{
		return new FileChooserItem.OnCreateChildFileListener()
		{

			@Override
			public void onSuccess(FileChooserItem item) {
				dismiss();
				item.onClickListener(FileChooserDialog.this);
			}

			@Override
			public void onError(String message) {
				stopProgressDialog();
				showError(message);
				
			}
			
		};
	}
	
	
	private void startProgressDialog()
	{
		_showProgressRequestedP = true;
		if (Looper.myLooper() != Looper.getMainLooper())
		{
			Runnable r = new Runnable()
			{
				public void run()
				{
					if (_progressDialog==null && _showProgressRequestedP)
					{
						_progressDialog = ProgressDialog.show(_parent,"", "");
					}
				}
			};
			_pathView.post(r);
		}
		else
		{
			if (_progressDialog==null && _showProgressRequestedP)
			{
				_progressDialog = ProgressDialog.show(_parent,"", "");
			}
			
		}
		
	}
	
	private void stopProgressDialog()
	{
		_showProgressRequestedP = false;
		if (Looper.myLooper() != Looper.getMainLooper())
		{
			Runnable r = new Runnable()
			{
				public void run()
				{
					if (_progressDialog!=null)
					{
						_progressDialog.dismiss();
						_progressDialog = null;
						
					}
				}
			};
			_pathView.post(r);
		}
		else
		{
			if (_progressDialog!=null)
			{
				_progressDialog.dismiss();
				_progressDialog = null;
				
			}
			
		}
		
	}
	
	private void setCurrentFolder(FileChooserItem f, ArrayList<FileChooserItem> children)
	{
		_currentFolder = f;
		setPathText(f.getFileName());
		_children.clear();
		_children.addAll(children);
		_fileAdapter.notifyDataSetChanged();
	}
	
	private void showError(final String text)
	{
		Runnable r = new Runnable()
		{
			public void run()
			{
				Toast.makeText(_activity, text, Toast.LENGTH_LONG).show();
			}
		};
		_cancelButton.post(r);
	}
	
	
	
	private void setPathText(String text)
	{
		_pathView.setText(text);
	}
	
	private void setPathText(FileChooserItem item)
	{
		java.io.File jf = new java.io.File(item.getFileName());
		setPathText(jf.getAbsolutePath());
	}
	
	@Override
	public void onChildrenRetrieved(FileChooserItem parentOfChildren, ArrayList<FileChooserItem> onChildren)
	{
		_selectedFile = null;
		_showProgressRequestedP = false;
		stopProgressDialog();
		setCurrentFolder(parentOfChildren, onChildren);
	}
	
	@Override
	public void onErrorsRetrievingChildren()
	{
		stopProgressDialog();
	}



	@Override
	protected void onStart() {
		
		super.onStart();
		
		setPathText(_currentFolder);
		_showProgressRequestedP = true;
		_currentFolder.getChildren(this);
		
		Runnable r = new Runnable()
		{
			public void run()
			{
				if (_showProgressRequestedP)
					startProgressDialog();
			}
		};
		
	
		_pathView.postDelayed(r, 0);
	}
}
