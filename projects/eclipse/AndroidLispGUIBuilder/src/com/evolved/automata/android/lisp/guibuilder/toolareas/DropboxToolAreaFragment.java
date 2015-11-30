package com.evolved.automata.android.lisp.guibuilder.toolareas;

import com.evolved.automata.android.lisp.guibuilder.CodeManager;
import com.evolved.automata.android.lisp.guibuilder.DropboxManager;
import com.evolved.automata.android.lisp.guibuilder.GuiBuilderConfiguration;
import com.evolved.automata.android.lisp.guibuilder.R;
import com.evolved.automata.android.lisp.guibuilder.DropboxManager.OnFileUploadListener;

import android.app.Fragment;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;

public class DropboxToolAreaFragment extends ToolAreaFragment 
{
	
	ImageButton _loadButton;
	ImageButton _saveButton;
	
	
	public DropboxToolAreaFragment(CodeManagementUIInterface uinterface)
	{
		super(uinterface);
	}
	
	@Override
	public void onCreate(Bundle savedInstanceState) 
	{
		super.onCreate(savedInstanceState);
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {
		super.onCreateView(inflater, container, savedInstanceState);
		return _cachedView;
	}
	
	@Override
	public int getLayoutResource()
	{
		return R.layout.dropbox_tool_area;
	}
	
	@Override
	public void configureLayout()
	{
		_loadButton = (ImageButton)_cachedView.findViewById(R.id.imgb_download_dropbox_file);
		_loadButton.setOnClickListener(getLoadButtonOnClickListener());
		_saveButton = (ImageButton)_cachedView.findViewById(R.id.imgb_upload_dropbox_file);
		_saveButton.setOnClickListener(getSaveButtonOnClickListener());
	}
	
	private View.OnClickListener getLoadButtonOnClickListener()
	{
		final GuiBuilderConfiguration.OnDropboxAuthorizedListener listener = new GuiBuilderConfiguration.OnDropboxAuthorizedListener()
		{

			@Override
			public void onAuthorized() 
			{
				showDropboxFileSelectDialog();
			}
			
		};
		
		return new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				if (GuiBuilderConfiguration.get().hasDropboxAuthorizationP())
				{
					showDropboxFileSelectDialog();
				}
				else
				{
					GuiBuilderConfiguration.get().showDropboxLoginDialog(getActivity(), listener);
				}
			}
		};
	}
	
	
	private void showDropboxFileSelectDialog()
	{
		CodeManager.get().showDropboxFileSelectDialog(getActivity(), getFileSelectedListener());
	}
	
	private View.OnClickListener getSaveButtonOnClickListener()
	{
		final OnFileUploadListener uploadListener = new OnFileUploadListener() {
			
			@Override
			public void uploaded(String filename) {
				
			}
			
			@Override
			public void onError(String message, Exception e) {
				_uiInterface.onError(message, e);
			}
		};
		
		return new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				// TODO: implement a save-as function and allow saving brand new files
				String lastFileUrl = CodeManager.get().getLastLoadedFileUrl();
				if (lastFileUrl!=null && CodeManager.get().getPathProtocol(lastFileUrl) == CodeManager.PathProtocol.DROPBOX)
				{
					DropboxManager.get().resynchLastDownloadedDropboxFile(_uiInterface.getCurrentLispEditorCode(), uploadListener);
					
				}
			}
		};
	}
	
	
	
	
	@Override
	public void onResume() {
		
		super.onResume();
	}

	@Override
	public void onPause() {
		
		super.onPause();
	}

	@Override
	public void onDestroyView() {
		// TODO Auto-generated method stub
		super.onDestroyView();
	}

	@Override
	public void onDestroy() {
		// TODO Auto-generated method stub
		super.onDestroy();
	}

}
