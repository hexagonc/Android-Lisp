package com.evolved.automata.android.lisp.guibuilder.toolareas;

import com.evolved.automata.android.lisp.guibuilder.CodeManager;
import com.evolved.automata.android.lisp.guibuilder.DropboxManager;
import com.evolved.automata.android.lisp.guibuilder.GuiBuilderConfiguration;
import com.evolved.automata.android.lisp.guibuilder.MenuManager;
import com.evolved.automata.android.guibuilder.R;
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
		
		return new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				_uiInterface.loadCurrentCodeEditorFromDropbox();
			}
		};
	}
	
	
//	private void showDropboxFileSelectDialog()
//	{
//		CodeManager.get().showDropboxFileSelectDialog(getActivity(), getFileSelectedListener(), false);
//	}
	
	private View.OnClickListener getSaveButtonOnClickListener()
	{
		
		
		return new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				_uiInterface.saveCurrentCodeEditorToDropbox();
			}
		};
	}
	
	
	
	
	@Override
	public void onResume() {
		
		super.onResume();
		MenuManager.get().setCodeEditorMenuType(MenuManager.MenuType.DEFAULT_AND_DROPBOX);
	}

	@Override
	public void onPause() {
		
		super.onPause();
	}

	@Override
	public void onDestroyView() {

		super.onDestroyView();
	}

	@Override
	public void onDestroy() {

		super.onDestroy();
	}

}
