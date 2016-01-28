package com.evolved.automata.android.lisp.guibuilder.toolareas;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;

import org.apache.commons.lang3.StringUtils;

import com.evolved.automata.android.lisp.guibuilder.CodeManager;
import com.evolved.automata.android.lisp.guibuilder.CodeManager.OnFileSelectedListener;
import com.evolved.automata.android.lisp.guibuilder.CodeManager.PathProtocol;
import com.evolved.automata.android.lisp.guibuilder.MenuManager;
import com.evolved.automata.android.lisp.guibuilder.R;

import android.app.Fragment;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;

public class LocalStorageToolAreaFragment extends ToolAreaFragment 
{
	ImageButton _saveButton;
	ImageButton _openButton;
	
	public LocalStorageToolAreaFragment(CodeManagementUIInterface uinterface)
	{
		super(uinterface);
	}
	
	@Override
	public void onCreate(Bundle savedInstanceState) {

		super.onCreate(savedInstanceState);
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {

		return super.onCreateView(inflater, container, savedInstanceState);
	}

	@Override
	public void onResume() {

		super.onResume();
		MenuManager.get().setCodeEditorMenuType(MenuManager.MenuType.DEFAULT_AND_LOCAL_STORAGE);
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

	@Override
	public void configureLayout() {
		_saveButton = (ImageButton)_cachedView.findViewById(R.id.imgb_local_storage_save);
		_saveButton.setOnClickListener(getSaveButtonOnClickListener());
		_openButton = (ImageButton)_cachedView.findViewById(R.id.imgb_local_storage_open);
		_openButton.setOnClickListener(getOpenButtonOnClickListener());
		
	}
	
	private View.OnClickListener getSaveButtonOnClickListener()
	{
		
		return new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				_uiInterface.saveCurrentCodeEditorToLocalStorage();
			}
		};
	}
	
	private View.OnClickListener getOpenButtonOnClickListener()
	{
		
		return new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				_uiInterface.loadCurrentCodeEditorFromLocalStorage();
			}
		};
	}
	

	@Override
	public int getLayoutResource() {
		
		return R.layout.local_storage_tool_area;
	}

}
