package com.evolved.automata.android.lisp.guibuilder.toolareas;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;

import org.apache.commons.lang3.StringUtils;

import com.evolved.automata.android.lisp.guibuilder.CodeManager;
import com.evolved.automata.android.lisp.guibuilder.CodeManager.OnFileSelectedListener;
import com.evolved.automata.android.lisp.guibuilder.CodeManager.PathProtocol;
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
	}

	@Override
	public void onPause() {
		// TODO Auto-generated method stub
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

	@Override
	public void configureLayout() {
		_saveButton = (ImageButton)_cachedView.findViewById(R.id.imgb_local_storage_save);
		_saveButton.setOnClickListener(getSaveButtonOnClickListener());
		_openButton = (ImageButton)_cachedView.findViewById(R.id.imgb_local_storage_open);
		_openButton.setOnClickListener(getOpenButtonOnClickListener());
	}
	
	private View.OnClickListener getSaveButtonOnClickListener()
	{
		final OnFileSelectedListener listener = new OnFileSelectedListener()
		{

			@Override
			public void onFileSelected(String fullPath, String fileContents,
					PathProtocol protocol, boolean replaceEditor) {
				
				String newContents = _uiInterface.getCurrentLispEditorCode();
				FileOutputStream fistream = null;
				try
				{
					
					fistream = new FileOutputStream(fullPath, false);
					fistream.write(newContents.getBytes(Charset.forName("UTF-8")));
					
					CodeManager.get().setLastLoadedFileUrl(fullPath, protocol);
					CodeManager.get().setLastLoadedLocalStorageParentFolder(fullPath);
					String[] parts = StringUtils.split(fullPath, "/");
					_uiInterface.setCodeTitle(parts[parts.length-1]);
				}
				catch (Exception e)
				{
					onError("Error saving file: " + fullPath,e);
				}
				finally
				{
					if (fistream != null)
						try {
							fistream.close();
						} catch (IOException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
				}
				
					
			}

			@Override
			public void onError(String message, Exception e) {
				_uiInterface.onError(message, e);
			}
			
		};
		return new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				CodeManager.get().showLocalStorageFileSelectDialog(getActivity(), listener);
			}
		};
	}
	
	private View.OnClickListener getOpenButtonOnClickListener()
	{
		final OnFileSelectedListener listener = new OnFileSelectedListener()
		{

			@Override
			public void onFileSelected(String fullPath, String fileContents,
					PathProtocol protocol, boolean replaceEditor) {
				if (replaceEditor)
					_uiInterface.replaceCodeEdit(fileContents);
				else
					_uiInterface.insertCodeAtEditCursor(fileContents);
				
				String url = CodeManager.get().getLastLoadedFileUrl();
				String filenameshort = CodeManager.get().getShortFileNameFromPathUrl(url);
				_uiInterface.setCodeTitle(filenameshort);
					
			}

			@Override
			public void onError(String message, Exception e) {
				_uiInterface.onError(message, e);
			}
			
		};
		return new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				CodeManager.get().showLocalStorageFileSelectDialog(getActivity(), listener);
			}
		};
	}
	

	@Override
	public int getLayoutResource() {
		
		return R.layout.local_storage_tool_area;
	}

}
