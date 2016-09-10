package com.evolved.automata.android.lisp.guibuilder.toolareas;

import com.evolved.automata.android.lisp.guibuilder.CodeManager;
import com.evolved.automata.android.lisp.guibuilder.GuiBuilderConfiguration;
import com.evolved.automata.android.lisp.guibuilder.CodeManager.OnFileSelectedListener;
import com.evolved.automata.android.lisp.guibuilder.CodeManager.PathProtocol;
import com.evolved.automata.android.lisp.guibuilder.fragments.CodeEditFragment;

import android.app.Fragment;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;

public abstract class ToolAreaFragment extends Fragment
{
	protected CodeManagementUIInterface _uiInterface;
	
	
	ViewGroup _cachedView = null;
	
	public ToolAreaFragment(CodeManagementUIInterface uinterface)
	{
		super();
		_uiInterface = uinterface;
	}
	
	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {
		
		if (_cachedView == null)
		{
			_cachedView = (ViewGroup)inflater.inflate(getLayoutResource(), container, false);
			configureLayout();
		}
		return _cachedView;
	}
	
	@Override
	public void onDestroyView() {
		
		super.onDestroyView();
		_cachedView = null;
	}
	
	protected OnFileSelectedListener getFileSelectedListener()
	{
		return new OnFileSelectedListener()
		{

			@Override
			public void onFileSelected(String path, String fileContents,
					PathProtocol protocol, boolean replaceEditor) {
				if (replaceEditor)
				{
					_uiInterface.replaceCodeEdit(fileContents);
				}
				else
					_uiInterface.insertCodeAtEditCursor(fileContents);
				String previousUri = CodeManager.get().getLastLoadedFileUrl();
				_uiInterface.setCodeTitle(CodeManager.get().getShortFileNameFromPathUrl(previousUri));
				
				
			}

			@Override
			public void onError(String message, Exception e) {
				onLoadError(message, e);
			}
			
		};
	}
	
	protected void onLoadError(String message, Exception e)
	{
		_uiInterface.onError(message, e);
	}
	
	public abstract void configureLayout();
	public abstract int getLayoutResource();
}
