package com.evolved.automata.android.lisp.guibuilder.toolareas;

import com.evolved.automata.android.lisp.guibuilder.CodeManager;
import com.evolved.automata.android.lisp.guibuilder.R;
import com.evolved.automata.android.lisp.guibuilder.CodeManager.OnCodeTemplateSelectedListener;

import android.app.Fragment;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;

public class CodeTemplateToolAreaFragment extends ToolAreaFragment 
{
	ImageButton _insertButton;
	ImageButton _createButton;
	
	
	public CodeTemplateToolAreaFragment(CodeManagementUIInterface uinterface)
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
	public int getLayoutResource()
	{
		return R.layout.code_template_tool_area;
	}

	@Override
	public void onResume() {
		// TODO Auto-generated method stub
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
		_insertButton = (ImageButton)_cachedView.findViewById(R.id.imgb_open_code_template);
		_insertButton.setOnClickListener(getInsertButtonOnClickListener());
		
		_createButton = (ImageButton)_cachedView.findViewById(R.id.imgb_create_code_template);
		_createButton.setOnClickListener(getTemplateCreateButtonOnClickListener());
		
		
	}
	
	private View.OnClickListener getTemplateCreateButtonOnClickListener()
	{
		return new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				String code = _uiInterface.getHighlightedText();
				if (code!=null && code.trim().length()>0)
				{
					CodeManager.get().showCodeTemplateCreateDialog(getActivity(), code);
				}
				else
					_uiInterface.showHintText("Can't create a blank code template", 7000);
				
				
			}
		};
	}
	
	
	private View.OnClickListener getInsertButtonOnClickListener()
	{
		final OnCodeTemplateSelectedListener listener = new OnCodeTemplateSelectedListener()
		{

			@Override
			public void templateSelected(String template, boolean replaceEditor) {
				if (replaceEditor)
					_uiInterface.replaceCodeEdit(template);
				else
					_uiInterface.insertCodeAtEditCursor(template);
			}
			
		};
		
		return new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				CodeManager.get().showCodeTemplateSelectDialog(getActivity(), listener);
				
				
			}
		};
	}
	
}
