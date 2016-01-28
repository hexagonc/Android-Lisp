package com.evolved.automata.android.lisp.guibuilder.toolareas;

import com.evolved.automata.android.lisp.guibuilder.CodeManager;
import com.evolved.automata.android.lisp.guibuilder.MenuManager;
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

		super.onResume();
		MenuManager.get().setCodeEditorMenuType(MenuManager.MenuType.DEFAULT_AND_CODE_TEMPLATES);
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
				_uiInterface.showTemplateCreateDialog();
				
			}
		};
	}
	
	
	private View.OnClickListener getInsertButtonOnClickListener()
	{
		
		
		return new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				_uiInterface.showTemplateSelectDialog();
				
				
			}
		};
	}
	
}
