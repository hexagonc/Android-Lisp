package com.evolved.automata.android.lisp.guibuilder;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedList;

import com.evolved.automata.KeyValuePair;



import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.support.v4.view.PagerAdapter;
import android.support.v4.view.ViewPager;
import android.text.Editable;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterViewFlipper;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;



public class TemplateManagerViewPager extends ViewPager {
	
	private interface ViewConfigurator
	{
		public int getViewResource();
		public void addView( ViewGroup parent);
		public View myView();
		public void updateView();
	}
	
	Context _parent;
	LinkedHashMap<String, String> _templates = null;
	OnTemplateSelectedListener _listener;
	private boolean _updatedP = false;
	String _selectedTemplate = "";
	boolean _replaceEditorContents = false;
	LinkedHashMap<String, String> _sampleMap = new LinkedHashMap<>();
	
	public TemplateManagerViewPager(Context activity, AttributeSet attrib){
		super(activity, attrib);
		_parent = activity;
		
	}
	
	@Override
	public boolean onInterceptTouchEvent(MotionEvent arg0) {
		
		return false;
	}

	@Override
	public boolean onTouchEvent(MotionEvent arg0) {
		
		return false;
	}
	
	
	public void setData(LinkedHashMap<String, String> templates, OnTemplateSelectedListener listener)
	{
		_templates = templates;
		_listener = listener;
		configureView();
	}
	
	public TemplateManagerViewPager(Activity activity, LinkedHashMap<String, String> templates, OnTemplateSelectedListener listener)
	{
		super(activity);
		_parent = activity;
		_templates = templates;
		_listener = listener;
		configureView();
	}
	
	private void configureView()
	{
		final ArrayList<ViewConfigurator> configList = new ArrayList<ViewConfigurator>();
		configList.add(new CodeTemplateSelectConfigurator());
		configList.add(new CodePreviewConfigurator());
		
		PagerAdapter templateAdapter = new PagerAdapter  ()
				{
					@Override
					public Object instantiateItem(ViewGroup parent, int position)
					{
						ViewConfigurator conf = configList.get(position);
						conf.addView(parent);
						conf.updateView();
						
						return conf;
					}

					@Override
					public int getCount() {
						
						return 2;
					}

					@Override
					public boolean isViewFromObject(View arg0, Object arg1) {
						ViewConfigurator c = (ViewConfigurator)arg1;
						return c.myView() == arg0;
					}
				};
		setAdapter(templateAdapter);
		
		setOnPageChangeListener(new ViewPager.OnPageChangeListener() {
			
			@Override
			public void onPageSelected(int position) {
				ViewConfigurator conf = configList.get(position);
				conf.updateView();
			}
			
			@Override
			public void onPageScrolled(int arg0, float arg1, int arg2) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void onPageScrollStateChanged(int arg0) {
				// TODO Auto-generated method stub
				
			}
		});
	}
	
	
	private class CodeTemplateSelectConfigurator implements ViewConfigurator
	{
		
		EditText templateNameEdit;
		ListView templateList;
		ArrayList<String> _templateNameList;
		ArrayAdapter<String> _listAdapter;
		Button previewTemplatesButton;
		View myView;
		
		
		public CodeTemplateSelectConfigurator()
		{
			
		}

		@Override
		public int getViewResource() {
			
			return R.layout.code_template_select_page;
		}
		
		public void updateView()
		{
			
		}
		
		private void mergePredefinedUserDefinedTemplates()
		{
			_templateNameList.clear();
			LinkedList<KeyValuePair<String, String>> samples = CodeManager.get().getPredefinedCodeTemplates();
			String key, value;
			for (KeyValuePair<String, String> pair:samples)
			{
				key = pair.GetKey();
				
				if (!_templates.containsKey(key))
				{
					_templateNameList.add(pair.GetKey());
				}
				_sampleMap.put(pair.GetKey(), pair.GetValue());
			}
			
			_templateNameList.addAll(_templates.keySet());
		}

		@Override
		public void addView(ViewGroup pager) 
		{
			if (myView == null)
			{
				_templateNameList = new ArrayList<String>();
				LayoutInflater inflater = (LayoutInflater)_parent.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
				myView = inflater.inflate(getViewResource(), pager, false);
				ViewGroup container = (ViewGroup)myView;
				templateNameEdit = (EditText)container.findViewById(R.id.edit_template_name);
				templateList = (ListView)container.findViewById(R.id.lst_template_names);
				
				_listAdapter = new ArrayAdapter<String>(_parent, R.layout.code_template_select_list_item, _templateNameList);
				templateList.setAdapter(_listAdapter);
				
				templateList.setOnItemLongClickListener(new AdapterView.OnItemLongClickListener() {

					@Override
					public boolean onItemLongClick(AdapterView<?> parent, View view,
							int position, long id) {
						_selectedTemplate =  _templateNameList.get(position);
						
						finish();
						
						return true;
					}
					
				});
				
				templateList.setOnItemClickListener(new AdapterView.OnItemClickListener() {

					@Override
					public void onItemClick(AdapterView<?> parent, View view,
							int position, long id) {
						_selectedTemplate = _templateNameList.get(position);
						templateNameEdit.setText(_selectedTemplate);
						updatePreviewButton();
					}
				});
				
				previewTemplatesButton = (Button)container.findViewById(R.id.but_preview_template);
				previewTemplatesButton.setOnClickListener(new View.OnClickListener() {
					
					@Override
					public void onClick(View v) {
						
						setCurrentItem(1);
					}
				});
				
				mergePredefinedUserDefinedTemplates();
				_listAdapter.notifyDataSetChanged();
				
				updatePreviewButton();
				
				Button okButton = (Button)container.findViewById(R.id.but_load_template);
				okButton.setOnClickListener(new View.OnClickListener() {
					
					@Override
					public void onClick(View v) {
						finish();
					}
				});
				Button cancel = (Button)container.findViewById(R.id.but_cancel_op);
				cancel.setOnClickListener(new View.OnClickListener() {
					
					@Override
					public void onClick(View v) {
						_selectedTemplate = null;
						finish();
						
					}
				});
				
				CheckBox check = (CheckBox)container.findViewById(R.id.chk_replace_editor);
				_replaceEditorContents = check.isChecked();
				check.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
					
					@Override
					public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
						_replaceEditorContents = isChecked;
					}
				});
				
				Button renameTemplate = (Button)container.findViewById(R.id.but_rename_template);
				renameTemplate.setOnClickListener(new View.OnClickListener() {
					
					@Override
					public void onClick(View v) {
						if (_selectedTemplate.length()>0)
						{
							if (templateNameEdit.getText().toString().trim().length()>0)
							{
								String priorValue = _templates.get(_selectedTemplate);
								_templates.remove(_selectedTemplate);
								_updatedP = true;
								_selectedTemplate = templateNameEdit.getText().toString().trim();
								_templates.put(_selectedTemplate , priorValue);
								
								_templateNameList.clear();
								_templateNameList.addAll(_templates.keySet());
								_listAdapter.notifyDataSetChanged();
							}
							else
							{
								Toast.makeText(_parent, "Need a valid name to rename '" + _selectedTemplate + "' to", Toast.LENGTH_LONG).show();
							}
							
						}
						else
						{
							Toast.makeText(_parent, "You must select a template to rename", Toast.LENGTH_LONG).show();
						}
					}
				});
				
				Button deleteTemplate = (Button)container.findViewById(R.id.but_delete_template);
				deleteTemplate.setOnClickListener(new View.OnClickListener() {
					
					@Override
					public void onClick(View v) {
						if (_selectedTemplate.length()>0)
						{
							if (templateNameEdit.getText().toString().trim().length()>0)
							{
								
								_updatedP = true;
								
								showConfirmationDialog();
							}
							else
							{
								Toast.makeText(_parent, "Need a valid name to delete '" + _selectedTemplate + "' to", Toast.LENGTH_LONG).show();
							}
							
						}
						else
						{
							Toast.makeText(_parent, "You must select a template to delete", Toast.LENGTH_LONG).show();
						}
					}
				});
				
				pager.addView(container);
			}
			
			
		}
		
		private void updatePreviewButton()
		{
			previewTemplatesButton.setEnabled(_selectedTemplate.trim().length()>0);
			
		}
		
		private void showConfirmationDialog()
		{
			AlertDialog.Builder builder = new AlertDialog.Builder(_parent);
			AlertDialog dialog = builder.setTitle("Delete Template " + _selectedTemplate + "?").setMessage("This is irreversible.").setPositiveButton("Ok",  new DialogInterface.OnClickListener() {
				
				
				@Override
				public void onClick(DialogInterface dialog, int which) {
					_templates.remove(_selectedTemplate);
					mergePredefinedUserDefinedTemplates();
					_listAdapter.notifyDataSetChanged();
					_selectedTemplate = "";
					templateNameEdit.setText(_selectedTemplate);
					updatePreviewButton();
					dialog.dismiss();
				}
			}).setNegativeButton("Cancel", new DialogInterface.OnClickListener() {
				
				@Override
				public void onClick(DialogInterface dialog, int which) {
					dialog.dismiss();
				}
			}).create();
			dialog.show();
		}

		@Override
		public View myView() {
			
			return myView;
		}
		
		
	}
	
	
	private class CodePreviewConfigurator implements ViewConfigurator
	{
		
		EditText templateValueEdit;
		TextView selectedView;
		View myView = null;
		public CodePreviewConfigurator()
		{
			
		}
		
		@Override
		public View myView() {
			
			return myView;
		}

		@Override
		public int getViewResource() {
			
			return R.layout.code_template_preview_page;
		}
		
		
		public void updateView()
		{
			selectedView.setText(_selectedTemplate);
			if (_templates.containsKey(_selectedTemplate))
				templateValueEdit.setText(_templates.get(_selectedTemplate));
			else
				templateValueEdit.setText(_sampleMap.get(_selectedTemplate));
		}

		@Override
		public void addView(ViewGroup pager) {
			if (myView == null)
			{
				LayoutInflater inflater = (LayoutInflater)_parent.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
				myView = inflater.inflate(getViewResource(), pager, false);
				ViewGroup parent = (ViewGroup)myView;
				
				templateValueEdit = (EditText)parent.findViewById(R.id.edit_template_view);
				
				selectedView = (TextView)parent.findViewById(R.id.txt_template_name);
				
				Button okButton = (Button)parent.findViewById(R.id.but_load_template);
				okButton.setOnClickListener(new View.OnClickListener() {
					
					@Override
					public void onClick(View v) {
						finish(templateValueEdit.getText().toString());
					}
				});
				Button cancel = (Button)parent.findViewById(R.id.but_cancel_op);
				cancel.setOnClickListener(new View.OnClickListener() {
					
					@Override
					public void onClick(View v) {
						_selectedTemplate = null;
						finish();
						
					}
				});
				
				Button backTemplates = (Button)parent.findViewById(R.id.but_back);
				backTemplates.setOnClickListener(new View.OnClickListener() {
					
					@Override
					public void onClick(View v) {
						setCurrentItem(0);
					}
				});
				
				Button updateTemplate = (Button)parent.findViewById(R.id.but_update_template);
				updateTemplate.setOnClickListener(new View.OnClickListener() {
					
					@Override
					public void onClick(View v) {
						if (templateValueEdit.getText().toString().trim().length()>0)
						{
							_templates.put(_selectedTemplate , templateValueEdit.getText().toString().trim());
							_updatedP = true;
							Toast.makeText(_parent, "Updated definition of: '" + _selectedTemplate + "'", Toast.LENGTH_LONG).show();
						}
						else
						{
							Toast.makeText(_parent, "No contents defined for template", Toast.LENGTH_LONG).show();
						}
					}
				});
				
				pager.addView(parent);
			}
			
			updateView();
			
			
			
		}
		
		
	}
	
	
	private void finish()
	{
		String value = null;
		if (_templates.containsKey(_selectedTemplate))
			value = _templates.get(_selectedTemplate);
		else
			value = _sampleMap.get(_selectedTemplate);
		
		if (_updatedP)
			_listener.onTemplateSelectedWithUpdates(_selectedTemplate, value, _templates, _replaceEditorContents);
		else
			_listener.onTemplateSelected(_selectedTemplate, value, _replaceEditorContents);
	}
	
	private void finish(String valueOverride)
	{
		if (_updatedP)
			_listener.onTemplateSelectedWithUpdates(_selectedTemplate, valueOverride, _templates, _replaceEditorContents);
		else
			_listener.onTemplateSelected(_selectedTemplate, valueOverride, _replaceEditorContents);
	}
	
	
	
	
}
