package com.evolved.automata.android.lisp.views;

import java.util.ArrayList;
import java.util.HashMap;

import org.apache.commons.lang3.tuple.Triple;

import android.content.Context;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.FrameLayout;
import android.widget.RelativeLayout;
import android.widget.Spinner;

import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.Value;

public class SpinnerViewProxy extends ViewProxy
{
	
	ArrayAdapter<Triple<ViewProxy, ViewProxy, FunctionTemplate>> _adapter = null;
	ArrayList<Triple<ViewProxy, ViewProxy, FunctionTemplate>> _spinnerSpecList = new ArrayList<Triple<ViewProxy, ViewProxy, FunctionTemplate>>();
	AdapterView.OnItemSelectedListener _selectListener = null;
	int _selection = 0;
	
	public SpinnerViewProxy(final Context con, HashMap<String, Value> keywords, ArrayList<Triple<ViewProxy, ViewProxy, FunctionTemplate>> spinnerSpecList)
	{
		super(con, keywords);
		_spinnerSpecList = spinnerSpecList;
		_adapter = new ArrayAdapter<Triple<ViewProxy, ViewProxy, FunctionTemplate>>(con, 0, spinnerSpecList)
				{
		    @Override
			public View getView(int position, View cachedView, ViewGroup listItemParentView)
			{
				if (cachedView == null)
				{
					LayoutInflater inflater = (LayoutInflater)con.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
					RelativeLayout parent = (RelativeLayout)inflater.inflate(com.evolved.automata.android.tools.R.layout.lisp_spinner_item_layout, listItemParentView, false);
					Triple<ViewProxy, ViewProxy, FunctionTemplate> spec = _spinnerSpecList.get(position);
					ViewProxy listDisplayView = spec.getLeft();
					
					parent.addView(listDisplayView.createView(parent));
					return parent;
				}
				else
					return cachedView;
			}
		    
		    @Override
			public View getDropDownView(int position, View cachedView, ViewGroup listItemParentView)
			{
				if (cachedView == null)
				{
					
					LayoutInflater inflater = (LayoutInflater)con.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
					RelativeLayout parent = (RelativeLayout)inflater.inflate(com.evolved.automata.android.tools.R.layout.lisp_spinner_item_layout, listItemParentView, false);
					
					Triple<ViewProxy, ViewProxy, FunctionTemplate> spec = _spinnerSpecList.get(position);
					ViewProxy listDisplayView = spec.getMiddle();
					View v = listDisplayView.createView(parent);

					parent.addView(v);
					return parent;
				}
				else
					return cachedView;
			}
		    
		};
		
		_selectListener = new AdapterView.OnItemSelectedListener() {

			@Override
			public void onItemSelected(AdapterView<?> parent, View view,
					int position, long id) {
				Triple<ViewProxy, ViewProxy, FunctionTemplate> spec = _spinnerSpecList.get(position);
				FunctionTemplate action = spec.getRight();
				_selection = position;
				_lispInterpreter.evaluateFunction(action);
				
			}

			@Override
			public void onNothingSelected(AdapterView<?> parent) {
				Log.d("spinner", "On nothing selected: selection value " + _selection);
			}
		};
	}
	
	public void setSelected(int i)
	{
		_selection = i;
		View actual;
		if (encapsulated != null && (actual = encapsulated.get())!= null)

			((Spinner)actual).setSelection(_selection);
	}
	
	public void updateSpec(ArrayList<Triple<ViewProxy, ViewProxy, FunctionTemplate>> spinnerSpecList)
	{
		_spinnerSpecList.clear();
		_spinnerSpecList.addAll(spinnerSpecList);
		if (encapsulated!=null)
		{
			_adapter.notifyDataSetChanged();
		}
	}

	@Override
	public View createBaseView() {
		Spinner spinner = new Spinner(context);
		spinner.setAdapter(_adapter);
		spinner.setOnItemSelectedListener(_selectListener);
		spinner.setSelection(_selection);
		return spinner;
	}
	
	
}
