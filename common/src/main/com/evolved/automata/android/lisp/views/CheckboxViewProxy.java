package com.evolved.automata.android.lisp.views;

import java.util.HashMap;

import android.view.View;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.CompoundButton;

import com.evolved.automata.lisp.Value;
import android.content.Context;
import com.evolved.automata.android.EvaluateException;


public class CheckboxViewProxy extends TextViewProxy
{
	public static final String CHECKED = ":checked";
	
	boolean _isChecked = false;
	
	CompoundButton.OnCheckedChangeListener _changeListener = null;
	
	public CheckboxViewProxy(Context con, HashMap<String, Value> keymap, String text)
	{
		super(con, keymap, text);
		this.text = text;
	}
	
	public boolean isChecked()
	{
		if (encapsulated == null)
			return _isChecked;
		
		return _isChecked = ((CheckBox)encapsulated).isChecked();
	}
	
	public void setOnCheckChangedListener(final String containlet)
	{
		_changeListener = new CompoundButton.OnCheckedChangeListener() {
			
			@Override
			public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
				
				_lispInterpreter.evaluateExpression(containlet, true);
			}
		};
		if (encapsulated!=null)
		{
			((CheckBox)encapsulated).setOnCheckedChangeListener(_changeListener);
		}
	}
	
	
	@Override
	public void applyAttribures(HashMap<String, Value> keywords)
	{
		super.applyAttribures(keywords);
		processChecked();
		if (encapsulated!=null)
			((CheckBox)encapsulated).setChecked(_isChecked);
	}
	
	public void processChecked()
	{
		try
		{
			Value value = getMapValue(_keys, CHECKED);
			if (!value.isNull() && value.getString().equals("true"))
			{
				_isChecked = true;
			}
			else
				_isChecked = false;
		}
		catch (Exception e)
		{
			throw new EvaluateException("Invalid attribute for check-box");
		}
		
	}
	
	@Override
	public View createBaseView()
	{
		CheckBox tv = new CheckBox(context);
		createBaseView(tv);
		processChecked();
		tv.setChecked(_isChecked);
		return tv;
	}
}
