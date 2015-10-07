package com.evolved.automata.android.lisp.views;

import java.util.HashMap;

import android.view.View;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.CompoundButton;

import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;
import android.content.Context;
import com.evolved.automata.android.EvaluateException;


public class CheckboxViewProxy extends TextViewProxy
{
	public static final String CHECKED = ":checked"; // "true" | "false"
	public static final String CHECK_CHECKED_LISTENER = ":on-check-changed";
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
	
	public void setChecked(boolean checked)
	{
		_isChecked = checked;
		if (encapsulated != null)
		{
			CompoundButton cb = (CompoundButton)encapsulated;
			cb.setChecked(_isChecked);
		}
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
	public void processCheckChangedListener()
	{
		Value listener = getMapValue(_keys, CHECK_CHECKED_LISTENER);
		if (!listener.isNull() && listener.isString())
		{
			final String base = listener.getString();
			_changeListener = new CompoundButton.OnCheckedChangeListener() {
				
				@Override
				public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
					String total = String.format("(let ((is-checked %1$s)) %2$s)", (isChecked)?"1":"F", base); 
					_lispInterpreter.evaluateExpression(total, true);
				}
			};
		}
		if (encapsulated!=null)
			((CompoundButton)encapsulated).setOnCheckedChangeListener(_changeListener);
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
		processCheckChangedListener();
		tv.setChecked(_isChecked);
		return tv;
	}
}
