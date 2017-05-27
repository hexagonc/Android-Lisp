package com.evolved.automata.android.lisp.views;

import java.util.HashMap;

import android.content.Context;
import android.view.View;

import android.widget.CheckBox;
import android.widget.RadioButton;

import com.evolved.automata.android.EvaluateException;
import com.evolved.automata.lisp.Value;

public class RadioButtonProxy extends CheckboxViewProxy
{
	
	public RadioButtonProxy(Context con, HashMap<String, Value> keymap, String text)
	{
		super(con, keymap, text);
		
	}
	
	@Override
	public void applyAttribures(HashMap<String, Value> keywords)
	{
		super.applyAttribures(keywords);
		processChecked();
		View actual;
		if (encapsulated != null && (actual = encapsulated.get())!= null)
			((RadioButton)actual).setChecked(_isChecked);
	}
	
	@Override
	public boolean isChecked()
	{
		View actual;
		if (encapsulated != null && (actual = encapsulated.get())!= null)
			return _isChecked = ((RadioButton)actual).isChecked();
		else
			return _isChecked;
		

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
			throw new EvaluateException("Invalid attribute for radio-button");
		}
		
	}
	
	@Override
	public View createBaseView()
	{
		RadioButton rb = new RadioButton(context);
		createBaseView(rb);
		processChecked();
		processCheckChangedListener();
		rb.setChecked(_isChecked);
		return rb;
	}
	
}
