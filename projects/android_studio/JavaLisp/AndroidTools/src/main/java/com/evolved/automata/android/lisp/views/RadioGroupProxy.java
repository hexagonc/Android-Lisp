package com.evolved.automata.android.lisp.views;

import java.util.HashMap;
import java.util.LinkedList;

import android.content.Context;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.RadioGroup;

import com.evolved.automata.lisp.Value;

public class RadioGroupProxy extends LinearLayoutViewProxy
{
	static final String CHECKED_CHANGED_LISTENER = ":on-checked-changed"; // String lisp expression.  This expression will be evaluated
																		 // in a lexical scope where the integer variable "checked-button-id" will 
																		// will be defined.
	
	RadioGroup.OnCheckedChangeListener _changeListener = null;
	public RadioGroupProxy(Context con, HashMap<String, Value> keymap, int oorientation)
	{
		super(con, keymap, oorientation);
		processRadioKeys(keymap);
	}
	
	private void processRadioKeys(HashMap<String, Value> keymap)
	{
		Value listener = getMapValue(keymap, CHECKED_CHANGED_LISTENER);
		if (!listener.isNull() && listener.isString())
		{
			final String baseExpression = listener.getString();
			_changeListener = new RadioGroup.OnCheckedChangeListener()
			{

				@Override
				public void onCheckedChanged(RadioGroup group, int checkedId) {
					String totalExpression = String.format("(let ((checked-button-id %1$s)) %2$s )", baseExpression);
					_lispInterpreter.evaluateExpression(totalExpression, false);
				}
				
			};
					
		}
	}
	
	@Override
	public ViewGroup getPreconfiguredView()
	{
		RadioGroup layout = new RadioGroup(context);
		processChildAlignment(_keys);
		layout.setOrientation(orientation);
		layout.setGravity(gravity);
		
		return layout;
	}
	
	 
	
}
