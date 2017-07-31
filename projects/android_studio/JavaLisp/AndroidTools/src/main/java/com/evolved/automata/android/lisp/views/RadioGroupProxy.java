package com.evolved.automata.android.lisp.views;

import java.util.HashMap;
import java.util.LinkedList;

import android.content.Context;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.RadioGroup;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;

public class RadioGroupProxy extends LinearLayoutViewProxy
{

	RadioGroup.OnCheckedChangeListener _changeListener = null;
	public RadioGroupProxy(Context con, HashMap<String, Value> keymap, int oorientation)
	{
		super(con, keymap, oorientation);
		processRadioKeys(keymap);
	}
	
	private void processRadioKeys(HashMap<String, Value> keymap)
	{
		final Value code = getMapValue(keymap, CheckboxViewProxy.CHECK_CHECKED_LISTENER);
		if (!code.isNull())
		{
			final Value transformed = NLispTools.getMinimalEnvironment(_currentEnv, code);
			_changeListener = new RadioGroup.OnCheckedChangeListener()
			{

				@Override
				public void onCheckedChanged(RadioGroup group, int checkedId) {
					Environment newEnviroment = new Environment(_currentEnv);
					newEnviroment.mapValue("checked-index", NLispTools.makeValue(checkedId));
					_lispInterpreter.evaluatePreParsedValue(newEnviroment, transformed, true);
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
