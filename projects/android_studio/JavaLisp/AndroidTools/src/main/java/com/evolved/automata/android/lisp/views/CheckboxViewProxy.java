package com.evolved.automata.android.lisp.views;

import java.lang.ref.WeakReference;
import java.util.HashMap;

import android.view.View;

import android.widget.CheckBox;
import android.widget.CompoundButton;


import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.Lambda;
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
		View actual;
		if (encapsulated != null && (actual = encapsulated.get())!= null)
			return _isChecked = ((CheckBox)actual).isChecked();
		else
			return _isChecked;

	}
	
	public void setChecked(boolean checked)
	{
		_isChecked = checked;
		View actual;
		if (encapsulated != null && (actual = encapsulated.get())!= null)

		{
			CompoundButton cb = (CompoundButton)actual;
			cb.setChecked(_isChecked);
		}
	}
	
	
	
	public void setOnCheckChangedListener(final Value lambdaValue)
	{
        if (!lambdaValue.isLambda())
            return;

        _changeListener = new CompoundButton.OnCheckedChangeListener() {

            @Override
            public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {


                Lambda function = (Lambda) lambdaValue.getLambda();
                function.setActualParameters(new Value[]{NLispTools.makeValue(isChecked)});

                _lispInterpreter.evaluateFunction(function, _currentEnv);
            }
        };

        View actual;
        if (encapsulated != null && (actual = encapsulated.get())!= null)
            ((CompoundButton)actual).setOnCheckedChangeListener(_changeListener);
	}
	
	
	@Override
	public void applyAttribures(HashMap<String, Value> keywords)
	{
		super.applyAttribures(keywords);
		processChecked();
		View actual;
		if (encapsulated != null && (actual = encapsulated.get())!= null)
			((CheckBox)actual).setChecked(_isChecked);
	}
	public void processCheckChangedListener()
	{

		final Value code = getMapValue(_keys, CHECK_CHECKED_LISTENER);
		if (code.isNull())
			return;
		final Value transformed = NLispTools.getMinimalEnvironment(_currentEnv, code);
        _changeListener = new CompoundButton.OnCheckedChangeListener() {

            @Override
            public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {

                Environment evaluatedEnvironment = new Environment(_currentEnv);
                evaluatedEnvironment.mapValue("is-checked-p", NLispTools.makeValue(isChecked));
                _lispInterpreter.evaluatePreParsedValue(evaluatedEnvironment, transformed, true);
            }
        };

		View actual;
		if (encapsulated != null && (actual = encapsulated.get())!= null)
			((CompoundButton)actual).setOnCheckedChangeListener(_changeListener);
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
        encapsulated = new WeakReference<View>(tv);
		createBaseView(tv);
		processChecked();
		processCheckChangedListener();
		tv.setChecked(_isChecked);
		return tv;
	}
}
