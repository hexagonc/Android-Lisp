package com.evolved.automata.android.lisp.views;

import java.util.HashMap;

import android.content.Context;
import android.view.View;
import android.widget.SeekBar;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;

public class SeekBarViewProxy extends ViewProxy
{
	public static final String SEEK_CHANGE_LISTENER_KEY = ":on-value-changed";
	public static final String DEFAULT_SELECTED_VALUE_BINDING_NAME = "value";
	double _minValue = 0;
	double _maxValue = 100;
	double _currentValue = 0;
    double _smoothingMilli = 200;
	
	SeekBar.OnSeekBarChangeListener _seekChangeListener = null;
	public SeekBarViewProxy(Context con, HashMap<String, Value> keywords, double minimumValue, double maximumValue, double smoothingMilli)
	{
		super(con, keywords);
		_minValue = minimumValue;
		_maxValue = maximumValue;
		_smoothingMilli = smoothingMilli;
		final Value changeListenerValue = keywords.get(SEEK_CHANGE_LISTENER_KEY);
		if (changeListenerValue != null && !changeListenerValue.isNull())
		{
			_seekChangeListener = new SeekBar.OnSeekBarChangeListener() {
				long lastProcessMilli = 0;
				@Override
				public void onStopTrackingTouch(SeekBar seekBar) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void onStartTrackingTouch(SeekBar seekBar) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void onProgressChanged(SeekBar seekBar, int progress,
						boolean fromUser) {
					
					if (!fromUser)
						return;

                    if (System.currentTimeMillis() - lastProcessMilli >= _smoothingMilli){
                        _currentValue = progress/100.0*(_maxValue - _minValue) + _minValue;
                        Value dValue = NLispTools.makeValue(_currentValue);

                        Environment parameterEnv = new Environment(_currentEnv);
                        parameterEnv.mapValue(DEFAULT_SELECTED_VALUE_BINDING_NAME, dValue);
                        Value transformed = NLispTools.getMinimalEnvironment(parameterEnv, changeListenerValue);
                        _lispInterpreter.evaluatePreParsedValue(parameterEnv, transformed, true);
                        lastProcessMilli = System.currentTimeMillis();
                    }

					
				}
			};

		}
	}
	
	public void setCurrentValue(double value)
	{
		_currentValue = value;
		updateCurrentValue();
		
	}
	
	private void updateCurrentValue()
	{
		View actual;
		if (encapsulated != null && (actual = encapsulated.get())!= null)
		{
			SeekBar bar = (SeekBar)actual;
			int progressValue = Math.max(0, Math.min(100, (int)((_currentValue - _minValue)/(_maxValue - _minValue)*100)));
			bar.setProgress(progressValue);
		}
	}
	
	public void setMinimumValue(double value)
	{
		_minValue = value;
		updateCurrentValue();
	}
	
	public void setMaximumValue(double value)
	{
		_maxValue = value;
		updateCurrentValue();
	}
	
	public double getCurrentValue()
	{
		return _currentValue;
	}
	
	
	@Override
	public View createBaseView() {
		SeekBar bar = new SeekBar(context);
		bar.setOnSeekBarChangeListener(_seekChangeListener);
		int progressValue = Math.max(0, Math.min(100, (int)((_currentValue - _minValue)/(_maxValue - _minValue)*100)));
		bar.setProgress(progressValue);
		return bar;
	}
}
