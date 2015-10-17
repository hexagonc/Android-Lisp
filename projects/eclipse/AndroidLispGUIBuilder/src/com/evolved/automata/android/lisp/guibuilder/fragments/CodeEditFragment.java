package com.evolved.automata.android.lisp.guibuilder.fragments;



import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;

import com.evolved.automata.KeyValuePair;
import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.android.AppStateManager;
import com.evolved.automata.android.lisp.guibuilder.R;
import com.evolved.automata.android.lisp.guibuilder.MainActivity.LogType;
import com.evolved.automata.android.lisp.views.ViewProxy;
import com.evolved.automata.lisp.Value;

import android.app.Fragment;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.Spinner;
import android.widget.SpinnerAdapter;
import android.widget.TextView;

public class CodeEditFragment extends LispBuilderFragment 
{
	

	ViewGroup _cachedView = null;
	Button _runButton = null;
	Button _renderButton = null;
	Button _clearScreenButton = null;
	EditText _editText = null;
	EditText _messageText = null;
	Spinner _sampleSpinner = null;
	TextView _titleText = null;
	String _prompt;
	Value _lastResult;
	String _initialCommand = null;
	String PRIOR_COMMAND_KEY = "PRIOR_COMMAND_KEY";
	SpinnerAdapter _spinnerAdapter;
	ArrayList<KeyValuePair<String, String> > _codeSamples = new ArrayList<>();
	int _initiallySelectedItem = 0;
	boolean _first = true;
	Handler _main = new Handler(Looper.getMainLooper());
	@Override
	public void onCreate(Bundle savedInstanceState) {
		
		super.onCreate(savedInstanceState);
		
		_prompt = getActivity().getString(R.string.console_prompt);
		if (_first)
		{
			configureCodeDropdown();
			_first = false;
		}
		
		
	}
	
	
	
	private void configureCodeDropdown()
	{
		KeyValuePair<String, String> codeSpec;
		HashMap<String, String> predefinedSampleMap = new HashMap<String, String>();
		predefinedSampleMap.put("tic-tac-toe ui only", "tic_tac_toe_simple.lisp");
		predefinedSampleMap.put("tic-tac-toe with simple ai", "tic_tac_toe_playable.lisp");
		predefinedSampleMap.put("lego mindstorms sample", "mindstorms_sample.lisp");
		predefinedSampleMap.put("test", "test.lisp");
		String[] predefinedSamples = new String[]{"test", "tic-tac-toe ui only", "tic-tac-toe with simple ai", "lego mindstorms sample"};
		String assetCode = null;
		try
		{
			for (String title:predefinedSamples)
			{
				assetCode = AndroidTools.getFileAssetAsString(predefinedSampleMap.get(title), getActivity());
				codeSpec = new KeyValuePair<String, String>(title, assetCode);
				_codeSamples.add(codeSpec);
			}
		}
		catch (Exception e)
		{
			AppStateManager.getInstance().onError("CodeEditFragment:configureCodeDropdown", e);
		}
		
		String prior = null;
		prior = AndroidTools.getStringPreferenceSetting(PRIOR_COMMAND_KEY, null);
		
		if (prior !=null)
		{
			codeSpec = new KeyValuePair<String, String>("previous", prior);
			_codeSamples.add(codeSpec);
			_initialCommand = prior;
		}
		else
			_initialCommand = "";
	}
	
	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {
		
		if (_cachedView == null)
		{
			LinearLayout layout  = (LinearLayout)inflater.inflate(R.layout.lisp_edit_layout, container, false);
			_cachedView = layout;
			configureView();
		}
		return _cachedView;
		
	}

	@Override
	public void onStart() {
		
		super.onStart();
		String prior = AndroidTools.getStringPreferenceSetting(PRIOR_COMMAND_KEY, null);
		if (prior != null)
			_editText.setText(prior);
	}

	@Override
	public void onResume() {
	
		super.onResume();
	}

	@Override
	public void onPause() {
		
		super.onPause();
	}

	@Override
	public void onStop() {
		String command = _editText.getText().toString();
		AndroidTools.setStringPreferenceSetting(PRIOR_COMMAND_KEY, command);
		super.onStop();
	}

	@Override
	public void onDestroyView() {
		super.onDestroyView();
	}
	
	private void configureView()
	{
		_runButton = (Button)_cachedView.findViewById(R.id.but_run);
		_runButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				String command = _editText.getText().toString();
				if (command != null && command.length()>0)
				{
					Value result = _data.getInterpreter().evaluateExpression(command, false);
					if (result != null)
					{
						onResult(result);
					}
				}
			}
		});
		
		_renderButton = (Button)_cachedView.findViewById(R.id.but_render);
		_renderButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				if (_lastResult != null && _lastResult.getObjectValue() instanceof ViewProxy)
				{
					_data.setViewProxy((ViewProxy)_lastResult.getObjectValue());
					_data.switchToRenderTab();
				}
			}
		});
		
		_clearScreenButton = (Button)_cachedView.findViewById(R.id.but_clear);
		_clearScreenButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				_editText.setText("");
			}
		});
		
		_editText = (EditText)_cachedView.findViewById(R.id.edit_code_view);
		_messageText = (EditText)_cachedView.findViewById(R.id.edit_console_output);
		_editText.setHorizontallyScrolling(true);
		_editText.setHorizontalScrollBarEnabled(true);
		_editText.setVerticalScrollBarEnabled(true);
		_titleText = (TextView)_cachedView.findViewById(R.id.txt_title);
				
		_sampleSpinner = (Spinner)_cachedView.findViewById(R.id.spinner_samples);
		
		ArrayAdapter<KeyValuePair<String, String>> _spinnerAdapter = new ArrayAdapter<KeyValuePair<String, String>>(getActivity(), 0, _codeSamples )
				{
					public View getView(int position, View cachedView, ViewGroup parent)
					{
						if (cachedView == null)
						{
							LayoutInflater inflater = getActivity().getLayoutInflater(); 
							cachedView = inflater.inflate(R.layout.code_sample_spinner_item, parent, false);
						}
						TextView actualView = (TextView)cachedView;
						
						KeyValuePair<String, String> titleSpec = _codeSamples.get(position);
						actualView.setText(titleSpec.GetKey());
						return actualView;
					}
					
					public View getDropDownView(int position, View cachedView, ViewGroup parent)
					{
						if (cachedView == null)
						{
							LayoutInflater inflater = getActivity().getLayoutInflater(); 
							cachedView = inflater.inflate(R.layout.code_sample_spinner_item, parent, false);
						}
						TextView actualView = (TextView)cachedView;
						
						KeyValuePair<String, String> titleSpec = _codeSamples.get(position);
						actualView.setText(titleSpec.GetKey());
						return actualView;
					}
				};
		_sampleSpinner.setAdapter(_spinnerAdapter);
		
		_sampleSpinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {

			

			@Override
			public void onItemSelected(AdapterView<?> parent, View view,
					int position, long id) {
				onSampleSpinnerSelected(position);
			}

			@Override
			public void onNothingSelected(AdapterView<?> parent) {
				onSampleSpinnerSelected(_initiallySelectedItem);
			}
			
		});
		_sampleSpinner.setSelection(_initiallySelectedItem);
	}

	@Override
	public void onError(Exception e) {
		appendResult(e.toString());
		log(LogType.ERROR, e.toString());
	}
	
	@Override
	public void onResult(Value v)
	{
		if (v != null)
		{
			if (v.getObjectValue() instanceof ViewProxy)
			{
				_lastResult = v;
			}
			String svalue = v.toString();
			log(LogType.INFO, svalue);
			appendResult(svalue);
		}
		else
			appendResult("null");
	}
	
	private void onSampleSpinnerSelected(int position)
	{
		KeyValuePair<String, String> titleSpec = _codeSamples.get(position);
		String code = titleSpec.GetValue();
		_editText.setText(code);
		_titleText.setText(titleSpec.GetKey());
	}
	
	
	
	private void log(String msg)
	{
		log(LogType.VERBOSE, msg);
	}
	
	private void log(LogType type, String msg)
	{
		switch (type)
		{
			case INFO:
				Log.i("MainActivity", msg);
				break;
			case ERROR:
				Log.e("MainActivity", msg);
				break;
			default:
				Log.v("MainActivity", msg);
				break;
		}
	}
	
	
	private void appendResult(String value)
	{
		String newText = _messageText.getText().toString() + "\n" + _prompt + value;
		
		_messageText.setText(newText);
		_messageText.setSelection(newText.length(), newText.length());
		
	}


	// Called on a background thread
	@Override
	public void onOutput(final Value v) {
		_messageText.post(new Runnable(){
			public void run()
			{
				if (v != null)
				{
					if (v.getObjectValue() instanceof ViewProxy)
					{
						_lastResult = v;
					}
					String svalue = v.toString();
					log(LogType.INFO, svalue);
					appendResult(svalue);
				}
				else
					appendResult("null");
			}
		});
	}



	@Override
	public void onIncompleteInputException(String message) {
		appendResult(message);
		log(LogType.ERROR, message);
	}



	@Override
	public void onGeneralException(Exception e) {
		appendResult(e.toString());
		log(LogType.ERROR, e.toString());
	}
	
	
	
}
