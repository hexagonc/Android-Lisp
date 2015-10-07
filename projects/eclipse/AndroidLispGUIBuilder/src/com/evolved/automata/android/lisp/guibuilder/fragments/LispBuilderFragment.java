package com.evolved.automata.android.lisp.guibuilder.fragments;

import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.android.lisp.AndroidLispInterpreter;
import com.evolved.automata.android.lisp.AndroidLispInterpreter.ResponseListener;
import com.evolved.automata.android.lisp.guibuilder.GlobalData;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.Value;

import android.app.Fragment;
import android.util.Log;

public abstract class LispBuilderFragment extends Fragment implements AndroidLispInterpreter.ResponseListener 
{
	protected GlobalData _data;

	public void setData(GlobalData data)
	{
		_data = data;
	}
	
	public GlobalData getData(GlobalData data)
	{
		return _data;
	}
	
	@Override
	public void onError(Exception e) 
	{
		AndroidTools.showshortMessageToast(e.toString(), getActivity());
		Log.e("RenderFragment", e.toString());
	}
	
	@Override
	public void onResult(Value v)
	{
		
	}
	
}
