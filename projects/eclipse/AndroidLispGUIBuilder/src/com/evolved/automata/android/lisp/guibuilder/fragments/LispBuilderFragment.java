package com.evolved.automata.android.lisp.guibuilder.fragments;

import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.android.lisp.AndroidLispInterpreter;
import com.evolved.automata.android.lisp.AndroidLispInterpreter.ResponseListener;
import com.evolved.automata.android.lisp.guibuilder.GlobalInterface;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.LispInterpreter;
import com.evolved.automata.lisp.Value;

import android.app.Fragment;
import android.util.Log;

public abstract class LispBuilderFragment extends Fragment implements LispInterpreter.LispResponseListener, AndroidLispInterpreter.ResponseListener 
{
	protected GlobalInterface _data;

	public void setGlobalInterface(GlobalInterface data)
	{
		_data = data;
	}
	
	public GlobalInterface getGlobalInterface()
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
	
	
	public abstract void onEnvironmentReset();
	
}
