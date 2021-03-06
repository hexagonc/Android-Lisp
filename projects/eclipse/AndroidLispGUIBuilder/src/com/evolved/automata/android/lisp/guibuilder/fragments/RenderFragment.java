package com.evolved.automata.android.lisp.guibuilder.fragments;

import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.android.lisp.guibuilder.MenuManager;
import com.evolved.automata.android.lisp.guibuilder.R;
import com.evolved.automata.android.lisp.views.ViewProxy;
import com.evolved.automata.lisp.Value;

import android.app.Activity;
import android.app.Fragment;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;

public class RenderFragment extends LispBuilderFragment
{

	RelativeLayout _cachedView = null;
	ViewProxy _currentProxy = null;
	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {
		if (_cachedView == null)
		{
			_cachedView = (RelativeLayout)inflater.inflate(R.layout.lisp_render_view, container, false);
			updateView();
		}
		return _cachedView;
	}

	@Override
	public void onStart() {
		
		super.onStart();
		MenuManager.get().setRenderingMenuMode();
	}

	@Override
	public void onResume() {

		super.onResume();
		if (_data.getViewProxy() == null)
			onEnvironmentReset();
	}

	@Override
	public void onPause() {
		// TODO Auto-generated method stub
		super.onPause();
	}

	@Override
	public void onStop() {
		// TODO Auto-generated method stub
		super.onStop();
	}

	@Override
	public void onDestroyView() {
		ViewProxy.clearDetachedViewProxies();
		super.onDestroyView();
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

	@Override
	public void onAttach(Activity activity) {
		super.onAttach(activity);
		updateView();
		
	}
	
	private void updateView()
	{
		if (_cachedView == null)
			return;
		if (_currentProxy != _data.getViewProxy() && null != _data.getViewProxy())
		{
			_currentProxy = _data.getViewProxy();
			try
			{
				_cachedView.removeAllViews();
				ViewProxy.clearDetachedViewProxies();
				View result = _currentProxy.createView(_cachedView, new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT));
				if (result != null)
				{
					_cachedView.addView(result);
				}
			}
			catch (Exception e)
			{
				onError(e);
			}
			
		}
	}

	@Override
	public void onOutput(Value out) {
		
	}

	@Override
	public void onIncompleteInputException(String message) {
		AndroidTools.showshortMessageToast(message, getActivity());
		Log.e("RenderFragment", message);
	}

	@Override
	public void onGeneralException(Throwable e) {
		AndroidTools.showshortMessageToast(e.toString(), getActivity());
		Log.e("RenderFragment", e.toString());
	}
	
	

	@Override
	public void onEnvironmentReset() {
		_currentProxy = null;
		if (_cachedView != null)
			_cachedView.removeAllViews();
	}
	
}
