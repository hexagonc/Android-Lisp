package com.evolved.automata.android.lisp.guibuilder;

import android.app.ActionBar;
import android.app.ActionBar.Tab;
import android.app.ActionBar.TabListener;
import android.app.Activity;
import android.app.Fragment;
import android.app.FragmentTransaction;
import android.content.ComponentName;
import android.content.Context;
import android.media.AudioManager;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;

import java.util.ArrayList;
import java.util.HashMap;

import com.evolved.automata.KeyValuePair;
import com.evolved.automata.android.lisp.AndroidLispInterpreter;
import com.evolved.automata.android.lisp.AndroidLispInterpreter.ResponseListener;
import com.evolved.automata.android.lisp.guibuilder.events.ActivityLifeCycleEventListener;
import com.evolved.automata.android.lisp.guibuilder.events.EventManager;
import com.evolved.automata.android.lisp.guibuilder.fragments.CodeEditFragment;
import com.evolved.automata.android.lisp.guibuilder.fragments.LispBuilderFragment;
import com.evolved.automata.android.lisp.guibuilder.fragments.RenderFragment;
import com.evolved.automata.android.lisp.views.ViewEvaluator;
import com.evolved.automata.android.mindstorms.NXTBluetoothManager;
import com.evolved.automata.android.mindstorms.lisp.NXTLispFunctions;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;

public class MainActivity extends Activity implements TabListener, AndroidLispInterpreter.ResponseListener, GlobalInterface.UIControlListener 
{
	
	HashMap<Class<? extends LispBuilderFragment>, String> _fragmentLabelMap = null;
	ArrayList<KeyValuePair<Class<? extends LispBuilderFragment>, String>> _actionBarTabSpec;
	HashMap<String, KeyValuePair<Class<? extends LispBuilderFragment>, LispBuilderFragment>> _fragmentMap;
	HashMap<String, ActionBar.Tab> _tabMap = null;
	
	LispBuilderFragment _currentFragment = null;
	
	GlobalInterface _data = null;
	
	String _CONSOLE_TAB_NAME = "Console";
	String _RENDER_TAB_NAME = "Rendered";
	String _startTabLabel = _CONSOLE_TAB_NAME;
	
	public enum LogType
	{
		INFO, ERROR, VERBOSE
	}
	ActivityLifeCycleEventListener _activityEventListener;
	ComponentName cname;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		_actionBarTabSpec = new ArrayList<KeyValuePair<Class<? extends LispBuilderFragment>, String>>();
		_actionBarTabSpec.add(new KeyValuePair<Class<? extends LispBuilderFragment>,String>(RenderFragment.class, _RENDER_TAB_NAME));
		_actionBarTabSpec.add(new KeyValuePair<Class<? extends LispBuilderFragment>,String>(CodeEditFragment.class, _CONSOLE_TAB_NAME));
		_fragmentMap = new HashMap<>();
		_tabMap = new HashMap<>();
		setContentView(R.layout.activity_main);
		try
		{
			setupGlobalData();
		}
		catch (Exception e)
		{
			log(LogType.ERROR, e.toString());
		}
		
		configureActionBarUI();
		_activityEventListener = EventManager.getInstance().getLifeCycleEventNotifier();
		
		_activityEventListener.onCreate(this);
		AudioManager manager = (AudioManager)getSystemService(Context.AUDIO_SERVICE);
		cname = new ComponentName(this, BluetoothButtonEventReceiver.class);
		manager.registerMediaButtonEventReceiver(cname);
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
	
	private void setupGlobalData() throws InstantiationException, IllegalAccessException
	{
		
		_data = ((GuiBuilderApplication)getApplication()).getGlobalData();
		ViewEvaluator.bindFunctions(_data.getEnvironment(), this, _data.getInterpreter());
		_data.setForegroundLispResponseListener(this);
		_data.setControlListener(this);
		_data.setViewProxy(null);
	}
	
	private void updateWorkspaceInActionbarTitle()
	{
		ActionBar bar = getActionBar();
		bar.setTitle(CodeManager.get().getWorkspace().getCurrentProject().getName());
	}
	
	private void configureActionBarUI()
	{
		ActionBar.Tab tab = null;
		ActionBar bar = getActionBar();
		String label;
		for (KeyValuePair<Class<? extends LispBuilderFragment>, String> spec:_actionBarTabSpec)
		{
			tab = bar.newTab();
			label = spec.GetValue();
			tab.setText(label);
			tab.setTag(label);
			tab.setTabListener(this);
			_fragmentMap.put(label, new KeyValuePair<Class<? extends LispBuilderFragment>, LispBuilderFragment>(spec.GetKey(), null));
			bar.addTab(tab, _startTabLabel.equals(label));
			_tabMap.put(label, tab);
		}
		bar.setNavigationMode(ActionBar.NAVIGATION_MODE_TABS);
		
		bar.show();
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.main, menu);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		// Handle action bar item clicks here. The action bar will
		// automatically handle clicks on the Home/Up button, so long
		// as you specify a parent activity in AndroidManifest.xml.
		
		return MenuManager.get().onOptionsItemSelected(item) || super.onOptionsItemSelected(item);
	}

	@Override
	public void onTabSelected(Tab tab, FragmentTransaction ft) {
		String label = (String)tab.getTag();
		KeyValuePair<Class<? extends LispBuilderFragment>, LispBuilderFragment> kvSpec = _fragmentMap.get(label);
		LispBuilderFragment cachedFrag = kvSpec.GetValue();
		Class<? extends LispBuilderFragment> fclass = kvSpec.GetKey();
		
		if (cachedFrag == null)
		{
			cachedFrag = (LispBuilderFragment)Fragment.instantiate(this, fclass.getName());
			cachedFrag.setGlobalInterface(_data);
			_fragmentMap.put(label, new KeyValuePair<Class<? extends LispBuilderFragment>, LispBuilderFragment>(fclass, cachedFrag));
			ft.add(android.R.id.content, cachedFrag);
		}
		else
			ft.add(android.R.id.content, cachedFrag);
		_currentFragment = cachedFrag;
	}

	@Override
	public void onTabUnselected(Tab tab, FragmentTransaction ft) 
	{
		String label = (String)tab.getTag();
		KeyValuePair<Class<? extends LispBuilderFragment>, LispBuilderFragment> kvSpec = _fragmentMap.get(label);
		LispBuilderFragment cachedFrag = kvSpec.GetValue();
		if (cachedFrag != null)
			ft.remove(cachedFrag);
	}

	@Override
	public void onTabReselected(Tab tab, FragmentTransaction ft) {
		
	}

	@Override
	public void onError(Exception e) {
		if (_currentFragment != null)
			_currentFragment.onError(e);
	}
	
	@Override
	public void onResult(Value v)
	{
		if (_currentFragment != null)
			_currentFragment.onResult(v);
	}

	@Override
	public void switchToRenderTab() {
		switchToTab(_RENDER_TAB_NAME);
	}

	@Override
	public void switchToCommandEditTab() {
		switchToTab(_CONSOLE_TAB_NAME);
		
	}
	
	private void switchToTab(String tagName)
	{
		Tab selected = getActionBar().getSelectedTab();
		if (selected != _tabMap.get(tagName))
			getActionBar().selectTab(_tabMap.get(tagName));
	}

	@Override
	protected void onDestroy() {
		if (!_data.isRunningInBackground())
		{
			_data.shutdownAll();
			if (NXTBluetoothManager.getInstance()!=null)
				NXTBluetoothManager.getInstance().stopNXTBluetoothService();
		}
		_activityEventListener.onDestroy(this);
		AudioManager manager = (AudioManager)getSystemService(Context.AUDIO_SERVICE);
		manager.unregisterMediaButtonEventReceiver(cname);
		super.onDestroy();
	}

	@Override
	public Activity getActivity() {
	
		return this;
	}

	@Override
	protected void onStart() {

		super.onStart();
		updateWorkspaceInActionbarTitle();
		_activityEventListener.onStart(this);
	}

	@Override
	protected void onResume() {

		super.onResume();
		_activityEventListener.onResume(this);
	}

	@Override
	protected void onPause() {
		_activityEventListener.onPause(this);
		super.onPause();
	}

	@Override
	protected void onStop() {
		_activityEventListener.onStop(this);
		super.onStop();
	}

	@Override
	public boolean onPrepareOptionsMenu(Menu menu) {
		
		return MenuManager.get().onPrepareOptionsMenu(menu) || super.onPrepareOptionsMenu(menu);
	}
	
}
