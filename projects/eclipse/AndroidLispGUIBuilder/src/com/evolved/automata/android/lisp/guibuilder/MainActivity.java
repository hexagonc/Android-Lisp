package com.evolved.automata.android.lisp.guibuilder;

import android.app.ActionBar;
import android.app.ActionBar.Tab;
import android.app.ActionBar.TabListener;
import android.app.Activity;
import android.app.Fragment;
import android.app.FragmentTransaction;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;

import java.util.ArrayList;
import java.util.HashMap;

import com.evolved.automata.KeyValuePair;
import com.evolved.automata.android.lisp.AndroidLispInterpreter;
import com.evolved.automata.android.lisp.AndroidLispInterpreter.ResponseListener;
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

public class MainActivity extends Activity implements TabListener, AndroidLispInterpreter.ResponseListener, GlobalData.UIControlListener 
{
	
	HashMap<Class<? extends LispBuilderFragment>, String> _fragmentLabelMap = null;
	ArrayList<KeyValuePair<Class<? extends LispBuilderFragment>, String>> _actionBarTabSpec;
	HashMap<String, KeyValuePair<Class<? extends LispBuilderFragment>, LispBuilderFragment>> _fragmentMap;
	HashMap<String, ActionBar.Tab> _tabMap = null;
	
	AndroidLispInterpreter _interpreter;
	Environment _lispEnvironnent;
	LispBuilderFragment _currentFragment = null;
	GlobalData _data = null;
	
	String _CONSOLE_TAB_NAME = "Console";
	String _RENDER_TAB_NAME = "Render";
	String _startTabLabel = _CONSOLE_TAB_NAME;
	
	public enum LogType
	{
		INFO, ERROR, VERBOSE
	}
	
	
	
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
		_lispEnvironnent = new Environment();
		_interpreter = new AndroidLispInterpreter(this, _lispEnvironnent, this);
		_data = new GlobalData(this);
		
		
		NLispTools.addDefaultFunctionsAddMacros(_lispEnvironnent);
		ViewEvaluator.bindFunctions(_lispEnvironnent, this, _interpreter);
		ExtendedFunctions.addExtendedFunctions(_lispEnvironnent);
		NXTLispFunctions.addFunctions(_lispEnvironnent, NXTBluetoothManager.getInstance());
		_data.setLispInterpreter(_interpreter);
		_data.setEnvironment(_lispEnvironnent);
		_data.setViewProxy(null);
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
		int id = item.getItemId();
		if (id == R.id.action_settings) {
			return true;
		}
		return super.onOptionsItemSelected(item);
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
			cachedFrag.setData(_data);
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
		
		super.onDestroy();
	}
	
}
