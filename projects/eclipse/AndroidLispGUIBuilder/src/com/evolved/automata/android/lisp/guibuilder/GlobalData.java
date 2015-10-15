package com.evolved.automata.android.lisp.guibuilder;

import android.content.Context;

import com.evolved.automata.android.lisp.AndroidLispInterpreter;
import com.evolved.automata.android.lisp.views.ViewEvaluator;
import com.evolved.automata.android.lisp.views.ViewProxy;
import com.evolved.automata.android.mindstorms.NXTBluetoothManager;
import com.evolved.automata.android.mindstorms.lisp.NXTLispFunctions;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.LispInterpreter;
import com.evolved.automata.lisp.NLispTools;

public class GlobalData 
{
	public interface UIControlListener
	{
		public void switchToRenderTab();
		public void switchToCommandEditTab();
	}
	
	Environment _env;
	ViewProxy _proxy;
	AndroidLispInterpreter _interpreter;
	UIControlListener _uicListener;
	LispInterpreter _backgroundInterpreter;
	Context _context = null;
	
	public GlobalData(Context context) throws InstantiationException, IllegalAccessException
	{
		_context = context;
		_env = new Environment();
		_interpreter = new AndroidLispInterpreter(context, _env, null);
		
		_backgroundInterpreter = new LispInterpreter(_env);
		
		NLispTools.addDefaultFunctionsAddMacros(_env);
		//ViewEvaluator.bindFunctions(_env, _context, _interpreter);
		ExtendedFunctions.addExtendedFunctions(_env);
		NXTLispFunctions.addFunctions(_env, NXTBluetoothManager.getInstance());
		
	}
	
	public Environment getEnvironment()
	{
		return _env;
	}
	
	public void setControlListener(UIControlListener controlListener)
	{
		_uicListener = controlListener;
	}
	
	public void setForegroundLispResponseListener(AndroidLispInterpreter.ResponseListener responseListener)
	{
		_interpreter.setErrorListener(responseListener);
	}
	
	public LispInterpreter.LispInputListener setBackgroundLispResponseListener(LispInterpreter.LispResponseListener responseListener)
	{
		return _backgroundInterpreter.start(responseListener, true);
	}
	
	public AndroidLispInterpreter getInterpreter()
	{
		return _interpreter;
	}
	
	
	public void setViewProxy(ViewProxy proxy)
	{
		_proxy = proxy;
	}
	
	public ViewProxy getViewProxy()
	{
		return _proxy;
	}
	
	public void switchToRenderTab()
	{
		_uicListener.switchToRenderTab();
	}
	
	public void switchToCommandEditTab()
	{
		_uicListener.switchToCommandEditTab();
		
	}
}
