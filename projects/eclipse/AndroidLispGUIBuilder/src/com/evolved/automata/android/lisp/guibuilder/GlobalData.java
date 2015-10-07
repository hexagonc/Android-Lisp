package com.evolved.automata.android.lisp.guibuilder;

import com.evolved.automata.android.lisp.AndroidLispInterpreter;
import com.evolved.automata.android.lisp.views.ViewProxy;
import com.evolved.automata.lisp.Environment;

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
	
	
	public GlobalData(UIControlListener controlListener)
	{
		_uicListener = controlListener;
	}
	
	public void setLispInterpreter(AndroidLispInterpreter interpreter)
	{
		_interpreter = interpreter;
	}
	
	public AndroidLispInterpreter getInterpreter()
	{
		return _interpreter;
	}
	
	public Environment getEnvironment()
	{
		return _env;
	}
	
	public void setEnvironment(Environment env)
	{
		_env = env;
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
