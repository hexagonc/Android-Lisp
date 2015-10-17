package com.evolved.automata.android.lisp.guibuilder;

import android.app.Activity;
import android.content.Context;

import com.evolved.automata.android.lisp.AndroidLispInterpreter;
import com.evolved.automata.android.lisp.views.ViewEvaluator;
import com.evolved.automata.android.lisp.views.ViewProxy;
import com.evolved.automata.android.mindstorms.NXTBluetoothManager;
import com.evolved.automata.android.mindstorms.lisp.NXTLispFunctions;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.LispInterpreter;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;

public class GlobalInterface implements LispInterpreter.LispResponseListener, AndroidLispInterpreter.ResponseListener
{
	public interface UIControlListener
	{
		public void switchToRenderTab();
		public void switchToCommandEditTab();
		public Activity getActivity();
	}
	
	public interface LispResponseListener
	{
		public void onBackgroundResult(Value value);
		public void onForegroundResult(Value value);
		public void onBackgroundError(String message);
		public void onForegroundError(Exception e);
	}
	
	Environment _env;
	ViewProxy _proxy;
	AndroidLispInterpreter _interpreter;
	UIControlListener _uicListener;
	LispInterpreter _backgroundInterpreter;
	Context _context = null;
	LispInterpreter.LispResponseListener _backgroundResponseListener = null;
	
	LispInterpreter.LispInputListener _backgroundLispControlListener = null;
	AndroidLispInterpreter.ResponseListener _foregroundResponseListener = null;
	LispResponseListener _generalLispListener = null;
	
	boolean _callBothCallbacks = false;
	
	public GlobalInterface(Context context) throws InstantiationException, IllegalAccessException
	{
		_context = context;
		_env = new Environment();
		_interpreter = new AndroidLispInterpreter(context, _env, null);
		
		_backgroundInterpreter = new LispInterpreter(_env);
		
		NLispTools.addDefaultFunctionsAddMacros(_env);
		//ViewEvaluator.bindFunctions(_env, _context, _interpreter);
		ExtendedFunctions.addExtendedFunctions(_env);
		NXTLispFunctions.addFunctions(_env, NXTBluetoothManager.getInstance());
		_interpreter.setResponseListener(this);
		_backgroundLispControlListener = _backgroundInterpreter.start(this, true);
		_env.mapFunction("evaluate-background", evaluate_background());
		_env.mapFunction("evaluate-foreground", evaluate_foreground());
	}
	
	private FunctionTemplate evaluate_background()
	{
		return new FunctionTemplate ()
		{

			public Object clone()
			{
				return evaluate_background();
			}
			
			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				_backgroundLispControlListener.evaluateValue(_actualParameters[0]);
				return Environment.getNull();
			}
			
		};
	}
	
	private FunctionTemplate evaluate_foreground()
	{
		return new FunctionTemplate ()
		{

			public Object clone()
			{
				return evaluate_foreground();
			}
			
			@Override
			public Value evaluate(Environment env, boolean resume)
					throws InstantiationException, IllegalAccessException {
				_interpreter.evaluatePreParsedValue(env, _actualParameters[0], true);
				return Environment.getNull();
			}
			
		};
	}
	
	
	public Environment getEnvironment()
	{
		return _env;
	}
	
	
	
	// .oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.
	//						UI Interface Methods
	// These methods allow owners of the GlobalInterface to control the
	// user interface.
	// .oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.
	
	/**
	 * Called by loosely coupled clients that need to be switch to the render tab
	 */
	public void switchToRenderTab()
	{
		_uicListener.switchToRenderTab();
	}
	
	/**
	 * Called by loosely coupled clients that need to switch to the command edit tab
	 */
	public void switchToCommandEditTab()
	{
		_uicListener.switchToCommandEditTab();
		
	}
	
	/**
	 * Sets the ViewProxy that will be rendered on the RenderTab.  This is set by a code that generate
	 * the user interface from Lisp
	 * @param proxy
	 */
	public void setViewProxy(ViewProxy proxy)
	{
		_proxy = proxy;
	}
	
	/**
	 * Used to get the last created ViewProxy by the Lisp process
	 * @return
	 */
	public ViewProxy getViewProxy()
	{
		return _proxy;
	}
	
	/**
	 * Sets the object that will actually execute the UI commands.  This will tend to be an Activity.  This
	 * method would normally be set by a closely coupled external object, often that object that creates the
	 * GlobalInterface or a closely related object
	 * @param controlListener
	 */
	public void setControlListener(UIControlListener controlListener)
	{
		_uicListener = controlListener;
	}
	
	
	// <>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>
	//						Set External Lisp Response Listeners
	// Set listeners for responses from Lisp interpreters
	// <>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>+<>
	
	
	/**
	 * Use this method to define single callback that can receive both foreground and background
	 * lisp responses
	 * @param listener
	 */
	public void setLispListener(LispResponseListener listener)
	{
		_generalLispListener = listener;
	}
	
	
	public void setForegroundLispResponseListener(AndroidLispInterpreter.ResponseListener responseListener)
	{
		_foregroundResponseListener = responseListener;
	}
	
	public void setBackgroundLispResponseListener(LispInterpreter.LispResponseListener responseListener)
	{
		_backgroundResponseListener = responseListener;
		
	}
	
	// <~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~>
	//					Lisp Control Interfaces 
	//	These are methods that allow external clients to control the lisp process, 
	//	primarily by executing string lisp source code and evaluating pre-parsed
	//	lisp expressions
	// <~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~>
	
	
	/**
	 * This method provides the foreground lisp interpreter which allows parsing and evaluating code on the
	 * foreground (main thread)
	 * @return
	 */
	public AndroidLispInterpreter getInterpreter()
	{
		return _interpreter;
	}
	
	/**
	 * This provides an interface that allows evaluating expressions on a background thread
	 * @return
	 */
	public LispInterpreter.LispInputListener getBackgroundLispController()
	{
		return _backgroundLispControlListener;
	}
	

	// _-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-_
	//									AndroidLispInterpreter.ResponseListener
	// _-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-__-^-_
	
	@Override
	public void onError(Exception e) {
		if (_generalLispListener != null)
		{
			_generalLispListener.onForegroundError(e);
			if (_callBothCallbacks && _foregroundResponseListener != null)
				_foregroundResponseListener.onError(e);
		}
		else
		if (_foregroundResponseListener != null)
			_foregroundResponseListener.onError(e);
		
	}

	@Override
	public void onResult(Value v) {
		if (_generalLispListener != null)
		{
			_generalLispListener.onForegroundResult(v);
			if (_callBothCallbacks && _foregroundResponseListener != null)
				_foregroundResponseListener.onResult(v);
		}
		else
		if (_foregroundResponseListener != null)
			_foregroundResponseListener.onResult(v);	
	}
	
	// ,-`\__,-`\__,-`\__,-`\__,-`\__,-`\__,-`\__,-`\__,-`\__,-`\__,-`\__,-`\__,-`\__
	//								LispInterpreter.LispResponseListener callback methods
	// ,-`\__,-`\__,-`\__,-`\__,-`\__,-`\__,-`\__,-`\__,-`\__,-`\__,-`\__,-`\__,-`\__
	
	
	@Override
	public void onOutput(Value out) {
		if (_generalLispListener != null)
		{
			_generalLispListener.onBackgroundResult(out);
			if (_callBothCallbacks && _backgroundResponseListener != null)
				_backgroundResponseListener.onOutput(out);
		}
		else
		if (_backgroundResponseListener != null)
			_backgroundResponseListener.onOutput(out);
	}

	@Override
	public void onIncompleteInputException(String message) {
		if (_generalLispListener != null)
		{
			_generalLispListener.onBackgroundError(message);
			if (_callBothCallbacks && _backgroundResponseListener != null)
				_backgroundResponseListener.onIncompleteInputException(message);
		}
		else
		if (_backgroundResponseListener != null)
			_backgroundResponseListener.onIncompleteInputException(message);
	}

	@Override
	public void onGeneralException(Exception e) {
		if (_generalLispListener != null)
		{
			_generalLispListener.onBackgroundError(e.toString());
			if (_callBothCallbacks && _backgroundResponseListener != null)
				_backgroundResponseListener.onGeneralException(e);
		}
		else
		if (_backgroundResponseListener != null)
			_backgroundResponseListener.onGeneralException(e);
	}
}
