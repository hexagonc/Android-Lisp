package com.evolved.automata.android.lisp.guibuilder;

import java.util.HashSet;

import android.app.Activity;
import android.content.Context;

import com.evolved.automata.android.AppStateManager;
import com.evolved.automata.android.lisp.AndroidLispInterpreter;
import com.evolved.automata.android.lisp.views.ViewEvaluator;
import com.evolved.automata.android.lisp.views.ViewProxy;
import com.evolved.automata.android.mindstorms.NXTBluetoothManager;
import com.evolved.automata.android.mindstorms.lisp.NXTLispFunctions;
import com.evolved.automata.android.speech.SpeechInterface;
import com.evolved.automata.android.speech.SpeechInterface.SPEECH_STATUS;
import com.evolved.automata.android.speech.SpeechInterface.SpeechControlInterface;
import com.evolved.automata.android.speech.SpeechInterface.SpeechListener;
import com.evolved.automata.android.speech.SpeechInterface.TTS_STATUS;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.LambdaValue;
import com.evolved.automata.lisp.LispInterpreter;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;

public class GlobalInterface implements LispInterpreter.LispResponseListener, AndroidLispInterpreter.ResponseListener, SpeechListener
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
	boolean _ttsAvailableP = false;
	SpeechInterface _speechInterface = null;
	
	public static final String _SPEECH_LOG_LABEL = "GUI-BUILDER-SPEECH";
	
	
	private boolean _isRunningInBackgroundP = false;
	
	HashSet<String> _expectedWords = null;
	public static String _SPEECH_AVAILABLE_VAR_NAME = "SPEECH_AVAILABLE_P";
	public static String _ASR_AVAILABLE_VAR_NAME = "ASR_AVAILABLE_P";
	public static String _TTS_STATUS_VAR_NAME = "TTS_STATUS";
	public static String _ASR_STATUS_VAR_NAME = "ASR_STATUS";
	
	public static final String _TTS_STATUS_STARTED_STATUS_VALUE = "TTS_STARTED";
	public static final String _TTS_STATUS_COMPLETE_VALUE = "TTS_COMPLETE";
	public static final String _TTS_STATUS_INTERRUPTED_VALUE = "TTS_INTERRUPTED";
	
	public static final String _ASR_STATUS_STARTED = "ASR_STARTED";
	public static final String _ASR_STATUS_RECOGNITION_ERROR = "RECOGNITION_ERROR";
	public static final String _ASR_STATUS_RECOGNITION_COMPLETE = "RECOGNITION_COMPLETE";
	
	
	FunctionTemplate _asrListenerLambda = null;
	FunctionTemplate _ttsListenerLambda = null;
	
	boolean _asrAvailableP = false;
	SpeechControlInterface _speechControlInterface = null;
	
	
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
		_expectedWords = new HashSet<String>();
		setupSpeechInterface();
	}
	
	public boolean isRunningInBackground()
	{
		return _isRunningInBackgroundP;
	}
	
	public void enableRunningInBackground(boolean enable)
	{
		_isRunningInBackgroundP = enable;
	}
	private void setupSpeechInterface()
	{
		_speechInterface = new SpeechInterface(_context, _SPEECH_LOG_LABEL, new android.speech.tts.TextToSpeech.OnInitListener() {
			
			@Override
			public void onInit(int status) {
				_ttsAvailableP = status == android.speech.tts.TextToSpeech.SUCCESS;
				setupTTS();
			}
		},_expectedWords );
		
		_env.mapValue(_ASR_STATUS_VAR_NAME, Environment.getNull());
		_env.mapValue(_TTS_STATUS_VAR_NAME, Environment.getNull());
		try
		{
			_speechInterface.startup();
			_speechControlInterface = _speechInterface.setSpeechListener(this);
			_asrAvailableP = true;
			_env.mapFunction("register-asr-listener", register_asr_listener());
			_env.mapFunction("start-speech-recognition", start_speech_recognition());
			
		}
		catch (Exception e)
		{
			_asrAvailableP = false;
			
		}
		_env.mapValue(_ASR_AVAILABLE_VAR_NAME, NLispTools.makeValue(_asrAvailableP));
	}
	
	
	private SimpleFunctionTemplate register_tts_listener()
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)register_tts_listener();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				_ttsListenerLambda = (FunctionTemplate)evaluatedArgs[0].getLambda(); 
				return evaluatedArgs[0];
			}
			
		};
	}
	
	private SimpleFunctionTemplate register_asr_listener()
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)register_asr_listener();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				_asrListenerLambda = (FunctionTemplate)evaluatedArgs[0].getLambda(); 
				return evaluatedArgs[0];
			}
			
		};
	}
	
	private SimpleFunctionTemplate start_speech_recognition()
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)start_speech_recognition();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				_speechControlInterface.initiateListening(); 
				return NLispTools.makeValue(asrRequested());
			}
			
		};
	}
	
	private SimpleFunctionTemplate tts()
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)tts();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] evaluatedArgs) {
				String text = evaluatedArgs[0].getString();
				_speechControlInterface.speakMessage(text);
				ttsRequested();
				return evaluatedArgs[0];
			}
			
		};
	}
	
	private void setupTTS()
	{
		_env.mapValue(_SPEECH_AVAILABLE_VAR_NAME, NLispTools.makeValue(_ttsAvailableP));
		if (_ttsAvailableP)
		{
			_env.mapFunction("register-tts-listener", register_tts_listener());
			_env.mapFunction("tts", tts());
		}
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
	
	
	
	private String ttsRequested()
	{
		_env.mapValue(_TTS_STATUS_VAR_NAME, NLispTools.makeValue(_TTS_STATUS_STARTED_STATUS_VALUE));
		return _TTS_STATUS_STARTED_STATUS_VALUE;
	}
	
	private void ttsComplete(TTS_STATUS  status)
	{
		_env.mapValue(_TTS_STATUS_VAR_NAME, NLispTools.makeValue(status.toString()));
	}
	
	private String asrRequested()
	{
		_env.mapValue(_ASR_STATUS_VAR_NAME, NLispTools.makeValue(_ASR_STATUS_STARTED));
		return _ASR_STATUS_STARTED;
		
	}
	
	private void asrCompleted(SPEECH_STATUS status)
	{
		_env.mapValue(_ASR_STATUS_VAR_NAME, NLispTools.makeValue(status.toString()));
	}
	
	
	
	public Environment getEnvironment()
	{
		return _env;
	}
	
	public void stopBackgroundLispInterpreter()
	{
		_backgroundInterpreter.stop(false, 0);
	}
	
	public void shutdownSpeechInterface()
	{
		_speechInterface.shutdown();
	}
	
	public void shutdownAll()
	{
		_backgroundInterpreter.stop(false, 0);
		_speechInterface.shutdown();
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

	@Override
	public void onTTSComplete(TTS_STATUS status) {
		ttsComplete(status);
		if (_ttsListenerLambda != null)
		{
			Value[] args =  new Value[]{NLispTools.makeValue(status.toString())};
			
			FunctionTemplate actual=(FunctionTemplate)_ttsListenerLambda.clone();
			actual.setActualParameters(args);
			_interpreter.evaluateFunction(actual);
		}
		
	}

	@Override
	public void onASRComplete(SPEECH_STATUS status, String speech, int errorCode) {
		asrCompleted(status);
		if (_asrListenerLambda != null)
		{
			Value[] args = new Value[]{(speech == null)?Environment.getNull():NLispTools.makeValue(speech), NLispTools.makeValue(status.toString()), (speech == null)?NLispTools.makeValue(SpeechInterface.mapSpeechErrorToResponse(errorCode)):Environment.getNull()};
			
			FunctionTemplate actual= (FunctionTemplate)_asrListenerLambda.clone();
			actual.setActualParameters(args);
			
			_interpreter.evaluateFunction(actual);
		}
	}

	@Override
	public void onInit() {
		
	}

	@Override
	public void log(String tag, String message) {
		AppStateManager.getInstance().simpleMessage(tag, message);
	}
}