package com.evolved.automata.android.speech;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.speech.RecognitionListener;
import android.speech.RecognizerIntent;
import android.speech.SpeechRecognizer;
import android.speech.tts.TextToSpeech;
import android.speech.tts.TextToSpeech.OnInitListener;
import android.speech.tts.TextToSpeech.OnUtteranceCompletedListener;
import com.evolved.automata.AITools;
import com.evolved.automata.WeightedValue;

import com.evolved.automata.android.speech.speaking.SpeakController;

public class SpeechInterface implements RecognitionListener, OnInitListener, OnUtteranceCompletedListener
{
	public enum TTS_STATUS
	{
		COMPLETE,
		SPEECH_INTERRUPTED,
		SPEECH_NOT_READY
	}
	
	public enum SPEECH_STATUS
	{
		RECOGNITION_ERROR,
		RECOGNITION_COMPLETE
	}
	
	public class SpeechControlReceiver extends BroadcastReceiver
	{
		
		public void onReceive(Context arg0, Intent intent) {
			if (intent!=null)
			{
				int code = intent.getIntExtra(SPEECH_CONTROL_LISTENER_CONTROL_PARAMETER_KEY, -1);
				switch (code)
				{
					case SPEECH_CONTROL_LISTENER_INITIATE_LISTENING:
						startRecognition();
						break;
					case SPEECH_CONTROL_LISTENER_CANCEL_LISTENING:
						break;
					default:
				}
			}
			
			
		}
	}
	
	public interface SpeechListener
	{
		public void onTTSComplete(TTS_STATUS status);
		public void onASRComplete(SPEECH_STATUS status, String speech, int errorCode);
		public void onInit(int ttsInitStatus, boolean asrAvailable);
		public void log(String tag, String message);
	}
	
	public interface SpeechControlInterface
	{
		public void speakMessage(String text);
		public void initiateListening();
	}
	
	public static final String SPEECH_CONTROL_LISTENER_ACTION = SpeechControlReceiver.class.getName();
	public static final int SPEECH_CONTROL_LISTENER_INITIATE_LISTENING = 0;
	public static final int SPEECH_CONTROL_LISTENER_CANCEL_LISTENING = 1;
	public static final String SPEECH_CONTROL_LISTENER_CONTROL_PARAMETER_KEY = "SPEECH_CONTROL_LISTENER_CONTROL_PARAMETER_KEY";
	
	
	
	SpeechControlReceiver _controlReceiver = null;
	SpeechListener _speechListener = null;
	SpeakController _controller;
	
	boolean _speechReady = false;
	SpeechRecognizer _recognizer;
	OnInitListener _initListener;
	boolean _speakRequested = false;
	
	Object _interfaceSynch = new Object();
	
	boolean _listeningP = false;
	
	private final int MAX_SPEECH_RESULTS = 5;
	private final int MAX_SPEECH_DELAY_INTERVAL = 3000;
	
	String _speechStartPrompt = "listening";
	String _speechEndPrompt = "okay";
	Handler _mainHandler = new Handler(Looper.getMainLooper());
	
	public final String SPEECH_STATUS_KEY = "SPEECH_STATUS_KEY";
	public final String SPEECH_FULL_RESULT_KEY = "SPEECH_FULL_RESULT_KEY";
	public final String SPEECH_PARTIAL_RESULT_KEY = "SPEECH_PARTIAL_RESULT_KEY";
	public final String SPEECH_RECOGNITION_COMPLETE = "SPEECH_RECOGNITION_COMPLETE";
	HashSet<String> _dictionary = null;
	
	Context _context;
	String _speechLabel = "Speech";
	ConnectivityManager _connManager = null;
	boolean _networkSpeechAvailableP = true;
	boolean _asrAvailableP = false;
	
	public SpeechInterface(Context context, String speechLabel, OnInitListener initListener, HashSet<String> dictionary)
	{
		_initListener = initListener;
		_context = context;
		_speechLabel = speechLabel;
		_dictionary = dictionary;
		_connManager = (ConnectivityManager)context.getSystemService(Context.CONNECTIVITY_SERVICE);
		_asrAvailableP = SpeechRecognizer.isRecognitionAvailable(_context);
	}
	
	public SpeechControlInterface setSpeechListener(SpeechListener listener)
	{
		_speechListener = listener;
		return new SpeechControlInterface()
		{

			@Override
			public void speakMessage(String text) {
				SpeechInterface.this.speakMessage(text);
			}

			@Override
			public void initiateListening() 
			{
				startRecognition();
			}
			
		};
	}
	
	private Intent getSpeechIntent()
	{
		Intent speechIntent = new Intent(RecognizerIntent.ACTION_RECOGNIZE_SPEECH);
		speechIntent.putExtra(RecognizerIntent.EXTRA_LANGUAGE_MODEL, RecognizerIntent.LANGUAGE_MODEL_FREE_FORM);
		speechIntent.putExtra(RecognizerIntent.EXTRA_CALLING_PACKAGE, _context.getPackageName());
		speechIntent.putExtra(RecognizerIntent.EXTRA_MAX_RESULTS, MAX_SPEECH_RESULTS);
		speechIntent.putExtra(RecognizerIntent.EXTRA_LANGUAGE, Locale.ENGLISH);
		speechIntent.putExtra(RecognizerIntent.EXTRA_PARTIAL_RESULTS, true);
		speechIntent.putExtra(RecognizerIntent.EXTRA_SPEECH_INPUT_COMPLETE_SILENCE_LENGTH_MILLIS , MAX_SPEECH_DELAY_INTERVAL);
		  
		return speechIntent;
	}
	
	private void startRecognition()
	{
		NetworkInfo ninfo = _connManager.getActiveNetworkInfo();
		_networkSpeechAvailableP = ninfo!=null && ninfo.isAvailable();
		synchronized (_interfaceSynch)
		{
			if (_speakRequested)
			{
				_speakRequested =  false;
				notifyTTSStatus(TTS_STATUS.SPEECH_INTERRUPTED);
				
				_controller.stopPlayingCurrentText();
			}
			
			final Runnable listen = new Runnable()
			{
				public void run()
				{
					_readyForSpeechInitiated = false;
					_recognizer.startListening(getSpeechIntent());
					synchronized (_interfaceSynch)
					{
						_listeningP = true;
					}
					
				}
			};
			
			if (_speechReady && _speechStartPrompt!=null && _speechStartPrompt.trim().length()>0)
			{
				_controller.speakSimpleMessage(_speechStartPrompt, new TextToSpeech.OnUtteranceCompletedListener() {
					
					@Override
					public void onUtteranceCompleted(String utteranceId) {
						_mainHandler.post(listen);
					}
				});
			}
			else
			{
				_mainHandler.post(listen);
			}
			
			
		}
	}
	
	private void logMessage(String tag, String message, boolean flag)
	{
		if (_speechListener != null)
			_speechListener.log(tag, message);
	}
	
	private void logMessage(String tag, String message)
	{
		if (_speechListener != null)
			_speechListener.log(tag, message);
	}
	
	private boolean speakMessage(String message)
	{
		logMessage(_speechLabel, "Received speak message", true);
		synchronized (_interfaceSynch)
		{
			if (_listeningP)
			{
				notifyTTSStatus(TTS_STATUS.SPEECH_INTERRUPTED);
				return false;
			}
			else
			{
				if (!_speechReady)
				{
					notifyTTSStatus(TTS_STATUS.SPEECH_NOT_READY);
					return false;
				}
			}
			_speakRequested=true;
			_controller.updateMessages(new String[]{message}, false);
			_controller.playCurrentText();
			return true;
		}
		
	}
	
	
	private void notifyTTSStatus(TTS_STATUS status)
	{
		if (_speechListener!=null)
			_speechListener.onTTSComplete(status);
			
	}
	
	private void notifySpeechStatus(SPEECH_STATUS status, String fullResults)
	{
		if (_speechListener!=null)
		{
			_speechListener.onASRComplete(status, fullResults, 0);
		}
	}
	
	private void notifySpeechStatus(SPEECH_STATUS status)
	{
		notifySpeechStatus(status, null);
	}
	
	private void notifySpeechStatus(SPEECH_STATUS status, int errorStatus)
	{
		if (_speechListener!=null)
		{
			String error = mapSpeechErrorToString(errorStatus);
			_speechListener.log("speech", error);
			_speechListener.onASRComplete(status, null, errorStatus);
		}
	}
	
	public static Intent getInitiateListeningSpeechIntent()
	{
		Intent intent = new Intent(SPEECH_CONTROL_LISTENER_ACTION);
		intent.putExtra(SPEECH_CONTROL_LISTENER_CONTROL_PARAMETER_KEY, SPEECH_CONTROL_LISTENER_INITIATE_LISTENING);
		return intent;
	}
	
	public void startup()
	{
		_controller = new SpeakController(_context.getApplicationContext());
		_controller.setExternalInitListener(this);
		_controller.setUtteranceEndListener(this);
		_controller.start();
		
		if (SpeechRecognizer.isRecognitionAvailable(_context))
		{
			_recognizer =  SpeechRecognizer.createSpeechRecognizer(_context.getApplicationContext());
			_recognizer.setRecognitionListener(this);
			_controlReceiver = new SpeechControlReceiver();
			
			_context.registerReceiver(_controlReceiver, new IntentFilter(SPEECH_CONTROL_LISTENER_ACTION));
		}
		
	}
	
	public void shutdown()
	{
		if (_recognizer!=null)
			_recognizer.destroy();
		if (_controller != null)
		{
			synchronized (_interfaceSynch)
			{
				_controller.stopPlayingCurrentText();
				if (_speakRequested)
				{
					notifyTTSStatus(TTS_STATUS.SPEECH_INTERRUPTED);
					
				}
				_speakRequested =  false;
				_listeningP = false;
				_speechReady = false;
			}
			_controller.shutdown();
			_controller = null;
		}
			
		_recognizer = null;
		if (_controlReceiver!=null)
		{
			try
			{
				_context.unregisterReceiver(_controlReceiver);
			}
			catch (IllegalStateException ie) // If not already registered
			{
				
			}
			
		}
	}

	@Override
	public void onBeginningOfSpeech() {
		logMessage(":: Speech", "onBeginningOfSpeech", true);
	}
	@Override
	public void onBufferReceived(byte[] arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void onEndOfSpeech() {
		logMessage(":: Speech", "onEndOfSpeech", true);
	}

	@Override
	public void onError(int error) {
		
		if (error == SpeechRecognizer.ERROR_NO_MATCH && !_readyForSpeechInitiated)
		{
			logMessage(":: Speech", "onError: ERROR_NO_MATCH probably spurious", true);
			
			return;
		}
		notifySpeechStatus(SPEECH_STATUS.RECOGNITION_ERROR, error);
		synchronized (_interfaceSynch)
		{
			_listeningP = false;
			
		}
	}

	@Override
	public void onEvent(int eventType, Bundle params) {
		logMessage(":: Speech", "onEvent", true);
	}

	@Override
	public void onPartialResults(Bundle partialResults) {
		ArrayList<String> resultList = partialResults.getStringArrayList(SpeechRecognizer.RESULTS_RECOGNITION);
		
		Object unstable = partialResults.get("android.speech.extra.UNSTABLE_TEXT");
		logMessage(":: Speech", resultList.toString() + " remaining: " + unstable + " ", true);
		
	}

	boolean _readyForSpeechInitiated = false;
	@Override
	public void onReadyForSpeech(Bundle params) {
		logMessage(":: Speech", "ready for speecg", true);
		_readyForSpeechInitiated = true;
	}

	@Override
	public void onResults(final Bundle results) {
		logMessage(":: Speech", "Speech Recognizer onResults: Queueing end prompt speech: [" + _speechEndPrompt + "]", true);
		if (_speechReady)
		{
			_controller.speakSimpleMessage(_speechEndPrompt, new OnUtteranceCompletedListener() {
				
				@Override
				public void onUtteranceCompleted(String utteranceId) 
				{
					synchronized (_interfaceSynch)
					{
						_listeningP = false;
						processSpeechResults(results);
					}
					
				}
			});
		}
		else
		{
			synchronized (_interfaceSynch)
			{
				_listeningP = false;
				processSpeechResults(results);
			}
		}
		
	}
	
	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//    Start of Result Processing Helper Functions 
	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	private void processSpeechResults(Bundle results)
	{
		logMessage("speech interface", "Speech Recognizer onResults: Received results.  results: " + results.getStringArrayList(SpeechRecognizer.RESULTS_RECOGNITION));
		String interpretation = null;
		try
		{
			if (_dictionary!=null)
				interpretation = advancedProcessSpeechResultsWithConfidence(results,_dictionary);
			else
				interpretation = simpleProcessSpeechResultsWithConfidence(results);
		}
		catch (Exception e)
		{
			logMessage("jane mind:::", e.toString(), true);
			
		}
		
		if (interpretation!=null)
		{
			notifySpeechStatus(SPEECH_STATUS.RECOGNITION_COMPLETE, interpretation);
		}
		else
			notifySpeechStatus(SPEECH_STATUS.RECOGNITION_ERROR);
		
	}
	
	private String simpleProcessSpeechResultsWithConfidence(Bundle results)
	{
		ArrayList<String> resultList = results.getStringArrayList(SpeechRecognizer.RESULTS_RECOGNITION);
		if (results.containsKey(SpeechRecognizer.CONFIDENCE_SCORES))
		{
			float[] confidence = results.getFloatArray(SpeechRecognizer.CONFIDENCE_SCORES);
			LinkedList<List<WeightedValue<String>>> components = convertResultList(resultList, confidence);
			return selectWeightedInterpretation(components);
		}
		else
			return simpleProcessSpeechResults(results);
		
	}
	
	private String simpleProcessSpeechResults(Bundle results)
	{
		
		ArrayList<String> resultList = results.getStringArrayList(SpeechRecognizer.RESULTS_RECOGNITION);
		LinkedList<String[]> components = convertResultList(resultList);
		
		return selectInterpretation(components);
	}
	
	private String advancedProcessSpeechResults(Bundle results, HashSet<String> dictionary)
	{
		ArrayList<String> resultList = results.getStringArrayList(SpeechRecognizer.RESULTS_RECOGNITION);
		LinkedList<String[]> components = convertResultList(resultList), filtered = new LinkedList<String[]>();
		LinkedList<String> newSet;
		for (String[] pos:components)
		{
			newSet = new LinkedList<String>();
			for (String word:pos)
			{
				if (dictionary.contains(word))
				{
					newSet.add(word);
				}
					
			}
			if (newSet.size()==0)
				filtered.add(pos);
			else
				filtered.add(newSet.toArray(new String[0]));
		}
		return selectInterpretation(filtered);
	}
	
	private String advancedProcessSpeechResultsWithConfidence(Bundle results, HashSet<String> dictionary)
	{
		ArrayList<String> resultList = results.getStringArrayList(SpeechRecognizer.RESULTS_RECOGNITION);
		if (results.containsKey(SpeechRecognizer.CONFIDENCE_SCORES))
		{
			float[] confidence = results.getFloatArray(SpeechRecognizer.CONFIDENCE_SCORES);
			LinkedList<List<WeightedValue<String>>> components = convertResultList(resultList, confidence), filtered = new LinkedList<List<WeightedValue<String>>>();
			List<WeightedValue<String>> newSet;
			double maxWeight = 0.0, weightThreshold = 0.2;
			if (!_networkSpeechAvailableP) // embedded recognition sometimes returns confidence scores of 0.0 even with good results
				weightThreshold = 0.0;
			String greatestWord;
			for (List<WeightedValue<String>> pos:components)
			{
				newSet = new LinkedList<WeightedValue<String>>();
				maxWeight = 0;
				for (WeightedValue<String> word:pos)
				{
					if (word.GetWeight()>maxWeight)
					{
						greatestWord = word.GetValue();
						maxWeight = word.GetWeight();
					}
					if (word.GetWeight()>weightThreshold && dictionary.contains(word.GetValue()))
					{
						newSet.add(word);
					}
						
				}
				if (maxWeight>=weightThreshold)
				{
					if (newSet.size()==0)
						filtered.add(pos);
					else
						filtered.add(newSet);
				}
				
			}
			
			return selectWeightedInterpretation(filtered);
		}
		else
			return advancedProcessSpeechResults(results, dictionary);
	}
	
	LinkedList<String[]> convertResultList(ArrayList<String> resultList)
	{
		String[] component;
		LinkedList<LinkedList<String>> subComponent = new LinkedList<LinkedList<String>>();
		LinkedList<String[]> out = new LinkedList<String[]>();
		
		LinkedList<String> inner;
		int startIndex;
		for (String possibility:resultList)
		{
			component = possibility.split(" ");
			startIndex = 0;
			for (LinkedList<String> part:subComponent)
			{
				part.add(component[startIndex]);
				startIndex++;
			}
			
			
			for (int i=startIndex;i<component.length;i++)
			{
				inner = new  LinkedList<String>();
				subComponent.add(inner);
				inner.add(component[i]);
			}
		}
		
		for (LinkedList<String> part:subComponent)
		{
			component = part.toArray(new String[0]);
			out.add(component);
		}
		return out;
	}
	
	
	LinkedList<List<WeightedValue<String>>> convertResultList(ArrayList<String> resultList, float[] confidence)
	{
		String[] component;
		LinkedList<List<WeightedValue<String>>> out = new LinkedList<List<WeightedValue<String>>>();
		
		List<WeightedValue<String>> inner;
		int startIndex;
		int confIndex = 0;
		for (String possibility:resultList)
		{
			component = possibility.split(" ");
			startIndex = 0;
			for (List<WeightedValue<String>> part:out)
			{
				if (startIndex==component.length)
					break;
				part.add(new WeightedValue<String>(component[startIndex], confidence[confIndex]));
				startIndex++;
			}
			
			
			for (int i=startIndex;i<component.length;i++)
			{
				inner = new LinkedList<WeightedValue<String>>();
				out.add(inner);
				inner.add(new WeightedValue<String>(component[i], confidence[confIndex]));
			}
			confIndex++;
		}
		
		return out;
	}
	
	private String selectWeightedInterpretation(LinkedList<List<WeightedValue<String>>> possibilities)
	{
		StringBuilder sBuilder = new StringBuilder();
		WeightedValue<String> selected;
		for (List<WeightedValue<String>> words:possibilities)
		{
			if (words.size()==1)
				selected = words.get(0);
			else
				selected = AITools.ChooseWeightedRandomFair(words);
			if (selected!=null)
			{
				sBuilder.append(selected.GetValue());
				sBuilder.append(" ");
			}
			
		}
		return sBuilder.toString().trim();
	}
	
	private String selectInterpretation(LinkedList<String[]> possibilities)
	{
		StringBuilder sBuilder = new StringBuilder();
		for (String[] words:possibilities)
		{
			sBuilder.append(AITools.ChooseRandom(words));
			sBuilder.append(" ");
		}
		return sBuilder.toString().trim();
	}
	
	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//    End of Result Processing Helper Functions 
	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	
	@Override
	public void onRmsChanged(float rmsdB) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void onInit(int status) 
	{
		_speechReady = status == TextToSpeech.SUCCESS;
		if (_initListener!=null)
			_initListener.onInit(status);
		if (_speechListener!=null)
		{
			_speechListener.onInit(status, _asrAvailableP);
		}
	}

	@Override
	public void onUtteranceCompleted(String arg0) {
		synchronized (_interfaceSynch)
		{
			_speakRequested =  false;
			notifyTTSStatus(TTS_STATUS.COMPLETE);
		}
	}
	
	public static String mapSpeechErrorToString(int errorCode)
	{
		switch (errorCode)
		{
			case SpeechRecognizer.ERROR_AUDIO:
				return "ERROR_AUDIO";
			case SpeechRecognizer.ERROR_CLIENT:
				return "ERROR_CLIENT";
			case SpeechRecognizer.ERROR_INSUFFICIENT_PERMISSIONS:
				return "ERROR_INSUFFICIENT_PERMISSIONS";
			case SpeechRecognizer.ERROR_NETWORK:
				return "ERROR_NETWORK";
			case SpeechRecognizer.ERROR_NETWORK_TIMEOUT:
				return "ERROR_NETWORK_TIMEOUT";
			case SpeechRecognizer.ERROR_NO_MATCH:
				return "ERROR_NO_MATCH";
			case SpeechRecognizer.ERROR_RECOGNIZER_BUSY:
				return "ERROR_RECOGNIZER_BUSY";
			case SpeechRecognizer.ERROR_SERVER:
				return "ERROR_SERVER";
			case SpeechRecognizer.ERROR_SPEECH_TIMEOUT:
				return "ERROR_SPEECH_TIMEOUT";
			default:
					return "Unknown speech error";
			
		}
	}
	
	public static String mapSpeechErrorToResponse(int errorCode)
	{
		switch (errorCode)
		{
			case SpeechRecognizer.ERROR_AUDIO:
				return "There was an audio error";
			case SpeechRecognizer.ERROR_CLIENT:
				return "There was an error with the client";
			case SpeechRecognizer.ERROR_INSUFFICIENT_PERMISSIONS:
				return "You have insufficient permissions to do recognition.";
			case SpeechRecognizer.ERROR_NETWORK:
				return "There was a network error";
			case SpeechRecognizer.ERROR_NETWORK_TIMEOUT:
				return "There was a network timeout";
			case SpeechRecognizer.ERROR_NO_MATCH:
				return "I didn't understand anything that you said";
			case SpeechRecognizer.ERROR_RECOGNIZER_BUSY:
				return "I'm busy";
			case SpeechRecognizer.ERROR_SERVER:
				return "There was a server error";
			case SpeechRecognizer.ERROR_SPEECH_TIMEOUT:
				return "I didn't hear any thing.  Say cancel if you want to quit";
			default:
					return "Unknown speech error";
			
		}
	}
	
	/*
	private class LispEvaluator extends FlexStaticEvaluator
	{
		public LispEvaluator()
		{
			super();
		}
		
		@Override
		public CompiledEvaluator clone()
		{
			CompiledEvaluator out = new LispEvaluator();
			out.setArgs(sargs);
			return out;
		}
		
		@Override
		protected void populateFunctions() {
			map("start-speech",
					new InternalEvaluator()
					{
						@Override
						public Argument eval(Argument[] args)
						{
							args = getEvaluatedArguments(args);
							if (invalidArgs(args, 1, false) || Environment.isNull(args[0]))
								return null;
							
							String speech = Environment.getStringArgResult(args[0]);
							_app.logMessage(":: Speech", "Lisp Evaluator: Start speech.  Requested speech [" + speech + "]", true);
							
							if (speakMessage(speech))
								return Environment.makeAtom(speech);
							else
								return null;
						}

						@Override
						public void resetState() {
							// TODO Auto-generated method stub
							
						}

						
					}
			);
			
			map("start-listening",
					new InternalEvaluator()
					{
						@Override
						public Argument eval(Argument[] args)
						{
						
							_app.logMessage(":: Speech", "Lisp Evaluator: Initiated listening by lisp", true);
							
							startRecognition();
							return null;
						}

						@Override
						public void resetState() {
							// TODO Auto-generated method stub
							
						}

						
					}
			);
		}

	}
	*/
}
