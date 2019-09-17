package com.evolved.automata.android.speech.speaking;
import android.content.*;
import android.speech.tts.*;
import android.speech.tts.TextToSpeech.OnUtteranceCompletedListener;


import java.util.concurrent.*;
import java.util.*;

public class SpeakController implements TextToSpeech.OnInitListener, TextToSpeech.OnUtteranceCompletedListener
{
	TextToSpeech speechEngine;
	private final int PLAY_CURRENT_MESSAGE=0;
	private final int STOP_CURRENT_MESSAGE=1;
	private final int MOVE_TO_NEXT_MESSAGE=2;
	private final int MOVE_TO_PREV_MESSAGE=4;
	private final int RESTART_CURRENT_MESSAGE=5;
	private final int GET_POSITION=7;
	private final int GET_TOTAL_MESSAGES=8;
	private final int QUIT=6;
	
	String[] messages=null;
	int currentPosition=0;
	int totalMessages=0;
	boolean speechReadyP=false;
	boolean finishedP=true;
	Thread controlThread;
	LinkedBlockingQueue<Integer> messageQueue;
	LinkedBlockingQueue<Integer> positionQueue;
	LinkedBlockingQueue<Integer> utteranceCompleteQueue;
	LinkedList<TextToSpeech.OnUtteranceCompletedListener> externalListeners;
	TextToSpeech.OnUtteranceCompletedListener overideUtteranceCompleteListener;
	TextToSpeech.OnInitListener externalInitListener;
	
	public SpeakController(Context context)
	{
		
		speechEngine = new TextToSpeech(context, this);
		
		messageQueue = new LinkedBlockingQueue<Integer>();
		positionQueue = new LinkedBlockingQueue<Integer>();
		utteranceCompleteQueue = new LinkedBlockingQueue<Integer>();
		Runnable messageInterpreter = getInterpreterRunnable();
		
		controlThread = new Thread(messageInterpreter);
		externalListeners = new LinkedList<TextToSpeech.OnUtteranceCompletedListener>();
	}
	
	public SpeakController(Context context, TextToSpeech.OnInitListener listener)
	{
		
		speechEngine = new TextToSpeech(context, this);
		
		messageQueue = new LinkedBlockingQueue<Integer>();
		positionQueue = new LinkedBlockingQueue<Integer>();
		utteranceCompleteQueue = new LinkedBlockingQueue<Integer>();
		Runnable messageInterpreter = getInterpreterRunnable();
		
		controlThread = new Thread(messageInterpreter);
		externalListeners = new LinkedList<TextToSpeech.OnUtteranceCompletedListener>();
		externalInitListener = listener;
	}
	
	public void setExternalInitListener(TextToSpeech.OnInitListener listener)
	{
		externalInitListener = listener;
	}
	
	private Runnable getInterpreterRunnable()
	{
		return new Runnable(){
			public void run()
			{
				int messageId=0;
				java.util.HashMap<String, String> speechParams = new java.util.HashMap<String, String>();
				speechParams.put(TextToSpeech.Engine.KEY_PARAM_UTTERANCE_ID, "general");
				try
				{
					messageId = messageQueue.take();
					while (messageId!=QUIT)
					{
						switch (messageId)
						{
							case PLAY_CURRENT_MESSAGE:
								if ((currentPosition<totalMessages)&&(speechReadyP))
								{
									if (speechEngine.isSpeaking())
										speechEngine.stop();
									updateSpeechFinishedStatus(true, false);
									speechEngine.speak(messages[currentPosition],TextToSpeech.QUEUE_FLUSH, speechParams);
									
								}
								break;
							case STOP_CURRENT_MESSAGE:
								speechEngine.stop();
								break;
							case MOVE_TO_NEXT_MESSAGE:
								if (currentPosition<totalMessages-1)
								{
									currentPosition=Math.min(totalMessages-1, currentPosition+1);
									
									messageQueue.put(PLAY_CURRENT_MESSAGE);
								}
								break;
							case MOVE_TO_PREV_MESSAGE:
								if (currentPosition>0)
								{
									currentPosition=Math.max(0, currentPosition-1);
									
									messageQueue.put(PLAY_CURRENT_MESSAGE);
								}
								break;
							case RESTART_CURRENT_MESSAGE:
								messageQueue.put(PLAY_CURRENT_MESSAGE);
								break;
							case GET_POSITION:
								positionQueue.put(currentPosition);
								break;
							case GET_TOTAL_MESSAGES:
								positionQueue.put(totalMessages);
								break;
						}
						
						messageId = messageQueue.take();
					}
				}
				catch (Exception e)
				{
					java.io.StringWriter traceText = new java.io.StringWriter();
					java.io.PrintWriter pWriter = new java.io.PrintWriter(traceText,true);
					e.printStackTrace(pWriter);
					pWriter.close();
					throw new RuntimeException(pWriter.toString());
				}
				finally
				{
					if (speechEngine.isSpeaking())
					{
						speechEngine.stop();
					}
					speechEngine.shutdown();
				}
			}
		};
	}
	
	public void waitUntilFinishSpeaking()
	{
		while (!updateSpeechFinishedStatus(false, false))
		{
			try
			{
				Thread.sleep(50);
			}
			catch (Exception e)
			{
				return;
			}
		}
	}
	
	
	public String getCurrentText()
	{
		if ((messages!=null)&&(messages.length>0)&&(0<=currentPosition)&&(totalMessages>currentPosition))
			return messages[currentPosition];
		else
			return null;
	}
	
	public String getNextText()
	{
		if ((messages!=null)&&(messages.length>0)&&(-1<currentPosition)&&(totalMessages-1>currentPosition))
			return messages[currentPosition+1];
		else
			return null;
	}
	public String getPrevText()
	{
		if ((messages!=null)&&(messages.length>0)&&(0<currentPosition)&&(totalMessages>currentPosition))
			return messages[currentPosition-1];
		else
			return null;
	}
	
	
	public void start()
	{
		if (!controlThread.isAlive())
		{
			controlThread = new Thread(getInterpreterRunnable());
			controlThread.start();
		}
		
	}
	
	public void addUtteranceEndListener(TextToSpeech.OnUtteranceCompletedListener listener)
	{
		externalListeners.add(listener);
	}
	
	public void setUtteranceEndListener(TextToSpeech.OnUtteranceCompletedListener listener)
	{
		
		externalListeners.clear();
		if (listener != null)
			externalListeners.add(listener);
	}
	
	public int getCurrentMessageIndex()
	{
		try
		{
			messageQueue.put(GET_POSITION);
			return positionQueue.take();
		}
		catch (Exception e)
		{
			
		}
		return 0;
	}
	
	public int getTotalMessages()
	{
		try
		{
			messageQueue.put(GET_TOTAL_MESSAGES);
			return positionQueue.take();
		}
		catch (Exception e)
		{
			
		}
		return 0;
	}
	
	public void updateMessages(String[] textMessages)
	{
		if ((textMessages!=null)&&(textMessages.length>0))
		{
			stopPlayingCurrentText();
			messages = textMessages;
			currentPosition=0;
			totalMessages = messages.length;
			
		}
	}
	
	public void speakSimpleMessage(String text, OnUtteranceCompletedListener listener)
	{
		overideUtteranceCompleteListener = listener;
		updateMessages(new String[]{text}, false);
		playCurrentText();
	}
	
	public void updateMessages(String[] textMessages, boolean waitUntilFinish)
	{
		if ((textMessages!=null)&&(textMessages.length>0))
		{
			if (waitUntilFinish)
				waitUntilFinishSpeaking();
			stopPlayingCurrentText();
			messages = textMessages;
			currentPosition=0;
			totalMessages = messages.length;
			
		}
	}
	
	public void playCurrentText()
	{
		
		sendMessage(PLAY_CURRENT_MESSAGE);
	}
	
	public void playNextText()
	{
		sendMessage(MOVE_TO_NEXT_MESSAGE);
	}
	
	public void playPrevText()
	{
		sendMessage(MOVE_TO_PREV_MESSAGE);
	}
	
	
	public void stopPlayingCurrentText()
	{
		sendMessage(STOP_CURRENT_MESSAGE);
	}
	
	
	
	public void shutdown()
	{
		sendMessage(QUIT);
	}

	private void sendMessage(int messageId)
	{
		try
		{
			messageQueue.put(messageId);
		}
		catch (Exception e)
		{
			
		}
	}
	
    @Override
    public void onInit(int Status)
    {
    	
        if (Status == TextToSpeech.SUCCESS)
        {
        	try
        	{
        		
        		speechEngine.setOnUtteranceCompletedListener(this);
        		speechReadyP=true;
        	}
        	catch (Exception e)
        	{
        		
        	}
        }
        if (externalInitListener!=null)
			externalInitListener.onInit(Status);
    }

	
	public void onUtteranceCompleted(String arg0) {
		
		updateSpeechFinishedStatus(true, true);
		if (overideUtteranceCompleteListener!=null)
		{
			overideUtteranceCompleteListener.onUtteranceCompleted(arg0);
			overideUtteranceCompleteListener = null;
		}
		else
		{
			for (TextToSpeech.OnUtteranceCompletedListener listener: externalListeners)
			{
				listener.onUtteranceCompleted(arg0);
			}
		}
		
	}
   
    private synchronized boolean updateSpeechFinishedStatus(boolean update, boolean speechStatus)
    {
    	if (update)
    		finishedP=speechStatus;
    	return finishedP;
    }
}
