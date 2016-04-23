package com.evolved.automata.android.lisp.guibuilder;



import com.evolved.automata.android.speech.SpeechInterface;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentSender.SendIntentException;
import android.util.Log;
import android.view.KeyEvent;

public class BluetoothButtonEventReceiver extends BroadcastReceiver 
{

	@Override
	public void onReceive(Context context, Intent intent) {
		if (intent!=null)
		{
			KeyEvent event = intent.getParcelableExtra(Intent.EXTRA_KEY_EVENT);
			if (event.getAction() == KeyEvent.ACTION_DOWN && event.getKeyCode() == KeyEvent.KEYCODE_MEDIA_PREVIOUS)
			{
				Intent listenIntent = SpeechInterface.getInitiateListeningSpeechIntent();
				context.sendBroadcast(listenIntent);
			}
			Log.d("Media button Event", "Event is: " + intent.toString());
		}
		
		
	}

}
