package com.evolved.automata.android.lisp.guibuilder;

import com.evolved.automata.android.speech.SpeechInterface;

/**
 * Created by Evolved8 on 5/5/17.
 */

public interface LispSpeechInterface {


    interface ListeningStateListener {
        void onASRComplete(SpeechInterface.SPEECH_STATUS status, String speech, int errorCode);
    }

    interface SpeechStateListener {
        void onTTSComplete(SpeechInterface.TTS_STATUS status);
    }

    void startListening(ListeningStateListener stateListener);

    void startSpeaking(String text, SpeechStateListener stateListener);



}
