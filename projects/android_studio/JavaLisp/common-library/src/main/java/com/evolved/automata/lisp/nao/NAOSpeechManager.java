package com.evolved.automata.lisp.nao;

public class NAOSpeechManager {
    public static final String SPEECH_SERVICE_NAME = "ALTextToSpeech";

    NAOManager mNAOManager;

    NAOSpeechManager(NAOManager parentManager)
    {
        mNAOManager = parentManager;
    }

    static void addSpeechFunctions()
    {
        // No input arugment functions
        NAOManager.addQiFunctionSpec(SPEECH_SERVICE_NAME, "getLanguage", new TypeHelper.QiArgumentType[0], true);
        NAOManager.addQiFunctionSpec(SPEECH_SERVICE_NAME, "getAvailableVoices",  new TypeHelper.QiArgumentType[0], true);
        NAOManager.addQiFunctionSpec(SPEECH_SERVICE_NAME, "getAvailableLanguages", new TypeHelper.QiArgumentType[0], true);

        NAOManager.addQiFunctionSpec(SPEECH_SERVICE_NAME, "getVolume", new TypeHelper.QiArgumentType[0], true);
        NAOManager.addQiFunctionSpec(SPEECH_SERVICE_NAME, "setVolume", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.Float}, false);

        NAOManager.addQiFunctionSpec(SPEECH_SERVICE_NAME, "setVoice", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.String}, false);
        NAOManager.addQiFunctionSpec(SPEECH_SERVICE_NAME, "say", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.String}, false);

        NAOManager.addQiFunctionSpec(SPEECH_SERVICE_NAME, "subscribe", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.String}, false);
        NAOManager.addQiFunctionSpec(SPEECH_SERVICE_NAME, "unsubscribe", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.String}, false);

        NAOManager.addQiFunctionSpec(SPEECH_SERVICE_NAME, "setParameter", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.String, TypeHelper.QiArgumentType.Float}, false);
        NAOManager.addQiFunctionSpec(SPEECH_SERVICE_NAME, "getParameter", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.String}, true);


    }
}
