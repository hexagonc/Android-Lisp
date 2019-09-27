package com.evolved.automata.lisp.nao;

/**
 * Created by Evolved8 on 7/9/17.
 */

public class NAOVisionManager {

    public static final String VIDEO_DEVICE_SERVICE_NAME = "ALVideoDevice";

    static void addVisionFunctions()
    {

        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "subscribeCamera", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.String, TypeHelper.QiArgumentType.Integer, TypeHelper.QiArgumentType.Integer, TypeHelper.QiArgumentType.Integer,TypeHelper.QiArgumentType.Integer}, true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "unsubscribe", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.String}, true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "getSubscribers", new TypeHelper.QiArgumentType[0], true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "getCameraIndexes", new TypeHelper.QiArgumentType[0], true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "getActiveCamera", new TypeHelper.QiArgumentType[0], true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "setActiveCamera", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.Integer}, true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "getFrameRate", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.Integer}, true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "getResolution", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.Integer}, true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "getColorSpace", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.Integer}, true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "getParameter", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.Integer, TypeHelper.QiArgumentType.Integer}, true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "getParameterRange", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.Integer, TypeHelper.QiArgumentType.Integer}, true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "setParameter", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.Integer, TypeHelper.QiArgumentType.Integer, TypeHelper.QiArgumentType.Integer}, true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "openCamera", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.Integer}, true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "closeCamera", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.Integer}, true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "isCameraOpen", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.Integer}, true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "startCamera", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.Integer}, true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "stopCamera", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.Integer}, true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "isCameraStarted", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.Integer}, true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "getDirectRawImageRemote", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.String}, true);
        NAOManager.addQiFunctionSpec(VIDEO_DEVICE_SERVICE_NAME, "getImageRemote", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.String}, true);


    }

}
