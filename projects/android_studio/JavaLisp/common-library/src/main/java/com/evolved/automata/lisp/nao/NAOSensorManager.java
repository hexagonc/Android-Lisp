package com.evolved.automata.lisp.nao;

/**
 * Created by Evolved8 on 6/20/17.
 */

public class NAOSensorManager {

    public static final String MEMORY_SERVICE_NAME = "ALMemory";
    public static final String BATTERY_SERVICE_NAME = "ALBattery";
    public static final String TOUCH_SERVICE_NAME = "ALTouch";
    public static final String SONAR_SERVICE_NAME = "ALSonar";



    NAOManager mNAOManager;

    NAOSensorManager(NAOManager parentManager)
    {
        mNAOManager = parentManager;
    }

    static void addMemoryFunctions()
    {
        // No input arugment functions
        NAOManager.addQiFunctionSpec(MEMORY_SERVICE_NAME, "getData", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.String}, true);
        NAOManager.addQiFunctionSpec(BATTERY_SERVICE_NAME, "enablePowerMonitoring", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.Boolean}, false);
        NAOManager.addQiFunctionSpec(BATTERY_SERVICE_NAME, "getBatteryCharge", new TypeHelper.QiArgumentType[0], true);


        NAOManager.addQiFunctionSpec(TOUCH_SERVICE_NAME, "getSensorList", new TypeHelper.QiArgumentType[0], true);
        NAOManager.addQiFunctionSpec(TOUCH_SERVICE_NAME, "getStatus", new TypeHelper.QiArgumentType[0], true);

        NAOManager.addQiFunctionSpec(SONAR_SERVICE_NAME, "subscribe", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.String}, false);
        NAOManager.addQiFunctionSpec(SONAR_SERVICE_NAME, "unsubscribe", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.String}, false);
        NAOManager.addQiFunctionSpec(SONAR_SERVICE_NAME, "updatePeriod", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.String, TypeHelper.QiArgumentType.Integer}, false);
        NAOManager.addQiFunctionSpec(SONAR_SERVICE_NAME, "updatePrecision", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.String, TypeHelper.QiArgumentType.Float}, false);
        NAOManager.addQiFunctionSpec(SONAR_SERVICE_NAME, "getCurrentPeriod", new TypeHelper.QiArgumentType[0], true);
        NAOManager.addQiFunctionSpec(SONAR_SERVICE_NAME, "getCurrentPrecision", new TypeHelper.QiArgumentType[0], true);
        NAOManager.addQiFunctionSpec(SONAR_SERVICE_NAME, "getMemoryKeyList", new TypeHelper.QiArgumentType[0], true);
        NAOManager.addQiFunctionSpec(SONAR_SERVICE_NAME, "getOutputNames", new TypeHelper.QiArgumentType[0], true);


    }

}
