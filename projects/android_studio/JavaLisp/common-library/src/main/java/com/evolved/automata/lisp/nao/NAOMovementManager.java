package com.evolved.automata.lisp.nao;

import com.aldebaran.qi.AnyObject;

/**
 * Created by Evolved8 on 6/20/17.
 */

public class NAOMovementManager {

    public static final String MOTION_SERVICE_NAME = "ALMotion";
    public static final String POSTURE_SERVICE_NAME = "ALRobotPosture";



    NAOManager mNAOManager;


    NAOMovementManager(NAOManager manager)
    {
        mNAOManager = manager;

    }

    static void addMovementFunctions()
    {

        // No input arugment functions
        NAOManager.addQiFunctionSpec(MOTION_SERVICE_NAME, "wakeUp", new TypeHelper.QiArgumentType[0], false);
        NAOManager.addQiFunctionSpec(MOTION_SERVICE_NAME, "rest", new TypeHelper.QiArgumentType[0], false);
        NAOManager.addQiFunctionSpec(MOTION_SERVICE_NAME, "robotIsWakeUp", new TypeHelper.QiArgumentType[0], true);
        NAOManager.addQiFunctionSpec(MOTION_SERVICE_NAME, "moveInit", new TypeHelper.QiArgumentType[0], false); // blocking
        NAOManager.addQiFunctionSpec(MOTION_SERVICE_NAME, "waitUntilMoveIsFinished", new TypeHelper.QiArgumentType[0], false); // blocking
        NAOManager.addQiFunctionSpec(MOTION_SERVICE_NAME, "moveIsActive", new TypeHelper.QiArgumentType[0], true);
        NAOManager.addQiFunctionSpec(MOTION_SERVICE_NAME, "stopMove", new TypeHelper.QiArgumentType[0], false);
        NAOManager.addQiFunctionSpec(MOTION_SERVICE_NAME, "getRobotVelocity", new TypeHelper.QiArgumentType[0], true);
        NAOManager.addQiFunctionSpec(POSTURE_SERVICE_NAME, "getPosture", new TypeHelper.QiArgumentType[0], true);
        NAOManager.addQiFunctionSpec(POSTURE_SERVICE_NAME, "getPostureList", new TypeHelper.QiArgumentType[0], true);




        // Floating point input argument functions
        NAOManager.addQiFunctionSpec(MOTION_SERVICE_NAME, "move", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.Float, TypeHelper.QiArgumentType.Float, TypeHelper.QiArgumentType.Float}, false);
        NAOManager.addQiFunctionSpec(MOTION_SERVICE_NAME, "moveTo", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.Float, TypeHelper.QiArgumentType.Float, TypeHelper.QiArgumentType.Float}, false);
        NAOManager.addQiFunctionSpec(MOTION_SERVICE_NAME, "moveToward", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.Float, TypeHelper.QiArgumentType.Float, TypeHelper.QiArgumentType.Float}, false);

        // Mixed input args

        NAOManager.addQiFunctionSpec(POSTURE_SERVICE_NAME, "goToPosture", new TypeHelper.QiArgumentType[]{TypeHelper.QiArgumentType.String, TypeHelper.QiArgumentType.Float}, true); // blocking


    }
}
