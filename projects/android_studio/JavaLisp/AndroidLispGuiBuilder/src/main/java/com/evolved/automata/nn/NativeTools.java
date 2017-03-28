package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 3/26/17.
 */
public class NativeTools {

    static {
        System.loadLibrary("neural_tools");

    }

    public static native void setSeed(long seed);
    public static native void setRNDAlgorithm(int alg);
    public static native double randomLCG();

    public static native void setDebugStatus(boolean status);


    // standard low level interface
    public static native void initializeAllWeights(float[] networkSpec);
    public static native void resetNetworkToInitialState(float[] networkSpec);

    public static native void forwardPass(float[] networkSpec, float[] inputActivation);
    public static native void setInputActivation(float[] networkSpec, float[] data);
    public static native float updateForwardPassErrors(float[] networkSpec, float[] targetOutput);
    public static native void updateWeightsFromErrors(float[] networkSpec,  int updateType);
    public static native void getOutputActivation(float[] networkSpec, float[] outputBuffer);

    // higher level
    public static native float learnInputOutputPairMap(int numTrainingExamples, float[] networkSpec, float[] trainingSpec, int itemIndex, float[] specBuffer);
    public static native void learnInputOutputPairMapWithDetails(int numTrainingExamples, float[] networkSpec, float[] trainingSpec, int itemIndex, float[] specBuffer, float[] resultBuffer);
    public static native int learnTrainingSpec(int numTrainingExamples, float[] networkSpec, float[] trainingSpec, int maxSteps, float convergenceThreshold, boolean allowWeightResetsP, int updateType);

    public static native int createCappedLSTM(float[] networkSpec, float[] startInput, float[] stopInput);
    public static native int appendVectorToSequence(float[] cappedNetworkSpec, float[] startInput, float[] stopInput, float[] newInput, int maxSteps, float convergenceThreshold, boolean allowWeightResetsP, int updateType);
    public static native void resetCappedLSTM(float[] networkSpec, float[] initialState, boolean onlyMetaDataP);


}
