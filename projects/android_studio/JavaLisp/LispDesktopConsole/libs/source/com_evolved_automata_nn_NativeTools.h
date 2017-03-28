/* DO NOT EDIT THIS FILE - it is machine generated */
#include <jni.h>
/* Header for class com_evolved_automata_nn_NativeTools */

#ifndef _Included_com_evolved_automata_nn_NativeTools
#define _Included_com_evolved_automata_nn_NativeTools
#ifdef __cplusplus
extern "C" {
#endif


/*
 * Class:     com_evolved_automata_nn_NativeTools
 * Method:    setDebugStatus
 * Signature: (Z)V
 */
JNIEXPORT void JNICALL Java_com_evolved_automata_nn_NativeTools_setDebugStatus
  (JNIEnv *, jclass, jboolean);

/*
 * Class:     com_evolved_automata_nn_NativeTools
 * Method:    setSeed
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_com_evolved_automata_nn_NativeTools_setSeed
  (JNIEnv *, jclass, jlong);

/*
 * Class:     com_evolved_automata_nn_NativeTools
 * Method:    setRNDAlgorithm
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_com_evolved_automata_nn_NativeTools_setRNDAlgorithm
  (JNIEnv *, jclass, jint);

/*
 * Class:     com_evolved_automata_nn_NativeTools
 * Method:    randomLCG
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_com_evolved_automata_nn_NativeTools_randomLCG
  (JNIEnv *, jclass);

/*
 * Class:     com_evolved_automata_nn_NativeTools
 * Method:    initializeAllWeights
 * Signature: ([F)V
 */
JNIEXPORT void JNICALL Java_com_evolved_automata_nn_NativeTools_initializeAllWeights
  (JNIEnv *, jclass, jfloatArray);

/*
 * Class:     com_evolved_automata_nn_NativeTools
 * Method:    resetNetworkToInitialState
 * Signature: ([F)V
 */
JNIEXPORT void JNICALL Java_com_evolved_automata_nn_NativeTools_resetNetworkToInitialState
  (JNIEnv *, jclass, jfloatArray);

/*
 * Class:     com_evolved_automata_nn_NativeTools
 * Method:    forwardPass
 * Signature: ([F[F)V
 */
JNIEXPORT void JNICALL Java_com_evolved_automata_nn_NativeTools_forwardPass
  (JNIEnv *, jclass, jfloatArray, jfloatArray);

/*
 * Class:     com_evolved_automata_nn_NativeTools
 * Method:    setInputActivation
 * Signature: ([F[F)V
 */
JNIEXPORT void JNICALL Java_com_evolved_automata_nn_NativeTools_setInputActivation
  (JNIEnv *, jclass, jfloatArray, jfloatArray);

/*
 * Class:     com_evolved_automata_nn_NativeTools
 * Method:    updateForwardPassErrors
 * Signature: ([F[F)F
 */
JNIEXPORT jfloat JNICALL Java_com_evolved_automata_nn_NativeTools_updateForwardPassErrors
  (JNIEnv *, jclass, jfloatArray, jfloatArray);

/*
 * Class:     com_evolved_automata_nn_NativeTools
 * Method:    updateWeightsFromErrors
 * Signature: ([FI)V
 */
JNIEXPORT void JNICALL Java_com_evolved_automata_nn_NativeTools_updateWeightsFromErrors
  (JNIEnv *, jclass, jfloatArray, jint);

/*
 * Class:     com_evolved_automata_nn_NativeTools
 * Method:    getOutputActivation
 * Signature: ([F[F)V
 */
JNIEXPORT void JNICALL Java_com_evolved_automata_nn_NativeTools_getOutputActivation
  (JNIEnv *, jclass, jfloatArray, jfloatArray);

/*
 * Class:     com_evolved_automata_nn_NativeTools
 * Method:    learnInputOutputPairMap
 * Signature: (I[F[FI[F)F
 */
JNIEXPORT jfloat JNICALL Java_com_evolved_automata_nn_NativeTools_learnInputOutputPairMap
  (JNIEnv *, jclass, jint, jfloatArray, jfloatArray, jint, jfloatArray);

/*
 * Class:     com_evolved_automata_nn_NativeTools
 * Method:    learnInputOutputPairMapWithDetails
 * Signature: (I[F[FI[F[F)V
 */
JNIEXPORT void JNICALL Java_com_evolved_automata_nn_NativeTools_learnInputOutputPairMapWithDetails
  (JNIEnv *, jclass, jint, jfloatArray, jfloatArray, jint, jfloatArray, jfloatArray);

/*
 * Class:     com_evolved_automata_nn_NativeTools
 * Method:    learnTrainingSpec
 * Signature: (I[F[FIFZI)I
 */
JNIEXPORT jint JNICALL Java_com_evolved_automata_nn_NativeTools_learnTrainingSpec
  (JNIEnv *, jclass, jint, jfloatArray, jfloatArray, jint, jfloat, jboolean, jint);

/*
 * Class:     com_evolved_automata_nn_NativeTools
 * Method:    createCappedLSTM
 * Signature: ([F[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_evolved_automata_nn_NativeTools_createCappedLSTM
  (JNIEnv *, jclass, jfloatArray, jfloatArray, jfloatArray);

/*
 * Class:     com_evolved_automata_nn_NativeTools
 * Method:    appendVectorToSequence
 * Signature: ([F[F[F[FIFZI)I
 */
JNIEXPORT jint JNICALL Java_com_evolved_automata_nn_NativeTools_appendVectorToSequence
  (JNIEnv *, jclass, jfloatArray, jfloatArray, jfloatArray, jfloatArray, jint, jfloat, jboolean, jint);

/*
 * Class:     com_evolved_automata_nn_NativeTools
 * Method:    resetCappedLSTM
 * Signature: ([F[FZ)V
 */
JNIEXPORT void JNICALL Java_com_evolved_automata_nn_NativeTools_resetCappedLSTM
  (JNIEnv *, jclass, jfloatArray, jfloatArray, jboolean);

#ifdef __cplusplus
}
#endif
#endif
