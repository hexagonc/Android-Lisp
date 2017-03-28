#ifndef _tools_h
#define _tools_h
#include <float.h>
#include <math.h>
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>




extern const float FLOAT_MIN_VALUE;
extern const float FLOAT_MAX_VALUE;
typedef unsigned char boolean;

#define true 1
#define false 0
#define max(a, b) (((a) <= (b)) ? (b) : (a))
#define min(a, b) (((a) >= (b)) ? (b) : (a))
#define abs(a) (((a) < 0 ) ? (-1 * (a)) : (a))

#define roundToInt(v) (((v) < 0.5) ? 0 : 1)

extern float sign(float value);

extern void roundVectorToInt(float* vector, int length);

extern boolean roundedEquals(float lvalue, float rvalue);

// -<><><><><><><><><><><><><><><><><><><><>-
// Randomization parameters
// -<><><><><><><><><><><><><><><><><><><><>-

enum RNG_ALGORITHM
{
	JAVA_DERIVATIVE = 0, PARK_AND_MILLER = 1, MERSENNE_TWISTER = 2, PLATFORM_DEFAULT = 3
};

typedef enum RNG_ALGORITHM RNG_ALGORITHM;

extern RNG_ALGORITHM RNG_METHOD;

// .-_-. .-_-. .-_-. .-_-. .-_-. .-_-. .-_-. .-_-. 
// Randomization functions
// .-_-. .-_-. .-_-. .-_-. .-_-. .-_-. .-_-. .-_-. 
extern void setSeed(long seed);

extern void setRNGMethod(RNG_ALGORITHM method);

extern long getInitialRNGSeed();

// adapted from numerical recipes in c
extern double randomLCG();

extern double randomGaussian();

extern int probabilisticSample(float* weights, int numWeights, float weightSum, boolean failureOnAllZeroP);

extern void printVector(float* vector, int length, boolean useLinefeed);

extern double getTwisterRNG();
extern double platformDefaultRNG();
extern double javaRNG();
extern double parkMillerRNG();


#endif