#include "tools.h"
#include <stdint.h>

static long initialSeed = 1;
static long currentSeed = 1;


RNG_ALGORITHM RNG_METHOD = JAVA_DERIVATIVE;

const float FLOAT_MIN_VALUE = (-1 * FLT_MAX + 1);
const float FLOAT_MAX_VALUE = FLT_MAX;


// Park and Miller RNG constants
static const long int IA = 16807;
static const long int IM = 2147483647L;
static const double AM = 1.0/IM;
static const long int IQ = 127773;
static const long int IR = 2836;
static const long int MASK = 123459876;

// Java derivative RNG constants
static const long int LCG_M = 1L << 48;
static const long int LCG_A = 25214903917L;
static const long int LCG_C = 11L;
static const long int LCG_DIVIDER = 1L << 32;

// Mersenne Twister adapted from Wikipedia
// Define MT19937 constants (32-bit RNG)

static const uint32_t N = 624;
static const uint32_t M = 397;
static const uint32_t R = 31;
static const uint32_t A = 0x9908B0DF;
static const uint32_t F = 1812433253;
static const uint32_t U = 11;
static const uint32_t S = 7;
static const uint32_t B = 0x9D2C5680;
static const uint32_t T = 15;
static const uint32_t C = 0xEFC60000;
static const uint32_t L = 18;
static const uint32_t MASK_LOWER = (1ull << R) - 1;
static const uint32_t MASK_UPPER = (1ull << R);


static uint32_t  mt[N];
static uint16_t  __index =0;

// Re-init with a given seed
static void Initialize(const uint32_t  seed)
{
    uint32_t  i;

    mt[0] = seed;

    for ( i = 1; i < N; i++ )
    {
        mt[i] = (F * (mt[i - 1] ^ (mt[i - 1] >> 30)) + i);
    }

    __index = N;
}

static void Twist()
{
    uint32_t  i, x, xA;

    for ( i = 0; i < N; i++ )
    {
        x = (mt[i] & MASK_UPPER) + (mt[(i + 1) % N] & MASK_LOWER);

        xA = x >> 1;

        if ( x & 0x1 )
            xA ^= A;

        mt[i] = mt[(i + M) % N] ^ xA;
    }

    __index = 0;
}

// Obtain a 32-bit random number
static uint32_t ExtractU32()
{
    uint32_t  y;
    int       i = __index;

    if ( __index >= N )
    {
        Twist();
        i = __index;
    }

    y = mt[i];
    __index = i + 1;

    y ^= (mt[i] >> U);
    y ^= (y << S) & B;
    y ^= (y << T) & C;
    y ^= (y >> L);

    return y;
}

double getTwisterRNG()
{
    return ((double)ExtractU32())/UINT32_MAX;
}

double platformDefaultRNG()
{
    return (1.0*rand())/RAND_MAX;
}


double javaRNG()
{
    long nextSeed = ((LCG_A*currentSeed + LCG_C) % LCG_M);
    currentSeed =  nextSeed;
    long ranLong = nextSeed >> 17;
    // have to add 0.5 since nextSeed can overflow to negative numbers and randLong will be between -2^31 to 2^31 - 1
    double pointFive = 0.5;

    double ranDouble = ranLong;
    double lDivider = LCG_DIVIDER;

    return  pointFive + ranDouble/lDivider;
}

double parkMillerRNG()
{
    double ans;
    currentSeed^=MASK;
    long k = currentSeed/IQ;
    currentSeed = IA * (currentSeed - k * IQ) - IR * k;
    if (currentSeed < 0)
        currentSeed+=IM;
    ans = AM * currentSeed;
    currentSeed^=MASK;
    return ans;
}



// This is not defined for 
long getInitialRNGSeed()
{
    return initialSeed;
}

void setRNGMethod(RNG_ALGORITHM method)
{
    RNG_METHOD = method;
}

void setSeed(long seed)
{
    initialSeed = seed;
    switch (RNG_METHOD)
    {
        case JAVA_DERIVATIVE:
        case PARK_AND_MILLER:
        {
            currentSeed =  seed;
        }
        break;
        case PLATFORM_DEFAULT:
        {
            srand(abs(seed) % UINT32_MAX);
        }
        break;
        case MERSENNE_TWISTER:
        {
            Initialize(abs(seed) % UINT32_MAX);
        }
        break;
    }
    
}

double randomLCG()
{
    switch (RNG_METHOD)
    {
        case JAVA_DERIVATIVE:
        {
            return javaRNG();
        }
        case PARK_AND_MILLER:
        {
            return parkMillerRNG();
        }
        case MERSENNE_TWISTER:
        {
            return getTwisterRNG();
        }
        case PLATFORM_DEFAULT:
        {
            return platformDefaultRNG();
        }
        default:
            return platformDefaultRNG();
    }

}



double randomGaussian()
{
    float fac, rsq, v1, v2;

    do
    {
        v1 = (float)(2*randomLCG() - 1);
        v2 = (float)(2*randomLCG() - 1);
        rsq = v1*v1 + v2*v2;

    }while (rsq >=1 || rsq == 0);
    fac = (float)sqrt(-2*log(rsq)/rsq);
    return v2*fac;
}

int probabilisticSample(float* weights, int numWeights, float weightSum, boolean failureOnAllZeroP)
{
    float cutPoint = (float)randomLCG()*weightSum;

    for (int i = 0; i < numWeights;i++)
    {
        if (weights[i] > cutPoint)
            return i;
        else
        {
            cutPoint -= weights[i];

        }
    }
    if (failureOnAllZeroP)
        return -1;
    else
        return (int)(randomLCG() * numWeights);
}


float sign(float value)
{
    if (value > 0)
        return 1;
    else if (value == 0)
        return 0;
    else
        return -1;
}

void roundVectorToInt(float* vector, int length)
{
    for (int i = 0; i < length; i++)
    {
        vector[i] = roundToInt(vector[i]);
    }
}

void printVector(float* vector, int length, boolean useLinefeed)
{

    if (useLinefeed)
        printf("[\n");
    else
        printf("[");
    for (int i = 0; i < length; i++)
    {
        if (i > 0)
        {
            printf(", ");
        }
        if (useLinefeed)
            printf("%f\n", vector[i]);
        else
            printf("%f", vector[i]);
    }
    if (useLinefeed)
        printf("]\n");
    else
        printf("]");
}

boolean roundedEquals(float lvalue, float rvalue)
{
    return roundToInt(lvalue) == roundToInt(rvalue);
}

