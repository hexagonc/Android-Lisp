#ifndef _neural_library_h
#define _neural_library_h
#include <math.h>
#include <float.h>
#include <time.h>
#include <limits.h>
#include "tools.h"



extern boolean DEBUG;


static int PREDICTOR_ID = 0;

enum WeightUpdateType
{
    RPROP = 0, DEFAULT = 1
};



typedef enum WeightUpdateType WeightUpdateType;

extern WeightUpdateType updateTypeToInt(int update);

typedef struct TrainingSpecIndex {
	int totalDataLength;
	int recordLength;
	int inputLength;
	int outputLength;
	int numTrainingSamples;
	int inputOffsetIndex;
	int outputOffsetIndex;
	int errorMaskOffsetIndex;
	int skipMinAcceptabledErrorCheckIndex;
    int resetNetworkStateIndex;
    int useAverageErrorIndex;
    int hasErrorMaskIndex;

} TrainingSpecIndex;

typedef struct NetworkSpec {
	float* networkSpec;
	int totalDataLength;
	int inputLayerWidth;
	int outputLayerWidth;
	int memoryCellWidth;
	float learningRate;
	float initialDelta;
	float minInitialWeight;
	float maxInitialWeight;
	float maxDelta;
	float minDelta;
	float n_Max;
	float n_Min;
	float convergenceThreshold;
	WeightUpdateType updateType;
	int* feedforwardLinkOrder;
	int numForwardLinks;
	int* weightUpdateLinkOrder;
	int numWeightUpdates;
	int outputErrorFunctionId;
	int outputActivationFunctionId;

} NetworkSpec;



#define CURRENT_VERSION 1
#define MIN_INITIAL_WEIGHT -1

#define   CURRENT_VERSION  1
#define   MIN_INITIAL_WEIGHT  -1
#define   MAX_INITIAL_WEIGHT  1
#define   N_PLUS  1.2F
#define   N_MINUS  0.5F
#define   MIN_WEIGHT_DELTA  0
#define   MAX_WEIGHT_DELTA  50
#define   INITIAL_WEIGHT_DELTA  0.0125F
#define   LEARNING_RATE 0.001F
// not currently used
#define   MOMENTUM_FRACTION   0.1F
// ******************************************************
// START: Port these ants to c/c++ as #define variables/defines
// ******************************************************

#define   ERROR_STATUS_FAILURE  1
#define   ERROR_STATUS_SUCESS  0

#define   LINK_DATA_SOURCE_LAYER_ID_IDX  0
#define   LINK_DATA_TARGET_LAYER_ID_IDX  1
#define   LINK_DATA_WEIGHT_MATRIX_IDX  2

#define   NUM_LAYERS  8
#define   NUM_CUSTOM_DATA_REGISTERS  10

static const int VERSION_IDX = 0;
static const int NETWORK_LENGTH_IDX = VERSION_IDX + 1;
static const int CUSTOM_VARIABLE_DATA_IDX = (NETWORK_LENGTH_IDX + 1);
static const int   CUSTOM_DATA_1_IDX  = (CUSTOM_VARIABLE_DATA_IDX + 1);
static const int   FORWARD_PASS_LINK_ORDER_DATA_IDX  = (CUSTOM_DATA_1_IDX + NUM_CUSTOM_DATA_REGISTERS);
static const int   BACKWARD_PASS_LINK_ORDER_DATA_IDX = (FORWARD_PASS_LINK_ORDER_DATA_IDX + 1);

static const int   MIN_INITIAL_WEIGHT_IDX = (BACKWARD_PASS_LINK_ORDER_DATA_IDX + 1);
static const int   MAX_INITIAL_WEIGHT_IDX = (MIN_INITIAL_WEIGHT_IDX + 1);
static const int   N_PLUS_IDX = (MAX_INITIAL_WEIGHT_IDX + 1);
static const int   N_MINUS_IDX = (N_PLUS_IDX + 1);
static const int   MIN_WEIGHT_DELTA_IDX = (N_MINUS_IDX + 1);
static const int   MAX_WEIGHT_DELTA_IDX = (MIN_WEIGHT_DELTA_IDX + 1);
static const int   INITIAL_WEIGHT_DELTA_IDX = (MAX_WEIGHT_DELTA_IDX + 1);
static const int   LEARNING_RATE_IDX = (INITIAL_WEIGHT_DELTA_IDX + 1);
static const int   MOMENTUM_FRACTION_IDX  = (LEARNING_RATE_IDX + 1);




static const int OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX = (MOMENTUM_FRACTION_IDX + 1);

static const int LAYER_ACTIVATION_FUNCTION_ID_IDX = (OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX + 1);
// Start of data map indexes
/**
 * Location of the mapping between linkIds and link data indices
 */
static const int LINK_ID_TO_DATA_IDX_IDX = (LAYER_ACTIVATION_FUNCTION_ID_IDX + NUM_LAYERS);

/**
 * Defines the connectivity for each layer in the canonical layer order,
 * which is I, O, CI, CO, F, IG, OG, P
 * Forward link spec from layer i to layer j can be found as
 * NETWORK_GRAPH_BASE_IDX + 2*i.  A link spec consists of first the number
 * of links and second, an array of the layerids themselves
 */
static const int   NETWORK_GRAPH_BASE_IDX = (LINK_ID_TO_DATA_IDX_IDX + NUM_LAYERS * NUM_LAYERS);
static const int   INPUT_NODE_COUNT_IDX = (NETWORK_GRAPH_BASE_IDX + (1 + NUM_LAYERS)* NUM_LAYERS);
static const int   OUTPUT_NODE_COUNT_IDX = (INPUT_NODE_COUNT_IDX + 1);
static const int   NUM_CELL_STATES_IDX = (OUTPUT_NODE_COUNT_IDX + 1);
static const int   LAYER_DIMEN_MAP_IDX = (NUM_CELL_STATES_IDX + 1);
static const int   LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX = (LAYER_DIMEN_MAP_IDX + NUM_LAYERS);
static const int   LAYER_STATE_BASE_IDX = (LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + NUM_LAYERS);


#define   INPUT_LAYER_ID  0
#define   OUTPUT_LAYER_ID  1
#define   CELL_INPUT_LAYER_ID  2
#define   CELL_OUTPUT_LAYER_ID  3
#define   FORGET_LAYER_ID  4
#define   INPUT_GATE_LAYER_ID  5
#define   OUTPUT_GATE_LAYER_ID  6
#define   PEEPHOLE_LAYER_ID  7


// Activation function ids
#define   SIGMOID_ACTIVATION_ID  0
#define   HYPERTANGENT_ACTIVATION_ID  1
#define   RECTILINEAR_ACTIVATION_ID  2
#define   IDENTITY_ACTIVATION_ID  3
#define   SOFTMAX_ACTIVATION_ID  4


static const int LAYER_ID_ACTIVATION_FUNCTION_MAP[] = {IDENTITY_ACTIVATION_ID, SIGMOID_ACTIVATION_ID, SIGMOID_ACTIVATION_ID, SIGMOID_ACTIVATION_ID, SIGMOID_ACTIVATION_ID, SIGMOID_ACTIVATION_ID, SIGMOID_ACTIVATION_ID, HYPERTANGENT_ACTIVATION_ID};


#define MSE_ERROR_FUNCTION_ID  0
#define CROSS_ENTROPY_ERROR_ID 1



// Custom data 

static const int MATCH_COUNT = 0;
static const int FAILURE_COUNT = 1;
static const int LENGTH = 2;
static const int WEIGHT = 3;
static const int ID = 4;


typedef float (*unary_function) (float value);
typedef float (*unary_vector_function) (float* data, int vectorSpecificationIndex, int length, int i);
typedef double (*output_error_function) (float* networkData, float* expectedOutput);
typedef double (*output_error_function_derivative) (float* networkData, float* expectedOutput, int i);

typedef struct OutputErrorFunction { 
	output_error_function error;
	output_error_function_derivative errorPartialDerivative;
} OutputErrorFunction;




// Functions

static float sigmoidFunction(float value)
{
	return (float) (1.0/(1.0 + exp(-1.0* value)));
}

static float sigmoidDerivative(float value)
{
	double sig = 1.0/(1.0 + exp(-1.0* value));
	return (sig * (1.0 - sig));
}

static float hypertangent(float value)
{
	return (float)tanh(value);
}

static float hypertangentDerivative(float value)
{
	double coshh = cosh(value);
	return (float)(1.0/(coshh/coshh));
}

static float identity(float value)
{
	return value;
}

static float identityDerivative(float value)
{
	return 1;
}

static float rectilinear(float value)
{
	if (value < 0)
	{
		return 0;
	}
	return value;
}

static float rectilinearDerivative(float value)
{
	if (value < 0)
		return 0;
	return 1;
}



static float softmaxVectorOperator(float* data, int vectorSpecificationIndex, int length, int i)
{

    double sum = 0, maxValue = FLOAT_MIN_VALUE, iValue=0, value, vectorValue;
    // calculate
    for (int j = 0; j < length;j++ )
    {
        vectorValue = data[vectorSpecificationIndex + j];
        maxValue = max(maxValue, vectorValue);
    }

    for (int j = 0; j < length;j++ )
    {
        vectorValue = data[vectorSpecificationIndex + j] - maxValue;
        value = exp(vectorValue );
        sum +=value;
        if (j == i)
        {
            iValue = value;
        }
    }



    if (sum == 0.0)
    { // this probably means numerical underflow somewhere.  Will have to treat all possibilities as the same
        return 1.0/length;
    }
    float out = (iValue/sum);
    
    
    // this shouldn't be possible
    if (out != out)
    {
        // Handle overflow issues
        // see: https://www-s.acm.illinois.edu/webmonkeys/book/c_guide/2.7.html#exp

    }
    return out;
}

static float softmaxVectorDerivativeOperator(float* data, int vectorSpecificationIndex, int length, int i)
{
    float s_i = softmaxVectorOperator(data, vectorSpecificationIndex, length, i);
    return s_i * (1.0 - s_i);
}

static float sigmoidVectorOperator(float* data, int vectorSpecificationIndex, int length, int i)
{
    float vectorValue = data[vectorSpecificationIndex + i];

    return sigmoidFunction(vectorValue);
}

static float sigmoidDerivativeOperator(float* data, int vectorSpecificationIndex, int length, int i)
{
    float vectorValue = data[vectorSpecificationIndex  + i];
    return sigmoidDerivative(vectorValue);
}

static float hypertangentVectorOperator(float* data, int vectorSpecificationIndex, int length,  int i)
{
    float vectorValue = data[vectorSpecificationIndex + i];

    return hypertangent(vectorValue);
}


static float hypertangentVectorDerivativeOperator(float* data, int vectorSpecificationIndex, int length, int i)
{
    float vectorValue = data[vectorSpecificationIndex + i];
    return hypertangentDerivative(vectorValue);
}

static float rectilinearVectorOperator(float* data, int vectorSpecificationIndex, int length, int i)
{
    float vectorValue = data[vectorSpecificationIndex + i];

    return rectilinear(vectorValue);
}

static float rectilinearVectorDerivativeOperator(float* data, int vectorSpecificationIndex, int length, int i)
{
    float vectorValue = data[vectorSpecificationIndex + i];
    return rectilinearDerivative(vectorValue);
}

static float identityVectorOperator(float* data, int vectorSpecificationIndex, int length, int i)
{
    float vectorValue = data[vectorSpecificationIndex + i];

    return identity(vectorValue);
}

static float identityVectorOperatorDerivative(float* data, int vectorSpecificationIndex, int length, int i)
{
    float vectorValue = data[vectorSpecificationIndex + i];
    return identityDerivative(vectorValue);
}

static unary_vector_function ACTIVATION_FUNCTION_MAP[] = {sigmoidVectorOperator, hypertangentVectorOperator, rectilinearVectorOperator, identityVectorOperator, softmaxVectorOperator};
static unary_vector_function ACTIVATION_FUNCTION_DERIVATIVE_MAP[] = {sigmoidDerivativeOperator, hypertangentVectorDerivativeOperator, rectilinearVectorDerivativeOperator, identityVectorOperatorDerivative, softmaxVectorDerivativeOperator};


// -<><><><><><><><><><><><><><><><><><><><>-
// Main Data Access Macros and Functions
// -<><><><><><><><><><><><><><><><><><><><>-

// Network data access macros







#define getLinkId( sourceId,  targetId) ((sourceId) * NUM_LAYERS + (targetId))


#define getTargetLayerFromLinkId(linkId) (((int)(linkId)) % NUM_LAYERS)
#define getSourceLayerFromLinkId(linkId) (((int)(linkId)) / NUM_LAYERS)

static int getLinkDataIndex(float* networkSpec, int linkId)
{
    return (int)networkSpec[LINK_ID_TO_DATA_IDX_IDX + linkId];
}

static int getLinkSourceLayerId(float* networkSpec, int linkId)
{
    int linkDataIdx = getLinkDataIndex(networkSpec, linkId);
    return (int)networkSpec[linkDataIdx + LINK_DATA_SOURCE_LAYER_ID_IDX];
}

static int getLinkSourceLayerIdIdx(int data_idx)
{
    return data_idx + LINK_DATA_SOURCE_LAYER_ID_IDX;
}

static int getLinkTargetLayerIdIdx(int data_idx)
{
    return data_idx + LINK_DATA_TARGET_LAYER_ID_IDX;
}

static int getLinkWeightIndex(int rows, int cols, int data_idx)
{
    return data_idx + LINK_DATA_WEIGHT_MATRIX_IDX;
}

static int getLinkWeightDeltaIndex(int rows, int cols, int data_idx)
{
    return data_idx + LINK_DATA_WEIGHT_MATRIX_IDX + rows*cols;
}

static int getLinkElligibilityTraceIndex(int rows, int cols, int data_idx)
{
    return data_idx + LINK_DATA_WEIGHT_MATRIX_IDX + 2 * rows*cols;
}

static int getLinkCalculatedGradientIndex(int rows, int cols, int data_idx)
{
    return data_idx + LINK_DATA_WEIGHT_MATRIX_IDX + 3 * rows*cols;
}

static int getLinkPrevCalculatedGradientIndex(int rows, int cols, int data_idx)
{
    return data_idx + LINK_DATA_WEIGHT_MATRIX_IDX + 4 * rows*cols;
}

static int getLayerActivationFunctionId(float* networkSpec, int layerId)
{
    return (int)networkSpec[LAYER_ACTIVATION_FUNCTION_ID_IDX + layerId];
}

static unary_vector_function getLayerActivationFunction(float* networkSpec, int layerId)
{

    return  ACTIVATION_FUNCTION_MAP[getLayerActivationFunctionId(networkSpec, layerId)];
}

static unary_vector_function getLayerActivationFunctionDerivative(float* networkSpec, int layerId)
{

    return  ACTIVATION_FUNCTION_DERIVATIVE_MAP[getLayerActivationFunctionId(networkSpec, layerId)];
}

static int getLayerStateIndex(float* networkSpec, int layerId)
{
    return (int)networkSpec[LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerId];
}

static int getLayerWidth(float* networkSpec, int layerId)
{
    return (int)networkSpec[LAYER_DIMEN_MAP_IDX + layerId];
}

static int getLinkSourceLayerWidth(float* networkSpec, int linkId)
{
    int linkDataIdx = getLinkDataIndex(networkSpec, linkId);
    int layerId = (int)networkSpec[linkDataIdx + LINK_DATA_SOURCE_LAYER_ID_IDX];
    return getLayerWidth(networkSpec, layerId);
}

static int getLinkTargetLayerId(float* networkSpec, int linkId)
{
    int linkDataIdx = getLinkDataIndex(networkSpec, linkId);
    return (int)networkSpec[linkDataIdx + LINK_DATA_TARGET_LAYER_ID_IDX];
}

static int getLinkTargetLayerWidth(float* networkSpec, int linkId)
{
    int linkDataIdx = getLinkDataIndex(networkSpec, linkId);
    int layerId = (int)networkSpec[linkDataIdx + LINK_DATA_TARGET_LAYER_ID_IDX];
    return getLayerWidth(networkSpec, layerId);
}

static int getOutputLayerMaskIndex(float* networkSpec)
{
    int layer_width = getLayerWidth(networkSpec, OUTPUT_LAYER_ID);
    int layer_state_idx = getLayerStateIndex(networkSpec, OUTPUT_LAYER_ID);
    return layer_state_idx + 4 * layer_width;
}

/*
    #define getLinkDataIndex(networkSpec, linkId) ( (int) (networkSpec[LINK_ID_TO_DATA_IDX_IDX + linkId]))
    #define getLinkSourceLayerIdIdx(data_idx) (data_idx + LINK_DATA_SOURCE_LAYER_ID_IDX)
    #define getLinkTargetLayerIdIdx(data_idx) (data_idx + LINK_DATA_TARGET_LAYER_ID_IDX)
    #define getLinkWeightIndex(rows, cols, data_idx) (data_idx + LINK_DATA_WEIGHT_MATRIX_IDX)
    #define getLinkWeightDeltaIndex(rows, cols, data_idx) (data_idx + LINK_DATA_WEIGHT_MATRIX_IDX + (rows)*(cols))
    #define getLinkElligibilityTraceIndex( rows,  cols,  data_idx) (data_idx + LINK_DATA_WEIGHT_MATRIX_IDX + 2 * (rows)*(cols))
    #define getLinkCalculatedGradientIndex( rows,  cols,  data_idx)(data_idx + LINK_DATA_WEIGHT_MATRIX_IDX + 3 * (rows)*(cols))
    #define getLinkPrevCalculatedGradientIndex( rows,  cols,  data_idx) (data_idx + LINK_DATA_WEIGHT_MATRIX_IDX + 4 * (rows)*(cols))
    #define getLayerActivationFunctionId(networkSpec, layerId) ((int)(networkSpec[LAYER_ACTIVATION_FUNCTION_ID_IDX + (layerId)]))
    #define getLayerActivationFunction(networkSpec, layerId) (ACTIVATION_FUNCTION_MAP[getLayerActivationFunctionId(networkSpec, layerId)])
    #define getLayerActivationFunctionDerivative(networkSpec, layerId) (ACTIVATION_FUNCTION_DERIVATIVE_MAP[getLayerActivationFunctionId(networkSpec, layerId)])
    #define getLayerStateIndex(networkSpec, layerId) ((int)(networkSpec[LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + (layerId)]))
    #define getLayerWidth(networkSpec, layerId) ((int)(networkSpec[LAYER_DIMEN_MAP_IDX + layerId]))
    #define getLinkSourceLayerWidth(networkSpec, linkId) ( getLayerWidth(networkSpec, (int)(networkSpec[getLinkDataIndex(networkSpec, linkId) + LINK_DATA_SOURCE_LAYER_ID_IDX])))

    #define getLinkTargetLayerId(networkSpec, linkId) (  (int)networkSpec[(int)(networkSpec[getLinkDataIndex(networkSpec, linkId)]) + LINK_DATA_TARGET_LAYER_ID_IDX] )
    #define getLinkSourceLayerId(networkSpec, linkId) (  (int)networkSpec[(int)(networkSpec[getLinkDataIndex(networkSpec, linkId)]) + LINK_DATA_SOURCE_LAYER_ID_IDX] )


    #define getLinkTargetLayerWidth(networkSpec, linkId) (getLayerWidth(networkSpec, (int)(networkSpec[getLinkDataIndex(networkSpec, linkId) + LINK_DATA_TARGET_LAYER_ID_IDX])))
    #define getOutputLayerMaskIndex(networkSpec) ( getLayerStateIndex(networkSpec, OUTPUT_LAYER_ID) + 4 * getLayerWidth(networkSpec, OUTPUT_LAYER_ID))

  */

// Learning spec Data accessor macros


static int getBackwardPassLinkIndex(float* networkSpec)
{
    return (int)networkSpec[BACKWARD_PASS_LINK_ORDER_DATA_IDX];
}

static int getForwardPassLinkIndex(float* networkSpec)
{
    return (int)networkSpec[FORWARD_PASS_LINK_ORDER_DATA_IDX];
}

static void setMaxInitialWeight(float* networkSpec, float weight)
{
    networkSpec[MAX_INITIAL_WEIGHT_IDX] = weight;
}

static float getMaxInitialWeight(float* networkSpec)
{
    return networkSpec[MAX_INITIAL_WEIGHT_IDX];
}

static void setMinInitialWeight(float* networkSpec, float weight)
{
    networkSpec[MIN_INITIAL_WEIGHT_IDX] = weight;
}

static float getMinInitialWeight(float* networkSpec)
{
    return networkSpec[MIN_INITIAL_WEIGHT_IDX];
}

static void setRPROPInitialWeightDelta(float* networkSpec, float delta)
{
    networkSpec[INITIAL_WEIGHT_DELTA_IDX] = delta;
}

static float getRPROPInitialWeightDelta(float* networkSpec)
{
    return networkSpec[INITIAL_WEIGHT_DELTA_IDX];
}

static void setRPROPMaxWeightUpdateDelta(float* networkSpec, float delta)
{
    networkSpec[MAX_WEIGHT_DELTA_IDX] = delta;
}

static float getRPROPMaxWeightUpdateDelta(float* networkSpec)
{
    return networkSpec[MAX_WEIGHT_DELTA_IDX];
}

static void setRPROPMinWeightUpdateDelta(float* networkSpec, float delta)
{
    networkSpec[MIN_WEIGHT_DELTA_IDX] = delta;
}

static float getRPROPMinWeightUpdateDelta(float* networkSpec)
{
    return networkSpec[MIN_WEIGHT_DELTA_IDX];
}

static void setRROPN_Minus(float* networkSpec, float n_minus)
{
    networkSpec[N_MINUS_IDX] = n_minus;
}

static float getRROPN_Minus(float* networkSpec)
{
    return networkSpec[N_MINUS_IDX];
}

static void setRROPN_Plux(float* networkSpec, float n_plus)
{
    networkSpec[N_PLUS_IDX] = n_plus;
}

static float getRROPN_Plus(float* networkSpec)
{
    return networkSpec[N_PLUS_IDX];
}

static void setLearningRate(float* networkSpec, float learningRate)
{
    networkSpec[LEARNING_RATE_IDX] = learningRate;
}


static float getLearningRate(float* networkSpec)
{
    return networkSpec[LEARNING_RATE_IDX];
}
/*
    #define getBackwardPassLinkIndex(networkSpec) ((int)networkSpec[BACKWARD_PASS_LINK_ORDER_DATA_IDX])
    #define getForwardPassLinkIndex(networkSpec) ((int)networkSpec[FORWARD_PASS_LINK_ORDER_DATA_IDX])
    #define setMaxInitialWeight( networkSpec, weight) networkSpec[MAX_INITIAL_WEIGHT_IDX] = weight
    #define getMaxInitialWeight(networkSpec) (networkSpec[MAX_INITIAL_WEIGHT_IDX])
    #define setMinInitialWeight( networkSpec, weight) networkSpec[MIN_INITIAL_WEIGHT_IDX] = weight
    #define getMinInitialWeight(networkSpec) (networkSpec[MIN_INITIAL_WEIGHT_IDX])
    #define setRPROPInitialWeightDelta(networkSpec, delta) networkSpec[INITIAL_WEIGHT_DELTA_IDX] = delta
    #define getRPROPInitialWeightDelta(networkSpec) (networkSpec[INITIAL_WEIGHT_DELTA_IDX])
    #define setRPROPMaxWeightUpdateDelta(networkSpec, delta) networkSpec[MAX_WEIGHT_DELTA_IDX] = delta
    #define getRPROPMaxWeightUpdateDelta(networkSpec) (networkSpec[MAX_WEIGHT_DELTA_IDX])
    #define setRPROPMinWeightUpdateDelta(networkSpec, delta) networkSpec[MIN_WEIGHT_DELTA_IDX] = delta
    #define getRPROPMinWeightUpdateDelta(networkSpec) (networkSpec[MIN_WEIGHT_DELTA_IDX])
    #define setRROPN_Minus(networkSpec, n_minus) networkSpec[N_MINUS_IDX] = n_minus
    #define getRROPN_Minus(networkSpec) (networkSpec[N_MINUS_IDX])
    #define setRROPN_Plux(networkSpec, n_plus) networkSpec[N_PLUS_IDX] = n_plus
    #define getRROPN_Plus(networkSpec) (networkSpec[N_PLUS_IDX])
    #define setLearningRate (networkSpec, learningRate) networkSpec[LEARNING_RATE_IDX] = learningRate
    #define getLearningRate(networkSpec) (networkSpec[LEARNING_RATE_IDX])
*/


static int getLayerActivationIndex(float* networkSpec, int layerId)
{
    int layer_width = getLayerWidth(networkSpec, layerId);
    int layer_state_idx = getLayerStateIndex(networkSpec, layerId);
    if (layerId == PEEPHOLE_LAYER_ID)
        return layer_state_idx;
    else
        return layer_state_idx + layer_width;
}

static int getLayerNetInputIndex(float* networkSpec, int layerId)
{
    int layer_width = getLayerWidth(networkSpec, layerId);
    int layer_state_idx = getLayerStateIndex(networkSpec, layerId);
    if (layerId == PEEPHOLE_LAYER_ID)
        return layer_state_idx + layer_width;
    else
        return layer_state_idx;
}

static int getLayerPartialNetInputIndex(float* networkSpec, int layerId)
{
    int layer_width = getLayerWidth(networkSpec, layerId);
    int layer_state_idx = getLayerStateIndex(networkSpec, layerId);
    if (layerId == PEEPHOLE_LAYER_ID)
        return -1;
    else
        return layer_state_idx + 2 * layer_width;
}

static int getLayerErrorResponsibilityIndex(float* networkSpec, int layerId)
{
    int layer_width = getLayerWidth(networkSpec, layerId);
    int layer_state_idx = getLayerStateIndex(networkSpec, layerId);
    if (layerId == PEEPHOLE_LAYER_ID)
        return -1;
    else
        return layer_state_idx + 3 * layer_width;
}

// -<><><><><><><><><><><><><><><><><><><><>-
// Helper functions and Macros
// -<><><><><><><><><><><><><><><><><><><><>-
#define getOutputLinkErrorPartialDerivative(networkSpec, sourceNodeIndex) ( networkSpec[getLayerErrorResponsibilityIndex(networkSpec, CELL_OUTPUT_LAYER_ID) + (sourceNodeIndex)])
#define initializeNodeState(networkSpec) ( resetMemoryCellState(networkSpec))
#define getMatrixCellIndex( cols,  i,  j) ( (cols) * (i) + (j))
#define getVectorValue(all_data, vector_idx, i) ( all_data[(vector_idx) + (i)] )
#define getMatrixValue(cols, all_data, matrix_idx, i, j) ( all_data[(matrix_idx) + getMatrixCellIndex(cols, i, j)] )

// Training Spec accessor macros
#define getInputIndex(index, offset) (offset + index.inputOffsetIndex)
#define getExpectedOutputIndex(index, offset) (offset + index.outputOffsetIndex)
#define getErrorMaskIndex(index, offset) (offset + index.errorMaskOffsetIndex)
#define getSkipMinAcceptableErrorPIndex(index, offset) (offset + index.skipMinAcceptabledErrorCheckIndex)
#define getResetNetworkStatePIndex(index, offset) (offset + index.resetNetworkStateIndex)
#define getUseAverageErrorPIndex(index, offset) (offset + index.useAverageErrorIndex)
#define getHasErrorMaskIndex(index, offset) (offset + index.hasErrorMaskIndex)



// o-:-o o-:-o o-:-o o-:-o o-:-o o-:-o o-:-o o-:-o 
// Error functions
// o-:-o o-:-o o-:-o o-:-o o-:-o o-:-o o-:-o o-:-o 


static double mseError(float* networkData, float* expectedOutput)
{
	int errorMaskIndex =  getOutputLayerMaskIndex(networkData);
    int width = getLayerWidth(networkData, OUTPUT_LAYER_ID);
    int output_layer_activation_idx = getLayerActivationIndex(networkData, OUTPUT_LAYER_ID);
    double mse = 0, error;
    double output_activation, expected_activation;
    float mask;
    float weightSum = 0;
    for (int i = 0;i < width;i++)
    {
        output_activation = networkData[output_layer_activation_idx + i];
        expected_activation = expectedOutput[i];
        error = (expected_activation - output_activation);
        mask = getVectorValue(networkData, errorMaskIndex, i);
        mse += error*error*mask;
        weightSum+=mask;
    }
    return mse/weightSum;
}

static double mseErrorDerivative(float* networkData, float* expectedOutput, int i)
{
    int errorMaskIndex =  getOutputLayerMaskIndex(networkData);
    int width = getLayerWidth(networkData, OUTPUT_LAYER_ID);
    int output_layer_activation_idx = getLayerActivationIndex(networkData, OUTPUT_LAYER_ID);
    double output_activation = networkData[output_layer_activation_idx + i];
    float mask = getVectorValue(networkData, errorMaskIndex, i);
    return 2*(expectedOutput[i] - output_activation)* mask/width;
}



static double crossEntropyError(float* networkData, float* expectedOutput)
{
	int outputLayerActivationFunctionId = getLayerActivationFunctionId(networkData, OUTPUT_LAYER_ID);
    switch (outputLayerActivationFunctionId)
    {
        case SIGMOID_ACTIVATION_ID:
            // TODO: implement stable version for sigmoid
        case SOFTMAX_ACTIVATION_ID:
            // TODO: implement stable version for softmax
        case RECTILINEAR_ACTIVATION_ID:
            // TODO: implement stable version for ReLU
        default:
        {
            int width = getLayerWidth(networkData, OUTPUT_LAYER_ID);
            double error = 0, output, expected, errorMask;
            int activationIndex = getLayerActivationIndex(networkData, OUTPUT_LAYER_ID);
            int errorMaskIndex =  getOutputLayerMaskIndex(networkData);
            float nodeError;
            double minY = 0.001;
            // Normally we would use ∑t_i *Ln(y_i)
            for (int i = 0; i < width; i++)
            {
                nodeError = 0;
                errorMask = getVectorValue(networkData, errorMaskIndex, i);

                expected = expectedOutput[i];
                if (errorMask != 0 && expected != 0)
                {

                    output = getVectorValue(networkData, activationIndex, i);
                    nodeError = (float)(expected * log(max(minY, output)/expected));

                }

                error +=nodeError*errorMask;

            }
            // TODO: handle the case of NaN
            
            return -1.0*error;
        }

    }
}

static double crossEntropyDerivative(float* networkData, float* expectedOutput, int i)
{
    int errorMaskIndex =  getOutputLayerMaskIndex(networkData);
    int activationIndex = getLayerActivationIndex(networkData, OUTPUT_LAYER_ID);
    return (expectedOutput[i] - getVectorValue(networkData, activationIndex, i))*getVectorValue(networkData, errorMaskIndex, i);
}



static const OutputErrorFunction MeanSquaredError = {mseError,  mseErrorDerivative};
static const OutputErrorFunction CrossEntropyError = {crossEntropyError, crossEntropyDerivative};

//OutputErrorFunction ERROR_FUNCTION_MAP[] = {MeanSquaredError, CrossEntropyError};
static OutputErrorFunction ERROR_FUNCTION_MAP[] = { {mseError,  mseErrorDerivative}, {crossEntropyError, crossEntropyDerivative}};


// (*.*) (*.*) (*.*) (*.*) (*.*) (*.*) (*.*) (*.*) (*.*) (*.*) 
//				Main Interface functions (to be exposed to java)
// (*.*) (*.*) (*.*) (*.*) (*.*) (*.*) (*.*) (*.*) (*.*) (*.*) 

void copyNetwork(float* baseNetwork, float* targetNetwork);

float getId(float* networkSpec);

void setId(float* networkSpec, float id);

int getFailureCount(float* networkSpec);

void incrementFailureCount(float* networkSpec);

void incrementCustomParameter(float* networkSpec, int parameter);

void decrementCustomParameter(float* networkSpec, int parameter, boolean allowNegativeP);

void resetMatchCount(float* networkSpec);

void resetFailureCount(float* networkSpec);

void decrementFailureCount(float* networkSpec);

void decrementMatchCount(float* networkSpec);

void incrementMatchCount(float* networkSpec);

int getMatchCount(float* networkSpec);

int getCappedLSTMLength(float* networkSpec);

void setCappedLSTMLength(float* networkSpec, int i);

void incrementLength(float* networkSpec);

boolean isAtFinalState(float* networkSpec, float* finalState);

void resetCappedLSTM(float* networkSpec, float* initialState, boolean onlyMetaDataP);

int createCappedLSTM(float* networkSpec, float* startInput, float* stopInput);

static int getNumCustomData(float* networkSpec)
{
    return NUM_CUSTOM_DATA_REGISTERS;
}

static int setCustomData(float* networkSpec, float data, int index)
{
    if (index >= getNumCustomData(networkSpec) || index < 0)
        return ERROR_STATUS_FAILURE;
    networkSpec[CUSTOM_DATA_1_IDX + index] = data;
    return ERROR_STATUS_SUCESS;
}


static float getCustomData(float* networkSpec, int index, float errorValue)
{
    if (index >= getNumCustomData(networkSpec) || index < 0)
        return errorValue;
    return networkSpec[CUSTOM_DATA_1_IDX + index];
}

static int getCustomVariableDataIndex(float* networkSpec)
{
    return (int)networkSpec[CUSTOM_VARIABLE_DATA_IDX];
}

static void setOutputErrorMask(float* rawData, float* mask)
{
	int outputLength = getLayerWidth(rawData, OUTPUT_LAYER_ID);
    for (int i = 0;i < outputLength;i++)
    {
        rawData[getOutputLayerMaskIndex(rawData) + i] = mask[i];
    }
}

static void setOutputErrorMaskFromTrainingSpec(float* networkSpec, TrainingSpecIndex index,  float* specBuffer)
{
	int outputWidth = getLayerWidth(networkSpec, OUTPUT_LAYER_ID);
    for (int i = 0;i < outputWidth;i++)
    {
        networkSpec[getOutputLayerMaskIndex(networkSpec) + i] = specBuffer[i + getErrorMaskIndex(index, 0)];
    }
}

static void clearOutputErrorMask(float* rawData)
{
    int width = getLayerWidth(rawData, OUTPUT_LAYER_ID);
    for (int i = 0;i < width;i++)
    {
        rawData[getOutputLayerMaskIndex(rawData) + i] = 1;
    }
}


/**
 * Copies the node state from one networkSpec to another that are the same node dimension
 * @param sourceNetworkSpec
 * @param targetNetworkSpec
 */
static void loadNodeStateFromOtherNetwork(float *sourceNetworkSpec, float* targetNetworkSpec)
{
    int feedforward_link_count_index = getForwardPassLinkIndex(sourceNetworkSpec);
    for (int i = LAYER_STATE_BASE_IDX;i<feedforward_link_count_index;i++)
    {
        targetNetworkSpec[i] = sourceNetworkSpec[i];
    }
}




// -.<>.- -.<>.- -.<>.- -.<>.- -.<>.- -.<>.- -.<>.- -.<>.- 
// 			Main Interface Functions (to be exposed to java)
// -.<>.- -.<>.- -.<>.- -.<>.- -.<>.- -.<>.- -.<>.- -.<>.- 


/**
 * Call this at the beginning of the training input sequence
 * @param networkSpec
 */
void resetNetworkToInitialState(float* networkSpec);

/**
 * Call this after creating a new network or when rerolling all weights
 * Basically, this, together with resetNetworkToInitialState creates a brand new random LSTM network
 * that is ready to be trained with new training sequences.  After this, you should call any of
 * setMaxInitialWeight
 * setMinInitialWeight
 * setRPROPInitialWeightDelta
 * setRPROPMaxWeightUpdateDelta
 * setRPROPMinWeightUpdateDelta
 * setRROPN_Minus
 * setRROPN_Plus
 * setLearningRate
 *
 * to reconfigure the network from its initial configuration from the builder
 * @param networkSpec

 */
void initializeAllWeights(float* networkSpec);

void setInputActivation(float* networkSpec, float* data);
void setInputActivationFromTrainingSpec(float* networkSpec, TrainingSpecIndex index,  float* specBuffer);

void forwardPassWithErrorMask(float* networkSpec, float* inputActivation, boolean applyErrorMaskToInput);
void forwardPass(float* networkSpec, float* inputActivation);

void getOutputActivation(float* networkSpec, float* outputBuffer);

/**
 * Use this to calculate the errors after each forward pass.  This should be called eventually after
 * EACH forward pass or else the error responsibility vectors will be wrong
 * @param networkSpec
 * @param targetOutput
 * @return
 */
float updateForwardPassErrors(float* networkSpec, float* targetOutput);


/**
 * Use this function to update the weights after calculating the gradients along each step of the sequence
 * This can be called after a batch of learning is complete.  Between each sequence, you should reset the
 * network state via resetNetworkToInitialState
 * @param networkSpec

 */
void updateWeightsFromErrors(float* networkSpec,  WeightUpdateType updateType);

float getOutputError(float* networkSpec, float* targetOutput);
float getRoundedOutputError(float* networkSpec, float* targetOutput);

extern void setDebugStatus(boolean enable);
// o - -- o o - -- o o - -- o o - -- o o - -- o o - -- o 
// Network creation functions (not exposed to Java)
// o - -- o o - -- o o - -- o o - -- o o - -- o o - -- o 


NetworkSpec getBaseStandardNetworkSpec(int numInputNodes, int numOutputNodes, int sizeMemoryCellLayer, int outputErrorFunctionId, int outputActivationFucntionId, WeightUpdateType updateType, boolean fillP);
void fillNetwork(NetworkSpec *spec);
void freeNetworkSpec(NetworkSpec spec);

// .............................................
// Network Training Functions (not to be exposed to Java)
// .............................................

TrainingSpecIndex getTrainingSpecIndex(float* networkSpec, int numSamples);

void forwardPassFromTraininingSpec(float* networkSpec, TrainingSpecIndex index, float* specBuffer);
float updateForwardPassErrorsFromTraininingSpec(float* networkSpec, TrainingSpecIndex index, float* specBuffer);


boolean verifyTrainingResult(TrainingSpecIndex index, float* trainingResults, float* trainingSpec);

float learnInputOutputPairMap(TrainingSpecIndex index, float* networkSpec, float* trainingSpec, int itemIndex, float* specBuffer);

void learnInputOutputPairMapWithDetails(TrainingSpecIndex index, float* networkSpec, float* trainingSpec, int itemIndex, float* specBuffer, float* resultBuffer);

int learnTrainingSpec(TrainingSpecIndex index, float* networkSpec, float* trainingSpec, int maxSteps, float convergenceThreshold, boolean allowWeightResetsP, WeightUpdateType updateType);

// o - -- o o - -- o o - -- o o - -- o o - -- o o - -- o 
// Network Training functions
// o - -- o o - -- o o - -- o o - -- o o - -- o o - -- o 


// This creates the buffer for the entire training spec
float* createTrainingSpecBuffer(TrainingSpecIndex trainingSpec);
void freeTrainingSpecBuffer(float* trainingSpecBuffer);

// save data to a record
void setTrainingSpecData(float* trainingSpec, TrainingSpecIndex index, int itemIndex, float* input, float* expectedOutput, float* errorMask, boolean skipMinAcceptabledErrorCheckP, boolean resetNetworkStateP, boolean useAverageErrorP);

// fill a buffer for a record
void fillTrainingSpecBuffer(float* trainingSpec, TrainingSpecIndex index, int itemIndex, float* specBuffer);

// This creates the buffer for an individual item in the training spec (mostly you should prefer fillTrainingSpecBuffer)
float* getTrainingSpecBufferForItem(float* trainingSpec, TrainingSpecIndex index, int itemIndex);
void freeTrainingSpecBufferForItem(float* buffer);


// Capped LSTM functions

int createCappedLSTM(float* networkSpec, float* startInput, float* stopInput);

int appendVectorToSequence(float* cappedNetworkSpec, float* startInput, float* stopInput, float* newInput, int maxSteps, float convergenceThreshold, boolean allowWeightResetsP, WeightUpdateType updateType);

void resetCappedLSTM(float* networkSpec, float* initialState, boolean onlyMetaDataP);

boolean isAtFinalState(float* networkSpec, float* finalState);


#endif