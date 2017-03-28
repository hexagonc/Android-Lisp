
#include <math.h>
#include "neural_library.h"
#include <stdio.h>

boolean DEBUG = true;

void setDebugStatus(boolean enable)
{
    DEBUG = enable;
}

WeightUpdateType updateTypeToInt(int update)
{
    switch (update)
    {
        case 0:
            return RPROP;
        case 1:
            return DEFAULT;
    }
    return DEFAULT;
}

/**
 * Negative values will be treated as having 0 weight
 * @param networkSpec
 * @param vectorOffset
 * @param length
 * @param minValue
 * @param failureOnAllZeroP
 * @return
 */
int sampleVectorIndexProportionally(float* networkSpec, int vectorOffset, int length, float minValue, boolean failureOnAllZeroP)
{
    float weightSum = 0;
    for (int i = 0; i < length;i++)
    {
        if (networkSpec[vectorOffset + i] >= minValue)
            weightSum+=networkSpec[vectorOffset + i];

    }

    float cutPoint = (float)randomLCG()*weightSum;

    for (int i=0;i < length;i++)
    {
        if (networkSpec[vectorOffset + i] < minValue)
            continue;
        if (networkSpec[vectorOffset + i] > cutPoint)
        {
            return i;
        }
        else
        {
            cutPoint-=networkSpec[vectorOffset + i];
        }
    }


    if (failureOnAllZeroP)
        return -1;
    else
        return (int)(randomLCG()*length);
}

// <*> <*> <*> <*> <*> <*> <*> <*> <*> <*> <*> <*> <*> 
// Internal functions
// <*> <*> <*> <*> <*> <*> <*> <*> <*> <*> <*> <*> <*> 
boolean vectorEquals(int length, float* lvalue, float* rvalue)
{
    for (int k = 0;k < length;k++ )
    {
        if (lvalue[k] != rvalue[k])
        	return false;
    }
    return true;
}

void arrayCopy(int length, float* source, float* target)
{
    for (int k = 0;k < length;k++ )
    {
        target[k] = source[k];
    }
    
}

void vectorCopy(int length, float* all_data, int source_vector_idx_offset, int target_vector_idx_offset)
{
    for (int k = 0;k < length;k++ )
    {
        all_data[target_vector_idx_offset + k] = all_data[source_vector_idx_offset + k];
    }
}

void vectorElementwiseMap(int length, float* all_data, int source_idx, int target_idx, unary_vector_function operation)
{
    for (int i = 0; i < length;i++)
    {

        all_data[target_idx + i] = operation(all_data, source_idx, length, i);
    }
}

void vectorElementwiseSet(int length, float* all_data, int vector_idx_offset, float value)
{
    for (int k = 0;k < length;k++ )
    {
        all_data[vector_idx_offset + k] = value;
    }
}

void matrixMultiplyByVector(int rows, int cols, float* all_data, int transform_matrix_id, int source_vector_idx, int target_vector_id, boolean incrementTargetVectorP)
{
    float value;
    float inputValue;
    float rowValue;
    

    for (int i = 0; i < rows;i++)
    {
        value = 0;
        for (int j = 0;j < cols;j++)
        {
            inputValue = all_data[source_vector_idx + j];
            rowValue = all_data[transform_matrix_id + getMatrixCellIndex(cols, i, j)];
            value +=rowValue*inputValue;
        }


        if (incrementTargetVectorP)
            all_data[target_vector_id + i] += value;
        else
            all_data[target_vector_id + i] = value;
    }
}

void setMatrixValue(int cols, float* all_data, int maxtrix_offset_index, int i, int j, float value)
{
    int offsetIndex = getMatrixCellIndex(cols, i, j);
    all_data[maxtrix_offset_index + offsetIndex] = value;
}

void matrixElementwiseSet(int rows, int cols, float* all_data, int matrix_offset_index, float value)
{
    for (int i = 0;i < rows*cols;i++)
    {
        all_data[matrix_offset_index + i] = value;
    }
}

void matrixElementwiseRandomize(int rows, int cols, float* all_data, int matrix_offset_index, float minValue, float maxValue)
{
	double ran = 0;
    for (int i = 0;i < rows*cols;i++)
    {
		ran = randomLCG();

        all_data[matrix_offset_index + i] = (float)(minValue + (maxValue - minValue)*(float)ran);
    }
}

void updateCellOutputState(float* networkSpec)
{
    // calculate cell activation, then update the celloutput activation, multiplying by the input gate
    int outputGateActivationIdx = getLayerActivationIndex(networkSpec, OUTPUT_GATE_LAYER_ID);

    int peepholeActivationIdx = getLayerActivationIndex(networkSpec, PEEPHOLE_LAYER_ID);
    int cellOutputWidth = getLayerWidth(networkSpec, CELL_OUTPUT_LAYER_ID);
    int cellOutputActivationIdx = getLayerActivationIndex(networkSpec, CELL_OUTPUT_LAYER_ID);


    for (int i = 0;i < cellOutputWidth;i++)
    {
        networkSpec[cellOutputActivationIdx + i] = networkSpec[outputGateActivationIdx + i]*networkSpec[peepholeActivationIdx + i];
    }
}


void updateMemoryCellState(float* networkSpec)
{

    int peephole_net_input_idx = getLayerNetInputIndex(networkSpec, PEEPHOLE_LAYER_ID);
    int forget_gate_activation_idx = getLayerActivationIndex(networkSpec, FORGET_LAYER_ID);
    int cell_input_activation_idx = getLayerActivationIndex(networkSpec, CELL_INPUT_LAYER_ID);
    int input_gate_activation_idx = getLayerActivationIndex(networkSpec, INPUT_GATE_LAYER_ID);
    int peephole_layer_width = getLayerWidth(networkSpec, PEEPHOLE_LAYER_ID);
    int peepholeActivationIdx = getLayerActivationIndex(networkSpec, PEEPHOLE_LAYER_ID);
    unary_vector_function activation = getLayerActivationFunction(networkSpec, PEEPHOLE_LAYER_ID);
    for (int i = 0;i < peephole_layer_width;i++)
    {

        networkSpec[peephole_net_input_idx + i] =
                networkSpec[peephole_net_input_idx + i] * networkSpec[forget_gate_activation_idx + i] +
                networkSpec[cell_input_activation_idx + i]*networkSpec[input_gate_activation_idx + i];

        // precalculating the cell state (peephole net activation)
        networkSpec[peepholeActivationIdx + i] = activation(networkSpec, peephole_net_input_idx, peephole_layer_width, i);
    }
}

void pushNetInput(float* networkSpec, int layerId)
{
    int layer_activation_idx = getLayerActivationIndex(networkSpec, layerId);
    int layer_partial_net_idx = getLayerPartialNetInputIndex(networkSpec, layerId);
    int layer_net_input_idx = getLayerNetInputIndex(networkSpec, layerId);

    int layerWidth = getLayerWidth(networkSpec, layerId);

    /*
    if (DEBUG)
    	debug("PushNetInput: layerId - [%d]  layer_activation_idx - [%d] layer_partial_net_idx - [%d] layer_net_input_idx - [%d] layerWidth - [%d]\n", layerId, layer_activation_idx, layer_partial_net_idx, layer_net_input_idx, layerWidth);
    */
    unary_vector_function activationFunction = getLayerActivationFunction(networkSpec, layerId);
    switch (layerId)
    {
        case PEEPHOLE_LAYER_ID: // do nothing, peephole can't be the target of external links
            break;
        case OUTPUT_GATE_LAYER_ID:
            vectorCopy(layerWidth, networkSpec, layer_partial_net_idx, layer_net_input_idx);
            vectorElementwiseSet(layerWidth, networkSpec, layer_partial_net_idx, 0);
            vectorElementwiseMap(layerWidth, networkSpec, layer_net_input_idx, layer_activation_idx, activationFunction);
            updateCellOutputState(networkSpec);
            break;
        case INPUT_GATE_LAYER_ID:
            vectorCopy(layerWidth, networkSpec, layer_partial_net_idx, layer_net_input_idx);
            vectorElementwiseSet(layerWidth, networkSpec, layer_partial_net_idx, 0);
            vectorElementwiseMap(layerWidth, networkSpec, layer_net_input_idx, layer_activation_idx, activationFunction);
            updateMemoryCellState(networkSpec);
            break;
        default:
            vectorCopy(layerWidth, networkSpec, layer_partial_net_idx, layer_net_input_idx);
            vectorElementwiseSet(layerWidth, networkSpec, layer_partial_net_idx, 0);
            vectorElementwiseMap(layerWidth, networkSpec, layer_net_input_idx, layer_activation_idx, activationFunction);
    }
}

void setErrorResponsibility(float* networkSpec, int layerId, float* targetOutput)
{
    int layer_width = getLayerWidth(networkSpec, layerId);

    int layer_error_responsibility_idx = getLayerErrorResponsibilityIndex(networkSpec, layerId);
    int memory_cell_state_idx = getLayerNetInputIndex(networkSpec, PEEPHOLE_LAYER_ID);
    int memory_cell_state_squashed_idx = getLayerActivationFunctionId(networkSpec, PEEPHOLE_LAYER_ID);
    int net_input_idx = getLayerNetInputIndex(networkSpec, layerId);
    unary_vector_function activationSquashedDerivative = getLayerActivationFunctionDerivative(networkSpec, layerId);
    int output_gate_activation_idx;
    OutputErrorFunction errorFunction;
    unary_vector_function outputActivationPrime;
    float outputError;
    unary_vector_function memory_cell_squashed_derivative;
    switch (layerId)
    {
        case OUTPUT_LAYER_ID: //
            {
		        errorFunction = ERROR_FUNCTION_MAP[(int)networkSpec[OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX]];

		        outputActivationPrime = getLayerActivationFunctionDerivative(networkSpec, OUTPUT_LAYER_ID);

		        for (int i = 0;i < layer_width;i++)
		        {
		            networkSpec[layer_error_responsibility_idx + i] =
		                    (float)(errorFunction.errorPartialDerivative(networkSpec, targetOutput, i)*outputActivationPrime(networkSpec, net_input_idx, layer_width, i));
		        }
		        setErrorResponsibility(networkSpec, CELL_OUTPUT_LAYER_ID, targetOutput);
		    }
            break;
        case INPUT_GATE_LAYER_ID: //
        case CELL_INPUT_LAYER_ID:
        case FORGET_LAYER_ID:
        	{
	            output_gate_activation_idx = getLayerActivationIndex(networkSpec, OUTPUT_GATE_LAYER_ID);
	            memory_cell_squashed_derivative = getLayerActivationFunctionDerivative(networkSpec, PEEPHOLE_LAYER_ID);
	            int memory_cell_width = getLayerWidth(networkSpec, PEEPHOLE_LAYER_ID);
	            for (int i=0;i<layer_width;i++)
	            {
	                outputError = getOutputLinkErrorPartialDerivative(networkSpec, i);
	                networkSpec[layer_error_responsibility_idx + i] =
	                        networkSpec[output_gate_activation_idx + i]*memory_cell_squashed_derivative(networkSpec, memory_cell_state_idx, memory_cell_width, i)*outputError;
	            }
	        }
            break;
        case OUTPUT_GATE_LAYER_ID: //
            for (int i=0;i<layer_width;i++)
            {
                outputError = getOutputLinkErrorPartialDerivative(networkSpec, i);
                networkSpec[layer_error_responsibility_idx + i] = activationSquashedDerivative(networkSpec, net_input_idx, layer_width,i) *
                        networkSpec[memory_cell_state_squashed_idx + i]*outputError;
            }
            break;
        case CELL_OUTPUT_LAYER_ID: //
        	{
	            int output_layer_error_responsibility_idx; 
	            output_layer_error_responsibility_idx= getLayerErrorResponsibilityIndex(networkSpec, OUTPUT_LAYER_ID);
	            int output_layer_width = getLayerWidth(networkSpec, OUTPUT_LAYER_ID);
	            int linkId = getLinkId(CELL_OUTPUT_LAYER_ID, OUTPUT_LAYER_ID);
	            int output_link_data_idx = getLinkDataIndex(networkSpec, linkId);

	            int output_link_weight_idx = getLinkWeightIndex(output_layer_width, layer_width, output_link_data_idx);
	            float error = 0;
	            int output_layer_mask_id = getOutputLayerMaskIndex(networkSpec);
	            for (int j = 0; j < layer_width;j++)
	            {
	                error = 0;
	                for (int k = 0; k < output_layer_width;k++)
	                {
	                    error += networkSpec[output_layer_mask_id + k]*getMatrixValue(layer_width, networkSpec, output_link_weight_idx, k, j)*networkSpec[output_layer_error_responsibility_idx + k];
	                }
	                networkSpec[layer_error_responsibility_idx + j] = error;
	            }
	        }
            break;
    }
}

void feedforward(float* networkSpec, int linkId)
{
    int sourceLayerId = getLinkSourceLayerId(networkSpec, linkId);
    int targetLayerId = getLinkTargetLayerId(networkSpec, linkId);

    int link_data_idx = getLinkDataIndex(networkSpec, linkId);

    int sourceLayerWidth = getLayerWidth(networkSpec, sourceLayerId);
    int targetLayerWidth = getLayerWidth(networkSpec, targetLayerId);
    int linkWeightMatrixIdx = getLinkWeightIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);
    int sourceNodeStateActivation_idx = getLayerActivationIndex(networkSpec, sourceLayerId);
    int targetPartialNetInput_idx = getLayerPartialNetInputIndex(networkSpec, targetLayerId);

    /*
    if (DEBUG)
    {
    	debug("Feedforward: sourceLayerId - [%d] targetLayerId - [%d] link_data_idx - [%d] sourceLayerWidth - [%d], targetLayerWidth - [%d], linkWeightMatrixIdx - [%d], sourceNodeStateActivation_idx - [%d], targetPartialNetInput_idx - [%d]", sourceLayerId, targetLayerId, link_data_idx, sourceLayerWidth, targetLayerWidth, linkWeightMatrixIdx, sourceNodeStateActivation_idx, targetPartialNetInput_idx);	
    }
    */

    matrixMultiplyByVector(targetLayerWidth, sourceLayerWidth, networkSpec, linkWeightMatrixIdx, sourceNodeStateActivation_idx, targetPartialNetInput_idx, true);
}

void updateElligibilityTrace(float* networkSpec, int linkId)
{
    int link_data_idx = getLinkDataIndex(networkSpec, linkId);
    int source_layer_id = (int)networkSpec[getLinkSourceLayerIdIdx(link_data_idx)];
    int target_layer_id = (int)networkSpec[getLinkTargetLayerIdIdx(link_data_idx)];

    int sourceLayerWidth = getLayerWidth(networkSpec, source_layer_id);
    int targetLayerWidth = getLayerWidth(networkSpec, target_layer_id);
    int source_activation_idx = getLayerActivationIndex(networkSpec, source_layer_id);
    int eligibilityTrace_idx = getLinkElligibilityTraceIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);
    int forget_gate_activation_idx = getLayerActivationIndex(networkSpec, FORGET_LAYER_ID);
    int target_net_input_idx = getLayerNetInputIndex(networkSpec, target_layer_id);
    float sourceActivation;
    float previousTraceCellValue, newTraceValue;

    int peephole_net_input_idx;

    switch (target_layer_id)
    {
        case OUTPUT_LAYER_ID:
        case OUTPUT_GATE_LAYER_ID: // + reviewed +
            for (int i = 0; i < targetLayerWidth; i++)
            {
                for (int j = 0; j < sourceLayerWidth;j++)
                {
                    sourceActivation = networkSpec[source_activation_idx + j];
                    setMatrixValue(sourceLayerWidth, networkSpec, eligibilityTrace_idx, i, j, sourceActivation);
                }
            }

            break;
        case CELL_INPUT_LAYER_ID: // + reviewed +
        	{
	            int input_gate_activation_idx = getLayerActivationIndex(networkSpec, INPUT_GATE_LAYER_ID);
	            unary_vector_function squashedCellInputDerivative = getLayerActivationFunctionDerivative(networkSpec, CELL_INPUT_LAYER_ID);
	            for (int i = 0; i < targetLayerWidth; i++)
	            {
	                for (int j = 0; j < sourceLayerWidth;j++)
	                {
	                    sourceActivation = networkSpec[source_activation_idx + j];
	                    // can set the squashing function of the cell input to the identity function to simplify targetNetInputSquashed to 1
	                    float targetNetInputSquashed = squashedCellInputDerivative(networkSpec, target_net_input_idx, targetLayerWidth, i);

	                    previousTraceCellValue = getMatrixValue(sourceLayerWidth, networkSpec, eligibilityTrace_idx, i, j);

	                    newTraceValue = previousTraceCellValue * getVectorValue(networkSpec, forget_gate_activation_idx, i) +
	                            getVectorValue(networkSpec, input_gate_activation_idx, i)*targetNetInputSquashed*sourceActivation;

	                    setMatrixValue(sourceLayerWidth, networkSpec, eligibilityTrace_idx, i, j, newTraceValue);
	                }
	            }
	        }
            break;
        case INPUT_GATE_LAYER_ID: // + reviewed +
        	{
	            unary_vector_function targetActivationDerivative = getLayerActivationFunctionDerivative(networkSpec, target_layer_id);
	            int cell_input_activation_idx = getLayerActivationIndex(networkSpec, CELL_INPUT_LAYER_ID);
	            for (int i = 0; i < targetLayerWidth; i++)
	            {
	                for (int j = 0; j < sourceLayerWidth;j++)
	                {
	                    sourceActivation = networkSpec[source_activation_idx + j];

	                    previousTraceCellValue = getMatrixValue(sourceLayerWidth, networkSpec, eligibilityTrace_idx, i, j);

	                    newTraceValue = previousTraceCellValue * getVectorValue(networkSpec, forget_gate_activation_idx, i) +
	                            getVectorValue(networkSpec, cell_input_activation_idx, i) * targetActivationDerivative(networkSpec, target_net_input_idx, targetLayerWidth, i)*sourceActivation;

	                    setMatrixValue(sourceLayerWidth, networkSpec, eligibilityTrace_idx, i, j, newTraceValue);
	                }
	            }
	        }
            break;
        case FORGET_LAYER_ID: // + reviewed +
        	{
	            peephole_net_input_idx = getLayerNetInputIndex(networkSpec, PEEPHOLE_LAYER_ID);

	            unary_vector_function forgetGetSquashedDerivative = getLayerActivationFunctionDerivative(networkSpec, FORGET_LAYER_ID);
	            float forgetSquashedPrime;
	            for (int i = 0; i < targetLayerWidth; i++)
	            {
	                for (int j = 0; j < sourceLayerWidth;j++)
	                {
	                    sourceActivation = networkSpec[source_activation_idx + j];

	                    forgetSquashedPrime = forgetGetSquashedDerivative(networkSpec, target_net_input_idx, targetLayerWidth, i);

	                    previousTraceCellValue = getMatrixValue(sourceLayerWidth, networkSpec, eligibilityTrace_idx, i, j);

	                    newTraceValue = previousTraceCellValue * getVectorValue(networkSpec, forget_gate_activation_idx, i) +
	                            getVectorValue(networkSpec, peephole_net_input_idx, i)*forgetSquashedPrime*sourceActivation;

	                    setMatrixValue(sourceLayerWidth, networkSpec, eligibilityTrace_idx, i, j, newTraceValue);
	                }
	            }
	        }
            break;

    }
}


void updatePartialGradient(float* networkSpec, int linkId, float* targetOutput)
{

    updateElligibilityTrace(networkSpec, linkId);

    int link_data_idx = getLinkDataIndex(networkSpec, linkId);
    int source_layer_id = (int)networkSpec[getLinkSourceLayerIdIdx(link_data_idx)];
    int target_layer_id = (int)networkSpec[getLinkTargetLayerIdIdx(link_data_idx)];
    int sourceLayerWidth = getLayerWidth(networkSpec, source_layer_id);
    int targetLayerWidth = getLayerWidth(networkSpec, target_layer_id);

    int calculated_gradient_idx = getLinkCalculatedGradientIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);
    int target_error_responsibility_idx = getLayerErrorResponsibilityIndex(networkSpec, target_layer_id);
    int eligibility_trace_idx = getLinkElligibilityTraceIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);
    float negativeDerivative;
    float priorCalculatedGradient;

    setErrorResponsibility(networkSpec, target_layer_id, targetOutput);

    for (int i = 0; i < targetLayerWidth;i++)
    {
        for (int j = 0; j < sourceLayerWidth;j++)
        {
            negativeDerivative = getMatrixValue(sourceLayerWidth, networkSpec, eligibility_trace_idx, i, j)*getVectorValue(networkSpec, target_error_responsibility_idx, i);
            priorCalculatedGradient = getMatrixValue(sourceLayerWidth, networkSpec, calculated_gradient_idx, i, j);
            setMatrixValue(sourceLayerWidth, networkSpec, calculated_gradient_idx, i, j, priorCalculatedGradient - negativeDerivative);
        }
    }
}

void updateWeights(float* networkSpec, int linkId,  WeightUpdateType updateType)
{
    float n_minus = getRROPN_Minus(networkSpec);
    float n_plus = getRROPN_Plus(networkSpec);
    float deltaMax = getRPROPMaxWeightUpdateDelta(networkSpec);
    float deltaMin = getRPROPMinWeightUpdateDelta(networkSpec);
    switch (updateType)
    {
        case RPROP:
        {
            int link_data_idx = getLinkDataIndex(networkSpec, linkId);
            int source_layer_id = (int)networkSpec[getLinkSourceLayerIdIdx(link_data_idx)];
            int target_layer_id = (int)networkSpec[getLinkTargetLayerIdIdx(link_data_idx)];
            int sourceLayerWidth = getLayerWidth(networkSpec, source_layer_id);
            int targetLayerWidth = getLayerWidth(networkSpec, target_layer_id);

            int calculated_gradient_idx = getLinkCalculatedGradientIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);
            int weight_matrix_idx = getLinkWeightIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);
            int weight_delta_matrix_idx = getLinkWeightDeltaIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);
            int prevCalculated_gradient_idx = getLinkPrevCalculatedGradientIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);

            float prevDelta, prevWeight, newWeight, prevCalculatedGradient, calculatedGradient, value, newDelta;
            for (int i = 0; i < targetLayerWidth;i++)
            {
                for (int j = 0; j < sourceLayerWidth;j++)
                {
                    prevDelta = getMatrixValue(sourceLayerWidth, networkSpec, weight_delta_matrix_idx, i, j);
                    prevCalculatedGradient = getMatrixValue(sourceLayerWidth, networkSpec, prevCalculated_gradient_idx, i, j);
                    calculatedGradient = getMatrixValue(sourceLayerWidth, networkSpec, calculated_gradient_idx, i, j);

                    if (prevCalculatedGradient*calculatedGradient > 0)
                        value = min(prevDelta*n_plus, deltaMax);
                    else if (prevCalculatedGradient*calculatedGradient < 0)
                    {
                        setMatrixValue(sourceLayerWidth, networkSpec, calculated_gradient_idx, i, j, 0);
                        value = max(prevDelta*n_minus, deltaMin);
                    }
                    else
                        value = prevDelta;

                    setMatrixValue(sourceLayerWidth, networkSpec, weight_delta_matrix_idx, i, j, value);
                    setMatrixValue(sourceLayerWidth, networkSpec, prevCalculated_gradient_idx, i, j, calculatedGradient);
                }
            }


            for (int i = 0; i < targetLayerWidth;i++)
            {
                for (int j = 0; j < sourceLayerWidth;j++)
                {
                    prevWeight = getMatrixValue(sourceLayerWidth, networkSpec, weight_matrix_idx, i, j);
                    calculatedGradient = getMatrixValue(sourceLayerWidth, networkSpec, calculated_gradient_idx, i, j);
                    newDelta = getMatrixValue(sourceLayerWidth, networkSpec, weight_delta_matrix_idx, i, j);
                    newWeight = prevWeight - sign(calculatedGradient)*newDelta;

                    setMatrixValue(sourceLayerWidth, networkSpec, weight_matrix_idx, i, j, newWeight);
                    setMatrixValue(sourceLayerWidth, networkSpec, calculated_gradient_idx, i, j, 0); // clear calculated gradient
                }
            }
        }
        break;
        case DEFAULT:
        {
            int link_data_idx = getLinkDataIndex(networkSpec, linkId);
            int source_layer_id = (int)networkSpec[getLinkSourceLayerIdIdx(link_data_idx)];
            int target_layer_id = (int)networkSpec[getLinkTargetLayerIdIdx(link_data_idx)];
            int sourceLayerWidth = getLayerWidth(networkSpec, source_layer_id);
            int targetLayerWidth = getLayerWidth(networkSpec, target_layer_id);

            int calculated_gradient_idx = getLinkCalculatedGradientIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);
            int weight_matrix_idx = getLinkWeightIndex(targetLayerWidth, sourceLayerWidth, link_data_idx);

            float learningRate = getLearningRate(networkSpec);
            for (int i = 0; i < targetLayerWidth;i++)
            {
                for (int j = 0;j<sourceLayerWidth;j++)
                {
                    float gradient = getMatrixValue(sourceLayerWidth, networkSpec, calculated_gradient_idx, i, j);
                    float prevWeight = getMatrixValue(sourceLayerWidth, networkSpec, weight_matrix_idx, i, j);
                    // doing gradient descent so we move in the direction of - ∂E/∂W_i_j
                    setMatrixValue(sourceLayerWidth, networkSpec, weight_matrix_idx, i, j, prevWeight - learningRate*gradient);
                    setMatrixValue(sourceLayerWidth, networkSpec, calculated_gradient_idx, i, j, 0); // clear calculated gradient
                }
            }

        }

    }

}

void resetLayerNodeState(float* networkSpec, int layerId)
{
    int layer_activation_idx = getLayerActivationIndex(networkSpec, layerId);
    int layer_partial_net_idx = getLayerPartialNetInputIndex(networkSpec, layerId);

    int layerWidth = getLayerWidth(networkSpec, layerId);

    switch (layerId)
    {
        // initialize all gates as open initially.
        case INPUT_GATE_LAYER_ID:
        case FORGET_LAYER_ID:
        case OUTPUT_GATE_LAYER_ID:
            vectorElementwiseSet(layerWidth, networkSpec, layer_partial_net_idx, 0);
            vectorElementwiseSet(layerWidth, networkSpec, layer_activation_idx, 1);
            break;
        case CELL_OUTPUT_LAYER_ID:
        case CELL_INPUT_LAYER_ID:
            vectorElementwiseSet(layerWidth, networkSpec, layer_partial_net_idx, 0);
            pushNetInput(networkSpec, layerId);
            break;
        case PEEPHOLE_LAYER_ID:
        	{
	            int net_input_idx = getLayerNetInputIndex(networkSpec, layerId);
	            vectorElementwiseSet(layerWidth, networkSpec, net_input_idx, 0);
	            unary_vector_function function = getLayerActivationFunction(networkSpec, layerId);
	            vectorElementwiseMap(layerWidth, networkSpec, net_input_idx, layer_activation_idx, function);
	        }
            break;
    }
}

void resetMemoryCellState(float* networkSpec)
{
    int layerIds[] = {CELL_INPUT_LAYER_ID, CELL_OUTPUT_LAYER_ID, PEEPHOLE_LAYER_ID, INPUT_GATE_LAYER_ID, OUTPUT_GATE_LAYER_ID};
    int length = 5;
    for (int i = 0;i < length;i++)
    {
        resetLayerNodeState(networkSpec, layerIds[i]);
    }
}

void clearLinkWeightHistory(float* networkSpec, int linkId)
{
    int linkDataIdx = getLinkDataIndex(networkSpec, linkId);
    int sourceLayerWidth = getLinkSourceLayerWidth(networkSpec, linkId);
    int targetLayerWidth = getLinkTargetLayerWidth(networkSpec, linkId);
    int elligibilityTraceIdx = getLinkElligibilityTraceIndex(targetLayerWidth, sourceLayerWidth, linkDataIdx);

    matrixElementwiseSet(targetLayerWidth, sourceLayerWidth, networkSpec, elligibilityTraceIdx, 0);
    // why not reset prevCalculated gradient and previous weight delta?
    // answer: because this happens when you update the weights in updateWeightsFromErrors

}

void initializeLink(float* networkSpec, int linkId, float initialDelta, float minWeight, float maxWeight)
{
    int linkDataIdx = getLinkDataIndex(networkSpec, linkId);
    int sourceLayerWidth = getLinkSourceLayerWidth(networkSpec, linkId);
    int targetLayerWidth = getLinkTargetLayerWidth(networkSpec, linkId);

    int weight_idx = getLinkWeightIndex(targetLayerWidth, sourceLayerWidth, linkDataIdx);
    int weight_delta_idx = getLinkWeightDeltaIndex(targetLayerWidth, sourceLayerWidth, linkDataIdx);
    int calculated_gradient_idx = getLinkCalculatedGradientIndex(targetLayerWidth, sourceLayerWidth, linkDataIdx);
    int prev_calculated_gradient_idx = getLinkPrevCalculatedGradientIndex(targetLayerWidth, sourceLayerWidth, linkDataIdx);
    int elligibilityTraceIdx = getLinkElligibilityTraceIndex(targetLayerWidth, sourceLayerWidth, linkDataIdx);


    matrixElementwiseRandomize(targetLayerWidth, sourceLayerWidth, networkSpec, weight_idx, minWeight, maxWeight);

    matrixElementwiseSet(targetLayerWidth, sourceLayerWidth, networkSpec, elligibilityTraceIdx, 0);

    matrixElementwiseSet(targetLayerWidth, sourceLayerWidth, networkSpec, calculated_gradient_idx, 0);
    matrixElementwiseSet(targetLayerWidth, sourceLayerWidth, networkSpec, prev_calculated_gradient_idx, 0);
    matrixElementwiseSet(targetLayerWidth, sourceLayerWidth, networkSpec, weight_delta_idx, initialDelta);
}

void resetWeightHistory(float* networkSpec)
{
    int backprop_link_count_idx = getBackwardPassLinkIndex(networkSpec);
    int linkIdIdx = 0;
    int linkId;

    int numBackwardPassLinks = (int)networkSpec[backprop_link_count_idx];
    int update_gradient_link_order = backprop_link_count_idx + 1;
    for (linkIdIdx = 0; linkIdIdx < numBackwardPassLinks; linkIdIdx++ )
    {
        linkId = (int)networkSpec[update_gradient_link_order + linkIdIdx];
        clearLinkWeightHistory(networkSpec, linkId);
    }
}


// -.<>.- -.<>.- -.<>.- -.<>.- -.<>.- -.<>.- -.<>.- -.<>.- 
// 			Main Interface Functions
// -.<>.- -.<>.- -.<>.- -.<>.- -.<>.- -.<>.- -.<>.- -.<>.- 

void copyNetwork(float* baseNetwork, float* targetNetwork)
{
	int networkLength = baseNetwork[NETWORK_LENGTH_IDX];
	for (int i = 0; i < networkLength; i++)
    {
    	targetNetwork[i] = baseNetwork[i];
    }
}

void getOutputActivation(float* networkSpec, float* outputBuffer)
{
    int layerWidth = getLayerWidth(networkSpec, OUTPUT_LAYER_ID);
    int output_activation_idx = getLayerActivationIndex(networkSpec, OUTPUT_LAYER_ID);
    
    for (int i =0;i < layerWidth;i++)
        outputBuffer[i] = networkSpec[output_activation_idx + i];
}


void setInputActivation(float* networkSpec, float* data)
{
    int layerWidth = getLayerWidth(networkSpec, INPUT_LAYER_ID);
    int input_activation_idx = getLayerActivationIndex(networkSpec, INPUT_LAYER_ID);
    for (int i =0;i < layerWidth;i++)
        networkSpec[input_activation_idx + i] = data[i];
}

void setInputActivationFromTrainingSpec(float* networkSpec, TrainingSpecIndex index,  float* specBuffer)
{
    int layerWidth = getLayerWidth(networkSpec, INPUT_LAYER_ID);
    int input_activation_idx = getLayerActivationIndex(networkSpec, INPUT_LAYER_ID);
    for (int i =0;i < layerWidth;i++)
        networkSpec[input_activation_idx + i] = specBuffer[getInputIndex(index, 0) + i];
}


/**
 * Call this after setting the initial input activation
 * @param networkSpec

 */
void forwardPass(float* networkSpec, float* inputActivation)
{
    int feedforward_link_count_idx = getForwardPassLinkIndex(networkSpec);
    setInputActivation(networkSpec, inputActivation);
    int linkIdIdx = 0;
    int linkId;

    int numFeedforwardLinks = (int)networkSpec[feedforward_link_count_idx];
    int feedforward_order_link_idx = feedforward_link_count_idx + 1;
    for (linkIdIdx = 0; linkIdIdx < numFeedforwardLinks; linkIdIdx++ )
    {
        linkId = (int)networkSpec[feedforward_order_link_idx + linkIdIdx];
        //debug("Forward pass linkId: %d\n", linkId);
        if (linkId > 0)
        // techically, 0 is a valid linkId but since linkId of 0 is a recurrent link from the input layer to itself,
        // you would never call feedforward on it
        {

            feedforward(networkSpec, linkId);
        }
        else
        {
            // In this case, -linkId is the target layer id whose node state should be finalized (move partial
            // net input to the net input and calculate the overall node activation
            //debug("Push net input pass linkId: %d\n", linkId);
            pushNetInput(networkSpec, (-1 * linkId));
        }
    }
}

/**
 * Call this after setting the initial input activation
 * @param networkSpec

 */
void forwardPassFromTraininingSpec(float* networkSpec, TrainingSpecIndex index, float* specBuffer)
{
    int feedforward_link_count_idx = getForwardPassLinkIndex(networkSpec);

    // TODO - handle case of 
    /*
    if (specBuffer[getHasErrorMaskIndex(index, 0)] == true)
    {
        int maskIndex = getOutputLayerMaskIndex(networkSpec);
        for (int i = 0; i < inputActivation.length;i++)
        {
            float mask = networkSpec[maskIndex + i];
            inputActivation[i]*=mask;
        }
    }
    */
    setInputActivationFromTrainingSpec(networkSpec, index, specBuffer);

    int linkIdIdx = 0;
    int linkId;

    int numFeedforwardLinks = (int)networkSpec[feedforward_link_count_idx];
    int feedforward_order_link_idx = feedforward_link_count_idx + 1;
    for (linkIdIdx = 0; linkIdIdx < numFeedforwardLinks; linkIdIdx++ )
    {
        linkId = (int)networkSpec[feedforward_order_link_idx + linkIdIdx];
        if (linkId > 0)
        // techically, 0 is a valid linkId but since linkId of 0 is a recurrent link from the input layer to itself,
        // you would never call feedforward on it
        {

            feedforward(networkSpec, linkId);
        }
        else
        {
            // In this case, -linkId is the target layer id whose node state should be finalized (move partial
            // net input to the net input and calculate the overall node activation
            pushNetInput(networkSpec, (-1 * linkId));
        }
    }
}


/**
 * Use this function to update the weights after calculating the gradients along each step of the sequence
 * This can be called after a batch of learning is complete.  Between each sequence, you should reset the
 * network state via resetNetworkToInitialState
 * @param networkSpec

 */
void updateWeightsFromErrors(float* networkSpec,  WeightUpdateType updateType)
{
    int backprop_link_count_idx = getBackwardPassLinkIndex(networkSpec);
    int linkIdIdx = 0;
    int linkId;

    int numBackwardPassLinks = (int)networkSpec[backprop_link_count_idx];
    int update_gradient_link_order = backprop_link_count_idx + 1;
    for (linkIdIdx = 0; linkIdIdx < numBackwardPassLinks; linkIdIdx++ )
    {
        linkId = (int)networkSpec[update_gradient_link_order + linkIdIdx];
        updateWeights(networkSpec, linkId, updateType);
    }

}


/**
 * Use this to calculate the errors after each forward pass.  This should be called eventually after
 * EACH forward pass or else the error responsibility vectors will be wrong
 * @param networkSpec
 * @param targetOutput
 * @return
 */
float updateForwardPassErrors(float* networkSpec, float* targetOutput)
{
    int backprop_link_count_idx = getBackwardPassLinkIndex(networkSpec);
    int linkIdIdx = 0;
    int linkId;

    int numBackwardPassLinks = (int)networkSpec[backprop_link_count_idx];
    int update_gradient_link_order = backprop_link_count_idx + 1;
    for (linkIdIdx = 0; linkIdIdx < numBackwardPassLinks; linkIdIdx++ )
    {
        linkId = (int)networkSpec[update_gradient_link_order + linkIdIdx];
        updatePartialGradient(networkSpec, linkId, targetOutput);
    }

    OutputErrorFunction outError = ERROR_FUNCTION_MAP[(int)networkSpec[OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX]];
    return (float)outError.error(networkSpec, targetOutput);
}

/**
 * Use this to calculate the errors after each forward pass.  This should be called eventually after
 * EACH forward pass or else the error responsibility vectors will be wrong
 * @param networkSpec
 * @param targetOutput
 * @return
 */
float updateForwardPassErrorsFromTraininingSpec(float* networkSpec, TrainingSpecIndex index, float* specBuffer)
{
    int backprop_link_count_idx = getBackwardPassLinkIndex(networkSpec);
    int linkIdIdx = 0;
    int linkId;

    int numBackwardPassLinks = (int)networkSpec[backprop_link_count_idx];
    int update_gradient_link_order = backprop_link_count_idx + 1;

    int outputWidth = getLayerWidth(networkSpec, OUTPUT_LAYER_ID);
    float targetOutput[outputWidth];
    for (int i = 0; i < outputWidth; i++)
    {
    	targetOutput[i] = specBuffer[getExpectedOutputIndex(index, 0) + i];
    }

    for (linkIdIdx = 0; linkIdIdx < numBackwardPassLinks; linkIdIdx++ )
    {
        linkId = (int)networkSpec[update_gradient_link_order + linkIdIdx];
        updatePartialGradient(networkSpec, linkId, targetOutput);
    }

    
    OutputErrorFunction outError = ERROR_FUNCTION_MAP[(int)networkSpec[OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX]];
    return (float)outError.error(networkSpec, targetOutput);
}


float getOutputError(float* networkSpec, float* targetOutput)
{
    OutputErrorFunction outError = ERROR_FUNCTION_MAP[(int)networkSpec[OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX]];
    return (float)outError.error(networkSpec, targetOutput);
}

float getRoundedOutputError(float* networkSpec, float* targetOutput)
{
    int idx = getLayerActivationIndex(networkSpec, OUTPUT_LAYER_ID);
    int width = getLayerWidth(networkSpec, OUTPUT_LAYER_ID);
    float prior;
    for (int i = idx; i < width + idx; i++)
    {
        prior = networkSpec[i];
        networkSpec[i] = roundToInt(prior);
    }
    OutputErrorFunction outError = ERROR_FUNCTION_MAP[(int)networkSpec[OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX]];
    return (float)outError.error(networkSpec, targetOutput);
}


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
void initializeAllWeights(float* networkSpec)
{
    int backprop_link_count_idx = getBackwardPassLinkIndex(networkSpec);
    float initialDelta  = getRPROPInitialWeightDelta(networkSpec);
    float minWeight = getMinInitialWeight(networkSpec);
    float maxWeight = getMaxInitialWeight(networkSpec);
    int linkIdIdx = 0;
    int linkId;
    int numBackwardPassLinks = (int)networkSpec[backprop_link_count_idx];
    int update_gradient_link_order = backprop_link_count_idx + 1;
    for (linkIdIdx = 0; linkIdIdx < numBackwardPassLinks; linkIdIdx++ )
    {
        linkId = (int)networkSpec[update_gradient_link_order + linkIdIdx];
        initializeLink(networkSpec, linkId, initialDelta, minWeight, maxWeight);
    }
}

/**
 * Call this at the beginning of the training input sequence
 * @param networkSpec
 */
void resetNetworkToInitialState(float* networkSpec)
{
    //printVector(networkSpec, (int)networkSpec[NETWORK_LENGTH_IDX], true);
    resetWeightHistory(networkSpec);
    initializeNodeState(networkSpec);
}



// o - -- o o - -- o o - -- o o - -- o o - -- o o - -- o 
// Network creation functions
// o - -- o o - -- o o - -- o o - -- o o - -- o o - -- o 

TrainingSpecIndex getTrainingSpecIndex(float* networkSpec, int numSamples)
{
	int inputWidth = getLayerWidth(networkSpec, INPUT_LAYER_ID);
	int outputWidth = getLayerWidth(networkSpec, OUTPUT_LAYER_ID);
	TrainingSpecIndex index;

	
	int inputOffsetIndex = 0;
	int outputOffsetIndex = inputOffsetIndex + inputWidth;
	int errorMaskOffsetIndex = outputOffsetIndex + outputWidth;
	int skipMinAcceptabledErrorCheckIndex = errorMaskOffsetIndex + outputWidth;
	int resetNetworkStateIndex = skipMinAcceptabledErrorCheckIndex + 1;
	int useAverageErrorIndex = resetNetworkStateIndex + 1;
	int hasErrorMaskIndex = useAverageErrorIndex + 1;

	int recordLength = hasErrorMaskIndex + 1;

	index.totalDataLength = recordLength * numSamples;
	index.recordLength = recordLength; 
	index.inputLength = inputWidth;
	index.outputLength = outputWidth;
	index.numTrainingSamples = numSamples;
	index.inputOffsetIndex = inputOffsetIndex;
	index.outputOffsetIndex = outputOffsetIndex;
	index.errorMaskOffsetIndex = errorMaskOffsetIndex;
	index.skipMinAcceptabledErrorCheckIndex = skipMinAcceptabledErrorCheckIndex;
	index.resetNetworkStateIndex = resetNetworkStateIndex;
	index.useAverageErrorIndex = useAverageErrorIndex;
	index.hasErrorMaskIndex = hasErrorMaskIndex;
	return index;
}


float* createTrainingSpecBuffer(TrainingSpecIndex trainingSpec)
{
	return (float*)malloc( sizeof(float) * trainingSpec.totalDataLength );
}

void freeTrainingSpecBuffer(float* trainingSpecBuffer)
{
	free(trainingSpecBuffer);
}


void setTrainingSpecData(float* trainingSpec, TrainingSpecIndex index, int itemIndex, float* input, float* expectedOutput, float* errorMask, boolean skipMinAcceptabledErrorCheckP, boolean resetNetworkStateP, boolean useAverageErrorP)
{
	int baseOffset = itemIndex * index.recordLength;
	int inputIndex = getInputIndex(index, baseOffset);
	int outputIndex = getExpectedOutputIndex(index, baseOffset);
	int errorIndex = getErrorMaskIndex(index, baseOffset);
	int skipMinAcceptabledErrorCheckIndex = getSkipMinAcceptableErrorPIndex(index, baseOffset);
	int resetNetworkStateIndex = getResetNetworkStatePIndex(index, baseOffset);
	int useAverageErrorIndex = getUseAverageErrorPIndex(index, baseOffset);
	int hasErrorMaskIndex = getHasErrorMaskIndex(index, baseOffset);

	int i;
	for (i = 0; i < index.inputLength; i++)
	{
		trainingSpec[i + inputIndex] = input[i];
	}

	for (i = 0; i < index.outputLength; i++)
	{
		trainingSpec[i + outputIndex] = expectedOutput[i];
	}

	if (errorMask)
	{
		for (i = 0; i < index.outputLength; i++)
		{
			trainingSpec[i + errorIndex] = errorMask[i];
		}
		trainingSpec[hasErrorMaskIndex] = 1.0f;
	}
	else
	{
		trainingSpec[hasErrorMaskIndex] = 0.0f;
	}

	trainingSpec[skipMinAcceptabledErrorCheckIndex] = skipMinAcceptabledErrorCheckP;
	trainingSpec[resetNetworkStateIndex] = resetNetworkStateP;
	trainingSpec[useAverageErrorIndex] = useAverageErrorP;
}


/**
This fills the dataBuffer with the input-expectedOutput pair for the itemIndex(th) item in the trainingSpec as well as defining rules for whether the
network should be reset or not and whether to use error masks
*/
void fillTrainingSpecBuffer(float* trainingSpec, TrainingSpecIndex index, int itemIndex, float* dataBuffer)
{
	int baseOffset = itemIndex * index.recordLength;
	int inputIndex = getInputIndex(index, baseOffset);
	int outputIndex = getExpectedOutputIndex(index, baseOffset);
	int errorIndex = getErrorMaskIndex(index, baseOffset);
	int skipMinAcceptabledErrorCheckIndex = getSkipMinAcceptableErrorPIndex(index, baseOffset);
	int resetNetworkStateIndex = getResetNetworkStatePIndex(index, baseOffset);
	int useAverageErrorIndex = getUseAverageErrorPIndex(index, baseOffset);
	int hasErrorMaskIndex = getHasErrorMaskIndex(index, baseOffset);

	int i;
	for (i = 0; i < index.inputLength; i++)
	{
		dataBuffer[i + getInputIndex(index, 0)] = trainingSpec[i + inputIndex];
	}

	for (i = 0; i < index.outputLength; i++)
	{
		dataBuffer[i + getExpectedOutputIndex(index, 0)] = trainingSpec[i + outputIndex];
	}

	if (trainingSpec[hasErrorMaskIndex] != 0.0f)
	{
		for (i = 0; i < index.outputLength; i++)
		{
			dataBuffer[i + getErrorMaskIndex(index, 0)] = trainingSpec[i + errorIndex];
		}
	}

	dataBuffer[getSkipMinAcceptableErrorPIndex(index, 0)] = trainingSpec[skipMinAcceptabledErrorCheckIndex];
	dataBuffer[getResetNetworkStatePIndex(index, 0)] = trainingSpec[resetNetworkStateIndex];
	dataBuffer[getUseAverageErrorPIndex(index, 0)] = trainingSpec[useAverageErrorIndex];
	dataBuffer[getHasErrorMaskIndex(index, 0)] = trainingSpec[hasErrorMaskIndex];
}


// Not normally used

float* getTrainingSpecBufferForItem(float* trainingSpec, TrainingSpecIndex index, int itemIndex)
{
	float* dataBuffer = (float*)malloc(index.recordLength * sizeof(float));

	if (!dataBuffer)
	{
		// raise error
		printf("Failed to create training spec buffer\n");
		return dataBuffer;
	}
	else
	{
		fillTrainingSpecBuffer(trainingSpec, index, itemIndex, dataBuffer);
		return dataBuffer;
	}
}

void freeTrainingSpecBufferForItem(float* buffer)
{
	if (buffer)
	{
		free(buffer);
	}
}


// o - -- o o - -- o o - -- o o - -- o o - -- o o - -- o 
// Network Training functions
// o - -- o o - -- o o - -- o o - -- o o - -- o o - -- o 

boolean verifyTrainingResult(TrainingSpecIndex index, float* trainingResults, float* trainingSpec)
{
	int numSamples = index.numTrainingSamples;
	int recordLength = index.recordLength;
	int outputWidth = index.outputLength;
	int resultRecordWidth = outputWidth + 1;
	
    int i = 0, j;
    float expectedOutput;
    float resultOutput;
    for (i = 0; i < numSamples; i++)
    {
        for (j = 0;j < outputWidth;j++)
        {
        	expectedOutput = trainingSpec[ getExpectedOutputIndex(index, recordLength * i) + j];
        	resultOutput = trainingResults[resultRecordWidth * i  + 1 + j];

            if (roundToInt(expectedOutput) != roundToInt(resultOutput))
                return false;
        }
        
    }
    return true;
}

// TODO - correct this to take into account error masks correctly
float learnInputOutputPairMap(TrainingSpecIndex index, float* networkSpec, float* trainingSpec, int itemIndex, float* specBuffer)
{
    
	fillTrainingSpecBuffer(trainingSpec, index, itemIndex, specBuffer);
    
    if (specBuffer[getHasErrorMaskIndex(index, 0)] == true)
    {
        setOutputErrorMaskFromTrainingSpec (networkSpec, index, specBuffer);

    }
    
    forwardPassFromTraininingSpec(networkSpec, index, specBuffer);

    return updateForwardPassErrorsFromTraininingSpec(networkSpec, index, specBuffer);
}

/**
	Fills specBuffer with the itemIndex(th) element in the trainingSpec, then performs a forward/backward pass and 
	fills resultBuffer with the error of actual output against the expected output along with the actual output.
	First element of resultBuffer is the error.  The remaining values are the output result of the forward pass
*/
void learnInputOutputPairMapWithDetails(TrainingSpecIndex index, float* networkSpec, float* trainingSpec, int itemIndex, float* specBuffer, float* resultBuffer)
{
	
	float error = learnInputOutputPairMap(index, networkSpec, trainingSpec, itemIndex, specBuffer);
	int outputWidth = index.outputLength;
	int resultWidth = outputWidth + 1;
	resultBuffer[ resultWidth * itemIndex] = error;
	int output_activation_idx = getLayerActivationIndex(networkSpec, OUTPUT_LAYER_ID);
	for (int i = 0; i < outputWidth; i++)
	{
		resultBuffer[resultWidth * itemIndex + i + 1] = networkSpec[output_activation_idx + i ];
	}
}

int learnTrainingSpec(TrainingSpecIndex index, float* networkSpec, float* trainingSpec, int maxSteps, float convergenceThreshold, boolean allowWeightResetsP, WeightUpdateType updateType)
{
	PREDICTOR_ID++;
	const int numSamples = index.numTrainingSamples;
	int outputWidth = index.outputLength;
	const int resultWidth = outputWidth + 1;
	float resultBuffer[resultWidth * numSamples];
	const int recordLength = index.recordLength;
	float maxError = FLOAT_MIN_VALUE, error=0, prevError = 0, averageError = 0;
    boolean afterWeightInitialization = true, returnAverage = false;
    float compError=0;
    float errors[numSamples];

    int j;
    int i = 0;
    float specBuffer[recordLength];
    initializeAllWeights(networkSpec);

    
    for (i=0; i < maxSteps; i++)
    {
        resetNetworkToInitialState(networkSpec);
        maxError = FLOAT_MIN_VALUE;
        for (j = 0; j < numSamples; j++)
        {
        	fillTrainingSpecBuffer(trainingSpec, index, j, specBuffer);
            returnAverage = specBuffer[getUseAverageErrorPIndex(index, 0)];
            if (specBuffer[getResetNetworkStatePIndex(index, 0)] == true)
                resetNetworkToInitialState(networkSpec);

            learnInputOutputPairMapWithDetails(index, networkSpec, trainingSpec, j, specBuffer, resultBuffer);
            error = resultBuffer[resultWidth * j];
            averageError = averageError*i/(i+1.0F)+error/(i + 1.0F);

            if (specBuffer[getSkipMinAcceptableErrorPIndex(index, 0)] == false)
            {
                maxError = max(maxError, error);
            }

            if (returnAverage)
                compError = averageError;
            else
                compError = maxError;

            errors[j] = error;
        }

        boolean verify = verifyTrainingResult(index, resultBuffer, trainingSpec);
        if (verify)
        {
        	if (DEBUG)
        	{
        		printf("Learned pattern %d in %d steps and error: %f\n", PREDICTOR_ID, i + 1, compError);
        	}
            return i + 1;
        }


        if (afterWeightInitialization)
        {
            afterWeightInitialization = false;
            prevError = compError;
            updateWeightsFromErrors(networkSpec, updateType);
        }
        else
        {
            float convergenceFraction = (abs(compError - prevError)/compError);
            if (convergenceFraction < convergenceThreshold && allowWeightResetsP)
            {
                initializeAllWeights(networkSpec);
                afterWeightInitialization = true;

            }
            else
            {
            	updateWeightsFromErrors(networkSpec, updateType);
            	prevError = compError;	
            }
        }
    }

    if (DEBUG)
	{
		printf("Predictor (%d) failed to learn pattern after  %d steps and error: %f\n", PREDICTOR_ID, i, compError);
	}

    return i;
}

int createCappedLSTM(float* networkSpec, float* startInput, float* stopInput)
{
    TrainingSpecIndex index = getTrainingSpecIndex(networkSpec, 1);
    const int length = index.totalDataLength;
    float trainingSpec[length];

	setTrainingSpecData(trainingSpec, index, 0, startInput, stopInput, NULL, false, false, false);
    

    initializeAllWeights(networkSpec);
    int steps = learnTrainingSpec(index, networkSpec, trainingSpec, 20, 0.001F, true, RPROP);
    setCustomData(networkSpec, 0, MATCH_COUNT);
    setCustomData(networkSpec, 0, FAILURE_COUNT);
    setCustomData(networkSpec, 0, WEIGHT);
    return steps;
}

void resetCappedLSTM(float* networkSpec, float* initialState, boolean onlyMetaDataP)
{
    if (!onlyMetaDataP)
    {
        resetNetworkToInitialState(networkSpec);
        forwardPass(networkSpec, initialState);
    }
    setCustomData(networkSpec, 0, MATCH_COUNT);
    setCustomData(networkSpec, 0, FAILURE_COUNT);
    setCustomData(networkSpec, 0, WEIGHT);
}




boolean isAtFinalState(float* networkSpec, float* finalState)
{
	int outputWidth = getLayerWidth(networkSpec, OUTPUT_LAYER_ID);
	float out[outputWidth]; 
    getOutputActivation(networkSpec, out);
    roundVectorToInt(out, outputWidth);

    return vectorEquals(outputWidth, out, finalState);
}

void incrementLength(float* networkSpec)
{
    setCappedLSTMLength(networkSpec, 1 + getCappedLSTMLength(networkSpec));
}

void setCappedLSTMLength(float* networkSpec, int i)
{
    setCustomData(networkSpec, i, LENGTH);
}


int getCappedLSTMLength(float* networkSpec)
{
    return (int)getCustomData(networkSpec, LENGTH, -1);
}

int getMatchCount(float* networkSpec)
{
    return (int)getCustomData(networkSpec, MATCH_COUNT, -1);
}

void incrementMatchCount(float* networkSpec)
{
    int prior = (int)getCustomData(networkSpec, MATCH_COUNT, -1);
    setCustomData(networkSpec, prior + 1, MATCH_COUNT);

}

void decrementMatchCount(float* networkSpec)
{
    int prior = (int)getCustomData(networkSpec, MATCH_COUNT, -1);
    setCustomData(networkSpec, max(0, prior - 1), MATCH_COUNT);

}

void decrementFailureCount(float* networkSpec)
{
    decrementCustomParameter(networkSpec, FAILURE_COUNT, false);

}


void resetFailureCount(float* networkSpec)
{
    setCustomData(networkSpec, 0, FAILURE_COUNT);

}

void resetMatchCount(float* networkSpec)
{
    setCustomData(networkSpec, 0, MATCH_COUNT);

}




void decrementCustomParameter(float* networkSpec, int parameter, boolean allowNegativeP)
{
    int prior;
    float badValue = -1;
    if (allowNegativeP)
         badValue = FLOAT_MIN_VALUE;

    prior = (int)getCustomData(networkSpec, parameter, badValue);
    if (prior == badValue){
        // Error

    }
    if (allowNegativeP)
        setCustomData(networkSpec, prior - 1, parameter);
    else
        setCustomData(networkSpec, max(0, prior - 1), parameter);
}


void incrementCustomParameter(float* networkSpec, int parameter)
{
    int prior;
    float badValue = -1;


    prior = (int)getCustomData(networkSpec, parameter, badValue);
    if (prior == badValue){
        // Error

    }
    setCustomData(networkSpec, prior + 1, parameter);
}



void incrementFailureCount(float* networkSpec)
{
    int prior = (int)getCustomData(networkSpec, FAILURE_COUNT, -1);
    setCustomData(networkSpec, prior + 1, FAILURE_COUNT);
}

int getFailureCount(float* networkSpec)
{
    return (int)getCustomData(networkSpec, FAILURE_COUNT, -1);
}

void setId(float* networkSpec, float id)
{
    setCustomData(networkSpec, id, ID);
}

float getId(float* networkSpec)
{
    return getCustomData(networkSpec, ID, -1) ;
}


int appendVectorToSequence(float* cappedNetworkSpec, float* startInput, float* stopInput, float* newInput, int maxSteps, float convergenceThreshold, boolean allowWeightResetsP, WeightUpdateType updateType)
{
	int sequenceLength = getCappedLSTMLength(cappedNetworkSpec) + 1;
	TrainingSpecIndex index = getTrainingSpecIndex(cappedNetworkSpec, sequenceLength);
	float trainingSpec[index.totalDataLength];
	const int dataWidth = index.inputLength;
	
    const int networkLength = cappedNetworkSpec[NETWORK_LENGTH_IDX];
    float copy[networkLength];

    copyNetwork(cappedNetworkSpec, copy);

    float *input = startInput;
    float output[dataWidth];
    

    resetNetworkToInitialState(copy);
    forwardPass(copy, input);
    getOutputActivation(copy, output);


    int i;
    for (i = 0; i < sequenceLength; i++)
    {
    	if (!vectorEquals(dataWidth, output, stopInput))
    	{
    		setTrainingSpecData(trainingSpec, index, i, input, output, NULL, false, false, false);
    		arrayCopy(dataWidth, output, input);
			forwardPass(copy, input);
			getOutputActivation(copy, output);
			roundVectorToInt(output, dataWidth);
    	}
    	else
    	{
    		break;
    	}
    }
    setTrainingSpecData(trainingSpec, index, i - 1, input, newInput, NULL, false, false, false);
    setTrainingSpecData(trainingSpec, index, i, newInput, stopInput, NULL, false, false, false);
    
    initializeAllWeights(copy);

    int stepsUsed = learnTrainingSpec(index, copy, trainingSpec, maxSteps, convergenceThreshold, allowWeightResetsP, updateType);

    if (stepsUsed <= maxSteps)
    {
    	copyNetwork(copy, cappedNetworkSpec);
        
        incrementLength(cappedNetworkSpec);
    }

    return stepsUsed;
}


NetworkSpec getBaseStandardNetworkSpec(int numInputNodes, int numOutputNodes, int sizeMemoryCellLayer, int outputErrorFunctionId, int outputActivationFucntionId, WeightUpdateType updateType, boolean fillP)
{
	const int updateLength = 11;
	const int forwardLength = 15;
	NetworkSpec spec;
	spec.inputLayerWidth = numInputNodes;
	spec.outputLayerWidth = numOutputNodes;
	spec.memoryCellWidth = sizeMemoryCellLayer;
	spec.initialDelta = 0.012F;
	spec.maxDelta = 50;
	spec.minDelta = 0;
	spec.n_Max = 1.2;
	spec.n_Min = 0.5;
	spec.convergenceThreshold = 0.0001;
	spec.updateType = updateType;
	spec.outputErrorFunctionId = outputErrorFunctionId;
	spec.outputActivationFunctionId = outputActivationFucntionId;
	spec.minInitialWeight = -1;
	spec.maxInitialWeight = 1;
	spec.learningRate = 0.001F;
	
	spec.weightUpdateLinkOrder =  (int*)malloc(sizeof(int) * updateLength);
	spec.feedforwardLinkOrder =  (int*)malloc(sizeof(int) * forwardLength);
	spec.numForwardLinks = forwardLength;
	spec.numWeightUpdates = updateLength;

	int i = 0;
	spec.weightUpdateLinkOrder[i++] =  getLinkId(CELL_OUTPUT_LAYER_ID, OUTPUT_LAYER_ID);
	spec.weightUpdateLinkOrder[i++] = getLinkId(CELL_OUTPUT_LAYER_ID, INPUT_GATE_LAYER_ID);
	spec.weightUpdateLinkOrder[i++] = getLinkId(CELL_OUTPUT_LAYER_ID, OUTPUT_GATE_LAYER_ID);
	spec.weightUpdateLinkOrder[i++] = getLinkId(CELL_OUTPUT_LAYER_ID, CELL_INPUT_LAYER_ID);
	spec.weightUpdateLinkOrder[i++] = getLinkId(OUTPUT_GATE_LAYER_ID, INPUT_GATE_LAYER_ID);
	spec.weightUpdateLinkOrder[i++] = getLinkId(OUTPUT_GATE_LAYER_ID, CELL_INPUT_LAYER_ID);
	spec.weightUpdateLinkOrder[i++] = getLinkId(INPUT_GATE_LAYER_ID, OUTPUT_GATE_LAYER_ID);
	spec.weightUpdateLinkOrder[i++] = getLinkId(INPUT_GATE_LAYER_ID, CELL_INPUT_LAYER_ID);
	spec.weightUpdateLinkOrder[i++] = getLinkId(INPUT_LAYER_ID, INPUT_GATE_LAYER_ID);
	spec.weightUpdateLinkOrder[i++] = getLinkId(INPUT_LAYER_ID, OUTPUT_GATE_LAYER_ID);
	spec.weightUpdateLinkOrder[i++] = getLinkId(INPUT_LAYER_ID, CELL_INPUT_LAYER_ID);

	i = 0;
	spec.feedforwardLinkOrder[i++] = getLinkId(CELL_OUTPUT_LAYER_ID, INPUT_GATE_LAYER_ID);
	spec.feedforwardLinkOrder[i++] = getLinkId(CELL_OUTPUT_LAYER_ID, OUTPUT_GATE_LAYER_ID);
	spec.feedforwardLinkOrder[i++] = getLinkId(OUTPUT_GATE_LAYER_ID, INPUT_GATE_LAYER_ID);
	spec.feedforwardLinkOrder[i++] = getLinkId(CELL_OUTPUT_LAYER_ID, CELL_INPUT_LAYER_ID);
	spec.feedforwardLinkOrder[i++] = getLinkId(OUTPUT_GATE_LAYER_ID, CELL_INPUT_LAYER_ID);
	spec.feedforwardLinkOrder[i++] = getLinkId(INPUT_GATE_LAYER_ID, OUTPUT_GATE_LAYER_ID);
	spec.feedforwardLinkOrder[i++] = getLinkId(INPUT_GATE_LAYER_ID, CELL_INPUT_LAYER_ID);
	spec.feedforwardLinkOrder[i++] = getLinkId(INPUT_LAYER_ID, CELL_INPUT_LAYER_ID);
	spec.feedforwardLinkOrder[i++] = -1*CELL_INPUT_LAYER_ID;
	spec.feedforwardLinkOrder[i++] = getLinkId(INPUT_LAYER_ID, INPUT_GATE_LAYER_ID);
	spec.feedforwardLinkOrder[i++] = -1*INPUT_GATE_LAYER_ID;
	spec.feedforwardLinkOrder[i++] = getLinkId(INPUT_LAYER_ID, OUTPUT_GATE_LAYER_ID);
	spec.feedforwardLinkOrder[i++] = -1 * OUTPUT_GATE_LAYER_ID;
	spec.feedforwardLinkOrder[i++] = getLinkId(CELL_OUTPUT_LAYER_ID, OUTPUT_LAYER_ID);
	spec.feedforwardLinkOrder[i++] = -1 * OUTPUT_LAYER_ID;

	if (fillP)
	{
		fillNetwork(&spec);
	}
	return spec;
}


void freeNetworkSpec(NetworkSpec spec)
{
	if (spec.networkSpec)
	{
		free(spec.networkSpec);
	}

	if (spec.feedforwardLinkOrder)
	{
		free(spec.feedforwardLinkOrder);
	}
	if (spec.weightUpdateLinkOrder)
	{
		free(spec.weightUpdateLinkOrder);
	}
}

void fillNetwork(NetworkSpec *spec)
{
	int i;
	int inputLayerWidth = spec->inputLayerWidth;
	int outputLayerWidth = spec->outputLayerWidth;
	int memoryStateWidth = spec->memoryCellWidth;
	int forwardLinkWidth = spec->numForwardLinks;
	int updateLinkWidth = spec->numWeightUpdates;

	const int VARIABLE_HEADER_WIDTH = LAYER_STATE_BASE_IDX;

	const int NETWORK_STATE_WIDTH = inputLayerWidth * 4 + outputLayerWidth * 5 + memoryStateWidth * 22;
	const int NETWORK_UPDATE_WIDTH =  max(1, updateLinkWidth) + 1 + max(1, forwardLinkWidth) + 1;

	
	const int HEADER_WIDTH = NETWORK_STATE_WIDTH + VARIABLE_HEADER_WIDTH + NETWORK_UPDATE_WIDTH;
	float networkHeader[HEADER_WIDTH];

	for (i = 0; i < HEADER_WIDTH;i++)
	{
		networkHeader[i] = 0.0;
	}

	networkHeader[VERSION_IDX] = CURRENT_VERSION;

	// Set main learning parameters
	networkHeader[N_MINUS_IDX] = spec->n_Min;
	networkHeader[N_PLUS_IDX] = spec->n_Max;
	networkHeader[LEARNING_RATE_IDX] = spec->learningRate;
	networkHeader[MIN_WEIGHT_DELTA_IDX] = spec->minDelta;
	networkHeader[MAX_WEIGHT_DELTA_IDX] = spec->maxDelta;
	networkHeader[INITIAL_WEIGHT_DELTA_IDX] = spec->initialDelta;
	networkHeader[MIN_INITIAL_WEIGHT_IDX] = spec->minInitialWeight;
	networkHeader[MAX_INITIAL_WEIGHT_IDX] = spec->maxInitialWeight;

	


    // Set layer activation function ids
	for (i = 0; i < NUM_LAYERS; i++)
	{
		networkHeader[LAYER_ACTIVATION_FUNCTION_ID_IDX + i] = (float)LAYER_ID_ACTIVATION_FUNCTION_MAP[i];
	}

	networkHeader[LAYER_ACTIVATION_FUNCTION_ID_IDX + OUTPUT_LAYER_ID] = (float)spec->outputActivationFunctionId;
	networkHeader[OUTPUT_LAYER_ERROR_FUNCTION_ID_IDX] = (float)spec->outputErrorFunctionId;
	
	// Define Main layer node specifications
	networkHeader[INPUT_NODE_COUNT_IDX] = inputLayerWidth;
	networkHeader[OUTPUT_NODE_COUNT_IDX] = outputLayerWidth;
	networkHeader[NUM_CELL_STATES_IDX] = memoryStateWidth;


	// Define Layer Widths
	int layerIdIdx = 0;
    networkHeader[LAYER_DIMEN_MAP_IDX + layerIdIdx++] = (float)inputLayerWidth;
    networkHeader[LAYER_DIMEN_MAP_IDX + layerIdIdx++] = (float)outputLayerWidth;
    for (; layerIdIdx < NUM_LAYERS; layerIdIdx++)
    {
        networkHeader[LAYER_DIMEN_MAP_IDX + layerIdIdx] =  (float)memoryStateWidth;
    }

	// Reconstruct layer connectivity map from feedforward link list
	boolean connections[NUM_LAYERS*NUM_LAYERS];

	for (i = 0; i < NUM_LAYERS*NUM_LAYERS; i++)
	{
		connections[i] = false;
	}

	// Outgoing link count
	int linkCount[NUM_LAYERS];

	for (i = 0; i < NUM_LAYERS; i++)
	{
		linkCount[i] = 0;
	}

	int linkId;
	int sourceId, targetId;
	int linkWeightWidth = 0;
	int sourceWidth;
	int targetWidth;
	

	// TODO - decide if this is still needed.  This segment is currently (3/22/2017) not used except for the calculation
	// of linkWeightWidth and linkCount[sourceId]
	for (i = 0; i < spec->numForwardLinks; i++)
	{
		linkId = spec->feedforwardLinkOrder[i];
		if (linkId >= 0 && connections[linkId] == false)
		{
			sourceId = getSourceLayerFromLinkId(linkId);
			targetId = getTargetLayerFromLinkId(linkId);

			sourceWidth = getLayerWidth(networkHeader, sourceId);
			targetWidth = getLayerWidth(networkHeader, targetId);
			linkWeightWidth += LINK_DATA_WEIGHT_MATRIX_IDX + 5 * sourceWidth * targetWidth;
			//networkHeader[NETWORK_GRAPH_BASE_IDX + sourceId*(NUM_LAYERS + 1) + 1 + linkCount[sourceId]] = (float)targetId;
			connections[linkId] = true;
			linkCount[sourceId]++;

		}
		
	}


	// TODO - decide if this is still needed.  This segment is currently (3/22/2017) not used except for the calculation
	// of linkWeightWidth and linkCount[sourceId].  If keeping this then should consolidate with the code that sets the 
	// linkweight data indices (this is calculated similarly at the end of this function)
	for (i = 0; i < spec->numWeightUpdates;i++)
	{
		linkId = spec->weightUpdateLinkOrder[i];
		sourceId = getSourceLayerFromLinkId(linkId);
		targetId = getTargetLayerFromLinkId(linkId);

		if (connections[linkId] == false)
		{
			connections[linkId] = true;
			// Data is offset by one to accomodate the count of number of target nodes for the source (as calculated in next block)
			//networkHeader[NETWORK_GRAPH_BASE_IDX + sourceId*(NUM_LAYERS + 1) + 1 + linkCount[sourceId]] = (float)targetId;
			sourceWidth = getLayerWidth(networkHeader, sourceId);
			targetWidth = getLayerWidth(networkHeader, targetId);
			linkWeightWidth += LINK_DATA_WEIGHT_MATRIX_IDX + 5 * sourceWidth * targetWidth;
			linkCount[sourceId]++;
		}
	}

	for (sourceId = 0; sourceId < NUM_LAYERS; sourceId++)
	{
		if (linkCount[sourceId] > 0)
		{
			networkHeader[NETWORK_GRAPH_BASE_IDX + sourceId*(NUM_LAYERS + 1)] = linkCount[sourceId];
		}
	}

	
	float *networkSpec = (float*)malloc( sizeof(float) * (HEADER_WIDTH + linkWeightWidth));
	int totalLength = HEADER_WIDTH + linkWeightWidth;
	networkHeader[NETWORK_LENGTH_IDX] = totalLength;

	for (i = 0; i < totalLength;i++)
	{
		networkSpec[i] = 0.0;
	}

	spec->totalDataLength = totalLength;

	for (int j = 0; j < HEADER_WIDTH; j++)
	{
		networkSpec[j] = networkHeader[j];
	}

	// Define layer node state data
	int I_length = max(1, inputLayerWidth);
    int O_length = max(1, outputLayerWidth);
    int P_length = max(1, memoryStateWidth);

    layerIdIdx = 0;
    int input_layer_net_input_idx = LAYER_STATE_BASE_IDX;
    int input_layer_activation_idx = input_layer_net_input_idx +  I_length;
    int input_layer_partial_net_input_idx =  input_layer_activation_idx + I_length;
    int input_layer_error_responsibility_idx = input_layer_partial_net_input_idx + I_length;
    networkSpec[LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerIdIdx++] = (float)input_layer_net_input_idx;


    int output_layer_net_input_idx = input_layer_error_responsibility_idx + I_length;
    int output_layer_activation_idx = output_layer_net_input_idx + O_length;
    int output_layer_partial_net_input_idx =  output_layer_activation_idx + O_length;
    int output_layer_error_responsibility_idx = output_layer_partial_net_input_idx + O_length;
    int output_layer_error_mask_idx = output_layer_error_responsibility_idx + O_length;
    networkSpec[LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerIdIdx++] = (float)output_layer_net_input_idx;
    for (i=0;i < O_length;i++)
    {
        networkSpec[output_layer_error_mask_idx + i] = 1.0F;
	}

    int cell_input_net_input_idx = output_layer_error_mask_idx + O_length;
    int cell_input_activation_idx = cell_input_net_input_idx + P_length;
    int cell_input_partial_net_input_idx =  cell_input_activation_idx + P_length;
    int cell_input_error_responsibility_idx = cell_input_partial_net_input_idx + P_length;
    networkSpec[LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerIdIdx++] = (float)cell_input_net_input_idx;


    int cell_output_net_input_idx = cell_input_error_responsibility_idx + P_length;
    int cell_output_activation_idx = cell_output_net_input_idx + P_length;
    int cell_output_partial_net_input_idx =  cell_output_activation_idx + P_length;
    int cell_output_error_responsibility_idx = cell_output_partial_net_input_idx + P_length;
    networkSpec[LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerIdIdx++] = (float)cell_output_net_input_idx;


    int forget_gate_net_input_idx = cell_output_error_responsibility_idx + P_length;
    int forget_gate_activation_idx = forget_gate_net_input_idx + P_length;
    int forget_gate_partial_net_input_idx =  forget_gate_activation_idx + P_length;
    int forget_gate_error_responsibility_idx = forget_gate_partial_net_input_idx + P_length;
    networkSpec[LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerIdIdx++] = (float)forget_gate_net_input_idx;


    int input_gate_net_input_idx = forget_gate_error_responsibility_idx + P_length;
    int input_gate_activation_idx = input_gate_net_input_idx  + P_length;
    int input_gate_partial_net_input_idx =  input_gate_activation_idx + P_length;
    int input_gate_error_responsibility_idx = input_gate_partial_net_input_idx + P_length;
    networkSpec[LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerIdIdx++] = (float)input_gate_net_input_idx;


    int output_gate_net_input_idx = input_gate_error_responsibility_idx + P_length;
    int output_gate_activation_idx = output_gate_net_input_idx + P_length;
    int output_gate_partial_net_input_idx =  output_gate_activation_idx + P_length;
    int output_gate_error_responsibility_idx = output_gate_partial_net_input_idx + P_length;
    networkSpec[LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerIdIdx++] = (float)output_gate_net_input_idx;


    int memory_cell_activation_idx = output_gate_error_responsibility_idx + P_length;
    int memory_cell_net_input_idx = memory_cell_activation_idx + P_length;
    networkSpec[LAYER_ID_TO_STATE_DATA_BASE_IDX_MAP_IDX + layerIdIdx++] = (float)memory_cell_activation_idx;

    // ********************************************
    // START: Parameters to be passed into main neural network functions
    // ********************************************

    // feedforward and backpropagation data
    int backprop_link_count_idx;
    int feedforward_link_count_idx;
    feedforward_link_count_idx = memory_cell_net_input_idx + P_length; // ** pass to function **
    networkSpec[feedforward_link_count_idx] = (float)forwardLinkWidth;
    networkSpec[FORWARD_PASS_LINK_ORDER_DATA_IDX] = (float)feedforward_link_count_idx;


    int feedforward_link_data_idx = feedforward_link_count_idx + 1;
    

    for (i = 0; i < forwardLinkWidth; i++ )
    {
    	networkSpec[feedforward_link_data_idx + i] = spec->feedforwardLinkOrder[i];
    }

    int feedforward_data_width = max(1, forwardLinkWidth);
    backprop_link_count_idx = feedforward_link_data_idx + feedforward_data_width; // *** pass to function **
    int backprop_link_data_idx = backprop_link_count_idx + 1;
    networkSpec[backprop_link_count_idx] = (float)updateLinkWidth;
    networkSpec[BACKWARD_PASS_LINK_ORDER_DATA_IDX] = (float)backprop_link_count_idx;

    for (i = 0; i < updateLinkWidth; i++)
    {
    	networkSpec[backprop_link_data_idx + i] = spec->weightUpdateLinkOrder[i];
    }

    int weightUpdateDataWidth = max(1, updateLinkWidth);
    int nextIndex = backprop_link_data_idx + weightUpdateDataWidth;

    
    for (int linkId = 0; linkId < NUM_LAYERS*NUM_LAYERS; linkId++)
    {
		if (connections[linkId] == true)
		{
			sourceId = getSourceLayerFromLinkId(linkId);
			targetId = getTargetLayerFromLinkId(linkId);
			sourceWidth = getLayerWidth(networkSpec, sourceId);
			targetWidth = getLayerWidth(networkSpec, targetId);
			networkSpec[LINK_ID_TO_DATA_IDX_IDX + linkId] = nextIndex;
			networkSpec[nextIndex + LINK_DATA_SOURCE_LAYER_ID_IDX] = (float)sourceId;
			networkSpec[nextIndex + LINK_DATA_TARGET_LAYER_ID_IDX] = (float)targetId;
			nextIndex += LINK_DATA_WEIGHT_MATRIX_IDX + 5 * (sourceWidth * targetWidth);

		}
		else
		{
			networkSpec[LINK_ID_TO_DATA_IDX_IDX + linkId] = -1;
		}
    }
    networkSpec[CUSTOM_VARIABLE_DATA_IDX] = nextIndex;

    //printVector(networkSpec, (int)networkSpec[NETWORK_LENGTH_IDX], true);
    spec->networkSpec = networkSpec;
    
}



