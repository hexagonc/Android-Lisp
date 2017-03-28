#include "tools.h"
#include "neural_library.h"
#include "dbg.h"



void feedForwardTest(float* networkSpec)
{
	float initialValue = 0.5;
	float input[1] = {initialValue};

	float output[1];

	forwardPass(networkSpec, input);

	getOutputActivation(networkSpec, output);

	float value = output[0];

	debug("Input: %f - Output: %f\n", initialValue, value);
}


void simpleLearningTest(float* networkSpec, float* testInput, int inputLength, WeightUpdateType updateType, int maxSteps)
{

	printf("Testing manual learning.\n\n");

	float inputBuffer[1];
	float expectedOutputBuffer[1];
	float output[1];
	
	int trainingSpecLength = inputLength - 1;
	

	long randomSeed = 100;
	setRNGMethod(MERSENNE_TWISTER);
	setSeed(randomSeed);


	initializeAllWeights(networkSpec);
	float error ;
	float maxError = FLOAT_MIN_VALUE;
	float errors[trainingSpecLength];
	float outputValues[trainingSpecLength];
	int j, i, base = 0;
	float prevError;
	float convergenceThreshold = 0.0001F;
	boolean second = false;

	
	boolean pass = true;
	boolean showMaxErrorP = false;

	for (j = 0; j < maxSteps; j++)
	{
		maxError = FLOAT_MIN_VALUE;
		resetNetworkToInitialState(networkSpec);
		pass = true;
		for (i = 0; i < trainingSpecLength; i++)
		{

			inputBuffer[0] = testInput[i];
			expectedOutputBuffer[0] = testInput[i + 1];
			forwardPass(networkSpec, inputBuffer);
			error = updateForwardPassErrors(networkSpec, expectedOutputBuffer);
			getOutputActivation(networkSpec, output);
			outputValues[i] = output[0];
			errors[i] = error;
			maxError = max(maxError, error);
			if (!roundedEquals(output[0], expectedOutputBuffer[0]))
			{
				pass = false;
			}
		}

		if (pass)
		{
			printf("Found solution after %d steps!\n", j);
			break;
		}
		if (second && updateType == RPROP)
		{
			if (abs(prevError - maxError) < convergenceThreshold)
			{
				initializeAllWeights(networkSpec);
				second = false;
				printf("Reseting weights after %d steps\n", j - base);
				base = j;
				continue;
			}
		}
		second = true;
		prevError = maxError;
		updateWeightsFromErrors(networkSpec, updateType);
		if (showMaxErrorP)
			printf("Max error for step %d is %10.10f\n", j, maxError);
	}
	printf("Final errors after %d steps and max error %f: ", j, maxError);
	printVector(errors, trainingSpecLength, false);
	printf("\n");

	printf("Final output: ");
	printVector(outputValues, trainingSpecLength, false);
	printf("\n");
}


void testTrainingSpec(float *networkSpec, float* testInput, int inputLength, WeightUpdateType updateType, int maxSteps)
{
	printf("Testing training spec learning.\n\n");
	float inputBuffer[1];
	float expectedOutputBuffer[1];
	float output[1];

	const int trainingSpecLength = inputLength - 1;

	
	TrainingSpecIndex index = getTrainingSpecIndex(networkSpec, trainingSpecLength);

	float* trainingSpec = createTrainingSpecBuffer(index);

	long randomSeed = 100;
	setRNGMethod(MERSENNE_TWISTER);
	setSeed(randomSeed);
	initializeAllWeights(networkSpec);
	
	int i, j, base = 0;
	float prevError;
	float convergenceThreshold = 0.0001F;
	

	for (i = 0; i < trainingSpecLength; i++)
	{
		inputBuffer[0] = testInput[i];
		expectedOutputBuffer[0] = testInput[i + 1];
		
		setTrainingSpecData(trainingSpec, index, i, inputBuffer, expectedOutputBuffer, NULL, false, false, false);

	}

	const int recordLength = index.recordLength;
	int resultRecordWidth = index.outputLength + 1;
	const int resultLength = resultRecordWidth * index.numTrainingSamples;

	float specBuffer[recordLength];
	float resultBuffer[resultLength];
	float error = 0, maxError = 0;
	float errors[trainingSpecLength];
	float outputValues[trainingSpecLength];

	boolean second = false;
	boolean pass = true, matches, showMaxErrorP = false;

	for (j = 0; j < maxSteps; j++)
	{
		resetNetworkToInitialState(networkSpec);
		
		pass = true;
		maxError = FLOAT_MIN_VALUE;

		for (i = 0; i < trainingSpecLength; i++)
		{
			learnInputOutputPairMapWithDetails(index, networkSpec, trainingSpec, i, specBuffer, resultBuffer);
			error = resultBuffer[i*resultRecordWidth];
			errors[i] = error;
			outputValues[i] = resultBuffer[i*resultRecordWidth + 1];
			maxError = max(maxError, error);	
		}
		pass = verifyTrainingResult(index, resultBuffer, trainingSpec);
		
		if (pass)
		{
			printf("Found solution after %d steps!\n", j);
			break;
		}

		if (second)
		{
			if (abs(prevError - maxError)/maxError < convergenceThreshold)
			{
				second = false;
				initializeAllWeights(networkSpec);
				printf("Reseting weights after %d steps\n", j - base);
				base = j;
				continue;
			}
		}
		prevError = maxError;
		second = true;
		updateWeightsFromErrors(networkSpec, updateType);
		if (showMaxErrorP)
			printf("Max error for step %d is %10.10f\n", j, maxError);

	}

	printf("Final errors after %d steps and max error %f: ", j, maxError);
	printVector(errors, trainingSpecLength, false);
	printf("\n");

	printf("Final output: ");
	printVector(outputValues, trainingSpecLength, false);
	printf("\n");

	freeTrainingSpecBuffer(trainingSpec);
}


void testFullTrainingSpec(float *networkSpec, float* testInput, int inputLength, WeightUpdateType updateType, int maxSteps)
{
	printf("Testing Full Training spec learning.\n\n");
	float inputBuffer[1];
	float expectedOutputBuffer[1];
	float output[1];

	const int trainingSpecLength = inputLength - 1;
	float convergenceThreshold = 0.0001F;
	
	TrainingSpecIndex index = getTrainingSpecIndex(networkSpec, trainingSpecLength);

	float* trainingSpec = createTrainingSpecBuffer(index);

	


	for (int i = 0; i < trainingSpecLength; i++)
	{
		inputBuffer[0] = testInput[i];
		expectedOutputBuffer[0] = testInput[i + 1];
		
		setTrainingSpecData(trainingSpec, index, i, inputBuffer, expectedOutputBuffer, NULL, false, false, false);

	}

	printf("Training Spec: ");
	printVector(trainingSpec, index.totalDataLength, false);
	printf("\n");

	long randomSeed = 100;
	setRNGMethod(MERSENNE_TWISTER);
	setSeed(randomSeed);
	// Output

	int steps = 0;
	steps = learnTrainingSpec(index, networkSpec, trainingSpec, maxSteps, convergenceThreshold, true, updateType);

	printf("Final learning result is: %d\n", steps);

	freeTrainingSpecBuffer(trainingSpec);
}

void randomTests()
{
	long randomSeed = 100;
	setRNGMethod(MERSENNE_TWISTER);
	setSeed(randomSeed);

	double ran = randomLCG();
	debug("Using initial random seed: %ld yielding initial value: %f\n", randomSeed, ran);
}

int main(int argc, char *argv[])
{
	debug("Testing neural network\n");

	// Create standard neural network
	debug("Creating standard neural network\n");

	int numInputNodes = 1;
	int numOutputNodes = 1;
	int numMemoryCellStates = 8;
	int outputErrorFunctionId = MSE_ERROR_FUNCTION_ID;
	int outputActivationFunctionId = SIGMOID_ACTIVATION_ID;

	// Create the network spec
	NetworkSpec spec = getBaseStandardNetworkSpec(numInputNodes, numOutputNodes, numMemoryCellStates, outputErrorFunctionId, outputActivationFunctionId, RPROP, true);
	float* networkSpec = spec.networkSpec;

	float testInput[] = { 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0};
	int inputLength = (sizeof(testInput))/(sizeof(float));
	int maxSteps = 20000;

	debug("Network length: %d\n", spec.totalDataLength);
	WeightUpdateType updateType = RPROP;
	simpleLearningTest(networkSpec, testInput, inputLength, updateType, maxSteps);
	
	testTrainingSpec(networkSpec, testInput, inputLength, updateType, maxSteps);

	testFullTrainingSpec(networkSpec, testInput, inputLength, updateType, maxSteps);

finish:
	
	
	freeNetworkSpec(spec);
	//freeTrainingSpecBuffer(trainingSpec);
	

	return 0;
}
