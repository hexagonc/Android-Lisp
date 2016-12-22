package com.evolved.automata.lisp.nn;

import com.evolved.automata.AITools;
import com.evolved.automata.IndexedValueMapper;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.LispValueMapper;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.nn.LSTMLearningResult;
import com.evolved.automata.nn.LSTMNetwork;
import com.evolved.automata.nn.NNTools;
import com.evolved.automata.nn.Vector;
import com.evolved.automata.nn.VectorValueMapper;

/**
 * Created by Evolved8 on 12/19/16.
 */
public class NeuralNetLispInterface {
    public static void addNeuralNetFunctions(Environment env)
    {
        env.mapFunction("create-simple-lstm-network", createSimpleLSTMNetwork());
        env.mapFunction("simple-lstm-learn-sequence", simpleLSTMLearnSequence());
        env.mapFunction("simple-lstm-serialize-weights", simpleLSTMSerializeWeights());
        env.mapFunction("simple-lstm-load-serialized-weights", simpleLSTMLoadSerializedWeights());
        env.mapFunction("simple-lstm-extrapolate-sequence", simpleLSTMExtrapolateSequence());

        env.mapFunction("simple-lstm-save-node-state", simpleLSTMSaveNodeState());
        env.mapFunction("simple-lstm-load-state", simpleLSTMLoadNodeState());

        env.mapFunction("simple-lstm-use-current-state-as-initial", setUseCurrentStateAsInitial());
        env.mapFunction("simple-lstm-use-default-initial-state", useDefaultInitialActivation());
    }

    public static SimpleFunctionTemplate setUseCurrentStateAsInitial()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) setUseCurrentStateAsInitial();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, false);

                LSTMNetwork lstm = (LSTMNetwork)evaluatedArgs[0].getObjectValue();
                lstm.setInitialNodeActivationAsCurrent();
                return evaluatedArgs[0];
            }
        };
    }

    public static SimpleFunctionTemplate useDefaultInitialActivation()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) useDefaultInitialActivation();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, false);

                LSTMNetwork lstm = (LSTMNetwork)evaluatedArgs[0].getObjectValue();
                lstm.useDefaultInitialNodeActivation();
                return evaluatedArgs[0];
            }
        };
    }


    public static SimpleFunctionTemplate createSimpleLSTMNetwork()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) createSimpleLSTMNetwork();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(3, true, true);
                int numInputNodes = (int)evaluatedArgs[0].getIntValue();
                int numMemoryCellStateNodes = (int)evaluatedArgs[1].getIntValue();
                int numOutputNodes = (int)evaluatedArgs[2].getIntValue();

                String serializedWeights = null;
                if (evaluatedArgs.length>3)
                {
                    serializedWeights = evaluatedArgs[3].getString();
                }

                LSTMNetwork lstm = NNTools.getStandardLSTM(numInputNodes, numMemoryCellStateNodes, numOutputNodes, serializedWeights);

                return ExtendedFunctions.makeValue(lstm);
            }
        };
    }

    public static SimpleFunctionTemplate simpleLSTMLearnSequence()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMLearnSequence();
            }

            // First argument is LSTM
            // Second argument is a training input, list of binary lists
            // ( {0, 1}*, {0, 1}*, {0, 1}*, ...)
            // Third argument is the max number of steps to iterate
            // Fourth arugment is the maximum acceptable error across all
            // training inputs that will cause the iteration to exit early

            // Optional fifth argument allows weights to be randomized if the
            // lstm gets into a local minimum above the acceptable error
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(4, true, true);

                LSTMNetwork lstm = (LSTMNetwork)evaluatedArgs[0].getObjectValue();
                Value[] trainingSequence = evaluatedArgs[1].getList();
                double maxSteps = evaluatedArgs[2].getFloatValue();
                double maxAcceptableError = evaluatedArgs[3].getFloatValue();

                boolean allowWeightRandomization = true;

                if (evaluatedArgs.length > 4)
                {
                    allowWeightRandomization = !evaluatedArgs[4].isNull();
                }

                lstm.setAllowRerollingWeights(allowWeightRandomization);



                double[] result = lstm.learnSequence(AITools.mapValues(trainingSequence, new IndexedValueMapper<Value, Vector>() {
                    @Override
                    public Vector map(Value input, int index)
                    {
                        return listToVector(input);
                    }

                    @Override
                    public Vector[] getEmptyOutput()
                    {
                        return new Vector[0];
                    }
                }),maxSteps, maxAcceptableError );

                Value[] lispResult = new Value[3];
                LSTMLearningResult learningResult = LSTMLearningResult.make(result);
                lispResult[0] = NLispTools.makeValue(learningResult.maxStepError);
                lispResult[1] = NLispTools.makeValue(learningResult.numIterations);
                lispResult[2] = NLispTools.makeValue(learningResult.stepErrors);
                return NLispTools.makeValue(lispResult);
            }
        };
    }

    public static SimpleFunctionTemplate simpleLSTMSerializeWeights()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSerializeWeights();
            }

            // First argument is LSTM
            // Returns a string of the serialized LSTM, which can be used in create-simple-lstm-network or simple-lstm-load-serialized-weights
            // or
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                LSTMNetwork lstm = (LSTMNetwork)evaluatedArgs[0].getObjectValue();

                return NLispTools.makeValue(lstm.serializeLinkWeights());
            }
        };
    }

    public static SimpleFunctionTemplate simpleLSTMSaveNodeState()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSaveNodeState();
            }

            // First argument is LSTM
            // Returns a string of the serialized LSTM node state, which can be used in
            // or
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                LSTMNetwork lstm = (LSTMNetwork)evaluatedArgs[0].getObjectValue();

                return NLispTools.makeValue(lstm.serializeNetworkActivationState());
            }
        };
    }


    public static SimpleFunctionTemplate simpleLSTMLoadSerializedWeights()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMLoadSerializedWeights();
            }

            // First argument is LSTM
            // Second argument is a string representing the serialized weights
            // This will not work or will lead to exceptions down the road if the
            // structure of the LSTM is incompatible with the weights
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, true, true);

                LSTMNetwork lstm = (LSTMNetwork)evaluatedArgs[0].getObjectValue();
                String weights = evaluatedArgs[1].getString();
                lstm.decodeSerializedLinksToLinkBuffer(weights);
                lstm.loadbufferedLinkWeights();

                return evaluatedArgs[0];
            }
        };
    }

    public static SimpleFunctionTemplate simpleLSTMLoadNodeState()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMLoadNodeState();
            }

            // First argument is LSTM
            // Second argument is a string representing the serialized node activations
            // This will not work or will lead to exceptions down the road if the
            // structure of the LSTM is incompatible with the states
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, true, true);

                LSTMNetwork lstm = (LSTMNetwork)evaluatedArgs[0].getObjectValue();
                String states = evaluatedArgs[1].getString();
                lstm.loadSerializedNetworkActivationState(states);

                return evaluatedArgs[0];
            }
        };
    }

    public static SimpleFunctionTemplate simpleLSTMExtrapolateSequence()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMExtrapolateSequence();
            }

            // First argument is LSTM
            // Second argument is a seed input, list of binary lists
            // ( {0, 1}*, {0, 1}*, {0, 1}*, ...)

            // Third argument is the number of steps to extrapolate

            // Optional fourth argument indicates whether to round the output node
            // activations to 0 or 1 depending on whether the raw value is <=0.5.
            // This can improve the extrapolated results but may not be desired if the
            // output is interpreted as a continuous value (usually not the case if
            // using a sigmoid activation function for the output layer
            // ** By default, this flag is considered true

            //
            // Optional fifth arugment indicates whether to reset the state of
            // the LSTM to its default state.  This is true by default.  You would
            // want to set this to false if the network has already been driven
            // to some (non-default) state and you want to do forward extrapolation
            // to look ahead.  If this is the case, you usually will want the seed
            // input, the second parameter, to be an empty list

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(3, true, true);

                LSTMNetwork lstm = (LSTMNetwork)evaluatedArgs[0].getObjectValue();
                Value[] seedInput = evaluatedArgs[1].getList();
                int extrapSteps = (int)evaluatedArgs[2].getFloatValue();
                boolean roundOutput = true;
                boolean continuePriorState = false;


                if (evaluatedArgs.length > 3)
                {
                    roundOutput = !evaluatedArgs[3].isNull();
                }

                if (evaluatedArgs.length > 4)
                {
                    continuePriorState = !evaluatedArgs[4].isNull();
                }

                lstm.setRoundOutput(roundOutput);

                Vector[] extrapolatedResult = lstm.extrapolate(AITools.mapValues( seedInput, new VectorValueMapper<Value>() {
                    @Override
                    public Vector map(Value input, int index)
                    {
                        return listToVector(input);
                    }
                }), extrapSteps, continuePriorState);

                Value[] lispResult = AITools.mapValues(extrapolatedResult, new LispValueMapper<Vector>() {
                    @Override
                    public Value map(Vector input, int index)
                    {
                        return NLispTools.makeValue(input.raw());
                    }
                });
                return NLispTools.makeValue(lispResult);
            }
        };
    }

    private static Vector listToVector(Value value)
    {
        Value[] list = value.getList();
        int L = list.length;
        double[] components = new double[L];

        for (int i = 0;i<L;i++)
        {
            components[i] = list[i].getFloatValue();
        }

        return new Vector(components);
    }


}
