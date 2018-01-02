package com.evolved.automata.lisp.nn;

import com.evolved.automata.AITools;
import com.evolved.automata.IndexedValueMapper;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.Lambda;
import com.evolved.automata.lisp.LispValueMapper;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.StringHashtableValue;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.lisp.editor.SimpleTextEditor;
import com.evolved.automata.nn.FastLSTMNetwork;
import com.evolved.automata.nn.LSTMLearningResult;
import com.evolved.automata.nn.LSTMNetwork;
import com.evolved.automata.nn.LinkedLSTM;
import com.evolved.automata.nn.LinkedLSTMSet;
import com.evolved.automata.nn.NNTools;
import com.evolved.automata.nn.SLSTMSet;
import com.evolved.automata.nn.SequenceLSTM;
import com.evolved.automata.nn.SequencePredictor;
import com.evolved.automata.nn.Vector;
import com.evolved.automata.nn.VectorValueMapper;
import com.evolved.automata.nn.VectorViewer;
import com.evolved.automata.nn.WeightMatrix;

import java.util.Arrays;
import java.util.HashMap;

import static com.evolved.automata.nn.FastLSTMNetwork.roundToInt;

/**
 * Created by Evolved8 on 12/19/16.
 */
public class NeuralNetLispInterface {
    public static void addNeuralNetFunctions(Environment env)
    {
        env.mapFunction("create-simple-lstm-network", createSimpleLSTMNetwork());
        env.mapFunction("create-simple-classification-lstm-network", createSimpleClassificationLSTMNetwork());
        env.mapFunction("create-simple-sequence-lstm", createSimpleSequenceLSTMNetwork());

        env.mapFunction("simple-lstm-learn-sequence", simpleLSTMLearnSequence());
        env.mapFunction("simple-lstm-learn-sequence-classes", simpleLSTMLearnSequenceClasses());

        env.mapFunction("simple-lstm-serialize-weights", simpleLSTMSerializeWeights());
        env.mapFunction("simple-lstm-get-raw-weights", simpleLSTMGetRawWeights());

        env.mapFunction("simple-lstm-load-serialized-weights", simpleLSTMLoadSerializedWeights());
        env.mapFunction("simple-lstm-set-raw-weights", simpleLSTMSetRawWeights());

        env.mapFunction("simple-lstm-extrapolate-sequence", simpleLSTMExtrapolateSequence());
        env.mapFunction("simple-lstm-view-sequence-output", simpleLSTMViewSequenceOutput());


        env.mapFunction("simple-lstm-serialize-node-state", simpleLSTMSerializeNodeState());
        env.mapFunction("simple-lstm-get-node-state", simpleLSTMGetNodeState());
        env.mapFunction("simple-lstm-set-node-state", simpleLSTMSetNodeState());
        env.mapFunction("simple-lstm-load-serialized-node-state", simpleLSTMLoadSerializedNodeState());

        env.mapFunction("simple-lstm-use-current-state-as-initial", setUseCurrentStateAsInitial());
        env.mapFunction("simple-lstm-use-default-initial-state", useDefaultInitialActivation());


        env.mapFunction("create-linked-slstm", createLinkedLSTM());
        env.mapFunction("create-linked-set-lstm", createLinkedLSTMSet()); // creates

        env.mapFunction("prediction-lstm-observe-predict", predictionLSTMObservePredict());
        env.mapFunction("prediction-lstm-get-best-prediction", PredictionLSTMgetBestPrediction());
        env.mapFunction("prediction-lstm-view-data", predictionLSTMViewData());
        env.mapFunction("prediction-lstm-allow-mid-sequence-matching", setAllowInitialMidSequenceMatching());
        env.mapFunction("prediction-lstm-get-serialized-data", predictionLSTMGetSerializedData());
        env.mapFunction("prediction-lstm-load-serialized-data", predictionLSTMLoadData());
        env.mapFunction("prediction-lstm-reset", predictionLSTMReset());
        env.mapFunction("prediction-lstm-set-prediction-tester", predictionLSTMSetDefinePredictionTester());
        env.mapFunction("linked-lstm-join", linkedLSTMJoin());
        env.mapFunction("linked-lstm-set-merge", linkedLSTMSetMerge());

        env.mapFunction("prediction-lstm-set-prediction-aggregator", PredictionLSTMSetBestPredictionAggregator());
        env.mapFunction("prediction-lstm-get-items", PredictionLSTMGetItems());
        env.mapFunction("prediction-lstm-set-max-error-per-input", PredictionLSTMMaxErrorPerInputVector());
        env.mapFunction("prediction-lstm-set-max-steps-per-input", predictionLSTMSetMaxLearningStepsPerInputVector());
        env.mapFunction("prediction-lstm-remove-all-items", PredictionLSTMRemoveAllItems());


        env.mapFunction("simple-slstm-add", simpleSLSTMAdd());
        env.mapFunction("simple-slstm-get-all-items", simpleSLSTMGetAllItems());
        env.mapFunction("simple-slstm-get-size", simpleSLSTMGetSize());
        env.mapFunction("simple-slstm-remove-first", simpleSLSTMRemoveFirst());
        env.mapFunction("simple-slstm-remove-last", simpleSLSTMRemoveLast());
        env.mapFunction("simple-slstm-undo-remove-last", simpleSLSTMUndoRemoveLast());
        env.mapFunction("simple-slstm-remove-all", simpleSLSTMRemoveAll());


        env.mapFunction("fast-make-binary-sequence-lstm", FastMakeBinarySequenceLSTM());
        env.mapFunction("fast-make-state-sequence-lstm", FastMakeStateSequenceLSTM());
        env.mapFunction("fast-forward-pass", FastForwardPass());
        env.mapFunction("fast-reset-to-initial-state", FastResetToInitialState());
        env.mapFunction("fast-randomize-weights", FastRandomizeWeigts());

        env.mapFunction("fast-calculate-error-deltas", FastCalculateErrorDeltas());

        env.mapFunction("fast-update-weights-rprop", FastUpdateWeightsRrop());
        env.mapFunction("fast-update-weights-gradient-descent", FastUpdateWeightsGradientDescent());

        env.mapFunction("fast-get-output-error", FastGetOutputError());

        env.mapFunction("fast-round-vector", FastRoundVector());

        env.mapFunction("fast-set-node-state", FastSetNodeState());
        env.mapFunction("fast-get-node-state", FastGetNodeState());

        env.mapFunction("fast-copy-lstm", FastCopyLSTM());

        env.mapFunction("fast-rounded-vector-equals", FastRoundedVectorEquals());

        env.mapFunction("fast-network-to-string", FastNetworkToString());
        env.mapFunction("fast-string-to-fast-network", FastStringToFastNetwork());

        env.mapFunction("fast-nodestate-to-string", FastNodestateToString());
        env.mapFunction("fast-string-to-nodestate", FastStringToNodestate());

        env.mapFunction("fast-num-to-tally-vector", FastNumToTallyVector());
        env.mapFunction("fast-tally-vector-to-num", FastTallyVectorToNum());

        env.mapFunction("fast-enum-vector-to-num", FastEnumVectorToNum());
        env.mapFunction("fast-num-to-enum-vector", FastNumToEnumVector());
    }

    public static SimpleFunctionTemplate FastNumToEnumVector()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastNumToEnumVector();
            }

            // First argument is a vector
            // Second argument is radiix
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, true, true);

                int value = (int)evaluatedArgs[0].getIntValue();
                int radiix = (int)evaluatedArgs[1].getIntValue();

                Value[] out = new Value[radiix];


                for (int i = 1; i <= radiix ;i++)
                {
                    if (i == value)
                        out[i-1] = NLispTools.makeValue(1);
                    else
                        out[i-1] = NLispTools.makeValue(0);
                }

                return NLispTools.makeValue(out);
            }
        };
    }


    public static SimpleFunctionTemplate FastEnumVectorToNum()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastEnumVectorToNum();
            }

            // First argument is a vector
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, true, true);

                float[] vector = getFloatData(evaluatedArgs[0]);

                int value = 0;

                for (int i = 0; i < vector.length ;i++)
                {
                    if (vector[i] > 0.5)
                    {
                        value = i+1;
                        break;
                    }
                }

                return NLispTools.makeValue(value);
            }
        };
    }


    public static SimpleFunctionTemplate FastTallyVectorToNum()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastTallyVectorToNum();
            }

            // First argument is a vector
            // [Optional] Second argument indicates if vector represents signed number
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, true, true);

                float[] vector = getFloatData(evaluatedArgs[0]);

                int sign = 1, offset = 0;
                if (evaluatedArgs.length > 1)
                {

                    if (!evaluatedArgs[1].isNull())
                    {
                        offset = 1;
                        if (vector[0] > 0.5)
                        {
                            sign = -1;
                        }
                        else
                            sign = 1;
                    }
                }

                int value = 0;

                for (int i = offset; i < vector.length ;i++)
                {
                    if (vector[i] > 0.5)
                    {
                        value += sign;
                    }
                }

                return NLispTools.makeValue(value);
            }
        };
    }


    public static SimpleFunctionTemplate FastNumToTallyVector()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastNumToTallyVector();
            }

            // First argument is an integer
            // Second argument is the radiix
            // [Optional] Third argument is a flag indicating whether result is signed
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, true, true);

                int num = (int)evaluatedArgs[0].getIntValue();
                int radiix = (int)evaluatedArgs[1].getIntValue();

                int offset = 0;
                if (evaluatedArgs.length > 2 && !evaluatedArgs[2].isNull())
                {
                    offset = 1;
                }

                Value[] out = new Value[radiix + offset];
                if (offset > 0)
                {
                    if (num < 0)
                        out[0] = NLispTools.makeValue(1);
                    else
                        out[0] = NLispTools.makeValue(0);
                }

                for (int i = 0; i < radiix;i++)
                {
                    if (i < Math.abs(num))
                    {
                        out[i + offset] = NLispTools.makeValue(1);
                    }
                    else
                        out[i + offset] = NLispTools.makeValue(0);
                }

                return NLispTools.makeValue(out);
            }
        };
    }


    public static SimpleFunctionTemplate FastRoundVector()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastRoundVector();
            }

            // First argument is the data list
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                return roundDataValues(evaluatedArgs[0]);
            }
        };
    }

    public static SimpleFunctionTemplate FastRoundedVectorEquals()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastRoundedVectorEquals();
            }

            // First argument is the first data vector list
            // Second argument is the second data vector list
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, true);

                float[] first = getFloatData(evaluatedArgs[0]);
                float[] second = getFloatData(evaluatedArgs[1]);

                if (first.length != second.length)
                    return NLispTools.makeValue(false);
                for (int i = 0;i < first.length;i++)
                {
                    if (roundToInt(first[i]) != roundToInt(second[i]))
                        return NLispTools.makeValue(false);
                }
                return evaluatedArgs[1];
            }
        };
    }

    public static SimpleFunctionTemplate FastMakeBinarySequenceLSTM()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastMakeBinarySequenceLSTM();
            }

            // First argument is the input/output width
            // Second argument is a number of memory cell states

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, true);

                int numInputOutput = (int)evaluatedArgs[0].getIntValue();
                int memorycellStates = (int)evaluatedArgs[1].getIntValue();

                LSTMNetworkProxy proxy = LSTMNetworkProxy.makeStandardBinarySequenceNetwork(numInputOutput, memorycellStates);
                return ExtendedFunctions.makeValue(proxy);
            }
        };
    }


    public static SimpleFunctionTemplate FastMakeStateSequenceLSTM()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastMakeStateSequenceLSTM();
            }

            // First argument is the input/output width
            // Second argument is a number of memory cell states

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, true);

                int numInputOutput = (int)evaluatedArgs[0].getIntValue();
                int memorycellStates = (int)evaluatedArgs[1].getIntValue();

                LSTMNetworkProxy proxy = LSTMNetworkProxy.makeStandardStateSequenceNetwork(numInputOutput, memorycellStates);
                return ExtendedFunctions.makeValue(proxy);
            }
        };
    }

    static float[] getFloatData(Value data)
    {
        Value[] vec = data.getList();
        float[] out = new float[vec.length];
        for (int i = 0; i < vec.length; i++)
            out[i] = (float)vec[i].getIntValue();
        return out;
    }

    static Value getLispDataValue(float[] data)
    {
        Value[] out = new Value[data.length];
        for (int i = 0; i < data.length;i++)
        {
            out[i] = NLispTools.makeValue(data[i]);
        }
        return NLispTools.makeValue(out);
    }

    static Value roundDataValues(Value data)
    {
        Value[] out = data.getList();
        for (int i = 0;i < out.length;i++)
        {
            if (out[i].getFloatValue() > 0.5F)
                out[i] = NLispTools.makeValue(1);
            else
                out[i] = NLispTools.makeValue(0);
        }
        return NLispTools.makeValue(out);
    }

    public static SimpleFunctionTemplate FastNodestateToString()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastNodestateToString();
            }

            // First argument is the NodeState

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                LSTMNetworkProxy.NodeState state = (LSTMNetworkProxy.NodeState)evaluatedArgs[0].getObjectValue();


                return NLispTools.makeValue(state.serialize());
            }
        };
    }

    public static SimpleFunctionTemplate FastStringToNodestate()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastStringToNodestate();
            }

            // First argument is a string nodestae

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                String nodeStateString = evaluatedArgs[0].getString();
                LSTMNetworkProxy.NodeState state = LSTMNetworkProxy.NodeState.createNodeState(nodeStateString);
                return ExtendedFunctions.makeValue(state);
            }
        };
    }


    public static SimpleFunctionTemplate FastNetworkToString()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastNetworkToString();
            }

            // First argument is the LSTM

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                LSTMNetworkProxy proxy = (LSTMNetworkProxy)evaluatedArgs[0].getObjectValue();

                String serialized = proxy.serialize();
                return NLispTools.makeValue(serialized);
            }
        };
    }

    public static SimpleFunctionTemplate FastStringToFastNetwork()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastStringToFastNetwork();
            }

            // First argument is a string

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                String networkString = evaluatedArgs[0].getString();
                float[] data =  LSTMNetworkProxy.deserializeFloatString(networkString);
                return ExtendedFunctions.makeValue(LSTMNetworkProxy.make(data));
            }
        };
    }

    public static SimpleFunctionTemplate FastRandomizeWeigts()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastRandomizeWeigts();
            }

            // First argument is the LSTM
            // Optional second argument is an double fraction of weights to randomize
            // Optional third argument is a constant value to set a random fraction of
            // the weights to.  If this value isn't set then a random fraction of the
            // weights be set to a random value

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, true, true);

                LSTMNetworkProxy proxy = (LSTMNetworkProxy)evaluatedArgs[0].getObjectValue();
                if (evaluatedArgs.length > 1 && !evaluatedArgs[1].isNull())
                {
                    float randomizeFraction = (float)evaluatedArgs[1].getFloatValue();
                    if (evaluatedArgs.length > 2)
                    {
                        float randomSetValue = (float)evaluatedArgs[2].getFloatValue();
                        proxy.randomizeNetworkWeights(randomizeFraction,randomSetValue );
                    }
                    else
                        proxy.randomizeNetworkWeights(randomizeFraction);
                }
                else
                    proxy.randomizeNetworkWeights();
                return evaluatedArgs[0];
            }
        };
    }



    public static SimpleFunctionTemplate FastResetToInitialState()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastResetToInitialState();
            }

            // First argument is the LSTM

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, true);

                LSTMNetworkProxy proxy = (LSTMNetworkProxy)evaluatedArgs[0].getObjectValue();
                proxy.resetNetworkToInitialState();
                return evaluatedArgs[0];
            }
        };
    }


    public static SimpleFunctionTemplate FastForwardPass()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastForwardPass();
            }

            // First argument is the LSTM
            // Second argument is an input vector

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, true);

                LSTMNetworkProxy proxy = (LSTMNetworkProxy)evaluatedArgs[0].getObjectValue();
                float[] input = getFloatData(evaluatedArgs[1]);
                float[] output = proxy.executeForwardPass(input);
                return getLispDataValue(output);
            }
        };
    }


    public static SimpleFunctionTemplate FastCopyLSTM()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastCopyLSTM();
            }

            // First argument is the LSTM

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                LSTMNetworkProxy proxy = (LSTMNetworkProxy)evaluatedArgs[0].getObjectValue();

                return ExtendedFunctions.makeValue(LSTMNetworkProxy.duplicate(proxy));
            }
        };
    }


    public static SimpleFunctionTemplate FastGetNodeState()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastGetNodeState();
            }

            // First argument is the LSTM

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                LSTMNetworkProxy proxy = (LSTMNetworkProxy)evaluatedArgs[0].getObjectValue();

                return ExtendedFunctions.makeValue(proxy.getCurrentNodeState());
            }
        };
    }

    public static SimpleFunctionTemplate FastSetNodeState()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastSetNodeState();
            }

            // First argument is the LSTM
            // Second argument is a NodeState object

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, true);

                LSTMNetworkProxy proxy = (LSTMNetworkProxy)evaluatedArgs[0].getObjectValue();

                LSTMNetworkProxy.NodeState state = (LSTMNetworkProxy.NodeState)evaluatedArgs[1].getObjectValue();
                proxy.setNodeState(state);
                return evaluatedArgs[0];
            }
        };
    }


    public static SimpleFunctionTemplate FastUpdateWeightsRrop()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastUpdateWeightsRrop();
            }

            // First argument is the LSTM

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                LSTMNetworkProxy proxy = (LSTMNetworkProxy)evaluatedArgs[0].getObjectValue();
                float[] input = getFloatData(evaluatedArgs[1]);
                proxy.updateWeights(LSTMNetwork.WeightUpdateType.RPROP);
                return evaluatedArgs[0];
            }
        };
    }

    public static SimpleFunctionTemplate FastUpdateWeightsGradientDescent()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastUpdateWeightsGradientDescent();
            }

            // First argument is the LSTM

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                LSTMNetworkProxy proxy = (LSTMNetworkProxy)evaluatedArgs[0].getObjectValue();
                proxy.updateWeights(LSTMNetwork.WeightUpdateType.DEFAULT);

                return evaluatedArgs[0];
            }
        };
    }

    public static SimpleFunctionTemplate FastCalculateErrorDeltas()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastCalculateErrorDeltas();
            }

            // First argument is the LSTM
            // Second argument is the expected output
            // Optional third argument is a flag indicating whether to round the expected output
            // Optional fourth argument is a flag indicating whether to round the calculated output
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, true, true);

                LSTMNetworkProxy proxy = (LSTMNetworkProxy)evaluatedArgs[0].getObjectValue();
                float[] expectedOutput = getFloatData(evaluatedArgs[1]);
                boolean roundExpected =  (evaluatedArgs.length > 2)?!evaluatedArgs[2].isNull():false;
                boolean roundCalculatedExpected =  (evaluatedArgs.length > 3)?!evaluatedArgs[3].isNull():false;

                float error = proxy.getOutputError(expectedOutput, roundExpected, roundCalculatedExpected, true);
                return NLispTools.makeValue(error);
            }
        };
    }


    public static SimpleFunctionTemplate FastGetOutputError()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) FastGetOutputError();
            }

            // First argument is the LSTM
            // Second argument is the expected output
            // Optional third argument is a flag indicating whether to round the expected output
            // Optional fourth argument is a flag indicating whether to round the calculated output
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, true, true);

                LSTMNetworkProxy proxy = (LSTMNetworkProxy)evaluatedArgs[0].getObjectValue();
                float[] expectedOutput = getFloatData(evaluatedArgs[1]);
                boolean roundExpected =  (evaluatedArgs.length > 2)?!evaluatedArgs[2].isNull():false;
                boolean roundCalculatedExpected =  (evaluatedArgs.length > 3)?!evaluatedArgs[3].isNull():false;

                float error = proxy.getOutputError(expectedOutput, roundExpected, roundCalculatedExpected, false);
                return NLispTools.makeValue(error);
            }
        };
    }

    public static SimpleFunctionTemplate PredictionLSTMMaxErrorPerInputVector()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) PredictionLSTMMaxErrorPerInputVector();
            }

            // First argument is the PredictionLSTM
            // Second argument is a numeric error float

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, true);

                SequencePredictor lstmSet = (SequencePredictor)evaluatedArgs[0].getObjectValue();
                double maxError = evaluatedArgs[1].getFloatValue();
                lstmSet.setMaxLearningError(maxError);

                return evaluatedArgs[0];
            }
        };
    }

    public static SimpleFunctionTemplate predictionLSTMSetMaxLearningStepsPerInputVector()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) predictionLSTMSetMaxLearningStepsPerInputVector();
            }

            // First argument is the PredictionLSTM
            // Second argument is the max number of steps

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, true);

                SequencePredictor lstmSet = (SequencePredictor)evaluatedArgs[0].getObjectValue();
                int maxSteps = (int)evaluatedArgs[1].getIntValue();
                lstmSet.setMaxLearningSteps(maxSteps);

                return evaluatedArgs[0];
            }
        };
    }



    public static SimpleFunctionTemplate predictionLSTMViewData()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) predictionLSTMViewData();
            }

            // First argument is the SLSTMSet

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                SequencePredictor llstm = (SequencePredictor)evaluatedArgs[0].getObjectValue();
                String data = llstm.getDataView();

                return NLispTools.makeValue(data);
            }
        };
    }



    public static SimpleFunctionTemplate predictionLSTMGetSerializedData()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) predictionLSTMGetSerializedData();
            }

            // First argument is the SLSTMSet

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                SequencePredictor pslstm = (SequencePredictor)evaluatedArgs[0].getObjectValue();
                String serialized = pslstm.serializedForm();
                return NLispTools.makeValue(serialized);

            }
        };
    }


    public static SimpleFunctionTemplate predictionLSTMLoadData()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) predictionLSTMLoadData();
            }

            // First argument is the SLSTMSet

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, true);

                SequencePredictor pslstm = (SequencePredictor)evaluatedArgs[0].getObjectValue();
                String data = evaluatedArgs[1].getString();
                pslstm.loadData(data);

                return evaluatedArgs[0];
            }
        };
    }


    public static SimpleFunctionTemplate PredictionLSTMRemoveAllItems()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) PredictionLSTMRemoveAllItems();
            }

            // First argument is the SLSTMSet

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                SequencePredictor lstmSet = (SequencePredictor)evaluatedArgs[0].getObjectValue();
                lstmSet.clearAllSLSTMs();
                return evaluatedArgs[0];
            }
        };
    }


    public static SimpleFunctionTemplate simpleSLSTMRemoveAll()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleSLSTMRemoveAll();
            }

            // First value is the SLSTM
            // Returns the original SLSTM
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                SequenceLSTM lstm = (SequenceLSTM)evaluatedArgs[0].getObjectValue();
                lstm.clear();
                return evaluatedArgs[0];
            }
        };
    }

    public static SimpleFunctionTemplate simpleSLSTMRemoveLast()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleSLSTMRemoveLast();
            }

            // First value is the SLSTM
            // Returns the original SLSTM (not the value removed)
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                SequenceLSTM lstm = (SequenceLSTM)evaluatedArgs[0].getObjectValue();
                lstm.removeLast();
                return evaluatedArgs[0];
            }
        };
    }

    public static SimpleFunctionTemplate simpleSLSTMUndoRemoveLast()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleSLSTMUndoRemoveLast();
            }

            // First value is the SLSTM
            // Returns the original SLSTM (not the value removed)
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                SequenceLSTM lstm = (SequenceLSTM)evaluatedArgs[0].getObjectValue();
                lstm.undoRemoveLast();
                return evaluatedArgs[0];
            }
        };
    }


    public static SimpleFunctionTemplate simpleSLSTMRemoveFirst()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleSLSTMRemoveFirst();
            }

            // First value is the SLSTM
            // Returns the previous first element
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                SequenceLSTM lstm = (SequenceLSTM)evaluatedArgs[0].getObjectValue();
                Vector oldFirst = lstm.removeFirst();
                if (oldFirst != null)
                {
                    return NLispTools.makeValue(oldFirst.raw());
                }
                else
                    return Environment.getNull();
            }
        };
    }




    public static SimpleFunctionTemplate simpleSLSTMGetAllItems()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleSLSTMGetAllItems();
            }

            // First value is the SLSTM
            // Returns the Vectors in the SLSTM as a list
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                SequenceLSTM lstm = (SequenceLSTM)evaluatedArgs[0].getObjectValue();
                Vector[] out = lstm.getCurrentSequence();



                return NLispTools.makeValue(AITools.mapValues(out, new LispValueMapper<Vector>() {

                    @Override
                    public Value map(Vector input, int index)
                    {
                        return NLispTools.makeValue(input.raw());
                    }
                })  );
            }
        };
    }


    public static SimpleFunctionTemplate simpleSLSTMGetSize()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleSLSTMGetSize();
            }

            // First value is the SLSTM

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                SequenceLSTM lstm = (SequenceLSTM)evaluatedArgs[0].getObjectValue();
                return NLispTools.makeValue(lstm.getSequenceLength());
            }
        };
    }


    public static SimpleFunctionTemplate simpleSLSTMAdd()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleSLSTMAdd();
            }

            // First value is the SLSTM
            // second value is a list representing the vector to add
            // Third argument is the max number of steps to iterate
            // Fourth arugment is the maximum acceptable error across all
            // training inputs that will cause the iteration to exit early
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(4, false, true);

                SequenceLSTM lstm = (SequenceLSTM)evaluatedArgs[0].getObjectValue();
                Value valueToAdd = evaluatedArgs[1];
                int maxSteps = (int)evaluatedArgs[2].getIntValue();
                double maxAcceptableError = evaluatedArgs[3].getFloatValue();




                double[] result = lstm.add(listToVector(valueToAdd), maxSteps, maxAcceptableError);

                Value[] lispResult = new Value[3];
                LSTMLearningResult learningResult = LSTMLearningResult.make(result);
                lispResult[0] = NLispTools.makeValue(learningResult.maxStepError);
                lispResult[1] = NLispTools.makeValue(learningResult.numIterations);
                lispResult[2] = NLispTools.makeValue(learningResult.stepErrors);
                return NLispTools.makeValue(lispResult);
            }
        };
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

                LSTMNetwork lstm  = (LSTMNetwork)evaluatedArgs[0].getObjectValue();
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
                boolean useGradientDescent = false;


                String serializedWeights = null;
                if (evaluatedArgs.length>3 && !evaluatedArgs[3].isNull())
                {
                    serializedWeights = evaluatedArgs[3].getString();
                }

                if (evaluatedArgs.length > 4)
                    useGradientDescent = !evaluatedArgs[3].isNull();

                LSTMNetwork lstm =null ;

                if (!useGradientDescent)
                    lstm = NNTools.getStandardLSTM(numInputNodes, numMemoryCellStateNodes, numOutputNodes, serializedWeights);
                else
                    lstm = NNTools.getStandardGDLSTM(numInputNodes, numMemoryCellStateNodes, numOutputNodes, serializedWeights);

                return ExtendedFunctions.makeValue(lstm);
            }
        };
    }

    public static SimpleFunctionTemplate createSimpleClassificationLSTMNetwork()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) createSimpleClassificationLSTMNetwork();
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

                LSTMNetwork lstm = NNTools.getStandardClassificationLSTM(numInputNodes, numMemoryCellStateNodes, numOutputNodes, serializedWeights);
                lstm.setRoundOutput(false);
                return ExtendedFunctions.makeValue(lstm);
            }
        };
    }

    public static SimpleFunctionTemplate createSimpleSequenceLSTMNetwork()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) createSimpleSequenceLSTMNetwork();
            }

            // First argument is the number of input nodes
            // Second argument is the number of nodes in the memory cell state
            // Optional third argument is the set of serialized weights to initialize the
            // weights with
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, true, true);
                int numInputNodes = (int)evaluatedArgs[0].getIntValue();
                int numMemoryCellStateNodes = (int)evaluatedArgs[1].getIntValue();

                String serializedWeights = null;
                if (evaluatedArgs.length>2 && !evaluatedArgs[2].isNull())
                {
                    serializedWeights = evaluatedArgs[2].getString();
                }

                SequenceLSTM slstm = NNTools.getStandardSequenceLSTM(numInputNodes, numMemoryCellStateNodes, serializedWeights);

                return ExtendedFunctions.makeValue(slstm);
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
                }), maxSteps, maxAcceptableError);

                Value[] lispResult = new Value[3];
                LSTMLearningResult learningResult = LSTMLearningResult.make(result);
                lispResult[0] = NLispTools.makeValue(learningResult.maxStepError);
                lispResult[1] = NLispTools.makeValue(learningResult.numIterations);
                lispResult[2] = NLispTools.makeValue(learningResult.stepErrors);
                return NLispTools.makeValue(lispResult);
            }
        };
    }


    public static SimpleFunctionTemplate simpleLSTMLearnSequenceClasses()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMLearnSequenceClasses();
            }

            // First argument is LSTM
            // Second argument is a list of training input of vector category pairs.
            //  ( ( [vector_i] [category_i]) ( [vector_i+1] [category_i+1]) ... )

            // Third argument is the max number of possible classes
            // Forth argument is max number of steps
            // Fifth arugment is the maximum acceptable error across all
            // training inputs that will cause the iteration to exit early

            // Optional Sixed argument allows weights to be randomized if the
            // lstm gets into a local minimum above the acceptable error
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(5, true, true);

                LSTMNetwork lstm = (LSTMNetwork)evaluatedArgs[0].getObjectValue();
                Value[] trainingSequences = evaluatedArgs[1].getList();

                Vector[][] vectorInputs = new Vector[trainingSequences.length][];
                int[] categories = new int[trainingSequences.length];
                Value[] trainingPair;
                Value vectorList, trainingClass;
                for (int i=0;i<trainingSequences.length;i++)
                {
                    trainingPair = trainingSequences[i].getList();
                    vectorList = trainingPair[0];
                    trainingClass = trainingPair[1];

                    vectorInputs[i] = AITools.mapValues(vectorList.getList(), new VectorValueMapper<Value>() {
                        @Override
                        public Vector map(Value input, int index)
                        {
                            return listToVector(input);
                        }
                    });
                    categories[i] = (int)trainingClass.getIntValue();
                }


                int numClasses = (int)evaluatedArgs[2].getIntValue();


                double maxSteps = evaluatedArgs[3].getFloatValue();
                double maxAcceptableError = evaluatedArgs[4].getFloatValue();

                boolean allowWeightRandomization = true;

                if (evaluatedArgs.length > 5)
                {
                    allowWeightRandomization = !evaluatedArgs[5].isNull();
                }

                lstm.setAllowRerollingWeights(allowWeightRandomization);



                double[] result = lstm.learnPatternClass(vectorInputs, categories, numClasses, maxSteps, maxAcceptableError);

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

    public static SimpleFunctionTemplate simpleLSTMGetRawWeights()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMGetRawWeights();
            }

            // First argument is LSTM
            // Returns a string hashtable of the weights, suitable for use in simple-lstm-set-raw-weights
            //
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                LSTMNetwork lstm = (LSTMNetwork)evaluatedArgs[0].getObjectValue();

                HashMap<String, WeightMatrix> rawWeights = lstm.getLinkWeightMap(true);
                HashMap<String, Value> out = new HashMap<String, Value>();
                for (String key: rawWeights.keySet())
                {
                    out.put(key, NLispTools.makeValue(rawWeights.get(key).getWeights()));
                }
                return new StringHashtableValue(out);
            }
        };
    }


    public static SimpleFunctionTemplate simpleLSTMSerializeNodeState()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSerializeNodeState();
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

    public static SimpleFunctionTemplate simpleLSTMGetNodeState()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMGetNodeState();
            }

            // First argument is LSTM
            // Returns a string hashtable of the network node activations, suitable for use in
            // simple-lstm-set-node-state
            // or
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                LSTMNetwork lstm = (LSTMNetwork)evaluatedArgs[0].getObjectValue();
                HashMap<String, Vector> activationMap = lstm.getNetworkActivationSnapshot();

                HashMap<String, Value> out = new HashMap<String, Value>();

                for (String key: activationMap.keySet())
                {
                    out.put(key, NLispTools.makeValue(activationMap.get(key).raw()));
                }

                return new StringHashtableValue(out);
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

    public static SimpleFunctionTemplate simpleLSTMSetRawWeights()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetRawWeights();
            }

            // First argument is LSTM
            // Second argument is a string hashtable representing the raw weights, such as that returned
            // by simple-lstm-get-raw-weights
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, false);

                LSTMNetwork lstm = (LSTMNetwork)evaluatedArgs[0].getObjectValue();
                HashMap<String, Value> weightMap = evaluatedArgs[1].getStringHashtable();
                HashMap<String, WeightMatrix> input = new HashMap<String, WeightMatrix>();

                for (String key: weightMap.keySet())
                {
                    input.put(key, listToWeightMatrix(weightMap.get(key)));
                }

                lstm.setBufferedLinkWeightMap(input);
                lstm.loadbufferedLinkWeights();

                return evaluatedArgs[0];
            }
        };
    }


    public static SimpleFunctionTemplate simpleLSTMLoadSerializedNodeState()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMLoadSerializedNodeState();
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


    public static SimpleFunctionTemplate simpleLSTMSetNodeState()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetNodeState();
            }

            // First argument is LSTM
            // Second argument is a string hashtable representing the node activations, such as that
            // returned by simple-lstm-get-node-state
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, true, true);

                LSTMNetwork lstm = (LSTMNetwork)evaluatedArgs[0].getObjectValue();
                HashMap<String, Vector> newActivationMap = new HashMap<String, Vector>();

                HashMap<String, Value> activationMap = evaluatedArgs[1].getStringHashtable();

                for (String key: activationMap.keySet())
                {
                    newActivationMap.put(key, listToVector(activationMap.get(key)));
                }
                lstm.setNetworkActivation(newActivationMap);
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

    public static SimpleFunctionTemplate simpleLSTMViewSequenceOutput()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMViewSequenceOutput();
            }

            // First argument is LSTM
            // Second argument is the sequence of input vectors
            // ( {0, 1}*, {0, 1}*, {0, 1}*, ...)

            // Third argument is used to determine if you want to drive the input
            // from the context of the network's current state or whether to initialize
            // the network first.

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, true, true);

                LSTMNetwork lstm = (LSTMNetwork)evaluatedArgs[0].getObjectValue();
                Value[] seedInput = evaluatedArgs[1].getList();

                boolean continuePriorState = false;


                if (evaluatedArgs.length > 2)
                {
                    continuePriorState = !evaluatedArgs[2].isNull();
                }

                Vector[] extrapolatedResult = lstm.viewSequenceOutput(AITools.mapValues( seedInput, new VectorValueMapper<Value>() {
                    @Override
                    public Vector map(Value input, int index)
                    {
                        return listToVector(input);
                    }
                }), continuePriorState);

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


    // SLSTMSet

    public static SimpleFunctionTemplate createLinkedLSTM()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) createLinkedLSTM();
            }

            // Uses keyword parameters
            // :SEGMENT-INPUT-OUTPUT-NODE-COUNT
            // :SEGMENT-MEMORY-CELL-NODE-COUNT
            // :BUFFER-SIZE
            // :MAX-PER-ITEM-LEARNING-STEPS
            // :MAX-ERROR-FRACTION
            // :MAX-CONSECUTIVE-FAILURES
            // :VECTOR-VIEWER-LAMBDA
            // :ALLOW-CONTINUOUS-PREDICTION

            // should be a moderately large number

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                Value inputNodeCount = keywordParameters.get(":SEGMENT-INPUT-OUTPUT-NODE-COUNT");
                Value memoryCellNodeCount = keywordParameters.get(":SEGMENT-MEMORY-CELL-NODE-COUNT");
                Value bufferSize = keywordParameters.get(":BUFFER-SIZE");
                Value maxStepsValue = keywordParameters.get(":MAX-PER-ITEM-LEARNING-STEPS");
                Value maxErrorValue = keywordParameters.get(":MAX-ERROR-FRACTION");
                Value maxFailureCountValue = keywordParameters.get(":MAX-CONSECUTIVE-FAILURES");
                Value viewerLambda = keywordParameters.get(":VECTOR-VIEWER-LAMBDA");
                Value continuousPrediction = keywordParameters.get(":ALLOW-CONTINUOUS-PREDICTION");

                LinkedLSTM.Builder builder = LinkedLSTM.getLinkedLSTMBuider();

                int maxSteps = 150;
                if (maxStepsValue != null)
                {
                    maxSteps = (int) maxStepsValue.getIntValue();
                }

                double maxError = 0.1;
                if (maxErrorValue != null)
                {
                    maxError = maxErrorValue.getFloatValue();
                }
                int failureCount = 2;
                if (maxFailureCountValue != null)
                {
                    failureCount = (int) maxFailureCountValue.getIntValue();
                }

                boolean allowFailingSegmentsToPredictP = true;

                if (continuousPrediction!=null)
                {
                    allowFailingSegmentsToPredictP = !continuousPrediction.isNull();
                }

                builder.
                        setNumMemoryCellNodes((int) memoryCellNodeCount.getIntValue()).
                        setNumInputNodes((int) inputNodeCount.getIntValue()).
                        setLSTMBufferSize((int) bufferSize.getIntValue()).
                        setMaxLearningSteps(maxSteps).
                        setMaxConsecutiveFailures(failureCount).
                        setMaxError(maxError).
                        setSimpleMaxPredictionAggregator();

                if (viewerLambda != null)
                {

                    builder.setCustomVectorViewer(lambdaToVectorViewer((Lambda)viewerLambda.getLambda(), env));
                }

                if (allowFailingSegmentsToPredictP)
                {
                    builder.setAllowFailingLSTMsToPredict();
                }
                else
                {
                    builder.setPreventFailingLSTMsToPredict();
                }
                LinkedLSTM llstm = builder.build();

                return ExtendedFunctions.makeValue(llstm);
            }
        };
    }

    public static SimpleFunctionTemplate createLinkedLSTMSet()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) createLinkedLSTMSet();
            }

            // Uses keyword parameters
            // :SEGMENT-INPUT-OUTPUT-NODE-COUNT
            // :SEGMENT-MEMORY-CELL-NODE-COUNT
            // :BUFFER-SIZE
            // :MAX-PER-ITEM-LEARNING-STEPS
            // :MAX-ERROR-FRACTION
            // :MAX-CONSECUTIVE-FAILURES
            // :VECTOR-VIEWER-LAMBDA
            // :ALLOW-CONTINUOUS-PREDICTION

            // should be a moderately large number

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                Value inputNodeCount = keywordParameters.get(":SEGMENT-INPUT-OUTPUT-NODE-COUNT");
                Value memoryCellNodeCount = keywordParameters.get(":SEGMENT-MEMORY-CELL-NODE-COUNT");
                Value bufferSize = keywordParameters.get(":BUFFER-SIZE");
                Value maxStepsValue = keywordParameters.get(":MAX-PER-ITEM-LEARNING-STEPS");
                Value maxErrorValue = keywordParameters.get(":MAX-ERROR-FRACTION");
                Value maxFailureCountValue = keywordParameters.get(":MAX-CONSECUTIVE-FAILURES");
                Value viewerLambda = keywordParameters.get(":VECTOR-VIEWER-LAMBDA");
                Value continuousPrediction = keywordParameters.get(":ALLOW-CONTINUOUS-PREDICTION");

                LinkedLSTMSet.Builder builder = LinkedLSTMSet.getBuider();

                int maxSteps = 150;
                if (maxStepsValue != null)
                {
                    maxSteps = (int) maxStepsValue.getIntValue();
                }

                double maxError = 0.1;
                if (maxErrorValue != null)
                {
                    maxError = maxErrorValue.getFloatValue();
                }
                int failureCount = 2;
                if (maxFailureCountValue != null)
                {
                    failureCount = (int) maxFailureCountValue.getIntValue();
                }

                boolean allowFailingSegmentsToPredictP = true;

                if (continuousPrediction!=null)
                {
                    allowFailingSegmentsToPredictP = !continuousPrediction.isNull();
                }

                builder.
                        setNumMemoryCellNodes((int) memoryCellNodeCount.getIntValue()).
                        setNumInputNodes((int) inputNodeCount.getIntValue()).
                        setLSTMBufferSize((int) bufferSize.getIntValue()).
                        setMaxLearningSteps(maxSteps).
                        setMaxConsecutiveFailures(failureCount).
                        setMaxError(maxError).
                        setSimpleMaxPredictionAggregator();

                if (viewerLambda != null)
                {

                    builder.setCustomVectorViewer(lambdaToVectorViewer((Lambda)viewerLambda.getLambda(), env));
                }

                if (allowFailingSegmentsToPredictP)
                {
                    builder.setAllowFailingLSTMsToPredict();
                }
                else
                {
                    builder.setPreventFailingLSTMsToPredict();
                }
                LinkedLSTMSet llstm = builder.build();

                return ExtendedFunctions.makeValue(llstm);
            }
        };
    }


    public static SimpleFunctionTemplate predictionLSTMObservePredict()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) predictionLSTMObservePredict();
            }

            // First argument is the LinkedLSTM or LSTMSet
            // Second argument is the vector of a new observation.  Should have as many dimensions
            // as the number of input nodes defined in the SLSTMSet
            // Third argument is a boolean parameter indicating if the system can laern


            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, true, true);

                SequencePredictor predictor = (SequencePredictor)evaluatedArgs[0].getObjectValue();


                Vector[] predictions = null;
                if (evaluatedArgs.length  ==  3)
                {
                    Value newInput = evaluatedArgs[1];
                    Value createNew = evaluatedArgs[2];
                    predictions = predictor.observePredictNext(listToVector(newInput), !createNew.isNull());
                }
                else
                {
                    predictions = predictor.observePredictNext();
                }

                if (predictions != null)
                    return NLispTools.makeValue( AITools.mapValues(predictions, new LispValueMapper<Vector>()
                    {

                                @Override
                                public Value map(Vector input, int index)
                                {
                                    if (input != null)
                                        return NLispTools.makeValue(input.raw());
                                    else
                                        return Environment.getNull();
                                }
                            }
                    ));
                else
                {
                    return Environment.getNull();
                }


            }
        };
    }

    public static SimpleFunctionTemplate PredictionLSTMgetBestPrediction()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) PredictionLSTMgetBestPrediction();
            }

            // First argument is the SLSTMSet

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, false);

                SequencePredictor predictor = (SequencePredictor)evaluatedArgs[0].getObjectValue();
                Vector best = predictor.getBestPrediction();
                if (best != null)
                {
                    return NLispTools.makeValue(best.raw());
                }
                else
                {
                    return Environment.getNull();
                }



            }
        };
    }

    public static SimpleFunctionTemplate setAllowInitialMidSequenceMatching()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) setAllowInitialMidSequenceMatching();
            }

            // First argument is the SequencePredictor
            // Second argument is a boolean parameter indicator whether
            // to allow mid-sequence matching


            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, false);

                SequencePredictor llstm  = (SequencePredictor)evaluatedArgs[0].getObjectValue();
                boolean allowMidSequence = !evaluatedArgs[1].isNull();

                llstm.setAllowMidSequenceInitialPrediction(allowMidSequence);

                return evaluatedArgs[0];

            }
        };
    }



    public static SimpleFunctionTemplate simpleLSTMSetCommitNewSLSTMs()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetCommitNewSLSTMs();
            }

            // First argument is the SLSTMSet
            // Optional Second argument is a boolean parameter indicating whether
            // to save temp slstm if possible, defaults to true
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, true, true);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();
                boolean savePatternTempBuffer = true;

                if (evaluatedArgs.length > 1)
                {
                    savePatternTempBuffer = !evaluatedArgs[1].isNull();
                }
                lstmSet.markIndexState(savePatternTempBuffer);
                return evaluatedArgs[0];

            }
        };
    }

    public static SimpleFunctionTemplate predictionLSTMReset()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) predictionLSTMReset();
            }

            // First argument is the SequencePredictor

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                SequencePredictor llstm = (SequencePredictor)evaluatedArgs[0].getObjectValue();
                llstm.clearAllPredictions();
                return evaluatedArgs[0];

            }
        };
    }

    public static SimpleFunctionTemplate simpleLSTMSetFlushTempBuffer()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetFlushTempBuffer();
            }

            // First argument is the SLSTMSet
            // Optional second argument is a boolean parameter indicating whether to flush
            // the buffer without adding them to the set.  This just clears the buffer
            //
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, true, true);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();
                boolean flushWithoutAdditionP = false;

                if (evaluatedArgs.length > 1 && !evaluatedArgs[1].isNull())
                {
                    flushWithoutAdditionP = true;
                }
                lstmSet.flushTempBuffer(flushWithoutAdditionP);
                return evaluatedArgs[0];

            }
        };
    }

    public static SimpleFunctionTemplate simpleLSTMRollbackSLSTMSet()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMRollbackSLSTMSet();
            }

            // First argument is the SLSTMSet
            // Optional second argument is a boolean parameter indicating whether to clear
            // the temp slstm.  Considered true if second argument is missing

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, true, false);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();

                boolean clearTemp = true;
                if (evaluatedArgs.length > 1)
                    clearTemp = !evaluatedArgs[1].isNull();

                lstmSet.restoreState(clearTemp);
                return evaluatedArgs[0];

            }
        };
    }

    public static SimpleFunctionTemplate simpleLSTMSetGetAggregateState()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetGetAggregateState();
            }

            // First argument is the SLSTMSet

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, true, true);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();
                Vector aggregateState = lstmSet.getAggregatePrediction();

                if (aggregateState != null)
                    return NLispTools.makeValue(aggregateState.raw());
                else
                    return Environment.getNull();


            }
        };
    }


    public static SimpleFunctionTemplate PredictionLSTMGetItems()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) PredictionLSTMGetItems();
            }

            // First argument is the SequencePredictor

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                SequencePredictor lstmSet = (SequencePredictor)evaluatedArgs[0].getObjectValue();
                SequenceLSTM[] members = lstmSet.getMembers();
                return NLispTools.makeValue(AITools.mapValues(members, new LispValueMapper<SequenceLSTM>() {
                    @Override
                    public Value map(SequenceLSTM input, int index)
                    {
                        return ExtendedFunctions.makeValue(input);
                    }
                }));
            }
        };
    }

    public static SimpleFunctionTemplate PredictionLSTMSetBestPredictionAggregator()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) PredictionLSTMSetBestPredictionAggregator();
            }

            // First argument is the SLSTMSet
            // Second argument is an lambda function that takes 3 arguments, the first being the list of predictions made by
            // the set.  The second lambda argument will be the net match counts for each feature segment (Sequence LSTM)
            // minus the failure counts.  The third lambda argument will be the list of the SequenceLSTMs
            // t
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, true, true);

                SequencePredictor plstm = (SequencePredictor)evaluatedArgs[0].getObjectValue();
                Lambda lambdaValue = (Lambda)evaluatedArgs[1].getLambda();

                plstm.setCustomPredictionAggregator(lambdaToOutputAggregator(lambdaValue, env));


                return evaluatedArgs[0];

            }
        };
    }


    public static SimpleFunctionTemplate predictionLSTMSetDefinePredictionTester()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) predictionLSTMSetDefinePredictionTester();
            }

            // First argument is the SequencePredictor
            // Second argument is an lambda function that takes 3 arguments, the first being a vector of the actual input
            // the set.  The second lambda argument will be a Vector of the predicted input, the third is the index of
            // the segment doing the predicting
            // Lambda function should reutrn a real number between 0 and 1 where 1 indicates maximum
            // weight to assign to the
            // Optional third argument is a double value defining the failure weight at and below which
            // the SequencePredictor should reset the weight of a segment

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, true, true);

                SequencePredictor predictor = (SequencePredictor)evaluatedArgs[0].getObjectValue();
                Lambda lambdaValue = (Lambda)evaluatedArgs[1].getLambda();

                double failureThreshold = 0;

                if (evaluatedArgs.length > 2)
                {
                    failureThreshold = evaluatedArgs[2].getFloatValue();
                }
                predictor.setPredictionEvaluator(lambdaToStatePredictionTester(lambdaValue, failureThreshold, env));


                return evaluatedArgs[0];

            }
        };
    }


    public static SimpleFunctionTemplate linkedLSTMJoin()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) linkedLSTMJoin();
            }

            // First argument is a LinkedLSTM
            // Second argument is a LinkedLSTM
            // Returns the first LinkedLSTM joined with the second
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, true);

                LinkedLSTM head = (LinkedLSTM)evaluatedArgs[0].getObjectValue();
                LinkedLSTM tail = (LinkedLSTM)evaluatedArgs[1].getObjectValue();

                head.join(tail);
                return evaluatedArgs[0];
            }
        };
    }


    public static SimpleFunctionTemplate linkedLSTMSetMerge()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) linkedLSTMSetMerge();
            }

            // First argument is a Linked LSTM Set
            // Second argument is a LinkedLSTM
            // Returns the Linked LSTM Set merged with the LinkedLSTM
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, true);

                LinkedLSTMSet head = (LinkedLSTMSet)evaluatedArgs[0].getObjectValue();
                LinkedLSTM tail = (LinkedLSTM)evaluatedArgs[1].getObjectValue();

                head.merge(tail);
                return evaluatedArgs[0];
            }
        };
    }



    public static WeightMatrix listToWeightMatrix(Value nestedLists)
    {
        double[][] out = NLispTools.listToDoubleArray(nestedLists);
        return new WeightMatrix(out);
    }



    public static Vector listToVector(Value value)
    {

        return new Vector(NLispTools.getDoubleArrayFromValue(value));
    }

    private static SequencePredictor.BestPredictionAggregator lambdaToOutputAggregator(final Lambda lambda, final Environment outer)
    {
        return new SequencePredictor.BestPredictionAggregator()
        {

            @Override
            public Vector aggregateResult(Vector[] lastPrediction, int[] netMatchCounts, SequenceLSTM[] featureSegments)
            {
                Value[] setPredictions = AITools.mapValues(lastPrediction, new LispValueMapper<Vector>() {
                    @Override
                    public Value map(Vector input, int index)
                    {
                        if (input != null)
                            return NLispTools.makeValue(input.raw());
                        else
                            return Environment.getNull();
                    }
                });

                Value[] netMatchValues = new Value[netMatchCounts.length];

                for (int i =0;i<netMatchCounts.length;i++)
                {
                    netMatchValues[i] = NLispTools.makeValue(netMatchCounts[i]);
                }

                Value[] segmentValues = AITools.mapValues(featureSegments, new LispValueMapper<SequenceLSTM>() {
                    @Override
                    public Value map(SequenceLSTM input, int index)
                    {
                        if (input != null)
                            return ExtendedFunctions.makeValue(input);
                        else
                            return Environment.getNull();
                    }
                });

                Value[] args = new Value[]{NLispTools.makeValue(setPredictions), NLispTools.makeValue(netMatchValues), NLispTools.makeValue(segmentValues)};
                lambda.setActualParameters(args);

                try
                {
                    Value out = lambda.evaluate(outer, false);
                    if (!out.isNull())
                        return listToVector(out);
                    else
                        return null;
                } catch (Exception e)
                {
                    e.printStackTrace();
                    throw new RuntimeException(e);
                }

            }
        };
    }


    private static SequencePredictor.PredictionComparator lambdaToStatePredictionTester(final Lambda lambda, final double resetWeight, final Environment outer)
    {
        return new SequencePredictor.PredictionComparator()
        {

            public double weighPrediction(Vector actualState, Vector predictedState, int index)
            {

                if (actualState == null || predictedState == null)
                    return 0;

                Value actualValue = NLispTools.makeValue(actualState.raw()), predictedStateValue = NLispTools.makeValue(predictedState.raw());

                Value[] args = new Value[]{actualValue, predictedStateValue, NLispTools.makeValue(index)};

                lambda.setActualParameters(args);
                try
                {
                    Value out = lambda.evaluate(outer, false);
                    return out.getFloatValue();
                } catch (Exception e)
                {
                    e.printStackTrace();
                    throw new RuntimeException(e);
                }
            }

            @Override
            public double getResetThresholdWeight()
            {
                return resetWeight;
            }
        };
    }

    private static VectorViewer lambdaToVectorViewer(final Lambda lambda, final Environment outer)
    {
        return new VectorViewer()
        {

            @Override
            public String toString(Vector v)
            {
                Value listVector = NLispTools.makeValue(v.raw());
                Value[] args = new Value[]{listVector};
                lambda.setActualParameters(args);
                try
                {
                    Value out = lambda.evaluate(outer, false);
                    return out.toString();
                } catch (Exception e)
                {
                    e.printStackTrace();
                    throw new RuntimeException(e);
                }
            }


        };
    }

}
