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
import com.evolved.automata.nn.LSTMLearningResult;
import com.evolved.automata.nn.LSTMNetwork;
import com.evolved.automata.nn.NNTools;
import com.evolved.automata.nn.SLSTMSet;
import com.evolved.automata.nn.SequenceLSTM;
import com.evolved.automata.nn.Vector;
import com.evolved.automata.nn.VectorValueMapper;
import com.evolved.automata.nn.WeightMatrix;

import java.util.HashMap;

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


        env.mapFunction("create-slstm-set", simpleLSTMCreateSet());
        env.mapFunction("slstm-set-observe", simpleLSTMSetObserve());
        env.mapFunction("slstm-set-get-size", simpleLSTMSetGetNumSLSTMs());
        env.mapFunction("slstm-set-set-observation-update-policy", simpleLSTMSetObservationRecognitionPolicy());
        env.mapFunction("slstm-set-get-current-completions", simpleLSTMSetGetCompletion());
        env.mapFunction("slstm-set-get-predictions-of-next-observation", simpleLSTMSetGetLastPredictions());

        env.mapFunction("slstm-set-set-max-error-per-input", simpleLSTMSetSetMaxErrorPerInputVector());
        env.mapFunction("slstm-set-set-max-steps-per-input", simpleLSTMSetSetMaxLearningStepsPerInputVector());

        env.mapFunction("slstm-set-set-flush-policy", simpleLSTMSetSetFlushPolicy());
        env.mapFunction("slstm-set-flush-temp-buffer", simpleLSTMSetFlushTempBuffer());
        env.mapFunction("slstm-set-commit-changes", simpleLSTMSetCommitNewSLSTMs());
        env.mapFunction("slstm-set-rollback-changes", simpleLSTMRollbackSLSTMSet());
        env.mapFunction("slstm-set-get-aggregate-prediction", simpleLSTMSetGetAggregateState());
        env.mapFunction("slstm-set-define-aggregator", simpleLSTMSetDefinePredictionAggregator());
        env.mapFunction("slstm-set-define-prediction-tester", simpleLSTMSetDefinePredictionTester());


        env.mapFunction("slstm-set-add-hierarchical-set", simpleLSTMSetAddHierarchicalSet());
        env.mapFunction("slstm-set-propagate-all-observations", simpleLSTMSetPropagateAllObservationsToHigher());
        env.mapFunction("slstm-set-remove-all-items", simpleLSTMSetRemoveAllItems());
        env.mapFunction("slstm-set-get-items", simpleLSTMSetGetItems());
        env.mapFunction("slstm-set-set-items", simpleLSTMSetSetItems());

        env.mapFunction("simple-slstm-add", simpleSLSTMAdd());
        env.mapFunction("simple-slstm-get-all-items", simpleSLSTMGetAllItems());
        env.mapFunction("simple-slstm-get-size", simpleSLSTMGetSize());
        env.mapFunction("simple-slstm-remove-first", simpleSLSTMRemoveFirst());
        env.mapFunction("simple-slstm-remove-last", simpleSLSTMRemoveLast());
        env.mapFunction("simple-slstm-undo-remove-last", simpleSLSTMUndoRemoveLast());
        env.mapFunction("simple-slstm-remove-all", simpleSLSTMRemoveAll());

    }

    public static SimpleFunctionTemplate simpleLSTMSetSetMaxErrorPerInputVector()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetSetMaxErrorPerInputVector();
            }

            // First argument is the SLSTMSet
            // Second argument is a numeric error float

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, true);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();
                double maxError = evaluatedArgs[1].getFloatValue();
                lstmSet.setMaxLearningError(maxError);

                return evaluatedArgs[0];
            }
        };
    }

    public static SimpleFunctionTemplate simpleLSTMSetSetMaxLearningStepsPerInputVector()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetSetMaxLearningStepsPerInputVector();
            }

            // First argument is the SLSTMSet
            // Second argument is the max number of steps

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, true);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();
                int maxSteps = (int)evaluatedArgs[1].getIntValue();
                lstmSet.setMaxLearningSteps(maxSteps);

                return evaluatedArgs[0];
            }
        };
    }



    public static SimpleFunctionTemplate simpleLSTMSetObservationRecognitionPolicy()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetObservationRecognitionPolicy();
            }

            // First argument is the SLSTMSet
            // Second argument is a string enum parameter indicating whether to allow continuous updating of all
            // lstms with each observation, regardless of whether that lstm predicted the last observation.
            // Possible values for second parameter are:
            // AT_PATTERN_BOUNDARY
            //      This is the default policy.  A pattern recognition is  continous,
            //      starting from the first vector of one of the SLSTMs contained in the
            //      set.  This policy is the fastest to execute and has the most consistent
            //      behavior since information that propagates to higher layers (if present)
            //      will be less noisy.  However, there might be some meaningful patterns that are
            //      not recognized with this policy since it cannot recognize patterns that start
            //      from mid-sequence but still complete.
            // IN_PATTERN_INTERIOR_WHEN_POSSIBLE
            //      Same as AT_PATTERN_BOUNDARY except that if there is at least one SLSTM that
            //      is being successfully matched from its starting point boundary then other
            //      SLSTMs are allowed to match from their interior.  This is useful if you are
            //      primarily interested in the SLSTMs that fully match from the start but want
            //      to allow the possibility of some SLSTMs matching from their interior as a way
            //      to provide contextual information to the higher layers
            // CONTIUOUS
            //      Continuously match all SLSTMs in the set against the next observation, regardless
            //      of there history.  This is the slowest policy but allows patterns to be matched
            //      mid-sequence or from the beginning of the sequence.
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, true);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();
                lstmSet.setPatternRecognitionPolicy(SLSTMSet.PatternRecognitionPolicy.valueOf(evaluatedArgs[1].getString()));

                return evaluatedArgs[0];
            }
        };
    }



    public static SimpleFunctionTemplate simpleLSTMSetGetLastPredictions()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetGetLastPredictions();
            }

            // First argument is the SLSTMSet

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();
                Vector[] predictionsOfLastObservation = lstmSet.getLastPredictedOutput();

                if (predictionsOfLastObservation != null)
                    return NLispTools.makeValue(AITools.mapValues(predictionsOfLastObservation, new LispValueMapper<Vector>() {
                        @Override
                        public Value map(Vector input, int index)
                        {
                            return NLispTools.makeValue(input.raw());
                        }
                    }));
                else
                    return Environment.getNull();

            }
        };
    }


    public static SimpleFunctionTemplate simpleLSTMSetPropagateAllObservationsToHigher()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetPropagateAllObservationsToHigher();
            }

            // First argument is the SLSTMSet

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, true);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();
                lstmSet.setPropagateAllObservationsToHigherLevel(!evaluatedArgs[1].isNull());

                return evaluatedArgs[0];
            }
        };
    }


    public static SimpleFunctionTemplate simpleLSTMSetRemoveAllItems()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetRemoveAllItems();
            }

            // First argument is the SLSTMSet

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();
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

    public static SimpleFunctionTemplate simpleLSTMCreateSet()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMCreateSet();
            }

            // First argument is the number of input nodes
            // Second argument is the number of nodes in the memory cell of each
            // sequential lstm
            // Third is the number of slstms that define the state of the set, this
            // should be a moderately large number

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(3, true, true);

                int numInputNodes = (int)evaluatedArgs[0].getIntValue();
                int numMemoryStates = (int)evaluatedArgs[1].getIntValue();
                int numSetStates = (int)evaluatedArgs[2].getIntValue();


                SLSTMSet lstmSet = new SLSTMSet(numInputNodes, numMemoryStates, numSetStates);


                return ExtendedFunctions.makeValue(lstmSet);
            }
        };
    }


    public static SimpleFunctionTemplate simpleLSTMSetObserve()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetObserve();
            }

            // First argument is the SLSTMSet
            // Second argument is the vector of a new observation.  Should have as many dimensions
            // as the number of input nodes defined in the SLSTMSet
            // Third argument is a boolean parameter indicating if the SLSTMSet should create new
            // patterns


            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(3, false, true);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();

                Value newInput = evaluatedArgs[1];
                Value createNew = evaluatedArgs[2];


                Vector slstmState = lstmSet.observe(listToVector(newInput), !createNew.isNull());

                if (slstmState != null)
                    return NLispTools.makeValue(slstmState.raw());
                else
                {
                    return Environment.getNull();
                }


            }
        };
    }

    public static SimpleFunctionTemplate simpleLSTMSetGetNumSLSTMs()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetGetNumSLSTMs();
            }

            // First argument is the SLSTMSet

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, false);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();

                return NLispTools.makeValue(lstmSet.getNumPatterns());

            }
        };
    }

    public static SimpleFunctionTemplate simpleLSTMSetGetCompletion()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetGetNumSLSTMs();
            }

            // First argument is the SLSTMSet
            // Returns a list of the set buffer with the fraction of completion from the last observation

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, false);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();
                Vector completion = lstmSet.getLastPatternCompletion();
                if (completion != null)
                    return NLispTools.makeValue(completion.raw());
                else
                    return Environment.getNull();

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

    public static SimpleFunctionTemplate simpleLSTMSetSetFlushPolicy()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetSetFlushPolicy();
            }

            // First argument is the SLSTMSet
            // Second argument is a string enum indicating when the set actually
            // Possible values are:
            // ON_DEMAND
            //      Only flushes the temp buffer when (simple-slstm-set-flush-temp-buffer) is called.
            //      Until this happens, the SLSTMSet will not learn new items automatically.
            //      This can create duplicates
            // IMMEDIATELY
            //      SLSTMSet learns new patterns automatically and incorporates them into the
            //      as it goes
            // NEVER
            //      This means that the buffer is not used at all.  This is the same as ON_DEMAND
            //      except that the buffer is never filled
            // incorporates new patterns
            //
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, true);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();
                String policyString = evaluatedArgs[1].getString();
                lstmSet.setTempBufferFlushPolicy(SLSTMSet.TempSLSTMFlushPolicy.valueOf(policyString));
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


    public static SimpleFunctionTemplate simpleLSTMSetSetItems()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetSetItems();
            }

            // First argument is the SLSTMSet

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, true);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();
                Value[] items = evaluatedArgs[1].getList();

                SequenceLSTM[] members = new SequenceLSTM[items.length];

                for (int i = 0;i < members.length;i++)
                {
                    members[i]= (SequenceLSTM)items[i].getObjectValue();
                }
                lstmSet.setMembers(members);
                return evaluatedArgs[0];

            }
        };
    }


    public static SimpleFunctionTemplate simpleLSTMSetGetItems()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetGetItems();
            }

            // First argument is the SLSTMSet

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();
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

    public static SimpleFunctionTemplate simpleLSTMSetDefinePredictionAggregator()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetDefinePredictionAggregator();
            }

            // First argument is the SLSTMSet
            // Second argument is an lambda function that takes 2 arguments, the first being the list of predictions made by
            // the set.  The second lambda argument will be the aggregate prediction from the higher order SLSTMSet if present
            // t
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, true, true);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();
                Lambda lambdaValue = (Lambda)evaluatedArgs[1].getLambda();

                lstmSet.setPredictionAggregator(lambdaToOutputAggregator(lambdaValue, env));


                return evaluatedArgs[0];

            }
        };
    }


    public static SimpleFunctionTemplate simpleLSTMSetDefinePredictionTester()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetDefinePredictionTester();
            }

            // First argument is the SLSTMSet
            // Second argument is an lambda function that takes 2 arguments, the first being a vector of the actual input
            // the set.  The second lambda argument will be a Vector of the predicted input
            // t
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, true, true);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();
                Lambda lambdaValue = (Lambda)evaluatedArgs[1].getLambda();

                lstmSet.setStatePredictionTester(lambdaToStatePredictionTester(lambdaValue, env));


                return evaluatedArgs[0];

            }
        };
    }


    public static SimpleFunctionTemplate simpleLSTMSetAddHierarchicalSet()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) simpleLSTMSetAddHierarchicalSet();
            }

            // First argument is the SLSTMSet
            // Optional Second argument is the hierarchical SLSTMSet.  If not present, a default SLSTMSet will be created and returned
            // Optional argument is an lambda function that takes 2 arguments, the first being the list of predictions made by
            // the set.  The second lambda argument will be the aggregate prediction from the higher order SLSTMSet if present
            // Return value is the newly created SLSTMSet
            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, true, true);

                SLSTMSet lstmSet = (SLSTMSet)evaluatedArgs[0].getObjectValue();

                if (evaluatedArgs.length > 1)
                {
                    SLSTMSet higherLstmSet = (SLSTMSet)evaluatedArgs[1].getObjectValue();
                    Lambda lambdaValue = null;

                    if (evaluatedArgs.length > 2)
                    {
                        lambdaValue = (Lambda)evaluatedArgs[1].getLambda();
                        return ExtendedFunctions.makeValue(lstmSet.addHierarchicalLayer(lambdaToOutputAggregator(lambdaValue, env), higherLstmSet));

                    }
                    else
                        return ExtendedFunctions.makeValue(lstmSet.addHierarchicalLayer(higherLstmSet));
                }
                else
                    return ExtendedFunctions.makeValue(lstmSet.addHierarchicalLayer());

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

    private static SLSTMSet.OutputAggregator lambdaToOutputAggregator(final Lambda lambda, final Environment outer)
    {
        return new SLSTMSet.OutputAggregator()
        {

            @Override
            public Vector aggregateResult(Vector[] lastPrediction, Vector hierarchicalAggregateResult)
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

                Value higherLevelAggregateResult = Environment.getNull();

                if (hierarchicalAggregateResult != null)
                    higherLevelAggregateResult = NLispTools.makeValue(hierarchicalAggregateResult.raw());

                Value[] args = new Value[]{NLispTools.makeValue(setPredictions), higherLevelAggregateResult};
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


    private static SLSTMSet.StatePredictionTester lambdaToStatePredictionTester(final Lambda lambda, final Environment outer)
    {
        return new SLSTMSet.StatePredictionTester()
        {

            public boolean areMatched(Vector actualState, Vector predictedState)
            {

                if (actualState == null || predictedState == null)
                    return false;

                Value actualValue = NLispTools.makeValue(actualState.raw()), predictedStateValue = NLispTools.makeValue(predictedState.raw());

                Value[] args = new Value[]{actualValue, predictedStateValue};

                lambda.setActualParameters(args);
                try
                {
                    Value out = lambda.evaluate(outer, false);
                    return !out.isNull();
                } catch (Exception e)
                {
                    e.printStackTrace();
                    throw new RuntimeException(e);
                }
            }
        };
    }

}
