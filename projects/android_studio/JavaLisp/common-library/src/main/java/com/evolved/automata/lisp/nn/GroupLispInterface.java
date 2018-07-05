package com.evolved.automata.lisp.nn;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.Lambda;
import com.evolved.automata.lisp.LambdaValue;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.nn.NNTools;
import com.evolved.automata.nn.Vector;
import com.evolved.automata.nn.util.FeatureModel;
import com.evolved.automata.nn.util.Group;
import com.evolved.automata.nn.util.LearningConfiguration;
import com.evolved.automata.nn.util.StringSerializer;
import com.evolved.automata.nn.util.WorldModel;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;

import java.util.ArrayList;
import java.util.stream.Collectors;

/**
 * Created by Evolved8 on 6/17/18.
 */

public class GroupLispInterface {

    public static void addNeuralNetFunctions(Environment env)
    {
        env.mapFunction("model-get-default-config", modelGetDefaultLearningConfig());
        env.mapFunction("model-create-world", modelCreateWorld());
        env.mapFunction("model-set-thread-count", modelSetThreadCount());
        env.mapFunction("world-serialized-group-data", worldSerializeGroupData());

        env.mapFunction("world-create-group-type", worldCreateGroupType());
        env.mapFunction("world-load-serialized-group-data", worldLoadSerializedGroupData());

        env.mapFunction("world-add-group", worldAddGroup());
        env.mapFunction("world-get-group", worldGetGroup());
        env.mapFunction("world-process-input", worldProcessInput());

        env.mapFunction("group-type-set-default-string-serializer", groupTypeSetDefaultStringSerializer());

        env.mapFunction("group-import-feature", groupImportFeature());
        env.mapFunction("group-set-decay-interval", groupSetDecayInterval());
        env.mapFunction("group-get-focus-feature", groupGetFocusFeature());
        env.mapFunction("group-reset", groupResetAll());

        env.mapFunction("group-set-sleep-listener", groupSetSleepListener());
        env.mapFunction("group-force-sleep", groupForceSleep());
        env.mapFunction("group-set-sleep-cycle-multiplier", groupSetSleepCycleMultiplier());
        env.mapFunction("group-set-feature-recycle-fraction", groupSetFeatureRecycleFraction());

        env.mapFunction("group-set-mode", groupSetMode());
        env.mapFunction("group-toggle-memory-management", groupToggleMemoryManagement());
        env.mapFunction("group-set-max-duration-milli", groupSetMaxDurationMilli());
        env.mapFunction("group-add-feature", groupAddFeature());
        env.mapFunction("group-remove-feature", groupRemoveFeature());
        env.mapFunction("group-increase-feature-value-fraction", groupIncreaseFeatureValueFraction());
        env.mapFunction("group-decrease-feature-value-fraction", groupDecreaseFeatureValueFraction());
        env.mapFunction("group-set-feature-metadata", groupSetCustomMetadata());
        env.mapFunction("group-get-feature-metadata", groupGetCustomMetadata());
        env.mapFunction("group-process-input", groupProcessInput());
        env.mapFunction("group-get-ordered-processed-features", groupGetOrderedProcessedFeatures());
        env.mapFunction("feature-extrapolate-values", featureExtrapolateValues());
        env.mapFunction("feature-get-length", featureLength());
        env.mapFunction("feature-force-complete", featureForceComplete());
        env.mapFunction("feature-get-distance-to-final-state", featureGetDistanceToFinalState());
        env.mapFunction("feature-get-state", featureGetState());
        env.mapFunction("feature-get-group-allocation-index", featureGetGroupAllocationIndex());
        env.mapFunction("feature-get-next-predicted-value", featureGetNextPredictedValue());
        env.mapFunction("feature-set-custom-meta-data", featureSetCustomMetadata());
        env.mapFunction("feature-get-custom-meta-data", featureGetCustomMetadata());


    }


    public static SimpleFunctionTemplate modelGetDefaultLearningConfig()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) modelGetDefaultLearningConfig();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                LearningConfiguration config = WorldModel.getFeatureLearningConfiguration();
                return ExtendedFunctions.makeValue(config);
            }
        };
    }

    public static SimpleFunctionTemplate modelCreateWorld()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) modelCreateWorld();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, true, true);
                int allocation = (int)evaluatedArgs[0].getIntValue();
                if (evaluatedArgs.length > 1){
                    LearningConfiguration config = (LearningConfiguration)evaluatedArgs[1].getObjectValue();
                    return ExtendedFunctions.makeValue(new WorldModel(allocation, config));

                }
                else {
                    return ExtendedFunctions.makeValue(new WorldModel(allocation));
                }

            }
        };
    }


    public static SimpleFunctionTemplate modelSetThreadCount()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) modelSetThreadCount();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, true, true);
                int count = (int)evaluatedArgs[0].getIntValue();
                FeatureModel.THREAD_COUNT = count;

                return evaluatedArgs[0];
            }
        };
    }

    public static SimpleFunctionTemplate worldCreateGroupType()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) worldCreateGroupType();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(7, true, true);
                WorldModel model = (WorldModel)evaluatedArgs[0].getObjectValue();
                String name = (String)evaluatedArgs[1].getString();
                int inputOutputNodes = (int)evaluatedArgs[2].getIntValue();
                int memoryCellNodes = (int)evaluatedArgs[3].getIntValue();
                int defaultGroupWeight = (int)evaluatedArgs[4].getIntValue();
                int featureBufferSize = (int)evaluatedArgs[5].getIntValue();
                int minimumBufferOverlap  = (int)evaluatedArgs[6].getIntValue();
                LearningConfiguration config = null;
                if (evaluatedArgs.length>7){
                    config = (LearningConfiguration)evaluatedArgs[7].getObjectValue();
                }
                else {
                    config = WorldModel.getFeatureLearningConfiguration();
                }

                WorldModel.GroupType type = model.createGroupType(name, inputOutputNodes, memoryCellNodes, defaultGroupWeight, featureBufferSize, minimumBufferOverlap, config);


                return ExtendedFunctions.makeValue(type);
            }
        };
    }

    public static SimpleFunctionTemplate worldLoadSerializedGroupData()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) worldLoadSerializedGroupData();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, true, true);
                WorldModel model = (WorldModel)evaluatedArgs[0].getObjectValue();
                String serialized = (String)evaluatedArgs[1].getString();

                try
                {
                    byte[] data = NNTools.BASE64.decodeBase64(serialized);
                    model.deserializeGroups(data);
                }
                catch (Exception e)
                {
                    throw new RuntimeException(e);
                }

                return evaluatedArgs[0];
            }
        };
    }

    public static SimpleFunctionTemplate worldSerializeGroupData()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) worldSerializeGroupData();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, true, true);
                WorldModel model = (WorldModel)evaluatedArgs[0].getObjectValue();

                byte[] data = model.serializeGroupBytes();
                String serialized = NNTools.BASE64.encodeBase64(data);

                return NLispTools.makeValue(serialized);
            }
        };
    }

    public static SimpleFunctionTemplate worldAddGroup()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) worldAddGroup();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(3, false, false);
                WorldModel model = (WorldModel)evaluatedArgs[0].getObjectValue();
                String groupName = (String)evaluatedArgs[1].getString();

                if (evaluatedArgs[2].isString()){
                    return ExtendedFunctions.makeValue(model.addGroup(groupName, evaluatedArgs[2].getString()));
                }
                else {
                    return ExtendedFunctions.makeValue(model.addGroup(groupName, (WorldModel.GroupType)evaluatedArgs[2].getObjectValue()));
                }
            }
        };
    }

    public static SimpleFunctionTemplate worldGetGroup()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) worldGetGroup();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(3, false, false);
                WorldModel model = (WorldModel)evaluatedArgs[0].getObjectValue();
                String typeName = evaluatedArgs[1].getString();
                String groupName = (String)evaluatedArgs[2].getString();


                return ExtendedFunctions.makeValue(model.getGroup(typeName, groupName).getGroup());
            }
        };
    }

    public static SimpleFunctionTemplate worldProcessInput()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) worldProcessInput();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, true, true);
                WorldModel model = (WorldModel)evaluatedArgs[0].getObjectValue();
                Value input = evaluatedArgs[1];

                Vector vinput = NeuralNetLispInterface.listToVector(input);

                ArrayList<WorldModel.GroupSpecification> spec = null;

                if (evaluatedArgs.length > 2){
                    if (!evaluatedArgs[2].isNull()) {
                        if (evaluatedArgs[2].isString()){
                            spec = model.processNextInput(vinput, evaluatedArgs[2].getString());
                        }
                        else {
                            spec = model.processNextInput(vinput, (WorldModel.GroupType) evaluatedArgs[2].getObjectValue());
                        }
                    }
                }

                if (spec != null){
                    Value[] outv= spec.stream().map(v->ExtendedFunctions.makeValue(v.getGroup())).collect(Collectors.toList()).toArray(new Value[0]);
                    return NLispTools.makeValue(outv);
                }
                else
                    return Environment.getNull();
            }
        };
    }

    static String processGroupValue(Value value){
        if (value.isString()){
            return value.getString();
        }
        else if (value.getObjectValue() instanceof Group){
            return ((Group)value.getObjectValue()).getName();
        }
        else
            return null;
    }

    static String processGroupTypeValue(Value value){
        if (value.isString()){
            return value.getString();
        }
        else if (value.getObjectValue() instanceof Group){
            return ((Group)value.getObjectValue()).getName();
        }
        else
            return null;
    }

    public static SimpleFunctionTemplate groupSetMode()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupSetMode();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, false);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                String modeName = (String)evaluatedArgs[1].getString();

                group.setMode(Group.MODE.valueOf(modeName));
                return evaluatedArgs[0];
            }
        };
    }

    public static SimpleFunctionTemplate groupImportFeature()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupImportFeature();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, true, true);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                FeatureModel model = (FeatureModel)evaluatedArgs[1].getObjectValue();
                boolean dreamToFreeP  = (evaluatedArgs.length > 2 && !evaluatedArgs[2].isNull());

                Group result = group.importFeature(model, dreamToFreeP);
                if (result != null)
                    return evaluatedArgs[0];
                else
                    return Environment.getNull();
            }
        };
    }

    public static SimpleFunctionTemplate groupSetDecayInterval()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupSetDecayInterval();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, false);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                int numProcessStepsPerDecay = (int)evaluatedArgs[1].getIntValue();

                Group result = group.setDecayInterval(numProcessStepsPerDecay);
                return evaluatedArgs[0];
            }
        };
    }

    public static SimpleFunctionTemplate groupGetFocusFeature()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupGetFocusFeature();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, false);
                Group group = (Group)evaluatedArgs[0].getObjectValue();

                FeatureModel result = group.getFocusModel();
                if (result != null)
                    return ExtendedFunctions.makeValue(result);
                else
                    return Environment.getNull();
            }
        };
    }


    public static SimpleFunctionTemplate groupSetMaxDurationMilli()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupSetMaxDurationMilli();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, false);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                int durationMilli = (int)evaluatedArgs[1].getIntValue();

                group.setMaximumAmountOfProcessTime(durationMilli);
                return evaluatedArgs[0];
            }
        };
    }


    public static SimpleFunctionTemplate groupProcessInput()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupProcessInput();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, false);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                Group.MODE mode = group.processInput(NeuralNetLispInterface.listToVector(evaluatedArgs[1]));

                return NLispTools.makeValue(mode.name());
            }
        };
    }

    public static SimpleFunctionTemplate groupResetAll()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupResetAll();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, true, true);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                group.resetAll(evaluatedArgs.length > 1 && !evaluatedArgs[1].isNull());

                return evaluatedArgs[0];
            }
        };
    }

    public static SimpleFunctionTemplate groupTypeSetDefaultStringSerializer()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupTypeSetDefaultStringSerializer();
            }

            @Override
            public Value evaluate(final Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, true, true);
                WorldModel.GroupType type = (WorldModel.GroupType)evaluatedArgs[0].getObjectValue();
                type.setCustomDataStringSerializer(new StringSerializer() {
                    @Override
                    public String serialize(Object o)
                    {
                        if (o == null)
                            return "F";
                        else
                            return ((Value)o).serializedForm();
                    }

                    @Override
                    public Object deserialize(String data)
                    {
                        if (data == null)
                            return Environment.getNull();
                        else
                        {
                            try
                            {
                                return env.evaluate(data, false);
                            } catch (Exception e)
                            {
                                throw new RuntimeException(e);
                            }
                        }
                    }
                });
                return evaluatedArgs[0];
            }
        };
    }


    public static SimpleFunctionTemplate groupSetSleepListener()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupSetSleepListener();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, true);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                final Lambda listener = (Lambda) evaluatedArgs[1].getLambda();

                Group.MemoryManagementListener memoryListener = new Group.MemoryManagementListener() {
                    @Override
                    public void onStartMemoryManagement(int totalAllocation)
                    {
                        listener.setActualParameters(new Value[]{NLispTools.makeValue(totalAllocation), NLispTools.makeValue(totalAllocation)});
                        try
                        {
                            listener.evaluate(env, false);
                        } catch (Exception e)
                        {
                            throw new RuntimeException(e);
                        }
                    }

                    @Override
                    public void onFinishedMemoryManagement(ArrayList<Triple<FeatureModel, String, ArrayList<Vector>>> recycled)
                    {
                        Value[] out = recycled.stream().map((Triple<FeatureModel, String, ArrayList<Vector>> p)-> ExtendedFunctions.makeValue(p.getLeft())).collect(Collectors.toList()).toArray(new Value[0]);

                        listener.setActualParameters(new Value[]{NLispTools.makeValue(false), NLispTools.makeValue(out)});
                        try
                        {
                            listener.evaluate(env, false);
                        }  catch (Exception e)
                        {
                            throw new RuntimeException(e);
                        }
                    }
                };

                group.setMemoryListener(memoryListener);
                return evaluatedArgs[0];
            }
        };
    }

    public static SimpleFunctionTemplate groupForceSleep()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupForceSleep();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, true);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                group.sleep();

                return evaluatedArgs[0];
            }
        };
    }


    public static SimpleFunctionTemplate groupGetOrderedProcessedFeatures()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupGetOrderedProcessedFeatures();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, false);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                ArrayList<FeatureModel> features = group.getOrderedFeatures();

                Value[] v = features.stream().map(f-> ExtendedFunctions.makeValue(f)).collect(Collectors.toList()).toArray(new Value[0]);

                return NLispTools.makeValue(v);
            }
        };
    }

    public static SimpleFunctionTemplate groupToggleMemoryManagement()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupToggleMemoryManagement();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, false);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                boolean enable = !evaluatedArgs[1].isNull();
                group.setMemoryManagement(enable);

                return evaluatedArgs[0];
            }
        };
    }


    // Set the number of sleep feature iterations as a multiple of the number
    // of complete features
    public static SimpleFunctionTemplate groupSetSleepCycleMultiplier()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupSetSleepCycleMultiplier();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, false);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                double multiplier = evaluatedArgs[1].getFloatValue();
                group.setSleepCycleMultipler(multiplier);

                return evaluatedArgs[0];
            }
        };
    }

    public static SimpleFunctionTemplate groupSetFeatureRecycleFraction()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupSetFeatureRecycleFraction();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, false);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                double fraction = evaluatedArgs[1].getFloatValue();
                group.setMemoryRecycleFraction(fraction);

                return evaluatedArgs[0];
            }
        };
    }


    public static SimpleFunctionTemplate featureExtrapolateValues()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) featureExtrapolateValues();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, false);
                FeatureModel feature = (FeatureModel)evaluatedArgs[0].getObjectValue();
                ArrayList<Vector> values = feature.extrapFeature();

                Value[] v = values.stream().map(value-> NeuralNetLispInterface.getLispDataValue(value.rawFloat())).collect(Collectors.toList()).toArray(new Value[0]);

                return NLispTools.makeValue(v);
            }
        };
    }

    public static SimpleFunctionTemplate featureLength()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) featureLength();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, false);
                FeatureModel feature = (FeatureModel)evaluatedArgs[0].getObjectValue();

                return NLispTools.makeValue(feature.getFeatureLength());
            }
        };
    }

    public static SimpleFunctionTemplate featureForceComplete()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) featureForceComplete();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, false);
                FeatureModel feature = (FeatureModel)evaluatedArgs[0].getObjectValue();
                feature.forceComplete();
                return NLispTools.makeValue(feature.getFeatureLength());
            }
        };
    }

    public static SimpleFunctionTemplate featureGetDistanceToFinalState()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) featureGetDistanceToFinalState();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, false);
                FeatureModel feature = (FeatureModel)evaluatedArgs[0].getObjectValue();

                return NLispTools.makeValue(feature.getDistanceToFinalState());
            }
        };
    }

    public static SimpleFunctionTemplate featureGetState()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) featureGetState();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, false);
                FeatureModel feature = (FeatureModel)evaluatedArgs[0].getObjectValue();

                return NLispTools.makeValue(feature.getState().name());
            }
        };
    }



    public static SimpleFunctionTemplate groupIncreaseFeatureValueFraction()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupIncreaseFeatureValueFraction();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(3, false, false);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                FeatureModel feature = (FeatureModel)evaluatedArgs[1].getObjectValue();
                double fraction = evaluatedArgs[2].getFloatValue();
                group.increaseFeatureValueFraction(feature, fraction);

                return NLispTools.makeValue(feature.getState().name());
            }
        };
    }


    public static SimpleFunctionTemplate groupDecreaseFeatureValueFraction()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupDecreaseFeatureValueFraction();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(3, false, false);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                FeatureModel feature = (FeatureModel)evaluatedArgs[1].getObjectValue();
                double fraction = evaluatedArgs[2].getFloatValue();
                group.decreaseFeatureValueFraction(feature, fraction);

                return NLispTools.makeValue(feature.getState().name());
            }
        };
    }


    public static SimpleFunctionTemplate groupSetCustomMetadata()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupSetCustomMetadata();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(3, false, false);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                FeatureModel feature = (FeatureModel)evaluatedArgs[1].getObjectValue();

                group.setCustomMetadata(feature, evaluatedArgs[2]);

                return evaluatedArgs[2];
            }
        };
    }

    public static SimpleFunctionTemplate groupGetCustomMetadata()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupGetCustomMetadata();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, false);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                FeatureModel feature = (FeatureModel)evaluatedArgs[1].getObjectValue();

                Object v = group.getCustomMetadata(feature);

                if (v == null)
                    return Environment.getNull();
                else
                    return (Value)v;
            }
        };
    }


    public static SimpleFunctionTemplate groupRemoveFeature()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupRemoveFeature();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, false);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                if (evaluatedArgs[1].isInteger()){
                    group.removeFeature((int)evaluatedArgs[1].getIntValue());
                }
                else {
                    FeatureModel feature = (FeatureModel)evaluatedArgs[1].getObjectValue();
                    group.removeFeature(feature);
                }

                return evaluatedArgs[0];
            }
        };
    }

    public static SimpleFunctionTemplate groupAddFeature()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) groupAddFeature();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, false);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                FeatureModel feature = (FeatureModel)evaluatedArgs[1].getObjectValue();
                group.addFeature(feature);

                return evaluatedArgs[0];
            }
        };
    }


    public static SimpleFunctionTemplate featureGetNextPredictedValue()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) featureGetNextPredictedValue();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, false);
                FeatureModel feature = (FeatureModel)evaluatedArgs[0].getObjectValue();

                return NeuralNetLispInterface.getLispDataValue(feature.getPredictedOutput().rawFloat());
            }
        };
    }

    public static SimpleFunctionTemplate featureSetCustomMetadata()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) featureSetCustomMetadata();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(2, false, false);
                FeatureModel feature = (FeatureModel)evaluatedArgs[0].getObjectValue();
                Group.FeatureMetaData meta = (Group.FeatureMetaData)feature.getMetaData();
                meta.setCustomMetadata(evaluatedArgs[1]);
                return evaluatedArgs[0];
            }
        };
    }

    public static SimpleFunctionTemplate featureGetCustomMetadata()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) featureGetCustomMetadata();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, false);
                FeatureModel feature = (FeatureModel)evaluatedArgs[0].getObjectValue();
                Group.FeatureMetaData meta = (Group.FeatureMetaData)feature.getMetaData();
                if (meta.getCustomMetadata() != null)
                    return (Value)meta.getCustomMetadata();
                else
                    return Environment.getNull();
            }
        };
    }

    public static SimpleFunctionTemplate featureGetGroupAllocationIndex()
    {
        return new SimpleFunctionTemplate() {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) featureGetGroupAllocationIndex();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs)
            {
                checkActualArguments(1, false, false);
                FeatureModel feature = (FeatureModel)evaluatedArgs[0].getObjectValue();
                Group.FeatureMetaData meta = (Group.FeatureMetaData)feature.getMetaData();
                return NLispTools.makeValue(meta.getAllocationIndex());
            }
        };
    }
}
