package com.evolved.automata.lisp.nn;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.ListValue;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.nn.Vector;
import com.evolved.automata.nn.util.FeatureModel;
import com.evolved.automata.nn.util.Group;
import com.evolved.automata.nn.util.LearningConfiguration;
import com.evolved.automata.nn.util.WorldModel;

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
        env.mapFunction("world-create-group-type", worldCreateGroupType());
        env.mapFunction("world-add-group", worldAddGroup());
        env.mapFunction("get-set-mode", groupSetMode());
        env.mapFunction("world-get-group", worldGetGroup());
        env.mapFunction("group-set-max-duration-milli", groupSetMaxDurationMilli());
        env.mapFunction("group-add-feature", groupAddFeature());
        env.mapFunction("group-remove-feature", groupRemoveFeature());
        env.mapFunction("group-increase-feature-value-fraction", groupIncreaseFeatureValueFraction());

        env.mapFunction("group-process-input", groupProcessInput());
        env.mapFunction("group-get-ordered-processed-features", groupGetOrderedProcessedFeatures());
        env.mapFunction("feature-extrapolate-values", featureExtrapolateValues());
        env.mapFunction("feature-get-length", featureLength());
        env.mapFunction("feature-get-distance-to-final-state", featureGetDistanceToFinalState());
        env.mapFunction("feature-get-state", featureGetState());



        env.mapFunction("group-toggle-memory-management", groupToggleMemoryManagement());
        env.mapFunction("feature-get-next-predicted-value", featureGetNextPredictedValue());
        env.mapFunction("world-process-input", worldProcessInput());

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
                int baseAllocation = (int)evaluatedArgs[4].getIntValue();
                int featureBufferSize = (int)evaluatedArgs[5].getIntValue();
                int minimumBufferOverlap  = (int)evaluatedArgs[6].getIntValue();
                LearningConfiguration config = null;
                if (evaluatedArgs.length>7){
                    config = (LearningConfiguration)evaluatedArgs[6].getObjectValue();
                }
                else {
                    config = WorldModel.getFeatureLearningConfiguration();
                }

                WorldModel.GroupType type = model.createGroupType(name, inputOutputNodes, memoryCellNodes, baseAllocation, featureBufferSize, minimumBufferOverlap, config);


                return ExtendedFunctions.makeValue(type);
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
                checkActualArguments(1, false, false);
                WorldModel model = (WorldModel)evaluatedArgs[0].getObjectValue();
                String groupName = (String)evaluatedArgs[1].getString();

                return ExtendedFunctions.makeValue(model.getGroup(groupName));
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
                    Value[] outv= spec.stream().map(v->ExtendedFunctions.makeValue(v)).collect(Collectors.toList()).toArray(new Value[0]);
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
                checkActualArguments(1, false, false);
                Group group = (Group)evaluatedArgs[0].getObjectValue();
                group.resetAll();

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

    public static SimpleFunctionTemplate groupSpec()
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
}
