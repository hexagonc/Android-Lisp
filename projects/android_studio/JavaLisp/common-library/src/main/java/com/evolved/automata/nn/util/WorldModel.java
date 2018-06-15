package com.evolved.automata.nn.util;

import com.evolved.automata.lisp.nn.LSTMNetworkProxy;
import com.evolved.automata.nn.Vector;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

/**
 * Created by Evolved8 on 6/6/18.
 */

public class WorldModel {

    public static class GroupType {

        int numInputOutputNodes;
        int numMemoryCellNodes;
        int initiaWeight;
        int featureBufferSize;
        int minimumBufferOverlap;
        String name;
        LearningConfiguration config;

        public GroupType(int inputOutputNodes, int memoryCellNodes, int initialAllocationWeight, int featureBufferSize, int minimumBufferOverlap, LearningConfiguration config){
            numInputOutputNodes = inputOutputNodes;
            numMemoryCellNodes = memoryCellNodes;
            this.initiaWeight = initialAllocationWeight;
            this.config = config;
            this.minimumBufferOverlap = minimumBufferOverlap;
            this.featureBufferSize = featureBufferSize;
        }

        public GroupType setName(String n){
            name = n;
            return this;
        }

        public String getName(){
            return name;
        }

        public int getFeatureBufferSize(){
            return featureBufferSize;
        }

        public int getMinimumBufferOverlap(){
            return minimumBufferOverlap;
        }

        public LearningConfiguration getLearningConfig(){
            return config;
        }

        public int getInitialWeight(){
            return initiaWeight;
        }

        public int getInputOutputNodes(){
            return numInputOutputNodes;
        }

        public int getNumMemoryCellStates(){
            return numMemoryCellNodes;
        }
    }

    public static class GroupSpecification{
        Group _group;
        int _numAllocations;
        int _weight;
        GroupType _type;
        int _boundaryCount = 0;

        public GroupSpecification(GroupType type, Group group){
            _group = group;
            _type = type;
            _weight = _type.getInitialWeight();
            _numAllocations = 0;
        }

        public GroupSpecification assertBoundary(){
            _group.resetAll(false);
            _boundaryCount++;
            return this;
        }

        public GroupType getType(){
            return _type;
        }

        public Group getGroup(){
            return _group;
        }

        public GroupSpecification incrementAllocations(){
            return setNumAllocations(getNumAllocations()+ 1);
        }

        public GroupSpecification setNumAllocations(int allocations){
            _numAllocations = allocations;
            return this;
        }

        public int getNumAllocations(){
            return _numAllocations;
        }

        public GroupSpecification setWeight(int max){
            _weight = max;
            return this;
        }

        public int getWeight(){
            return _weight;
        }

        public GroupSpecification incrementWeight(){
            return setWeight(getWeight() + 1);
        }

        /**
         * Can't decrement max allocations below the total number of allocations
         * @return
         */
        public GroupSpecification decrementWeight(){
            return setWeight(Math.max(1, getWeight() - 1));
        }


    }

    HashMap<String, GroupType> mGroupTypes = new HashMap<String, GroupType>();

    public final GroupType BASIC;

    HashMap<String, GroupSpecification> mGroups = new HashMap<String, GroupSpecification>();

    int mTotalAllocation;

    LearningConfiguration mBaseLearningConfiguration;

    public WorldModel(int allocation){
        this(allocation, getFeatureLearningConfiguration().setDebugLevel(0));
    }

    public WorldModel(int allocation, LearningConfiguration baseLearningConfig){
        mTotalAllocation = allocation;
        mBaseLearningConfiguration = baseLearningConfig;
        BASIC = new GroupType(10,30, 1, 4, 3, mBaseLearningConfiguration);
        mGroupTypes.put(BASIC.toString(), BASIC);
    }

    public GroupType createGroupType(String name, int inputOutputNodes, int memoryCellNodes, int baseAllocation, int featureBufferSize, int minimumBufferOverlap, LearningConfiguration config){
        GroupType type = new GroupType(inputOutputNodes, memoryCellNodes, baseAllocation, featureBufferSize, minimumBufferOverlap, config).setName(name);
        mGroupTypes.put(name, type);
        return type;
    }

    public static LearningConfiguration getFeatureLearningConfiguration(){
        LearningConfiguration configuration = new LearningConfiguration();
        configuration.set(LearningConfiguration.KEY.ANNEALING_FRACTION, Double.valueOf(0.1F));
        configuration.set(LearningConfiguration.KEY.BEST_SOLUTION_BONUS_MILLI, Integer.valueOf(500));
        configuration.set(LearningConfiguration.KEY.NUM_SOLUTION_BUFFER, Integer.valueOf(10));
        configuration.set(LearningConfiguration.KEY.INITIAL_RANDOM_FRACTION, Float.valueOf(0.5F));
        configuration.set(LearningConfiguration.KEY.MAX_DURATION_MILLI, Integer.valueOf(3000));
        configuration.setMatchEqualityError(0.1);
        configuration.setHistoryLength(4);
        //configuration.set(LearningConfiguration.KEY.MAX_ITERATIONS, Integer.valueOf(3000));
        //configuration.set(LearningConfiguration.KEY.BEST_SOLUTION_BONUS_ITERATIONS, Integer.valueOf(400));
        return configuration;
    }

    public boolean canAllocateAnotherFeature(String groupKey){
        double totalWeight = 0;
        double myWeight = 0;
        GroupSpecification mySpec = null;
        for (String key:mGroups.keySet()){
            int weight = mGroups.get(key).getWeight();
            if (key.equals(groupKey)){
                mySpec = mGroups.get(key);
                myWeight = weight;
            }

            totalWeight+=weight;
        }

        int maxAllocations = (int)(mTotalAllocation*myWeight/totalWeight);
        return maxAllocations > mySpec.getNumAllocations();
    }

    public WorldModel incrementGroupAllocationCount(String groupKey){
        mGroups.get(groupKey).incrementAllocations();
        return this;
    }

    public Group addGroup(String name, LearningConfiguration configuration,  GroupType type){
        Group group = new Group(name, configuration, this, type);

        mGroups.put(name, new GroupSpecification(type, group));
        return group;
    }

    public Group addGroup(String name, LearningConfiguration configuration,  String type){
        return addGroup(name, configuration, mGroupTypes.get(type));
    }

    public Group addGroup(String name, GroupType type){
        return addGroup(name, mBaseLearningConfiguration, type);
    }

    public Group addGroup(String name, String type){
        return addGroup(name, mBaseLearningConfiguration,mGroupTypes.get(type));
    }

    public FeatureModel requestFeature(Group group){
        return requestFeature(group.getName());
    }

    public FeatureModel requestFeature(String groupName){
        GroupSpecification spec = mGroups.get(groupName);
        if (canAllocateAnotherFeature(groupName)){
            spec.incrementAllocations();
            GroupType type = spec.getType();

            FeatureModel model = new FeatureModel(type.getInputOutputNodes(), type.getNumMemoryCellStates(), type.getLearningConfig().copy());
            return model;
        }
        else
            return null;
    }


    public Group getGroup(String name){
        return mGroups.get(name).getGroup();
    }

    public ArrayList<GroupSpecification> getGroups(GroupType type){
        return findGroups(0, new GroupType[]{type}, null);
    }


    public ArrayList<GroupSpecification> findGroups(int inputWidth, GroupType[] groupTypes, String[] groupNames) {
        String[] typeNames = null;
        if (groupTypes != null){
            typeNames = new String[groupTypes.length];
            for (int i = 0;i < groupTypes.length;i++)
                typeNames[i] = groupTypes[i].getName();
        }
        return findGroups(inputWidth, typeNames, groupNames);
    }

    public ArrayList<GroupSpecification> findGroups(int inputWidth, String[] groupTypeNames, String[] groupNames) {
        ArrayList<GroupSpecification> out = new ArrayList<GroupSpecification>();
        GroupSpecification spec;


        if (groupNames != null && groupNames.length > 0)
        {
            for (String s:groupNames){
                spec = mGroups.get(s);
                if (spec!= null && spec.getType().getInputOutputNodes() == inputWidth){
                    out.add(spec);
                }
            }
        }

        if (out.size() > 0){
            return out;
        }

        HashSet<String> filterTypes = new HashSet<>();
        if (groupTypeNames == null || groupTypeNames.length == 0){
            groupTypeNames = mGroupTypes.keySet().toArray(new String[0]);
        }

        for (String groupTypeName:groupTypeNames){
            filterTypes.add(groupTypeName);
        }

        for (String groupName:mGroups.keySet()){
            spec = mGroups.get(groupName);
            if ((inputWidth <= 0 || spec.getType().getInputOutputNodes() == inputWidth) && (filterTypes.size() == 0 || filterTypes.contains(spec.getType().getName()))){
                out.add(spec);
            }

        }

        return out;
    }

    public ArrayList<GroupSpecification> processNextInput(Vector input){
        return processNextInput(input, (String[])null);
    }

    public ArrayList<GroupSpecification> processNextInput(Vector input, GroupType[] groups){
        ArrayList<GroupSpecification> relevantGroups = findGroups(input.dimen(), groups, null);

        for (GroupSpecification spec:relevantGroups){
            Group group = spec.getGroup();
            group.processInput(input);
        }
        return relevantGroups;
    }

    public ArrayList<GroupSpecification> processNextInput(Vector input, GroupType type){
        return processNextInput(input, new GroupType[]{type});
    }

    public Group.MODE processNextInput(Vector input, Group group){
        return group.processInput(input);
    }


    public ArrayList<GroupSpecification> processNextInput(Vector input, String[] groupNames){
        ArrayList<GroupSpecification> relevantGroups = findGroups(input.dimen(), (String[])null, groupNames);

        for (GroupSpecification spec:relevantGroups){
            Group group = spec.getGroup();
            group.processInput(input);
        }
        return relevantGroups;
    }

    public ArrayList<GroupSpecification> processNextInput(Vector input, String groupName){
        return processNextInput(input, new String[]{groupName});
    }

    public ArrayList<GroupSpecification> assertBoundary(String[] groupNames){
        ArrayList<GroupSpecification> relevantGroups = findGroups(-1, (String[])null, groupNames);

        for (GroupSpecification spec:relevantGroups){
            spec.assertBoundary();
        }
        return relevantGroups;
    }

    public ArrayList<GroupSpecification> assertBoundary(String groupName){
        return assertBoundary(new String[]{groupName});
    }


    public ArrayList<GroupSpecification> assertBoundary(GroupType[] types){
        ArrayList<GroupSpecification> relevantGroups = findGroups(-1, types, null);

        for (GroupSpecification spec:relevantGroups){
            spec.assertBoundary();
        }
        return relevantGroups;
    }

    public ArrayList<GroupSpecification> assertBoundary(GroupType type){
        return assertBoundary(new GroupType[]{type});
    }
}
