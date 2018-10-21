package com.evolved.automata.nn.util;

import com.evolved.automata.lisp.nn.LSTMNetworkProxy;
import com.evolved.automata.nn.Vector;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Created by Evolved8 on 6/6/18.
 */

public class WorldModel {

    public class GroupType {

        int numInputOutputNodes;
        int numMemoryCellNodes;
        int initiaWeight;
        int featureBufferSize;
        int minimumBufferOverlap;
        int maxAllocation = -1;
        String name;
        LearningConfiguration config;
        StringSerializer _serializer;

        HashMap<String, GroupSpecification> _groupMap = new HashMap<>();

        LearningConfiguration.InputValidator _validator = null;


        public byte[] serializeGroups(){
            GroupSerializer.Builder b = GroupSerializer.get().serialize();
            b.add(_groupMap.size());
            for (Map.Entry<String, GroupSpecification> pair:_groupMap.entrySet()){
                b.add(pair.getKey());
                b.add(ByteBuffer.class, ByteBuffer.wrap(pair.getValue().serializeBytes()));
            }
            return b.build();
        }

        public GroupType fillGroups(byte[] groupMap){
            ArrayList values = GroupSerializer.get().deserialize(groupMap);
            int size = (Integer)values.get(0);
            int j = 1;
            String groupName = null;
            for (int i = 0;i<size*2;i++){

                if (i % 2 == 0){
                    groupName = (String)values.get(j++);
                }
                else {
                    ByteBuffer buffer = (ByteBuffer)values.get(j++);
                    GroupSpecification spec = GroupSpecification.deserializeBytes(buffer.array(), this);
                    _groupMap.put(groupName, spec);
                }
            }
            return this;
        }

        public Group addGroup(String name){
            Group g = new Group(name, this);
            _groupMap.put(name, new GroupSpecification(this, g));
            mTotalWeight+=initiaWeight;
            return g;
        }

        public GroupType importGroup(Group g, boolean updateExisting){
            GroupSpecification prior = _groupMap.get(g.getName());

            if (updateExisting && prior != null) {
                prior._group = g;
            }
            else {
                _groupMap.put(g.getName(), new GroupSpecification(this, g));
                mTotalWeight+=initiaWeight;
            }

            return this;
        }


        public HashMap<String, GroupSpecification> getGroups(){
            return _groupMap;
        }

        public boolean canAllocateAnotherFeature(String groupKey){

            GroupSpecification mySpec = _groupMap.get(groupKey);
            double weight = mySpec.getWeight();

            int maxAllocations = (int)(mTotalAllocation*weight/mTotalWeight);
            return maxAllocations > mySpec.getNumAllocations();
        }

        public GroupType incrementAllocation(String groupKey){
            GroupSpecification mySpec = _groupMap.get(groupKey);
            mySpec.incrementAllocations();
            return this;
        }

        public GroupType decrementAllocation(String groupKey){
            GroupSpecification mySpec = _groupMap.get(groupKey);
            mySpec.decrementAllocations();
            return this;
        }

        public FeatureModel requestFeature(String groupName){
            GroupSpecification spec = _groupMap.get(groupName);
            if (canAllocateAnotherFeature(groupName)){
                spec.incrementAllocations();

                FeatureModel model = new FeatureModel(getInputOutputNodes(), getNumMemoryCellStates(), getLearningConfig().copy());
                mNumAllocated++;
                return model;
            }
            else
                return null;
        }

        public FeatureModel requestFeature(Group g){
            return requestFeature(g.getName());
        }

        public GroupType setInputValidator(LearningConfiguration.InputValidator validator){
            _validator = validator;
            config.setInputValidator(validator);
            return this;
        }

        public GroupType setCustomDataStringSerializer(StringSerializer serializer){
            _serializer = serializer;
            return this;
        }

        public StringSerializer getCustomDataStringSerializer(){
            return _serializer;
        }

        public GroupType(int inputOutputNodes, int memoryCellNodes, int initialAllocationWeight, int featureBufferSize, int minimumBufferOverlap, LearningConfiguration config){
            numInputOutputNodes = inputOutputNodes;
            numMemoryCellNodes = memoryCellNodes;
            this.initiaWeight = initialAllocationWeight;
            this.config = config;
            this.minimumBufferOverlap = minimumBufferOverlap;
            this.featureBufferSize = featureBufferSize;

        }

        public GroupType setMaxAllocation(int max){
            maxAllocation = max;
            return this;
        }

        public int getMaxAllocation(){
            return maxAllocation;
        }

        public boolean hasMaxAllocation(){
            return maxAllocation != -1;
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

        public byte[] serializeBytes(){
            return GroupSerializer.get().serialize().add(ByteBuffer.class, ByteBuffer.wrap(_group.serializeBytes())).add(_numAllocations).add(_weight).add(_boundaryCount).build();
        }

        public static GroupSpecification deserializeBytes(byte[] data, GroupType type){
            GroupSpecification spec = new GroupSpecification();
            ArrayList values = GroupSerializer.get().deserialize(data);
            ByteBuffer buffer = (ByteBuffer)values.get(0);
            spec._group = Group.deserializeBytes(buffer.array(), type);
            spec._numAllocations = (Integer)values.get(1);
            spec._weight = (Integer)values.get(2);
            spec._boundaryCount = (Integer)values.get(3);
            spec._type = type;
            return spec;
        }

        GroupSpecification(){

        }

        public GroupSpecification(GroupType type, Group group){
            _group = group;
            _type = type;
            _weight = _type.getInitialWeight();
            _numAllocations = 0;
        }

        public GroupSpecification assertBoundary(){
            _group.resetAll();
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

        public GroupSpecification decrementAllocations(){
            return setNumAllocations(Math.max(0, getNumAllocations() - 1));
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

    int mTotalAllocation = 0;
    int mNumAllocated = 0;
    int mTotalWeight = 0;

    LearningConfiguration mBaseLearningConfiguration;

    public WorldModel(int allocation){
        this(allocation, getFeatureLearningConfiguration().setDebugLevel(0));
    }

    public WorldModel(int allocation, LearningConfiguration baseLearningConfig){
        mTotalAllocation = allocation;
        mBaseLearningConfiguration = baseLearningConfig;

        BASIC = new GroupType(10,30, 1, 4, 3, mBaseLearningConfiguration);
        BASIC.setName("BASIC");
        mGroupTypes.put("BASIC", BASIC);
    }

    public byte[] serializeGroupBytes(){
        GroupSerializer.Builder b = GroupSerializer.get().serialize()
                .add(mTotalAllocation)
                .add(mNumAllocated)
                .add(mTotalWeight);

        int size = mGroupTypes.size();
        b.add(size);
        for (Map.Entry<String, GroupType> pair:mGroupTypes.entrySet()){
            String typeName = pair.getKey();
            GroupType type = pair.getValue();
            b.add(typeName);
            b.add(ByteBuffer.class, ByteBuffer.wrap(type.serializeGroups()));
        }
        return b.build();
    }

    public WorldModel deserializeGroups(byte[] groupData){
        ArrayList values = GroupSerializer.get().deserialize(groupData);
        mTotalAllocation = (Integer)values.get(0);
        mNumAllocated = (Integer)values.get(1);
        mTotalWeight = (Integer)values.get(2);
        int size = (Integer)values.get(3);
        int j = 4;
        String typeName = null;
        for (int i = 0;i<2*size;i++){

            ByteBuffer typeGroupsData = null;
            if (i % 2 == 0){
                typeName = (String)values.get(j++);
            }
            else {
                typeGroupsData = (ByteBuffer)values.get(j++);
                GroupType type = mGroupTypes.get(typeName);
                type.fillGroups(typeGroupsData.array());
            }
        }
        return this;

    }



    public GroupType createGroupType(String name, int inputOutputNodes, int memoryCellNodes, int defaultGroupWeight, int featureBufferSize, int minimumBufferOverlap, LearningConfiguration config){
        GroupType type = new GroupType(inputOutputNodes, memoryCellNodes, defaultGroupWeight, featureBufferSize, minimumBufferOverlap, config).setName(name);
        mGroupTypes.put(name, type);
        return type;
    }

    public GroupType createGroupType(String name, int inputOutputNodes, int memoryCellNodes, int defaultGroupWeight, int featureBufferSize, int minimumBufferOverlap){
        return createGroupType(name, inputOutputNodes, memoryCellNodes, defaultGroupWeight, featureBufferSize, minimumBufferOverlap, mBaseLearningConfiguration);
    }

    public static LearningConfiguration getFeatureLearningConfiguration(){
        LearningConfiguration configuration = new LearningConfiguration();
        configuration.set(LearningConfiguration.KEY.ANNEALING_FRACTION, Double.valueOf(0.1F));
        configuration.set(LearningConfiguration.KEY.BEST_SOLUTION_BONUS_MILLI, Integer.valueOf(500));
        configuration.set(LearningConfiguration.KEY.NUM_SOLUTION_BUFFER, Math.max(1, Integer.valueOf(Math.round(10.0F/FeatureModel.THREAD_COUNT))));
        configuration.set(LearningConfiguration.KEY.INITIAL_RANDOM_FRACTION, Float.valueOf(0.5F));
        configuration.set(LearningConfiguration.KEY.MAX_DURATION_MILLI, Integer.valueOf(3000));
        configuration.setMatchEqualityError(0.1);
        configuration.setHistoryLength(4);
        //configuration.set(LearningConfiguration.KEY.MAX_ITERATIONS, Integer.valueOf(3000));
        //configuration.set(LearningConfiguration.KEY.BEST_SOLUTION_BONUS_ITERATIONS, Integer.valueOf(400));
        return configuration;
    }


    public WorldModel incrementGroupAllocationCount(String groupKey){
        mGroupTypes.entrySet().stream().map(pair->pair.getValue()).forEach(type->{
            type.getGroups().entrySet().stream().filter(epair->epair.getKey().equals(groupKey)).map(epair->epair.getValue()).forEach(spec->spec.incrementAllocations());
        });


        return this;
    }

    public Group addGroup(String name, GroupType type){

        return mGroupTypes.get(type.getName()).addGroup(name);
    }


    public Group addGroup(String name, String type){
        return addGroup(name, mGroupTypes.get(type));
    }


    public GroupType getGroupType(String name){
        return mGroupTypes.get(name);
    }

    public ArrayList<GroupSpecification> findGroups(String name){
        ArrayList<GroupSpecification> g = new ArrayList<>();
        mGroupTypes.entrySet().stream().map(p->p.getValue()).filter(type->type.getGroups().containsKey(name)).forEach(type->g.add(type.getGroups().get(name)));
        return g;
    }

    public ArrayList<GroupSpecification> getGroups(GroupType type){
        ArrayList<GroupSpecification> o = new ArrayList<>();
        o.addAll(mGroupTypes.get(type.getName()).getGroups().entrySet().stream().map(pair->pair.getValue()).collect(Collectors.toList()));
        return o;
    }

    public GroupSpecification getGroup(GroupType type, String name){

        return getGroup(type.getName(), name);
    }

    public GroupSpecification getGroup(String typename, String name){

        return mGroupTypes.get(typename).getGroups().get(name);
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


        HashSet<String> groupTypeNameSet = new HashSet<>();
        if (groupTypeNames == null){
            groupTypeNameSet.addAll(mGroupTypes.keySet());
        }
        else
            groupTypeNameSet.addAll(Arrays.stream(groupTypeNames).collect(Collectors.toList()));

        if (groupNames != null && groupNames.length>0){
            for (String s:groupNames){
                out.addAll(findGroups(s).stream().filter(Spec->groupTypeNameSet.contains(Spec.getType().getName())&&Spec.getType().getInputOutputNodes()==inputWidth).collect(Collectors.toList()));
            }
        }
        else {
            groupTypeNameSet.stream().map(n->mGroupTypes.get(n)).filter(type->type.getInputOutputNodes()==inputWidth).forEach(type->{
                out.addAll(getGroups(type));
            });
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
