package com.evolved.automata.nn.util;

import com.evolved.automata.AITools;
import com.evolved.automata.WeightedValue;
import com.evolved.automata.nn.NNTools;
import com.evolved.automata.nn.Vector;

import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.PriorityQueue;
import java.util.stream.Collectors;

/**
 * Created by Evolved8 on 5/28/18.
 */

public class Group {

    public enum MODE {
        LEARNING, EXTRAPOLATION, MIXED, SLEEPING
    }

    public static class FeatureValueMetadata {
        double _prefCount = 0;
        double _incidenceCount = 1;

        public FeatureValueMetadata updateInstance(){
            _incidenceCount++;
            return this;
        }

        public FeatureValueMetadata updatePreference(){
            _prefCount++;
            _incidenceCount++;
            return this;
        }

        @Override
        public String toString(){
            return String.format("[%1$d %2$d %3$.3f", (int)_incidenceCount, (int)_prefCount, getPreferenceFraction());
        }

        public double getPreferenceFraction(){
            return _prefCount/_incidenceCount;
        }

        public double getPreferenceCount(){
            return _prefCount;
        }

        public FeatureValueMetadata decay(double decayFraction){
            _prefCount = _prefCount * decayFraction;
            return this;
        }

        public FeatureValueMetadata copy(){
            FeatureValueMetadata vp = new FeatureValueMetadata();
            vp._incidenceCount = _incidenceCount;
            vp._prefCount = _prefCount;
            return vp;
        }

        public FeatureValueMetadata reset(){
            _prefCount = 0;
            _incidenceCount = 1;
            return this;
        }

    }

    public interface MemoryManagementListener {
        void onStartMemoryManagement(int totalAllocation);
        void onFinishedMemoryManagement(ArrayList<Pair<String, ArrayList<Vector>>> recycled);

    }


    PriorityQueue<FeatureModel> mGroupHeap;
    String mKey;
    LearningConfiguration mConfiguration;
    HashMap<Integer, FeatureModel> mFeatureMap;
    Integer[] mIndexIterator;
    int _nextIndex = 0;

    HashMap<Integer, FeatureValueMetadata> mPreferenceMap = new HashMap<>();


    int mUpdateCount = 0;
    int mResetInterval = 10;
    public final double DECAY_FRACTION = 0.1;
    MODE mMode = MODE.LEARNING;

    WorldModel mWorld;

    boolean mDreamWithFractionP = true;
    boolean mDebugEnabledP = false;

    MemoryManagementListener mMemorymanagementListener = null;

    HashSet<Integer> mCompleteFeatures = new HashSet<>();
    HashSet<Integer> mInternalAvailable = new HashSet<>();

    FeatureModel mFocusModel = null;
    FeatureModel mBufferModel = null;
    double mSlowLearningThresholdMultiple = 5;
    int mMinimumTemporalSampleCount = 20;
    boolean mAutoResetOnDurationExceptionP = false;

    int mMinimumUpdateCountForRecycle = 10;

    int MAX_BUFFER_MILLI = 30*1000; // 30 seconds
    boolean mEnableMemoryManagmentP = true;
    boolean mResetOnTimeoutsP = false;
    boolean mLimitBufferStateP = true;




    public static class FeatureMetaData {
        public int _allocationIndex = 0;
        public long _learningTime = 0;
        public int _updateCount = 0;
        public int _averageLearningTime = 0;
        public int _usageCount = 0;
        public boolean _isInUseP = false;

        public FeatureMetaData reset(){
            _isInUseP = false;
            _usageCount = 0;
            _averageLearningTime = 0;
            _updateCount = 0;
            _learningTime = 0;
            return this;
        }

        public boolean isInUse(){
            return _isInUseP;
        }

        public FeatureMetaData setInUse(boolean isInUse){
            _isInUseP = isInUse;
            return this;
        }

        public int getAllocationIndex(){
            return _allocationIndex;
        }

        public FeatureMetaData setAllocationIndex(int index){
            _allocationIndex = index;
            return this;
        }

        public long getTotalLearningTime(){
            return _learningTime;
        }

        public void updateStepLearningMilli(long time){
            _averageLearningTime = (int)(_averageLearningTime*_updateCount/(1.0 + _updateCount) + time/(1.0 + _updateCount));
            _updateCount++;
            _learningTime+=time;
        }

        public int getAvgLearningTime(){
            return _averageLearningTime;
        }

        public int getUsageCount(){
            return _usageCount;
        }

        public FeatureMetaData updateUsageCount(){
            _usageCount++;
            return this;
        }
    }

    public static class DreamSpec {
        ArrayList<Vector> _totalDreamSequence;
        StringBuilder _stringForm;
        ArrayList<Pair<Integer, ArrayList<Vector>>> _dreamOrder;
        HashMap<Integer, Integer> _selectionCount;
        HashMap<Integer, FeatureValueMetadata> _dreamPrefs;
        boolean _includeReps = false;
        HashSet<Integer> _dreamedValues;

        public DreamSpec(){
            _dreamedValues = new HashSet<Integer>();
            _totalDreamSequence = new ArrayList<>();
            _stringForm = new StringBuilder();
            _dreamOrder = new ArrayList<Pair<Integer, ArrayList<Vector>>>();
            _selectionCount = new HashMap<>();
        }

        public HashMap<Integer, Integer> getSelectionCountMap(){
            return _selectionCount;
        }

        public DreamSpec updateInput(Integer featureIndex, ArrayList<Vector> values, LearningConfiguration config){
            AITools.incrementMap(_selectionCount, featureIndex);
            if (_dreamedValues.contains(featureIndex))
            {
                if (_stringForm.length() > 0 && _includeReps){
                    _stringForm.append(", ");
                    _stringForm.append("[").append(featureIndex).append("]");
                }
                return this;
            }

            _dreamedValues.add(featureIndex);
            if (_stringForm.length() > 0){
                _stringForm.append(", ");
            }
            _stringForm.append("[").append(featureIndex).append("]");
            boolean first = true;
            for (Vector v:values){
                if (!first){
                    _stringForm.append(", ");
                }
                _stringForm.append(config.getInputString(v.rawFloat()));
                first = false;
            }
            _dreamOrder.add(Pair.of(featureIndex, values));
            _totalDreamSequence.addAll(values);
            return this;
        }

        public String getDreamedValue(){
            return _stringForm.toString();
        }

        public DreamSpec setDreamPrefs(HashMap<Integer, FeatureValueMetadata> s){
            _dreamPrefs = s;
            return this;
        }

        public HashMap<Integer, FeatureValueMetadata> getDreamPreferences(){
            return _dreamPrefs;
        }

    }

    Comparator<FeatureModel> mStandardValueComparator = new Comparator<FeatureModel>() {
        @Override
        public int compare(FeatureModel prior, FeatureModel other)
        {
            float[] priorPrediction = NNTools.roundToInt(prior.getPredictedOutput().rawFloat());
            float[] otherPredction = NNTools.roundToInt(other.getPredictedOutput().rawFloat());

            boolean priorValid = mConfiguration.isValidInput(priorPrediction);
            boolean otherValid = mConfiguration.isValidInput(otherPredction);
            updateIncidence(prior);
            updateIncidence(other);

            if (priorValid && !otherValid)
            {
                updatePreference(prior);
                return -1;
            }
            else if (otherValid && !priorValid){
                updatePreference(other);
                return 1;
            }


            int score = 0;
            if (prior.getState() == FeatureModel.STATE.MATCHING &&
                    other.getState() != FeatureModel.STATE.MATCHING)
            {

                score = -1;
            }
            else if (other.getState() == FeatureModel.STATE.MATCHING &&
                    prior.getState() != FeatureModel.STATE.MATCHING)
            {
                score = 1;
            }
            else if (prior.getState() == FeatureModel.STATE.MATCHING &&
                    other.getState() == FeatureModel.STATE.MATCHING)
            {
                if (prior.matchedLastInput() && !other.matchedLastInput()){
                    score = -1;
                }
                else if (!prior.matchedLastInput() && other.matchedLastInput()){
                    score = 1;
                }
                else
                    score = -1* Double.compare(prior.getDistanceToFinalState(), other.getDistanceToFinalState());
            }
            else {
                score = -1* Double.compare(prior.getMatchHistory().getTrend(), other.getMatchHistory().getTrend());
            }


            if (score == -1)
                updatePreference(prior);
            else if (score == 1){
                updatePreference(other);
            }
            return score;
        }
    };

    Comparator<FeatureModel> mPassiveValueComparator = new Comparator<FeatureModel>() {
        @Override
        public int compare(FeatureModel prior, FeatureModel other)
        {
            float[] priorPrediction = NNTools.roundToInt(prior.getPredictedOutput().rawFloat());
            float[] otherPredction = NNTools.roundToInt(other.getPredictedOutput().rawFloat());

            boolean priorValid = mConfiguration.isValidInput(priorPrediction);
            boolean otherValid = mConfiguration.isValidInput(otherPredction);
            updateIncidence(prior);
            updateIncidence(other);

            if (priorValid && !otherValid)
            {
                updatePreference(prior);
                return -1;
            }
            else if (otherValid && !priorValid){
                updatePreference(other);
                return 1;
            }


            int score = 0;
            if (prior.getState() == FeatureModel.STATE.MATCHING &&
                    other.getState() != FeatureModel.STATE.MATCHING)
            {

                score = -1;
            }
            else if (other.getState() == FeatureModel.STATE.MATCHING &&
                    prior.getState() != FeatureModel.STATE.MATCHING)
            {
                score = 1;
            }
            else if (prior.getState() == FeatureModel.STATE.MATCHING &&
                    other.getState() == FeatureModel.STATE.MATCHING)
            {
                if (prior.matchedLastInput() && !other.matchedLastInput()){
                    score = -1;
                }
                else if (!prior.matchedLastInput() && other.matchedLastInput()){
                    score = 1;
                }
                else
                    score = -1* Double.compare(prior.getDistanceToFinalState(), other.getDistanceToFinalState());
            }
            else {
                score = -1* Double.compare(prior.getMatchHistory().getTrend(), other.getMatchHistory().getTrend());
            }

            if (score == -1)
                updatePreference(prior);
            else if (score == 1){
                updatePreference(other);
            }
            return score;
        }
    };

    public FeatureMetaData createMetadata(int index){
        FeatureMetaData mdata = new FeatureMetaData();
        mdata._allocationIndex = index;
        return mdata;
    }

    boolean mDreamToFreeMemoryP = true;
    boolean mSortByUsageP = false;
    double mRecycleCutoffFraction = 0.2F;
    int mMinimumUsageForRecycle = 10;
    int mAvgStepLearningMilli = 0;
    int mTemporalUpdateCount = 0;
    PriorityQueue<Integer> recycleQueue = null;
    WorldModel.GroupType mType;

    PriorityQueue<FeatureModel> mFocusQueue;

    public void updateStepLearningMilli(long time){
        mAvgStepLearningMilli = (int)(mAvgStepLearningMilli*mTemporalUpdateCount/(1.0 + mTemporalUpdateCount) + time/(1.0 + mTemporalUpdateCount));
        mTemporalUpdateCount++;
    }



    public Group(String key, LearningConfiguration configuration, WorldModel world, WorldModel.GroupType type){
        mWorld = world;
        mType = type;
        mFeatureMap = new HashMap<>();
        mIndexIterator = new Integer[0];
        mConfiguration = configuration;

        mKey = key;
        mGroupHeap = new PriorityQueue<>(10, mStandardValueComparator);
        mFocusQueue = new PriorityQueue<>(10, mPassiveValueComparator);
    }

    public Group setMemoryListener(MemoryManagementListener listener){
        mMemorymanagementListener = listener;
        return this;
    }

    public Group setDebugEnabled(boolean enabled){
        mDebugEnabledP = enabled;
        return this;
    }

    public Group setLearningBoundaryMultiple(double multiple){
        mSlowLearningThresholdMultiple = multiple;
        return this;
    }

    public Group setBoundaryOnOvertime(boolean enabled){
        mAutoResetOnDurationExceptionP = enabled;
        return this;
    }

    public Group setMinimumOvertimeSampleCount(int count){
        mMinimumTemporalSampleCount = count;
        return this;
    }

    public Group setMinimumRecycleUsageCount(int minimumUsage){
        mMinimumUsageForRecycle  =minimumUsage;
        return this;
    }

    public Group setDreamToFreeMemory(boolean enabled){
        mDreamToFreeMemoryP  =enabled;
        return this;
    }

    public Group setSortDreamPreferencesByUsage(boolean yes){
        mSortByUsageP = yes;
        return this;
    }

    public Group setMemoryRecycleFraction(double recycleFraction){
        mRecycleCutoffFraction = Math.abs(recycleFraction);
        return this;
    }


    public String getName(){
        return mKey;
    }

    public Group setMode(MODE mode){
        mMode = mode;
        if (mMode == MODE.LEARNING){
            if (mFocusModel != null){

            }
            mFocusModel = null;
            mBufferModel = null;

        }
        return this;
    }

    public MODE processInput(Vector input){
        switch (mMode){
            case LEARNING:
            {
                mMode = learnNextInput(input);
            }
            break;
            case SLEEPING:
            case EXTRAPOLATION:
            {
                processAllCompleteModels(input);
            }
            break;
            case MIXED:
            {
                processAllCompleteModels(input);
                mMode = learnNextInput(input);

            }
            break;
        }
        return mMode;
    }


    public MODE learnNextInput(Vector input){
        
        if (mFocusModel == null){
            mFocusModel = getAnotherFeature();
        }

        if (mFocusModel == null){
            return MODE.EXTRAPOLATION;
        }

        if (mBufferModel == null){
            mBufferModel = getAnotherFeature();
        }

        mFocusModel.setLabel("CURRENT");
        int featureBufferSize = mType.getFeatureBufferSize();
        int minimumBufferOverlap = mType.getMinimumBufferOverlap();
        long startTime, duration;

        if (!mFocusModel.isMatching()){
            if (mFocusModel.getFeatureLength() == featureBufferSize + minimumBufferOverlap)
            {
                mFocusModel.setConfiguration(mConfiguration);
            }

            startTime = System.currentTimeMillis();
            System.out.println("Extending model");
            mFocusModel.processNextInput(input);
            duration = System.currentTimeMillis() - startTime;

            if (mFocusModel.isBuilding()){
                updateStepLearningMilli(duration);
                FeatureMetaData featureMetaData = getFeatureInternalMetaData(mFocusModel);
                featureMetaData.updateStepLearningMilli(duration);
            }
            else
                mFocusModel.setLabel("");
        }
        else {
            System.out.println("Recognition skip");
        }

        if (mBufferModel != null){
            if (mBufferModel.getFeatureLength() == featureBufferSize){
                mBufferModel.shiftForward();
            }

            System.out.println("Building buffer");
            mBufferModel.processNextInput(input);

            if (mBufferModel.isComplete()){
                mBufferModel.setLabel("");
                mBufferModel = null;
                System.out.println("Buffer exhausted");
            }
            else if (mFocusModel.isFinished()){
                mFocusModel.setLabel("");
                mFocusModel = mBufferModel;
                mFocusModel.setLabel("CURRENT");
                mBufferModel = null;
            }
        }
        else if (mFocusModel.isFinished()){
            mFocusModel.setLabel("");
            mFocusModel = null;
            mMode = MODE.EXTRAPOLATION;
        }

        return mMode;
    }

    public DreamSpec dream(){
        if (mFeatureMap.size() > 0){
            double maxFraction = mPreferenceMap.keySet().stream().map(i->Double.valueOf(mPreferenceMap.get(i).getPreferenceFraction())).reduce(0D, (Double left, Double right)->Math.max(left, right));

            if (maxFraction > 0){
                ArrayList<WeightedValue<Integer>> weights = new ArrayList<>();
                weights.addAll(mPreferenceMap.keySet().stream().filter(index->mFeatureMap.get(index).isComplete()).map((Integer key)->{
                    return new WeightedValue<Integer>(key, mPreferenceMap.get(key).getPreferenceFraction());
                }).collect(Collectors.toList()));

                return dream(AITools.ChooseWeightedRandomFair(weights).GetValue());
            }
            else {
                Integer[] keys = mFeatureMap.keySet().stream().filter(i -> mFeatureMap.get(i).isComplete()).collect(Collectors.toList()).toArray(new Integer[0]);

                int randomIndex = (int)(Math.random()*keys.length);
                return dream(keys[randomIndex]);
            }

        }
        else
            return null;
    }

    FeatureModel selectedGoodModel(){
        if (mPreferenceMap.size() == 0)
            return null;

        ArrayList<WeightedValue<Integer>> prefs = new ArrayList<WeightedValue<Integer>>();

        for (Integer allocationIndex:mPreferenceMap.keySet().stream().filter( i->mFeatureMap.get(i).isComplete()).collect(Collectors.toList())){
            if (mDreamWithFractionP)
                prefs.add(new WeightedValue<Integer>(allocationIndex, mPreferenceMap.get(allocationIndex).getPreferenceFraction()));
            else
                prefs.add(new WeightedValue<Integer>(allocationIndex, mPreferenceMap.get(allocationIndex).getPreferenceCount()));
        }

        WeightedValue<Integer> selected = AITools.ChooseWeightedRandomFair(prefs);
        return mFeatureMap.get(selected.GetValue());
    }

    HashMap<Integer, FeatureValueMetadata> copyPrefs(HashMap<Integer, FeatureValueMetadata> p){
        HashMap<Integer, FeatureValueMetadata> n = new HashMap<Integer, FeatureValueMetadata>();
        for (Integer k:p.keySet()){
            n.put(k, p.get(k).copy());
        }
        return n;
    }

    public DreamSpec dream(int dreamFocusIndex){
        MODE priorMode = mMode;
        mMode = MODE.SLEEPING;

        HashMap<Integer, FeatureValueMetadata> backup = copyPrefs(mPreferenceMap);

        boolean scaleDreamsByFeatureLength = true;

        DreamSpec dreamedOutput = new DreamSpec();

        int maxLength = 0;
        boolean debugP = mDebugEnabledP;
        ArrayList<FeatureModel> out = null;

        for (int cycle = 0; cycle < 30;cycle++){
            final Integer dreamIndex = dreamFocusIndex;
            FeatureModel dreamFocus = mFeatureMap.get(dreamIndex);
            ArrayList<Vector> dreamInput = dreamFocus.extrapFeature();
            dreamedOutput.updateInput(dreamIndex, dreamInput, mConfiguration);


            final HashSet<Integer> exclusion = new HashSet<>();
            exclusion.add(dreamIndex);
            if (mFocusModel != null){
                exclusion.add(getFeatureInternalMetaData(mFocusModel)._allocationIndex);
            }
            if (mBufferModel != null){
                exclusion.add(getFeatureInternalMetaData(mBufferModel)._allocationIndex);
            }

            ArrayList<FeatureModel> dreamModels = new ArrayList<>();
            mPreferenceMap.keySet().stream().filter((Integer i)->!exclusion.contains(i) && mFeatureMap.get(i).isComplete()).forEach((Integer i)->{
                FeatureModel m = mFeatureMap.get(i);
                dreamModels.add(m);
                m.resetRecognition();

            });

            PriorityQueue<FeatureModel> dreamQueue = new PriorityQueue<>(10, mStandardValueComparator);

            int len = dreamInput.size(), i = 1;
            for (Vector input:dreamInput){

                for (FeatureModel dreamModel:dreamModels){
                    dreamModel.processNextInput(input);
                    if (i == len){
                        dreamQueue.add(dreamModel);
                    }
                }
                i++;
            }

            FeatureModel best = dreamQueue.peek();
            if (best.isMatching())
            {
                dreamFocusIndex = getFeatureInternalMetaData(best).getAllocationIndex();
            }
            else {
                ArrayList<WeightedValue<Integer>> prefs = new ArrayList<WeightedValue<Integer>>();

                double weight;
                if (scaleDreamsByFeatureLength){
                    maxLength = dreamModels.stream().map(m->m.getFeatureLength()).reduce(0, (l,r)->Math.max(l, r));
                }

                for (FeatureModel model:dreamModels){
                    Integer modelIndex = getFeatureInternalMetaData(model).getAllocationIndex();

                    if (scaleDreamsByFeatureLength)
                        weight = model.getFeatureLength()/maxLength;
                    else
                        weight = 1;

                    if (mDreamWithFractionP)
                        prefs.add(new WeightedValue<Integer>(modelIndex, weight* mPreferenceMap.get(modelIndex).getPreferenceFraction()));
                    else
                        prefs.add(new WeightedValue<Integer>(modelIndex, weight* mPreferenceMap.get(modelIndex).getPreferenceCount()));
                }

                WeightedValue<Integer> selected = AITools.ChooseWeightedRandomFair(prefs);
                dreamFocusIndex = selected.GetValue();
            }
        }

        mMode =  priorMode;
        Integer [] prefKeys = mPreferenceMap.keySet().toArray(new Integer[0]);
        for (Integer k:prefKeys){
            if (!mFeatureMap.get(k).isComplete()){
                mPreferenceMap.remove(k);
            }
        }

        dreamedOutput.setDreamPrefs(mPreferenceMap);
        mPreferenceMap = copyPrefs(backup);
        return dreamedOutput;
    }


    public void setMemoryManagement(boolean set){
        mEnableMemoryManagmentP = set;
    }


    public FeatureModel getAnotherFeature(){
        FeatureModel model = mWorld.requestFeature(this), selected = null;
        FeatureMetaData meta = null;
        if (model == null){
            if (recycleQueue!= null && recycleQueue.size() > 0){
                model = mFeatureMap.get(recycleQueue.poll());
            }

            if (recycleQueue != null && recycleQueue.size() > 0 && model == null){
                System.out.println("RFucdskf");
            }

            if (mEnableMemoryManagmentP) {
                if (model == null && mDreamToFreeMemoryP){
                    if (mMemorymanagementListener != null){
                        mMemorymanagementListener.onStartMemoryManagement(mFeatureMap.size());
                    }

                    DreamSpec dreamResult = dream();
                    HashMap<Integer, Integer> selectionCount = dreamResult.getSelectionCountMap();
                    HashMap<Integer, FeatureValueMetadata> prefs = dreamResult.getDreamPreferences();

                    double maxLength = mFeatureMap.keySet().stream().map(i->mFeatureMap.get(i).getFeatureLength()).reduce(0, (Integer left, Integer right)->{
                       return Math.max(left.intValue(), right.intValue());
                    });

                    Comparator<Integer> usageComparator = new Comparator<Integer>() {
                        @Override
                        public int compare(Integer left, Integer right)
                        {
                            return Double.compare(selectionCount.get(left)*mFeatureMap.get(left).getFeatureLength()/maxLength, selectionCount.get(right)*mFeatureMap.get(right).getFeatureLength()/maxLength);
                        }
                    };

                    Comparator<Integer> prefComparator = new Comparator<Integer>() {
                        @Override
                        public int compare(Integer left, Integer right)
                        {
                            return Double.compare(prefs.get(left).getPreferenceFraction()*mFeatureMap.get(left).getFeatureLength()/maxLength, prefs.get(right).getPreferenceFraction()*mFeatureMap.get(right).getFeatureLength()/maxLength);
                        }
                    };

                    Comparator<Integer> selectedComparator;
                    if (mSortByUsageP)
                        selectedComparator = usageComparator;
                    else
                        selectedComparator = prefComparator;

                    recycleQueue = new PriorityQueue<>(10, selectedComparator);
                    int length = prefs.size();
                    int recycleCount = (int)Math.round(length * mRecycleCutoffFraction);

                    ArrayList<Pair<String, ArrayList<Vector>>> managementResults = new ArrayList<>();
                    ArrayList<FeatureModel> freed = new ArrayList<>();

                    int i = 0;

                    for (Integer selectedIndex:prefs.keySet().stream().sorted(prefComparator).collect(Collectors.toList())){
                        if (i >= recycleCount){
                            break;
                        }

                        model = mFeatureMap.get(selectedIndex);
                        if (mMemorymanagementListener != null){

                            ArrayList<Vector> rawExtra = model.extrapFeature();

                            ArrayList<String> sResult = new ArrayList<String>();
                            rawExtra.forEach((Vector v)->sResult.add(mConfiguration.getInputString(v.rawFloat())));
                            String displayForm = model.toString() + " -> " + sResult.toString();
                            managementResults.add(Pair.of(displayForm, rawExtra));

                        }

                        recycleQueue.add(selectedIndex);
                        meta = getFeatureInternalMetaData(model);
                        meta.reset();
                        model.forceInitialState();
                        FeatureValueMetadata value = mPreferenceMap.get(selectedIndex);
                        value.reset();

                        freed.add(model);
                        i++;
                    }

                    if (recycleQueue.size()>0)
                        model = mFeatureMap.get(recycleQueue.poll());
                    else
                        return null;
                }
            }
            else
                return null;

        }
        else {
            addFeature(model);
        }

        if (model != null){
            if (mLimitBufferStateP) {
                model.setConfiguration(mConfiguration.getUnlimitedConfiguration(MAX_BUFFER_MILLI));
            }
            else {
                model.setConfiguration(mConfiguration.getUnlimitedConfiguration());
            }

            meta = getFeatureInternalMetaData(model);
            meta.reset();
            meta.setInUse(true);
            model.setLabel("");
        }
        return model;
    }

    private void updatePreference(FeatureModel best){
        FeatureMetaData meta = getFeatureInternalMetaData(best);

        Integer b = Integer.valueOf(meta.getAllocationIndex());

        if (!mPreferenceMap.containsKey(b)){
            mPreferenceMap.put(b, new FeatureValueMetadata());
        }
        mPreferenceMap.get(b).updatePreference();
    }

    private void updateIncidence(FeatureModel best){
        FeatureMetaData meta = getFeatureInternalMetaData(best);

        Integer b = Integer.valueOf(meta.getAllocationIndex());

        if (!mPreferenceMap.containsKey(b)){
            mPreferenceMap.put(b, new FeatureValueMetadata());
        }
        mPreferenceMap.get(b).updateInstance();
    }

    public MODE getMode(){
        return mMode;
    }

    public HashMap<Integer, FeatureValueMetadata> getPreferenceMap(){
        return mPreferenceMap;
    }


    public Group addFeature(FeatureModel model){
        return addFeature(model, _nextIndex);
    }

    FeatureMetaData getFeatureInternalMetaData(FeatureModel model){
        FeatureMetaData data = (FeatureMetaData)model.getMetaData();
        if (data == null){
            model.setMetaData(data = new FeatureMetaData());
        }
        return data;
    }

    FeatureValueMetadata getFeatureValueMetadata(FeatureModel model){
        FeatureMetaData internalMeta = getFeatureInternalMetaData(model);
        return mPreferenceMap.get(internalMeta.getAllocationIndex());
    }


    public Group addFeature(FeatureModel model, int index){
        FeatureMetaData meta = getFeatureInternalMetaData(model);
        meta.setAllocationIndex(index);

        mFeatureMap.put(Integer.valueOf(index), model);

        FeatureValueMetadata valueMetadata = new FeatureValueMetadata();
        mPreferenceMap.put(Integer.valueOf(index), valueMetadata);

        int i=0;
        for (;mFeatureMap.containsKey(Integer.valueOf(i)); i++){

        }
        _nextIndex = i;
        return this;
    }

    public Group removeFeature(int index){
        mFeatureMap.remove(Integer.valueOf(index));
        return this;
    }

    public ArrayList<FeatureModel> getAllCompleteFeatures(){
        ArrayList<FeatureModel> out = new ArrayList<FeatureModel>();
        mGroupHeap.clear();
        for (Integer index:mFeatureMap.keySet()){

            FeatureModel model = mFeatureMap.get(index);
            if (model.isComplete()){
                out.add(model);
                mGroupHeap.add(model);
            }

        }
        return out;
    }

    public ArrayList<FeatureModel> getOrderedFeatures(){
        getAllCompleteFeatures();
        ArrayList<FeatureModel> out = new ArrayList<>();

        while (mGroupHeap.size() > 0){
            out.add(mGroupHeap.poll());
        }
        return out;
    }

    public boolean isSleeping(){
        return mMode == MODE.SLEEPING;
    }

    public void processAllCompleteModels(Vector input){
        ArrayList<FeatureModel> out = new ArrayList<>();
        mGroupHeap.clear();
        mFocusQueue.clear();

        for (Integer index: mFeatureMap.keySet()){
            FeatureModel model = mFeatureMap.get(index);
            if (model.isComplete()){
                model.processNextInput(input);
                if (!isSleeping()){
                    getFeatureInternalMetaData(model).updateUsageCount();
                }
                else if (model.isMatching()){
                    mFocusQueue.add(model);
                }

                mGroupHeap.add(model);
            }
        }

        if (mFocusQueue.size()>0 && (mFocusModel==null || mFocusModel.isNonMatching())){
            if (mFocusModel!=null && mFocusModel.isNonMatching())
            {
                mFocusModel.setLabel("");
            }
            mFocusModel = mFocusQueue.poll();
            mFocusModel.setLabel("CURRENT");
            System.out.println("Setting recognition focus");
        }
        else if (mFocusModel != null && mFocusModel.isNonMatching()){
            System.out.println("Discarding prior focus");
            if (mBufferModel != null){
                System.out.println("Using buffer as new focus");
                mFocusModel.setLabel("");
                mFocusModel = mBufferModel;
                mFocusModel.setLabel("CURRENT");
                mBufferModel = null;
            }
            else {
                mFocusModel.setLabel("");
                mFocusModel = null;
            }
        }
    }

    public void decayPreferenceMap(){
        for (Integer index:mPreferenceMap.keySet()){
            mPreferenceMap.get(index).decay(DECAY_FRACTION);
        }
    }

    // TODO: fix this when onlyComplete is false
    public Group resetAll(boolean onlyComplete){

        if (!onlyComplete){
            if (mBufferModel != null){
                mBufferModel.setLabel("");
                mBufferModel.forceComplete();
            }

            if (mFocusModel != null){
                mFocusModel.setLabel("");
                mFocusModel.forceComplete();
            }

            mFocusModel = null;
            mBufferModel = null;
        }

        boolean updateP = false;
        for (Integer index: mFeatureMap.keySet().stream().filter(i->(mFeatureMap.get(i).isComplete())).collect(Collectors.toList()) ){
            FeatureModel model = mFeatureMap.get(index);
            model.setLabel("");
            updateP = true;
            model.resetRecognition();

        }

        if (updateP){
            mUpdateCount++;
            if (mUpdateCount == mResetInterval){
                decayPreferenceMap();
                mUpdateCount = 0;
            }
        }

        return this;
    }

    public ArrayList<FeatureModel> getAllFeatures(){
        ArrayList<FeatureModel> models = new ArrayList<FeatureModel>();
        Integer[] order = mFeatureMap.keySet().toArray(new Integer[0]);
        Arrays.sort(order, new Comparator<Integer>() {
            @Override
            public int compare(Integer left, Integer right)
            {
                return Integer.compare(left.intValue(), right.intValue());
            }
        });

        for (Integer index:order){
            models.add(mFeatureMap.get(index));
        }
        return models;
    }

    public Integer[] getPreference(){
        Integer[] s = mPreferenceMap.keySet().toArray(new Integer[0]);
        Arrays.sort(s, new Comparator<Integer>() {
            @Override
            public int compare(Integer left, Integer right)
            {
                return -1 * Double.compare(mPreferenceMap.get(left).getPreferenceFraction(), mPreferenceMap.get(right).getPreferenceFraction());
            }
        });
        return s;
    }

    public FeatureModel getFeature(Integer index){
        return mFeatureMap.get(index);
    }

}
