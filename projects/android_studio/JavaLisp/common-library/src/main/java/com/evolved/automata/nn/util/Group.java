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
import java.util.Optional;
import java.util.PriorityQueue;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toList;

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

        public FeatureValueMetadata increaseValuePercent(double fraction){
            _incidenceCount+=(_incidenceCount - _prefCount)*fraction;
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

    FeatureModel mFocusModel = null;
    FeatureModel mBufferModel = null;

    int MAX_BUFFER_MILLI = 30*1000; // 30 seconds
    boolean mEnableMemoryManagmentP = true;
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

    boolean mSleepToFreeMemoryP = true;
    boolean mSortByUsageP = false;
    double mRecycleCutoffFraction = 0.2F;
    int mMinimumUsageForRecycle = 10;
    int mAvgStepLearningMilli = 0;
    int mTemporalUpdateCount = 0;
    PriorityQueue<Integer> recycleQueue = null;
    WorldModel.GroupType mType;
    ArrayList<FeatureModel> mLastProcessedFeatures = new ArrayList<FeatureModel>();
    PriorityQueue<FeatureModel> mFocusQueue;
    double mSleepCycleMultipler = 1;


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

    /**
     * Sets the maximum amount of time to try to learn the next input
     * @param milli
     * @return
     */
    public Group setMaximumAmountOfProcessTime(int milli){
        MAX_BUFFER_MILLI = milli;
        return this;
    }

    /**
     * Sets the minimum number of times processCompleteFeatures have to be run on a FeatureModel
     * before its preference weights can be decayed
     * @param minimumUsage
     * @return
     */
    public Group setMinimumRecycleUsageCount(int minimumUsage){
        mMinimumUsageForRecycle  =minimumUsage;
        return this;
    }

    /**
     * Indicates whether the Group uses sleeping to free memory.  This setting only takes effect
     * if
     * @param enabled
     * @return
     */
    public Group setSleepToFreeMemory(boolean enabled){
        mSleepToFreeMemoryP =enabled;
        return this;
    }

    /**
     * When true, sleeping frees FeatureModels by sorting them by the number of times that they
     * were encountered during sleep.  Otherwise, FeatureModels are sorted by their preference
     * value
     * @param yes
     * @return
     */
    public Group setSortDreamPreferencesByUsage(boolean yes){
        mSortByUsageP = yes;
        return this;
    }

    /**
     * Sets the fraction of FeatureModels that can be recycled at a time when
     * memory management is enabled.  Defaults to 0.2
     * @param recycleFraction
     * @return
     */
    public Group setMemoryRecycleFraction(double recycleFraction){
        mRecycleCutoffFraction = Math.abs(recycleFraction);
        return this;
    }

    /**
     * Sets the multiplier for number of times that the Group selects a FeatureModel to extrapolate
     * over during sleeping.  The total number of sleep iteration cycles is [multipler] * {num features}
     * [mSleepCycleMultipler] defaults to 1
     * @param multipler
     * @return
     */
    public Group setSleepCycleMultipler(double multipler){
        mSleepCycleMultipler = multipler;
        return this;
    }


    public String getName(){
        return mKey;
    }

    public Group setMode(MODE mode){

        if (mMode != mode && (mode == MODE.EXTRAPOLATION)){

            if (mFocusModel != null){
                mFocusModel.setLabel("");
                if (!mFocusModel.isComplete())
                {
                    mFocusModel.forceComplete();
                }
            }

            if (mBufferModel != null){
                mBufferModel.setLabel("");
                if (!mBufferModel.isComplete())
                {
                    mBufferModel.forceComplete();
                }
            }
            
            mFocusModel = null;
            mBufferModel = null;

        }
        mMode = mode;
        return this;
    }

    /**
     * When [set] is true, FeatureModels can be reused if the WorldModel will not provide any more
     * Features.  Currently, the only way to reuse FeatureModels in Groups is via sleep, so this
     * will have no effect unless setSleepToFreeMemory was also called with true
     * @param set
     */
    public void setMemoryManagement(boolean set){
        mEnableMemoryManagmentP = set;
    }

    public MODE getMode(){
        return mMode;
    }

    public HashMap<Integer, FeatureValueMetadata> getPreferenceMap(){
        return mPreferenceMap;
    }

    /**
     * Returns the Features in this Group sorted by the feature's 'match value'.  Match value
     * is based on an absolutist measure of
     * @return
     */
    public ArrayList<FeatureModel> getOrderedFeatures(){
        ArrayList<FeatureModel> out = new ArrayList<FeatureModel>();
        out.addAll(mFeatureMap.keySet().stream().map(i->mFeatureMap.get(i)).sorted(mPassiveValueComparator).collect(toList()));
        return out;
    }

    public boolean isSleeping(){
        return mMode == MODE.SLEEPING;
    }

    /**
     * Asserts that a boundary has occurred.  Every subsequent input is causally independent
     * of the prior ones
     * @return
     */
    public Group resetAll(){

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

        for (Integer index: mFeatureMap.keySet().stream().filter(i->(mFeatureMap.get(i).isComplete())).collect(toList()) ){
            FeatureModel model = mFeatureMap.get(index);
            model.setLabel("");
            model.resetRecognition();

        }

        return this;
    }

    /**
     * Returns all features in this Group in order of creation
     * @return
     */
    public ArrayList<FeatureModel> getAllFeatures(){
        ArrayList<FeatureModel> models = new ArrayList<FeatureModel>();
        models.addAll(mFeatureMap.keySet().stream().sorted(Integer::compare).map(i->mFeatureMap.get(i)).collect(toList()));
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

    private void updateStepLearningMilli(long time){
        mAvgStepLearningMilli = (int)(mAvgStepLearningMilli*mTemporalUpdateCount/(1.0 + mTemporalUpdateCount) + time/(1.0 + mTemporalUpdateCount));
        mTemporalUpdateCount++;
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

    public MODE processInput(Vector input, HashSet<FeatureModel> exclusion){
        switch (mMode){
            case LEARNING:
            {
                mMode = learnNextInput(input);
            }
            break;
            case SLEEPING:
            case EXTRAPOLATION:
            {
                processAllCompleteModels(input, exclusion);
            }
            break;
            case MIXED:
            {
                processAllCompleteModels(input, exclusion);
                mMode = learnNextInput(input);

            }
            break;
        }
        return mMode;
    }

    private MODE learnNextInput(Vector input){
        
        if (mFocusModel == null){
            mFocusModel = getAnotherFeature();
        }

        if (mFocusModel == null){
            if (mFeatureMap.keySet().stream().map(i -> mFeatureMap.get(i)).allMatch(model->model.getState() == FeatureModel.STATE.INITIAL)){
                System.out.println("Bad");
            }
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

            if (mFeatureMap.keySet().stream().map(i -> mFeatureMap.get(i)).allMatch(model->model.getState() == FeatureModel.STATE.INITIAL)){
                System.out.println("Bad");
            }
        }

        return mMode;
    }


    /**

     * @return
     */
    public DreamSpec sleep(){
        if (mFeatureMap.size() > 0){
            double maxFraction = mPreferenceMap.keySet().stream().map(i->Double.valueOf(mPreferenceMap.get(i).getPreferenceFraction())).reduce(0D, (Double left, Double right)->Math.max(left, right));

            if (maxFraction > 0){
                ArrayList<WeightedValue<Integer>> weights = new ArrayList<>();
                weights.addAll(mPreferenceMap.keySet().stream().filter(index->mFeatureMap.get(index).isComplete()).map((Integer key)->{
                    return new WeightedValue<Integer>(key, mPreferenceMap.get(key).getPreferenceFraction());
                }).collect(toList()));

                return sleep(AITools.ChooseWeightedRandomFair(weights).GetValue());
            }
            else {
                Integer[] keys = mFeatureMap.keySet().stream().filter(i -> mFeatureMap.get(i).isComplete()).collect(toList()).toArray(new Integer[0]);

                int randomIndex = (int)(Math.random()*keys.length);
                return sleep(keys[randomIndex]);
            }

        }
        else
            return null;
    }

    FeatureModel selectedGoodModel(){
        if (mPreferenceMap.size() == 0)
            return null;

        ArrayList<WeightedValue<Integer>> prefs = new ArrayList<WeightedValue<Integer>>();

        for (Integer allocationIndex:mPreferenceMap.keySet().stream().filter( i->mFeatureMap.get(i).isComplete()).collect(toList())){
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

    /**
     * Causes the Group to go into sleep mode.  In this mode, the system reprocesses the most common
     * FeatureModels, starting from dreamFocusIndex, and releases the least commonly used FeatureModels
     * for future recycling
     * @param dreamFocusIndex
     * @return
     */
    public DreamSpec sleep(int dreamFocusIndex){
        MODE priorMode = mMode;
        mMode = MODE.SLEEPING;

        HashMap<Integer, FeatureValueMetadata> backup = copyPrefs(mPreferenceMap);

        boolean scaleDreamsByFeatureLength = true;

        DreamSpec dreamedOutput = new DreamSpec();

        int maxLength = 0;
        boolean debugP = mDebugEnabledP;
        ArrayList<FeatureModel> out = null;
        int numCycles = (int)(mFeatureMap.size() * mSleepCycleMultipler);

        for (int cycle = 0; cycle < numCycles;cycle++){
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





    private FeatureModel getAnotherFeature(){
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
                if (model == null && mSleepToFreeMemoryP){
                    if (mMemorymanagementListener != null){
                        mMemorymanagementListener.onStartMemoryManagement(mFeatureMap.size());
                    }

                    DreamSpec dreamResult = sleep();
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

                    for (Integer selectedIndex:prefs.keySet().stream().sorted(prefComparator).collect(toList())){
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



    FeatureMetaData getFeatureInternalMetaData(FeatureModel model){
        FeatureMetaData data = (FeatureMetaData)model.getMetaData();
        if (data == null){
            model.setMetaData(data = new FeatureMetaData());
        }
        return data;
    }

    public FeatureValueMetadata getFeatureValueMetadata(FeatureModel model){
        FeatureMetaData internalMeta = getFeatureInternalMetaData(model);
        return mPreferenceMap.get(internalMeta.getAllocationIndex());
    }

    public Group increaseFeatureValueFraction(int index, double fraction){
        FeatureValueMetadata meta = mPreferenceMap.get(Integer.valueOf(index));
        if (meta != null){
            meta.increaseValuePercent(fraction);
            return this;
        }

        return null;
    }

    public Group increaseFeatureValueFraction(FeatureModel model, double fraction){
        int index = getFeatureInternalMetaData(model).getAllocationIndex();
        FeatureValueMetadata meta = mPreferenceMap.get(Integer.valueOf(index));
        if (meta != null){
            meta.increaseValuePercent(fraction);
            return this;
        }

        return null;
    }

    public Group addFeature(FeatureModel model){
        return addFeature(model, _nextIndex);
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

    public Group importFeature(FeatureModel model, boolean sleepToFreeMemory){
        Optional<FeatureModel> available = mFeatureMap.keySet().stream().map((i)->mFeatureMap.get(i)).filter(f->f.getState()== FeatureModel.STATE.INITIAL).findAny();
        if (available.isPresent()){
            removeFeature(available.get());
            addFeature(model);
            return this;
        }
        else if (sleepToFreeMemory){
            sleep();
            return importFeature(model, false);
        }
        else
            return null;
    }


    public Group removeFeature(FeatureModel model){
        FeatureMetaData meta = getFeatureInternalMetaData(model);

        Integer index = meta.getAllocationIndex();
        mFeatureMap.remove(index);
        mPreferenceMap.remove(index);

        int i=0;
        for (;mFeatureMap.containsKey(Integer.valueOf(i)); i++){

        }
        _nextIndex = i;
        return this;
    }

    public Group removeFeature(Integer index){

        mFeatureMap.remove(index);
        mPreferenceMap.remove(index);

        int i=0;
        for (;mFeatureMap.containsKey(Integer.valueOf(i)); i++){

        }
        _nextIndex = i;
        return this;
    }


    private void processAllCompleteModels(Vector input){
        processAllCompleteModels(input, null);
    }

    private void processAllCompleteModels(Vector input, HashSet<FeatureModel> excluded){
        mLastProcessedFeatures = new ArrayList<>();
        mGroupHeap.clear();
        mFocusQueue.clear();

        for (Integer index: mFeatureMap.keySet()){
            FeatureModel model = mFeatureMap.get(index);
            if (model.isComplete() && (excluded == null || !excluded.contains(model))){
                model.processNextInput(input);
                if (!isSleeping()){
                    getFeatureInternalMetaData(model).updateUsageCount();
                }
                else if (model.isMatching()){
                    mFocusQueue.add(model);
                }
                mLastProcessedFeatures.add(model);
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

        if (mMode == MODE.EXTRAPOLATION){
            mUpdateCount++;
            if (mUpdateCount == mResetInterval){
                decayPreferenceMap();
                mUpdateCount = 0;
            }
        }
    }

    int minDecayUsageCount = 40;

    public Group setMinimumDecayUsageCount(int c){
        minDecayUsageCount = c;
        return this;
    }

    private void decayPreferenceMap(){
        for (Integer index:mPreferenceMap.keySet()){
            FeatureModel model = mFeatureMap.get(index);
            if (model.isComplete() && getFeatureInternalMetaData(model).getAllocationIndex() > minDecayUsageCount){
                mPreferenceMap.get(index).decay(DECAY_FRACTION);
            }

        }
    }



}
