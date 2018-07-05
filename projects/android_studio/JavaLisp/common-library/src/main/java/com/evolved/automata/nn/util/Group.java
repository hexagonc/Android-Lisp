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
import java.util.LinkedList;
import java.util.Map;
import java.util.Optional;
import java.util.PriorityQueue;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toList;

/**
 * Created by Evolved8 on 5/28/18.
 */

public class Group {
    public final double DECAY_FRACTION = 0.1;
    public static final String FOCUS_LABEL = "FOCUS";
    public static final String BUFFER_LABEL = "BUFFER";

    public enum MODE {
        LEARNING, EXTRAPOLATION, MIXED, SLEEPING
    }

    public static class FeatureValueMetadata {
        double _prefCount = 0;
        double _incidenceCount = 1;

        FeatureValueMetadata(){

        }

        public byte[] serializeBytes(){
            return GroupSerializer.get().serialize().add(Double.valueOf(_prefCount)).add(Double.valueOf(_incidenceCount)).build();
        }

        public static FeatureValueMetadata deserializeBytes(byte[] data){
            ArrayList values = GroupSerializer.get().deserialize(data);

            FeatureValueMetadata meta = new FeatureValueMetadata();
            meta._prefCount = (Double)values.get(0);
            meta._incidenceCount = (Double)values.get(1);
            return meta;
        }

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
            decreaseValuePercent(decayFraction);
            return this;
        }

        public FeatureValueMetadata increaseValuePercent(double fraction){
            _prefCount+=(_incidenceCount - _prefCount)*fraction;
            return this;
        }

        public FeatureValueMetadata decreaseValuePercent(double fraction){
            _prefCount*=fraction;
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

    public static class FeatureMetaData {
        public int _allocationIndex = 0;
        public int _updateCount = 0;
        public int _averageLearningTime = 0;
        public int _usageCount = 0;
        public long _learningTime = 0;
        public boolean _isInUseP = false;
        public Object _userObject = null;

        String _serializedData = null;

        StringSerializer _serializer = null;

        FeatureMetaData(){

        }

        public FeatureMetaData reset(){
            _isInUseP = false;
            _usageCount = 0;
            _averageLearningTime = 0;
            _updateCount = 0;
            _learningTime = 0;
            return this;
        }

        public byte[] serializeBytes(){
            GroupSerializer.Builder b = GroupSerializer.get().serialize()
                .add(Integer.valueOf(_allocationIndex))
                    .add(Integer.valueOf(_updateCount))
                    .add(Integer.valueOf(_averageLearningTime))
                    .add(Integer.valueOf(_usageCount))
                    .add(Long.valueOf(_learningTime))
                    .add(Boolean.valueOf(_isInUseP));

            if (_serializer != null && _userObject != null){
                b.add(_serializer.serialize(_userObject));
            }
            return b.build();
        }

        public static FeatureMetaData deserializeBytes(byte[] data){
            ArrayList values = GroupSerializer.get().deserialize(data);
            FeatureMetaData meta = new FeatureMetaData();
            meta._allocationIndex = (Integer)values.get(0);
            meta._updateCount = (Integer)values.get(1);
            meta._averageLearningTime = (Integer)values.get(2);
            meta._usageCount = (Integer)values.get(3);
            meta._learningTime = (Long)values.get(4);
            meta._isInUseP = (Boolean)values.get(5);

            if (values.size()>6){
                meta._serializedData = (String)values.get(6);
            }
            return meta;
        }

        public void setStringSerializer(StringSerializer serializer){
            _serializer = serializer;
            if (_serializedData != null){
                _userObject = serializer.deserialize(_serializedData);
                _serializedData = null;
            }
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

        public FeatureMetaData setCustomMetadata(Object o){
            _userObject = o;
            return this;
        }

        public Object getCustomMetadata(){
            return _userObject;
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

            if (SUPPRESS_COMPARATOR_SIDE_EFFECTS){
                updateIncidence(prior);
                updateIncidence(other);
            }


            if (priorValid && !otherValid)
            {
                if (SUPPRESS_COMPARATOR_SIDE_EFFECTS)
                    updatePreference(prior);
                return -1;
            }
            else if (otherValid && !priorValid){
                if (SUPPRESS_COMPARATOR_SIDE_EFFECTS)
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


            if (SUPPRESS_COMPARATOR_SIDE_EFFECTS){
                if (score == -1)
                    updatePreference(prior);
                else if (score == 1){
                    updatePreference(other);
                }
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



    boolean mEnableMemoryManagmentP = true;
    boolean mLimitBufferStateP = true;
    boolean mSleepToFreeMemoryP = true;
    boolean mSortByUsageP = false;
    boolean mDreamWithFractionP = true;
    boolean mDebugEnabledP = false;
    boolean mAllowUnlimitedFeatureImportP = false;

    int mUpdateCount = 0;
    int mResetInterval = 10;
    int MAX_BUFFER_MILLI = 30*1000; // 30 seconds
    int mMinimumUsageForRecycle = 10;
    int mAvgStepLearningMilli = 0;
    int mTemporalUpdateCount = 0;
    int _nextIndex = 0;

    MODE mMode = MODE.LEARNING;

    double mSleepCycleMultipler = 1;
    double mRecycleCutoffFraction = 0.2F;

    String mKey;

    HashMap<Integer, FeatureValueMetadata> mPreferenceMap = new HashMap<>();
    LinkedList<Integer> mRecycleQueue = null;
    Integer[] mIndexIterator;

    HashMap<Integer, FeatureModel> mFeatureMap;


    FeatureModel mFocusModel = null;  // Set from featuremap
    FeatureModel mBufferModel = null; // Set from featuremap

    ArrayList<FeatureModel> mLastProcessedFeatures = new ArrayList<FeatureModel>(); // Set from featuremap
    PriorityQueue<FeatureModel> mFocusQueue; // Set from featuremap
    PriorityQueue<FeatureModel> mGroupHeap; // Set from featuremap


    MemoryManagementListener mMemorymanagementListener = null; // set externally

    WorldModel.GroupType mType;

    LearningConfiguration mConfiguration;

    StringSerializer mUserSerializer = null;

    boolean SUPPRESS_COMPARATOR_SIDE_EFFECTS = false;

    public byte[] serializeBytes(){
        GroupSerializer.Builder b = GroupSerializer.get().serialize()
                .add(Boolean.valueOf(mEnableMemoryManagmentP))
                .add(Boolean.valueOf(mLimitBufferStateP))
                .add(Boolean.valueOf(mSleepToFreeMemoryP))
                .add(Boolean.valueOf(mSortByUsageP))
                .add(Boolean.valueOf(mDreamWithFractionP))
                .add(Boolean.valueOf(mDebugEnabledP))
                .add(Boolean.valueOf(mAllowUnlimitedFeatureImportP))

                .add(Integer.valueOf(mUpdateCount))
                .add(Integer.valueOf(mResetInterval))
                .add(Integer.valueOf(MAX_BUFFER_MILLI))
                .add(Integer.valueOf(mMinimumUsageForRecycle))
                .add(Integer.valueOf(mAvgStepLearningMilli))
                .add(Integer.valueOf(mTemporalUpdateCount))
                .add(Integer.valueOf(_nextIndex))
                .add(Integer.valueOf(mMode.ordinal()))

                .add(Double.valueOf(mSleepCycleMultipler))
                .add(Double.valueOf(mRecycleCutoffFraction))

                .add(mKey);

        // preference map
        int size = mPreferenceMap.size(), i;
        if (size > 0){
            b.add(Integer.valueOf(size));

            for (Map.Entry<Integer, FeatureValueMetadata> pair:mPreferenceMap.entrySet()){
                b.add(pair.getKey());
                b.add(pair.getValue());
            }
        }

        // recyclequeue
        size = -1;
        if (mRecycleQueue != null){
            size = mRecycleQueue.size();
            b.add(Integer.valueOf(size));
            mRecycleQueue.stream().forEach(index->b.add(index));
        }
        else
            b.add(Integer.valueOf(size));

        // index iterator
        size = -1;
        if (mIndexIterator == null)
        {
            size = mIndexIterator.length;
            b.add(Integer.valueOf(size));
            Arrays.stream(mIndexIterator).forEach(index->b.add(index));
        }
        else
            b.add(Integer.valueOf(size));


        // featuremap
        size = mFeatureMap.size();
        b.add(Integer.valueOf(size));
        i = 0;
        for (Map.Entry<Integer, FeatureModel> pair:mFeatureMap.entrySet()){
            b.add(pair.getKey());
            b.add(FeatureModel.class, pair.getValue());
        }

        // focus model index
        if (mFocusModel == null){
            b.add(Integer.valueOf(-1));
        }
        else
            b.add(Integer.valueOf(getFeatureInternalMetaData(mFocusModel).getAllocationIndex()));

        // buffer model index
        if (mBufferModel == null){
            b.add(Integer.valueOf(-1));
        }
        else
            b.add(Integer.valueOf(getFeatureInternalMetaData(mBufferModel).getAllocationIndex()));

        // last processed features (currently unused)
        size = mLastProcessedFeatures.size();

        b.add(Integer.valueOf(size));
        mLastProcessedFeatures.stream().forEach(f->b.add(Integer.valueOf(getFeatureInternalMetaData(f).getAllocationIndex())));

        // focus queue
        size = mFocusQueue.size();
        b.add(Integer.valueOf(size));

        if (size > 0){
            PriorityQueue<FeatureModel> copyQueue = new PriorityQueue<FeatureModel>(size, mPassiveValueComparator);
            while (mFocusQueue.size()>0){
                FeatureModel m = mFocusQueue.poll();
                copyQueue.add(m);
                b.add(Integer.valueOf(getFeatureInternalMetaData(m).getAllocationIndex()));
            }
            mFocusQueue = copyQueue;
        }

        // Group heap (only used for the preference side-effects of using mStandardValueComparator)
        size = mGroupHeap.size();
        b.add(Integer.valueOf(size));

        if (size > 0){
            SUPPRESS_COMPARATOR_SIDE_EFFECTS = true;
            PriorityQueue<FeatureModel> copyQueue = new PriorityQueue<FeatureModel>(size, mStandardValueComparator);
            while (mGroupHeap.size()>0){
                FeatureModel m = mGroupHeap.poll();
                copyQueue.add(m);
                b.add(Integer.valueOf(getFeatureInternalMetaData(m).getAllocationIndex()));
            }
            mGroupHeap = copyQueue;
            SUPPRESS_COMPARATOR_SIDE_EFFECTS = false;
        }


        return b.build();
    }

    public static Group deserializeBytes(byte[] data, WorldModel.GroupType type){
        ArrayList values = GroupSerializer.get().deserialize(data);
        Group g = new Group();
        g.mEnableMemoryManagmentP = (Boolean)values.get(0);
        g.mLimitBufferStateP = (Boolean)values.get(1);
        g.mSleepToFreeMemoryP = (Boolean)values.get(2);
        g.mSortByUsageP = (Boolean)values.get(3);
        g.mDreamWithFractionP = (Boolean)values.get(4);
        g.mDebugEnabledP = (Boolean)values.get(5);
        g.mAllowUnlimitedFeatureImportP = (Boolean)values.get(6);

        g.mUpdateCount = (Integer)values.get(7);
        g.mResetInterval = (Integer)values.get(8);
        g.MAX_BUFFER_MILLI = (Integer)values.get(9);
        g.mMinimumUsageForRecycle = (Integer)values.get(10);
        g.mAvgStepLearningMilli = (Integer)values.get(11);
        g.mTemporalUpdateCount = (Integer)values.get(12);
        g._nextIndex = (Integer)values.get(13);
        g.mMode = MODE.values()[(Integer)values.get(14)];

        g.mSleepCycleMultipler = (Double)values.get(15);
        g.mRecycleCutoffFraction = (Double)values.get(16);

        g.mKey = (String)values.get(17);

        // preference map
        int offset = 18;
        Integer size = (Integer)values.get(offset++);
        int i = 0;
        Integer key = null;

        if (size > 0){

            for (i = 0;i<2*size;i++){

                if (i % 2 == 0){
                    key = (Integer)values.get(offset);
                }
                else {
                    g.mPreferenceMap.put(key, (FeatureValueMetadata) values.get(offset));
                }
                offset++;
            }
        }

        // recyclequeue
        size = (Integer)values.get(offset++);
        if (size == -1){
            g.mRecycleQueue = null;
        }
        else {
            g.mRecycleQueue = new LinkedList<>();
            for (i = 0; i < size;i++){
                g.mRecycleQueue.add((Integer)values.get(offset));
                offset++;
            }
        }

        // index iterator
        size = (Integer)values.get(offset++);
        if (size == -1){
            g.mIndexIterator = null;
        }
        else {
            g.mIndexIterator = new Integer[size];
            for (i = 0; i < size;i++){
                g.mIndexIterator[i] = (Integer)values.get(offset);
                offset++;
            }
        }

        // Load from type
        g.mType = type;
        g.mConfiguration = type.getLearningConfig();
        g.mUserSerializer = type.getCustomDataStringSerializer();

        // featuremap
        size = (Integer)values.get(offset++);
        g.mFeatureMap = new HashMap<>();
        for (i = 0;i<size*2;i++){

            if (i % 2 == 0){
                key = (Integer)values.get(offset);
            }
            else {
                FeatureModel model = (FeatureModel)values.get(offset);
                model.setMetadataSerializer(
                                (Object fMeta)-> ((FeatureMetaData)fMeta).serializeBytes(),
                                (byte[] d)->FeatureMetaData.deserializeBytes(d));

                g.mFeatureMap.put(key, model);
            }
            offset++;
        }

        // focus model index
        Integer focusId = (Integer)values.get(offset++);
        if (focusId == -1){
            g.mFocusModel = null;
        }
        else {
            g.mFocusModel = g.mFeatureMap.get(focusId);
        }

        // buffer model index

        Integer bufferId = (Integer)values.get(offset++);
        if (bufferId != -1){
            g.mBufferModel = null;
        }
        else {
            g.mBufferModel = g.mFeatureMap.get(bufferId);
        }

        // last processed features (currently unused)
        g.mLastProcessedFeatures = new ArrayList<>();
        size = (Integer)values.get(offset++);

        for (i = 0;i<size;i++){
            g.mLastProcessedFeatures.add(g.mFeatureMap.get((Integer)values.get(offset)));
            offset++;
        }

        // focus queue
        size = (Integer)values.get(offset++);
        for (i = 0;i<size;i++){
            g.mFocusQueue.add(g.mFeatureMap.get((Integer)values.get(offset)));
            offset++;
        }

        // Group heap (only used for the preference side-effects)
        size = (Integer)values.get(offset++);
        g.SUPPRESS_COMPARATOR_SIDE_EFFECTS = true;
        for (i = 0;i<size;i++){
            g.mGroupHeap.add(g.mFeatureMap.get((Integer)values.get(offset)));
            offset++;
        }
        g.SUPPRESS_COMPARATOR_SIDE_EFFECTS = false;


        return g;
    }

    Group(){
        mFeatureMap = new HashMap<>();
        mGroupHeap = new PriorityQueue<>(10, mStandardValueComparator);
        mFocusQueue = new PriorityQueue<>(10, mPassiveValueComparator);
        mIndexIterator = new Integer[0];
    }

    public Group(String key, WorldModel.GroupType type){
        mConfiguration = type.getLearningConfig();
        mType = type;
        mFeatureMap = new HashMap<>();
        mIndexIterator = new Integer[0];

        mKey = key;
        mGroupHeap = new PriorityQueue<>(10, mStandardValueComparator);
        mFocusQueue = new PriorityQueue<>(10, mPassiveValueComparator);
    }

    public WorldModel.GroupType getType(){
        return mType;
    }

    public LearningConfiguration getLearningConfig(){
        return mConfiguration;
    }



    public FeatureMetaData createMetadata(int index){
        FeatureMetaData mdata = new FeatureMetaData();
        mdata._allocationIndex = index;
        return mdata;
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

    public Group setAllowUnlimitedFeatureImports(boolean unlimited){
        mAllowUnlimitedFeatureImportP = unlimited;
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

    public Group setDecayInterval(int interval){
        mResetInterval = interval;
        return this;
    }

    public Group resetAll(){
        return resetAll(false);
    }
    /**
     * Asserts that a boundary has occurred.  Every subsequent input is causally independent
     * of the prior ones
     * @return
     */
    public Group resetAll(boolean skipFocusP){

        if (!skipFocusP) {
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




        for (Integer index: mFeatureMap.keySet().stream().filter(i->(mFeatureMap.get(i).isComplete())).collect(toList()) ){
            FeatureModel model = mFeatureMap.get(index);
            if (!skipFocusP || (model != mFocusModel && model != mBufferModel)) {
                model.setLabel("");
                model.resetRecognition();
            }


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
            return MODE.EXTRAPOLATION;
        }

        if (mBufferModel == null){
            mBufferModel = getAnotherFeature();
        }

        mFocusModel.setLabel(FOCUS_LABEL);
        int featureBufferSize = mType.getFeatureBufferSize();
        int minimumBufferOverlap = mType.getMinimumBufferOverlap();
        long startTime, duration;

        if (!mFocusModel.isMatching()){
            if (mFocusModel.getFeatureLength() == featureBufferSize + minimumBufferOverlap)
            {
                mFocusModel.setConfiguration(mConfiguration);
            }

            startTime = System.currentTimeMillis();
            //System.out.println("Extending model");
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
            //System.out.println("Recognition skip");
        }

        if (mBufferModel != null){
            mBufferModel.setLabel(BUFFER_LABEL);
            if (mBufferModel.getFeatureLength() == featureBufferSize){
                mBufferModel.shiftForward();
            }

            //System.out.println("Building buffer");
            mBufferModel.processNextInput(input);

            if (mBufferModel.isComplete()){
                mBufferModel.setLabel("");
                mBufferModel = null;
                //System.out.println("Buffer exhausted");
            }
            else if (mFocusModel.isFinished()){
                mFocusModel.setLabel("");
                mFocusModel = mBufferModel;
                mFocusModel.setLabel(FOCUS_LABEL);
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

                WeightedValue<Integer> selected = AITools.ChooseWeightedRandomFair(weights);
                return sleep(selected.GetValue());
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

                if (selected == null && prefs.size()>0){
                    dreamFocusIndex = prefs.get((int)(Math.random()*prefs.size())).GetValue();
                }
                else
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


    /**
     * Enhances related features without removing features
     * @param dreamFocusIndex
     * @return
     */
    public DreamSpec dayDream(int dreamFocusIndex, int numCycles){

        boolean scaleDreamsByFeatureLength = true;

        DreamSpec dreamedOutput = new DreamSpec();

        int maxLength = 0;
        boolean debugP = mDebugEnabledP;
        ArrayList<FeatureModel> out = null;

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

        dreamedOutput.setDreamPrefs(mPreferenceMap);

        return dreamedOutput;
    }



    private FeatureModel getAnotherFeature(){

        FeatureModel model = null, selected = null;
        model = mType.requestFeature(this);

        FeatureMetaData meta = null;
        if (model == null){
            if (mRecycleQueue!= null && mRecycleQueue.size() > 0){
                model = mFeatureMap.get(mRecycleQueue.removeFirst());
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

                    PriorityQueue<Integer> recycleQueue = new PriorityQueue<>(10, selectedComparator);
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

                    if (mMemorymanagementListener != null){
                        mMemorymanagementListener.onFinishedMemoryManagement(managementResults);
                    }

                    if (recycleQueue.size()>0)
                    {
                        model = mFeatureMap.get(recycleQueue.poll());
                        while (recycleQueue.size()>0)
                        {
                            if (mRecycleQueue == null)
                                mRecycleQueue = new LinkedList<>();
                            mRecycleQueue.add(recycleQueue.poll());
                        }
                    }
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
            meta.setStringSerializer(mUserSerializer);
            meta.reset();
            meta.setInUse(true);
            model.setLabel("");
            model.setMetadataSerializer(
                    (Object fMeta)-> ((FeatureMetaData)fMeta).serializeBytes(),
                    (byte[] data)->FeatureMetaData.deserializeBytes(data));

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
            data.setStringSerializer(mUserSerializer);
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

    public Group decreaseFeatureValueFraction(FeatureModel model, double fraction){
        int index = getFeatureInternalMetaData(model).getAllocationIndex();
        FeatureValueMetadata meta = mPreferenceMap.get(Integer.valueOf(index));
        if (meta != null){
            meta.decreaseValuePercent(fraction);
            return this;
        }

        return null;
    }

    public Group setCustomMetadata(FeatureModel model, Object data){
        getFeatureInternalMetaData(model).setCustomMetadata(data);

        return this;
    }

    public Object getCustomMetadata(FeatureModel model){
        return getFeatureInternalMetaData(model).getCustomMetadata();

    }

    public Group addFeature(FeatureModel model){
        return addFeature(model, _nextIndex);
    }

    public Group addFeature(FeatureModel model, int index){
        FeatureMetaData meta = getFeatureInternalMetaData(model);
        meta.setAllocationIndex(index);
        meta.setStringSerializer(mUserSerializer);

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
        }
        else if (!mAllowUnlimitedFeatureImportP && mType.canAllocateAnotherFeature(mType.getName())) {

            if (sleepToFreeMemory){
                sleep();
                return importFeature(model, false);
            }
            else
                return null;
        }
        addFeature(model);
        return this;

    }

    public FeatureModel getFocusModel(){
        FeatureModel out = null;
        if (mFocusModel != null){
            out = mFocusModel;
        }
        else if (mFocusQueue != null && mFocusQueue.size()>0){
            out = mFocusQueue.peek();
        }
        return out;
    }

    public Group removeFeature(FeatureModel model){
        FeatureMetaData meta = getFeatureInternalMetaData(model);

        if (mFocusModel == model){
            if (mBufferModel != null){
                mFocusModel.setLabel("");
                mFocusModel = mBufferModel;
                mFocusModel.setLabel(FOCUS_LABEL);

                mBufferModel = null;
            }
            else
            {
                mFocusModel = null;
            }
        }

        if (mFocusQueue != null && mFocusQueue.size()>0 && mFocusQueue.peek() == model){
            mFocusQueue.poll();
        }

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

        return removeFeature(mFeatureMap.get(index));
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

                if (model.isMatching()){
                    mFocusQueue.add(model);
                }
                mLastProcessedFeatures.add(model);
                mGroupHeap.add(model);
            }
        }

        if (!isSleeping()){
            if (mFocusQueue.size()>0 && (mFocusModel==null || mFocusModel.isNonMatching())){
                if (mFocusModel!=null && mFocusModel.isNonMatching())
                {
                    mFocusModel.setLabel("");
                }
                mFocusModel = mFocusQueue.poll();
                mFocusModel.setLabel(FOCUS_LABEL);
            }
            else if (mFocusModel != null && mFocusModel.isNonMatching()){
                //System.out.println("Discarding prior focus");
                if (mBufferModel != null){
                    //System.out.println("Using buffer as new focus");
                    mFocusModel.setLabel("");
                    mFocusModel = mBufferModel;
                    mFocusModel.setLabel(FOCUS_LABEL);
                    mBufferModel = null;
                }
                else {
                    mFocusModel.setLabel("");
                    mFocusModel = null;
                }
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
