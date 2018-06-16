package com.evolved.automata.nn.util;

import com.evolved.automata.lisp.nn.LSTMNetworkProxy;
import com.evolved.automata.nn.LSTMNetwork;
import com.evolved.automata.nn.NNTools;
import com.evolved.automata.nn.Vector;

import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;

import static com.evolved.automata.nn.FastLSTMNetwork.roundToInt;

/**
 * Created by Evolved8 on 6/2/18.
 */

public class FeatureModel {

    public enum STATE {
        INITIAL,
        BUILDING,
        MATCHING,
        NON_MATCHING
    }

    public enum HISTORY_STATE {
        IMPROVING, HOLDING_STEADY, WORSENING, MATCHING
    }

    public static class Similarity {
        public double _numMisses = 0;
        public double _numMatches = 0;
        public float[] _mask;
        public boolean _isMatch;
        public double _matchFraction = 0;

        private Similarity(int matches, float[] mask){
            _numMatches = matches;
            _numMisses = mask.length - matches;
            _mask = mask;
            _isMatch = _numMisses == 0;
            _matchFraction = _numMatches/mask.length;
        }

        public static Similarity getSimilarity(float[] v1, float[] v2){
            float[] matches = new float[v1.length];
            int matchCount = 0;
            for (int i= 0;i<v1.length;i++){
                if (NNTools.roundToInt(v1[i]) == NNTools.roundToInt(v2[i])){
                    matches[i] = 1;
                    matchCount++;
                }
                else {
                    matches[i] = 0;
                }
            }
            return new Similarity(matchCount, matches);
        }
    }

    public static class SimilaryHistory {
        double _equalityFraction = 0;
        Similarity[] _history;
        HISTORY_STATE _state = HISTORY_STATE.HOLDING_STEADY;
        double _trend = 0;
        int _index = 0;
        boolean allowStateWithoutFullHistoryP= true;
        public SimilaryHistory(int size, double equalityFraction){
            _equalityFraction = equalityFraction;
            _history = new  Similarity[size];
        }

        public SimilaryHistory clear(){
            for (int i =0;i<_history.length;i++)
                _history[i] = null;
            _trend = 0;
            _index = 0;
            return this;
        }

        public SimilaryHistory(LearningConfiguration config){
            this(config.getHistoryLength(), config.getMatchEqualityError());
        }

        public double getTrend(){
            return _trend;
        }

        public HISTORY_STATE getState(){
            return _state;
        }

        public HISTORY_STATE updateState(Similarity sim){
            double sum = 0, avg;
            boolean matching = true;

            int i = 0;
            // _index is the position to insert <sim> after the prior sum has been computed

            for (;i < _history.length && _history[i] != null;i++){
                sum+=_history[i]._matchFraction;
                if (i > 0)
                    matching = matching && _history[i]._isMatch;
            }

            if (i == _history.length){
                sum = sum - _history[0]._matchFraction + sim._matchFraction;
                _history[_history.length - 1] = sim;
                avg = sum/i;
                matching = matching && sim._isMatch;
                _index = i - 1;
            }
            else {
                _history[i] = sim;
                avg = sum/(i + 1);
                matching = matching && sim._isMatch && _history[0]._isMatch;
                _index = i;
            }

            _trend = sim._matchFraction - avg;

            if (matching){
                _state = HISTORY_STATE.MATCHING;
            }
            else if (Math.abs(_trend) <= _equalityFraction){
                _state = HISTORY_STATE.HOLDING_STEADY;
            }
            else if (_trend > 0){
                _state = HISTORY_STATE.IMPROVING;
            }
            else
                _state = HISTORY_STATE.WORSENING;
            return _state;
        }

        public Similarity getCurrentSimilarity(){
            return _history[_index];
        }
    }

    SimilaryHistory mSimilarityHistory;
    IncrementalUpdateSpec mCurrentRange;
    LearningConfiguration mConfiguration;
    LSTMNetworkProxy mNetwork;
    STATE mState;
    String mLabel = "";

    public static final String STASHED_STATE = "STASH";
    HashMap<String, LSTMNetworkProxy.NodeState> mStashedState = new HashMap<String, LSTMNetworkProxy.NodeState>();

    int mMatchCount = 0;
    int mFailureCount = 0;
    int MAX_FAILURE_COUNT = 1;
    boolean mExtrapPredictionP = true;
    Object mMetaData;
    boolean mMatchedLast = false;

    public Object getMetaData(){
        return mMetaData;
    }

    public FeatureModel setMetaData(Object data){
        mMetaData = data;
        return this;
    }

    public boolean matchedLastInput(){
        return mMatchedLast;
    }

    public FeatureModel markCurrentState(String key){
        if (mNetwork != null){

            mStashedState.put(key, mNetwork.getCurrentNodeState());
        }
        return this;
    }

    public String getLabel(){
        return mLabel;
    }

    public FeatureModel setLabel(String s){
        mLabel = s;
        return this;
    }

    public boolean restoreState(String key){
        LSTMNetworkProxy.NodeState state = mStashedState.get(key);
        if (state != null){
            mNetwork.setNodeState(state);
            return true;
        }
        return false;
    }

    public boolean hasMarkedState(String key){
        return mStashedState.containsKey(key);
    }


    public FeatureModel(LSTMNetworkProxy network, LearningConfiguration configuration){
        mConfiguration = configuration;
        mNetwork = network;
        mState = STATE.INITIAL;

        mCurrentRange = null;
        mSimilarityHistory = new SimilaryHistory(configuration);
    }

    public FeatureModel(int numInputOutputNodes, int numMemoryCells, LearningConfiguration configuration){
        this(LSTMNetworkProxy.makeStandardBinarySequenceNetwork(numInputOutputNodes, numMemoryCells, 0), configuration);
    }

//    public FeatureModel copyInto(FeatureModel other){
//        if (mCurrentRange != null){
//            if (other.mCurrentRange == null){
//                other.m
//            }
//        }
//
//    }

    public FeatureModel setConfiguration(LearningConfiguration config){
        mConfiguration = config;
        return this;
    }

    public LearningConfiguration getConfiguration(){
        return mConfiguration;
    }

    public STATE tryLearnNextData(float[] vectorData){
        return tryLearnNextData(new Vector(vectorData));
    }

    public boolean shiftForward(){
        if (mCurrentRange != null){
            return mCurrentRange.shiftForward();
        }
        else
            return false;
    }

    public FeatureModel resetRecognition(){
        if (isComplete()){
            mCurrentRange.setInitialState();
            mSimilarityHistory.clear();
            mState = STATE.NON_MATCHING;
        }
        mMatchedLast = false;
        return this;
    }

    public Vector getPredictedOutput(){
        return new Vector(NNTools.roundToInt(mNetwork.getOutputVaues()));
    }

    public String displayExtrapolatedFeature(){
        ArrayList<Vector> results = extrapFeature();
        StringBuilder s = new StringBuilder();
        for (Vector v:results){
            if (s.length() > 0)
                s.append(", ");
            s.append(mConfiguration.getInputString(v.rawFloat()));
        }
        return s.toString();
    }

    public Double getDistanceToFinalState(){
        if (mCurrentRange != null) {
            return mCurrentRange.getDistanceToEnd();
        }
        else
            return null;
    }

    public SimilaryHistory getMatchHistory(){
        return mSimilarityHistory;
    }

    public STATE processNextInput(Vector input){
        float[] rawInput = input.rawFloat();
        if (!isComplete()) {
            return tryLearnNextData(input);
        }
        else {
            float[] predictedOutput = mNetwork.getOutputVaues();
            mSimilarityHistory.updateState(Similarity.getSimilarity(predictedOutput, rawInput));
            boolean matchedPrediction = mMatchedLast = mSimilarityHistory.getCurrentSimilarity()._isMatch;
            switch (mState) {
                case MATCHING:
                {
                    if (matchedPrediction) {
                        mFailureCount = 0;
                        mMatchCount++;

                    }
                    else {
                        if (mFailureCount >= MAX_FAILURE_COUNT){
                            mFailureCount = 0;
                            mState = STATE.NON_MATCHING;
                            break;
                        }
                        else {
                            mFailureCount++;
                        }
                    }
                    if (mExtrapPredictionP) {
                        extrapNext();
                    }
                    else
                        extrapNext(rawInput);
                }
                break;
                case NON_MATCHING:
                {
                    if (matchedPrediction) {
                        mState = STATE.MATCHING;
                        extrapNext();
                        mMatchCount++;
                    }
                    else {
                        extrapNext(rawInput);
                    }
                }
                break;
            }
        }
        return mState;
    }

    private void extrapNext(){
        // Not using NNTools.roundToInt for minor efficiency gain
        float[] rawOutput = mNetwork.getOutputVaues();
        float[] out = new float[rawOutput.length];
        for (int i = 0;i<rawOutput.length;i++){
            out[i] = roundToInt(rawOutput[i]);
        }
        extrapNext(out);
    }


    private void extrapNext(float[] input){
        mNetwork.executeForwardPass(input);
    }


    public STATE tryLearnNextData(Vector vectorData){
        ArrayList<Pair<Vector, Vector>> inputOutputSpec = null;
        if (isComplete())
            return mState;
        if (mState == STATE.INITIAL){
            Pair<Vector, Vector> point = Pair.of(vectorData, vectorData);

            LearningConfiguration initialConfig = new LearningConfiguration();
            initialConfig.setDebugLevel(mConfiguration.getDebugLevel());
            initialConfig.set(LearningConfiguration.KEY.ANNEALING_FRACTION, Double.valueOf(0.1F));
            initialConfig.set(LearningConfiguration.KEY.BEST_SOLUTION_BONUS_MILLI, Integer.valueOf(1000));
            initialConfig.set(LearningConfiguration.KEY.NUM_SOLUTION_BUFFER, Integer.valueOf(5));
            initialConfig.set(LearningConfiguration.KEY.INITIAL_RANDOM_FRACTION, Float.valueOf(0.5F));

            inputOutputSpec = new ArrayList<Pair<Vector, Vector>>();
            inputOutputSpec.add(point);
            IncrementalUpdateSpec spec = simpleLearnSequence(mNetwork, inputOutputSpec, initialConfig);

            if (spec.successCriteriaSatisfied()){
                mState = STATE.BUILDING;
                mCurrentRange = spec;
                mNetwork = mCurrentRange.getNetwork();
                mCurrentRange.setLength(1);
            }
        }
        else {
            ArrayList<Vector> prior = mCurrentRange.extrapolateRange(true);
            prior.add(vectorData);
            inputOutputSpec =  getInputOututSpec(prior);

            IncrementalUpdateSpec spec = simpleLearnSequence(LSTMNetworkProxy.duplicate(mNetwork), inputOutputSpec, mConfiguration);

            if (spec.successCriteriaSatisfied()){
                mCurrentRange = spec;
                mNetwork = mCurrentRange.getNetwork();
                mCurrentRange.setLength(prior.size()-1);
            }
            else {
                mState = STATE.MATCHING;
            }
        }
        return mState;
    }

    public FeatureModel forceInitialState(){
        mState = STATE.INITIAL;
        if (mCurrentRange != null)
            mCurrentRange.setLength(0);
        mSimilarityHistory.clear();
        return this;
    }

    public FeatureModel forceComplete(){
        mState = STATE.NON_MATCHING;
        return this;
    }

    public FeatureModel forceBuilding(){
        if (isComplete())
        {
            mState = STATE.BUILDING;
            mSimilarityHistory.clear();
        }
        return this;
    }

    public int getFeatureLength(){
        if (mState != STATE.INITIAL)
            return mCurrentRange.getLength();
        else
            return 0;
    }

    public ArrayList<Vector> extrapFeature(){
        if (mCurrentRange != null){
            return mCurrentRange.extrapolateRange();
        }
        return null;
    }

    @Override
    public String toString(){
        StringBuilder s = new StringBuilder(getLabel()).append("[");
        s.append(mState.name()).append("] <").append(getFeatureLength()).append("> ");
        if (mState == STATE.MATCHING)
            s.append("|").append(String.format("%.4f", getDistanceToFinalState())).append("|");
        else if (mState == STATE.NON_MATCHING){
            s.append("|").append(String.format("%.4f", mSimilarityHistory.getTrend())).append("|");
        }

        return s.toString();
    }

    public boolean isComplete(){
        return mState == STATE.MATCHING || mState == STATE.NON_MATCHING;
    }

    public boolean isBuilding(){
        return mState == STATE.BUILDING;
    }

    public boolean isMatching(){
        return mState == STATE.MATCHING;
    }

    public boolean isNonMatching(){
        return mState == STATE.NON_MATCHING;
    }

    public boolean isFinished(){
        return mState == STATE.NON_MATCHING;
    }

    public STATE getState(){
        return mState;
    }

    public String dateParts(){
        return dateParts(System.currentTimeMillis());
    }

    public String dateParts(long time){
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeInMillis(time);
        int year = calendar.get(Calendar.YEAR);
        int month = calendar.get(Calendar.MONTH);
        int dayOfMonth = calendar.get(Calendar.DAY_OF_MONTH);
        int dayOfYear = calendar.get(Calendar.DAY_OF_YEAR);
        int hourOfDay = calendar.get(Calendar.HOUR_OF_DAY);
        int hourAMPM = calendar.get(Calendar.HOUR);
        int am_pm = calendar.get(Calendar.AM_PM);
        String am_pm_label;
        if (am_pm == Calendar.AM)
            am_pm_label = "AM";
        else
            am_pm_label = "PM";
        int minute = calendar.get(Calendar.MINUTE);
        int second = calendar.get(Calendar.SECOND);
        int millisecond = calendar.get(Calendar.MILLISECOND);

        return String.format("%d/%d/%d:%d:%d:%d", year, month, dayOfMonth, hourOfDay, minute, second, millisecond);
    }

    public void debug(String tag, String text) {
        if (!mConfiguration.isDebugTagAllowed(tag, 1)){
            return;
        }
        String log = String.format("%s [%s] - %s", dateParts(), tag, text);
        System.out.println(log);
    }

    public IncrementalUpdateSpec simpleLearnSequence(LSTMNetworkProxy network, ArrayList<Pair<Vector, Vector>> inputOutputSpec, LearningConfiguration configuration){
        String message = null;
        long startTime = System.currentTimeMillis();
        long stopTime = 0;
        Integer maxDuration = null;


        if (configuration.initialRandomFraction() != null)
            network.randomizeNetworkWeights(configuration.initialRandomFraction());

        double randomFraction = 1;
        double minRandomFraction = 0.05;
        double maxRandomFraction = 1;

        double convergenceThreshold = 0.00001;
        double successFraction = 0;
        double previousSequenceError = 0;
        boolean finishedP = false;
        int i = 0;
        // TODO: initialize this from prior steps provided in configuration
        int baseImprovementIndex = 0;
        int highestSegmentMatchCount = 0;
        int segmentMatchCount = 0;
        int numInputOutputPairs = inputOutputSpec.size();
        int newSolutionFailureCount = 0;

        int maxFailureCount = 0;
        boolean isFirstPair = true;
        boolean isLastPair = true;
        boolean firstPairMatchedP = false;
        boolean lastPairMatchedP = false;
        boolean allSegmentsMatchedP = false;
        boolean roundErrorsP = false;
        boolean hasPreviousSequenceError = false;
        boolean isValidP = false;
        boolean isSuccessCriteriaSatisfiedP = false;

        // TODO: initialize this from configuration
        int solutionIndex = 0;
        int improvementIndex = 0;
        int resetWidth = 50;
        if (configuration.getFailureIterations() != null){
            resetWidth = configuration.getFailureIterations();
        }

        Integer maxIterations = configuration.getMaxIterations();

        // TODO: make this part of configuration
        boolean chooseRandomlyAmongstViableSolutionsP = false;
        boolean currentNetworkIsBestP = false;
        Integer bestSolutionBufferSize = configuration.getBestSolutionBufferSize();
        if (bestSolutionBufferSize == null)
            bestSolutionBufferSize = 1;

        LSTMNetworkProxy currentNetwork = network;
        ArrayList<IncrementalUpdateSpec> bestNetworks = new ArrayList<IncrementalUpdateSpec>();
        ArrayList<Integer> annealingCountList = new ArrayList<Integer>();

        IncrementalUpdateSpec bestUpdateSpec = new IncrementalUpdateSpec(false, false, null, false, null, null, 0), lastValidUpdateSpec = null;
        double lastValidMatchFraction = 0;
        if (configuration.hasMaxDuration()) {
            maxDuration = configuration.getMaxDurationMilli();
            stopTime = startTime + maxDuration;
        }

        while (!finishedP &&
                (!configuration.hasMaxDuration() || (System.currentTimeMillis() < stopTime)) &&
                (!configuration.hasMaxIterations() || i < maxIterations))
        {

            Double sequenceError = null;
            Double pairError = null;
            allSegmentsMatchedP = true;
            lastPairMatchedP = false;
            LSTMNetworkProxy.NodeState initialState = null, temp;
            LSTMNetworkProxy.NodeState finalState = null;
            segmentMatchCount = 0;
            currentNetwork.resetNetworkToInitialState();

            // Process all segments of the input sequence
            int j = 0;
            for (Pair<Vector, Vector> inputOutputPair:inputOutputSpec) {
                float[] input = inputOutputPair.getLeft().rawFloat();
                float[] expectedOutput = inputOutputPair.getRight().rawFloat();
                isFirstPair = j == 0;
                isLastPair = j == numInputOutputPairs - 1;

                currentNetwork.executeForwardPass(input);

                if (isFirstPair) {
                    initialState =  currentNetwork.getCurrentNodeState();
                }

                if (isLastPair) {
                    finalState =  currentNetwork.getCurrentNodeState();
                }

                pairError = Double.valueOf(currentNetwork.getOutputError(expectedOutput, false, false, true));

                debug("FORWARD-PASS", "Errors: " + pairError);
                if (sequenceError == null) {
                    sequenceError = pairError;
                }
                else {
                    sequenceError = Math.max(sequenceError.doubleValue(), pairError.doubleValue());
                }

                float[] predictedOutput = currentNetwork.getOutputVaues();

                if (roundedEquals(predictedOutput, expectedOutput)) {
                    segmentMatchCount++;
                    if (isFirstPair) {
                        firstPairMatchedP = true;
                    }

                    if (isLastPair) {
                        lastPairMatchedP = true;
                    }

                }
                else {
                    allSegmentsMatchedP = false;
                }
                j++;
            }

            // Now assess the current lstm's performance
            if (allSegmentsMatchedP) {
                long endTime = System.currentTimeMillis();
                int seconds = (int)((endTime - startTime)/1000);
                message = ((configuration.requiresLastPairToMatch() && lastPairMatchedP)?"Done satisfying last pair requirements after ":"Done after") +
                        seconds +
                        " seconds and " +
                        i +
                        " steps " +
                        ((lastPairMatchedP)?lastValidMatchFraction:successFraction);

                debug("FINISHED", message);
                return new IncrementalUpdateSpec(true, true, currentNetwork, true, initialState, finalState, 1);
            }

            if ((currentNetworkIsBestP = highestSegmentMatchCount < segmentMatchCount) ||
                    (bestSolutionBufferSize > bestNetworks.size() && firstPairMatchedP && highestSegmentMatchCount == segmentMatchCount)) {

                // Handle case where current network is better than any previous
                if (currentNetworkIsBestP) {
                    debug("NEW-SOLUTION", "Match count: " + segmentMatchCount);
                    bestNetworks = new ArrayList<>();
                    highestSegmentMatchCount = segmentMatchCount;
                    successFraction = 1.0*segmentMatchCount/numInputOutputPairs;

                    boolean hasMaxIterationBonus = configuration.hasBestSolutionBonusIterations() && configuration.hasMaxIterations();
                    boolean hasMaxDurationBonus = configuration.hasBestSolutionBonusMilli() && configuration.hasMaxDuration();

                    boolean satisfiesLastPairCriteriaP = (lastPairMatchedP && configuration.requiresLastPairToMatch()) || !configuration.requiresLastPairToMatch();

                    if (satisfiesLastPairCriteriaP &&
                            !configuration.requiresCompleteMatchP() &&
                            configuration.earlyStopSuccessFraction() <= successFraction) {
                        if (hasMaxIterationBonus || hasMaxDurationBonus) {
                            if (hasMaxIterationBonus) {
                                maxIterations = i + maxIterations;
                                System.out.println("Extra iteration bonus: " + maxIterations);
                            }

                            if (hasMaxDurationBonus) {
                                stopTime = System.currentTimeMillis() + configuration.getBestSolutionBonusMilli();
                                System.out.println("Extra time bonus: " + dateParts(stopTime));
                            }
                        }
                        else {
                            finishedP = true;
                            debug("", "Early stop");
                        }

                    }
                    else {
                        if (hasMaxIterationBonus) {
                            maxIterations = i + maxIterations;
                            System.out.println("Extra iteration bonus: " + maxIterations);
                        }

                        if (hasMaxDurationBonus) {
                            stopTime = System.currentTimeMillis() + configuration.getBestSolutionBonusMilli();
                            System.out.println("Extra time bonus: " + dateParts(stopTime));
                        }
                    }
                }

                //

                debug("ANOTHER-SOLUTION", "Found another solution after: " + (i - improvementIndex) + " steps");
                newSolutionFailureCount = 0;
                improvementIndex = i;

                // ......................
                // Booleans
                isValidP = (lastPairMatchedP && configuration.requiresLastPairToMatch()) || bestNetworks.size() > 0;
                configuration.setResultIsValid(isValidP);

                isSuccessCriteriaSatisfiedP = isValidP &&
                        (allSegmentsMatchedP ||
                                (!configuration.requiresCompleteMatchP() &&
                                        (successFraction >= configuration.earlyStopSuccessFraction()) &&
                                        (!configuration.requiresLastPairToMatch() || lastPairMatchedP && configuration.requiresLastPairToMatch())));
                // +++++++++++++++++++++


                bestUpdateSpec = new IncrementalUpdateSpec(isValidP, isSuccessCriteriaSatisfiedP, currentNetwork, true, initialState, finalState, (float)successFraction);
                bestNetworks.add(bestUpdateSpec);
                annealingCountList.add(0);


                if (lastPairMatchedP && configuration.requiresLastPairToMatch()) {
                    debug("LAST-MATCH", "Found network satisfying last segment matching at " + successFraction);
                    lastValidUpdateSpec = bestUpdateSpec;
                    lastValidMatchFraction = successFraction;
                }
            }

            // Weight adaptation

            if (!finishedP) {
                if (hasPreviousSequenceError) {
                    // Not sure what roundErrorsP is for
                    boolean networkHasConverged = roundErrorsP || Math.abs((sequenceError - previousSequenceError)/sequenceError) < convergenceThreshold;
                    boolean tooLongSinceLastImprovement = ((i - improvementIndex) > resetWidth) && (i - baseImprovementIndex) > resetWidth;
                    if (networkHasConverged || tooLongSinceLastImprovement) {
                        int numBestSolutions = bestNetworks.size();
                        // Select one of the set of good solutions and use this
                        if (numBestSolutions > 0) {
                            int selectedIndex;
                            if (chooseRandomlyAmongstViableSolutionsP) {
                                selectedIndex = (int)(numBestSolutions * Math.random());
                            }
                            else {
                                selectedIndex = solutionIndex;
                            }
                            currentNetwork = LSTMNetworkProxy.duplicate(bestNetworks.get(selectedIndex).getNetwork());

                            newSolutionFailureCount = annealingCountList.get(selectedIndex);

                            annealingCountList.set(selectedIndex, newSolutionFailureCount + 1);

                            if (configuration.hasAnnealingFraction()) {
                                randomFraction = configuration.annealingFraction();
                            }
                            else {
                                // TODO: Fix this.  Doesn't work.  Is supposed to decrease with a longer .
                                // This else-clause is not supposed to decrease as newSolutionFailureCount increases
                                randomFraction = Math.min(maxRandomFraction, (minRandomFraction + (newSolutionFailureCount * (maxRandomFraction - minRandomFraction)/maxRandomFraction)));
                            }

                        }
                        else {
                            if (configuration.hasAnnealingFraction()) {
                                randomFraction = configuration.annealingFraction();
                            }
                            else
                                randomFraction = 1;
                        }

                        currentNetwork.randomizeNetworkWeights((float)randomFraction);

                        hasPreviousSequenceError = false;
                        newSolutionFailureCount++;
                        baseImprovementIndex = i;
                    }
                    else {
                        currentNetwork.updateWeights(LSTMNetwork.WeightUpdateType.RPROP);
                    }
                }
                else {
                    hasPreviousSequenceError = true;
                    currentNetwork.updateWeights(LSTMNetwork.WeightUpdateType.RPROP);
                }
            }

            previousSequenceError = sequenceError;
            i++;
        }

        long endTime = System.currentTimeMillis();
        int seconds = (int)((endTime - startTime)/1000);

        message = ((configuration.requiresLastPairToMatch() && lastPairMatchedP)?"Done satisfying last pair requirements after ":"Done after") +
                seconds +
                " seconds and " +
                i +
                " steps " +
                ((lastPairMatchedP)?lastValidMatchFraction:successFraction);

        debug("FINISHED", message);

        configuration.wasSuccessCriteriaSatisfied(isSuccessCriteriaSatisfiedP);

        int selectedIndex;
        if (bestNetworks.size() == 0){
            return bestUpdateSpec;
        }

        if (chooseRandomlyAmongstViableSolutionsP) {
            selectedIndex = (int)(bestNetworks.size() * Math.random());
        }
        else {
            selectedIndex = solutionIndex;
        }

        return bestNetworks.get(selectedIndex);
    }

    public static boolean roundedEquals(float[] first, float[] second) {
        if (first.length != second.length)
            return false;
        for (int i = 0;i < first.length;i++)
        {
            if (roundToInt(first[i]) != roundToInt(second[i]))
                return false;
        }
        return true;
    }

    public static ArrayList<Pair<Vector, Vector>> getInputOututSpec(Vector[] raw){
        ArrayList<Pair<Vector, Vector>> output = new ArrayList<Pair<Vector, Vector>>();
        for (int i = 0;i < raw.length - 1;i++){
            output.add(Pair.of(raw[i], raw[i+1]));
        }
        return output;
    }

    public static ArrayList<Pair<Vector, Vector>> getInputOututSpec(ArrayList<Vector> raw){
        Vector[] in = raw.toArray(new Vector[0]);

        return getInputOututSpec(in);
    }

}
