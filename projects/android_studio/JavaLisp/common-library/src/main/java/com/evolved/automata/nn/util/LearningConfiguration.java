package com.evolved.automata.nn.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;

import static com.evolved.automata.nn.util.LearningConfiguration.KEY.ANNEALING_FRACTION;
import static com.evolved.automata.nn.util.LearningConfiguration.KEY.BEST_SOLUTION_BONUS_ITERATIONS;
import static com.evolved.automata.nn.util.LearningConfiguration.KEY.BEST_SOLUTION_BONUS_MILLI;
import static com.evolved.automata.nn.util.LearningConfiguration.KEY.DATA_DISPLAY_LAMBDA;
import static com.evolved.automata.nn.util.LearningConfiguration.KEY.DEBUG_ALLOWED_TAGS;
import static com.evolved.automata.nn.util.LearningConfiguration.KEY.DEBUG_LEVEL;
import static com.evolved.automata.nn.util.LearningConfiguration.KEY.EARLY_STOP_SUCCESS_FRACTION;
import static com.evolved.automata.nn.util.LearningConfiguration.KEY.HISTORY_LENGTH;
import static com.evolved.automata.nn.util.LearningConfiguration.KEY.INITIAL_RANDOM_FRACTION;
import static com.evolved.automata.nn.util.LearningConfiguration.KEY.INPUT_VALIDATOR;
import static com.evolved.automata.nn.util.LearningConfiguration.KEY.MATCH_EQUALITY_ERROR;
import static com.evolved.automata.nn.util.LearningConfiguration.KEY.MAX_DURATION_MILLI;
import static com.evolved.automata.nn.util.LearningConfiguration.KEY.MAX_ITERATIONS;
import static com.evolved.automata.nn.util.LearningConfiguration.KEY.MUST_MATCH_FINAL_INPUT_PAIR;
import static com.evolved.automata.nn.util.LearningConfiguration.KEY.NUM_SOLUTION_BUFFER;
import static com.evolved.automata.nn.util.LearningConfiguration.KEY.PRIOR_RESULTS;
import static com.evolved.automata.nn.util.LearningConfiguration.KEY.RESET_ITER;
import static com.evolved.automata.nn.util.LearningConfiguration.KEY.STATUS_RESULT_IS_VALID;
import static com.evolved.automata.nn.util.LearningConfiguration.KEY.STATUS_SUCCESS_CRITERIA_SATISFIED;

/**
 * Created by Evolved8 on 5/28/18.
 */

public class LearningConfiguration {

    public interface InputValidator {
        boolean isValid(float[] raw);
    }

    public interface InputToStringConverter {
        String toString(float[] raw);
    }


    public enum KEY {
        ANNEALING_FRACTION(Double.class),
        SKIP_LAMBDA,
        MAX_DURATION_MILLI(Integer.class),
        BEST_SOLUTION_BONUS_ITERATIONS(Integer.class),
        BEST_SOLUTION_BONUS_MILLI(Integer.class),
        EARLY_STOP_SUCCESS_FRACTION(Double.class),
        MUST_MATCH_FINAL_INPUT_PAIR(Boolean.class),
        INPUT_IS_PRIOR_BEST_RESULT(Boolean.class),
        MIN_ACCEPTABLE_SUCCESS_FRACTION(Double.class),
        PRIOR_RESULTS,
        MAX_ITERATIONS(Integer.class),
        NUM_SOLUTION_BUFFER(Integer.class),
        STATUS_RESULT_IS_VALID(Boolean.class),
        STATUS_SUCCESS_CRITERIA_SATISFIED(Boolean.class),
        RESET_ITER(Integer.class),
        INITIAL_RANDOM_FRACTION(Double.class),
        DEBUG_LEVEL(Integer.class),
        DEBUG_ALLOWED_TAGS,
        HISTORY_LENGTH(Integer.class),
        MATCH_EQUALITY_ERROR(Double.class),
        INPUT_VALIDATOR,
        SLOW_LEARNING_THRESHOLD_MULTIPLE,
        RESET_ON_SLOW_LEARNING,
        DATA_DISPLAY_LAMBDA;

        Class type = null;

        boolean skip = false;
        KEY(Class c){
            type = c;
        }

        KEY(){
            type = null;
            skip = true;
        }

        public Class getType(){
            return type;
        }

    }

    public byte[] serializeBytes(){
        int numKeys = (int)_dataMap.entrySet().stream().filter(pair->!pair.getKey().skip).count();
        GroupSerializer.Builder b = GroupSerializer.get().serialize();
        b.add(Integer.valueOf(numKeys));
        for (Map.Entry<KEY, Object> pair:_dataMap.entrySet()){
            KEY key = pair.getKey();
            if (key.skip)
                continue;
            int typeIndex = key.ordinal();

            b.add(Integer.valueOf(typeIndex));
            b.add(pair.getValue());
        }
        return b.build();
    }

    public static LearningConfiguration deserializeBytes(byte[] data){
        ArrayList values = GroupSerializer.get().deserialize(data);
        HashMap<KEY, Object> map = new HashMap<KEY, Object>();
        int size = (Integer)values.get(0);

        if (size > 0){
            KEY[] v = KEY.values();
            KEY key = KEY.ANNEALING_FRACTION;
            for (int i = 1;i < values.size();i++){

                if (i % 2 == 1){
                    int type = (Integer)values.get(i);
                    key = v[type];
                }
                else {
                    map.put(key, values.get(i));
                }
            }
        }
        return new LearningConfiguration(map);
    }

    HashMap<KEY, Object> _dataMap = new HashMap<KEY, Object>();

    public LearningConfiguration(HashMap<KEY, Object> map){
        _dataMap = (HashMap<KEY, Object>)map.clone();
    }

    public LearningConfiguration setInputToStringConverter(InputToStringConverter displayRunnable){
        _dataMap.put(DATA_DISPLAY_LAMBDA, displayRunnable);
        return this;
    }

    public String getInputString(float[] input){
        InputToStringConverter converter = (InputToStringConverter)_dataMap.get(DATA_DISPLAY_LAMBDA);
        if (converter != null)
            return converter.toString(input);
        else
            return Arrays.toString(input);
    }

    public boolean isValidInput(float[] input){
        InputValidator v = getInputValidator();
        return v == null || v.isValid(input);
    }

    public InputValidator getInputValidator(){
        return (InputValidator)_dataMap.get(INPUT_VALIDATOR);
    }

    public LearningConfiguration setInputValidator(InputValidator validator){
        _dataMap.put(INPUT_VALIDATOR, validator);
        return this;
    }

    public LearningConfiguration copy(){
        HashMap<KEY, Object> map = new HashMap<KEY, Object>();

        for (KEY k:_dataMap.keySet()){
            Object v = _dataMap.get(k);
            if (k == DEBUG_ALLOWED_TAGS && v != null){
                HashSet<String> s = new HashSet<>(), c = (HashSet<String>)v;
                for (String key:c){
                    s.add(key);
                }
                map.put(k, s);
            }
            else
                map.put(k, v);
        }

        return new LearningConfiguration(map);
    }

    public Integer getHistoryLength(){
        Integer value = (Integer)_dataMap.get(HISTORY_LENGTH);
        if (value == null || value <1){
            return 1;
        }
        else
            return value;
    }

    public LearningConfiguration setHistoryLength(int length){
        _dataMap.put(HISTORY_LENGTH, length);
        return this;
    }


    public Double getMatchEqualityError(){
        Double v = (Double)_dataMap.get(MATCH_EQUALITY_ERROR);
        if (v == null)
            return Double.valueOf(0);
        else
            return v;
    }

    public LearningConfiguration setMatchEqualityError(double error){
        if (error >= 0){
            _dataMap.put(MATCH_EQUALITY_ERROR, Double.valueOf(error));
        }
        return this;
    }

    public LearningConfiguration addAllowedDebugTag(String tag){
        HashSet<String> tags = (HashSet<String>)_dataMap.get(DEBUG_ALLOWED_TAGS);
        if (tags == null){
            _dataMap.put(DEBUG_ALLOWED_TAGS, tags = new HashSet<String>());
        }
        tags.add(tag);
        return this;
    }

    public LearningConfiguration removeDebugTag(String tag){
        HashSet<String> tags = (HashSet<String>)_dataMap.get(DEBUG_ALLOWED_TAGS);
        if (tags != null){
            tags.remove(tag);
        }
        return this;
    }

    public boolean isDebugTagAllowed(String tag, int minDebugLevel){
        if (getDebugLevel() == null)
            return false;

        if (getDebugLevel() >= minDebugLevel){
            return true;
        }
        HashSet<String> tags = (HashSet<String>)_dataMap.get(DEBUG_ALLOWED_TAGS);
        if (tags != null){
            return tags.contains(tag);
        }
        else
            return false;
    }

    public LearningConfiguration(){

    }

    public Object getConfig(KEY key) {
        return _dataMap.get(key);
    }

    public Integer getDebugLevel(){
        return (Integer)_dataMap.get(DEBUG_LEVEL);
    }

    public LearningConfiguration setDebugLevel(Integer level){
        _dataMap.put(DEBUG_LEVEL, level);
        return this;
    }

    public Integer getFailureIterations(){
        return (Integer)_dataMap.get(RESET_ITER);
    }

    public Float initialRandomFraction(){
        return (Float)_dataMap.get(INITIAL_RANDOM_FRACTION);
    }

    public LearningConfiguration setInitialRandomFraction(Float randomFraction){
        _dataMap.put(INITIAL_RANDOM_FRACTION, randomFraction);
        return this;
    }

    public LearningConfiguration setFailureIterations(int iterations){
        _dataMap.put(RESET_ITER, iterations);
        return this;
    }

    public LearningConfiguration set(KEY key, Object value) {
        _dataMap.put(key, value);
        return this;
    }

    public LearningConfiguration setMaxIterations(int max){
        return set(MAX_ITERATIONS, max);
    }

    public LearningConfiguration setResultIsValid(boolean isValid){
        _dataMap.put(STATUS_RESULT_IS_VALID, isValid);
        return this;
    }

    public LearningConfiguration wasSuccessCriteriaSatisfied(boolean isSuccess){
        _dataMap.put(STATUS_SUCCESS_CRITERIA_SATISFIED, isSuccess);
        return this;
    }

    public double annealingFraction(){
        return (Double)_dataMap.get(ANNEALING_FRACTION);
    }

    public boolean hasAnnealingFraction(){
        return _dataMap.containsKey(ANNEALING_FRACTION);
    }

    public boolean requiresCompleteMatchP() {
        return !_dataMap.containsKey(EARLY_STOP_SUCCESS_FRACTION);
    }

    public Double earlyStopSuccessFraction (){
        return (Double)_dataMap.get(EARLY_STOP_SUCCESS_FRACTION);
    }


    public boolean requiresLastPairToMatch(){
        return  _dataMap.containsKey(MUST_MATCH_FINAL_INPUT_PAIR) &&
                (Boolean)_dataMap.get(MUST_MATCH_FINAL_INPUT_PAIR);
    }



    public boolean hasBestSolutionBonusMilli() {
        return _dataMap.containsKey(BEST_SOLUTION_BONUS_MILLI);
    }

    public Integer getBestSolutionBonusMilli() {
        return (Integer)_dataMap.get(BEST_SOLUTION_BONUS_MILLI);
    }


    public boolean hasBestSolutionBonusIterations () {
        return _dataMap.containsKey(BEST_SOLUTION_BONUS_ITERATIONS);
    }

    public Integer getBestSolutionBonusIterations (){
        return (Integer)_dataMap.get(BEST_SOLUTION_BONUS_ITERATIONS);
    }

    public boolean hasMaxDuration(){
        return _dataMap.containsKey(MAX_DURATION_MILLI);
    }


    public LearningConfiguration getUnlimitedConfiguration(){
        LearningConfiguration next = new LearningConfiguration(_dataMap);
        next._dataMap.remove(MAX_DURATION_MILLI);
        next._dataMap.remove(MAX_ITERATIONS);
        return next;
    }

    public LearningConfiguration getUnlimitedConfiguration(int maxPracticalDuration){
        LearningConfiguration next = new LearningConfiguration(_dataMap);
        next._dataMap.put(MAX_DURATION_MILLI, Integer.valueOf(maxPracticalDuration));
        next._dataMap.remove(MAX_ITERATIONS);
        return next;
    }

    public Integer getMaxDurationMilli(){
        return (Integer)_dataMap.get(MAX_DURATION_MILLI);
    }

    public LearningConfiguration setMaxDurationMilli(int max){
        _dataMap.put(MAX_DURATION_MILLI, Integer.valueOf(max));
        return this;
    }

    public boolean hasPriorResults(){
        return _dataMap.containsKey(PRIOR_RESULTS);
    }

    public IncrementalUpdateSpec getPriorResults() {
        return (IncrementalUpdateSpec)_dataMap.get(PRIOR_RESULTS);
    }


    public boolean hasMaxIterations() {
        return _dataMap.containsKey(MAX_ITERATIONS);
    }

    public Integer getMaxIterations() {
        return (Integer)_dataMap.get(MAX_ITERATIONS);
    }

    public boolean hasBestSolutionBufferSize() {
        return _dataMap.containsKey(NUM_SOLUTION_BUFFER);
    }

    public Integer getBestSolutionBufferSize() {
        return (Integer)_dataMap.get(NUM_SOLUTION_BUFFER);
    }
}
