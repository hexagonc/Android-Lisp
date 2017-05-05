package com.evolved.automata.android.lisp.guibuilder;

/**
 * Created by Evolved8 on 5/4/17.
 */

public interface LispDataAccessInterface {

    /**
     *
     * @param key
     * @param context
     * @param updateLastAccessP Updates the last modification timestamp when true
     * @return Null if the data does not exist at the key and value
     */
    String getData(String key, String context, boolean updateLastAccessP);

    /**
     *
     * @param key
     * @param context
     * @return null if the data is not present
     */
    String getData(String key, String context);
    String[] getAllKeys(String context);
    boolean hasData(String key, String context);
    boolean deleteData(String key, String context);
    int deleteAllData();
    int setData(String key, String context, String data);

    int deleteOldData( String context, long cutoffTime);
    String[] selectOldDataNames(String context, long cutoffTime);
}
