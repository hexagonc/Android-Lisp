package com.evolved.automata.editor;

import java.util.Iterator;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 10/16/17.
 */

public interface SearchableText {
    interface Result {
        void onResult(Exception error);
    }

    int getNumResults();
    LinkedList<TextSearchResult> getAllResults();
    Iterator<TextSearchResult> getResultIterator();
    void gotoSearchResult(TextSearchResult result, Result onComplete);
    void highlight(TextSearchResult result, Result onComplete);
    void highlightAllResults(Result onComplete);
    TextSearchResult gotoFirstResult();
    TextSearchResult gotoNextResult();
    TextSearchResult gotoPreviousResult();
    boolean hasNext();
}
