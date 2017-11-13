package com.evolved.automata.android.lisp.guibuilder.events;

import com.evolved.automata.editor.TextSearchResult;

import java.util.Iterator;

/**
 * Created by Evolved8 on 10/20/17.
 */

public class FindTextEvent {

    String mSearchText;
    Iterator<TextSearchResult> mResults = null;

    public FindTextEvent(String text)
    {
        mSearchText = text;
    }

    public void setResults(Iterator<TextSearchResult> results)
    {
        mResults = results;
    }

    public Iterator<TextSearchResult> getResults()
    {
        return mResults;
    }

    public String getSearchText()
    {
        return mSearchText;
    }



}
