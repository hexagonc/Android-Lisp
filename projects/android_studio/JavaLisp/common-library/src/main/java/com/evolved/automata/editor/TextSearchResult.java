package com.evolved.automata.editor;

import java.util.concurrent.Future;

/**
 * Created by Evolved8 on 10/16/17.
 */

public interface TextSearchResult {

    interface ResultHandler {
        void onReplaceComplete(TextSearchIndex updatedIndex);
    }

    int getLineNumber();
    int getStartPosition();
    String getContextualText(int maxCharactersBefore, int maxCharactersAfter);
    void replaceResultText(String newText, ResultHandler onUpdated);
    String previewTextReplacement(String replacement, int maxCharactersBefore, int maxCharactersAfter);
    String getString();
}
