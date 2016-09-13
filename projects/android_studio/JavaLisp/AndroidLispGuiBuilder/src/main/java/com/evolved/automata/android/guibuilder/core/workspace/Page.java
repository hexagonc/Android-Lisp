package com.evolved.automata.android.guibuilder.core.workspace;

/**
 * Created by Evolved8 on 9/12/16.
 */
public interface Page {
    String getName();
    String getText();
    void onDeselected();
    void onSelected();
    void setName(String name);
    void setText(String text);

    void onDeleted();
}
