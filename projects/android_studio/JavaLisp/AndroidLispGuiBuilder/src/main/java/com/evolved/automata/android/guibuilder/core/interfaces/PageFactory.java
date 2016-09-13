package com.evolved.automata.android.guibuilder.core.interfaces;

import com.evolved.automata.android.guibuilder.core.workspace.Page;

/**
 * Created by Evolved8 on 9/12/16.
 */
public interface PageFactory {
    public String serialize(Page page);
    public Page create(String serialized);
    public Page clone(Page page);
    public Page createNew();
}
