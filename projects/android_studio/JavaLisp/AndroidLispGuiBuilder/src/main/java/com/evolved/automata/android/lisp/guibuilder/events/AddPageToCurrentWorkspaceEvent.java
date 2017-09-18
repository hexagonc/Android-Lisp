package com.evolved.automata.android.lisp.guibuilder.events;

import com.evolved.automata.android.lisp.guibuilder.model.CodePage;

/**
 * Created by Evolved8 on 9/18/17.
 */

public class AddPageToCurrentWorkspaceEvent extends ALGBEvent {
    CodePage mPage;

    private AddPageToCurrentWorkspaceEvent(CodePage page)
    {
        super(ALGBEventTypes.ADD_WORKSPACE_PAGE);
        mPage = page;
    }

    public CodePage getPage()
    {
        return mPage;
    }

    public static ALGBEvent make(CodePage page)
    {
        return new AddPageToCurrentWorkspaceEvent(page);
    }
}
