package com.evolved.automata.android.lisp.guibuilder;

import java.util.ArrayList;


public interface OnChildFilesRequestedListener {
	public void onChildrenRetrieved(FileChooserItem parentOfChildren, ArrayList<FileChooserItem> children);
	public void onErrorsRetrievingChildren();
}
