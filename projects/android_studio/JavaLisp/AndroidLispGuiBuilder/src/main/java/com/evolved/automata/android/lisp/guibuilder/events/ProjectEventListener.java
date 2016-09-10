package com.evolved.automata.android.lisp.guibuilder.events;

public interface ProjectEventListener 
{
	public void currentCodePageDeleted(String newPageTitle, String newPageText, boolean hasPrevious, boolean hasNext);
}
