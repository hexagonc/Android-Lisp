package com.evolved.automata.android.lisp.guibuilder;

import java.util.LinkedHashMap;

public interface OnTemplateSelectedListener 
{
	public void onTemplateSelected(String name, String value, boolean replaceEditor);
	public void onTemplateSelectedWithUpdates(String name, String value, LinkedHashMap<String, String> templateMap, boolean replaceEditor);
	
}
