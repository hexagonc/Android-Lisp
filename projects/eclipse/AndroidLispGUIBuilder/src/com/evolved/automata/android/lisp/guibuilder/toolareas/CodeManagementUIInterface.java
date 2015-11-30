package com.evolved.automata.android.lisp.guibuilder.toolareas;

public interface CodeManagementUIInterface {
	public String getHighlightedText();
	public void replaceCodeEdit(String newCode);
	public void insertCodeAtEditCursor(String code);
	public void setCodeTitle(String title);
	public void onError(String message, Exception e);
	public String getCurrentLispEditorCode();

	public void showHintText(String text, int duration);
	public void showHintText(String text);
}
