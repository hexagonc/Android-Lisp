package com.evolved.automata.android.lisp.guibuilder.v2;

import android.app.Dialog;
import android.view.View;


public interface FileChooserItem {
	
	public interface OnCreateChildFileListener
	{
		public void onSuccess(FileChooserItem item);
		public void onError(String message);
	}
	
	public String getFileName();
	
	
	public String getFileNameShort();
	public void onClickListener(Dialog parent);
	public void getChildren(OnChildFilesRequestedListener onChildrenReceived);
	public boolean hasChildren();
	public FileChooserItem getParent();
	public int getViewResource();
	public void configureView(View inflatedView);
	public int compare(FileChooserItem f);
	
	/**
	 * Return true if the FileChooser should should a progress dialog while waiting on the callbacl
	 * @param name
	 * @param listener
	 * @return
	 */
	public boolean onCreateChildFolder(String name, OnCreateChildFileListener listener);
	/**
	 * Return true if the FileChooser should should a progress dialog while waiting on the callbacl
	 * @param name
	 * @param listener
	 * @return
	 */
	public boolean onCreateChildFile(String name, OnCreateChildFileListener listener);

}
