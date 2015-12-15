package com.evolved.automata.android.lisp.guibuilder;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.PriorityQueue;

import android.app.Dialog;
import android.graphics.Color;
import android.os.AsyncTask;
import android.util.Log;
import android.view.View;
import android.widget.TextView;

import com.dropbox.core.DbxException;
import com.dropbox.core.DbxStreamWriter;
import com.dropbox.core.NoThrowOutputStream;
import com.dropbox.core.v1.DbxClientV1;
import com.dropbox.core.v1.DbxEntry;
import com.dropbox.core.v1.DbxWriteMode;
import com.evolved.automata.android.AppStateManager;
import com.evolved.automata.android.lisp.guibuilder.CodeManager.OnFileSelectedListener;
import com.evolved.automata.android.lisp.guibuilder.DropboxManager.OnFileUploadListener;
import com.evolved.automata.android.lisp.guibuilder.FileChooserItem.OnCreateChildFileListener;
import com.evolved.automata.lisp.Value;



public class DropboxFile 
{
	public interface DropboxFileResponseListener
	{
		public void onListedFiles(ArrayList<DropboxFile> children,  Exception e);
		public void onAcquiredFile(DropboxFile file, Exception e);
		public void onAcquiredFileStringContents(String contents, Exception e);
	}
	
	
	DbxClientV1 _client = null;
	DropboxFile _parent = null;
	DbxEntry.WithChildren _entry = null;
	public DropboxFile(DbxClientV1 client, String path) throws DbxException
	{
		_client = client;
		_entry = client.getMetadataWithChildren(path);
		_parent = getParentInnner();
	}
	
	public boolean isFolder()
	{
		return _entry.entry.isFolder();
	}
	
	public void listFiles(final DropboxFileResponseListener responseListener)
	{
		if (isFolder())
		{
			final ArrayList<DropboxFile> out = new ArrayList<DropboxFile>();
			final List<Exception> exceptionContainer = new LinkedList<Exception>();
			
			AsyncTask<Void, Void, Void> task = new AsyncTask<Void, Void, Void>()
					{

						@Override
						protected void onPostExecute(Void result) {
							
							super.onPostExecute(result);
							
							if (responseListener!=null)
							{
								if (exceptionContainer.size()>0)
									responseListener.onListedFiles(out, exceptionContainer.get(0));
								else
									responseListener.onListedFiles(out, null);
							}
						}

						@Override
						protected Void doInBackground(Void... params) {
							
							List<DbxEntry> children = _entry.children;
							try
							{
								for (DbxEntry e:children)
								{
									out.add(new DropboxFile(_client, e.path));
								}
							}
							catch (Exception e)
							{
								exceptionContainer.add(e);
								
							}
							
							return null;
						}
						
					};
			task.execute();
		}
		else
			if (responseListener!=null)
				responseListener.onListedFiles(null, null);
	}
	
	public String getFullFileName()
	{
		return _entry.entry.path;
	}
	
	public String getName()
	{
		return _entry.entry.name;
	}
	
	private DropboxFile getParentInnner()
	{
		java.io.File f = new java.io.File(_entry.entry.path);
		String parent = f.getParent();
		if (parent != null)
		{
			try {
				return new DropboxFile(_client, parent);
			} catch (DbxException e) {
				Log.e("DB", e.toString());
				return null;
			}
		}
		else
			return null;
	}
	
	public DropboxFile getParent()
	{
		return _parent;
	}
	
	public FileChooserItem asFileChooserItem(final OnFileSelectedListener listener)
	{
		return  new FileChooserItem()
		{
			FileChooserItem thisItem;
			{
				thisItem = this;
			}
			
			@Override
			public String getFileName() {
				
				return getFullFileName();
			}

			@Override
			public String getFileNameShort() {
				// 
				return getName();
			}

			@Override
			public void onClickListener(Dialog parent) {
				getFileContents(listener);
			}

			@Override
			public void getChildren(final OnChildFilesRequestedListener onChildrenReceived) {
				if (isFolder())
				{
					listFiles(new DropboxFileResponseListener() {
						
						@Override
						public void onListedFiles(ArrayList<DropboxFile> children, Exception e) {
							ArrayList<FileChooserItem> directoryItems = new ArrayList<FileChooserItem>();
							
							PriorityQueue<FileChooserItem> sortHeap = new PriorityQueue<FileChooserItem>(1, new Comparator<FileChooserItem>()
									{
										public int compare(FileChooserItem left, FileChooserItem right)
										{
											return left.getFileNameShort().compareTo(right.getFileNameShort());
											
										}
									});
							
							for (DropboxFile child:children)
							{
								if (child.isFolder())
									sortHeap.add(child.asFileChooserItem(listener));
							}
							
							while (sortHeap.size() > 0)
							{
								directoryItems.add(sortHeap.remove());
							}
							
							
							
							for (DropboxFile child:children)
							{
								if (!child.isFolder())
									sortHeap.add(child.asFileChooserItem(listener));
							}
							
							while (sortHeap.size() > 0)
							{
								directoryItems.add(sortHeap.remove());
							}
							
							onChildrenReceived.onChildrenRetrieved(thisItem, directoryItems);
						}
						
						@Override
						public void onAcquiredFileStringContents(String contents, Exception e) {
							
						}
						
						@Override
						public void onAcquiredFile(DropboxFile file, Exception e) {
							
						}
					});
				}
			}

			@Override
			public boolean hasChildren() {

				return isFolder();
			}

			@Override
			public FileChooserItem getParent() {
				DropboxFile p = DropboxFile.this.getParent();
				if (p!=null)
					return p.asFileChooserItem(listener);
				return null;
			}

			@Override
			public int getViewResource() {
				
				return R.layout.file_chooser_item;
			}

			@Override
			public void configureView(View inflatedView) {
				TextView t = (TextView)inflatedView;
				
				if (isFolder())
				{
					t.setTextColor(Color.RED);
					t.setText(this.getFileNameShort() + "/");
				}
				else
				{
					t.setText(this.getFileNameShort());
					t.setTextColor(Color.BLACK);
				}
			}

			@Override
			public int compare(FileChooserItem f) {
				// TODO Auto-generated method stub
				return 0;
			}

			@Override
			public boolean onCreateChildFolder(final String name,
					final OnCreateChildFileListener alistener) {
				if (isFolder())
				{
					
					final LinkedList<String> errors = new LinkedList<String>();
					AsyncTask<Void, Void, DropboxFile> task  =new  AsyncTask<Void, Void, DropboxFile>()
					{

						@Override
						protected void onPostExecute(DropboxFile param)
						{
							if (param!=null)
								alistener.onSuccess(param.asFileChooserItem(listener));
							else
								alistener.onError(errors.getFirst());
						}
						
						@Override
						protected DropboxFile doInBackground(Void... params) {
							String folderName = getFullFileName() + "/" + name;
							try
							{
								_client.createFolder(folderName);
								return new DropboxFile(_client, folderName);
							}
							catch (Exception e)
							{
								AppStateManager.getInstance().onError("DropboxFile.onCreateChildFolder", e);
								errors.add(e.toString());
							}
							return null;
						}
						
					};
					task.execute();
					return true;
				}
				else
					return false;
			}

			@Override
			public boolean onCreateChildFile(final String name,
					final OnCreateChildFileListener alistener) {
				if (isFolder())
				{
					
					final LinkedList<String> errors = new LinkedList<String>();
					AsyncTask<Void, Void, DropboxFile> task  =new  AsyncTask<Void, Void, DropboxFile>()
					{

						@Override
						protected void onPostExecute(DropboxFile param)
						{
							if (param!=null)
								alistener.onSuccess(param.asFileChooserItem(listener));
							else
								alistener.onError(errors.getFirst());
						}
						
						@Override
						protected DropboxFile doInBackground(Void... params) {
							String fileName = getFullFileName() + "/" + name;
							try
							{
								_client.uploadFile(fileName, DbxWriteMode.force(), -1, new DbxStreamWriter<RuntimeException>()
										{

											@Override
											public void write(
													NoThrowOutputStream out)
													throws RuntimeException {
												out.write(" ".getBytes());
												out.flush();
											}
									
										});
								DropboxFile dfFile = new DropboxFile(_client, fileName);
								return dfFile;
							}
							catch (Exception e)
							{
								AppStateManager.getInstance().onError("DropboxFile.onCreateFile", e);
								errors.add(e.toString());
							}
							return null;
						}
						
					};
					task.execute();
					return true;
				}
				else
					return false;
			}
			
		};
	}
	
	
	public void getStringContents(final DropboxFileResponseListener responseListener)
	{
		if (!isFolder())
		{
			
			final List<Exception> exceptionContainer = new LinkedList<Exception>();
			
			AsyncTask<Void, Void, String> task = new AsyncTask<Void, Void, String>()
					{

						@Override
						protected void onPostExecute(String result) {
							
							super.onPostExecute(result);
							
							if (responseListener!=null)
							{
								if (exceptionContainer.size()>0)
									responseListener.onAcquiredFileStringContents(null, exceptionContainer.get(0));
								else
									responseListener.onAcquiredFileStringContents(result, null);
							}
						}

						@Override
						protected String doInBackground(Void... params) {
							
							
							try
							{
								ByteArrayOutputStream bistream = new ByteArrayOutputStream();
								_client.getFile(getFullFileName(), null, bistream);
								return bistream.toString();
							}
							catch (Exception e)
							{
								exceptionContainer.add(e);
								
							}
							
							return null;
						}
						
					};
			task.execute();
		}
		if (responseListener!=null)
			responseListener.onAcquiredFileStringContents(null, null);
	}
	
	private void getFileContents(final OnFileSelectedListener listener)
	{
		DropboxFileResponseListener responseListener = new DropboxFileResponseListener()
		{

			@Override
			public void onListedFiles(ArrayList<DropboxFile> children,
					Exception e) {
				// TODO Auto-generated method stub
				
			}

			@Override
			public void onAcquiredFile(DropboxFile file, Exception e) {
				// TODO Auto-generated method stub
				
			}

			@Override
			public void onAcquiredFileStringContents(String contents,
					Exception e) {
				if (e == null)
				{
					CodeManager.get().setLastLoadedDropboxFile(getFullFileName());
					CodeManager.get().setLastLoadedFileUrl(getFullFileName(), CodeManager.PathProtocol.DROPBOX);
					listener.onFileSelected(getFullFileName(), contents, CodeManager.PathProtocol.DROPBOX, true);
					
				}
				else
				{
					listener.onError("Errors retrieving the contents to: " + getFullFileName() , e);
				}
			}
			
		};
		
		getStringContents(responseListener);
	}
	
	public void updateContents(final String newContents, final OnFileUploadListener listener)
	{
		final LinkedList<Exception> exceptionList = new LinkedList<Exception>();
		AsyncTask<String, Void, String> task = new AsyncTask<String, Void, String>()
				{

					@Override
					protected void onPostExecute(String result) {
						
						super.onPostExecute(result);
						
						if (exceptionList.size()==0)
						{
							listener.uploaded(result);
						}
						else
						{
							listener.onError("Error uploading file: " + getFullFileName(), exceptionList.getFirst());
						}
					}

					@Override
					protected String doInBackground(String... params) {
						// TODO Update this to notify user if last downloaded version of file is not most recent
						// 
						byte[] data = newContents.getBytes(Charset.forName("UTF-8"));
						DbxClientV1.Uploader uploader = null;
						OutputStream ostream = null;
						try
						{
							uploader = _client.startUploadFile(getFullFileName(), DbxWriteMode.update(null), data.length);
							ostream = uploader.getBody();
							ostream.write(data);
							uploader.finish();
							return getFullFileName();
						}
						catch (Exception e)
						{
							exceptionList.add(e);
						}
						finally
						{
							if (uploader!=null)
								uploader.close();
							
						}
						return null;
					}
			
				};
		task.execute();
	}
}
