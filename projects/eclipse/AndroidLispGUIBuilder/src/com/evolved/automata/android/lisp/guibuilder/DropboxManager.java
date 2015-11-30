package com.evolved.automata.android.lisp.guibuilder;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Locale;

import com.dropbox.core.DbxRequestConfig;

import com.dropbox.core.v1.DbxClientV1;
import com.evolved.automata.android.lisp.guibuilder.DropboxFile.DropboxFileResponseListener;



import android.content.Context;

import android.os.AsyncTask;

public class DropboxManager 
{
	
	
	public interface OnFileUploadListener
	{
		public void uploaded(String filename);
		public void onError(String message, Exception e);
	}
	
	Context _context;
	private static DropboxManager _dManager = null;
	DbxClientV1 _dropboxClient = null;
	
	private DropboxManager(Context context, String accessToken)
	{
		_context = context;
		setClient(accessToken);
	}
	
	public static DropboxManager create(Context context, String accessToken)
	{
		if (_dManager == null)
			_dManager = new DropboxManager(context, accessToken);
		return _dManager;
	}
	
	public void updateToken(String accessToken)
	{
		setClient(accessToken);
	}
	
	public void setLastDownloadedFileRevision(String revision)
	{
		String key = GuiBuilderConfiguration.get().getStringResource(R.string.pref_key_last_dropbox_file_downloaded_revision);
		GuiBuilderConfiguration.get().putString(key, revision);
	}
	
	public String getLastDownloadedFileRevision()
	{
		String key = GuiBuilderConfiguration.get().getStringResource(R.string.pref_key_last_dropbox_file_downloaded_revision);
		return GuiBuilderConfiguration.get().getString(key, null);
	}
	
	public boolean allowOverwriteUnSynchronizedFileP()
	{
		String key = GuiBuilderConfiguration.get().getStringResource(R.string.pref_key_allow_overwrite_unsynchronized_dropbox_file);
		return GuiBuilderConfiguration.get().getBoolean(key, false);
	}
	
	public static DropboxManager get()
	{
		return _dManager;
	}
	
	public void getFile(final String path, final DropboxFile.DropboxFileResponseListener listener)
	{
		final LinkedList<Exception> elist = new LinkedList<Exception>();
		AsyncTask<Void, Void, DropboxFile> task = new AsyncTask<Void, Void, DropboxFile>()
		{

			@Override
			protected DropboxFile doInBackground(Void... params) {
				
				try
				{
					return new DropboxFile(_dropboxClient, path);
				}
				catch (Exception e)
				{
					elist.add(e);
				}
				return null;
			}

			@Override
			protected void onPostExecute(DropboxFile result) {
				
				super.onPostExecute(result);
				if (elist.size() == 0)
				{
					listener.onAcquiredFile(result, null);
				}
				else
					listener.onAcquiredFile(null, elist.getFirst());
				
			}
		
		};
		task.execute();
	}
	
	public void resynchLastDownloadedDropboxFile(final String newFileContents, final OnFileUploadListener uploadListener)
	{
		String lastFileUrl = CodeManager.get().getLastLoadedFileUrl();
		final String lastdownloadedFile = CodeManager.get().getAbsoluteFileNameFromPathUrl(lastFileUrl);
		
		DropboxFile.DropboxFileResponseListener listener = new DropboxFileResponseListener() {
			
			@Override
			public void onListedFiles(ArrayList<DropboxFile> children, Exception e) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void onAcquiredFileStringContents(String contents, Exception e) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void onAcquiredFile(DropboxFile file, Exception e) {
				if (e == null)
				{
					file.updateContents(newFileContents, uploadListener);
				}
				else
				{
					uploadListener.onError("Error retrieving file from Dropbox: " + lastdownloadedFile, e);
				}
			}
		};
		
		getFile(lastdownloadedFile, listener);
	}
	
	
	private DbxRequestConfig getDropboxRequestConfig()
	{
		
		return new DbxRequestConfig(_context.getString(R.string.dropbox_app_client_name), Locale.getDefault().toString());
	}
	
	private void setClient(String token)
	{
		_dropboxClient = new DbxClientV1(getDropboxRequestConfig(), token);
	}
	
	
}
