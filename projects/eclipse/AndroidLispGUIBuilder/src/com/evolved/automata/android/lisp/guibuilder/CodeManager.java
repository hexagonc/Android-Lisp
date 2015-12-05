package com.evolved.automata.android.lisp.guibuilder;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Locale;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;

import com.dropbox.core.DbxRequestUtil;
import com.evolved.automata.KeyValuePair;
import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.android.AppStateManager;




import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.content.DialogInterface;
import android.net.Uri;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.Button;
import android.widget.EditText;
import android.widget.FrameLayout;
import android.widget.TextView;
import android.widget.Toast;


public class CodeManager {

	Context _context;
	
	// TODO: Use Dagger for this eventually
	GuiBuilderConfiguration _guiConfiguration = null;
	
	final LinkedList<KeyValuePair<String, String>> _predefinedSamples;
	
	public interface OnCodeTemplateSelectedListener
	{
		public void templateSelected(String template, boolean replaceEditor);
	}
	
	
	public interface OnFileSelectedListener
	{
		public void onFileSelected(String fullFileName, String fileContents, PathProtocol protocol, boolean replaceEditor);
		public void onError(String message, Exception e);
	}
	
	
	
	public enum PathProtocol
	{
		DROPBOX("dropbox"),
		LOCAL_STORAGE("storage");
		
		String name;
		
		public String toString()
		{
			return name;
		}
		
		PathProtocol(String nname)
		{
			name = nname;
		}
	}
	
	static CodeManager _manager = null;
	
	private CodeManager(Context con)
	{
		_context = con;
		_guiConfiguration = GuiBuilderConfiguration.get();
		_predefinedSamples = setPredefinedCodeTemplates();
	}
	
	public static CodeManager create(Context con)
	{
		if (_manager == null)
		{
			_manager = new CodeManager(con);
		}
		return _manager;
			
	}
	
	
	public static CodeManager get()
	{
		return _manager;
	}
	
	public String getPathUrl(String basePath, PathProtocol protocol)
	{
		return protocol + ":" + basePath;
	}
	
	public String getShortFileNameFromPathUrl(String pathUrl)
	{
		String[] parts = StringUtils.split(pathUrl, "/");
		return parts[parts.length-1];
	}
	
	public String getAbsoluteFileNameFromPathUrl(String pathUrl)
	{
		String[] parts = StringUtils.split(pathUrl, ":");
		return parts[1];
	}
	
	public void clearLastLoadedFileUrl()
	{
		GuiBuilderConfiguration.get().putString(GuiBuilderConfiguration.get().getStringResource(R.string.pref_key_last_loaded_file_url), "");
	}
	
	
	public void setLastLoadedFileUrl(String basePath, PathProtocol protocol)
	{
		String url = getPathUrl(basePath, protocol);
		GuiBuilderConfiguration.get().putString(GuiBuilderConfiguration.get().getStringResource(R.string.pref_key_last_loaded_file_url), url);
	}
	
	public String getLastLoadedFileUrl()
	{
		String last = GuiBuilderConfiguration.get().getString(GuiBuilderConfiguration.get().getStringResource(R.string.pref_key_last_loaded_file_url), null); 
		return (last!=null && last.length()>0)?last:null;
	}
	
	public PathProtocol getPathProtocol(String fileUrl)
	{
		String[] parts = StringUtils.split(fileUrl, ":");
		if (PathProtocol.DROPBOX.toString().equals(parts[0]))
			return PathProtocol.DROPBOX;
		else if (PathProtocol.LOCAL_STORAGE.toString().equals(parts[0]))
			return PathProtocol.LOCAL_STORAGE;
		else
			return null;
	}
	
	public String getLastLoadedDropboxParentFolder()
	{
		String key = _guiConfiguration.getStringResource(R.string.pref_key_last_loaded_dropbox_parent_folder);
		return _guiConfiguration.getString(key, "/");
	}
	
	public String getLastLoadedLocalStorageParentFolder()
	{
		String key = _guiConfiguration.getStringResource(R.string.pref_key_last_loaded_local_storage_parent_folder);
		return _guiConfiguration.getString(key, _context.getExternalFilesDir(null).getAbsolutePath());
	}
	
	public void setLastLoadedLocalStorageParentFolder(String path)
	{
		String key = _guiConfiguration.getStringResource(R.string.pref_key_last_loaded_local_storage_parent_folder);
		_guiConfiguration.putString(key, path);
	}
	
	public void setLastLoadedDropboxParentFolder(String path)
	{
		String key = _guiConfiguration.getStringResource(R.string.pref_key_last_loaded_dropbox_parent_folder);
		_guiConfiguration.putString(key, path);
	}
	
	public void showLocalStorageFileSelectDialog(final Activity activity, final OnFileSelectedListener listener)
	{
		final String basePath = getLastLoadedLocalStorageParentFolder();
		FileChooserDialog dialog = new FileChooserDialog(activity, "Select file to view", getLocalStoragePathChooserItem(new File(basePath), listener));
		dialog.show();
	}
	
	public void showCodeTemplateCreateDialog(final Activity activity, String value)
	{
		final Dialog dialog = new Dialog(activity);
		dialog.setTitle("Name this template");
		
		
		dialog.setContentView(R.layout.code_template_create_dialog);
		
		final LinkedHashMap<String, String> codeTemplate = GuiBuilderConfiguration.get().getAllCodeSnippets();
		codeTemplate.put("test", "(make-hashtable)");
		codeTemplate.put("horizontal-layout", "(horizontal-layout )");
		
		
		final EditText previewText = (EditText)dialog.findViewById(R.id.edit_code_template_preview);
		previewText.setText(value);
		final EditText nameText = (EditText)dialog.findViewById(R.id.edit_code_template_name);
		final TextView warningText = (TextView)dialog.findViewById(R.id.txt_warning);
		nameText.addTextChangedListener(new TextWatcher()
		{

			@Override
			public void beforeTextChanged(CharSequence s, int start, int count,
					int after) {
				// TODO Auto-generated method stub
				
			}

			@Override
			public void onTextChanged(CharSequence s, int start, int before,
					int count) {
				if (codeTemplate.size()>0)
				{
					if (codeTemplate.containsKey(nameText.getText().toString()))
						warningText.setVisibility(View.VISIBLE);
					else
						warningText.setVisibility(View.INVISIBLE);
				}
				
			}

			@Override
			public void afterTextChanged(Editable s) {
				// TODO Auto-generated method stub
				
			}
			
		});
		
		
		
		Button okButton = (Button)dialog.findViewById(R.id.but_ok);
		okButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) 
			{	
				String name = nameText.getText().toString();
				String value = previewText.getText().toString();
				if (name.trim().length()>0)
				{
					
					GuiBuilderConfiguration.get().saveCodeSnippet(name, value);
				}
				else
					Toast.makeText(activity, activity.getString(R.string.code_template_not_created_warning), Toast.LENGTH_LONG).show();
				dialog.dismiss();
			}
		});
		
		Button cancelButton = (Button)dialog.findViewById(R.id.but_cancel);
		cancelButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				dialog.dismiss();
			}
		});
		
		dialog.show();
	}
	
	
	public void showDropboxFileSelectDialog(final Activity activity, final OnFileSelectedListener listener)
	{
		final String basePath = getLastLoadedDropboxParentFolder();
		DropboxManager.get().getFile(basePath, new DropboxFile.DropboxFileResponseListener() {
			
			@Override
			public void onListedFiles(ArrayList<DropboxFile> children, Exception e) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void onAcquiredFileStringContents(String contents, Exception e) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void onAcquiredFile(final DropboxFile file, Exception e) {
				if (e!=null)
				{
					showDropboxFileSelectDialog(activity, "/", listener);
				}
				else
				{
					FileChooserDialog dialog = new FileChooserDialog(activity, "Select file to view", file.asFileChooserItem(listener));
					dialog.show();
				}
			}
		});
	}
	
	private void showDropboxFileSelectDialog(final Activity activity, final String basePath, final OnFileSelectedListener listener)
	{
		
		DropboxManager.get().getFile(basePath, new DropboxFile.DropboxFileResponseListener() {
			
			@Override
			public void onListedFiles(ArrayList<DropboxFile> children, Exception e) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void onAcquiredFileStringContents(String contents, Exception e) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void onAcquiredFile(final DropboxFile file, Exception e) {
				if (e!=null)
					listener.onError( "Error getting base dropbox path at: " + basePath, e);
				else
				{
					FileChooserDialog dialog = new FileChooserDialog(activity, "Select file to view", file.asFileChooserItem(listener));
					dialog.show();
				}
			}
		});
	}
	
	public void showCodeTemplateSelectDialog(Activity activity, final OnCodeTemplateSelectedListener listener)
	{
		final Dialog dialog = new Dialog(activity);
		dialog.setTitle("Select a template");
		
		final LinkedHashMap<String, String> tmap = _guiConfiguration.getAllCodeSnippets();
		OnTemplateSelectedListener internalListener = new  OnTemplateSelectedListener()
		{

			@Override
			public void onTemplateSelected(String name, String value,
					boolean replaceEditor) {
				dialog.dismiss();
				listener.templateSelected(value, replaceEditor);
			}

			@Override
			public void onTemplateSelectedWithUpdates(String name,
					String value, LinkedHashMap<String, String> templateMap,
					boolean replaceEditor) {
				dialog.dismiss();
				_guiConfiguration.replaceCodeTemplates(templateMap);
				listener.templateSelected(value, replaceEditor);
			}
			
		};
		
		dialog.setContentView(R.layout.code_template_dialog_view);
		
		TemplateManagerViewPager codeTemplateParent = (TemplateManagerViewPager)dialog.findViewById(R.id.avf_code_template_dialog_parent);
		codeTemplateParent.setData(tmap, internalListener);
		
		dialog.show();
	}
	
	public FileChooserItem getLocalStoragePathChooserItem(final File f, final OnFileSelectedListener listener)
	{
		return new FileChooserItem()
		{
			FileChooserItem me = this;
			FileChooserItem parent = null;
			
			{
				String parent = f.getParent();
				if (parent !=null)
				{
					Log.d("CodeManager.Filechooseritem.getParent", parent);
					Log.d("CodeManager.Filechooseritem.basePath", _context.getExternalFilesDir(null).getAbsolutePath());
					File parentFile = new File(parent);
					if (isChildOf(parentFile, _context.getExternalFilesDir(null)))
					{
						this.parent = getLocalStoragePathChooserItem(parentFile, listener);
						
					}
					else
						this.parent = null;
				}
				else
					this.parent = null;
			}
			@Override
			public String getFileName() {
				
				return f.getAbsolutePath();
			}

			@Override
			public String getFileNameShort() {
				
				return f.getName();
			}

			@Override
			public void onClickListener(Dialog parent) {
				BufferedReader reader = null;
				try
				{
					reader = new BufferedReader(new FileReader(f.getAbsolutePath()));
					String lineinput = "";
					StringBuilder out = new StringBuilder();
					while ((lineinput = reader.readLine())!=null)
					{
						out.append(lineinput);
						out.append(System.getProperty("line.separator", "\n"));
					}
					reader.close();
					reader = null;
					
					setLastLoadedLocalStorageParentFolder(f.getParent());
					setLastLoadedFileUrl(f.getAbsolutePath(), PathProtocol.LOCAL_STORAGE);
					listener.onFileSelected(f.getAbsolutePath(), out.toString(), PathProtocol.LOCAL_STORAGE, true);
				}
				catch (Exception e)
				{
					listener.onError("Error opening file: " + f.getAbsolutePath(), e);
				}
				finally
				{
					try
					{
						if (reader!=null)
							reader.close();
					}
					catch (Exception e)
					{
						AppStateManager.getInstance().onEvent("CodeManager:SelectFile", "Exception", e.toString());
					}
				}
			}

			@Override
			public void getChildren(
					OnChildFilesRequestedListener onChildrenReceived) {
				ArrayList<FileChooserItem> children = null;
				if (f.isDirectory())
				{
					children = new ArrayList<>();
					for (File child:f.listFiles())
					{
						children.add(getLocalStoragePathChooserItem(child, listener));
					}
					
				}
				onChildrenReceived.onChildrenRetrieved(me, children);
				
			}

			@Override
			public boolean hasChildren() {
				
				return f.isDirectory();
			}

			@Override
			public FileChooserItem getParent() {
				return parent;
			}

			@Override
			public int getViewResource() {
				
				return R.layout.file_chooser_item;
			}

			@Override
			public void configureView(View inflatedView) {
				TextView tview = (TextView)inflatedView;
				if (hasChildren())
					tview.setText(f.getName() + "/");
				else
					tview.setText(f.getName());
			}

			@Override
			public int compare(FileChooserItem f) {
				
				return 0;
			}

			@Override
			public boolean onCreateChildFolder(String name,
					OnCreateChildFileListener alistener) {
				if (f.isDirectory())
				{
					File childFile = new File(f.getAbsolutePath() + "/" + name);
					boolean success = childFile.mkdir();
					if (success)
						alistener.onSuccess(getLocalStoragePathChooserItem(childFile, listener));
					else
						alistener.onError("Errors creating: " + childFile.getAbsolutePath());
				}
				return false;
			}

			@Override
			public boolean onCreateChildFile(String name,
					OnCreateChildFileListener alistener) {
				if (f.isDirectory())
				{
					File childFile = new File(f.getAbsolutePath() + "/" + name);
					boolean success = false;
					try {
						success = childFile.createNewFile();
						if (success)
							alistener.onSuccess(getLocalStoragePathChooserItem(childFile, listener));
						else
							alistener.onError(childFile.getAbsolutePath() + " already exists");
					} catch (IOException e) {
						
						alistener.onError("Errors creating: " + childFile.getAbsolutePath() + " " + e.toString());
					}
					
				}
				return false;
			}
			
		};
	}
	
	private boolean isChildOf(File child, File potentialParent)
	{
		return child.getAbsolutePath().indexOf(potentialParent.getAbsolutePath())>-1;
	}
	
	public LinkedList<KeyValuePair<String, String>> getPredefinedCodeTemplates()
	{
		return _predefinedSamples;
	}
	
	private LinkedList<KeyValuePair<String, String>> setPredefinedCodeTemplates()
	{
		LinkedList<KeyValuePair<String, String>> samples = new LinkedList<>();
		 
		HashMap<String, String> predefinedSampleMap = new HashMap<String, String>();
		predefinedSampleMap.put("tic-tac-toe ui only", "tic_tac_toe_simple.lisp");
		predefinedSampleMap.put("tic-tac-toe with simple ai", "tic_tac_toe_playable.lisp");
		predefinedSampleMap.put("lego mindstorms sample", "mindstorms_sample.lisp");
		predefinedSampleMap.put("test", "test.lisp");
		predefinedSampleMap.put("speech test", "speech_test.lisp");
		predefinedSampleMap.put("robot control samples", "mindstorms_robot_samples.lisp");
		String[] predefinedSamples = new String[]{"robot control samples", "lego mindstorms sample", "test", "speech test",  "tic-tac-toe ui only", "tic-tac-toe with simple ai"};
		String assetCode = null;
		KeyValuePair<String, String> codeSpec;
		try
		{
			for (String title:predefinedSamples)
			{
				assetCode = AndroidTools.getFileAssetAsString(predefinedSampleMap.get(title), _context);
				codeSpec = new KeyValuePair<String, String>(title, assetCode);
				samples.add(codeSpec);
			}
		}
		catch (Exception e)
		{
			AppStateManager.getInstance().onError("GuiBuilderConfiguration:getPredefinedCodeTemplates", e);
		}
		
		return samples;
	}
	
	
}