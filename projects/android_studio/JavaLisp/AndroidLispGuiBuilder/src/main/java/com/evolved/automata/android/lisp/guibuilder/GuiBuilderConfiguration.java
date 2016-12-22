package com.evolved.automata.android.lisp.guibuilder;

import java.security.SecureRandom;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Locale;

import org.apache.commons.lang3.StringUtils;

import com.dropbox.core.DbxRequestConfig;
import com.dropbox.core.DbxRequestUtil;
import com.dropbox.core.android.AuthActivity.SecurityProvider;
import com.dropbox.core.v2.DbxClientV2;
import com.evolved.automata.AITools;
import com.evolved.automata.KeyValuePair;
import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.android.AppStateManager;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.SharedPreferences;
import android.net.Uri;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.EditText;
import android.widget.FrameLayout;

public class GuiBuilderConfiguration 
{
	public interface OnDropboxAuthorizedListener
	{
		public void onAuthorized();
		
	};
	
	Handler _mainHandler = new Handler(Looper.getMainLooper());
	
	Context _context = null;
	SharedPreferences _sPreferences = null;
	
	// Rotor settings.  Set these to whatever you used to encode your app keys and secrets
	String r1 = "0";
	String r2 = "0";
	String r3 = "4";
	String r4 = "2";
	String r5 = "0";
	
	// Dial settings.  Set these to whatever you used to encode your app keys and secrets
	String d1 = "z";
	String d2 = "Z";
	String d3 = "1";
	String d4 = "a";
	String d5 = "F";
	
	
	// seed used to encode your app keys and secrets
	long seed = 201510121852L;
	
	private static GuiBuilderConfiguration _configuration = null; 
	LinkedHashMap<String, String> _codeSnippetMap;
	
	boolean _snippetMapSynchedP = false;
	String currentAccessToken = null;
	
	
	private GuiBuilderConfiguration(Context context)
	{
		_context = context;
		_sPreferences = context.getSharedPreferences("default", Context.MODE_PRIVATE);
		
		initialize();
		
	}
	
	
	
	
	public static GuiBuilderConfiguration create(Context context)
	{
		_configuration = new GuiBuilderConfiguration(context);
		return _configuration;
	}
	
	public static GuiBuilderConfiguration get()
	{
		return _configuration;
	}
	
	
	// .`--.`--.`--.`--.`--.`--.`--.`--.`--.`--.`--.`--.`--.`--.`--.`--.`--
	//						Task-specific functions
	// .`--.`--.`--.`--.`--.`--.`--.`--.`--.`--.`--.`--.`--.`--.`--.`--.`--
	
	
	
	
	public boolean hasDropboxAuthorizationP()
	{
		currentAccessToken = getString(getStringResource(R.string.pref_key_dropbox_current_access_token), null); 
		return  currentAccessToken != null && currentAccessToken.length()>0;
	}
	
	
	
	public boolean dropboxConfiguredP()
	{
		return getDropboxAppKey() != null;
	}
	
	public void showDropboxLoginDialog(Activity activity, final OnDropboxAuthorizedListener listener)
	{
		if (!dropboxConfiguredP())
			return;
		
		AlertDialog.Builder builder = new AlertDialog.Builder(activity);
		final AlertDialog ad = builder.create();
		Window w = ad.getWindow();
		WindowManager.LayoutParams wparams = w.getAttributes();
		wparams.height = 300;
		wparams.width = WindowManager.LayoutParams.MATCH_PARENT;
	
		wparams.gravity = Gravity.TOP;
		w.setAttributes(wparams);
		
		FrameLayout fl = new FrameLayout(activity);
		
		fl.setLayoutParams(new ViewGroup.MarginLayoutParams(ViewGroup.LayoutParams.FILL_PARENT, ViewGroup.LayoutParams.FILL_PARENT));
		fl.setFocusableInTouchMode(true);
		// Hack so that form fields work in WebView
		EditText hackView = new EditText(activity);
		hackView.setLayoutParams(new FrameLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT));
		hackView.setVisibility(View.GONE);
		fl.addView(hackView);
		final WebView view = new WebView(activity);
		view.setFocusableInTouchMode(true);
		final String idToken = createStateNonce();
		fl.addView(view);
		view.setLayoutParams(new FrameLayout.LayoutParams(ViewGroup.LayoutParams.FILL_PARENT, ViewGroup.LayoutParams.FILL_PARENT));
		final WebViewClient wc = new WebViewClient()
		{

			@Override
			public void onPageFinished(WebView view, String url) {
				Uri u = Uri.parse(url);
				String secret = u.getQueryParameter("oauth_token_secret");
				String nonce = u.getQueryParameter("state");
				if (secret!=null && idToken.equals(nonce))
				{
					saveAppToken(secret);
					DropboxManager dm = DropboxManager.get();
					if (dm!=null)
						dm.updateToken(secret);
					else
						DropboxManager.create(_context, secret);
					ad.dismiss();
					if (listener!=null)
					{
						Runnable updateRunnable = new Runnable()
						{
							public void run()
							{
								
									listener.onAuthorized();
							}
						};
						_mainHandler.post(updateRunnable);
					}
					
					
					
					
					return;
				}
				super.onPageFinished(view, url);
				
			}

			@Override
			public void onLoadResource(WebView view, String url) {
				// 
				super.onLoadResource(view, url);
				
				
			}
			
			@Override
			public boolean shouldOverrideUrlLoading(WebView view, String url)
			{
				return false;
			}
			
		};
		
		
		view.setWebViewClient(wc);
		
		String path = "1/connect";
        Locale locale = Locale.getDefault();

        // Web Auth currently does not support desiredUid and only one alreadyAuthUid (param n).
        // We use first alreadyAuthUid arbitrarily.
        // Note that the API treats alreadyAuthUid of 0 and not present equivalently.
        String alreadyAuthedUid = "0";
        String[] params = {
                "k", getDropboxAppKey(),
                "n", alreadyAuthedUid,
                "api", "1",
                "state",idToken };

        final String url = DbxRequestUtil.buildUrlWithParams(locale.toString(), "www.dropbox.com", path, params);

        
		view.getSettings().setJavaScriptEnabled(true);
		
		ad.setTitle(getStringResource(R.string.dropbox_authorization_dialog_title));
		
		ad.setView(fl);
		
		ad.setOnShowListener(new DialogInterface.OnShowListener() {
			
			@Override
			public void onShow(DialogInterface dialog) {
				view.loadUrl(url);
				view.requestFocus();
			}
		});
		ad.show();
	}
	
	// Code snippet specific Functions
	
	public LinkedHashMap<String, String> getAllCodeSnippets()
	{
		String codeSpec = getCodeSnippetsRaw();
		String codeDelimiter = getCodeSnippetDelimiter();
		
		String[] snippets = StringUtils.splitByWholeSeparator(codeSpec, codeDelimiter);
		_codeSnippetMap = new LinkedHashMap<String, String>();
		String[] pair = null;
		String label = null;
		String code = null;
		String pairDelimiter = getCodeLabelDelimiter();
		for (String keyCodePair:snippets)
		{
			pair = StringUtils.splitByWholeSeparator(keyCodePair, pairDelimiter);
			label = pair[0].trim();
			code = pair[1].trim();
			_codeSnippetMap.put(label, code);
		}
		
		return _codeSnippetMap;
	}
	
	
	public String getCodeSnippetDelimiter()
	{
		return getStringResource(R.string.code_snippets_delimiter);
	}
	
	public String getCodeLabelDelimiter()
	{
		return getStringResource(R.string.code_snippets_label_delimiter);
	}
	
	public void saveCodeSnippet(String name, String code)
	{
		String snippetsRaw = null;
		if (_codeSnippetMap.containsKey(name))
		{
			_codeSnippetMap.put(name, code);
			snippetsRaw = serializeCodeMap(false);
			
		}
		else
		{
			snippetsRaw = getCodeSnippetsRaw();
			if (snippetsRaw.length()>0)
			{
				snippetsRaw += getCodeSnippetDelimiter(); 
			}
			snippetsRaw += name + getCodeLabelDelimiter() + code;
		}
		putString(getCodeSnippetKey(), snippetsRaw);
	}
	
	public void replaceCodeTemplates(LinkedHashMap<String, String> newMap)
	{
		_codeSnippetMap = newMap;
		String snippetsRaw =serializeCodeMap(false);
		putString(getCodeSnippetKey(), snippetsRaw);
	}
	
	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//				General Usage Public Functions
	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	public Handler getMainHandler()
	{
		return _mainHandler;
	}
	
	public String putString(String key, String value)
	{
		_sPreferences.edit().putString(key, value).commit();
		return value;
	}
	
	public String putString(int resourceKey, String value)
	{
		_sPreferences.edit().putString(getStringResource(resourceKey), value).commit();
		return value;
	}
	
	public String getString(String key, String defaultValue)
	{
		return _sPreferences.getString(key, defaultValue);
	}
	
	public String getString(int resourceKey, String defaultValue)
	{
		return _sPreferences.getString(getStringResource(resourceKey), defaultValue);
	}
	
	public boolean getBoolean(String key, boolean defaultValue)
	{
		return _sPreferences.getBoolean(key, defaultValue);
	}
	
	public String getStringResource(int resourceId)
	{
		return _context.getString(resourceId);
	}
	
	
	// ----)(----)(----)(----)(----)(----)(----)(----)(----)(----)(
	//				Private Functions
	// ----)(----)(----)(----)(----)(----)(----)(----)(----)(----)(
	
	
	
	private String serializeCodeMap(boolean toFile)
	{
		StringBuilder codeSpec = new StringBuilder();
		boolean next = false;
		String pairDelimiter = getCodeLabelDelimiter();
		String codeDelimiter = getCodeSnippetDelimiter();
		String separator;
		if (toFile)
			separator = System.getProperty("line.separator", "\n");
		else
			separator = "";
		for (String key:_codeSnippetMap.keySet())
		{
			if (next)
				codeSpec.append(codeDelimiter).append(separator);
			
			codeSpec.append(key).append(separator).append(pairDelimiter).append(separator).append(_codeSnippetMap.get(key)).append(separator);
			
			next = true;
		}
		return codeSpec.toString();
	}
	
	private String getCodeSnippetsRaw()
	{
		return getString(getCodeSnippetKey(), "");
	}
	
	private String getCodeSnippetKey()
	{
		return getStringResource(R.string.pref_key_code_snippets);
	}
	
	private void initialize()
	{
		
		getAllCodeSnippets();
		if (dropboxConfiguredP() && hasDropboxAuthorizationP())
		{
			
			DropboxManager.create(_context, getDropboxAppToken());
		}
	}
	
	private String getDropboxAppKey()
	{
		String dropboxAppKey = getStringResource(R.string.enc_dropbox_app_key);
		if (dropboxAppKey.length()>0)
		{
			String n = encryptDecrypt(dropboxAppKey);
			Log.e( "<><><><><><><> ", "Encrypted app key: " + n);
			return n;
		}
			
		else
			return null;
	}
	
	private String getDropboxAppToken()
	{
		if (hasDropboxAuthorizationP())
		{
			String n = encryptDecrypt(currentAccessToken);
			Log.e( "<><><><><><><> ", "Encrypted token: " + n);
			return n;
		}
		else
			return null;
	}
	
	
	private String encryptDecrypt(String message)
	{
		StringBuilder key = new StringBuilder();
		key.append(r1).append(r2).append(r3).append(r4).append(r5).append(d1).append(d2).append(d3).append(d4).append(d5);
		
		return AITools.encodeDecode(message, getDictionary(), 5, 5, key.toString(), seed);
	}
	
	private String getDictionary()
	{
		StringBuilder out = new StringBuilder();
		for (int i=42;i<=122;i++)
			out.append((char )i);
		return out.toString();
	}
	
	private void saveAppToken(String token)
	{
		putString(getStringResource(R.string.pref_key_dropbox_current_access_token), encryptDecrypt(token));
	}
	
	private String createStateNonce() {
        final int NONCE_BYTES = 16; // 128 bits of randomness.
        byte randomBytes[] = new byte[NONCE_BYTES];
        getSecureRandom().nextBytes(randomBytes);
        StringBuilder sb = new StringBuilder();
        sb.append("oauth2:");
        for (int i = 0; i < NONCE_BYTES; ++i) {
            sb.append(String.format("%02x", (randomBytes[i]&0xff)));
        }
        return sb.toString();
    }
	
	private static SecureRandom getSecureRandom() {
        SecurityProvider prov = sSecurityProvider;
        if (null != prov) {
            return prov.getSecureRandom();
        }
        return new SecureRandom();
    }
	private static SecurityProvider sSecurityProvider = new SecurityProvider() {
        @Override
        public SecureRandom getSecureRandom() {
            return new SecureRandom();
        }
    };
	
}
