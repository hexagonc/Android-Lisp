package com.evolved.automata.android.lisp.guibuilder.fragments;



import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;

import org.apache.commons.lang3.StringUtils;

import com.evolved.automata.AITools;
import com.evolved.automata.KeyValuePair;
import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.android.AppStateManager;
import com.evolved.automata.android.lisp.guibuilder.CodeManager;
import com.evolved.automata.android.lisp.guibuilder.DropboxManager;
import com.evolved.automata.android.lisp.guibuilder.GuiBuilderConfiguration;
import com.evolved.automata.android.lisp.guibuilder.MenuManager;
import com.evolved.automata.android.lisp.guibuilder.ProjectManagementView;
import com.evolved.automata.android.lisp.guibuilder.R;
import com.evolved.automata.android.lisp.guibuilder.CodeManager.OnCodeTemplateSelectedListener;
import com.evolved.automata.android.lisp.guibuilder.CodeManager.OnFileSelectedListener;
import com.evolved.automata.android.lisp.guibuilder.CodeManager.PathProtocol;
import com.evolved.automata.android.lisp.guibuilder.DropboxManager.OnFileUploadListener;
import com.evolved.automata.android.lisp.guibuilder.MainActivity.LogType;
import com.evolved.automata.android.lisp.guibuilder.events.ActivityLifeCycleEventListener;
import com.evolved.automata.android.lisp.guibuilder.events.EventManager;
import com.evolved.automata.android.lisp.guibuilder.events.ProjectEventListener;
import com.evolved.automata.android.lisp.guibuilder.events.ToolAreaEventListener;
import com.evolved.automata.android.lisp.guibuilder.toolareas.CodeManagementUIInterface;
import com.evolved.automata.android.lisp.guibuilder.toolareas.CodeTemplateToolAreaFragment;
import com.evolved.automata.android.lisp.guibuilder.toolareas.DropboxToolAreaFragment;
import com.evolved.automata.android.lisp.guibuilder.toolareas.LocalStorageToolAreaFragment;
import com.evolved.automata.android.lisp.guibuilder.toolareas.ToolAreaFragment;
import com.evolved.automata.android.lisp.guibuilder.workspace.CodePage;
import com.evolved.automata.android.lisp.guibuilder.workspace.Project;
import com.evolved.automata.android.lisp.guibuilder.workspace.Workspace;
import com.evolved.automata.android.lisp.views.ViewProxy;
import com.evolved.automata.android.widgets.ShadowButton;
import com.evolved.automata.lisp.Value;

import android.app.ActionBar;
import android.app.Dialog;
import android.app.Fragment;
import android.app.FragmentTransaction;
import android.content.DialogInterface;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.text.InputType;
import android.text.Layout;
import android.util.Log;
import android.view.GestureDetector;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.CompoundButton;
import android.widget.EditText;
import android.widget.ImageButton;
import android.widget.LinearLayout;
import android.widget.RadioButton;
import android.widget.Spinner;
import android.widget.SpinnerAdapter;
import android.widget.TextView;
import android.widget.Toast;

public class CodeEditFragment extends LispBuilderFragment implements CodeManagementUIInterface
{
    RadioButton _dropboxToolAreaSelectRadio;
    RadioButton _codeTemplateToolAreaSourceSelectRadio;
    RadioButton _localStorageToolAreaSourceSelectRadio;
    
    CodeTemplateToolAreaFragment _codeTemplateFragment;
    LocalStorageToolAreaFragment _localStorageFragment;
    DropboxToolAreaFragment _dropboxFragment;
    ToolAreaFragment _currentToolAreaFragment;
    
    
    ViewGroup _cachedView = null;
    ShadowButton _runButton = null;
    ShadowButton _runExpButton = null;
    ShadowButton _renderButton = null;
    ShadowButton _clearScreenButton = null;
    
    
    ShadowButton _historyBackButton = null;
    ShadowButton _historyForwardButton = null;
    
    int _hintTextDuration = 0;
    
    EditText _editText = null;
    EditText _messageText = null;
    TextView _hintText = null;
    TextView _titleText = null;
    String _prompt;
    Value _lastResult;
    String _initialCommand = null;
    String PRIOR_COMMAND_KEY = "PRIOR_COMMAND_KEY";
    SpinnerAdapter _spinnerAdapter;
    ArrayList<KeyValuePair<String, String> > _codeSamples = new ArrayList<>();
    int _initiallySelectedItem = 0;
    boolean _first = true;
    Handler _main = new Handler(Looper.getMainLooper());
    public final String _PREFIX_COMMENT_TOKEN = ";";
    boolean isReadOnlyP = false;
    int _cursorPosition = -1;
    ProjectEventListener _projectEventListener;
    ToolAreaEventListener _toolAreaNotifier;
    GestureDetector _gestureDetector;
    @Override
    public void onCreate(Bundle savedInstanceState) {
        
        super.onCreate(savedInstanceState);
        _gestureDetector = new GestureDetector(this.getActivity(), _gestureDetectorListener, _main);
        _projectEventListener = new ProjectEventListener()
        {

			@Override
			public void currentCodePageDeleted(String newPageTitle,
					String newPageText, boolean hasPrevious, boolean hasNext) {
				_titleText.setText(newPageTitle);
				_editText.setText(newPageText);
				if (hasPrevious)
					_historyBackButton.setVisibility(View.VISIBLE);
				else
					_historyBackButton.setVisibility(View.INVISIBLE);
				
				if (hasNext)
					_historyForwardButton.setVisibility(View.VISIBLE);
				else
					_historyForwardButton.setVisibility(View.INVISIBLE);
			}
        	
        };
        EventManager.getInstance().setProjectEventListener(_projectEventListener);
        
        _toolAreaNotifier = EventManager.getInstance().getToolAreaEventNotifier();
        
        _prompt = getActivity().getString(R.string.console_prompt);
        if (_first)
        {
            //configureCodeDropdown();
            _first = false;
        }
        
        _codeTemplateFragment = new CodeTemplateToolAreaFragment(this);
        
        _localStorageFragment = new LocalStorageToolAreaFragment(this);
        
        _dropboxFragment = new DropboxToolAreaFragment(this);
        MenuManager.get().setUIInterface(this);
    }
    

    
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
            Bundle savedInstanceState) {
        
        if (_cachedView == null)
        {
            LinearLayout layout  = (LinearLayout)inflater.inflate(R.layout.lisp_edit_layout, container, false);
            _cachedView = layout;
            configureView();
        }
        return _cachedView;
        
    }

    @Override
    public void onStart() {
        
        super.onStart();
        String prior = AndroidTools.getStringPreferenceSetting(PRIOR_COMMAND_KEY, null);
        if (prior != null)
            _editText.setText(prior);
        
        MenuManager.get().setCodeEditorMenuMode();
    }

    @Override
    public void onResume() {
    
        super.onResume();
        Project proj = CodeManager.get().getCurrentProject();
		if (proj != null)
		{
			updateHistoryButtonEnability(proj);
		}
    }

    @Override
    public void onPause() {
        
        super.onPause();
    }

    @Override
    public void onStop() {
        String command = _editText.getText().toString();
        AndroidTools.setStringPreferenceSetting(PRIOR_COMMAND_KEY, command);
        super.onStop();
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        _cachedView = null;
        MenuManager.get().setUIInterface(null);
    }
    
    
    private void attachDropboxToolArea()
    {
        if (_currentToolAreaFragment != _dropboxFragment)
        {
            FragmentTransaction ft = getFragmentManager().beginTransaction();
            ft.replace(R.id.frm_tool_area_fragment_container, _dropboxFragment);
            ft.commit();
            _currentToolAreaFragment = _dropboxFragment;
            _toolAreaNotifier.dropboxToolsSelected();
        }
    }
    
    private void attachCodeTemplateToolArea()
    {
        if (_currentToolAreaFragment != _codeTemplateFragment)
        {
            FragmentTransaction ft = getFragmentManager().beginTransaction();
            ft.replace(R.id.frm_tool_area_fragment_container, _codeTemplateFragment);
            ft.commit();
            _currentToolAreaFragment = _codeTemplateFragment;
            _toolAreaNotifier.codeTemplateToolsSelected();
        }
    }
    
    private void attachLocalStorageToolArea()
    {
        if (_currentToolAreaFragment != _localStorageFragment)
        {
            FragmentTransaction ft = getFragmentManager().beginTransaction();
            ft.replace(R.id.frm_tool_area_fragment_container, _localStorageFragment);
            ft.commit();
            _currentToolAreaFragment = _localStorageFragment;
            _toolAreaNotifier.localStorageToolsSelected();
        }
    }
    
    private enum ToolAreaType
    {
        CODE_TEMPLATE,
        LOCAL_STORAGE,
        DROPBOX;
    }
    
    private ToolAreaType getLastClickedToolAreaType()
    {
        GuiBuilderConfiguration config = GuiBuilderConfiguration.get();
        String prev = config.getString(config.getStringResource(R.string.pref_key_last_clicked_toolarea_type), ToolAreaType.CODE_TEMPLATE.name());
        return ToolAreaType.valueOf(prev);
    }
    
    private void setLastClickedToolAreaType(ToolAreaType type)
    {
        GuiBuilderConfiguration config = GuiBuilderConfiguration.get();
        config.putString(config.getStringResource(R.string.pref_key_last_clicked_toolarea_type), type.name());
    }
    
    boolean first = true;
    private void configureToolArea()
    {
    	ToolAreaType type = getLastClickedToolAreaType();
        switch (type)
        {
            case LOCAL_STORAGE:
                attachLocalStorageToolArea();
                break;
            case CODE_TEMPLATE:
                attachCodeTemplateToolArea();
                break;
            case DROPBOX:
                attachDropboxToolArea();
                break;
        }
        LinearLayout dropboxSourceContainer = (LinearLayout)_cachedView.findViewById(R.id.lin_dropbox_source_container);
        _dropboxToolAreaSelectRadio = (RadioButton)_cachedView.findViewById(R.id.rb_select_source_dropbox);
        _dropboxToolAreaSelectRadio.setChecked(type == ToolAreaType.DROPBOX);
        _dropboxToolAreaSelectRadio.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
            
            @Override
            public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
            	if (first)
            		return;
                if (isChecked && getLastClickedToolAreaType()!=ToolAreaType.DROPBOX)
                {
                    _codeTemplateToolAreaSourceSelectRadio.setChecked(false);
                    _localStorageToolAreaSourceSelectRadio.setChecked(false);
                    attachDropboxToolArea();
                    setLastClickedToolAreaType(ToolAreaType.DROPBOX);
                }
            }
        });
        if (GuiBuilderConfiguration.get().dropboxConfiguredP())
        	dropboxSourceContainer.setVisibility(View.VISIBLE);
        else
        	dropboxSourceContainer.setVisibility(View.GONE);
        
        _codeTemplateToolAreaSourceSelectRadio = (RadioButton)_cachedView.findViewById(R.id.rb_select_source_code_template);
        _codeTemplateToolAreaSourceSelectRadio.setChecked(type == ToolAreaType.CODE_TEMPLATE);
        _codeTemplateToolAreaSourceSelectRadio.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
            
            @Override
            public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
            	if (first)
            		return;
                if (isChecked && getLastClickedToolAreaType()!=ToolAreaType.CODE_TEMPLATE)
                {
                    _dropboxToolAreaSelectRadio.setChecked(false);
                    _localStorageToolAreaSourceSelectRadio.setChecked(false);
                    attachCodeTemplateToolArea();
                    setLastClickedToolAreaType(ToolAreaType.CODE_TEMPLATE);
                }
            }
        });

        _localStorageToolAreaSourceSelectRadio = (RadioButton)_cachedView.findViewById(R.id.rb_select_source_local_storage);
        _localStorageToolAreaSourceSelectRadio.setChecked(type == ToolAreaType.LOCAL_STORAGE);
        _localStorageToolAreaSourceSelectRadio.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
            
            @Override
            public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
            	if (first)
            		return;
                if (isChecked && getLastClickedToolAreaType()!=ToolAreaType.LOCAL_STORAGE)
                {
                    _dropboxToolAreaSelectRadio.setChecked(false);
                    _codeTemplateToolAreaSourceSelectRadio.setChecked(false);
                    attachLocalStorageToolArea();
                    setLastClickedToolAreaType(ToolAreaType.LOCAL_STORAGE);
                }
            }
        });
        
        
        first = false;
        
    }
    
    
    private void configureView()
    {
    	_cursorPosition = -1;        
        
        configureToolArea();
        _titleText = (TextView)_cachedView.findViewById(R.id.txt_file_title);
        _runButton = (ShadowButton)_cachedView.findViewById(R.id.but_run);
        _runButton.setOnClickListener(new View.OnClickListener() {
            
            @Override
            public void onClick(View v) {
                String command = _editText.getText().toString();
                String commandWithoutComments = AITools.stripPrefixDelimitedComments(command, _PREFIX_COMMENT_TOKEN);
                if (command != null && command.length()>0)
                {
                    Value result = _data.getInterpreter().evaluateExpression(commandWithoutComments, false);
                    if (result != null)
                    {
                        onResult(result);
                    }
                }
            }
        });
        
        _runExpButton= (ShadowButton)_cachedView.findViewById(R.id.but_run_expr);
        _runExpButton.setOnClickListener(new View.OnClickListener() {
            
            @Override
            public void onClick(View v) {
                String command = _editText.getText().toString();
               
                if (command != null && command.length()>0)
                {
                    int cursor = _editText.getSelectionStart();
                    evaluateCursorExpression(command, cursor);
                }
            }
        });
        
        
        
        _renderButton = (ShadowButton)_cachedView.findViewById(R.id.but_render);
        _renderButton.setOnClickListener(new View.OnClickListener() {
            
            @Override
            public void onClick(View v) {
                if (_lastResult != null && _lastResult.getObjectValue() instanceof ViewProxy)
                {
                    _data.setViewProxy((ViewProxy)_lastResult.getObjectValue());
                    _data.switchToRenderTab();
                }
            }
        });
        
        _clearScreenButton = (ShadowButton)_cachedView.findViewById(R.id.but_clear);
        _clearScreenButton.setOnClickListener(new View.OnClickListener() {
            
            @Override
            public void onClick(View v) {
                _editText.setText("");
                setCodeTitle(GuiBuilderConfiguration.get().getStringResource(R.string.undefined_code_page_label));
                CodeManager.get().clearLastLoadedFileUrl();
            }
        });
        

        _editText = (EditText)_cachedView.findViewById(R.id.edit_code_view);
        _messageText = (EditText)_cachedView.findViewById(R.id.edit_console_output);
        _editText.setHorizontallyScrolling(true);
        _editText.setHorizontalScrollBarEnabled(true);
        _editText.setVerticalScrollBarEnabled(true);
//        _editText.setScrollBarStyle(TextView.SCROLLBARS_INSIDE_OVERLAY);
        _hintText = (TextView)_cachedView.findViewById(R.id.txt_hint_text);     
        
        _historyBackButton = (ShadowButton)_cachedView.findViewById(R.id.but_history_back);
        _historyForwardButton = (ShadowButton)_cachedView.findViewById(R.id.but_history_forward);
        
        _historyForwardButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				Project proj = CodeManager.get().getCurrentProject();
				if (proj != null)
				{
					_retainScroll = false;
					KeyValuePair<String, String> kv = proj.gotoNextPage();
					updateCodeDisplay(kv);
					updateHistoryButtonEnability(proj);
				}
			}
		});
        
        _historyBackButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				Project proj = CodeManager.get().getCurrentProject();
				if (proj != null)
				{
					_retainScroll = false;
					KeyValuePair<String, String> kv = proj.gotoPrevPage();
					updateCodeDisplay(kv);
					updateHistoryButtonEnability(proj);
				}
				
			}
		});
        
        Project proj = CodeManager.get().getCurrentProject();
		if (proj != null)
		{
			updateHistoryButtonEnability(proj);
			
			String url;
			if (proj.hasCurrentPage() && (url = proj.getCurrentPage().GetKey())!=null)
			{
				setCodeTitle(CodeManager.get().getShortFileNameFromPathUrl(url));
			}
			else
				setCodeTitle(GuiBuilderConfiguration.get().getStringResource(R.string.undefined_code_page_label));
		}
		else
		{
			String url = CodeManager.get().getLastLoadedFileUrl();
			if (url != null)
				setCodeTitle(CodeManager.get().getShortFileNameFromPathUrl(url));
		}
    }
    
    
    private void evaluateCursorExpression(String text, int cursorStart)
    {
    	
    	try
    	{
    		int stack = 1;
        	
        	int endIndex = 0;
        	for (endIndex = cursorStart;endIndex<text.length();endIndex++)
        	{
        		if (text.charAt(endIndex) == ')')
        		{
        			stack--;
        		}
        		else if (text.charAt(endIndex) == '(')
        		{
        			stack++;
        		}
        		if (stack == 0)
        			break;
        	}
        	if (endIndex == text.length())
        		endIndex--;
        	
        	int start = cursorStart;
        	for (start= cursorStart; start>=0;start--)
        	{
        		if (text.charAt(start) == '(')
        			break;
        	}
        	
        	Value result = _data.getInterpreter().evaluateExpression(text.substring(start, endIndex+1), false);
            if (result != null)
            {
            	onResultWithoutHistory(result);
            }
    	}
    	catch (Exception e)
    	{
    		onError(e);
    	}
    	
    	
    }

    @Override
    public void onError(Exception e) {
        appendResult(e.toString());
        log(LogType.ERROR, e.toString());
    }
    
    @Override
    public void onResult(Value v)
    {
        if (v != null)
        {
            if (v.getObjectValue() instanceof ViewProxy)
            {
                _lastResult = v;
            }
            String svalue = v.toString();
            
            log(LogType.INFO, svalue);
            appendResult(svalue);
            updateHistoryWithResult();
        }
        else
            appendResult("null");
    }
    
    
    public void onResultWithoutHistory(Value v)
    {
        if (v != null)
        {
            if (v.getObjectValue() instanceof ViewProxy)
            {
                _lastResult = v;
            }
            String svalue = v.toString();
            
            log(LogType.INFO, svalue);
            appendResult(svalue);
            
        }
        else
            appendResult("null");
    }
    
    private void updateHistoryWithResult()
    {
    	
        String code = _editText.getText().toString();
        String lastLoadedUrl = CodeManager.get().getLastLoadedFileUrl();
        
        Project proj = CodeManager.get().getCurrentProject();
        
        if (proj != null)
        {
        	proj.updateLastEvaluatedCode(lastLoadedUrl, code);
        	updateHistoryButtonEnability(proj);
        }
    	
    }
    
    private void updateDisplayWithCurrentProjectPage()
    {
    	Project proj = CodeManager.get().getCurrentProject();
        
        if (proj != null)
        {
        	updateHistoryButtonEnability(proj);
        	CodePage page = proj.getCurrentCodePage();
        	updateCodeDisplay(page);
        	updateHistoryButtonEnability(proj);
        }
        
        	
    	
    }
    
    private void updateCodeDisplay(CodePage page)
    {
    	if (page != null)
    	{
    		String fileUrl = page.getFileUrl();
        	String code = page.getCode();
        	
        	if (fileUrl != null)
        	{
        		setCodeTitle(CodeManager.get().getShortFileNameFromPathUrl(fileUrl));
        		CodeManager.get().setLastLoadedFileUrl(fileUrl);
        	}
        	else
        	{
        		setCodeTitle(GuiBuilderConfiguration.get().getStringResource(R.string.undefined_code_page_label));
        	}
    		replaceCodeEdit(code);
    	}
    	else
    	{
    		setCodeTitle(GuiBuilderConfiguration.get().getStringResource(R.string.undefined_code_page_label));
    		replaceCodeEdit("");
    	}
    }
    
    
    private void updateCodeDisplay(KeyValuePair<String, String> codeSpec)
    {
    	if (codeSpec != null)
    	{
    		String fileUrl = codeSpec.GetKey();
        	String code = codeSpec.GetValue();
        	
        	if (fileUrl != null)
        	{
        		setCodeTitle(CodeManager.get().getShortFileNameFromPathUrl(fileUrl));
        		CodeManager.get().setLastLoadedFileUrl(fileUrl);
        	}
        	else
        	{
        		setCodeTitle(GuiBuilderConfiguration.get().getStringResource(R.string.undefined_code_page_label));
        	}
    		replaceCodeEdit(code);
    		
    		
    	}
    }
    
    private void updateHistoryButtonEnability(Project proj)
    {
    	if (proj.getCurrentPage()!=null)
    	{
    		if (proj.hasNextPage())
    			_historyForwardButton.setVisibility(View.VISIBLE);
    		else
    			_historyForwardButton.setVisibility(View.INVISIBLE);
    		
    		if (proj.hasPrevPage())
    			_historyBackButton.setVisibility(View.VISIBLE);
    		else
    			_historyBackButton.setVisibility(View.INVISIBLE);
    	
    	}
    	else
    	{
    		_historyBackButton.setVisibility(View.INVISIBLE);
    		_historyForwardButton.setVisibility(View.INVISIBLE);
    	}
    }
    
    
    private void log(String msg)
    {
        log(LogType.VERBOSE, msg);
    }
    
    private void log(LogType type, String msg)
    {
        switch (type)
        {
            case INFO:
                Log.i("MainActivity", msg);
                break;
            case ERROR:
                Log.e("MainActivity", msg);
                break;
            default:
                Log.v("MainActivity", msg);
                break;
        }
    }
    
    
    private void appendResult(String value)
    {
        String newText = _messageText.getText().toString() + "\n" + _prompt + value;
        
        _messageText.setText(newText);
        _messageText.setSelection(newText.length(), newText.length());
        
    }


    // Called on a background thread
    @Override
    public void onOutput(final Value v) {
        _messageText.post(new Runnable(){
            public void run()
            {
                if (v != null)
                {
                    if (v.getObjectValue() instanceof ViewProxy)
                    {
                        _lastResult = v;
                    }
                    String svalue = v.toString();
                    log(LogType.INFO, svalue);
                    appendResult(svalue);
                }
                else
                    appendResult("null");
            }
        });
    }



    @Override
    public void onIncompleteInputException(String message) {
        appendResult(message);
        log(LogType.ERROR, message);
    }



    @Override
    public void onGeneralException(Throwable e) {
        appendResult(e.toString());
        log(LogType.ERROR, e.toString());
    }



    @Override
    public String getHighlightedText() {
        int start = _editText.getSelectionStart();
        int end = _editText.getSelectionEnd();
        String oldText = _editText.getText().toString();
        return oldText.substring(start, end);
    }


    boolean _retainScroll = true; 
    @Override
    public void replaceCodeEdit(String newCode) {
    	
    	if (_retainScroll)
    		_cursorPosition = _editText.getScrollY();
    	//if (<0)
    	//	_cursorPosition = Math.min(_editText.getSelectionStart(), newCode.length());
    	//else
    	//	_cursorPosition = Math.min(_cursorPosition, newCode.length());
        _editText.setText(newCode);
        if (_retainScroll)
        	_editText.setScrollY(_cursorPosition);
        
        _retainScroll = true;
    }



    @Override
    public void insertCodeAtEditCursor(String code) {
        int start = _editText.getSelectionStart();
        int end = _editText.getSelectionEnd();
        String oldText = _editText.getText().toString();
        String newtext = oldText.substring(0, start) + code + oldText.substring(end);
        _editText.setText(newtext);
    }
    
    @Override
    public void setCodeTitle(String title)
    {
        _titleText.setText(title);
    }
    
    @Override
    public void onError(String message, Exception e)
    {
        Toast.makeText(getActivity(), message, Toast.LENGTH_LONG).show();
        AppStateManager.getInstance().onError("ToolArea", e);
    }



    @Override
    public String getCurrentLispEditorCode() {
        // 
        return _editText.getText().toString().trim();
    }
    
    @Override
    public synchronized void showHintText(final String text, final int duration)
    {
        _hintTextDuration += duration;
        final Runnable resetRunnable =  new Runnable()
        {
            
            public void run()
            {
                _hintTextDuration-=duration;
                if (_hintTextDuration == 0)
                    _hintText.setText("");
            }
        };
        
        Runnable startRunnable = new Runnable()
        {
            public void run()
            {
                _hintText.setText(text);
                GuiBuilderConfiguration.get().getMainHandler().postDelayed(resetRunnable, duration);
            }
        };
        GuiBuilderConfiguration.get().getMainHandler().post(startRunnable);
    }
    
    @Override
    public void showHintText(final String text)
    {
        Runnable startRunnable = new Runnable()
        {
            public void run()
            {
                _hintText.setText(text);
                
            }
        };
        GuiBuilderConfiguration.get().getMainHandler().post(startRunnable);
    }



	@Override
	public void saveCurrentCodeEditorToDropbox() {
		final OnFileUploadListener uploadListener = new OnFileUploadListener() {
			
			@Override
			public void uploaded(String filename) {
				
			}
			
			@Override
			public void onError(String message, Exception e) {
				onError(message, e);
			}
		};
		
		// TODO: implement a save-as function and allow saving brand new files
		String lastFileUrl = CodeManager.get().getLastLoadedFileUrl();
		if (lastFileUrl!=null && CodeManager.get().getPathProtocol(lastFileUrl) == CodeManager.PathProtocol.DROPBOX)
		{
			DropboxManager.get().resynchLastDownloadedDropboxFile(getCurrentLispEditorCode(), uploadListener);
			
		}
	}



	@Override
	public void saveCurrentCodeEditorToLocalStorage() {
		final OnFileSelectedListener listener = new OnFileSelectedListener()
		{

			@Override
			public void onFileSelected(String fullPath, String fileContents,
					PathProtocol protocol, boolean replaceEditor) {
				
				String newContents = getCurrentLispEditorCode();
				FileOutputStream fistream = null;
				try
				{
					
					fistream = new FileOutputStream(fullPath, false);
					fistream.write(newContents.getBytes(Charset.forName("UTF-8")));
					
					CodeManager.get().setLastLoadedFileUrl(fullPath, protocol);
					CodeManager.get().setLastLoadedLocalStorageFile(fullPath);
					String[] parts = StringUtils.split(fullPath, "/");
					setCodeTitle(parts[parts.length-1]);
				}
				catch (Exception e)
				{
					onError("Error saving file: " + fullPath,e);
				}
				finally
				{
					if (fistream != null)
						try {
							fistream.close();
						} catch (IOException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
				}
				
					
			}

			@Override
			public void onError(String message, Exception e) {
				onError(message, e);
			}
			
		};
	
		CodeManager.get().showLocalStorageFileSelectDialog(getActivity(), listener, true);
	}



	@Override
	public void loadCurrentCodeEditorFromLocalStorage() {
		final OnFileSelectedListener listener = new OnFileSelectedListener()
		{

			@Override
			public void onFileSelected(String fullPath, String fileContents,
					PathProtocol protocol, boolean replaceEditor) {
				if (replaceEditor)
					replaceCodeEdit(fileContents);
				else
					insertCodeAtEditCursor(fileContents);
				
				String url = CodeManager.get().getLastLoadedFileUrl();
				
				String filenameshort = CodeManager.get().getShortFileNameFromPathUrl(url);
				setCodeTitle(filenameshort);
					
			}

			@Override
			public void onError(String message, Exception e) {
				onError(message, e);
			}
			
		};
		
		CodeManager.get().showLocalStorageFileSelectDialog(getActivity(), listener, false);
	}



	@Override
	public void loadCurrentCodeEditorFromDropbox() {
		final GuiBuilderConfiguration.OnDropboxAuthorizedListener listener = new GuiBuilderConfiguration.OnDropboxAuthorizedListener()
		{
			
			
			@Override
			public void onAuthorized() 
			{
				CodeManager.get().showDropboxFileSelectDialog(getActivity(), getFileSelectedListener(), false);
			}
			
		};
		
		if (GuiBuilderConfiguration.get().hasDropboxAuthorizationP())
		{
			CodeManager.get().showDropboxFileSelectDialog(getActivity(), getFileSelectedListener(), false);
		}
		else
		{
			GuiBuilderConfiguration.get().showDropboxLoginDialog(getActivity(), listener);
		}
		
	}
 
	
	// separate
	
	protected OnFileSelectedListener getFileSelectedListener()
	{
		return new OnFileSelectedListener()
		{

			@Override
			public void onFileSelected(String path, String fileContents,
					PathProtocol protocol, boolean replaceEditor) {
				if (replaceEditor)
				{
					replaceCodeEdit(fileContents);
				}
				else
					insertCodeAtEditCursor(fileContents);
				String previousUri = CodeManager.get().getLastLoadedFileUrl();
				setCodeTitle(CodeManager.get().getShortFileNameFromPathUrl(previousUri));
				
				
			}

			@Override
			public void onError(String message, Exception e) {
				onError(message, e);
			}
			
		};
	}



	@Override
	public void showTemplateCreateDialog() {
		String code = getHighlightedText();
		if (code!=null && code.trim().length()>0)
		{
			CodeManager.get().showCodeTemplateCreateDialog(getActivity(), code);
		}
		else
			showHintText("Can't create a blank code template", 7000);
		
	}



	@Override
	public void showTemplateSelectDialog() {
		final OnCodeTemplateSelectedListener listener = new OnCodeTemplateSelectedListener()
		{

			@Override
			public void templateSelected(String template, boolean replaceEditor) {
				if (replaceEditor)
					replaceCodeEdit(template);
				else
					insertCodeAtEditCursor(template);
			}
			
		};
		
		CodeManager.get().showCodeTemplateSelectDialog(getActivity(), listener);
	}
	
	@Override
	public void showProjectManagerDialog()
	{
		final Dialog d = new Dialog(getActivity());
		d.setTitle("Manage Projects");
		d.setContentView(R.layout.project_management_dialog_container);
		ProjectManagementView proj = (ProjectManagementView)d.findViewById(R.id.proj_management_container);
		proj.setManagementListener(new ProjectManagementView.ProjectSelectedListener() {
			
			@Override
			public void onSelected(String projectName) {
				Workspace w = CodeManager.get().getWorkspace();
				w.switchCurrentProject(projectName);
				updateDisplayWithCurrentProjectPage();
				
				d.dismiss();
			}
			
			@Override
			public void onCancelled() {
				d.dismiss();
			}
		});
		d.setOnDismissListener(new DialogInterface.OnDismissListener() {
			
			@Override
			public void onDismiss(DialogInterface dialog) {
				ActionBar bar = getActivity().getActionBar();
				Workspace w = CodeManager.get().getWorkspace();
				Project proj = w.getCurrentProject();
				bar.setTitle(proj.getName());
			}
		});
		d.show();
	}
	
	
	@Override
	public void toggleEditorReadOnlyStatus(boolean readOnly)
	{
		isReadOnlyP = readOnly;
		updateEditorReadOnlyStatus();
	}
	
	
	
	GestureDetector.OnGestureListener getGestureListener()
	{
		return new GestureDetector.OnGestureListener() {
			
			@Override
			public boolean onSingleTapUp(MotionEvent e) {
				// TODO Auto-generated method stub
				return false;
			}
			
			@Override
			public void onShowPress(MotionEvent e) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public boolean onScroll(MotionEvent e1, MotionEvent e2, float distanceX,
					float distanceY) {
				_editText.scrollBy((int)distanceX, (int)distanceY);
				return true;
			}
			
			@Override
			public void onLongPress(MotionEvent e) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public boolean onFling(MotionEvent e1, MotionEvent e2, float velocityX,
					float velocityY) {
				// TODO Auto-generated method stub
				return false;
			}
			
			@Override
			public boolean onDown(MotionEvent e) {
				// TODO Auto-generated method stub
				return false;
			}
		};
	}
	
	
	GestureDetector.OnGestureListener _gestureDetectorListener = getGestureListener();
	int offset = -1;
	private void updateEditorReadOnlyStatus()
	{
		if (isReadOnlyP)
		{
			//
			//_editText.setInputType(InputType.);
			//_editText.setFocusable(false);
			
			_editText.setOnTouchListener(new View.OnTouchListener() {

				@Override
				public boolean onTouch(View v, MotionEvent event) {
					_gestureDetector.onTouchEvent(event);
				    switch (event.getAction()) {
				      case MotionEvent.ACTION_UP:
				          Layout layout = ((EditText) v).getLayout();
				          float x = event.getX() + _editText.getScrollX();
				          float y = event.getY() + _editText.getScrollY();            
				          int line = layout.getLineForVertical((int) y);                      
				
				          // Here is what you wanted:
				
				          offset = layout.getOffsetForHorizontal( line,  x);
				
				          _editText.post(new Runnable()
			                {
			                public void run()
			                    {
			                    	_editText.setSelection(offset);
			                    }
			                });
			            
				          break;
				        }
				    return true;
				}

				});
			
			
			
			
		}
		else
		{
			_editText.setOnTouchListener(null);
		}
	}







	@Override
	public void onEnvironmentReset() {
		// TODO Auto-generated method stub
		
	}
	
}
