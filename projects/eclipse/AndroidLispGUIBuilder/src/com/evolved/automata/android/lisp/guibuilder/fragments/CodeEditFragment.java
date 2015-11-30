package com.evolved.automata.android.lisp.guibuilder.fragments;



import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;

import com.evolved.automata.AITools;
import com.evolved.automata.KeyValuePair;
import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.android.AppStateManager;
import com.evolved.automata.android.lisp.guibuilder.CodeManager;
import com.evolved.automata.android.lisp.guibuilder.GuiBuilderConfiguration;
import com.evolved.automata.android.lisp.guibuilder.R;
import com.evolved.automata.android.lisp.guibuilder.MainActivity.LogType;
import com.evolved.automata.android.lisp.guibuilder.toolareas.CodeManagementUIInterface;
import com.evolved.automata.android.lisp.guibuilder.toolareas.CodeTemplateToolAreaFragment;
import com.evolved.automata.android.lisp.guibuilder.toolareas.DropboxToolAreaFragment;
import com.evolved.automata.android.lisp.guibuilder.toolareas.LocalStorageToolAreaFragment;
import com.evolved.automata.android.lisp.guibuilder.toolareas.ToolAreaFragment;
import com.evolved.automata.android.lisp.views.ViewProxy;
import com.evolved.automata.android.widgets.ShadowButton;
import com.evolved.automata.lisp.Value;

import android.app.Fragment;
import android.app.FragmentTransaction;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.view.LayoutInflater;
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
    ShadowButton _renderButton = null;
    ShadowButton _clearScreenButton = null;
    
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
    
    @Override
    public void onCreate(Bundle savedInstanceState) {
        
        super.onCreate(savedInstanceState);
        
        _prompt = getActivity().getString(R.string.console_prompt);
        if (_first)
        {
            //configureCodeDropdown();
            _first = false;
        }
        
        _codeTemplateFragment = new CodeTemplateToolAreaFragment(this);
        
        _localStorageFragment = new LocalStorageToolAreaFragment(this);
        
        _dropboxFragment = new DropboxToolAreaFragment(this);
        
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
        
        String priorFilePath = CodeManager.get().getLastLoadedFileUrl();
        if (priorFilePath != null)
        {
        	String shortName = CodeManager.get().getShortFileNameFromPathUrl(priorFilePath);
        	setCodeTitle(shortName);
        }
    }

    @Override
    public void onResume() {
    
        super.onResume();
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
    }
    
    
    private void attachDropboxToolArea()
    {
        if (_currentToolAreaFragment != _dropboxFragment)
        {
            FragmentTransaction ft = getFragmentManager().beginTransaction();
            ft.replace(R.id.frm_tool_area_fragment_container, _dropboxFragment);
            ft.commit();
            _currentToolAreaFragment = _dropboxFragment;
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
                setCodeTitle("");
                CodeManager.get().clearLastLoadedFileUrl();
            }
        });
        
        // TODO: Consider adding a separate but to start a new file
        _editText = (EditText)_cachedView.findViewById(R.id.edit_code_view);
        _messageText = (EditText)_cachedView.findViewById(R.id.edit_console_output);
        _editText.setHorizontallyScrolling(true);
        _editText.setHorizontalScrollBarEnabled(true);
        _editText.setVerticalScrollBarEnabled(true);
        
        _hintText = (TextView)_cachedView.findViewById(R.id.txt_hint_text);     
        
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
        }
        else
            appendResult("null");
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



    @Override
    public void replaceCodeEdit(String newCode) {
        _editText.setText(newCode);
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
    
}
