package com.evolved.automata.android.lisp.views;

import java.util.HashMap;

import com.evolved.automata.lisp.Value;

import android.content.Context;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.KeyEvent;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;


public class EditViewProxy extends TextViewProxy
{
	public static final String HINT_TEXT = ":hint"; // text hint


	public EditViewProxy(Context con, HashMap<String, Value> keymap, String text)
	{
		super(con, keymap, text);
	}
	
	protected void processHintTextFromKeywords(HashMap<String, Value> keymap, EditText edit)
	{
		Value hint = getMapValue(keymap, HINT_TEXT);
		if (!hint.isNull() && hint.isString())
		{
			edit.setHint(hint.getString());
		}
	}
	
	@Override
	public View createBaseView()
	{
		EditText tv =
        new EditText(context) {
            OnEditorActionListener _listener = new OnEditorActionListener() {
                @Override
                public boolean onEditorAction(TextView textView, int i, KeyEvent keyEvent)
                {
                    if (keyEvent.getKeyCode() == KeyEvent.KEYCODE_BACK){
                        clearFocus();
                    }
                    return false;
                }
            };

            {
                setOnEditorActionListener(_listener);
            }

            @Override
            public boolean onKeyPreIme(int keyCode, KeyEvent event) {
                if (event.getKeyCode() == KeyEvent.KEYCODE_BACK){
                    clearFocus();
                }
                return super.onKeyPreIme(keyCode, event);
            }


        };
		createBaseView(tv);
		processHintTextFromKeywords(_keys, tv);

		tv.addTextChangedListener(new TextWatcher() {
			@Override
			public void beforeTextChanged(CharSequence charSequence, int i, int i1, int i2)
			{

			}

			@Override
			public void onTextChanged(CharSequence charSequence, int i, int i1, int i2)
			{

			}

			@Override
			public void afterTextChanged(Editable editable)
			{
				text = editable.toString();
			}
		});
		return tv;
	}
	
	@Override
	public void applyAttribures(HashMap<String, Value> keywords)
	{
		super.applyAttribures(keywords);
		View actual;
		if (encapsulated != null && (actual = encapsulated.get())!= null)
			processHintTextFromKeywords(_keys, (EditText)actual);
	}
}
