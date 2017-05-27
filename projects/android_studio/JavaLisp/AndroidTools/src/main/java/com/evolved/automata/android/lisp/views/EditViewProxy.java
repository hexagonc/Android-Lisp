package com.evolved.automata.android.lisp.views;

import java.util.HashMap;

import com.evolved.automata.lisp.Value;

import android.content.Context;
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
		EditText tv = new EditText(context);
		createBaseView(tv);
		processHintTextFromKeywords(_keys, tv);
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
