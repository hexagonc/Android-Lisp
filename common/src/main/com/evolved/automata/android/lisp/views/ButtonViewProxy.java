package com.evolved.automata.android.lisp.views;

import java.util.HashMap;

import com.evolved.automata.lisp.Value;
import android.content.Context;

import android.view.View;
import android.widget.Button;



public class ButtonViewProxy extends TextViewProxy
{
	public ButtonViewProxy(Context con, HashMap<String, Value> keymap, String text)
	{
		super(con, keymap, text);
	}
	
	@Override
	public View createBaseView()
	{
		Button tv = new Button(context);
		createBaseView(tv);
		return tv;
	}
	
}
