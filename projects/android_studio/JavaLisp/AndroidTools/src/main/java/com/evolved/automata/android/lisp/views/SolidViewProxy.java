package com.evolved.automata.android.lisp.views;

import java.util.HashMap;

import com.evolved.automata.lisp.Value;

import android.content.Context;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;


public class SolidViewProxy extends ViewProxy
{
	public SolidViewProxy(Context con, HashMap<String, Value> keymap)
	{
		super(con, keymap);
	}
	
	public View createBaseView()
	{
		return new View(context);
	}
}
