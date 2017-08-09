package com.evolved.automata.android.lisp.views;

import java.util.HashMap;

import android.content.Context;
import android.graphics.Color;
import android.graphics.Typeface;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;


import com.evolved.automata.android.EvaluateException;

import com.evolved.automata.android.tools.R;
import com.evolved.automata.android.widgets.ShadowButton;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;

public class ShadowButtonProxyNew extends TextViewProxy
{

	String text = null;
	int textColor;


	
	public ShadowButtonProxyNew(Context con, HashMap<String, Value> keymap, String text)
	{
		super(con, keymap, text);

	}
	

	


	
	@Override
	public View createBaseView()
	{
		Button tv = new Button(context);
		createBaseView(tv);
		tv.setShadowLayer(5, 10, 10, Color.parseColor("#8D8D8D"));
        tv.setBackgroundResource(R.drawable.shadow_button_background);
		return tv;
	}
}
