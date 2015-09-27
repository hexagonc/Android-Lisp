package com.evolved.automata.android.lisp.views;

import java.util.HashMap;
import java.util.LinkedList;

import com.evolved.automata.lisp.Value;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;


public class RelativeLayoutViewProxy extends ViewGroupProxy
{
	
	
	public RelativeLayoutViewProxy(Context con, HashMap<String, Value> keymap)
	{
		super(con, keymap);
		
	}
	
	
	@Override
	public ViewGroup getPreconfiguredView()
	{
		
		RelativeLayout layout = new RelativeLayout(context);
		return layout;
	}
	
	
}
