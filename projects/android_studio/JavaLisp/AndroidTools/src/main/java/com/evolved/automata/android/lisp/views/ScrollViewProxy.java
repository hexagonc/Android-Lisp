package com.evolved.automata.android.lisp.views;

import java.util.HashMap;
import java.util.LinkedList;

import android.view.View;
import android.view.ViewGroup;
import android.widget.RelativeLayout;
import android.widget.ScrollView;
import com.evolved.automata.lisp.Value;

import android.content.Context;

public class ScrollViewProxy extends ViewGroupProxy 
{
	
	
	public ScrollViewProxy(Context con, HashMap<String, Value> keywords)
	{
		super(con, keywords);
		
	}

	@Override
	public ViewGroup getPreconfiguredView()
	{
		ScrollView layout = new ScrollView(context);
		return layout;
		
	}
	
	
	
}
