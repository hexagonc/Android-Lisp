package com.evolved.automata.android.lisp.views;

import java.util.HashMap;

import com.evolved.automata.lisp.Value;

import android.content.Context;
import android.view.ViewGroup;
import android.widget.HorizontalScrollView;
import android.widget.ScrollView;


public class HorizontalScrollViewProxy extends ViewGroupProxy
{
	public HorizontalScrollViewProxy(Context con, HashMap<String, Value> keywords)
	{
		super(con, keywords);
		
	}

	@Override
	public ViewGroup getPreconfiguredView()
	{
		HorizontalScrollView layout = new HorizontalScrollView(context);
		return layout;
		
	}
}
