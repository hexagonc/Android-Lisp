package com.evolved.automata.android.widgets;

import android.content.Context;
import android.util.AttributeSet;
import android.util.Log;
import android.view.MotionEvent;
import android.widget.ScrollView;

public class ChildScrollView extends ScrollView
{
	public ChildScrollView(Context con)
	{
		super(con);
	}
	
	public ChildScrollView(Context con, AttributeSet attrib)
	{
		super(con, attrib);
		
	}

	@Override
	public boolean onInterceptTouchEvent(MotionEvent ev) {
		boolean out = super.onInterceptTouchEvent(ev);
		//Log.e("ChildScrollView", "intercepted touch event: " + out);
		return out;
	}

	@Override
	public boolean onTouchEvent(MotionEvent ev) {
		// TODO Auto-generated method stub
		boolean out = super.onTouchEvent(ev);
		//Log.e("ChildScrollView", "handled touch event: " + out);
		return out;
	}

	@Override
	public boolean dispatchTouchEvent(MotionEvent ev) {
		// TODO Auto-generated method stub
		return super.dispatchTouchEvent(ev);
	}
}
