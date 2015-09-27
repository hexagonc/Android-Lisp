package com.evolved.automata.android.widgets;

import android.content.Context;
import android.support.v4.view.ViewPager;
import android.util.AttributeSet;
import android.view.MotionEvent;

public class SwipelessViewPager extends ViewPager 
{
	public SwipelessViewPager(Context con)
	{
		super(con);
	}
	
	public SwipelessViewPager(Context con, AttributeSet attr)
	{
		super(con, attr);
	}

	@Override
	public boolean onInterceptTouchEvent(MotionEvent arg0) {
		
		return false;
	}

	@Override
	public boolean onTouchEvent(MotionEvent arg0) {
		
		return false;
	}
	
}
