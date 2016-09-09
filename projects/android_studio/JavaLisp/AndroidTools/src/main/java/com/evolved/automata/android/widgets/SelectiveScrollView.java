package com.evolved.automata.android.widgets;


import com.evolved.automata.android.tools.R;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import android.widget.ScrollView;

/**
 * The SelectiveScrollView only scrolls when touches fall outside of a particular child view or
 * when a scroll event occurs above a certain point
 * @author Evolved8
 *
 */
public class SelectiveScrollView extends ScrollView 
{
	View _child= null;
	int _childId = 0;
	int _scrollPadding = 0;
	AttributeSet _attrib;
	
	public SelectiveScrollView(Context con)
	{
		super(con);
	}
	
	public SelectiveScrollView(Context con, AttributeSet attrib)
	{
		super(con, attrib);
		_attrib = attrib;
		processAttributes(attrib);
	}
	
	private void processAttributes(AttributeSet attrib)
	{
		TypedArray ta = getContext().obtainStyledAttributes(attrib, R.styleable.Standard_Custom);
		_scrollPadding = ta.getDimensionPixelSize(R.styleable.Standard_Custom_scrollPadding, 0);
		
		ta.recycle();
	}
	
	public void setTopScrollPaddingPX(int padding)
	{
		_scrollPadding = padding;
	}
	
	public void setScrollBypassChildId(int id)
	{
		_childId = id;
		if (_childId>0)
			_child = findViewById(_childId);
	}
	
	public void setScrollBypassChild(View view)
	{
		_child = view;
	}
	
	@Override
	protected void onFinishInflate()
	{
		super.onFinishInflate();
		if (_childId>0)
			_child = findViewById(_childId);
			
	}
	
	private boolean  motionEventInView(View childView, MotionEvent me)
	{
		View parentView;
		int top = childView.getTop() - childView.getScrollY() - this.getScrollY();
		int height = childView.getHeight();
		while ((parentView = (View)childView.getParent())!=this)
		{
			top += parentView.getTop() - parentView.getScrollY();
			childView = parentView;
		}
		float y = me.getY();
		return (y>=top && y<=(top + height));
		
	}

	boolean parentHasControl;
	
	@Override
	public boolean onTouchEvent(MotionEvent ev) 
	{
		//Log.d("onTouchEvent ", ev.toString() + " parentHasControl = " + parentHasControl);
		switch (ev.getAction())
		{
			case MotionEvent.ACTION_DOWN:
				parentHasControl = ev.getY()<_scrollPadding || _child == null || !motionEventInView(_child, ev);
				if (parentHasControl)
				{
					//Log.d("onTouchEvent ", "doing normal scroll");
					return super.onTouchEvent(ev);
				}
				else
				{
					//Log.d("onTouchEvent ", "In bypass child.  Ignoring scroll");
					return false;
				}
			case MotionEvent.ACTION_MOVE:
				if (parentHasControl)
				{
					//Log.d("onTouchEvent ", "dragging with normal scroll");
					return super.onTouchEvent(ev);
				}
				else
				{
					//Log.d("onTouchEvent ", "bypassing scroll for drag");
					return false; // don't want to handle this touch event if in child view
				}
			case MotionEvent.ACTION_UP:
				
				if (parentHasControl)
				{
					//Log.d("onTouchEvent ", "dragging with normal scroll");
					parentHasControl = false;
					return super.onTouchEvent(ev);
				}
				else
				{
					//Log.d("onTouchEvent ", "bypassing scroll for drag");
					parentHasControl = false;
					return false; // don't want to handle this touch event if in child view
				} 
		}
		return super.onTouchEvent(ev);
		
	}
	
	

	@Override
	public boolean dispatchTouchEvent(MotionEvent ev) 
	{
		
		
		
		if (ev.getY()<_scrollPadding)
		{
			Log.d("dispatchTouchEvent ", "Moved into scroll padding.  Bypassing children");
			return super.onTouchEvent(ev);
		}
		else
			return super.dispatchTouchEvent(ev);
	}
	
	
}
