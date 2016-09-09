package com.evolved.automata.android.widgets;

import android.app.Activity;
import android.support.v4.view.PagerAdapter;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;



public abstract class ViewPagerConfigurator 
{
	protected Activity _parentActivity;
	protected View myView;
	protected int position;
	
	public ViewPagerConfigurator(Activity activity)
	{
		_parentActivity = activity;
		this.position = 0;	
	}
	
	public LayoutInflater getInflater()
	{
		return _parentActivity.getLayoutInflater();
	}
	
	public View getView(ViewGroup parent)
	{
		return myView;
	}
	
	public boolean isMyView(View v)
	{
		return myView == v;
	}
	
	public void destroyView(final ViewGroup parent)
	{
		Runnable r = new Runnable()
		{
			public void run()
			{
				for (int i=0;i<parent.getChildCount();i++)
				{
					if (parent.getChildAt(i) == myView)
						parent.removeViewAt(i);
				}
				myView = null;
			}
		};
		
		_parentActivity.runOnUiThread(r);
		
	}
	
	public abstract View createView(ViewGroup vg);
	
	public void addView(final ViewGroup parent)
	{
		if (myView == null)
		{
			myView = createView(parent);
			parent.addView(myView);
		}
		else
		{
			for (int i=0;i<parent.getChildCount();i++)
			{
				if (parent.getChildAt(i) == myView)
					return;
			}
			parent.addView(myView);
		}
		
	}
	
	public void updatePos(int i)
	{
		position = i;
	}
	
	public void markAsDeleted()
	{
		position = PagerAdapter.POSITION_NONE;
	}
	
	public void markPositionAsChanged()
	{
		position = PagerAdapter.POSITION_UNCHANGED;
	}
	
	public int getPosition()
	{
		return position;
	}
	
	public void setSelected(boolean selected)
	{
		
	}
}
