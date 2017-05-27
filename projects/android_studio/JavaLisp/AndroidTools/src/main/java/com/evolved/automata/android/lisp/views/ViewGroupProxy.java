package com.evolved.automata.android.lisp.views;

import java.util.HashMap;
import java.util.LinkedList;

import com.evolved.automata.lisp.Value;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;


public abstract class ViewGroupProxy extends ViewProxy
{
	protected LinkedList<ViewProxy> children;
	public ViewGroupProxy(Context con, HashMap<String, Value> keymap)
	{
		super(con, keymap);
		children = new LinkedList<ViewProxy>();
	}
	
	public ViewProxy addChild(ViewProxy proxy)
	{
		for (ViewProxy p:children)
		{
			if (p == proxy)
				return null;
		}
		children.add(proxy);
		View actual;
		if (encapsulated != null && (actual = encapsulated.get())!= null)
		{
			View out = proxy.createView((ViewGroup)actual);
			((ViewGroup)actual).addView(out);
		}
		return proxy;
	}
	
	public void removeAllViews()
	{
		children.clear();
		View actual;
		if (encapsulated != null && (actual = encapsulated.get())!= null)
		{
			((ViewGroup)actual).removeAllViews();
		}
	}
	
	public ViewProxy removeView(ViewProxy proxy)
	{
		boolean remove = false;
		for (ViewProxy p:children)
		{
			if (p == proxy)
			{
				remove = true;
				break;
			}
		}
		if (remove)
			children.remove(proxy);

		View actual;
		if (encapsulated != null && (actual = encapsulated.get())!= null && remove)
		{
			View out = proxy.getView();
			if (out != null)
				((ViewGroup)actual).removeView(out);
		}
		return proxy;
	}
	
	public abstract ViewGroup getPreconfiguredView(); 
	
	
	@Override
	public View createBaseView() 
	{
		ViewGroup main = getPreconfiguredView();
		View view = null;
		for (ViewProxy proxy:children)
		{
			view = proxy.createView(main);
			main.addView(view);
		}
		return main;
		
	}
}
