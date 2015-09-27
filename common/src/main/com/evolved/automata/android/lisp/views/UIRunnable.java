package com.evolved.automata.android.lisp.views;

import android.os.Handler;

public abstract class UIRunnable implements Runnable
{
	Handler handler;
	public UIRunnable(Handler h)
	{
		handler = h;
	}
	
	public void post()
	{
		handler.post(this);
	}
	
	public void postDelayed(long milli)
	{
		handler.postDelayed(this, milli);
	}
}
