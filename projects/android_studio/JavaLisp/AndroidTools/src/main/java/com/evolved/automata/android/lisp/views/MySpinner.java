package com.evolved.automata.android.lisp.views;

import android.content.Context;
import android.os.Parcelable;
import android.widget.Spinner;

public class MySpinner extends Spinner 
{
	public MySpinner(Context con)
	{
		super(con);
	}
	
	@Override
	public void setSelection(int item, boolean animate)
	{
		super.setSelection(item, animate);
	}
	
	@Override
	public void setSelection(int item)
	{
		super.setSelection(item);
	}
	
	@Override
	public void onRestoreInstanceState(Parcelable state)
	{
		super.onRestoreInstanceState(state);
	}
	
	
	public Parcelable onSaveInstanceState()
	{
		return super.onSaveInstanceState();
	}
}
