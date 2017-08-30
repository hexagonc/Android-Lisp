package com.evolved.automata.android.widgets;

import com.evolved.automata.android.tools.R;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.util.AttributeSet;
import android.util.DisplayMetrics;
import android.util.TypedValue;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.widget.LinearLayout;
import android.widget.TextView;

public class SimplePagerTab extends LinearLayout
{
	TextView _embedded;
	
	public SimplePagerTab(Context context)
	{
		super(context);
		setClickable(true);
		_embedded = new TextView(context);
		addView(_embedded);
		_embedded.setTextSize(TypedValue.COMPLEX_UNIT_PX, 12);
		
		_embedded.setTextColor(Color.BLACK);
	
		setBackgroundColor(Color.BLACK);
		
		_embedded.setBackgroundColor(Color.WHITE);
		ViewGroup.LayoutParams params = new ViewGroup.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT);
		setLayoutParams(params);
		setSelected(false);
		_embedded.setPadding(convertToPX(3), convertToPX(3), convertToPX(3), convertToPX(3));
	}
	
	public SimplePagerTab(Context context, AttributeSet attrs) 
	{
		super(context, attrs);
		_embedded = new TextView(context);
		addView(_embedded);
		setClickable(true);
		initializeFromAttributes(attrs);
		ViewGroup.LayoutParams params = getLayoutParams();
		if (params == null)
		{
			setLayoutParams(new ViewGroup.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT));
		}
		
	}
	
	void initializeFromAttributes(AttributeSet attrs)
	{
		TypedArray ta = getContext().obtainStyledAttributes(attrs, R.styleable.Standard_Custom);
		
		String text = ta.getString(R.styleable.Standard_Custom_text);
		_embedded.setText(text);
		
		float padding = ta.getDimensionPixelSize(R.styleable.Standard_Custom_padding, convertToPX(3));
		_embedded.setPadding((int)padding, (int)padding, (int)padding, (int)padding);
		
		float tsize = ta.getDimensionPixelSize(R.styleable.Standard_Custom_textSize, -1);
		if (tsize>0)
			_embedded.setTextSize(TypedValue.COMPLEX_UNIT_PX, (int)tsize);
		
		int color = ta.getColor(R.styleable.Standard_Custom_textColor, Color.BLACK);
		_embedded.setTextColor(color);
	
		int shadowColor = ta.getColor(R.styleable.Standard_Custom_shadowColor, Color.BLACK);
		setBackgroundColor(shadowColor);

		int tabColor = ta.getColor(R.styleable.Standard_Custom_backGround, Color.WHITE);
		_embedded.setBackgroundColor(tabColor);
		
		boolean selected = ta.getBoolean(R.styleable.Standard_Custom_selected, false);
		
		setSelected(selected);
		ta.recycle();
	}
	
	public void setOnClickListener(View.OnClickListener listener)
	{
		_embedded.setOnClickListener(listener);
	}
	
	public void setText(String text)
	{
		_embedded.setText(text);
	}
	
	public void setTextSize(int type, int size)
	{
		_embedded.setTextSize(type, size);
	}
	
	public void setTextSize( int size)
	{
		_embedded.setTextSize(TypedValue.COMPLEX_UNIT_PX, size);
	}
	
	public void setSelected(boolean selectedP)
	{
		if (selectedP)
		{
			setPadding(0, convertToPX(4), convertToPX(2), 0);
		}
		else
			setPadding(0, 0, convertToPX(2), 0);
	}
	
	int convertToPX(int dp)
	{
		DisplayMetrics dm = new DisplayMetrics();
		WindowManager m = (WindowManager)getContext().getSystemService(Context.WINDOW_SERVICE);
		m.getDefaultDisplay().getMetrics(dm);
		return (int)TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, dp, dm);
	}
}
