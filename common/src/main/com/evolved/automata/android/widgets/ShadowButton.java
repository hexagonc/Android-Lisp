package com.evolved.automata.android.widgets;
import com.evolved.automata.android.tools.R;


import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Typeface;
import android.util.AttributeSet;
import android.util.TypedValue;
import android.view.*;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.RelativeLayout;

public class ShadowButton extends RelativeLayout
{
	
	String _text;
	Paint _defPaint;
	
	Object _sync = new Object();
	View.OnClickListener _onClickListener;
	View.OnLongClickListener _onLongClickListener;
	
	int _width;
	int _height;
	Button pressedButton, unpressedButton;
	int _longPressMilli = android.view.ViewConfiguration.getLongPressTimeout();
	long _longPressTimeout = 0;
	
	public ShadowButton(Context context, String label)
	{
		super(context);
		_text = label;
		_defPaint = new Paint();
		_defPaint.setAlpha(255);
		LayoutInflater inflater = (LayoutInflater)context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.shadow_button_layout, this);
		
		pressedButton = (Button)findViewById(R.id.pressed);
		unpressedButton = (Button)findViewById(R.id.unpressed);
		
		pressedButton.setText(_text);
		unpressedButton.setText(_text);
		pressedButton.setVisibility(INVISIBLE);
		unpressedButton.setVisibility(VISIBLE);
		
	}
	
	public ShadowButton(Context context, AttributeSet attrs) 
	{
		super(context, attrs);
		
		_defPaint = new Paint();
		_defPaint.setAlpha(255);
		LayoutInflater inflater = (LayoutInflater)context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.shadow_button_layout, this);
		
		pressedButton = (Button)findViewById(R.id.pressed);
		unpressedButton = (Button)findViewById(R.id.unpressed);
		
		pressedButton.setVisibility(INVISIBLE);
		unpressedButton.setVisibility(VISIBLE);
		initializeFromAttributes(attrs);
		
		
	}
	void initializeFromAttributes(AttributeSet attrs)
	{
		TypedArray ta = getContext().obtainStyledAttributes(attrs, R.styleable.Standard_Custom);
		
		_text = ta.getString(R.styleable.Standard_Custom_text);
		
		pressedButton.setText(_text);
		unpressedButton.setText(_text);
		
		float tsize = ta.getDimensionPixelSize(R.styleable.Standard_Custom_textSize, 12);
		pressedButton.setTextSize(TypedValue.COMPLEX_UNIT_PX, (int)tsize);
		unpressedButton.setTextSize(TypedValue.COMPLEX_UNIT_PX, (int)tsize);
		
		int color = ta.getColor(R.styleable.Standard_Custom_textColor, 0);
		unpressedButton.setTextColor(color);
		pressedButton.setTextColor(color);
		
		ta.recycle();
	}
	
	public void setText(String text)
	{
		pressedButton.setText(text);
		unpressedButton.setText(text);
	}
	
	@Override
	public void setOnClickListener(View.OnClickListener listener)
	{
		_onClickListener = listener;
	}
	
	@Override
	public void setOnLongClickListener(View.OnLongClickListener listener)
	{
		_onLongClickListener = listener;
	}
	
	@Override
	public boolean onInterceptTouchEvent (MotionEvent ev)
	{
		return true;
	}
	
	public void setTextColor(int color)
	{
		unpressedButton.setTextColor(color);
		pressedButton.setTextColor(color);
	}
	
	public void setTypeface(Typeface tf, int style)
	{
		unpressedButton.setTypeface(tf, style);
		pressedButton.setTypeface(tf, style);
	}
	
	public void setTextSize(float pixel)
	{
		unpressedButton.setTextSize(pixel);
		pressedButton.setTextSize(pixel);
	}
	
	@Override
	public boolean onTouchEvent(MotionEvent event)
	{
		
		int action = event.getAction();
		//android.util.Log.i("ontouch",""+action);
		switch (action)
		{
			case MotionEvent.ACTION_DOWN:
				
				pressedButton.setVisibility(VISIBLE);
				unpressedButton.setVisibility(INVISIBLE);
				_longPressTimeout = System.currentTimeMillis() + _longPressMilli;
				
				return true;
			case MotionEvent.ACTION_UP:
				pressedButton.setVisibility(INVISIBLE);
				unpressedButton.setVisibility(VISIBLE);
				if (System.currentTimeMillis()>_longPressTimeout && _onLongClickListener!=null)
				{
					_onLongClickListener.onLongClick(this);
				}
				else if (_onClickListener!=null)
					_onClickListener.onClick(this);
				return true;
			
		}
		return false;
	}
}
