package com.evolved.automata.android.widgets;

import com.evolved.automata.android.tools.R;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.util.AttributeSet;
import android.util.Log;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;

public class TextLineView extends LinearLayout 
{
	
	int _numLines;
	String[] _lineText;
	Context _context;
	int _backgroundColor = 0;
	public TextLineView(int numLines, int backgroundColor, Context context) {
		super(context);
		_numLines = numLines;
		_context = context;
		_backgroundColor = backgroundColor;
		setBackgroundColor(_backgroundColor);
	}
	
	public TextLineView(Context context, AttributeSet attrs) {
		super(context, attrs);
		_context = context;
		initializeFromAttributes(attrs);
	}

	int[] attr_array = new int[]{R.attr.num_lines};
	
	void initializeFromAttributes(AttributeSet attrs)
	{
		String namespace = "http://schemas.android.com/apk/lib/com/evolved/automata/noetogenesis/tools";
		String data = attrs.getAttributeValue(namespace, "num_lines");
		//TypedArray tarray = _context.obtainStyledAttributes(attrs, attr_array);
		//_numLines = tarray.getInt(0, 12);
		//tarray.recycle();
		_numLines = Integer.parseInt(data);
	}
	
	public void addLine(String line, String color, boolean rollup)
	{
		
		
		TextView newView = addView(line, color);
		int child = getChildCount();
		
		if (child>=_numLines)
		{
			removeViewAt(0);
			addView(newView);
		}
		else
		{
			addView(newView);
		}
		
		
		invalidate();
	}
	
	TextView addView(String text, String color)
	{
		TextView tView = new TextView(_context);
		LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT);
		
		
		tView.setText(text);
		tView.setTextColor(Color.parseColor(color));
		tView.setSingleLine();
		tView.setLayoutParams(params);
		return tView;
	}
	
	void clear()
	{
		removeAllViews();
	}
}
