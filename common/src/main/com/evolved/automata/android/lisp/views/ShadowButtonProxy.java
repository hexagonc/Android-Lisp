package com.evolved.automata.android.lisp.views;

import java.util.HashMap;

import android.content.Context;
import android.graphics.Color;
import android.graphics.Typeface;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;


import com.evolved.automata.android.EvaluateException;

import com.evolved.automata.android.widgets.ShadowButton;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;

public class ShadowButtonProxy extends ViewProxy
{
	public static final String TEXT_SIZE = ":text-size"; // text pixel size in sp
	public static final String TEXT_STYLE = ":text-style"; // string combination of "bold" or "italic" joined by pipe '|' character if needed
	public static final String FONT_FACE = ":typeface"; // "normal" | "sans" | "serif" | "monospaced"
	public static final String TEXT_ALIGNMENT = ":text-align"; // string combination of "top" or "left" or "right" or "bottom" or "center" joined buy pipe '|' character if necessary
	public static final String TEXT_COLOR = ":text-color"; // raw color decimal number | android string color spec, such as #000 or @android:color/white

	
	String text = null;
	int textColor;
	
	public ShadowButtonProxy(Context con, HashMap<String, Value> keymap, String text)
	{
		super(con, keymap);
		this.text = text;
	}
	
	public ShadowButtonProxy setText(String text)
	{
		this.text = text;
		if (encapsulated!=null)
		{
			((ShadowButton)encapsulated).setText(text);
		}
		return this;
	}
	
	public String getText()
	{
		return text;
	}
	
	protected void processKeywords(HashMap<String, Value> keys, ShadowButton tview)
	{
		processTextSize(keys, tview);
		processFontFaceAndStyle(keys, tview);
		processTextColor(keys, tview);
	}
	
	protected void processTextColor(HashMap<String, Value> keys, ShadowButton tview)
	{
		Value color = getMapValue(keys, TEXT_COLOR);
		if (!color.isNull())
		{
			setTextColor(color, tview);
		}
	}
	
	public void setTextColor(Value color, ShadowButton tv)
	{
		if (NLispTools.isNumericType(color))
		{
			textColor = Double.valueOf(color.getFloatValue()).intValue();
			if (tv!=null)
				tv.setTextColor(textColor);
		}
		else if (color.isString())
		{
			textColor = Color.parseColor(color.getString());
			if (tv!=null)
				tv.setTextColor(textColor);
		}
		else
		{
			throw new EvaluateException("Invalid text color spec");
		}
	}
	
	public void setTextColor(Value color)
	{
		setTextColor(color, (ShadowButton)encapsulated);
	}
	
	
	public void processFontFaceAndStyle(HashMap<String, Value> keys, ShadowButton tview)
	{
		Value style = getMapValue(keys, TEXT_STYLE);
		Value face = getMapValue(keys, FONT_FACE);
		
		int styleValue = Typeface.NORMAL;
		Typeface tp = Typeface.DEFAULT;
		if (!style.isNull())
		{
			try
			{
				String styleSpec = style.getString();
				styleValue = 0;
				for (String spec:styleSpec.split("\\|"))
				{
					if (spec.equalsIgnoreCase("bold"))
						styleValue |= Typeface.BOLD;
					else if (spec.equalsIgnoreCase("italic"))
						styleValue |= Typeface.ITALIC;
					else
						throw new EvaluateException("Invalid text style spec: " + style);
				}
				
			}
			catch (Exception e)
			{
				throw new EvaluateException("Invalid text style spec: " + style);
			}
		}
			
		if (!face.isNull())
		{
			try
			{
				String faceSpec = face.getString();
				if (faceSpec.equalsIgnoreCase("normal"))
					tp = Typeface.DEFAULT;
				else if (faceSpec.equalsIgnoreCase("sans"))
					tp = Typeface.SANS_SERIF;
				else if (faceSpec.equalsIgnoreCase("serif"))
					tp = Typeface.SERIF;
				else if (faceSpec.equalsIgnoreCase("monospaced"))
					tp = Typeface.MONOSPACE;
				else
					throw new EvaluateException("Invalid fontface spec: " + face);
				
			}
			catch (Exception e)
			{
				throw new EvaluateException("Invalid fontface spec: " + face);
			}
		}
		
		tview.setTypeface(tp, styleValue);
		
	}
	
	public void processTextSize(HashMap<String, Value> keys, ShadowButton tview)
	{
		Value text = getMapValue(keys, TEXT_SIZE);
		if (!text.isNull())
		{
			Number value = Double.valueOf(text.getFloatValue());
			tview.setTextSize(value.floatValue());
		}
	}
	
	@Override
	public View createBaseView()
	{
		ShadowButton tv = new ShadowButton(context, text);
		processKeywords(_keys, tv);
		return tv;
	}
}
