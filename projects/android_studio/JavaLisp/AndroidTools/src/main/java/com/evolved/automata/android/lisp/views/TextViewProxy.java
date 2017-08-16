package com.evolved.automata.android.lisp.views;

import java.util.HashMap;
import java.util.HashSet;

import android.content.Context;
import android.graphics.Color;
import android.graphics.Typeface;
import android.text.Layout;
import android.view.Gravity;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;


import com.evolved.automata.android.EvaluateException;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;

import org.apache.commons.lang3.tuple.Pair;


public class TextViewProxy extends ViewProxy
{
	public static final String TEXT_SIZE = ":text-size"; // text pixel size in sp
	public static final String TEXT_STYLE = ":text-style"; // string combination of "bold" or "italic" joined by pipe '|' character if needed
	public static final String FONT_FACE = ":typeface"; // "normal" | "sans" | "serif" | "monospaced"
	public static final String TEXT_ALIGNMENT = ":text-align"; // string combination of "top" or "left" or "right" or "bottom" or "center" joined buy pipe '|' character if necessary
	public static final String TEXT_COLOR = ":text-color"; // raw color decimal number | android string color spec, such as #000 or @android:color/white



    public enum BOOLEAN_KEY
    {
        SINGLE_LINE(":singleline"), EDITABLE(":editable");
        String key;
        BOOLEAN_KEY(String keyname)
        {
            key = keyname;
        }

        @Override
        public String toString()
        {
            return key;
        }

        public static BOOLEAN_KEY fromKeyword(String word)
        {
            for (BOOLEAN_KEY k:values())
            {
                if (k.toString().equals(word))
                    return k;
            }
            return null;
        }
    }

	public static final HashSet<String> BOOLEAN_KEY_MAP = new HashSet<String>(){
		{
            for (BOOLEAN_KEY key: BOOLEAN_KEY.values())
            {
                add(key.toString());
            }

		}

	}; // boolean values can be: true | false

	
	protected String text = null;
	int textColor;
	
	public TextViewProxy(Context con, HashMap<String, Value> keymap, String text)
	{
		super(con, keymap);
		this.text = text;
	}
	
	public TextViewProxy setText(String text)
	{
		this.text = text;
		View actual;
		if (encapsulated != null && (actual = encapsulated.get())!= null)
		{
			((TextView)actual).setText(text);
		}
		return this;
	}
	
	public String getText()
	{
		View actual;
		if (encapsulated != null && (actual = encapsulated.get())!= null)
		{
			return text = ((TextView)actual).getText().toString();
		}
		else
			return text;
	}
	
	protected void processKeywords(HashMap<String, Value> keys, TextView tview)
	{
		processTextSize(keys, tview);
		processFontFaceAndStyle(keys, tview);
		processTextAlignment(keys, tview);
		processTextColor(keys, tview);
        processBooleanKeys(keys, tview);
	}

    protected void processBooleanKeys(HashMap<String, Value> keys, TextView tview)
    {

        for (String key:keys.keySet())
        {
            if (BOOLEAN_KEY_MAP.contains(key))
            {
                BOOLEAN_KEY enumKey = BOOLEAN_KEY.fromKeyword(key);
                String svalue = keys.get(key).getString();
                boolean bvalue = (svalue.equals("true"));
                switch (enumKey)
                {
                    case SINGLE_LINE:
                        tview.setSingleLine(bvalue);
                        break;
                    case EDITABLE:
                        if (!bvalue)
                        {
                            // TODO: Finish this later to allow scrolling
                            tview.setOnTouchListener(new View.OnTouchListener() {

                                @Override
                                public boolean onTouch(View v, MotionEvent event) {

                                    Pair<Float, Float> currentPos = Pair.of(event.getX(), event.getY());
                                    //Log.d("Vo<>oVo<>oVo", "Touch event: " + event.toString());
                                    switch (event.getAction()) {
                                        case MotionEvent.ACTION_DOWN:

                                            return true;
                                        case MotionEvent.ACTION_UP:

                                            return true;
                                    }
                                    return false;
                                }

                            });
                        }
                        else
                        {
                            tview.setOnTouchListener(null);
                        }

                        break;
                }
            }

        }


    }

	
	protected void processTextColor(HashMap<String, Value> keys, TextView tview)
	{
		Value color = getMapValue(keys, TEXT_COLOR);
		if (!color.isNull())
		{
			setTextColor(color, tview);
		}
	}
	
	public void setTextColor(Value color, TextView tv)
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
		setTextColor(color, (encapsulated != null)? (TextView)encapsulated.get():null);
	}
	
	
	public void processFontFaceAndStyle(HashMap<String, Value> keys, TextView tview)
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
					else if (spec.equalsIgnoreCase("normal"))
						styleValue |= Typeface.NORMAL;
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
	
	public void processTextSize(HashMap<String, Value> keys, TextView tview)
	{
		Value text = getMapValue(keys, TEXT_SIZE);
		if (!text.isNull())
		{
			Number value = Double.valueOf(text.getFloatValue());
			tview.setTextSize(value.floatValue());
		}
	}
	
	public void processTextAlignment(HashMap<String, Value> keys, TextView tview)
	{
		Value alignment = getMapValue(keys, TEXT_ALIGNMENT);
		if (!alignment.isNull())
		{
			try
			{
				String value = alignment.getString();
				
				int align = 0;
				
				for (String comp:value.split("\\|"))
				{
					if (comp.equalsIgnoreCase("left"))
						align |= Gravity.LEFT;
					else if (comp.equalsIgnoreCase("right"))
						align |= Gravity.RIGHT;
					else if (comp.equalsIgnoreCase("top"))
						align |= Gravity.TOP;
					else if (comp.equalsIgnoreCase("bottom"))
						align |= Gravity.BOTTOM;
					else if (comp.equalsIgnoreCase("center"))
						align |= Gravity.CENTER;
				}
				tview.setGravity(align);
			}
			catch (Exception e)
			{
				throw new EvaluateException("Invalid text alignment spec: " + alignment);
			}
			
			
		}
	}
	

	
	public View createBaseView(TextView tv)
	{
		processKeywords(_keys, tv);
		tv.setText(text);
		return tv;
	}
	
	@Override
	public View createBaseView()
	{
		TextView tv = new TextView(context);
		tv.setText(text);
        tv.setHorizontallyScrolling(true);
        tv.setTextIsSelectable(true);
		processKeywords(_keys, tv);
		return tv;
	}

	
	@Override
	public void applyAttribures(HashMap<String, Value> keywords)
	{
		super.applyAttribures(keywords);
		View actual;
		if (encapsulated != null && (actual = encapsulated.get())!= null)
		{
			processKeywords(keywords, (TextView)actual);
		}
	}
	
}
