package com.evolved.automata.android.lisp.views;

import java.util.HashMap;
import java.util.LinkedList;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.Resources;
import android.graphics.Bitmap;
import android.graphics.Color;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewGroup.MarginLayoutParams;
import android.view.ViewParent;
import android.view.WindowManager;
import android.widget.FrameLayout;
import android.widget.HorizontalScrollView;
import android.widget.LinearLayout;
import android.widget.RadioButton;
import android.widget.RelativeLayout;
import android.widget.ScrollView;


import com.evolved.automata.KeyValuePair;
import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.android.DeviceBehaviorOverrides;
import com.evolved.automata.android.EvaluateException;
import com.evolved.automata.android.lisp.AndroidLispInterpreter;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;

import com.nostra13.universalimageloader.core.ImageLoader;
import com.nostra13.universalimageloader.core.assist.FailReason;
import com.nostra13.universalimageloader.core.assist.ImageLoadingListener;

public abstract class ViewProxy 
{
	AndroidLispInterpreter _lispInterpreter;
	View encapsulated;
	static int TOP_ID = 100;
	int id = 0;
	
	protected int foregroundColor;
	protected int backgroundColor;
	protected String backgroundResourceUrl = null;
	protected int backgroundResourceId = 0;
	protected int visibility = View.VISIBLE;
	
	
	protected Resources res;
	
	int defaultWidth = ViewGroup.LayoutParams.WRAP_CONTENT;
	int defaultHeight = ViewGroup.LayoutParams.WRAP_CONTENT;
	protected boolean backgroundColorDefinedP = false;
	
	public static final String WIDTH_KEYWORD = ":width"; //  integer dp | "match_parent" | "fill_parent" | "wrap_content"
														//	if parent is horizontal-layout then "%#" where # is percent of parent width or "equal-space" meaning width is equally divided amongst all siblings
	public static final String HEIGHT_KEYWORD = ":height"; //  integer dp | "match_parent" | "fill_parent" | "wrap_content"
														  //	if parent is vertical-layout then can be "%#" of parent height or "equal-space" meaning height is equally divided amongst all siblings
	public static final String EQUAL_SPACE_WIDTH = "equal-space";
	public static final String BACKGROUND_COLOR = ":background-color"; // raw color decimal number | android string color spec, such as #000 or @android:color/white  
	public static final String BACKGROUND_DRAWABLE = ":background"; // decimal integer drawable resource id | string url of image resource 
	public static final String VISIBILITY = ":visibility"; // "visible" | "invisible" | "gone"
	
	public static final String MARGIN = ":margin"; // integer dp
	public static final String MARGIN_LEFT = ":margin-left"; // integer dp
	public static final String MARGIN_RIGHT = ":margin-right"; // integer dp
	public static final String MARGIN_TOP = ":margin-top"; // integer dp
	public static final String MARGIN_BOTTOM = ":margin-bottom"; // integer dp
	
	public static final String PADDING = ":padding"; // integer dp
	public static final String PADDING_LEFT = ":padding-left"; // integer dp
	public static final String PADDING_RIGHT = ":padding-right"; // integer dp
	public static final String PADDING_TOP = ":padding-top"; // integer dp
	public static final String PADDING_BOTTOM = ":padding-bottom"; // integer dp
	
	public static final String PARENT_ALIGN = ":parent-align"; // combination of "top" | "bottom" | "center" | "left" | "right" joined by pipe '|' character
	public static final String ON_CLICK = ":on-click"; // a string expression to be evaluated
	public static final String ON_LONG_CLICK = ":on-long-click"; // a string expression to be evaluated that can return a boolean result
	
	int topPadding = -1;
	int bottomPadding = -1;
	int rightPadding = -1;
	int leftPadding = -1;
	Context context;
	Environment _currentEnv = null;
	
	View.OnClickListener _onClickListener = null;
	View.OnLongClickListener _onLongClickListener = null;
	Drawable _backgroundDrawable = null;
	protected HashMap<String, Value> _keys = null;
	boolean _enabledP= true;
	
	private static LinkedList<ViewProxy> _proxyCache = new LinkedList<ViewProxy>();
	
	public ViewProxy(Context con, HashMap<String, Value> keywords)
	{
		context = con;
		res = con.getResources();
		_keys = keywords;
		id = TOP_ID++;
		synchronized (_proxyCache)
		{
			_proxyCache.add(this);
		}
	}
	
	public static void clearDetachedViewProxies()
	{
		synchronized (_proxyCache)
		{
			
			LinkedList<ViewProxy> newProxy = new LinkedList<ViewProxy>();
			for (ViewProxy v:_proxyCache)
			{
				if (v.hasDetachedViewP())
					v.clearView();
				else
					newProxy.add(v);
			}
			_proxyCache = newProxy;
		}
	}
	
	private boolean hasDetachedViewP()
	{
		if (encapsulated == null)
			return false;
		ViewParent parent = encapsulated.getParent();
		return parent == null;
	}
	
	private void clearView()
	{
		encapsulated = null;
	}
	
	public void setEnabled(boolean enabled)
	{
		_enabledP = enabled;
		if (encapsulated != null)
			encapsulated.setEnabled(_enabledP);
	}
	
	protected LinearLayout.LayoutParams processKeywords(LinearLayout parent, HashMap<String, Value> keys)
	{
		LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(defaultWidth, defaultHeight);
		processWidth(params, parent, keys);
		processHeight(params, parent, keys);
		processBaseKeywords(keys);
		processPaddingLeft(keys);
		processPaddingRight(keys);
		processPaddingTop(keys);
		processPaddingBottom(keys);
		processPadding(keys);
		
		processMarginLeft(params, keys);
		processMarginRight(params, keys);
		processMarginTop(params, keys);
		processMarginBottom(params, keys);
		processMargin(params, keys);
		processParentAlignment(params, keys);
		return params;
	}
	
	protected RelativeLayout.LayoutParams processKeywords(RelativeLayout parent, HashMap<String, Value> keys)
	{
		
		RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(defaultWidth, defaultHeight);
		processWidth(params, parent, keys);
		processHeight(params, parent, keys);
		processBaseKeywords(keys);
		processPaddingLeft(keys);
		processPaddingRight(keys);
		processPaddingTop(keys);
		processPaddingBottom(keys);
		processPadding(keys);
		
		processMarginLeft(params, keys);
		processMarginRight(params, keys);
		processMarginTop(params, keys);
		processMarginBottom(params, keys);
		processMargin(params, keys);
		processParentAlignment(params, keys);
		return params;
	}
	
	protected <T extends FrameLayout> FrameLayout.LayoutParams  processKeywords(T parent, HashMap<String, Value> keys)
	{
		
		FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(defaultWidth, defaultHeight);
		processBaseKeywords(keys);
		processWidth(params, parent, keys);
		processHeight(params, parent, keys);
		processPaddingLeft(keys);
		processPaddingRight(keys);
		processPaddingTop(keys);
		processPaddingBottom(keys);
		processPadding(keys);
		
		processMarginLeft(params, keys);
		processMarginRight(params, keys);
		processMarginTop(params, keys);
		processMarginBottom(params, keys);
		processMargin(params, keys);
		processParentAlignment(params, keys);
		return params;
	}
	
	protected void processBaseKeywords(HashMap<String, Value> keys)
	{
		processBackgroundColor(keys);
		processVisibility(keys);
		processBackground(keys);
	}
	
	public void setVisiblity(int visibility)
	{
		this.visibility= visibility;  
		if (encapsulated!=null)
			encapsulated.setVisibility(visibility);
	}
	
	public void setBackgroundColor(String color)
	{
		backgroundColor = Color.parseColor(color);
		backgroundColorDefinedP = true;
		if (encapsulated!=null)
			encapsulated.setBackgroundColor(backgroundColor);
	}
	
	public void setBackgroundImageUrl(String url)
	{
		
	}
	
	public View getView()
	{
		return encapsulated;
	}
	
	protected void processOnClickListenerKeys(HashMap<String, Value> keys)
	{
		final Value code = getMapValue(keys, ON_CLICK);
		if (code.isNull())
			return;
		if (code.isString())
		{
			final String codeContinuation = code.getString();
			_onClickListener = new View.OnClickListener()
			{

				@Override
				public void onClick(View v) {
					_lispInterpreter.evaluateExpression(codeContinuation, true);
				}
				
			};
			
		}
		else
		{
			final KeyValuePair<Environment, Value> transformed = NLispTools.getMinimalEnvironment(_currentEnv, code);
			_onClickListener = new View.OnClickListener()
			{

				@Override
				public void onClick(View v) {
					_lispInterpreter.evaluatePreParsedValue(transformed.GetKey(), transformed.GetValue(), true);
				}
				
			};
			
		}
	}
	
	
	protected void processOnClickListenerKeys(View vw, HashMap<String, Value> keys)
	{
		if (_onClickListener!=null)
			vw.setOnClickListener(_onClickListener);
		
	}
	
	protected void processOnLongClickListenerKeys(View vw, HashMap<String, Value> keys)
	{
		if (_onLongClickListener != null)
			vw.setOnLongClickListener(_onLongClickListener);
		
	}
	
	protected void processOnLongClickListenerKeys(HashMap<String, Value> keys)
	{
		final Value code = getMapValue(keys, ON_LONG_CLICK);
		if (code.isNull())
			return;
		if (code.isString())
		{
			final String codeContinuation = code.getString();
			_onLongClickListener = new View.OnLongClickListener()
			{

				@Override
				public boolean onLongClick(View v) {
					Value out = _lispInterpreter.evaluateExpression(codeContinuation, false);
					return out!=null&&!out.isNull();
				}

			};
			
		}
		else
		{
			final KeyValuePair<Environment, Value> transformed = NLispTools.getMinimalEnvironment(_currentEnv, code); 
			
			_onLongClickListener = new View.OnLongClickListener()
			{

				@Override
				public boolean onLongClick(View v) {
					Value out = _lispInterpreter.evaluatePreParsedValue(transformed.GetKey(), transformed.GetValue(), false);
					return out!=null&&!out.isNull();
				}

			};
			
		}
	}
	
	protected void processParentAlignment(RelativeLayout.LayoutParams params, HashMap<String, Value> keys)
	{
		Value alignArgument = getMapValue(keys, PARENT_ALIGN);
		
		if (!alignArgument.isNull())
		{
			try
			{
				String alignment = alignArgument.getString();
				for (String comp:alignment.split("\\|"))
				{
					if (comp.equalsIgnoreCase("top"))
						params.addRule(RelativeLayout.ALIGN_PARENT_TOP);
					else if (comp.equalsIgnoreCase("bottom"))
						params.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM);
					else if (comp.equalsIgnoreCase("left"))
						params.addRule(RelativeLayout.ALIGN_PARENT_LEFT);
					else if (comp.equalsIgnoreCase("right"))
						params.addRule(RelativeLayout.ALIGN_PARENT_RIGHT);
					else if (comp.equalsIgnoreCase("center"))
						params.addRule(RelativeLayout.CENTER_IN_PARENT);
					else
						throw new EvaluateException("Invalid parent alignment: " + alignArgument);
				}
			}
			catch (Exception e)
			{
				throw new EvaluateException("Invalid parent alignment: " + alignArgument);
			}
		}
				
	}
	
	protected void processParentAlignment(FrameLayout.LayoutParams params, HashMap<String, Value> keys)
	{
		Value alignArgument = getMapValue(keys, PARENT_ALIGN);
		
		if (!alignArgument.isNull())
		{
			try
			{
				String alignment = alignArgument.getString();
				int base = 0;
				for (String comp:alignment.split("\\|"))
				{
					if (comp.equalsIgnoreCase("top"))
						base|=Gravity.TOP;
					else if (comp.equalsIgnoreCase("bottom"))
						base|=Gravity.BOTTOM;
					else if (comp.equalsIgnoreCase("left"))
						base|=Gravity.LEFT;
					else if (comp.equalsIgnoreCase("right"))
						base|=Gravity.RIGHT;
					else if (comp.equalsIgnoreCase("center"))
						base|=Gravity.CENTER;
					else
						throw new EvaluateException("Invalid parent alignment: " + alignArgument);
				}
				params.gravity = base;
				
			}
			catch (Exception e)
			{
				throw new EvaluateException("Invalid parent alignment: " + alignArgument);
			}
		}
				
	}
	
	protected void processParentAlignment(LinearLayout.LayoutParams params, HashMap<String, Value> keys)
	{
		Value alignArgument = getMapValue(keys, PARENT_ALIGN);
		
		if (!alignArgument.isNull())
		{
			try
			{
				String comp = alignArgument.getString();
				if (comp.equalsIgnoreCase("top"))
					params.gravity = Gravity.TOP;
				else if (comp.equalsIgnoreCase("bottom"))
					params.gravity = Gravity.BOTTOM;
				else if (comp.equalsIgnoreCase("left"))
					params.gravity = Gravity.LEFT;
				else if (comp.equalsIgnoreCase("right"))
					params.gravity = Gravity.RIGHT;
				else if (comp.equalsIgnoreCase("center"))
					params.gravity = Gravity.CENTER;
				else
					throw new EvaluateException("Invalid parent alignment: " + alignArgument);
			}
			catch (Exception e)
			{
				throw new EvaluateException("Invalid parent alignment: " + alignArgument);
			}
		}
				
	}
	
	protected void processPaddingLeft(HashMap<String, Value> keys)
	{
		Value marginArg = getMapValue(keys, PADDING_LEFT);
		
		if (!marginArg.isNull())
		{
			try
			{
				Number margin = Double.valueOf(marginArg.getFloatValue());
				leftPadding = AndroidTools.convertDPtoPX(context, margin.intValue());
			}
			catch (Exception e)
			{
				throw new EvaluateException("Invalid left padding spec: " + marginArg);
			}
		}
	}
	
	protected void processPaddingRight(HashMap<String, Value> keys)
	{
		Value marginArg = getMapValue(keys, PADDING_RIGHT);
		
		if (!marginArg.isNull())
		{
			try
			{
				Number margin = Double.valueOf(marginArg.getFloatValue());
				rightPadding = AndroidTools.convertDPtoPX(context, margin.intValue());
			}
			catch (Exception e)
			{
				throw new EvaluateException("Invalid right padding spec: " + marginArg);
			}
		}
	}
	
	protected void processPaddingTop(HashMap<String, Value> keys)
	{
		Value marginArg = getMapValue(keys, PADDING_TOP);
		
		if (!marginArg.isNull())
		{
			try
			{
				Number margin = Double.valueOf(marginArg.getFloatValue());
				topPadding = AndroidTools.convertDPtoPX(context, margin.intValue());
			}
			catch (Exception e)
			{
				throw new EvaluateException("Invalid top padding spec: " + marginArg);
			}
		}
	}
	
	protected void processPaddingBottom(HashMap<String, Value> keys)
	{
		Value marginArg = getMapValue(keys, PADDING_BOTTOM);
		
		if (!marginArg.isNull())
		{
			try
			{
				Number margin = Double.valueOf(marginArg.getFloatValue());
				bottomPadding = AndroidTools.convertDPtoPX(context, margin.intValue());
			}
			catch (Exception e)
			{
				throw new EvaluateException("Invalid bottom padding spec: " + marginArg);
			}
		}
	}
	
	protected void processPadding(HashMap<String, Value> keys)
	{
		Value marginArg = getMapValue(keys, PADDING);
		
		if (!marginArg.isNull())
		{
			try
			{
				Number margin = Double.valueOf(marginArg.getFloatValue());
				leftPadding = AndroidTools.convertDPtoPX(context, margin.intValue());
				rightPadding = AndroidTools.convertDPtoPX(context, margin.intValue());
				topPadding = AndroidTools.convertDPtoPX(context, margin.intValue());
				bottomPadding = AndroidTools.convertDPtoPX(context, margin.intValue());
			}
			catch (Exception e)
			{
				throw new EvaluateException("Invalid padding spec: " + marginArg);
			}
		}
	}
	
	
	protected void processMarginLeft(ViewGroup.MarginLayoutParams params, HashMap<String, Value> keys)
	{
		Value marginArg = getMapValue(keys, MARGIN_LEFT);
		
		if (!marginArg.isNull())
		{
			try
			{
				Number margin = Double.valueOf(marginArg.getFloatValue());
				params.leftMargin = AndroidTools.convertDPtoPX(context, margin.intValue());
			}
			catch (Exception e)
			{
				throw new EvaluateException("Invalid left margin spec: " + marginArg);
			}
		}
	}
	
	protected void processMarginRight(ViewGroup.MarginLayoutParams params, HashMap<String, Value> keys)
	{
		Value marginArg = getMapValue(keys, MARGIN_RIGHT);
		
		if (!marginArg.isNull())
		{
			try
			{
				Number margin = Double.valueOf(marginArg.getFloatValue());
				params.rightMargin = AndroidTools.convertDPtoPX(context, margin.intValue());
			}
			catch (Exception e)
			{
				throw new EvaluateException("Invalid right margin spec: " + marginArg);
			}
		}
	}
	
	protected void processMarginTop(ViewGroup.MarginLayoutParams params, HashMap<String, Value> keys)
	{
		Value marginArg = getMapValue(keys, MARGIN_TOP);
		
		if (!marginArg.isNull())
		{
			try
			{
				Number margin = Double.valueOf(marginArg.getFloatValue());
				params.topMargin = AndroidTools.convertDPtoPX(context, margin.intValue());
			}
			catch (Exception e)
			{
				throw new EvaluateException("Invalid top margin spec: " + marginArg);
			}
		}
	}
	
	protected void processMarginBottom(ViewGroup.MarginLayoutParams params, HashMap<String, Value> keys)
	{
		Value marginArg = getMapValue(keys, MARGIN_BOTTOM);
		
		if (!marginArg.isNull())
		{
			try
			{
				Number margin = Double.valueOf(marginArg.getFloatValue());
				params.bottomMargin = AndroidTools.convertDPtoPX(context, margin.intValue());
			}
			catch (Exception e)
			{
				throw new EvaluateException("Invalid bottom margin spec: " + marginArg);
			}
		}
	}
	
	
	protected void processMargin(ViewGroup.MarginLayoutParams params, HashMap<String, Value> keys)
	{
		Value marginArg = getMapValue(keys, MARGIN);
		
		if (!marginArg.isNull())
		{
			try
			{
				Number margin = Double.valueOf(marginArg.getFloatValue());
				params.rightMargin = AndroidTools.convertDPtoPX(context, margin.intValue());
				params.topMargin = AndroidTools.convertDPtoPX(context, margin.intValue());
				params.bottomMargin = AndroidTools.convertDPtoPX(context, margin.intValue());
				params.leftMargin = AndroidTools.convertDPtoPX(context, margin.intValue());
			}
			catch (Exception e)
			{
				throw new EvaluateException("Invalid margin spec: " + marginArg);
			}
		}
	}
	// ~~~~~~~~~~~~~~~~~~
	
	
	
	
	// _.-\ _.-\ _.-\ _.-\ _.-\ _.-\ _.-\ _.-\ _.-\ _.-\ _.-\ _.-\ _.-\ 
	// Process functions
	// _.-\ _.-\ _.-\ _.-\ _.-\ _.-\ _.-\ _.-\ _.-\ _.-\ _.-\ _.-\ _.-\
	
	public void processVisibility(HashMap<String, Value> keys)
	{
		Value vis = getMapValue(keys, VISIBILITY);
		if (!vis.isNull())
		{
			if (vis.isString())
			{
				String name = vis.getString();
				if (name.toLowerCase().equals("visible"))
					visibility = View.VISIBLE;
				else if (name.toLowerCase().equals("invisible"))
					visibility = View.INVISIBLE;
				else if (name.toLowerCase().equals("gone"))
					visibility = View.GONE;
			}
			throw new EvaluateException("Invalid visibility value: " + vis.toString());
		}
	}
	
	public void processBackground(HashMap<String, Value> keys)
	{
		Value background = getMapValue(keys, BACKGROUND_DRAWABLE);
		if (background.isNull())
		{
			return;
		}
		else
		{
			if (NLispTools.isNumericType(background))
			{
				backgroundResourceId = Double.valueOf(background.getFloatValue()).intValue();
				return;
			}
			else if (background.isString())
			{
				backgroundResourceUrl = background.getString();
				return;
			}
			else if (background.isUserObject() && background.getObjectValue() instanceof Drawable)
			{
				_backgroundDrawable = (Drawable)background.getObjectValue();
				return;
			}
			throw new EvaluateException("Invalid background resource spec");
			
		}
	}
	
	
	public void processBackgroundColor(HashMap<String, Value> keys)
	{
		Value color = getMapValue(keys, BACKGROUND_COLOR);
		if (color .isNull())
		{
			backgroundColorDefinedP = false;
		}
		else
		{
			if (NLispTools.isNumericType(color))
			{
				backgroundColor = Double.valueOf(color.getFloatValue()).intValue();
				backgroundColorDefinedP = true;
			}
			else if (color.isString())
			{
				backgroundColor = Color.parseColor(color.getString());
				backgroundColorDefinedP = true;
			}
			else
			{
				backgroundColorDefinedP = false;
				throw new EvaluateException("Invalid background color spec");
			}
			
			
		}
	}
	
	
	public void processWidth(ViewGroup.MarginLayoutParams layout, ViewGroup parent, HashMap<String, Value> key)
	{
		Value width = getMapValue(key, WIDTH_KEYWORD);
		if (!width.isNull() && processWidthDimensionString(width, layout))
			return;
		Integer value = getInteger(key, WIDTH_KEYWORD);
		
		int actualValue = defaultWidth; 
		if (value != null)
			actualValue = AndroidTools.convertDPtoPX(context, value.intValue());
		
		layout.width = actualValue;
	}
	
	public void processWidth(LinearLayout.LayoutParams layout, LinearLayout parent, HashMap<String, Value> key)
	{
		int actualValue = defaultWidth;
		
		Value width = getMapValue(key, WIDTH_KEYWORD);
		
		if (width.isNull())
		{
			layout.width = defaultWidth;
			return;
		}
		else
		{
			if (parent!=null && parent.getOrientation() == LinearLayout.HORIZONTAL)
			{
				if (width.isString())
				{
					String percentage = width.getString();
					if (percentage.endsWith("%"))
					{
						String percentageValue = percentage.substring(0, percentage.length()-1);
						float percentageNum = Float.parseFloat(percentageValue);
						layout.weight = percentageNum/100F;
						layout.width = 0;
						parent.setWeightSum(1.0F);
						return;
					}
					else if (EQUAL_SPACE_WIDTH.equals(percentage))
					{
						layout.weight = 1;
						layout.width = 0;
						parent.setWeightSum(parent.getWeightSum() + 1);
						return;
					}
					
				}
				
			}
			
			if (processWidthDimensionString(width, layout))
				return;
			
			if (NLispTools.isNumericType(width))
			{
				actualValue = Double.valueOf(width.getFloatValue()).intValue();
				layout.width = AndroidTools.convertDPtoPX(context, actualValue);
				return;
			}
			throw new EvaluateException("Invalid width: " + width);
		}
		
	}
	
	private boolean  processWidthDimensionString(Value width, ViewGroup.LayoutParams layout)
	{
		if (width.isString())
		{
			if ("match_parent".equalsIgnoreCase(width.getString()) || "fill_parent".equalsIgnoreCase(width.getString()))
			{
				layout.width = ViewGroup.LayoutParams.MATCH_PARENT;
				return true;
			}
			else if ("wrap_content".equalsIgnoreCase(width.getString()))
			{
				layout.width = ViewGroup.LayoutParams.WRAP_CONTENT;
				return true;
			}
		}
		return false;
	}
	
	private boolean  processHeightDimensionString(Value width, ViewGroup.LayoutParams layout)
	{
		if (width.isString())
		{
			if ("match_parent".equalsIgnoreCase(width.getString()) || "fill_parent".equalsIgnoreCase(width.getString()))
			{
				layout.height = ViewGroup.LayoutParams.MATCH_PARENT;
				return true;
			}
			else if ("wrap_content".equalsIgnoreCase(width.getString()))
			{
				layout.height = ViewGroup.LayoutParams.WRAP_CONTENT;
				return true;
			}
		}
		return false;
	}
	
	public void processHeight(ViewGroup.MarginLayoutParams layout, ViewGroup parent, HashMap<String, Value> key)
	{
		Value height = getMapValue(key, HEIGHT_KEYWORD);
		if (!height.isNull() && processHeightDimensionString(height, layout))
			return;
		Integer value = getInteger(key, HEIGHT_KEYWORD);
		
		
		int actualValue = defaultHeight; 
		if (value != null)
			actualValue = AndroidTools.convertDPtoPX(context, value.intValue());
		
		layout.height = actualValue;
	}
	
	public void processHeight(LinearLayout.LayoutParams layout, LinearLayout parent, HashMap<String, Value> key)
	{
		int actualValue = defaultHeight;
		
		Value height = getMapValue(key, HEIGHT_KEYWORD);
		
		if (height.isNull())
		{
			layout.height = defaultHeight;
			return;
		}
		else
		{
			if (parent!=null && parent.getOrientation() == LinearLayout.VERTICAL)
			{
				if (height.isString())
				{
					String percentage = height.getString();
					if (percentage.endsWith("%"))
					{
						String percentageValue = percentage.substring(0, percentage.length()-1);
						float percentageNum = Float.parseFloat(percentageValue);
						layout.weight = percentageNum/100F;
						layout.height = 0;
						parent.setWeightSum(1.0F);
						return;
					}
					else if (EQUAL_SPACE_WIDTH.equals(percentage))
					{
						layout.weight = 1;
						layout.height = 0;
						parent.setWeightSum(parent.getWeightSum() + 1);
						return;
					}
					
				}
				
			}
			
			if (processHeightDimensionString(height, layout))
				return;
			
			if (NLispTools.isNumericType(height))
			{
				actualValue = Double.valueOf(height.getFloatValue()).intValue();
				layout.height = AndroidTools.convertDPtoPX(context, actualValue);
				
			}
			else
				throw new EvaluateException("Invalid height: " + height);
		}
		
	}
	
	private Integer getInteger(HashMap<String, Value> keywords, String key)
	{
		Value pre = keywords.get(key);
		
		if (pre == null)
			return null;
		Number width = Double.valueOf(pre.getFloatValue());
		return width.intValue();
		
		
	}
	
	private Number getNumber(HashMap<String, Value> keywords, String key)
	{
		Value pre = keywords.get(key);
		
		if (pre == null)
			return null;
		
		return Double.valueOf(pre.getFloatValue());
	}
	
	
	
	private Long getLong(HashMap<String, Value> keywords, String key)
	{
		Value pre = keywords.get(key);
		try
		{
			if (pre == null)
				return null;
			Number width = Double.valueOf(pre.getFloatValue());
			return width.longValue();
		}
		catch (Exception e)
		{
			throw new EvaluateException("Invalid :width specification");
		}
		
	}
	
	public void setLispInterpreter(Environment env, AndroidLispInterpreter interpreter)
	{
		_lispInterpreter = interpreter;
		_currentEnv = env;
		processOnLongClickListenerKeys(_keys);
		processOnClickListenerKeys(_keys);
	}
	
	public void setView(View v)
	{
		encapsulated = v;
	}
	
	public abstract View createBaseView();
	
	
	public View createView(ViewGroup parent)
	{
		View base = encapsulated = createBaseView();
		encapsulated.setEnabled(_enabledP);
		ViewGroup.LayoutParams params = null;
		if (parent instanceof LinearLayout)
		{
			params = processKeywords((LinearLayout)parent, _keys);
			
		}
		else if (parent instanceof RelativeLayout)
		{
			params = processKeywords((RelativeLayout)parent, _keys);
		}
		else if (parent instanceof ScrollView || parent instanceof HorizontalScrollView || parent instanceof FrameLayout)
		{
			params = processKeywords((FrameLayout)parent, _keys);
		}
		
		base.setLayoutParams(params);
		baseUpdate(base);
		return base;
	}
	
	public View createView(ViewGroup parent, ViewGroup.LayoutParams topParams)
	{
		View base = encapsulated = createBaseView();
		
		ViewGroup.LayoutParams params = topParams;
		if (parent instanceof LinearLayout)
		{
			params = processKeywords((LinearLayout)parent, _keys);
			
		}
		else if (parent instanceof RelativeLayout)
		{
			params = processKeywords((RelativeLayout)parent, _keys);
		}
		else if (parent instanceof ScrollView)
		{
			params = processKeywords((FrameLayout)parent, _keys);
		}
		if (topParams!=null)
		{
			base.setLayoutParams(params);
			processBaseKeywords(_keys);
			processPaddingLeft(_keys);
			processPaddingRight(_keys);
			processPaddingTop(_keys);
			processPaddingBottom(_keys);
			processPadding(_keys);
		}
		baseUpdate(base);
		return base;
	}
	
	protected void processImageSourceKeyword(final View vw, String surl)
	{
		ImageLoader.getInstance().loadImage(surl, new ImageLoadingListener() {
			
			@Override
			public void onLoadingStarted(String arg0, View arg1) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void onLoadingFailed(String arg0, View arg1, FailReason arg2) {
				Log.e("__))", "Error loading background:" + arg0 + " -- " + arg2);
			}
			
			@SuppressLint("NewApi")
			@Override
			public void onLoadingComplete(String arg0, View arg1, Bitmap arg2) {
				//Log.e("__))", "Load complete:" + arg0 + " -- " + arg1 + " -- " + arg2);
				BitmapDrawable back = new BitmapDrawable(arg2);
				WindowManager m = (WindowManager)context.getSystemService(Context.WINDOW_SERVICE);
				DisplayMetrics metrics = new DisplayMetrics();
				m.getDefaultDisplay().getMetrics(metrics);
				back.setTargetDensity(metrics);
				back.setGravity(Gravity.FILL);
				if (Build.VERSION.SDK_INT<16)
					vw.setBackgroundDrawable(back);
				else
					vw.setBackground(back);
			}
			
			@Override
			public void onLoadingCancelled(String arg0, View arg1) {
				// TODO Auto-generated method stub
				
			}
		});
	}
	
	@SuppressLint("NewApi")
	protected void baseUpdate(View view)
	{
		view.setVisibility(visibility);
		if (_backgroundDrawable!=null)
		{
			if (Build.VERSION.SDK_INT<16)
				view.setBackgroundDrawable(_backgroundDrawable);
			else
				view.setBackground(_backgroundDrawable);
			
		}
		else
		if (backgroundColorDefinedP)
			view.setBackgroundColor(backgroundColor);
		else if (backgroundResourceId!=0)
		{
			view.setBackgroundResource(backgroundResourceId);
		}
		else if (backgroundResourceUrl!=null)
		{
			processImageSourceKeyword(view, backgroundResourceUrl);
		}
		view.setId(id);
		// TODO: Don't set padding for radio buttons for certain devices because this breaks ui on certain
		//		 phones, namely the HTC EVO 4G LTE.  Need a better way to handle this
		if (!DeviceBehaviorOverrides.radioPaddingBugP() ||  !(view instanceof RadioButton))
		{
			view.setPadding((leftPadding == -1)?view.getPaddingLeft():leftPadding,
							(topPadding == -1)?view.getPaddingTop():topPadding, 
							(rightPadding == -1)?view.getPaddingRight():rightPadding,
							(bottomPadding == -1)?view.getPaddingBottom():bottomPadding);
		}
		processOnClickListenerKeys(view, _keys);
		processOnLongClickListenerKeys(view, _keys);
	}
	
	public void applyAttribures(HashMap<String, Value> keywords)
	{
		for (String key:keywords.keySet())
		{
			_keys.put(key, keywords.get(key));
		}
		
		if (encapsulated!=null)
		{
			ViewGroup.LayoutParams baseParams = encapsulated.getLayoutParams();
			
			if (encapsulated.getParent() instanceof FrameLayout)
			{
				processWidth((FrameLayout.LayoutParams)baseParams, (FrameLayout)encapsulated.getParent(), _keys);
				processHeight((FrameLayout.LayoutParams)baseParams, (FrameLayout)encapsulated.getParent(), _keys);
			}
			else if (encapsulated.getParent() instanceof LinearLayout)
			{
				processWidth((LinearLayout.LayoutParams)baseParams, (LinearLayout)encapsulated.getParent(), _keys);
				processHeight((LinearLayout.LayoutParams)baseParams, (LinearLayout)encapsulated.getParent(), _keys);
			}
			else if (encapsulated.getParent() instanceof RelativeLayout)
			{
				processWidth((RelativeLayout.LayoutParams)baseParams, (RelativeLayout)encapsulated.getParent(), _keys);
				processHeight((RelativeLayout.LayoutParams)baseParams, (RelativeLayout)encapsulated.getParent(), _keys);
			}
			
			processBaseKeywords(_keys);
			processPaddingLeft(_keys);
			processPaddingRight(_keys);
			processPaddingTop(_keys);
			processPaddingBottom(_keys);
			processPadding(_keys);
			
			processMarginLeft(((baseParams instanceof LinearLayout.LayoutParams)?(LinearLayout.LayoutParams)baseParams:
				 ((baseParams instanceof RelativeLayout.LayoutParams)?(RelativeLayout.LayoutParams)baseParams:(FrameLayout.LayoutParams)baseParams)), _keys);
			processMarginRight(((baseParams instanceof LinearLayout.LayoutParams)?(LinearLayout.LayoutParams)baseParams:
				 ((baseParams instanceof RelativeLayout.LayoutParams)?(RelativeLayout.LayoutParams)baseParams:(FrameLayout.LayoutParams)baseParams)), _keys);
			processMarginTop(((baseParams instanceof LinearLayout.LayoutParams)?(LinearLayout.LayoutParams)baseParams:
				 ((baseParams instanceof RelativeLayout.LayoutParams)?(RelativeLayout.LayoutParams)baseParams:(FrameLayout.LayoutParams)baseParams)), _keys);
			processMarginBottom(((baseParams instanceof LinearLayout.LayoutParams)?(LinearLayout.LayoutParams)baseParams:
				 ((baseParams instanceof RelativeLayout.LayoutParams)?(RelativeLayout.LayoutParams)baseParams:(FrameLayout.LayoutParams)baseParams)), _keys);
			processMargin(((baseParams instanceof LinearLayout.LayoutParams)?(LinearLayout.LayoutParams)baseParams:
				 ((baseParams instanceof RelativeLayout.LayoutParams)?(RelativeLayout.LayoutParams)baseParams:(FrameLayout.LayoutParams)baseParams)), _keys);
			if (baseParams instanceof LinearLayout.LayoutParams)
				processParentAlignment((LinearLayout.LayoutParams)baseParams, _keys);
			else if (baseParams instanceof RelativeLayout.LayoutParams)
			{
				processParentAlignment((RelativeLayout.LayoutParams)baseParams, _keys);
			}
			else
				processParentAlignment((FrameLayout.LayoutParams)baseParams, _keys);
			baseUpdate(encapsulated);
			
		}
	}
	
	public Value getMapValue(HashMap<String, Value> map, String key)
	{
		Value result = map.get(key);
		if (result != null)
			return result;
		else
			return Environment.getNull();
	}
	
	public int getId()
	{
		return id;
	}
}
