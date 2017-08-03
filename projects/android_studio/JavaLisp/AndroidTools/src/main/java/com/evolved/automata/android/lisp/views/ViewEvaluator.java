package com.evolved.automata.android.lisp.views;

import java.util.ArrayList;
import java.util.HashMap;

import org.apache.commons.lang3.tuple.Triple;


import android.app.Activity;
import android.app.AlertDialog;
import android.app.Activity;
import android.content.Context;
import android.content.DialogInterface;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.GradientDrawable;
import android.graphics.drawable.LayerDrawable;
import android.graphics.drawable.ShapeDrawable;
import android.graphics.drawable.StateListDrawable;
import android.os.Handler;
import android.os.Looper;
import android.util.DisplayMetrics;
import android.util.Log;
import android.util.StateSet;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewParent;
import android.view.WindowManager;
import android.widget.ArrayAdapter;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.Toast;

import com.evolved.automata.KeyValuePair;
import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.android.AppStateManager;
import com.evolved.automata.android.EvaluateException;
import com.evolved.automata.android.lisp.AndroidLispInterpreter;
import com.evolved.automata.android.tools.R;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SignalValue;
import com.evolved.automata.lisp.SimpleFunctionTemplate;

import com.evolved.automata.lisp.Value;
import com.nostra13.universalimageloader.core.DisplayImageOptions;
import com.nostra13.universalimageloader.core.ImageLoader;
import com.nostra13.universalimageloader.core.ImageLoaderConfiguration;

public class ViewEvaluator  {

	
	
	public static final String ON_POSITIVE_BUTTON_CLICK_KEY = ":on-positive";
	public static final String ON_CANCEL_BUTTON_CLICK_KEY = ":on-cancel";
	
	public static final String DIALOG_TITLE_KEY = ":dialog-title";
	public static final String DIALOG_MESSAGE_KEY = ":dialog-message";
	
	public static final String DIALOG_POSITIVE_BUTTON_TEXT = ":positive-text";
	public static final String DIALOG_CANCEL_BUTTON_TEXT = ":cancel-text";
	
	static boolean _imageConfiguredP = false;
	
	public static final String BATCH_UI_UPDATE_PROPERTY_NAME = "BATCH_UI_CHANGES_PROPERTY_NAME";
	
	private static Value[] getEvaluatedValues(Value[] v)
	{
		return v;
	}
	
	private static boolean getBatchUIUpdates(Environment env)
	{
		Value v = env.getEnvironmentProperty(BATCH_UI_UPDATE_PROPERTY_NAME);
		return v != null && !v.isNull();
	}
	
	private static boolean setBatchUIUpdates(Environment env, boolean enable)
	{
		env.mapEnvironmentProperty(BATCH_UI_UPDATE_PROPERTY_NAME, NLispTools.makeValue(enable));
		
		return enable;
	}
	
	public static void bindFunctions(final Environment env, final Activity activity, final AndroidLispInterpreter interpreter) 
	{

		if (!_imageConfiguredP)
		{
//			DisplayImageOptions defaultOptions = new DisplayImageOptions.Builder().cacheInMemory(true).build();
//			ImageLoaderConfiguration config = new ImageLoader
			
			ImageLoaderConfiguration config = ImageLoaderConfiguration.createDefault(activity);
			ImageLoader.getInstance().init(config);
			_imageConfiguredP = true;
		}
		

		
		env.mapFunction("show-view", show_view(env, activity, interpreter));
		
		env.mapFunction("hide-view", hide_view(env, activity, interpreter));
		
		
		env.mapFunction("set-background-color", set_background_color(env, activity, interpreter));
		
		env.mapFunction("solid", solid(env, activity, interpreter));
		
		
		env.mapFunction("get-text", get_text(env, activity, interpreter));
		
		env.mapFunction("set-text", set_text(env, activity, interpreter));
		
		env.mapFunction("image", image(env, activity, interpreter));
		
		env.mapFunction("set-check-changed-listener", set_check_changed_listener(env, activity, interpreter));
		
		env.mapFunction("is-checked", is_checked(env, activity, interpreter));
		
		env.mapFunction("set-checked", set_checked(env, activity, interpreter));
		
		
		env.mapFunction("checkbox", checkbox(env, activity, interpreter));
		
		env.mapFunction("shadow-button", shadow_button(env, activity, interpreter));
		
		env.mapFunction("button", button(env, activity, interpreter));
		
		env.mapFunction("radio-button", radio_button(env, activity, interpreter));
		
		env.mapFunction("edit", edit(env, activity, interpreter));
		
		
		env.mapFunction("text", text(env, activity, interpreter));
		
		env.mapFunction("horizontal-layout", horizontal_layout(env, activity, interpreter));
		
		env.mapFunction("horizontal-radio-group", horizontal_radio_group(env, activity, interpreter));
		
		env.mapFunction("vertical-radio-group", vertical_radio_group(env, activity, interpreter));
		
		
		
		
		env.mapFunction("vertical-layout", vertical_layout(env, activity, interpreter));
		
		env.mapFunction("relative", relative(env, activity, interpreter));
		
		env.mapFunction("create-view", create_view(env, activity, interpreter));
		
		env.mapFunction("show-short-toast", show_short_toast(env, activity, interpreter));
		
		env.mapFunction("show-long-toast", show_long_toast(env, activity, interpreter));
		
		env.mapFunction("scrollview", scrollview(env, activity, interpreter));
		
		env.mapFunction("spinner", spinner(env, activity, interpreter));
		
		env.mapFunction("seek-bar", seekBar(env, activity, interpreter));
		
		env.mapFunction("horizontal-scrollview", horizontal_scrollview(env, activity, interpreter));
		
		env.mapFunction("update-parameters", update_parameters(env, activity, interpreter));
		
		env.mapFunction("remove-all-views", remove_all_views(env, activity, interpreter));
		env.mapFunction("remove-view", remove_view(env, activity, interpreter));
		
		env.mapFunction("add-view", add_view(env, activity, interpreter));
		
		env.mapFunction("dialog", dialog(env, activity, interpreter));
		env.mapFunction("dismiss-dialog", dismiss_dialog(env, activity, interpreter));
		
		env.mapFunction("get-id", get_id(env, activity, interpreter));
		
		env.mapFunction("log", log(env, activity, interpreter));
		
		env.mapFunction("create-background", create_background(env, activity, interpreter));
		
		env.mapFunction("create-shadow-background", shadow_background(env, activity, interpreter));
		
		env.mapFunction("set-enabled", set_enabled(env, activity, interpreter));
		
		env.mapFunction("set-spinner-items", set_spinner_items(env, activity, interpreter));
		
		env.mapFunction("set-selected-spinner-item", set_selected_spinner_item(env, activity, interpreter));
		
		env.mapFunction("set-batch-ui-updates", set_batch_ui_updates());
		
		env.mapFunction("set-seek-value", setSeekValue());
		env.mapFunction("get-view-height", get_view_height(activity));
		env.mapFunction("get-view-width", get_view_width(activity));
	}

	private static int convertPixelsToDP(Context con, int pixels)
	{
		DisplayMetrics dm = new DisplayMetrics();
		WindowManager m = (WindowManager)con.getSystemService(Context.WINDOW_SERVICE);
		m.getDefaultDisplay().getMetrics(dm);

		double dpi = dm.densityDpi;
		int dp = (int)(pixels*160/dpi);
		return dp;
	}


	public static SimpleFunctionTemplate get_view_height(final Context con)
	{
		return new SimpleFunctionTemplate()
		{


			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)get_view_height(con);
			}

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);

				ViewProxy proxy = (ViewProxy)evaluatedArgs[0].getObjectValue();
				View actualView = proxy.getView();
				if (actualView != null)
				{
					int pixelHeight = actualView.getHeight();

					return NLispTools.makeValue(convertPixelsToDP(con, pixelHeight)) ;
				}
				else
					return Environment.getNull();

			}

		};
	}

	public static SimpleFunctionTemplate get_view_width(final Context con)
	{
		return new SimpleFunctionTemplate()
		{


			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)get_view_width(con);
			}

			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);

				ViewProxy proxy = (ViewProxy)evaluatedArgs[0].getObjectValue();
				View actualView = proxy.getView();
				if (actualView != null)
				{
					int pixelWidth = actualView.getWidth();

					return NLispTools.makeValue(convertPixelsToDP(con, pixelWidth)) ;
				}
				else
					return Environment.getNull();

			}

		};
	}

	
	public static SimpleFunctionTemplate set_batch_ui_updates()
	{
		return new SimpleFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)set_batch_ui_updates();
			}
			
			@Override
			public Value evaluate(Environment env,Value[] evaluatedArgs) {
				checkActualArguments(1, false, true);
				
				return NLispTools.makeValue(setBatchUIUpdates(env, !evaluatedArgs[0].isNull())) ;
			}
			
		};
	}
	
	public static ViewFunctionTemplate set_enabled(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)set_enabled(env, con, interpreter);
			}

			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				
				ViewProxy proxy = (ViewProxy)evaluatedArgs[0].getObjectValue();
				boolean enabled = !evaluatedArgs[1].isNull();
				proxy.setEnabled(enabled);
				if (proxy.getView()!=null && !getBatchUIUpdates(env))
					return continuationReturn(evaluatedArgs[0]);
				else
					return evaluatedArgs[0];
			}
			
		};
	}
	
	public static ViewFunctionTemplate shadow_background(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)shadow_background(env, con, interpreter);
			}

			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = kv.GetKey();
				HashMap<String, Value> keys = kv.GetValue();
				
				Value shadow_width = keys.get(":shadow-width");
				Value shadow_color = keys.get(":shadow-color");
				Value foreground_color = keys.get(":foreground-color");
				Value angle = keys.get(":shadow-angle");
				
				
				LayerDrawable shadow = (LayerDrawable)con.getResources().getDrawable(R.drawable.shadow);
				GradientDrawable shadowLayer = (GradientDrawable)shadow.getDrawable(0);
				GradientDrawable topLayer = (GradientDrawable)shadow.getDrawable(1);
				if (shadow_width != null)
				{
					
					int width = (int)shadow_width.getIntValue();
					double rads = angle.getFloatValue()/180*Math.PI;
					double bx = width*Math.cos(rads);
					double by = width*Math.sin(rads);
					
					// shadow inset
					shadow.setLayerInset(0, 
										AndroidTools.convertDPtoPX(con, (int)Math.max(0, bx)), 
										AndroidTools.convertDPtoPX(con, (int)Math.max(0, -by)),
										AndroidTools.convertDPtoPX(con, (int)Math.max(0, -bx)),
										AndroidTools.convertDPtoPX(con, (int)Math.max(0, by)));
					shadow.setLayerInset(1,
										 AndroidTools.convertDPtoPX(con, (int)Math.max(0, -bx)),
										 AndroidTools.convertDPtoPX(con, (int)Math.max(0, by)),
										 AndroidTools.convertDPtoPX(con, (int)Math.max(0, bx)), 
										 AndroidTools.convertDPtoPX(con, (int)Math.max(0, -by)));
				}
				if (foreground_color != null)
				{
					int color = 0;
					if (foreground_color.isString())
					{
						color = Color.parseColor(foreground_color.getString());
					}
					else
						color = (int)foreground_color.getIntValue();
					topLayer.setColor(color);
					//topLayer.getPaint().setColor(color);
				}
				
				if (shadow_color != null)
				{
					int color = 0;
					if (shadow_color.isString())
					{
						color = Color.parseColor(shadow_color.getString());
					}
					else
						color = (int)foreground_color.getIntValue();
					//shadowLayer.getPaint().setColor(color);
					shadowLayer.setColor(color);
				}
				
				return ExtendedFunctions.makeValue(shadow);
			}
			
		};
	}
	
	public static ViewFunctionTemplate create_background(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)create_background(env, con, interpreter);
			}

			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = kv.GetKey();
				HashMap<String, Value> keys = kv.GetValue();
				
				int widthAround = 0;
				
				Value width = keys.get(":border-width");
				if (width != null)
					widthAround = AndroidTools.convertDPtoPX(con, (int)width.getIntValue());
				
				int leftWidth = widthAround;
				int rightWidth = widthAround;
				int topWidth = widthAround;
				int bottomWidth = widthAround;
				
				float cornersAll = 0;
				if (keys.containsKey(":corner-radius"))
					cornersAll = AndroidTools.convertDPtoPX(con, (int)keys.get(":corner-radius").getIntValue());
				
				// TODO: add support for separate corner radii 
				
				float cornerRadiusTL = 0;
				float cornerRadiusTR = 0;
				float cornerRadiusBR = 0;
				float cornerRadiusBL = 0;
				
				
				Value borderColor = keys.get(":border-color");
				Value foregroundColor = keys.get(":foreground-color");
				
				
				
				Value left = keys.get(":border-left-width");
				Value right = keys.get(":border-right-width");
				Value top = keys.get(":border-top-width");
				Value bottom = keys.get(":border-bottom-width");
				
				Value startColor = keys.get(":foreground-start-color");
				Value endColor = keys.get(":foreground-stop-color");
				Value gradientDirection = keys.get(":foreground-gradient-type");
				
				if (left != null)
				{
					leftWidth = AndroidTools.convertDPtoPX(con, (int)left.getIntValue());
				}
				
				if (right != null)
				{
					rightWidth = AndroidTools.convertDPtoPX(con, (int)right.getIntValue());
				}
				
				if (top != null)
				{
					topWidth = AndroidTools.convertDPtoPX(con, (int)top.getIntValue());
				}
				
				if (bottom!=null)
				{
					bottomWidth = AndroidTools.convertDPtoPX(con, (int)bottom.getIntValue());
				}
				
				
				
				GradientDrawable borderLayout = new GradientDrawable();
				GradientDrawable topLayer = null;
				
				if (startColor!=null && endColor != null && gradientDirection != null)
				{
					int start = 0, stop = 0;
					if (startColor.isString())
					{
						start = Color.parseColor(startColor.getString());
					}
					else
						start = (int)startColor.getIntValue();
					
					
					if (endColor.isString())
					{
						stop = Color.parseColor(endColor.getString());
					}
					else
						stop = (int)endColor.getIntValue();
					
					String orientation = gradientDirection.getString();
					
					topLayer = new GradientDrawable(GradientDrawable.Orientation.valueOf(orientation), new int[]{start, stop});
				}
				else
					topLayer = new GradientDrawable();
				
				LayerDrawable border = new LayerDrawable(new GradientDrawable[]{borderLayout, topLayer});
				if (cornersAll>0)
				{
					
					borderLayout.setCornerRadius(cornersAll);
					topLayer.setCornerRadius(cornersAll);
				}
				
				if (foregroundColor != null)
				{
					int color = 0;
					if (foregroundColor.isString())
					{
						color = Color.parseColor(foregroundColor.getString());
					}
					else
						color = (int)foregroundColor.getIntValue();
					topLayer.setColor(color);
					
				}
				
				if (borderColor != null)
				{
					int color = 0;
					if (borderColor.isString())
					{
						color = Color.parseColor(borderColor.getString());
					}
					else
						color = (int)borderColor.getIntValue();
					
					borderLayout.setColor(color);
				}
				border.setLayerInset(1, leftWidth, topWidth, rightWidth, bottomWidth);
				
				Value finalValue = ExtendedFunctions.makeValue(border);
				// Handle states
				
				Value onDisabledValue = keys.get(":on-disabled-drawable");
				Value onPressedValue = keys.get(":on-pressed-drawable");
				
				if ((onDisabledValue!=null && !onDisabledValue.isNull()) ||
						(onPressedValue !=null && !onPressedValue.isNull()))
				{
					
					StateListDrawable finalDrawable = new StateListDrawable();
					
					if (onDisabledValue!=null)
					{
						Drawable onDisabledDrawable = (Drawable)onDisabledValue.getObjectValue();
						
						finalDrawable.addState(new int[]{ -android.R.attr.state_enabled}, onDisabledDrawable);
					}
					
					if (onPressedValue !=null )
					{
						Drawable onPressedDrawable = (Drawable)onPressedValue.getObjectValue();
						finalDrawable.addState(new int[]{android.R.attr.state_pressed}, onPressedDrawable);
					}
					finalDrawable.addState(StateSet.WILD_CARD, border);
					finalValue = ExtendedFunctions.makeValue(finalDrawable);
				}
				
						
				return finalValue;
			}
			
		};
	}
	
	
	public static ViewFunctionTemplate get_id(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)get_id(env, con, interpreter);
			}

			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = kv.GetKey();
				
				Value proxyArg = evaluatedArgs[0];
				final ViewProxy vp = (ViewProxy)proxyArg.getObjectValue();
				
				return NLispTools.makeValue(vp.getId());
			}
			
		};
	}
	
	
	public static ViewFunctionTemplate show_view(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)show_view(env, con, interpreter);
			}

			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				
				Value proxyArg = evaluatedArgs[0];
				final ViewProxy vp = (ViewProxy)proxyArg.getObjectValue();
                vp.setVisiblity(View.VISIBLE);
				if (vp.getView()!=null  && !getBatchUIUpdates(env))
					return continuationReturn(proxyArg);
				else
					return proxyArg;
			}
			
		};
	}
	
	
	
	public static ViewFunctionTemplate text(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)text(env, con, interpreter);
			}

			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				HashMap<String, Value> keys = kv.GetValue();
				
				
				
				Value text = NLispTools.makeValue("");
				if (evaluatedArgs.length>0)
					text = evaluatedArgs[0];
				TextViewProxy proxy = new TextViewProxy(con, keys, text.getString());
				proxy.setLispInterpreter(env, interpreter);
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	public static ViewFunctionTemplate hide_view(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)hide_view(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				
				Value proxyArg = evaluatedArgs[0];

				boolean shouldBeGoneP = (evaluatedArgs.length > 1 && !evaluatedArgs[1].isNull());
				final ViewProxy vp = (ViewProxy)proxyArg.getObjectValue();
				
				if (shouldBeGoneP)
					vp.setVisiblity(View.GONE);
				else
					vp.setVisiblity(View.INVISIBLE);
				
				if (vp.getView()!=null  && !getBatchUIUpdates(env))
					return continuationReturn(proxyArg);
				else
					return proxyArg;
			}
			
		};
	}
	
	public static ViewFunctionTemplate set_background_color(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)set_background_color(env, con, interpreter);
			}

			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				
				try
				{
					Value proxyArg = evaluatedArgs[0];
					final Value textArg = evaluatedArgs[1];
					final ViewProxy tv = (ViewProxy)proxyArg.getObjectValue();
					
					tv.setBackgroundColor(textArg.getString());
					if (tv.getView()!=null  && !getBatchUIUpdates(env))
						return continuationReturn(proxyArg);
					else
						return proxyArg;
				}
				catch (Exception e)
				{
					throw new EvaluateException("Invalid color Value");
				}
				
			}
			
		};
	}
	
	public static ViewFunctionTemplate solid(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)solid(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
			
				HashMap<String, Value> keys = kv.GetValue();
				
				
				SolidViewProxy proxy = new SolidViewProxy(con, keys);
				proxy.setLispInterpreter(env, interpreter);
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	public static ViewFunctionTemplate get_text(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)get_text(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				
				try
				{
					Value proxyArg = evaluatedArgs[0];
					
					TextViewProxy tv = (TextViewProxy)proxyArg.getObjectValue();
					String text = tv.getText();
					Value textArg = NLispTools.makeValue(text);
					return textArg;
				}
				catch (Exception e)
				{
					throw new EvaluateException("Invalid Values for get-text");
				}
				
			}
			
		};
	}

	public static ViewFunctionTemplate set_text(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)set_text(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				
				try
				{
					Value proxyArg = evaluatedArgs[0];
					final Value textArg = evaluatedArgs[1];
					final TextViewProxy tv = (TextViewProxy)proxyArg.getObjectValue();
					
					tv.setText(textArg.getString());
					if (tv.getView()!=null && !getBatchUIUpdates(env))
						return continuationReturn(proxyArg);
					else
						return proxyArg;
				}
				catch (Exception e)
				{
					throw new EvaluateException("Invalid Values for set-text");
				}
				
			}
			
		};
	}
	
	public static ViewFunctionTemplate image(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)image(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				
				HashMap<String, Value> keys = kv.GetValue();
				
				ImageViewProxy proxy = new ImageViewProxy(con, keys);
				proxy.setLispInterpreter(env, interpreter);
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	public static ViewFunctionTemplate set_check_changed_listener(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)set_check_changed_listener(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				
				
				CheckboxViewProxy proxy = (CheckboxViewProxy)evaluatedArgs[0].getObjectValue();
				Value onChangeLambda = evaluatedArgs[1];
				proxy.setOnCheckChangedListener(onChangeLambda);
				return evaluatedArgs[1];
			}
			
		};
	}
	
	public static ViewFunctionTemplate is_checked(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)is_checked(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = kv.GetKey();
				
				
				CheckboxViewProxy proxy = (CheckboxViewProxy)evaluatedArgs[0].getObjectValue();
				
				return NLispTools.makeValue(proxy.isChecked());
			}
			
		};
	}
	
	public static ViewFunctionTemplate set_checked(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)set_checked(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = kv.GetKey();
				
				
				CheckboxViewProxy proxy = (CheckboxViewProxy)evaluatedArgs[0].getObjectValue();
				proxy.setChecked(!evaluatedArgs[1].isNull());
				if (proxy.getView()!=null  && !getBatchUIUpdates(env))
					return continuationReturn(evaluatedArgs[0]);
				else
					return evaluatedArgs[0];
			}
			
		};
	}
	
	
	public static ViewFunctionTemplate checkbox(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)checkbox(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				HashMap<String, Value> keys = kv.GetValue();
				
				
				Value text = evaluatedArgs[0];
				CheckboxViewProxy proxy = new CheckboxViewProxy(con, keys, text.getString());
				proxy.setLispInterpreter(env, interpreter);
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	public static ViewFunctionTemplate radio_button(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)radio_button(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = kv.GetKey();
				HashMap<String, Value> keys = kv.GetValue();
				
				
				Value text = evaluatedArgs[0];
				RadioButtonProxy proxy = new RadioButtonProxy(con, keys, text.getString());
				proxy.setLispInterpreter(env, interpreter);
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	
	public static ViewFunctionTemplate shadow_button(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)shadow_button(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				HashMap<String, Value> keys = kv.GetValue();
				
				
				Value text = evaluatedArgs[0];
				ShadowButtonProxy proxy = new ShadowButtonProxy(con, keys, text.getString());
				proxy.setLispInterpreter(env, interpreter);
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	public static ViewFunctionTemplate button(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)button(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				HashMap<String, Value> keys = kv.GetValue();
				
				
				Value text = evaluatedArgs[0];
				ButtonViewProxy proxy = new ButtonViewProxy(con, keys, text.getString());
				proxy.setLispInterpreter(env, interpreter);
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	
	public static ViewFunctionTemplate edit(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)edit(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				HashMap<String, Value> keys = kv.GetValue();
				
				
				Value text = NLispTools.makeValue("");
				if (evaluatedArgs.length>0)
					text = evaluatedArgs[0];
				EditViewProxy proxy = new EditViewProxy(con, keys, text.getString());
				proxy.setLispInterpreter(env, interpreter);
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	public static ViewFunctionTemplate vertical_layout(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)vertical_layout(env, con, interpreter);
			}

			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				HashMap<String, Value> keys = kv.GetValue();
				
				
				LinearLayoutViewProxy proxy = new LinearLayoutViewProxy(con, keys, LinearLayout.VERTICAL);
				proxy.setLispInterpreter(env, interpreter);
				for (Value input:evaluatedArgs)
				{
					if (input.isUserObject() && input.getObjectValue() instanceof ViewProxy)
					{
						proxy.addChild((ViewProxy)input.getObjectValue());
					}
					else if (input.isList())
					{
						for (Value sub:input.getList())
						{
							if (sub.isUserObject() && sub.getObjectValue() instanceof ViewProxy)
							{
								proxy.addChild((ViewProxy)sub.getObjectValue());
							}
						}
					}
				}
				
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	
	public static ViewFunctionTemplate horizontal_layout(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)horizontal_layout(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				HashMap<String, Value> keys = kv.GetValue();
				
				
				LinearLayoutViewProxy proxy = new LinearLayoutViewProxy(con, keys, LinearLayout.HORIZONTAL);
				proxy.setLispInterpreter(env, interpreter);
				for (Value input:evaluatedArgs)
				{
					if (input.isUserObject() && input.getObjectValue() instanceof ViewProxy)
					{
						proxy.addChild((ViewProxy)input.getObjectValue());
					}
					else if (input.isList())
					{
						for (Value sub:input.getList())
						{
							if (sub.isUserObject() && sub.getObjectValue() instanceof ViewProxy)
							{
								proxy.addChild((ViewProxy)sub.getObjectValue());
							}
						}
					}
				}
				
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	public static ViewFunctionTemplate horizontal_radio_group(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)horizontal_radio_group(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = kv.GetKey();
				HashMap<String, Value> keys = kv.GetValue();
				
				
				RadioGroupProxy proxy = new RadioGroupProxy(con, keys, LinearLayout.HORIZONTAL);
				proxy.setLispInterpreter(env, interpreter);
				for (Value input:evaluatedArgs)
				{
					if (input.isUserObject() && input.getObjectValue() instanceof ViewProxy)
					{
						proxy.addChild((ViewProxy)input.getObjectValue());
					}
					else if (input.isList())
					{
						for (Value sub:input.getList())
						{
							if (sub.isUserObject() && sub.getObjectValue() instanceof ViewProxy)
							{
								proxy.addChild((ViewProxy)sub.getObjectValue());
							}
						}
					}
				}
				
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	public static ViewFunctionTemplate vertical_radio_group(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)vertical_radio_group(env, con, interpreter);
			}

			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = kv.GetKey();
				HashMap<String, Value> keys = kv.GetValue();
				
				
				RadioGroupProxy proxy = new RadioGroupProxy(con, keys, LinearLayout.VERTICAL);
				proxy.setLispInterpreter(env, interpreter);
				for (Value input:evaluatedArgs)
				{
					if (input.isUserObject() && input.getObjectValue() instanceof ViewProxy)
					{
						proxy.addChild((ViewProxy)input.getObjectValue());
					}
					else if (input.isList())
					{
						for (Value sub:input.getList())
						{
							if (sub.isUserObject() && sub.getObjectValue() instanceof ViewProxy)
							{
								proxy.addChild((ViewProxy)sub.getObjectValue());
							}
						}
					}
				}
				
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	
	public static ViewFunctionTemplate relative(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)relative(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				HashMap<String, Value> keys = kv.GetValue();
				
				
				RelativeLayoutViewProxy proxy = new RelativeLayoutViewProxy(con, keys);
				proxy.setLispInterpreter(env, interpreter);
				for (Value input:evaluatedArgs)
				{
					if (input.isUserObject() && input.getObjectValue() instanceof ViewProxy)
					{
						proxy.addChild((ViewProxy)input.getObjectValue());
					}
					else if (input.isList())
					{
						for (Value sub:input.getList())
						{
							if (sub.isUserObject() && sub.getObjectValue() instanceof ViewProxy)
							{
								proxy.addChild((ViewProxy)sub.getObjectValue());
							}
						}
					}
				}
				
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	public static ViewFunctionTemplate create_view(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)create_view(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				HashMap<String, Value> keys = kv.GetValue();
				
				
				ViewProxy proxy = (ViewProxy)evaluatedArgs[0].getObjectValue();
				
				View output = proxy.createView((LinearLayout)null, new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT ));
				
				return ExtendedFunctions.makeValue(output);
			}
			
		};
	}
	
	public static ViewFunctionTemplate show_short_toast(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)show_short_toast(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args)
			{
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				
	    		final String text = evaluatedArgs[0].getString();
	    		
	    		Toast.makeText(con, text, Toast.LENGTH_SHORT).show();
	    		return continuationReturn(evaluatedArgs[0]);
	    		
			}
		}
			
		;
	}
	
	public static ViewFunctionTemplate show_long_toast(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)show_long_toast(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args)
			{
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				
	    		final String text = evaluatedArgs[0].getString();
	    		
	    		Toast.makeText(con, text, Toast.LENGTH_LONG).show();
	    		
	    		return continuationReturn(evaluatedArgs[0]);
	    		
			}
		};
	}
	
	public static ViewFunctionTemplate scrollview(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)scrollview(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args)
			{
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				HashMap<String, Value> keys = kv.GetValue();
				
				
				ScrollViewProxy proxy = new ScrollViewProxy(con, keys);
				proxy.setLispInterpreter(env, interpreter);
				for (Value input:evaluatedArgs)
				{
					if (input.isUserObject() && input.getObjectValue() instanceof ViewProxy)
					{
						proxy.addChild((ViewProxy)input.getObjectValue());
					}
					else if (input.isList())
					{
						for (Value sub:input.getList())
						{
							if (sub.isUserObject() && sub.getObjectValue() instanceof ViewProxy)
							{
								proxy.addChild((ViewProxy)sub.getObjectValue());
							}
						}
					}
				}
				
				return ExtendedFunctions.makeValue(proxy);
			}
		};
	}
	
	
	public static ViewFunctionTemplate horizontal_scrollview(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)horizontal_scrollview(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args)
			{
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				HashMap<String, Value> keys = kv.GetValue();
				
				
				HorizontalScrollViewProxy proxy = new HorizontalScrollViewProxy(con, keys);
				proxy.setLispInterpreter(env, interpreter);
				for (Value input:evaluatedArgs)
				{
					if (input.isUserObject() && input.getObjectValue() instanceof ViewProxy)
					{
						proxy.addChild((ViewProxy)input.getObjectValue());
					}
					else if (input.isList())
					{
						for (Value sub:input.getList())
						{
							if (sub.isUserObject() && sub.getObjectValue() instanceof ViewProxy)
							{
								proxy.addChild((ViewProxy)sub.getObjectValue());
							}
						}
					}
				}
				
				return ExtendedFunctions.makeValue(proxy);
			}
		};
	}
	
	public static ViewFunctionTemplate update_parameters(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)update_parameters(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args)
			{
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				final HashMap<String, Value> keys = kv.GetValue();
				
				final ViewProxy proxy = (ViewProxy)evaluatedArgs[0].getObjectValue();
				final boolean requestLayout = evaluatedArgs.length > 1 && !evaluatedArgs[1].isNull();
				proxy.applyAttribures(keys);
				View view = proxy.getView();
				if (requestLayout && view != null && !getBatchUIUpdates(env))
				{
					ViewParent vp = view.getParent();
					if (vp !=null)
					{
						((View)vp).refreshDrawableState();
						((View)vp).requestLayout();
						((View)vp).invalidate();
					}
					
					
					return continuationReturn(evaluatedArgs[0]);
				}
				else
				{
					return evaluatedArgs[0];
				}
			}
		};
	}
	
	public static ViewFunctionTemplate remove_all_views(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)remove_all_views(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args)
			{
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				
				final ViewGroupProxy proxy = (ViewGroupProxy)evaluatedArgs[0].getObjectValue();
				
	    		ViewGroup view = (ViewGroup)proxy.getView();
	    		proxy.removeAllViews();
	    		if (view != null && !getBatchUIUpdates(env))
	    		{
	    			view.requestLayout();
	    			return continuationReturn(evaluatedArgs[0]);
	    		}
	    		else
	    			return evaluatedArgs[0];
	    		
			}
		};
	}
	
	public static ViewFunctionTemplate log(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)log(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args)
			{
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				String tag = evaluatedArgs[0].getString();
				StringBuilder message = new StringBuilder();
				for (int i=1; i< evaluatedArgs.length; i++)
				{
					Value v = evaluatedArgs[i];
					if (v.isString())
						message.append(v.getString());
					else
						message.append(v.toString());
				}
				
	    		AppStateManager.getInstance().simpleMessage(tag, message.toString());
				return evaluatedArgs[1];
			}
		};
	}
	
	
	public static ViewFunctionTemplate remove_view(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)remove_view(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args)
			{
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				
				ViewGroupProxy proxy = (ViewGroupProxy)evaluatedArgs[0].getObjectValue();
				ViewProxy child = (ViewProxy)evaluatedArgs[1].getObjectValue();
				
				ViewGroup parentV = (ViewGroup)proxy.getView();
				View childV = (View)child.getView();
				proxy.removeView(child);
				if (parentV != null && childV != null && !getBatchUIUpdates(env))
				{
					return continuationReturn(evaluatedArgs[1]);
				}
				else
					return evaluatedArgs[1];
			}
		};
	}
	
	public static ViewFunctionTemplate add_view(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)add_view(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args)
			{
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				
				final ViewGroupProxy proxy = (ViewGroupProxy)evaluatedArgs[0].getObjectValue();
				final ViewProxy child = (ViewProxy)evaluatedArgs[1].getObjectValue();
				ViewGroup parentV = (ViewGroup)proxy.getView();
				View childV = (View)child.getView();
				
				proxy.addChild(child);
				if (parentV != null && childV != null&& !getBatchUIUpdates(env))
				{
					parentV.requestLayout();
					return continuationReturn(evaluatedArgs[1]);
				}
				else
					return evaluatedArgs[1];
				
			}
		};
	}
	
	public static ViewFunctionTemplate dialog(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)dialog(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(final Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				HashMap<String, Value> keys = kv.GetValue();
				
				
				final String title = (keys.containsKey(DIALOG_TITLE_KEY))?keys.get(DIALOG_TITLE_KEY).getString():null;
				final String message = (keys.containsKey(DIALOG_MESSAGE_KEY))?keys.get(DIALOG_MESSAGE_KEY).getString():null;
				
				final Value onPositiveClick = (keys.containsKey(ON_POSITIVE_BUTTON_CLICK_KEY))?keys.get(ON_POSITIVE_BUTTON_CLICK_KEY):null;
				final Value onCancelClick = (keys.containsKey(ON_CANCEL_BUTTON_CLICK_KEY))?keys.get(ON_CANCEL_BUTTON_CLICK_KEY):null;
				
				final String positiveButtonText = (keys.containsKey(DIALOG_POSITIVE_BUTTON_TEXT))?keys.get(DIALOG_POSITIVE_BUTTON_TEXT).getString():null;
				final String cancelButtonText = (keys.containsKey(DIALOG_CANCEL_BUTTON_TEXT))?keys.get(DIALOG_CANCEL_BUTTON_TEXT).getString():null;
				
				AlertDialog.Builder builder = new AlertDialog.Builder(con);
				if (title!=null)
					builder.setTitle(title);
				
				if (positiveButtonText!=null)
				{
					
					if (onPositiveClick != null)
					{
						if (onPositiveClick.isString())
						{
							builder.setPositiveButton(positiveButtonText, new DialogInterface.OnClickListener() {
								
								@Override
								public void onClick(DialogInterface dialog, int which) {
									Value out = interpreter.evaluateExpression(onPositiveClick.getString(), true);
									dialog.dismiss();
								}
							});
						}
						else
						{
							final Value transformed = NLispTools.getMinimalEnvironment(env, onPositiveClick); 
							
							builder.setPositiveButton(positiveButtonText, new DialogInterface.OnClickListener() {
								
								@Override
								public void onClick(DialogInterface dialog, int which) {
									Value out = interpreter.evaluatePreParsedValue(env, transformed, true);
									dialog.dismiss();
								}
							});
						}
					}
					else
					{
						builder.setPositiveButton(positiveButtonText, new DialogInterface.OnClickListener() {
							
							@Override
							public void onClick(DialogInterface dialog, int which) {
								dialog.dismiss();
							}
						});
					}
					
				}
				
				if (cancelButtonText!=null)
				{
					
					if (onCancelClick != null)
					{
						if (onCancelClick.isString())
						{
							builder.setNegativeButton(cancelButtonText, new DialogInterface.OnClickListener() {
								
								@Override
								public void onClick(DialogInterface dialog, int which) {
									Value out = interpreter.evaluateExpression(onCancelClick.getString(), true);
									dialog.dismiss();
								}
							});
						}
						else
						{
							final Value transformed = NLispTools.getMinimalEnvironment(env, onCancelClick); 
							
							builder.setNegativeButton(cancelButtonText, new DialogInterface.OnClickListener() {
								
								@Override
								public void onClick(DialogInterface dialog, int which) {
									Value out = interpreter.evaluatePreParsedValue(env, transformed, true);
									dialog.dismiss();
								}
							});
						}
					}
					else
					{
						builder.setNegativeButton(cancelButtonText, new DialogInterface.OnClickListener() {
							
							@Override
							public void onClick(DialogInterface dialog, int which) {
								dialog.dismiss();
							}
						});
					}
					
				}
				
				if (message!=null)
					builder.setMessage(message);
				else
				{
					if (evaluatedArgs!=null && evaluatedArgs.length > 0)
					{
						ViewProxy proxy = (ViewProxy)evaluatedArgs[0].getObjectValue();
						RelativeLayout rel = new RelativeLayout(con);
						
						View out = proxy.createView(rel);
						rel.addView(out);
						
						builder.setView(rel);
					}
				}
				AlertDialog dialog = builder.create();
				dialog.show();

				return ExtendedFunctions.makeValue(dialog);
			}
		};
	}
	
	
	public static ViewFunctionTemplate dismiss_dialog(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)dismiss_dialog(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				AlertDialog dialog = (AlertDialog)evaluatedArgs[0].getObjectValue();
				dialog.dismiss();

				return ExtendedFunctions.makeValue(dialog);
			}
		};
	}
	
	public static ViewFunctionTemplate set_spinner_items(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)set_selected_spinner_item(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				
				SpinnerViewProxy proxy = (SpinnerViewProxy)evaluatedArgs[0].getObjectValue();
				
				Value[] viewSpec = evaluatedArgs[1].getList(); 
				ArrayList<Triple<ViewProxy, ViewProxy, FunctionTemplate>> spinnerSpecList = new ArrayList<Triple<ViewProxy, ViewProxy, FunctionTemplate>>();
				
				ViewProxy listDropdownView = null;
				ViewProxy selectedItemView = null;
				FunctionTemplate selectLambda = null;
				for (Value choiceItem:viewSpec)
				{
					listDropdownView = (ViewProxy)choiceItem.getList()[0].getObjectValue();
					selectedItemView = (ViewProxy)choiceItem.getList()[1].getObjectValue();
					selectLambda = choiceItem.getList()[2].getLambda();
					
					spinnerSpecList.add(Triple.of(listDropdownView, selectedItemView, selectLambda));
				}
				
				proxy.updateSpec(spinnerSpecList);
				if (proxy.getView()!=null && !getBatchUIUpdates(env))
					return continuationReturn(evaluatedArgs[0]);
				else
					return evaluatedArgs[0];
			}
			
		};
	}
	
	
	public static ViewFunctionTemplate set_selected_spinner_item(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)set_selected_spinner_item(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				
				SpinnerViewProxy proxy = (SpinnerViewProxy)evaluatedArgs[0].getObjectValue();
				int index = (int)evaluatedArgs[1].getIntValue();
				
				proxy.setSelected(index);
				Log.d("->->->", "set-selected-spinner-item" + index);
				if (proxy.getView()!=null && !getBatchUIUpdates(env))
					return continuationReturn(evaluatedArgs[0]);
				else
					return evaluatedArgs[0];
			}
			
		};
	}
	
	public static ViewFunctionTemplate spinner(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)spinner(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				
				Value[] viewSpec = evaluatedArgs[0].getList(); 
				ArrayList<Triple<ViewProxy, ViewProxy, FunctionTemplate>> spinnerSpecList = new ArrayList<Triple<ViewProxy, ViewProxy, FunctionTemplate>>();
				
				ViewProxy listDropdownView = null;
				ViewProxy selectedItemView = null;
				FunctionTemplate selectLambda = null;
				for (Value choiceItem:viewSpec)
				{
					listDropdownView = (ViewProxy)choiceItem.getList()[0].getObjectValue();
					selectedItemView = (ViewProxy)choiceItem.getList()[1].getObjectValue();
					selectLambda = choiceItem.getList()[2].getLambda();
					
					spinnerSpecList.add(Triple.of(listDropdownView, selectedItemView, selectLambda));
				}
						
				SpinnerViewProxy proxy = new SpinnerViewProxy(con, kv.GetValue(), spinnerSpecList);
				proxy.setLispInterpreter(env, interpreter);

				return ExtendedFunctions.makeValue(proxy);
			}
		};
	}
	
	public static ViewFunctionTemplate seekBar(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)seekBar(env, con, interpreter);
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				
				 
				Value minValue = evaluatedArgs[0];
				Value maxValue = evaluatedArgs[1];
				
						
				SeekBarViewProxy proxy = new SeekBarViewProxy(con, kv.GetValue(), minValue.getFloatValue(), maxValue.getFloatValue());
				proxy.setLispInterpreter(env, interpreter);
				
				if (evaluatedArgs.length>2)
				{
					Value currentValue = evaluatedArgs[2];
					proxy.setCurrentValue(currentValue.getFloatValue());
				}
				return ExtendedFunctions.makeValue(proxy);
			}
		};
	}
	
	
	public static ViewFunctionTemplate setSeekValue()
	{
		return new ViewFunctionTemplate()
		{

			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)setSeekValue();
			}
			
			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
					
				SeekBarViewProxy proxy = (SeekBarViewProxy)evaluatedArgs[0].getObjectValue();
				Value value = evaluatedArgs[1];
				
				proxy.setCurrentValue(value.getFloatValue());
				return ExtendedFunctions.makeValue(proxy);
			}
		};
	}
	
}
