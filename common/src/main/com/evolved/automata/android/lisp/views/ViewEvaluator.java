package com.evolved.automata.android.lisp.views;

import java.util.HashMap;


import android.app.Activity;
import android.app.AlertDialog;
import android.app.Activity;
import android.content.DialogInterface;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.GradientDrawable;
import android.graphics.drawable.LayerDrawable;
import android.graphics.drawable.ShapeDrawable;
import android.os.Handler;
import android.os.Looper;
import android.view.View;
import android.view.ViewGroup;
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

import com.evolved.automata.lisp.Value;
import com.nostra13.universalimageloader.core.DisplayImageOptions;
import com.nostra13.universalimageloader.core.ImageLoader;
import com.nostra13.universalimageloader.core.ImageLoaderConfiguration;

public class ViewEvaluator  {

	
	
	public static final String ON_POSITIVE_BUTTON_CLICK_KEY = ":onPositive";
	public static final String ON_CANCEL_BUTTON_CLICK_KEY = ":onCancel";
	
	public static final String DIALOG_TITLE_KEY = ":dialog-title";
	public static final String DIALOG_MESSAGE_KEY = ":dialog-message";
	
	public static final String DIALOG_POSITIVE_BUTTON_TEXT = ":positive-text";
	public static final String DIALOG_CANCEL_BUTTON_TEXT = ":cancel-text";
	
	static boolean _imageConfiguredP = false;
	
	private static Value[] getEvaluatedValues(Value[] v)
	{
		return v;
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
		
		env.mapFunction("remove-view", remove_view(env, activity, interpreter));
		
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
		
		env.mapFunction("horizontal-scrollview", horizontal_scrollview(env, activity, interpreter));
		
		env.mapFunction("update-parameters", update_parameters(env, activity, interpreter));
		
		env.mapFunction("remove-all-views", remove_all_views(env, activity, interpreter));
		env.mapFunction("remove-view", remove_view(env, activity, interpreter));
		
		env.mapFunction("add-view", add_view(env, activity, interpreter));
		
		env.mapFunction("dialog", dialog(env, activity, interpreter));
		
		env.mapFunction("get-id", get_id(env, activity, interpreter));
		
		env.mapFunction("log", log(env, activity, interpreter));
		
		env.mapFunction("create-border", create_border(env, activity, interpreter));
		
		env.mapFunction("create-shadow-background", shadow_background(env, activity, interpreter));
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
	
	public static ViewFunctionTemplate create_border(final Environment env, final Activity con, final AndroidLispInterpreter interpreter)
	{
		return new ViewFunctionTemplate()
		{
			@SuppressWarnings("unchecked")
			@Override
			public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
			{
				return (T)create_border(env, con, interpreter);
			}

			@Override
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = kv.GetKey();
				HashMap<String, Value> keys = kv.GetValue();
				
				Drawable border = con.getResources().getDrawable(R.drawable.border);
				return ExtendedFunctions.makeValue(border);
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
				final ViewProxy tv = (TextViewProxy)proxyArg.getObjectValue();
				tv.setVisiblity(View.VISIBLE);
				
				return continuationReturn(proxyArg);
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
				final ViewProxy vp = (ViewProxy)proxyArg.getObjectValue();
				
				vp.setVisiblity(View.INVISIBLE);
				return continuationReturn(proxyArg);
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
					return continuationReturn(proxyArg);
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
					return continuationReturn(proxyArg);
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
				String listener = evaluatedArgs[1].getString();
				proxy.setOnCheckChangedListener(listener);
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
				return evaluatedArgs[1];
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
				if (requestLayout && view != null)
				{
					view.requestLayout();
					return continuationReturn(evaluatedArgs[0]);
				}
				else
					return evaluatedArgs[0];
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
	    		if (view != null)
	    		{
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
				if (parentV != null && childV != null)
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
				if (parentV != null && childV != null)
				{
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
			public Value evaluate(Environment env, Value[] args) {
				KeyValuePair<Value[], HashMap<String, Value>> kv = NLispTools.getPartitionValues(args);
				Value[] evaluatedArgs = getEvaluatedValues(kv.GetKey());
				HashMap<String, Value> keys = kv.GetValue();
				
				
				final String title = keys.get(DIALOG_TITLE_KEY).getString();
				final String message = keys.get(DIALOG_MESSAGE_KEY).getString();
				
				final String onPositiveClick = keys.get(ON_POSITIVE_BUTTON_CLICK_KEY).getString();
				final String onCancelClick = keys.get(ON_CANCEL_BUTTON_CLICK_KEY).getString();
				
				final String positiveButtonText = keys.get(DIALOG_POSITIVE_BUTTON_TEXT).getString();
				final String cancelButtonText = keys.get(DIALOG_CANCEL_BUTTON_TEXT).getString();
				
				AlertDialog.Builder builder = new AlertDialog.Builder(con);
				if (title!=null)
					builder.setTitle(title);
				
				if (positiveButtonText!=null && onPositiveClick != null)
				{
					builder.setPositiveButton(positiveButtonText, new DialogInterface.OnClickListener() {
						
						@Override
						public void onClick(DialogInterface dialog, int which) {
							interpreter.evaluateExpression(onPositiveClick, true);
							dialog.dismiss();
						}
					});
				}
				
				if (cancelButtonText!=null && onCancelClick != null)
				{
					builder.setPositiveButton(cancelButtonText, new DialogInterface.OnClickListener() {
						
						@Override
						public void onClick(DialogInterface dialog, int which) {
							interpreter.evaluateExpression( onCancelClick, true);
							dialog.dismiss();
						}
					});
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

				return NLispTools.makeValue(1);
			}
		};
	}
	
}
