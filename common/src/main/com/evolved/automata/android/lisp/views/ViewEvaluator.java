package com.evolved.automata.android.lisp.views;

import java.util.HashMap;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.os.Handler;
import android.os.Looper;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.Toast;

import com.evolved.automata.KeyValuePair;
import com.evolved.automata.android.EvaluateException;
import com.evolved.automata.android.lisp.AndroidLispInterpreter;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.ExtendedFunctions;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
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
	
	
	
	public static void bindFunctions(final Environment env, final Context con, final AndroidLispInterpreter interpreter) 
	{

		if (!_imageConfiguredP)
		{
//			DisplayImageOptions defaultOptions = new DisplayImageOptions.Builder().cacheInMemory(true).build();
//			ImageLoaderConfiguration config = new ImageLoader
			
			ImageLoaderConfiguration config = ImageLoaderConfiguration.createDefault(con);
			ImageLoader.getInstance().init(config);
			_imageConfiguredP = true;
		}
		
		env.mapFunction("remove-view", remove_view(env, con, interpreter));
		
		env.mapFunction("show-view", show_view(env, con, interpreter));
		
		env.mapFunction("hide-view", hide_view(env, con, interpreter));
		
		
		env.mapFunction("set-background-color", set_background_color(env, con, interpreter));
		
		env.mapFunction("solid", solid(env, con, interpreter));
		
		
		env.mapFunction("get-text", get_text(env, con, interpreter));
		
		env.mapFunction("set-text", set_text(env, con, interpreter));
		
		env.mapFunction("image", image(env, con, interpreter));
		
		env.mapFunction("set-check-changed-listener", set_check_changed_listener(env, con, interpreter));
		
		env.mapFunction("is-checked", is_checked(env, con, interpreter));
		
		env.mapFunction("set-checked", set_checked(env, con, interpreter));
		
		
		env.mapFunction("checkbox", checkbox(env, con, interpreter));
		
		env.mapFunction("shadow-button", shadow_button(env, con, interpreter));
		
		env.mapFunction("button", button(env, con, interpreter));
		
		env.mapFunction("radio-button", radio_button(env, con, interpreter));
		
		env.mapFunction("edit", edit(env, con, interpreter));
		
		
		env.mapFunction("text", text(env, con, interpreter));
		
		env.mapFunction("horizontal-layout", horizontal_layout(env, con, interpreter));
		
		env.mapFunction("horizontal-radio-group", horizontal_radio_group(env, con, interpreter));
		
		env.mapFunction("vertical-radio-group", vertical_radio_group(env, con, interpreter));
		
		
		
		
		env.mapFunction("vertical-layout", vertical_layout(env, con, interpreter));
		
		env.mapFunction("relative", relative(env, con, interpreter));
		
		env.mapFunction("create-view", create_view(env, con, interpreter));
		
		env.mapFunction("show-short-toast", show_short_toast(env, con, interpreter));
		
		env.mapFunction("show-long-toast", show_long_toast(env, con, interpreter));
		
		env.mapFunction("scrollview", scrollview(env, con, interpreter));
		
		env.mapFunction("horizontal-scrollview", horizontal_scrollview(env, con, interpreter));
		
		env.mapFunction("update-parameters", update_parameters(env, con, interpreter));
		
		env.mapFunction("remove-all-views", remove_all_views(env, con, interpreter));
		env.mapFunction("remove-view", remove_view(env, con, interpreter));
		
		env.mapFunction("add-view", add_view(env, con, interpreter));
		
		env.mapFunction("dialog", dialog(env, con, interpreter));
		
		env.mapFunction("get-id", get_id(env, con, interpreter));
	}
	
	public static SimpleFunctionTemplate get_id(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{
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
	
	
	public static SimpleFunctionTemplate show_view(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{
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
				(new UIRunnable(new Handler(Looper.getMainLooper())) {
					
					@Override
					public void run() {
						tv.setVisiblity(View.VISIBLE);
					}
				}).post();
				return proxyArg;
			}
			
		};
	}
	
	
	
	public static SimpleFunctionTemplate text(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{
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
				proxy.setLispInterpreter(interpreter);
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	public static SimpleFunctionTemplate hide_view(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
				
				(new UIRunnable(new Handler(Looper.getMainLooper())) {
					
					@Override
					public void run() {
						vp.setVisiblity(View.INVISIBLE);
					}
				}).post();
				return proxyArg;
			}
			
		};
	}
	
	public static SimpleFunctionTemplate set_background_color(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{
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
					
					(new UIRunnable(new Handler(Looper.getMainLooper())) {
						
						@Override
						public void run() {
							tv.setBackgroundColor(textArg.getString());
						}
					}).post();
					return proxyArg;
				}
				catch (Exception e)
				{
					throw new EvaluateException("Invalid color Value");
				}
				
			}
			
		};
	}
	
	public static SimpleFunctionTemplate solid(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
				proxy.setLispInterpreter(interpreter);
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	public static SimpleFunctionTemplate get_text(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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

	public static SimpleFunctionTemplate set_text(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
					
					(new UIRunnable(new Handler(Looper.getMainLooper())) {
						
						@Override
						public void run() {
							tv.setText(textArg.getString());
						}
					}).post();
					return proxyArg;
				}
				catch (Exception e)
				{
					throw new EvaluateException("Invalid Values for set-text");
				}
				
			}
			
		};
	}
	
	public static SimpleFunctionTemplate image(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
				proxy.setLispInterpreter(interpreter);
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	public static SimpleFunctionTemplate set_check_changed_listener(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
	
	public static SimpleFunctionTemplate is_checked(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
	
	public static SimpleFunctionTemplate set_checked(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
	
	
	public static SimpleFunctionTemplate checkbox(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
				proxy.setLispInterpreter(interpreter);
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	public static SimpleFunctionTemplate radio_button(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
				proxy.setLispInterpreter(interpreter);
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	
	public static SimpleFunctionTemplate shadow_button(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
				proxy.setLispInterpreter(interpreter);
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	public static SimpleFunctionTemplate button(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
				proxy.setLispInterpreter(interpreter);
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	
	public static SimpleFunctionTemplate edit(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
				proxy.setLispInterpreter(interpreter);
				return ExtendedFunctions.makeValue(proxy);
			}
			
		};
	}
	
	public static SimpleFunctionTemplate vertical_layout(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{
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
				proxy.setLispInterpreter(interpreter);
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
	
	
	public static SimpleFunctionTemplate horizontal_layout(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
				proxy.setLispInterpreter(interpreter);
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
	
	public static SimpleFunctionTemplate horizontal_radio_group(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
				proxy.setLispInterpreter(interpreter);
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
	
	public static SimpleFunctionTemplate vertical_radio_group(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{
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
				proxy.setLispInterpreter(interpreter);
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
	
	
	public static SimpleFunctionTemplate relative(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
				proxy.setLispInterpreter(interpreter);
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
	
	public static SimpleFunctionTemplate create_view(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
	
	public static SimpleFunctionTemplate show_short_toast(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
	    		
	    		(new UIRunnable(new Handler(Looper.getMainLooper())) {
					
					@Override
					public void run() {
						Toast.makeText(con, text, Toast.LENGTH_SHORT).show();
					}
				}).post();
	    		
	    		
	    		return NLispTools.makeValue(text);
			}
		}
			
		;
	}
	
	public static SimpleFunctionTemplate show_long_toast(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
	    		
	    		(new UIRunnable(new Handler(Looper.getMainLooper())) {
					
					@Override
					public void run() {
						Toast.makeText(con, text, Toast.LENGTH_LONG).show();
					}
				}).post();
	    		
	    		
	    		return NLispTools.makeValue(text);
			}
		};
	}
	
	public static SimpleFunctionTemplate scrollview(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
				proxy.setLispInterpreter(interpreter);
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
	
	
	public static SimpleFunctionTemplate horizontal_scrollview(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
				proxy.setLispInterpreter(interpreter);
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
	
	public static SimpleFunctionTemplate update_parameters(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
	    		(new UIRunnable(new Handler(Looper.getMainLooper())) {
					
					@Override
					public void run() {
						
						View view = proxy.getView();
						if (requestLayout && view != null )
							view.requestLayout();
					}
				}).post();
	    		
				
				return evaluatedArgs[0];
			}
		};
	}
	
	public static SimpleFunctionTemplate remove_all_views(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
				
	    		
	    		(new UIRunnable(new Handler(Looper.getMainLooper())) {
					
					@Override
					public void run() {
						((ViewGroup)proxy.getView()).removeAllViews();
					}
				}).post();
	    		
				return evaluatedArgs[0];
			}
		};
	}
	
	public static SimpleFunctionTemplate remove_view(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
				
				final ViewGroupProxy proxy = (ViewGroupProxy)evaluatedArgs[0].getObjectValue();
				final ViewProxy child = (ViewProxy)evaluatedArgs[1].getObjectValue();
				boolean uiThread = evaluatedArgs.length> 2 && !evaluatedArgs[2].isNull();
				
				if (uiThread)
				{
					(new UIRunnable(new Handler(Looper.getMainLooper())) {
						
						@Override
						public void run() {
							proxy.removeView(child);
						}
					}).post();
				}
				else
					proxy.removeView(child);
	    		
				return evaluatedArgs[1];
			}
		};
	}
	
	public static SimpleFunctionTemplate add_view(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
				boolean uiThread = evaluatedArgs.length> 2 && !evaluatedArgs[2].isNull();
				
				if (uiThread)
				{
					(new UIRunnable(new Handler(Looper.getMainLooper())) {
						
						@Override
						public void run() {
							proxy.addChild(child);
						}
					}).post();
				}
				else
					proxy.addChild(child);
	    		
				return evaluatedArgs[1];
			}
		};
	}
	
	public static SimpleFunctionTemplate dialog(final Environment env, final Context con, final AndroidLispInterpreter interpreter)
	{
		return new SimpleFunctionTemplate()
		{

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
