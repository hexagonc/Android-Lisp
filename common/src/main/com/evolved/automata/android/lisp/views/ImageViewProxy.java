package com.evolved.automata.android.lisp.views;

import java.util.HashMap;


import android.graphics.Bitmap;
import android.graphics.drawable.BitmapDrawable;
import android.view.View;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import com.evolved.automata.lisp.Value;
import android.content.Context;
import com.nostra13.universalimageloader.core.ImageLoader;

public class ImageViewProxy extends ViewProxy
{
	public static final String SOURCE = ":src"; // string url for image
	
	
	public ImageViewProxy(Context con, HashMap<String, Value> keymap)
	{
		super(con, keymap);
	}
	
	protected void processImageSourceKeyword(HashMap<String, Value> keymap, ImageView vw)
	{
		Value url = getMapValue(keymap, SOURCE);
		if (!url.isNull() && url.isString())
		{
			String surl = url.getString();
			ImageLoader.getInstance().displayImage(surl, vw);
		}
	}
	
	
	public View createBaseView()
	{
		ImageView tv = new ImageView(context);
		tv.setScaleType(ScaleType.FIT_XY);
		tv.setAdjustViewBounds(false);
		processImageSourceKeyword(_keys, tv);
		return tv;
	}
	
	@Override
	public void applyAttribures(HashMap<String, Value> keywords)
	{
		super.applyAttribures(keywords);
		if (encapsulated!=null)
			processImageSourceKeyword(_keys, (ImageView)encapsulated);
	}
	
}