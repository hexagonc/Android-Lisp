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
    public static final String SCALE_TYPE = ":scaleType";


	
	
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
		else if (!url.isNull() && url.isInteger())
		{
			int imageDrawable = (int)url.getIntValue();
			vw.setImageResource(imageDrawable);
		}
	}

    protected void processImageScaleTypeKeyword(HashMap<String, Value> keymap, ImageView vw)
    {
        Value scaleTypeValue = getMapValue(keymap, SCALE_TYPE);
        ImageView.ScaleType scale = ScaleType.FIT_XY;
        if (!scaleTypeValue.isNull() && scaleTypeValue.isString())
        {
            if ( ScaleType.valueOf(scaleTypeValue.getString().toUpperCase())!=null)
            {
                scale = ScaleType.valueOf(scaleTypeValue.getString().toUpperCase());
            }
        }

        vw.setScaleType(scale);
    }
	
	public View createBaseView()
	{
		ImageView tv = new ImageView(context);
		tv.setScaleType(ScaleType.FIT_XY);
		tv.setAdjustViewBounds(false);
		processImageSourceKeyword(_keys, tv);
        processImageScaleTypeKeyword(_keys, tv);
		return tv;
	}
	
	@Override
	public void applyAttribures(HashMap<String, Value> keywords)
	{
		super.applyAttribures(keywords);
		View actual;
		if (encapsulated != null && (actual = encapsulated.get())!= null)
			processImageSourceKeyword(_keys, (ImageView)actual);
	}
	
}