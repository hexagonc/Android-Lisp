package com.evolved.automata.android.lisp.views;

import java.util.HashMap;
import java.util.LinkedList;
import com.evolved.automata.lisp.Value;
import android.content.Context;

import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;

import com.evolved.automata.android.EvaluateException;


public class LinearLayoutViewProxy extends ViewGroupProxy
{
	
	int orientation;
	int gravity;
	
	public static final String CHILD_ALIGNMENT = ":child-align";
	
	
	public LinearLayoutViewProxy(Context con, HashMap<String, Value> keymap, int oorientation)
	{
		super(con, keymap);
		orientation = oorientation;
		children = new LinkedList<ViewProxy>();
	}
	
	
	public void processChildAlignment(HashMap<String, Value> keymap)
	{
		Value align = getMapValue(keymap, CHILD_ALIGNMENT);
		
		if (!align.isNull())
		{
			try
			{
				String salignment = align.getString();
				gravity = 0;
				if (salignment.equalsIgnoreCase("left"))
					gravity = Gravity.LEFT;
				else if (salignment.equalsIgnoreCase("right"))
					gravity = Gravity.RIGHT;
				else if (salignment.equalsIgnoreCase("top"))
					gravity = Gravity.TOP;
				else if (salignment.equalsIgnoreCase("bottom"))
					gravity = Gravity.BOTTOM;
				else if (salignment.equalsIgnoreCase("center"))
					gravity = Gravity.CENTER;
				else
					throw new EvaluateException("Invalid child alignment spec: " + align);
			}
			catch (Exception e)
			{
				throw new EvaluateException("Invalid child alignment spec: " + align);
			}
		}	
	}
	
	@Override
	public ViewGroup getPreconfiguredView()
	{
		LinearLayout layout = new LinearLayout(context);
		processChildAlignment(_keys);
		layout.setOrientation(orientation);
		layout.setGravity(gravity);
		
		return layout;
	}
	
	
	
}
