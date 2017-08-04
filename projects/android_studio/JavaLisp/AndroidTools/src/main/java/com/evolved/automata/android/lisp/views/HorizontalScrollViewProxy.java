package com.evolved.automata.android.lisp.views;

import java.util.HashMap;

import com.evolved.automata.lisp.Value;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.HorizontalScrollView;
import android.widget.ScrollView;


public class HorizontalScrollViewProxy extends ViewGroupProxy
{

	int scrollTarget = 0;
	public HorizontalScrollViewProxy(Context con, HashMap<String, Value> keywords)
	{
		super(con, keywords);
		
	}

	@Override
	public ViewGroup getPreconfiguredView()
	{
		HorizontalScrollView layout = new HorizontalScrollView(context);
		return layout;
		
	}

    public void setScrollTargetX(int target, boolean smooth)
    {
        scrollTargetX = target;
        HorizontalScrollView actual = (HorizontalScrollView)encapsulated.get();
        if (actual !=null)
        {
            scrollTargetY = getView().getScrollY();
            if (smooth)
                actual.smoothScrollTo(scrollTargetX, scrollTargetY);
            else
                actual.scrollTo(scrollTargetX, scrollTargetY);

        }

    }

    public void scrollByX(int amount, boolean smooth)
    {
        HorizontalScrollView actual = (HorizontalScrollView)encapsulated.get();
        if (actual !=null)
        {
            if (smooth)
                actual.smoothScrollBy(amount, 0);
            else
                actual.scrollBy(amount, 0);

        }

    }
}
