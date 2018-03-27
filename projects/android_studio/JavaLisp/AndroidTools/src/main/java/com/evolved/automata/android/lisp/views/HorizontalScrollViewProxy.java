package com.evolved.automata.android.lisp.views;

import java.util.HashMap;

import com.evolved.automata.lisp.Value;

import android.content.Context;
import android.util.Log;
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

        HorizontalScrollView actual;
        if (encapsulated != null && ((actual = (HorizontalScrollView)encapsulated.get())!=null))
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
        View actual;
        if (encapsulated != null && ((actual = encapsulated.get()) != null))
        {
            Log.d(".;;.;.;.;.;.;.;.", "Scroling by " + amount);
            HorizontalScrollView hz = (HorizontalScrollView)actual;
            if (smooth)
                hz.smoothScrollBy(amount, 0);
            else
                hz.scrollBy(amount, 0);
        }


    }
}
