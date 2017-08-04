package com.evolved.automata.android.lisp.views;

import java.util.HashMap;
import java.util.LinkedList;

import android.view.View;
import android.view.ViewGroup;

import android.widget.RelativeLayout;
import android.widget.ScrollView;
import com.evolved.automata.lisp.Value;

import android.content.Context;

public class ScrollViewProxy extends ViewGroupProxy 
{
	
	int scrollTarget;
	public ScrollViewProxy(Context con, HashMap<String, Value> keywords)
	{
		super(con, keywords);
		
	}

	@Override
	public ViewGroup getPreconfiguredView()
	{
		ScrollView layout = new ScrollView(context);
		return layout;
		
	}


    @Override
    public void setScrollTargetY(int target, boolean smooth)
    {
        scrollTargetY = target;
        ScrollView actual = (ScrollView)encapsulated.get();
        if (actual !=null)
        {
            scrollTargetX = getView().getScrollX();
            if (smooth)
                actual.smoothScrollTo(scrollTargetX, scrollTargetY);
            else
                actual.scrollTo(scrollTargetX, scrollTargetY);

        }

    }

    @Override
    public void scrollByY(int amount, boolean smooth)
    {

        ScrollView actual = (ScrollView)encapsulated.get();
        if (actual !=null)
        {
            if (smooth)
                actual.smoothScrollBy(0, amount);
            else
                actual.scrollBy(0, amount);

        }
    }
	
}
