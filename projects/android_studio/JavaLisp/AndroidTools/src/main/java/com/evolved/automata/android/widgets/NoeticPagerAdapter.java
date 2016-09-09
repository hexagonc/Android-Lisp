package com.evolved.automata.android.widgets;

import java.util.ArrayList;


import android.content.Context;
import android.support.v4.view.PagerAdapter;
import android.view.View;
import android.view.ViewGroup;

public class NoeticPagerAdapter <T extends ViewPagerConfigurator> extends PagerAdapter 
{
	ArrayList<T> _configurators;
	Context _context;
	
	public NoeticPagerAdapter(Context context, ArrayList<T> list)
	{
		_context = context;
		_configurators = list;
		ViewPagerConfigurator configurator;
		for (int i = 0;i<_configurators.size();i++)
		{
			configurator = _configurators.get(i);
			configurator.updatePos(i);
			
		} 
	}
	
	public void clearData()
	{
		ViewPagerConfigurator configurator;
		for (int i = 0;i<_configurators.size();i++)
		{
			configurator = _configurators.get(i);
			configurator.markAsDeleted();
		}
		notifyDataSetChanged();
		_configurators.clear();
	}
	
	@Override
	public int getCount() {
		
		return _configurators.size();
	}

	@Override
	public boolean isViewFromObject(View view, Object object) {
		ViewPagerConfigurator configurator = (ViewPagerConfigurator)object;
		return configurator.isMyView(view);
	}

	@Override
	public void destroyItem(ViewGroup container, int position, Object object) {
		ViewPagerConfigurator configurator = (ViewPagerConfigurator)object;
		configurator.destroyView(container);
	}

	
	@Override
	public Object instantiateItem(ViewGroup container, int position) {
		ViewPagerConfigurator configurator = _configurators.get(position);
		configurator.addView(container);
		return configurator;
		
	}

	@Override
	public int getItemPosition(Object object) {
		ViewPagerConfigurator configurator = (ViewPagerConfigurator)object;
		return configurator.getPosition();
	}
	
	public void notifyDataSetChanged()
	{
		super.notifyDataSetChanged();
	}
	
	public boolean removePage(int position)
	{
		if (position<_configurators.size())
		{
			ViewPagerConfigurator configurator;
			for (int i = 0;i<_configurators.size();i++)
			{
				configurator = _configurators.get(i);
				if (i<position)
					configurator.updatePos(i);
				else if (i == position)
					configurator.markAsDeleted();
				else
					configurator.markPositionAsChanged();
			}
			
			_configurators.remove(position);
			notifyDataSetChanged();
			return true;
		}
		else
			return false;
	}
	
	public  void addPage(T newConfigurator)
	{
		ViewPagerConfigurator configurator;
		for (int i = 0;i<_configurators.size();i++)
		{
			configurator = _configurators.get(i);
			configurator.updatePos(i);
			
		}
		newConfigurator.updatePos(_configurators.size());
		_configurators.add(newConfigurator);
		notifyDataSetChanged();
	}
}
