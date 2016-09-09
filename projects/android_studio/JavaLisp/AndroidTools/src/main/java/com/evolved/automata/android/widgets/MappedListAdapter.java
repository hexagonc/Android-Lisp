package com.evolved.automata.android.widgets;

import java.util.ArrayList;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

public class MappedListAdapter extends ArrayAdapter<MappedListView.MappedListItem> 
{
	private ArrayList<MappedListView.MappedListItem> _items;
	int _listResourceId = 0;
	int _rootViewId = 0;
	Context _context;
	public MappedListAdapter(Context context, int itemLayoutResourceId, int rootViewId, ArrayList<MappedListView.MappedListItem> items)
	{
		super(context, itemLayoutResourceId, items);
		_items = items;
		_context = context;
		_listResourceId = itemLayoutResourceId;
		_rootViewId = rootViewId;
	}
	
	public View getView(int position, View convertView, ViewGroup parent)
	{
		View v = convertView;
		if (v == null)
		{
			LayoutInflater li = (LayoutInflater)_context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			v = li.inflate(_listResourceId, null);
		}
		MappedListView.MappedListItem item = _items.get(position);
		
		if (item!=null)
		{
			TextView itemDescription = (TextView)v.findViewById(_rootViewId);
			itemDescription.setText(item.getDescription());
		}
		
		return v;
		
	}
}
