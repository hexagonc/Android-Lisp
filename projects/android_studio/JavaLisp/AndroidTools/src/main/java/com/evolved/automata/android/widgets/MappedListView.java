package com.evolved.automata.android.widgets;


import java.util.ArrayList;

import android.content.Context;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.LinearLayout;
import android.widget.ListView;
import com.evolved.automata.android.tools.R;

public class MappedListView extends ListView
{
	String[][] _descriptionKeys;
	
	ArrayList<MappedListItem> _items;
	
	
	public static interface OnSelectListener
	{
		public void onItemSelected(String key);
		
	}
	
	public static class MappedListItem
	{
		String _description;
		String _key;
		
		public MappedListItem(String description, String key)
		{
			_description = description;
			_key = key;
		}
		
		public String getDescription()
		{
			return _description;
		}
		
		public String getKey()
		{
			return _key;
		}
	}
	
	MappedListAdapter _adapter;
	
	OnSelectListener _listener;
	
	public MappedListView(Context context, String[][] mappedValues)
	{
		super(context);
		_descriptionKeys = mappedValues;
		
		MappedListItem item;
		
		_items = new ArrayList<MappedListItem>();
		
		for (String[] descripKey:mappedValues)
		{
			item = new MappedListItem(descripKey[0], descripKey[1]);
			_items.add(item);
		}
		
		_adapter = new MappedListAdapter(context, R.layout.mapped_listview_item, R.id.mapped_item_top_textview, _items);
		
		setOnItemClickListener(new AdapterView.OnItemClickListener() {

			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position,
					long id) 
			{
				MappedListItem selected =  _items.get(position);
				if (_listener!=null)
					_listener.onItemSelected(selected.getKey());
				
			}
			
		});
		
		
	}
	
	public void setOnSelectListener(OnSelectListener listener)
	{
		_listener = listener;
	}
	
	
}
