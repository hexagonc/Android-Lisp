package com.evolved.automata.android.widgets;
import java.util.ArrayList;

import com.evolved.automata.android.tools.R;
import com.evolved.automata.android.widgets.MappedListView.MappedListItem;
import com.evolved.automata.android.widgets.MappedListView.OnSelectListener;


import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.graphics.*;
import android.util.AttributeSet;
import android.view.View;
import android.view.View.MeasureSpec;
import android.widget.LinearLayout;
import android.os.*;


public class GraphicTools 
{
	
	public static void showNoeticListDialog(Context context, String title, String onCancelString,String[][] descriptionMap, final OnSelectListener listener)
	{
		showNoeticListDialog(context, title, onCancelString, descriptionMap, R.layout.mapped_listview_item, R.id.mapped_item_top_textview, listener);
	}
	
	public static void showNoeticListDialog(Context context, String title, final String onCancelString, String[][] descriptionMap, int listItemLayout, int listViewRootId, final OnSelectListener listener)
	{
		
		MappedListItem item;
		
		final ArrayList<MappedListItem> _items = new ArrayList<MappedListItem>();
		
		for (String[] descripKey:descriptionMap)
		{
			item = new MappedListItem(descripKey[0], descripKey[1]);
			_items.add(item);
		}
		
		MappedListAdapter adapter = new MappedListAdapter(context, listItemLayout, listViewRootId, _items);
		
		//AlertDialog.Builder builder = new AlertDialog.Builder(context, android.R.style.Theme_Dialog);
		AlertDialog.Builder builder = new AlertDialog.Builder(context);
		builder.setAdapter(adapter, new DialogInterface.OnClickListener() {
			
			@Override
			public void onClick(DialogInterface dialog, int which) {
				MappedListItem item = _items.get(which);
				String key = item.getKey();
				if (listener!=null)
					listener.onItemSelected(key);
				
			}
		});
		
		builder.setOnCancelListener(new DialogInterface.OnCancelListener() {
			
			@Override
			public void onCancel(DialogInterface dialog) {
				
				if (listener!=null)
					listener.onItemSelected(onCancelString);
			}
		});
		builder.setTitle(title);
		builder.show();
	}
	
	
	public static Bitmap rollBitmapHorizontally(Bitmap bitmap, int width, int height, Paint paint, int offset, boolean rotateLeftP)
	{
		if (offset == 0)
			return bitmap;
		Bitmap newBitmap = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
		Canvas surface = new Canvas(newBitmap);
		int remaining = Math.min(width, width - offset);
		
		Bitmap oldLeftPart = null, oldRightPart = null;
		
		if (rotateLeftP)
		{
			// when rotating to the left, a part offset pixels width wraps from the left side of the bitmap to right
			oldLeftPart = Bitmap.createBitmap(bitmap, 0, 0, offset, height);
			oldRightPart = Bitmap.createBitmap(bitmap, offset, 0, remaining, height);
			surface.drawBitmap(oldRightPart, 0,0, paint);
			surface.drawBitmap(oldLeftPart, remaining, 0, paint);
		}
		else
		{
			oldLeftPart = Bitmap.createBitmap(bitmap, 0, 0, remaining, height);
			oldRightPart = Bitmap.createBitmap(bitmap, remaining, 0, offset, height);
			surface.drawBitmap(oldRightPart, 0,0, paint);
			surface.drawBitmap(oldLeftPart, offset, 0, paint);
		}
		
		return newBitmap;
	}
	
	public static Bitmap rollBitmapVertically(Bitmap bitmap, int width, int height, Paint paint, int offset, boolean rotateUpP)
	{
		if (offset == 0)
			return bitmap;
		Bitmap newBitmap = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
		Canvas surface = new Canvas(newBitmap);
		int remaining = Math.min(height, height - offset);
		
		Bitmap oldTopPart = null, oldBottomPart = null;
		
		if (rotateUpP)
		{
			// when rotating up, a part offset pixels height wraps from the top of the bitmap to the bottom
			oldTopPart = Bitmap.createBitmap(bitmap, 0, 0, width, offset);
			oldBottomPart = Bitmap.createBitmap(bitmap, 0, offset, width, remaining);
			
			surface.drawBitmap(oldBottomPart, 0,0, paint);
			surface.drawBitmap(oldTopPart, 0, remaining, paint);
		}
		else
		{
			oldTopPart = Bitmap.createBitmap(bitmap, 0, 0, width, remaining);
			oldBottomPart = Bitmap.createBitmap(bitmap, 0, remaining, width, offset);
			surface.drawBitmap(oldBottomPart, 0,0, paint);
			surface.drawBitmap(oldTopPart, 0, offset, paint);
		}
		
		return newBitmap;
	}
	
	
	public static Bitmap getTotalImage(LinearLayout layout, Paint paint)
	{
		int orientation = layout.getOrientation();
		if (orientation == LinearLayout.HORIZONTAL)
			return getHorizontalLinearLayoutRenderImage(layout, paint);
		else
			return getVerticalLinearLayoutRenderImage(layout, paint);
	}
	
//	public static Bitmap getViewBitmap(View view, Paint paint)
//	{
//		view.measure(MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED), MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED));
//		int height = view.getMeasuredHeight();
//		int width  = view.getMeasuredWidth();
//		Bitmap bitmap = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
//		Canvas surface = new Canvas(bitmap);
//		view.draw(surface);
//		return bitmap;
//	}
	
	private static Bitmap getHorizontalLinearLayoutRenderImage(LinearLayout layout, Paint paint)
	{
		View child;
		int width = 0;
		
		int childCount = layout.getChildCount();
		int totalWidth = 0, totalHeight = 0;
		int[] heights = new int[childCount];
		int[] widths = new int[childCount];
		
		for (int i=0;i < childCount; i++)
		{
			child = layout.getChildAt(i);
			child.measure(MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED), MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED));
			heights[i] = child.getMeasuredHeight();
			widths[i] = width = child.getMeasuredWidth();
			totalWidth +=width;
			if (heights[i] > totalHeight)
				totalHeight=heights[i];
		}
		
		Bitmap bitmap = Bitmap.createBitmap(totalWidth, totalHeight, Bitmap.Config.ARGB_8888), temp;
		
		Canvas surface = new Canvas(bitmap), tempSurface;
		int x = 0;
		
		for (int i=0; i< childCount; i++)
		{
			child = layout.getChildAt(i);
			child.layout(0, 0, widths[i], heights[i]);
			temp = Bitmap.createBitmap(widths[i], heights[i], Bitmap.Config.ARGB_8888);
			tempSurface = new Canvas(temp);
			child.draw(tempSurface);
			surface.drawBitmap(temp, x, 0, paint);
			x+=widths[i];
		}
		return bitmap;
	}
	
	
	private static Bitmap getVerticalLinearLayoutRenderImage(LinearLayout layout, Paint paint)
	{
		View child;
	
		int height = 0;
		
		int childCount = layout.getChildCount();
		int totalWidth = 0, totalHeight = 0;
		int[] heights = new int[childCount];
		int[] widths = new int[childCount];
		
		for (int i=0;i < childCount; i++)
		{
			child = layout.getChildAt(i);
			child.measure(MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED), MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED));
			heights[i] = height = child.getMeasuredHeight();
			widths[i] = child.getMeasuredWidth();
			totalHeight += height;
			if (widths[i] > totalWidth)
				totalWidth=widths[i];
		}
		
		Bitmap bitmap = Bitmap.createBitmap(totalWidth, totalHeight, Bitmap.Config.ARGB_8888), temp;
		
		Canvas surface = new Canvas(bitmap), tempSurface;
		int y = 0;
		
		for (int i=0; i< childCount; i++)
		{
			child = layout.getChildAt(i);
			child.layout(0, 0, widths[i], heights[i]);
			temp = Bitmap.createBitmap(widths[i], heights[i], Bitmap.Config.ARGB_8888);
			tempSurface = new Canvas(temp);
			child.draw(tempSurface);
			surface.drawBitmap(temp, 0, y, paint);
			y+=heights[i];
		}
		return bitmap;
	}
	
	
}
