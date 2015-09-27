package com.evolved.automata.android.widgets;
import android.graphics.*;
import android.view.View;
import android.view.View.MeasureSpec;
import android.widget.LinearLayout;
import android.os.*;


public class DragRotateListener 
{
	LinearLayout _layout;
	View _firstChild;
	View _lastChild;
	int _childCount;
	
	int _firstChildWidth;
	int _firstChildHeight;
	int _lastChildWidth;
	int _lastChildHeight;
	boolean _workToDo = false;
	
	int _xDelta;
	int _yDelta;
	int _orientation;
	OnItemRollListener _listener;
	
	public static interface OnItemRollListener
	{
		public void onItemRoll();
	}
	
	public DragRotateListener(LinearLayout layout, OnItemRollListener listener)
	{
		_layout =layout;
		_orientation = layout.getOrientation();
		updateChildMeasurements();
		_xDelta = 0;
		_yDelta = 0;
		_listener = listener;
	}
	
	public void updateChildMeasurements()
	{
		_childCount = _layout.getChildCount();
		if (_childCount>0)
		{
			_firstChild = _layout.getChildAt(0);
			_lastChild = _layout.getChildAt(_childCount-1);
			
			if (_firstChild!=_lastChild)
			{
				_workToDo = true;
				measureFirstChild();
				measureLastChild();
			}
		}
		else
		{
			if (_listener!=null)
				_listener.onItemRoll();
		}
	}
	
	private void internalUpdate(int displacement)
	{
		if (_orientation == LinearLayout.HORIZONTAL)
		{
			_xDelta+=displacement;
			if (_xDelta>0) // rotating right
			{
				if (Math.abs(_xDelta) > _lastChildWidth )
				{
					rotateLayoutForward();
					_xDelta = 0;
					if (_listener!=null)
						_listener.onItemRoll();
				}
			}
			else if (_xDelta<0)
			{
				if (Math.abs(_xDelta) > _firstChildWidth)
				{
					rotateLayoutBack();
					_xDelta = 0;
					if (_listener!=null)
						_listener.onItemRoll();
				}
			}
		}
		else
		{
			_yDelta+=displacement;
			if (_yDelta>0) // moving down
			{
				if (Math.abs(_yDelta) > _lastChildHeight)
				{
					rotateLayoutForward();
					_yDelta = 0;
					if (_listener!=null)
						_listener.onItemRoll();
				}
			}
			else if (_yDelta<0)
			{
				if (Math.abs(_yDelta) > _firstChildHeight)
				{
					rotateLayoutBack();
					_yDelta = 0;
					if (_listener!=null)
						_listener.onItemRoll();
				}
			}
		}
	}
	
	public void onUpdateDisplacement(final int displacement)
	{
		Handler main = new Handler(Looper.getMainLooper());
		main.post(new Runnable()
		{
			public void run()
			{
				internalUpdate(displacement);
			}
		});
	}
	
	private void rotateLayoutBack()
	{
		_layout.removeViewAt(0);
		_layout.addView(_firstChild);
		_lastChildWidth = _firstChildWidth;
		_lastChildHeight = _firstChildHeight;
		_lastChild = _firstChild;
		_firstChild = _layout.getChildAt(0);
		measureFirstChild();
	}
	
	private void rotateLayoutForward()
	{
		_layout.removeViewAt(_childCount - 1);
		_layout.addView(_lastChild, 0);
		_firstChildWidth = _lastChildWidth;
		_firstChildHeight = _lastChildHeight;
		_firstChild = _lastChild;
		_lastChild = _layout.getChildAt(_childCount - 1);
		measureLastChild();
	}
	
	private void measureFirstChild()
	{
		_firstChild.measure(MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED), MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED));
		_firstChildWidth = _firstChild.getMeasuredWidth();
		_firstChildHeight = _firstChild.getMeasuredHeight();
	}
	
	private void measureLastChild()
	{
		_lastChild.measure(MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED), MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED));
		_lastChildWidth = _lastChild.getMeasuredWidth();
		_lastChildHeight = _lastChild.getMeasuredHeight();
	}
}
