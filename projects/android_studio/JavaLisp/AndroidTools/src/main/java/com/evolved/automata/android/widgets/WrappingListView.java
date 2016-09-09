package com.evolved.automata.android.widgets;
import android.widget.*;
import android.content.*;
import android.os.*;
import android.util.*;
import android.view.*;
import android.graphics.*;
import com.evolved.automata.android.tools.R;
import android.content.res.*;


public class WrappingListView extends RelativeLayout
{
	LinearLayout _internalLinear;
	ImageView _internalView;
	
	int _orientation;
	int _deltax;
	int _deltay;
	
	float _lastX;
	float _lastY;
	
	Bitmap _imageBitmap;
	Object _synch = new Object();
	Paint defPaint;
	
	DragRotateListener _dragListener;
	
	Thread _rollToRestThread;
	
	int _bitmapHeight;
	int _bitmapWidth;
	
	public WrappingListView(Context context, boolean vertical)
	{
		super(context);
		initialize(context, vertical);
	}
	
	public WrappingListView(Context context, AttributeSet attriv)
	{
		super(context, attriv);
		String orientation = getStringCustomAttributeValue(context, attriv);
		if (orientation==null)
			initialize(context, true);
		else
			initialize(context, "vertical".toLowerCase().equals(orientation));
	}
	
	public static String getStringCustomAttributeValue(Context con, AttributeSet attrs)
	{
		TypedArray a = con.obtainStyledAttributes(attrs, R.styleable.WrappingListView);
		int count = a.getIndexCount();
		int attributeIndex=0;
		try
		{
			for (int i=0;i<count;i++)
			{
				attributeIndex = a.getIndex(i);
				if (attributeIndex == R.styleable.WrappingListView_orientation)
				{
					return a.getString(attributeIndex);
				}
				
			}
		}
		finally
		{
			a.recycle();
		}
		
		return null;
	}
	
	
	private void initialize(Context con, boolean vertical)
	{
		LayoutInflater inflater = (LayoutInflater)con.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		if (vertical)
			inflater.inflate(R.layout.wrapping_listview_layout, this);
		else
			inflater.inflate(R.layout.horizontal_wrapping_listview_layout, this);
		_internalLinear = (LinearLayout)findViewById(R.id.innerList);
		_internalView = (ImageView)findViewById(R.id.scrollOverlay);
		_internalView.setVisibility(View.VISIBLE);
		_internalLinear.setVisibility(View.INVISIBLE);
		_orientation = _internalLinear.getOrientation();
		defPaint = new Paint();
		_dragListener = new DragRotateListener(_internalLinear, new DragRotateListener.OnItemRollListener()
		{
			public void onItemRoll()
			{
				if (_rollToRestThread!=null)
					_rollToRestThread.interrupt();
			}
		});
	}
	
	private void setRollToRestThread()
	{
		_rollToRestThread = new Thread()
		{
			public void run()
			{
				Handler h = new Handler(Looper.getMainLooper());
				try
				{
					while (true)
					{
						h.post(new Runnable()
						{
							public void run() 
							{
								updateScroll();
							}
						});
						
						Thread.sleep(50);
					}
				}
				catch (InterruptedException ie)
				{
					
				}
				_deltax = _deltay = 0;
//				h.post(new Runnable()
//				{
//					public void run()
//					{
//						_internalLinear.setVisibility(VISIBLE);
//						_internalView.setVisibility(INVISIBLE);
//					}
//				}
//				);
				_rollToRestThread = null;
			}
		};
		_rollToRestThread.start();
	}
	
	private void updateScroll()
	{
		if (_orientation == LinearLayout.VERTICAL)
		{
			_dragListener.onUpdateDisplacement(_deltay);
			synchronized (_synch)
			{
				_imageBitmap = GraphicTools.rollBitmapVertically(_imageBitmap, _bitmapWidth, _bitmapHeight, defPaint, Math.abs(_deltay), _deltay < 0);
				_internalView.setImageBitmap(_imageBitmap);
				_internalView.invalidate();
			}
		}
		else
		{
			_dragListener.onUpdateDisplacement(_deltax);
			synchronized (_synch)
			{
				_imageBitmap = GraphicTools.rollBitmapHorizontally(_imageBitmap, _bitmapWidth, _bitmapHeight, defPaint, Math.abs(_deltax), _deltax < 0);
				_internalView.setImageBitmap(_imageBitmap);
				_internalView.invalidate();
			}
		}
	}
	
	boolean _moving = false;
	boolean _first = true;
	
	private void updateBitmap()
	{
		synchronized (_synch)
		{
			_imageBitmap = GraphicTools.getTotalImage(_internalLinear, defPaint);
			_bitmapWidth = _imageBitmap.getWidth();
			_bitmapHeight = _imageBitmap.getHeight();
		}
		_internalView.setImageBitmap(_imageBitmap);
		_internalView.invalidate();
	}
	
	
	@Override
	public boolean onTouchEvent(MotionEvent event)
	{
		int action = event.getAction();
		float currentX, currentY;
		switch (action)
		{
			case MotionEvent.ACTION_CANCEL:
				_deltax = _deltay = 0;
				break;
			case MotionEvent.ACTION_DOWN:
				_deltax = _deltay = 0;
				_lastX = event.getX();
				_lastY = event.getY();
				_moving = false;
				// TODO: Need to look at what happens when you stop the rolling while it is in progress
				if (_rollToRestThread!=null)
					_rollToRestThread.interrupt();
				updateBitmap();
				//_internalView.setVisibility(View.VISIBLE);
				break;
			case MotionEvent.ACTION_MOVE:
				currentX = event.getX();
				currentY = event.getY();
				_deltax = (int)(currentX - _lastX);
				_deltay = (int)(currentY - _lastY);
				_lastX = currentX;
				_lastY = currentY;
				updateScroll();
				break;
			case MotionEvent.ACTION_UP:
				setRollToRestThread();
				break;
			
		}
		return true;
	}
	
	public void addButton(Button button)
	{
		_internalLinear.addView(button);
		_dragListener.updateChildMeasurements();
		updateBitmap();
	}
}
