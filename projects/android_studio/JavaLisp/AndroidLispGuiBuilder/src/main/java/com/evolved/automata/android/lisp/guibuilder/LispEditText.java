package com.evolved.automata.android.lisp.guibuilder;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.text.Editable;
import android.text.Layout;
import android.text.Spanned;
import android.text.TextWatcher;
import android.text.style.BackgroundColorSpan;
import android.text.style.ForegroundColorSpan;
import android.util.AttributeSet;
import android.util.Log;
import android.view.GestureDetector;
import android.view.MotionEvent;
import android.view.View;
import android.widget.EditText;


import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.lisp.editor.CompositeNode;
import com.evolved.automata.lisp.editor.ParseNode;
import com.evolved.automata.lisp.editor.TopParseNode;

import org.apache.commons.lang3.tuple.Pair;

import java.util.LinkedList;
import java.util.concurrent.TimeUnit;

import io.reactivex.Observable;
import io.reactivex.ObservableEmitter;
import io.reactivex.ObservableOnSubscribe;
import io.reactivex.Observer;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.annotations.NonNull;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Function;

/**
 * Created by Evolved8 on 4/26/17.
 */

public class LispEditText extends EditText {

    public interface HeightEvent
    {
        boolean isLessThanMinimumEditableHeight();
    }

    public interface StateListener
    {
        void onCursorChange(int cursorPos);
        void onTextChange(String newText, int cursorPos);
        void onSelectionChanged(ParseNode newNode);
        void onReadOnlyStateChanged(boolean isReadOnly);
    }

    public interface ControlInterface
    {
        void setTopParseNode(TopParseNode topNode);
        void disableSelectionEffects();
        void enableSelectionEffects();
        void setReadOnly();
        void disableReadOnly();
        boolean isReadOnlyMode();
        String getText();
        ParseNode getSelection();
        TopParseNode getTopParseNode();

        int getCursorPos();
        void setText(String text);
        void setText(String text, int cursorPos);
        void setText(String text, int cursorPos, TopParseNode topNode);
        void setCursorPos(int pos);
        void clearCurrentSelection();
        void insertTextAtCursor(String text);
        void moveCursorLeft();
        void moveCursorRight();

        void moveToParent();
        void moveToFirstChild();
        void moveToPrevSibling(boolean wrapAround);
        void moveToNextSibling(boolean wrapAround);
        void setStateListener(StateListener listener);
    }


    public interface ParseStateEvent
    {
        boolean isStarting();
    }


    public static int SCROLL_THRESHOLD_DP = 10;
    public float SCROLL_THRESHOLD_PX;
    Pair<Float, Float> mInitialPos;

    StateListener mStateListener;

    ForegroundColorSpan mSelectionForegroundSpan;
    BackgroundColorSpan mSelectionBackgroundSpan;
    Observable<ParseNode> mUpdateObservable;

    TopParseNode mParseNode;
    ParseNode mCurrentSelection, mPreviousSelection;
    int mCursorPosition=0;
    int mSelectionColor = Color.rgb(198, 232, 237);
    boolean suppressSelectionUpdateP = false;
    long mUpdateTimeoutInterval = 2000;
    TimeUnit mUpdateTimeUnit = TimeUnit.MILLISECONDS;

    CodeUpdateListener mUpdateListener;
    boolean mAllowSelectionChangesP;
    boolean mReadOnlyModeP = false;

    boolean mTempSuppressHighlightingP = false;

    GestureDetector mReadOnlyGestureDetector;


    final int MINIMUM_EDITABLE_HEIGHT_DP = 100;
    final int MINIMUM_EXPANDABLE_EDITABLE_HEIGHT_INCREASE_DP = 10;

    int mMinimumExpandableHeightPx = 0;
    int mMinimumEditableHeightPx = 100;




    boolean mIsShorterThanMinimumEditableHeightP = false;

    public static class EditEvent
    {
        String nNewText;

        public EditEvent(String value)
        {
            nNewText = value;
        }
        public String getNewCodeText()
        {
            return nNewText;
        }
    }

    public class CodeUpdateListener implements TextWatcher, ObservableOnSubscribe<EditEvent>
    {

        ObservableEmitter<EditEvent> _subScriber;

        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after)
        {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count)
        {

        }

        @Override
        public void afterTextChanged(Editable s)
        {
            Log.d("<><><<><<><", "Updated text");
            String updated  =s.toString();
            mTempSuppressHighlightingP = true;
            clearSelectionDisplay();
            directUpdate(updated);

        }

        @Override
        public void subscribe(@NonNull ObservableEmitter<EditEvent> e) throws Exception
        {
            _subScriber = e;
        }

        public void directUpdate(String updated)
        {

            if (_subScriber != null)
                _subScriber.onNext(new EditEvent(updated));
            if (mStateListener != null)
                mStateListener.onTextChange(updated, mCursorPosition);
        }
    }



    private void init()
    {

        float d = getContext().getResources().getDimensionPixelSize(R.dimen.v2_result_pane_height);

        int mineditablePx = AndroidTools.convertDPtoPX(getContext(), MINIMUM_EDITABLE_HEIGHT_DP);

        mMinimumExpandableHeightPx = mineditablePx + (int)d;
        mMinimumEditableHeightPx = mineditablePx;


        mReadOnlyGestureDetector = new GestureDetector(getContext(), getGestureListener());
        mAllowSelectionChangesP = true;
        mStateListener = null;
        mSelectionForegroundSpan = new ForegroundColorSpan(Color.rgb(0, 0, 0));
        mSelectionBackgroundSpan = new BackgroundColorSpan(mSelectionColor);
        mPreviousSelection = null;
        mUpdateListener = new CodeUpdateListener();

        addTextChangedListener(mUpdateListener);
        SCROLL_THRESHOLD_PX = AndroidTools.convertDPtoPX(getContext(), SCROLL_THRESHOLD_DP);

    }

    public LispEditText(Context context)
    {
        super(context);
        init();
    }

    public LispEditText(Context context, AttributeSet attrs)
    {
        super(context, attrs);
        init();
    }

    private float distance(Pair<Float, Float> p1, Pair<Float, Float> p2)
    {
        float d1 = p1.getLeft() - p2.getLeft(), d2 = p1.getRight() - p2.getRight();
        return (float)Math.sqrt(d1*d1 + d2*d2);
    }



    @Override
    public boolean onTouchEvent(MotionEvent event)
    {
        Pair<Float, Float> currentPos = Pair.of(event.getX(), event.getY());
        //Log.d("Vo<>oVo<>oVo", "Touch event: " + event.toString());

        switch (event.getAction())
        {
            case MotionEvent.ACTION_DOWN:
                mInitialPos = currentPos;
                return super.onTouchEvent(event);
            case MotionEvent.ACTION_UP:
                float movementDistance = distance(mInitialPos, currentPos);
                if (movementDistance < SCROLL_THRESHOLD_PX)
                    return super.onTouchEvent(event);
                else
                {
                    // Do nothing since we don't want to move the cursor if the screen was scrolled
                    //Log.d("Vo<>oVo<>oVo", "Skipping touch up event since scrolling: " + movementDistance);
                    return true;
                }

        }


        return super.onTouchEvent(event);
    }

    public LispEditText(Context context, AttributeSet attrs, int defStyle)
    {
        super(context, attrs, defStyle);
        init();
    }


    GestureDetector.OnGestureListener getGestureListener()
    {
        return new GestureDetector.OnGestureListener() {

            @Override
            public boolean onSingleTapUp(MotionEvent e) {
                // TODO Auto-generated method stub
                return false;
            }

            @Override
            public void onShowPress(MotionEvent e) {
                // TODO Auto-generated method stub

            }

            @Override
            public boolean onScroll(MotionEvent e1, MotionEvent e2, float distanceX,
                                    float distanceY) {
                scrollBy((int)distanceX, (int)distanceY);
                return true;
            }

            @Override
            public void onLongPress(MotionEvent e) {
                // TODO Auto-generated method stub

            }

            @Override
            public boolean onFling(MotionEvent e1, MotionEvent e2, float velocityX,
                                   float velocityY) {
                // TODO Auto-generated method stub
                return false;
            }

            @Override
            public boolean onDown(MotionEvent e) {
                // TODO Auto-generated method stub
                return false;
            }
        };
    }


    public void setReadOnlyState(boolean suppressNotifications)
    {
        mReadOnlyModeP = true;
        if (mStateListener != null && !suppressNotifications)
            mStateListener.onReadOnlyStateChanged(mReadOnlyModeP);
        setOnTouchListener(new View.OnTouchListener() {

            @Override
            public boolean onTouch(View v, MotionEvent event) {
                mReadOnlyGestureDetector.onTouchEvent(event);
                Pair<Float, Float> currentPos = Pair.of(event.getX(), event.getY());
                //Log.d("Vo<>oVo<>oVo", "Touch event: " + event.toString());
                switch (event.getAction()) {
                    case MotionEvent.ACTION_DOWN:
                            mInitialPos = currentPos;
                            return true;
                    case MotionEvent.ACTION_UP:
                        float movementDistance = distance(mInitialPos, currentPos);
                        if (movementDistance < SCROLL_THRESHOLD_PX)
                        {
                            Layout layout = ((EditText) v).getLayout();
                            float x = event.getX() + getScrollX();
                            float y = event.getY() + getScrollY();
                            int line = layout.getLineForVertical((int) y);

                            final int offset = layout.getOffsetForHorizontal( line,  x);

                            post(new Runnable()
                            {
                                public void run()
                                {
                                    setSelection(offset);
                                }
                            });
                        }


                        break;
                }
                return true;
            }

        });
    }
    public void setReadOnlyState()
    {
        setReadOnlyState(false);
    }


    public void removeReadOnlyState()
    {
        setOnTouchListener(null);
        mReadOnlyModeP = false;
        if (mStateListener != null)
            mStateListener.onReadOnlyStateChanged(mReadOnlyModeP);
    }

    private void setSelectionDisplayPolicy(boolean displaySelection)
    {
        if (displaySelection != mAllowSelectionChangesP)
        {
            mAllowSelectionChangesP = displaySelection;
            if (!displaySelection)
            {
                getEditableText().removeSpan(mSelectionForegroundSpan);
                getEditableText().removeSpan(mSelectionBackgroundSpan);
                mCurrentSelection = null;
            }
            else
            {
                if (mParseNode != null)
                {
                    mCurrentSelection = mParseNode.findNode(mCursorPosition);
                    renderSelection(false);
                }

            }
        }
        mAllowSelectionChangesP = displaySelection;

    }

    public ControlInterface getControlInterface()
    {
        return new ControlInterface() {

            @Override
            public void setTopParseNode(TopParseNode topNode)
            {
                mParseNode = topNode;
                if (topNode != null)
                {

                    mCurrentSelection = mParseNode.findNode(mCursorPosition);
                    if (mReadOnlyModeP)
                    {
                        mAllowSelectionChangesP = true;
                        renderSelection(false);
                    }
                }
                else
                {
                    mCurrentSelection = null;

                }

            }

            @Override
            public void moveToParent()
            {
                LispEditText.this.moveToParent();
            }

            @Override
            public void moveToFirstChild()
            {
                LispEditText.this.moveToFirstChildToken();
            }

            @Override
            public void moveToPrevSibling(boolean wrapAround)
            {
                moveToPreviousSiblingToken(wrapAround);
            }

            @Override
            public void moveToNextSibling(boolean wrapAround)
            {
                moveToNextSiblingToken(wrapAround);
            }

            @Override
            public void disableSelectionEffects()
            {
                setSelectionDisplayPolicy(false);
            }

            @Override
            public void enableSelectionEffects()
            {
                setSelectionDisplayPolicy(true);
            }

            @Override
            public void setReadOnly()
            {
                setReadOnlyState();
                enableSelectionEffects();

            }

            @Override
            public void disableReadOnly()
            {
                mParseNode = null;
                mCurrentSelection = null;

                removeReadOnlyState();
                disableSelectionEffects();
            }

            @Override
            public boolean isReadOnlyMode()
            {
                return mReadOnlyModeP;
            }

            @Override
            public String getText()
            {
                return getEditableText().toString();
            }

            @Override
            public ParseNode getSelection()
            {
                return mCurrentSelection;
            }


            @Override
            public TopParseNode getTopParseNode()
            {
                return mParseNode;
            }

            @Override
            public int getCursorPos()
            {
                return mCursorPosition;
            }

            @Override
            public void setText(String text)
            {
                mParseNode = null;
                mCurrentSelection = null;
                setText(text, 0);
            }

            @Override
            public void setText(String text, int cursorPos)
            {
                mParseNode = null;
                mCurrentSelection = null;
                setText(text, cursorPos, null);
            }

            @Override
            public void setText(String text, int cursorPos, TopParseNode top)
            {
                if (text == null)
                    text = "";
                LispEditText.this.setText(text);
                boolean updateCursorPosP = false;
                if (0  <= cursorPos && cursorPos <= text.length())
                {
                    updateCursorPosP = true;
                    mCursorPosition = cursorPos;
                    setSelection(cursorPos, cursorPos);
                }
                else if (text.length() == 0)
                {
                    mCursorPosition = 0;
                    setSelection(0, 0);
                }

                if (top != null && mParseNode != top)
                {
                    mParseNode = top;
                    mCurrentSelection = mParseNode.findNode(cursorPos);
                    if (mReadOnlyModeP)
                    {
                        mAllowSelectionChangesP = true;
                        renderSelection(false);
                    }
                }
            }

            @Override
            public void setCursorPos(int cursorPos)
            {
                String text = getText().toString();
                if (text == null)
                    text = "";
                setText(text);
                if (cursorPos <= 0 && cursorPos <= text.length())
                {
                    mCursorPosition = cursorPos;
                    setSelection(cursorPos, cursorPos);
                }
                else if (text.length() == 0)
                {
                    mCursorPosition = 0;
                    setSelection(0, 0);
                }
            }

            @Override
            public void clearCurrentSelection()
            {
                if (mCurrentSelection != null)
                {
                    int start = mCurrentSelection.getStartIndex();
                    int end = mCurrentSelection.getLength() + start;
                    int newCursor = mCurrentSelection.getStartIndex();
                    String newText = getText().toString();
                    setText(newText.substring(0, start) + newText.substring(end + 1), newCursor);
                }
            }

            @Override
            public void insertTextAtCursor(String text)
            {
                String oldText = getText().toString();
                int selection = getSelectionStart();
                setText(oldText.substring(0, selection) + text + oldText.substring(selection), selection);
            }

            @Override
            public void moveCursorLeft()
            {
                LispEditText.this.moveCursorLeft();
            }

            @Override
            public void moveCursorRight()
            {
                LispEditText.this.moveCursorRight();
            }

            @Override
            public void setStateListener(StateListener listener)
            {
                mStateListener = listener;
            }
        };
    }

    public void moveCursorRight()
    {
        mCursorPosition = getSelectionStart();
        if (mCursorPosition < getText().toString().length())
        {

            setSelection(mCursorPosition + 1, mCursorPosition + 1);

        }
    }

    public void moveCursorLeft()
    {
        mCursorPosition = getSelectionStart();
        if (mCursorPosition > 0)
        {

            setSelection(mCursorPosition - 1, mCursorPosition - 1);
            
        }

    }


    @Override
    public void setText(CharSequence text, BufferType type)
    {
        super.setText(text, type);
        if (mUpdateListener != null)
        {
            mUpdateListener.directUpdate(text.toString());
        }


    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec)
    {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
        Log.d("......", "Measured height " + getMeasuredHeight() + " max collapsable height " + mMinimumEditableHeightPx + " min expandable height " + mMinimumExpandableHeightPx);

        if (getMeasuredHeight() <= mMinimumEditableHeightPx)
        {
            if (!mIsShorterThanMinimumEditableHeightP)
            {
                mIsShorterThanMinimumEditableHeightP = true;
                Tools.postEvent(new HeightEvent() {
                    @Override
                    public boolean isLessThanMinimumEditableHeight()
                    {
                        return mIsShorterThanMinimumEditableHeightP;
                    }
                });
            }

        }
        else if (getMeasuredHeight() >= mMinimumExpandableHeightPx)
        {
            if (mIsShorterThanMinimumEditableHeightP)
            {
                mIsShorterThanMinimumEditableHeightP = false;
                Tools.postEvent(new HeightEvent() {
                    @Override
                    public boolean isLessThanMinimumEditableHeight()
                    {
                        return mIsShorterThanMinimumEditableHeightP;
                    }
                });
            }

        }
    }

    @Override
    protected void onLayout(boolean changed, int left, int top, int right, int bottom)
    {
        super.onLayout(changed, left, top, right, bottom);
        Log.d("......", "Layout available height: " + Math.abs(top - bottom));
    }

    @Override
    protected void onSelectionChanged(int selStart, int selEnd)
    {
        super.onSelectionChanged(selStart, selEnd);
        if (!suppressSelectionUpdateP && selStart == selEnd && mParseNode != null) // purse cursor movement change
        {
            
            if (mStateListener != null && selStart != mCursorPosition)
            {
                mCursorPosition = selStart;
                mStateListener.onCursorChange(mCursorPosition);
            }

            mCurrentSelection = mParseNode.findNode(selStart);
            if (mCurrentSelection != null)
            {
                if (mCurrentSelection.getType() == ParseNode.TYPE.WHITE_SPACE)
                {
                    ParseNode parent = mCurrentSelection.getParent();
                    if (parent.getType() != ParseNode.TYPE.TOP)
                        mCurrentSelection = parent;
                }
                renderSelection(false);
            }
        }
        suppressSelectionUpdateP = false;
    }

    private void renderSelection(boolean updateCursorP)
    {
        if (!mAllowSelectionChangesP)
            return;

        if (mPreviousSelection != mCurrentSelection)
        {
            if (mStateListener != null)
            {
                mStateListener.onSelectionChanged(mCurrentSelection);
            }
            mPreviousSelection = mCurrentSelection;
        }
        if (mCurrentSelection != null)
        {
            int start = mCurrentSelection.getStartIndex();
            int length = mCurrentSelection.getLength();
            if (length > 0)
            {
                int end = start + length;
                Editable e = getEditableText();

                int flags = 0;
                switch (mCurrentSelection.getType())
                {
                    case TOP:
                    case VAR_NAME:
                    case NUMBER:
                        flags = Spanned.SPAN_INCLUSIVE_INCLUSIVE;
                        break;
                    case LIST:
                        flags = Spanned.SPAN_EXCLUSIVE_EXCLUSIVE;
                        break;
                }

                int expectedEnd = end, actualEnd = Math.min(end, e.toString().length());

                if (expectedEnd != actualEnd)
                {
                    Log.e("<><><><><>", "Code model out of sync with code text");
                }
                e.setSpan(mSelectionForegroundSpan, start, actualEnd, flags);
                e.setSpan(mSelectionBackgroundSpan, start, actualEnd, flags);
                if (updateCursorP)
                {
                    suppressSelectionUpdateP = true;
                    int startIndex = mCurrentSelection.getStartIndex();
                    setSelection(startIndex, startIndex);
                }
            }

        }
    }


    public void moveToNextSiblingToken(boolean wrap)
    {
        if (mCurrentSelection != null)
        {
            ParseNode nextSelection = mCurrentSelection.getNextTokenSibling(wrap);
            if (nextSelection != null)
            {
                mCurrentSelection = nextSelection;
                renderSelection(true);
            }
        }

    }

    public void moveToPreviousSiblingToken(boolean wrap)
    {
        if (mCurrentSelection != null)
        {
            ParseNode nextSelection = mCurrentSelection.getPreviousTokenSibling(wrap);
            if (nextSelection != null)
            {
                mCurrentSelection = nextSelection;
                renderSelection(true);
            }
        }

    }

    public void moveToFirstChildToken()
    {
        if (mCurrentSelection != null && mCurrentSelection instanceof CompositeNode)
        {
            CompositeNode comp = (CompositeNode)mCurrentSelection;
            LinkedList<ParseNode> children = comp.getChildren();
            if (children.size() > 0)
            {
                mCurrentSelection = children.getFirst();
                renderSelection(true);
            }
        }
    }

    public void moveToParent()
    {
        if (mCurrentSelection != null && mCurrentSelection.getParent() != null)
        {
            mCurrentSelection = mCurrentSelection.getParent();
            renderSelection(true);
        }
    }


    private void clearSelectionDisplay()
    {
        getEditableText().removeSpan(mSelectionBackgroundSpan);
        getEditableText().removeSpan(mSelectionForegroundSpan);
    }

}
