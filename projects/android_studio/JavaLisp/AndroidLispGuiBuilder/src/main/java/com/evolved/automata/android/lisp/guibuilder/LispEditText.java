package com.evolved.automata.android.lisp.guibuilder;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.support.v7.widget.AppCompatEditText;
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
import com.evolved.automata.android.lisp.guibuilder.events.ALGBEvent;
import com.evolved.automata.android.lisp.guibuilder.events.ALGBEventManager;
import com.evolved.automata.android.lisp.guibuilder.events.ALGBEventTypes;
import com.evolved.automata.android.lisp.guibuilder.events.CopyEvent;
import com.evolved.automata.android.lisp.guibuilder.events.PasteEvent;
import com.evolved.automata.events.Event;
import com.evolved.automata.events.EventManager;
import com.evolved.automata.lisp.editor.CompositeNode;
import com.evolved.automata.lisp.editor.EditorTransaction;
import com.evolved.automata.lisp.editor.ParseNode;
import com.evolved.automata.lisp.editor.SimpleTextEditor;
import com.evolved.automata.lisp.editor.TopParseNode;

import org.apache.commons.lang3.tuple.Pair;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;

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

public class LispEditText extends AppCompatEditText {

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
        void setEditorModel(SimpleTextEditor editorModel);
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



    public static int SCROLL_THRESHOLD_DP = 10;
    public float SCROLL_THRESHOLD_PX;
    Pair<Float, Float> mInitialPos;

    StateListener mStateListener;

    ForegroundColorSpan mSelectionForegroundSpan;
    BackgroundColorSpan mSelectionBackgroundSpan;

    TopParseNode mParseNode;
    ParseNode mCurrentSelection, mPreviousSelection;
    int mCursorPosition=0;
    int mSelectionColor = Color.rgb(198, 232, 237);
    boolean suppressSelectionUpdateP = false;

    CodeUpdateListener mUpdateListener;
    boolean mAllowSelectionChangesP;
    boolean mReadOnlyModeP = false;

    boolean mTempSuppressHighlightingP = false;

    GestureDetector mReadOnlyGestureDetector;


    final int MINIMUM_EDITABLE_HEIGHT_DP = 100;

    int mMinimumExpandableHeightPx = 0;
    int mMinimumEditableHeightPx = 100;


    SimpleTextEditor mEditorModel = null;

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
            if (count > 0)
            {
                String deleted = s.toString().substring(start, start + count);


                Log.d(".o0000o.o0000o.o0000o", String.format("deleted: %1$s, new cursor position is: %2$s", deleted, mCursorPosition - count));
            }
            Log.d(".o0000o.o0000o.o0000o", String.format("beforeTextChanged(:start %1$s :count %2$s :after %3$s)", start, count, after));

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count)
        {
            if (count > 0)
            {
                String inserted = s.toString().substring(start, count+ start);
                Log.d(".o0000o.o0000o.o0000o", String.format("inserted: %1$s, new cursor position is: %2$s", inserted, mCursorPosition));
            }
            Log.d(".o0000o.o0000o.o0000o", String.format("onTextChanged(:start %1$s :before %2$s :count %3$s)", start, before, count));
        }

        @Override
        public void afterTextChanged(Editable s)
        {
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
        mEditorModel = new SimpleTextEditor("", 0 , Tools.getEditorUndoHistoryLength(getContext()));

    }

    @Override
    protected void onAttachedToWindow()
    {
        super.onAttachedToWindow();
        Tools.registerEventHandler(this);
    }

    @Override
    protected void onDetachedFromWindow()
    {
        Tools.unRegisterEventHandler(this);
        super.onDetachedFromWindow();
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

    @Subscribe (threadMode = ThreadMode.MAIN)
    public void onPasteEvent(PasteEvent event)
    {
        ALGBEvent ce = ALGBEventManager.get().getEvent(ALGBEventTypes.COPY);
        if (ce != null)
        {
            //ALGBEventManager.get().removeEvent(ce.getType());
            CopyEvent copyEvent = (CopyEvent)ce;
            String newText = copyEvent.getText();
            Editable editable = getText();
            editable.clearSpans();
            // Update cursor position before updating text so that we send only one
            // notification to state listener in the CodeUpdateListener (if we update
            // mCursorPosition after inserting the text, there will be a TextChanged
            // state notification with the old cursor position then a separate
            // state change with the updated cursor position from the onSelectionChanged
            // callback
            int newCursorPos;
            if (mCurrentSelection != null && mParseNode != null)
            {
                int start = mCurrentSelection.getStartIndex();
                String selection = mCurrentSelection.toString();
                newCursorPos = start + newText.length();
                editable.replace(start, start + selection.length(), newText);

            }
            else
            {
                newCursorPos = mCursorPosition + newText.length();

                editable.insert(mCursorPosition, newText);
            }
            setText(editable.toString());
            setSelection(newCursorPos);
            //if (mStateListener != null)
            //    mStateListener.onTextChange(editable.toString(), mCursorPosition);

        }
    }



    // TODO: Finish this
    private void applyEditorTransaction(EditorTransaction trans, boolean updatHistory, boolean updateUI)
    {
        if (mEditorModel!=null)
        {
            int initCursor = mEditorModel.getCursorPosition();
            mEditorModel.applyTransaction(trans, updatHistory);

            if (updateUI)
            {
                int finalCur = mEditorModel.getCursorPosition();
                if (initCursor != finalCur)
                {
                    if (trans.getCharactersToInsert().length() == 0 && trans.getCharactersToDelete().length() == 0)
                    {
                        setSelection(mEditorModel.getCursorPosition());
                    }
                    String newText = mEditorModel.getText();
                    mStateListener.onTextChange(newText, mEditorModel.getCursorPosition());
                    setText(newText);
                }

                mCursorPosition = mEditorModel.getCursorPosition();




            }
        }
        else
            EventLog.get().logSystemError("Trying to apply editor transaction with undefined editor!");
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

                return false;
            }

            @Override
            public void onShowPress(MotionEvent e) {


            }

            @Override
            public boolean onScroll(MotionEvent e1, MotionEvent e2, float distanceX,
                                    float distanceY) {
                scrollBy((int)distanceX, (int)distanceY);
                return true;
            }

            @Override
            public void onLongPress(MotionEvent e) {

            }

            @Override
            public boolean onFling(MotionEvent e1, MotionEvent e2, float velocityX,
                                   float velocityY) {

                return false;
            }

            @Override
            public boolean onDown(MotionEvent e) {
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
            public void setEditorModel(SimpleTextEditor editorModel)
            {
                mEditorModel = editorModel;
            }

            @Override
            public void setTopParseNode(TopParseNode topNode)
            {

                boolean updateP = mParseNode != topNode;
                mParseNode = topNode;
                if (topNode != null && updateP)
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
                if (0  <= cursorPos && cursorPos <= text.length())
                {

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
            if (!text.toString().equals(getText().toString()))
                mUpdateListener.directUpdate(text.toString());
        }


    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec)
    {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);

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
    protected void onSelectionChanged(int selStart, int selEnd)
    {
        super.onSelectionChanged(selStart, selEnd);
        //if (mEditorModel != null && selStart == selEnd)
        //    mEditorModel.applyMoveCursorTransaction(selStart);

        if (!suppressSelectionUpdateP && selStart == selEnd && mParseNode != null)
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
        else if (selStart == selEnd)
        {
            if (mStateListener != null && selStart != mCursorPosition)
            {
                mCursorPosition = selStart;
                mStateListener.onCursorChange(mCursorPosition);
            }

        }
        Log.d("~)o(~ ~)o(~ ~)o(~ ~)o(~", "Selection changed: (" + selStart + ", " + selEnd + ")");
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
                    EventLog.get().logSystemError("Code model out of sync with code text");
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
