package com.evolved.automata.android.lisp.guibuilder.v2;

import android.content.ComponentName;
import android.content.Context;
import android.graphics.Color;
import android.text.Editable;
import android.text.Spanned;
import android.text.TextWatcher;
import android.text.style.BackgroundColorSpan;
import android.text.style.ForegroundColorSpan;
import android.util.AttributeSet;
import android.util.Log;
import android.widget.EditText;
import android.widget.TextView;

import com.evolved.automata.lisp.editor.CompositeNode;
import com.evolved.automata.lisp.editor.ParseNode;
import com.evolved.automata.lisp.editor.TopParseNode;

import java.util.LinkedList;
import java.util.concurrent.TimeUnit;

import io.reactivex.Observable;
import io.reactivex.ObservableEmitter;
import io.reactivex.ObservableOnSubscribe;
import io.reactivex.Observer;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.annotations.NonNull;
import io.reactivex.annotations.Nullable;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Cancellable;
import io.reactivex.functions.Function;

/**
 * Created by Evolved8 on 4/26/17.
 */

public class LispEditText extends EditText implements Observer<ParseNode> {

    ForegroundColorSpan mSelectionForegroundSpan;
    BackgroundColorSpan mSelectionBackgroundSpan;
    Observable<ParseNode> mUpdateObservable;

    TopParseNode mParseNode;
    ParseNode mCurrentSelection;
    int mCursorPosition=0;
    boolean suppressSelectionUpdateP = false;
    long mUpdateTimeoutInterval = 2000;
    TimeUnit mUpdateTimeUnit = TimeUnit.MILLISECONDS;

    CodeUpdateListener mUpdateListener;

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

    public static class CodeUpdateListener implements TextWatcher, ObservableOnSubscribe<EditEvent>
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
            directUpdate(updated);
        }

        @Override
        public void subscribe(@NonNull ObservableEmitter<EditEvent> e) throws Exception
        {
            _subScriber = e;
        }

        public void directUpdate(String updated)
        {

            _subScriber.onNext(new EditEvent(updated));
        }
    }



    private void init()
    {
        mSelectionForegroundSpan = new ForegroundColorSpan(Color.rgb(0, 0, 0));
        mSelectionBackgroundSpan = new BackgroundColorSpan(Color.rgb(188, 221, 255));

        mUpdateListener = new CodeUpdateListener();
        mUpdateObservable = Observable.create(mUpdateListener).debounce(mUpdateTimeoutInterval, mUpdateTimeUnit).map(new Function<EditEvent, ParseNode>(){

            @Override
            public ParseNode apply(@NonNull EditEvent editEvent) throws Exception
            {
                Log.d("<>< <>< <>< <>< <><", "Updating parser with new text");
                TopParseNode top = new TopParseNode();
                top.processAll(editEvent.getNewCodeText());
                return top;
            }
        }).observeOn(AndroidSchedulers.mainThread());
        mUpdateObservable.subscribe(this);
        addTextChangedListener(mUpdateListener);


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

    public LispEditText(Context context, AttributeSet attrs, int defStyle)
    {
        super(context, attrs, defStyle);
        init();
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
    protected void onSelectionChanged(int selStart, int selEnd)
    {
        super.onSelectionChanged(selStart, selEnd);
        if (!suppressSelectionUpdateP && selStart == selEnd && mParseNode != null) // purse cursor movement change
        {
            mCursorPosition = selStart;
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

                e.setSpan(mSelectionForegroundSpan, start, end, flags);
                e.setSpan(mSelectionBackgroundSpan, start, end, flags);
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


    @Override
    public void onSubscribe(@NonNull Disposable d)
    {

    }

    @Override
    public void onNext(@NonNull ParseNode value)
    {
        mParseNode = (TopParseNode)value;
        renderSelection(false);
    }

    @Override
    public void onError(@NonNull Throwable error)
    {

    }

    @Override
    public void onComplete()
    {

    }


}
