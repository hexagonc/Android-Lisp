package com.evolved.automata.android.lisp.guibuilder;

import android.app.Fragment;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.evolved.automata.android.lisp.guibuilder.v2.LispEditText;

/**
 * Created by Evolved8 on 5/7/17.
 */

public class LispResultFragment extends Fragment {



    public interface Controller
    {
        void setResult(String text, boolean suppressUpdate);
        boolean showPreviousResult();
        boolean showNextResult();
        boolean showOldestResult();
        boolean showNewestResult();
        public void setHistoryLength(int max);
    }

    private class ResultCursor
    {
        String result;
        ResultCursor next;
        ResultCursor prev;
        public ResultCursor(String data)
        {
            result = data;
            mTotalHistory++;
        }

        public ResultCursor(String data, ResultCursor next)
        {
            result = data;
            this.next = next;
            ResultCursor nextPrev = next.prev;
            next.prev = this;
            prev = nextPrev;
            mTotalHistory++;
        }

        public ResultCursor(ResultCursor prev, String data)
        {
            ResultCursor prevNext = prev.next;
            prev.next = this;
            next = prevNext;
            mTotalHistory++;
        }


    }


    int mTotalHistory = 0;
    int mMaxHistory = 10;
    ResultCursor mLast;
    ResultCursor mFirst;
    ResultCursor mCurrent;

    TextView mResultPane;

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState)
    {
        ViewGroup top = (ViewGroup)inflater.inflate(R.layout.v2_result_pane, container, false);
        mResultPane = (TextView)top.findViewById(R.id.v2_txt_result);
        return top;
    }

    public Controller getController()
    {
        return new Controller() {

            @Override
            public void setHistoryLength(int max)
            {
                mMaxHistory = max;
            }

            @Override
            public void setResult(String text, boolean suppressUpdate)
            {

                if (mLast == mFirst && mLast == null)
                {
                    mLast = mFirst = mCurrent = new ResultCursor(text);
                    mResultPane.setText(text);
                }
                else
                {
                    if (mCurrent == mLast)
                    {
                        mCurrent = mLast = new ResultCursor(mLast, text);
                        mResultPane.setText(text);
                    }
                    else
                    {
                        mLast = new ResultCursor(mLast, text);
                        if (!suppressUpdate)
                        {
                            mResultPane.setText(text);
                            mCurrent = mLast;
                        }
                    }
                    while (mTotalHistory > mMaxHistory)
                    {
                        if (mCurrent == mFirst)
                            mCurrent = mFirst.next;
                        mFirst = mFirst.next;
                        mTotalHistory--;
                    }

                }
            }

            @Override
            public boolean showPreviousResult()
            {
                if (mCurrent.prev != null)
                {
                    mCurrent = mCurrent.prev;
                    mResultPane.setText(mCurrent.result);
                    return true;
                }
                else
                    return false;
            }

            @Override
            public boolean showNextResult()
            {
                if (mCurrent.next != null)
                {
                    mCurrent = mCurrent.next;
                    mResultPane.setText(mCurrent.result);
                    return true;
                }
                else
                    return false;
            }

            @Override
            public boolean showOldestResult()
            {
                if (mFirst != null)
                {
                    mCurrent = mFirst;
                    mResultPane.setText(mCurrent.result);
                    return true;
                }
                else
                    return false;

            }

            @Override
            public boolean showNewestResult()
            {
                if (mLast != null)
                {
                    mCurrent = mLast;
                    mResultPane.setText(mCurrent.result);
                    return true;
                }
                else
                    return false;

            }
        };
    }
}
