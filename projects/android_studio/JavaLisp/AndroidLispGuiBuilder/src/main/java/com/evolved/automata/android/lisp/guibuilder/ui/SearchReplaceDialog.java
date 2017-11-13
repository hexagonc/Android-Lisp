package com.evolved.automata.android.lisp.guibuilder.ui;

import android.app.Dialog;
import android.content.DialogInterface;
import android.graphics.Color;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.app.DialogFragment;
import android.support.v7.app.AppCompatDialogFragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.AutoCompleteTextView;
import android.widget.Button;
import android.widget.Toast;

import com.evolved.automata.android.lisp.guibuilder.EventLog;
import com.evolved.automata.android.lisp.guibuilder.R;
import com.evolved.automata.android.lisp.guibuilder.Tools;
import com.evolved.automata.android.lisp.guibuilder.events.GoToLineNumber;
import com.evolved.automata.android.lisp.guibuilder.events.UpdateHighLightEvent;
import com.evolved.automata.android.lisp.guibuilder.model.CodePage;
import com.evolved.automata.editor.TextSearchIndex;
import com.evolved.automata.editor.TextSearchResult;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;

import io.reactivex.Observer;
import io.reactivex.annotations.NonNull;
import io.reactivex.disposables.Disposable;

/**
 * Created by Evolved8 on 10/21/17.
 */

public class SearchReplaceDialog extends AppCompatDialogFragment {

    int mStyle = android.app.DialogFragment.STYLE_NORMAL;
    int mTheme = android.support.v7.appcompat.R.style.Theme_AppCompat_Light_Dialog_Alert;

    TextSearchResult prevResult = null;

    AutoCompleteTextView mSearchAutoTExt;
    AutoCompleteTextView mReplaceAutoText;

    Button mFindNextButton;
    Button mFindPrevButton;

    Button mReplaceButton;
    Button mReplaceAllButton;
    Button mCancel;

    ArrayList<String> mSearchHistory;
    ArrayList<String> mReplaceHistory;



    AdapterView.OnItemClickListener mOnSearchHistoryClickListener;
    AdapterView.OnItemClickListener mOnReplaceHistoryClickListener;


    String mLastSearchText = null;

    Iterator<TextSearchResult> mSearchResults = null;

    ArrayAdapter<String> mSearchHistoryAdapter;
    ArrayAdapter<String> mReplaceHistoryAdapter;

    ArrayList<TextSearchResult> mNavigatableResults = null;

    int mDefaultAutoCompleteAdapterResourceId = android.R.layout.simple_dropdown_item_1line;

    int mSearchPos = -1;
    CodePage mPage;

    int mSelectColor = Color.parseColor("#FFF68A");

    public static SearchReplaceDialog make(CodePage page)
    {
        SearchReplaceDialog dialog = new SearchReplaceDialog();
        dialog.setPage(page);



        return dialog;
    }

    private void setPage(CodePage page)
    {
        mPage = page;
        mSearchHistory = new ArrayList<String>();
        mSearchHistory.addAll(mPage.getSearchHistory());

        mReplaceHistory = new ArrayList<String>();
        mReplaceHistory.addAll(mPage.getReplaceHistory());

    }

    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setStyle(mStyle, mTheme);
    }

    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState)
    {

        return super.onCreateDialog(savedInstanceState);
    }

    private void searchNext(final String text)
    {
        if ( mSearchResults == null ||!text.equals(mLastSearchText))
        {
            mLastSearchText = text;
            mNavigatableResults = new ArrayList<>();
            if (mPage.isTopParseNodeValid())
            {
                mSearchResults = mPage.findText(text);
            }
            else
            {
                mPage.findText(text, new Observer<Iterator<TextSearchResult>>() {
                    @Override
                    public void onSubscribe(@NonNull Disposable d)
                    {

                    }

                    @Override
                    public void onNext(@NonNull Iterator<TextSearchResult> textSearchResultIterator)
                    {
                        mSearchResults = textSearchResultIterator;
                        gotoNext();
                    }

                    @Override
                    public void onError(@NonNull Throwable e)
                    {

                    }

                    @Override
                    public void onComplete()
                    {

                    }
                });
                return;
            }

        }
        gotoNext();
    }



    private void clearLastHighlighResult()
    {
        if (prevResult != null)
        {
            int start = prevResult.getStartPosition();
            int end = start + prevResult.getString().length();
            Tools.postEvent( new UpdateHighLightEvent(start, end, UpdateHighLightEvent.HIGHLIGHT_ACTION.CLEAR, mSelectColor));
        }
    }

    private void highlightResult(TextSearchResult result)
    {
        int start = result.getStartPosition();
        int end = start + result.getString().length();
        prevResult = result;
        Tools.postEvent( new UpdateHighLightEvent(start, end, UpdateHighLightEvent.HIGHLIGHT_ACTION.SET, mSelectColor));

    }

    private void gotoResult(TextSearchResult result)
    {
        clearLastHighlighResult();
        highlightResult(result);
        Tools.postEvent(new GoToLineNumber(result.getLineNumber()));
    }

    private void gotoNext()
    {
        if (mSearchPos + 1 < mNavigatableResults.size())
        {
            mSearchPos++;
            TextSearchResult result = mNavigatableResults.get(mSearchPos);

            gotoResult(result);
        }
        else if (mSearchResults.hasNext())
        {
            TextSearchResult result = mSearchResults.next();
            mNavigatableResults.add(result);
            mSearchPos = mNavigatableResults.size() - 1;
            gotoResult(result);
        }
        else if (mNavigatableResults.size() > 0)
        {

            mSearchPos = -1;
            gotoNext();
        }
        else
        {
            Toast.makeText(getActivity(), "No results", Toast.LENGTH_LONG).show();
        }
    }

    private void gotoPrev()
    {
        if (mNavigatableResults.size() > 1)
        {
            if (mSearchPos == 0)
            {
                mSearchPos = mNavigatableResults.size();
            }
            mSearchPos--;
            TextSearchResult result = mNavigatableResults.get(mSearchPos);
            gotoResult(result);
        }

    }


    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState)
    {
        ViewGroup top = (ViewGroup) inflater.inflate(R.layout.search_dialog, container, false);


        mSearchAutoTExt = (AutoCompleteTextView)top.findViewById(R.id.auto_search_texts);
        mReplaceAutoText = (AutoCompleteTextView)top.findViewById(R.id.auto_replace_text);
        mFindNextButton = (Button)top.findViewById(R.id.but_search_next);
        mFindPrevButton = (Button)top.findViewById(R.id.but_search_prev);
        mReplaceButton = (Button)top.findViewById(R.id.but_replace);
        mReplaceAllButton = (Button)top.findViewById(R.id.but_replace_all);
        mCancel = (Button)top.findViewById(R.id.but_dialog_finish);
        configureDialog();
        mPage.updateSearchIndex();
        getDialog().setTitle("Find/Replace Text");
        return top;
    }

    private void configureDialog()
    {
        mSearchHistoryAdapter = new ArrayAdapter<String>(getContext(), mDefaultAutoCompleteAdapterResourceId, mSearchHistory);
        mSearchAutoTExt.setAdapter(mSearchHistoryAdapter);
        mSearchAutoTExt.setThreshold(1);

        mReplaceHistoryAdapter = new ArrayAdapter<String>(getContext(), mDefaultAutoCompleteAdapterResourceId, mReplaceHistory);
        mReplaceAutoText.setAdapter(mReplaceHistoryAdapter);
        mReplaceAutoText.setThreshold(1);

        mFindNextButton.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View view)
            {
                String searchText = mSearchAutoTExt.getText().toString();
                if (searchText.length() > 0)
                {
                    searchNext(searchText);
                }
                else
                {
                    Toast.makeText(getActivity(), "No text to search for", Toast.LENGTH_LONG).show();
                }
            }
        });

        mFindPrevButton.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View view)
            {
                gotoPrev();
            }
        });

        mCancel.setOnClickListener(new View.OnClickListener()
        {
            @Override
            public void onClick(View view)
            {
                dismiss();
            }
        });
    }

    @Override
    public void onDismiss(DialogInterface dialog)
    {
        super.onDismiss(dialog);
        clearLastHighlighResult();
    }

    @Override
    public void onStart()
    {
        super.onStart();
    }

    @Override
    public void onResume()
    {
        super.onResume();
    }

    @Override
    public void onPause()
    {
        super.onPause();
    }

    @Override
    public void onStop()
    {
        super.onStop();
    }

}
