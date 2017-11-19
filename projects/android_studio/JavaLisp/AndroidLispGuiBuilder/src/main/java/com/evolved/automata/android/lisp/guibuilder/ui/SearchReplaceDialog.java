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
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.RelativeLayout;
import android.widget.Toast;

import com.evolved.automata.android.lisp.guibuilder.EventLog;
import com.evolved.automata.android.lisp.guibuilder.LispEditText;
import com.evolved.automata.android.lisp.guibuilder.R;
import com.evolved.automata.android.lisp.guibuilder.Tools;
import com.evolved.automata.android.lisp.guibuilder.events.GetSelectionBoundsEvent;
import com.evolved.automata.android.lisp.guibuilder.events.GoToLineNumber;
import com.evolved.automata.android.lisp.guibuilder.events.NormalModeEvent;
import com.evolved.automata.android.lisp.guibuilder.events.ReplaceModeEvent;
import com.evolved.automata.android.lisp.guibuilder.events.ReplaceTextEvent;
import com.evolved.automata.android.lisp.guibuilder.events.UpdateHighLightEvent;
import com.evolved.automata.android.lisp.guibuilder.model.CodePage;
import com.evolved.automata.editor.TextSearchIndex;
import com.evolved.automata.editor.TextSearchResult;

import org.apache.commons.lang3.StringUtils;

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

    LispEditText.Selection mSelection = null;

    AdapterView.OnItemClickListener mOnSearchHistoryClickListener;
    AdapterView.OnItemClickListener mOnReplaceHistoryClickListener;


    String mLastSearchText = null;

    Iterator<TextSearchResult> mSearchResults = null;

    ArrayAdapter<String> mSearchHistoryAdapter;
    ArrayAdapter<String> mReplaceHistoryAdapter;

    ArrayList<TextSearchResult> mNavigatableResults = null;

    int mDefaultAutoCompleteAdapterResourceId = android.R.layout.simple_dropdown_item_1line;

    int mInitialSearchStartPos=0;
    int mSearchPos = 0;
    int mSearchEndPos;
    int mPrevSearchPos = -1;

    CodePage mPage;
    String mPrevSearchText;

    boolean mUseSearchIndexP = false;

    int mSelectColor = Color.parseColor("#FFF68A");

    String mSearchText;

    CheckBox mSearchInSelectionChk;

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
        mSearchText = mPage.getExpr();
        mSearchEndPos = mSearchText.length();
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



    private void updateSelection(String text)
    {
        String[] lines = StringUtils.splitPreserveAllTokens(mSearchText.substring(0, mSearchPos), '\n');
        int lineNumber = lines.length;

        Tools.postEvent(new GoToLineNumber(lineNumber));
        Tools.postEvent(new UpdateHighLightEvent(mSearchPos, mSearchPos + text.length(), UpdateHighLightEvent.HIGHLIGHT_ACTION.SET, mSelectColor));
    }


    private void clearCurrentHighlighResult()
    {
        if (mPrevSearchText != null)
        {
            Tools.postEvent( new UpdateHighLightEvent(mSearchPos, mSearchPos + mPrevSearchText.length(), UpdateHighLightEvent.HIGHLIGHT_ACTION.CLEAR, mSelectColor));
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

        mSearchInSelectionChk = (CheckBox)top.findViewById(R.id.chk_search_selection);
        mSearchInSelectionChk.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
            @Override
            public void onCheckedChanged(CompoundButton compoundButton, boolean isChecked)
            {
                toogleSearchInSelection(isChecked);
            }
        });

        CheckBox replaceToggleButton = (CheckBox)top.findViewById(R.id.chk_show_replace);
        final RelativeLayout replaceContainer =  (RelativeLayout)top.findViewById(R.id.rel_replace_container);
        replaceToggleButton.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
            @Override
            public void onCheckedChanged(CompoundButton compoundButton, boolean isChecked)
            {
                if (isChecked)
                {
                    replaceContainer.setVisibility(View.VISIBLE);
                }
                else
                    replaceContainer.setVisibility(View.INVISIBLE);
            }
        });

        mReplaceAllButton = (Button)top.findViewById(R.id.but_replace_all);


        mCancel = (Button)top.findViewById(R.id.but_dialog_finish);
        configureDialog();
        mPage.updateSearchIndex();
        getDialog().setTitle("Find/Replace Text");
        return top;
    }

    private void configureDialog()
    {
        mReplaceButton.setOnClickListener(new View.OnClickListener()
                                          {

                                              @Override
                                              public void onClick(View view)
                                              {
                                                  String searchText = mReplaceAutoText.getText().toString();
                                                  if (searchText.length() > 0)
                                                  {
                                                      mPage.updateReplaceHistory(searchText);
                                                      if (!mReplaceHistory.contains(searchText))
                                                      {
                                                          mReplaceHistory.add(searchText);
                                                          mReplaceHistoryAdapter.notifyDataSetChanged();
                                                      }

                                                  }


                                                  replace();

                                              }
                                          }
        );

        mReplaceAllButton.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View view)
            {
                String searchText = mReplaceAutoText.getText().toString();
                if (searchText.length() > 0)
                {
                    mPage.updateReplaceHistory(searchText);
                    if (!mReplaceHistory.contains(searchText))
                    {
                        mReplaceHistory.add(searchText);
                        mReplaceHistoryAdapter.notifyDataSetChanged();
                    }
                }
                replaceAll();
            }
        });

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
                    mPage.updateSearchHistory(searchText);
                    if (!mSearchHistory.contains(searchText))
                    {
                        mSearchHistory.add(searchText);
                        mSearchHistoryAdapter.notifyDataSetChanged();
                    }

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
                String searchText = mSearchAutoTExt.getText().toString();
                if (searchText.length() > 0)
                {
                    mPage.updateSearchHistory(searchText);
                    if (!mSearchHistory.contains(searchText))
                    {
                        mSearchHistory.add(searchText);
                        mSearchHistoryAdapter.notifyDataSetChanged();
                    }
                    gotoPrev(searchText);
                }
                else
                {
                    Toast.makeText(getActivity(), "No text to search for", Toast.LENGTH_LONG).show();
                }

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
        clearCurrentHighlighResult();
    }

    @Override
    public void onStart()
    {
        super.onStart();

        toogleSearchInSelection(true);
        Tools.postEvent(new ReplaceModeEvent());
        Tools.postEvent(new GetSelectionBoundsEvent() {
            @Override
            public void onSelection(LispEditText.Selection selection)
            {
                mSelection = selection;
                mSearchInSelectionChk.setChecked(mSelection != null);
            }
        });
    }

    private void toogleSearchInSelection(boolean inSelection)
    {

        if (mSelection != null && inSelection)
        {
            mSearchPos = mInitialSearchStartPos = mSelection.getSelectionStart();
            mSearchEndPos = mSelection.getSelectionEnd();
        }
        else
        {
            mInitialSearchStartPos = 0;
            mSearchEndPos = mSearchText.length();
        }

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
        Tools.postEvent(new NormalModeEvent());
        //Tools.unRegisterEventHandler(this);
        super.onStop();
    }


    private void searchNext(final String text)
    {
        int nextPos = findNextPositionInRange(text);
        if (nextPos != -1)
        {
            clearCurrentHighlighResult();
            mPrevSearchText = text;
            mSearchPos = nextPos;
            updateSelection(text);


        }
    }

    private void gotoPrev(String text)
    {
        int prevPos = findPrevPositionInRange(text);
        if (prevPos != -1)
        {
            clearCurrentHighlighResult();
            mPrevSearchText = text;
            mSearchPos = prevPos;
            updateSelection(text);
        }
    }



    private int findNextPositionInRange(String textToFind)
    {
        if (textToFind.length() == 0)
            return -1;

        int replacePos = -1;
        if (mSearchPos + textToFind.length() <= mSearchEndPos)
        {
            if (mSearchPos != mInitialSearchStartPos)
                replacePos = mSearchText.indexOf(textToFind, mSearchPos + 1);
            else
                replacePos = mSearchText.indexOf(textToFind, mSearchPos);
        }
        if (replacePos == -1 || replacePos >= mSearchEndPos)
        {
            replacePos = mSearchText.indexOf(textToFind, mInitialSearchStartPos);
        }
        if (replacePos == -1 || replacePos >= mSearchEndPos)
            return -1;
        else
            return replacePos;
    }

    private int findPrevPositionInRange(String textToFind)
    {
        if (textToFind.length() == 0)
            return -1;

        int replacePos = mSearchText.substring(0, mSearchPos).lastIndexOf(textToFind, mInitialSearchStartPos);
        if (replacePos == -1 || replacePos >= mSearchPos)
        {

            replacePos = mSearchText.lastIndexOf(textToFind, mSearchPos);

        }
        if (replacePos == -1 || replacePos >= mSearchEndPos)
            return -1;
        else
            return replacePos;
    }

    private void replace()
    {
        replace(mReplaceAutoText.getText().toString(), mSearchAutoTExt.getText().toString());
    }

    private void replace(String newText, String oldText)
    {

        if (oldText.length() > 0 && mSearchText.substring(mSearchPos, mSearchPos + oldText.length()).equals(oldText))
        {
            clearCurrentHighlighResult();
            mPrevSearchText = null;

            mSearchText = mSearchText.substring(0, mSearchPos) + newText + mSearchText.substring(mSearchPos + oldText.length());
            Tools.postEvent(ReplaceTextEvent.make(mSearchPos, oldText.length(), newText));
        }
        else
            Toast.makeText(getActivity(), "Find the text to replace", Toast.LENGTH_LONG).show();

    }

    private void replaceAll()
    {
        String newText = mReplaceAutoText.getText().toString();
        String oldText = mSearchAutoTExt.getText().toString();
        int nextPos;
        while ((nextPos = findNextPositionInRange(oldText))!= -1)
        {
            mSearchPos = nextPos;
            replace(newText, oldText);
        }

    }



}
