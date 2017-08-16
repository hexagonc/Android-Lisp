package com.evolved.automata.android.lisp.views;

import android.content.Context;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.KeyEvent;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.AutoCompleteTextView;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.EditText;
import android.widget.TextView;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.Lambda;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by Evolved8 on 8/15/17.
 */

public class AutoCompleteTextViewProxy extends EditViewProxy {

    ArrayList<String> mOptionList;
    ArrayAdapter<String> mOptionAdapter;


    public static final String _ON_SELECTION_CHANGED = ":on-selection-changed";
    public static final String _SELECTED_INDEX = ":selected-index";
    public static final String _COMPLETION_THRESHOLD = ":completion-threshold";

    AdapterView.OnItemClickListener mItemClickListener;



    int mSelectedIndex = 0;

    int mCompletionThreshold = 2;


    public AutoCompleteTextViewProxy(Context con, HashMap<String, Value> keymap, String text, String[] optionItemList)
    {
        super(con, keymap, text);
        mOptionList = new ArrayList<String>();
        if (optionItemList != null)
        {
            for (String s : optionItemList)
            {
                mOptionList.add(s);
            }
        }
        mOptionAdapter  = new ArrayAdapter<String>(con, android.R.layout.simple_dropdown_item_1line, mOptionList);

    }



    public AutoCompleteTextViewProxy updateOptionList(String[] newOptionList)
    {
        mOptionList.clear();
        for (String s:newOptionList)
        {
            mOptionList.add(s);
        }

        View actual = null;
        if (encapsulated != null && (actual = encapsulated.get())!=null)
        {
            AutoCompleteTextView aview = (AutoCompleteTextView)actual;
            aview.setAdapter(mOptionAdapter);
            mOptionAdapter.clear();
            mOptionAdapter.addAll(mOptionList);
        }
        return this;
    }


    @Override
    public View createBaseView()
    {
        AutoCompleteTextView autoComleteTextView = new AutoCompleteTextView(context);
        autoComleteTextView.setAdapter(mOptionAdapter);
        if (mOptionList.size() > mSelectedIndex)
            autoComleteTextView.setSelection(mSelectedIndex);
        else
            autoComleteTextView.clearListSelection();

        autoComleteTextView.setThreshold(mCompletionThreshold);
        return createBaseView(autoComleteTextView);
    }

    @Override
    protected void processKeywords(HashMap<String, Value> keys, TextView tview)
    {
        super.processKeywords(keys, tview);
        processSelectionChangedListener(keys, (AutoCompleteTextView)tview);
        processSelectionKeywords(keys, (AutoCompleteTextView)tview);
        processThresholdKeywords(keys, (AutoCompleteTextView)tview);
    }


    public void processThresholdKeywords(HashMap<String, Value> keywords, AutoCompleteTextView actual)
    {
        Value index = keywords.get(_COMPLETION_THRESHOLD);
        if (index != null && index.isInteger())
        {
            mCompletionThreshold = (int)index.getIntValue();
            actual.setThreshold(mCompletionThreshold);
        }
    }




    public void processSelectionKeywords(HashMap<String, Value> keywords, AutoCompleteTextView actual)
    {
        Value index = keywords.get(_SELECTED_INDEX);
        if (index != null && index.isInteger())
        {
            int i = (int)index.getIntValue();

            if (i < mOptionList.size())
            {
                mSelectedIndex = i;
                actual.setSelection(i);
            }
        }
    }

    public void processSelectionChangedListener(HashMap<String, Value> keys, AutoCompleteTextView actual)
    {

        final Value code = getMapValue(keys, _ON_SELECTION_CHANGED);
        if (code.isNull())
            return;
        final Value transformed = NLispTools.getMinimalEnvironment(_currentEnv, code);
        mItemClickListener = new AdapterView.OnItemClickListener() {

            @Override
            public void onItemClick(AdapterView<?> adapterView, View view, int i, long l)
            {
                Environment evaluatedEnvironment = new Environment(_currentEnv);
                evaluatedEnvironment.mapValue("selected-index", NLispTools.makeValue(i));

                _lispInterpreter.evaluatePreParsedValue(evaluatedEnvironment, transformed, true);
            }



        };

        actual.setOnItemClickListener(mItemClickListener);
    }


    public void setSelection(int selectionIndex)
    {
        mSelectedIndex = selectionIndex;
        View actual;
        if (encapsulated != null && (actual = encapsulated.get())!= null)
            ((AutoCompleteTextView)actual).setSelection(mSelectedIndex);
    }

    public void setCompletionThreshold(int threshold)
    {
        mCompletionThreshold = threshold;
        View actual;
        if (encapsulated != null && (actual = encapsulated.get())!= null)
            ((AutoCompleteTextView)actual).setThreshold(mCompletionThreshold);
    }

    public void setOnSelectionChangedListener(final Value lambdaValue)
    {
        if (!lambdaValue.isLambda())
            return;

        mItemClickListener = new AdapterView.OnItemClickListener(){

            @Override
            public void onItemClick(AdapterView<?> adapterView, View view, int i, long l)
            {
                Lambda function = (Lambda) lambdaValue.getLambda();
                function.setActualParameters(new Value[]{NLispTools.makeValue(i)});

                _lispInterpreter.evaluateFunction(function, _currentEnv);
            }


        };


        View actual;
        if (encapsulated != null && (actual = encapsulated.get())!= null)
            ((AutoCompleteTextView)actual).setOnItemClickListener(mItemClickListener);
    }


}
