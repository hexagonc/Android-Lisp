package com.evolved.automata.android.lisp.views;

import android.content.Context;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.Lambda;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by Evolved8 on 8/22/17.
 */

public class ListViewProxy extends ViewProxy {
    ArrayList<ViewProxy> mChildren;

    ArrayAdapter<ViewProxy> mViewAdapter;
    AdapterView.OnItemClickListener mItemClickListener = null;
    AdapterView.OnItemSelectedListener mItemSelectListener = null;
    AdapterView.OnItemLongClickListener mItemLongClickListener = null;

    public static final String _ON_ITEM_CLICK_LISTENER = ":on-item-clicked";
    public static final String _ON_ITEM_SELECT_LISTENER = ":on-item-selected";
    public static final String _ON_ITEM_LONG_CLICK_LISTENER = ":on-item-long-click";

    public ListViewProxy(final Context con, HashMap<String, Value> keys, ArrayList<ViewProxy> children)
    {
        super(con, keys);
        mChildren = children;
        mViewAdapter = new ArrayAdapter<ViewProxy>(con, 0, mChildren) {

            @NonNull
            @Override
            public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent)
            {
                ViewProxy proxy = mChildren.get(position);
                if (proxy.getView() == null)
                    convertView = proxy.createView(parent, new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT));
                else
                    convertView = proxy.getView();

                return convertView;
            }
        };
    }

    public View createBaseView()
    {
        ListView actual = new ListView(context);
        actual.setAdapter(mViewAdapter);
        return actual;
    }

    public void processItemSelectListenerKeywords(HashMap<String, Value> keys, View actual)
    {
        Value code = keys.get(_ON_ITEM_SELECT_LISTENER);
        if (code != null && !code.isNull())
        {
            final Value transformed = NLispTools.getMinimalEnvironment(_currentEnv, code);
            mItemSelectListener = new AdapterView.OnItemSelectedListener(){


                @Override
                public void onItemSelected(AdapterView<?> adapterView, View view, int i, long l)
                {
                    Environment evaluatedEnvironment = new Environment(_currentEnv);
                    evaluatedEnvironment.mapValue("selected-index", NLispTools.makeValue(i));

                    _lispInterpreter.evaluatePreParsedValue(evaluatedEnvironment, transformed, true);
                }

                @Override
                public void onNothingSelected(AdapterView<?> adapterView)
                {
                    Environment evaluatedEnvironment = new Environment(_currentEnv);
                    evaluatedEnvironment.mapValue("selected-index", Environment.getNull());

                    _lispInterpreter.evaluatePreParsedValue(evaluatedEnvironment, transformed, true);
                }

            };

            if (actual != null)
            {
                ((ListView)actual).setOnItemSelectedListener(mItemSelectListener);
            }
        }
    }


    public void processItemClickListenerKeywords(HashMap<String, Value> keys, View actual)
    {
        Value code = keys.get(_ON_ITEM_CLICK_LISTENER);
        if (code != null && !code.isNull())
        {
            final Value transformed = NLispTools.getMinimalEnvironment(_currentEnv, code);
            mItemClickListener = new AdapterView.OnItemClickListener(){


                @Override
                public void onItemClick(AdapterView<?> adapterView, View view, int i, long l)
                {
                    Environment evaluatedEnvironment = new Environment(_currentEnv);
                    evaluatedEnvironment.mapValue("selected-index", NLispTools.makeValue(i));

                    _lispInterpreter.evaluatePreParsedValue(evaluatedEnvironment, transformed, true);
                }
            };

            if (actual != null)
            {
                ((ListView)actual).setOnItemClickListener(mItemClickListener);
            }
        }
    }

    public void processItemLongClickListenerKeywords(HashMap<String, Value> keys, View actual)
    {
        Value code = keys.get(_ON_ITEM_LONG_CLICK_LISTENER);
        if (code != null && !code.isNull())
        {
            final Value transformed = NLispTools.getMinimalEnvironment(_currentEnv, code);
            mItemLongClickListener = new AdapterView.OnItemLongClickListener(){


                @Override
                public boolean onItemLongClick(AdapterView<?> adapterView, View view, int i, long l)
                {
                    Environment evaluatedEnvironment = new Environment(_currentEnv);
                    evaluatedEnvironment.mapValue("selected-index", NLispTools.makeValue(i));

                    _lispInterpreter.evaluatePreParsedValue(evaluatedEnvironment, transformed, true);
                    return true;
                }

            };

            if (actual != null)
            {
                ((ListView)actual).setOnItemLongClickListener(mItemLongClickListener);
            }
        }
    }

    @Override
    protected void baseUpdate(View view)
    {
        super.baseUpdate(view);
        processItemSelectListenerKeywords(_keys, view);
        processItemClickListenerKeywords(_keys, view);
        processItemLongClickListenerKeywords(_keys, view);
    }

    public void setOnItemClickListener(final Value lambdaValue)
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
            ((ListView)actual).setOnItemClickListener(mItemClickListener);
    }


}
