package com.evolved.automata.android.lisp.guibuilder;

import android.app.Fragment;
import android.os.Bundle;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import com.evolved.automata.android.lisp.guibuilder.R;
import com.evolved.automata.android.lisp.views.ViewProxy;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.Value;

/**
 * Created by Evolved8 on 5/8/17.
 */

public class RenderFragment extends Fragment {


    Environment mPageEnvironment;
    public static final String VIEW_PROXY_VAR_NAME = "VIEW_PROXY";

    CodePage mPage;


    @Override
    public View onCreateView(LayoutInflater inflater,ViewGroup container, Bundle savedInstanceState)
    {

        ViewGroup group = (ViewGroup)inflater.inflate(R.layout.v2_render_pane, container, false);
        Value proxy = mPageEnvironment.getVariableValue(VIEW_PROXY_VAR_NAME);
        if (proxy != null && !proxy.isNull())
        {
            Object ovalue = proxy.getObjectValue();
            if (ovalue instanceof ViewProxy)
            {
                ViewProxy vproxy = (ViewProxy)ovalue;
                vproxy.setLispInterpreter(mPageEnvironment, mPage.mBasePageLispContext.getForegroundInterpreter());
                View v = vproxy.createView(group, new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT));

                if (v != null)
                {
                    group.addView(v);
                    Log.i("-+*+--+*+--+*+-", "RenderFragment onCreateView - added new proxy to fragment");
                }
                else
                {
                    Log.i("-+*+--+*+--+*+-", "RenderFragment onCreateView - failed to create view because proxy didn't create a view");
                }



            }
            else
            {
                Log.i("-+*+--+*+--+*+-", "RenderFragment onCreateView - failed to create view due to incorrect boject type");
            }
        }
        else
        {
            Log.i("-+*+--+*+--+*+-", "RenderFragment onCreateView - failed to create new view because proxy doesn't exist");
        }

        return group;

    }

    @Override
    public void onStart()
    {
        super.onStart();
        Log.i("-+*+--+*+--+*+-", "RenderFragment onStart");
    }

    @Override
    public void onResume()
    {
        super.onResume();
        Log.i("-+*+--+*+--+*+-", "RenderFragment onREsume");
    }

    @Override
    public void onPause()
    {
        Log.i("-+*+--+*+--+*+-", "RenderFragment onPause");
        super.onPause();
    }

    @Override
    public void onStop()
    {
        Log.i("-+*+--+*+--+*+-", "RenderFragment onStop");
        super.onStop();
    }

    @Override
    public void onDestroyView()
    {
        Log.i("-+*+--+*+--+*+-", "RenderFragment onDestroyView");
        super.onDestroyView();
    }

    @Override
    public void onDetach()
    {
        Log.i("-+*+--+*+--+*+-", "RenderFragment onDetach");
        super.onDetach();
    }

    public void setEnvironment(Environment pageEnvironment, CodePage page)
    {
        mPageEnvironment = pageEnvironment;
        mPage = page;

    }
}
