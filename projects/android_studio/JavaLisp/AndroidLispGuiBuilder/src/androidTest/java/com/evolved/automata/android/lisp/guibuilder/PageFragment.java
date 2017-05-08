package com.evolved.automata.android.lisp.guibuilder;

import android.app.Fragment;
import android.app.FragmentTransaction;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Toast;

/**
 * Created by Evolved8 on 5/8/17.
 */

public class PageFragment extends Fragment {

    CodePageFragment mCodeFragment;
    RenderFragment mUIFragment;

    Fragment mCurrentFragment;

    CodePage mPage;

    LispContext mUIContext;

    public PageFragment()
    {
        mCodeFragment = new CodePageFragment();
        mUIFragment = new RenderFragment();
        mCurrentFragment = mCodeFragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        mUIContext = new LispContext(mPage.getBasePageLispContext(), getActivity());
        mUIContext.setActivity(getActivity());

        mUIFragment.setEnvironment(mUIContext.getEnvironment());
        mCodeFragment.setLispContext(mUIContext);
    }

    public void setPage(CodePage page)
    {
        mPage = page;
        mCodeFragment.setCodePage(page);
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState)
    {
        return inflater.inflate(R.layout.v2_page, container, false);
    }

    public void switchToRenderView()
    {

        if (mCurrentFragment != mUIFragment)
        {
            Log.i(".:.:.:.:.:", "Starting transaction to switch to RenderFragment");
            FragmentTransaction trans = getChildFragmentManager().beginTransaction();
            trans.remove(mCurrentFragment);
            trans.add(R.id.v2_frag_page_form_container, mUIFragment);
            trans.setTransition(FragmentTransaction.TRANSIT_FRAGMENT_FADE);
            trans.commit();

            mCurrentFragment = mUIFragment;
        }

    }

    public void switchToCodeView()
    {
        if (mCurrentFragment != mCodeFragment)
        {
            Log.i(".:.:.:.:.:", "Starting transaction to switch to CodePageFragment");
            FragmentTransaction trans = getChildFragmentManager().beginTransaction();
            trans.remove(mCurrentFragment);
            trans.add(R.id.v2_frag_page_form_container, mCodeFragment);
            trans.setTransition(FragmentTransaction.TRANSIT_FRAGMENT_FADE);
            trans.commit();

            mCurrentFragment = mCodeFragment;
        }

    }



    @Override
    public void onStart()
    {
        super.onStart();
        FragmentTransaction trans = getChildFragmentManager().beginTransaction();
        trans.replace(R.id.v2_frag_page_form_container, mCurrentFragment);

        trans.commit();
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

    @Override
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater)
    {
        inflater.inflate(R.menu.v2_code_display, menu);

        inflater.inflate(R.menu.v2_render_display, menu);
        super.onCreateOptionsMenu(menu, inflater);
    }

    @Override
    public void onPrepareOptionsMenu(Menu menu)
    {
        MenuItem toCode = menu.findItem(R.id.v2_menu_to_code);
        MenuItem toRender = menu.findItem(R.id.v2_menu_render);

        if (mCurrentFragment == mCodeFragment)
        {
            toRender.setVisible(true);
            toCode.setVisible(false);
        }
        else
        {
            toRender.setVisible(false);
            toCode.setVisible(true);
        }
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item)
    {
        switch (item.getItemId())
        {
            case R.id.v2_menu_render:
                if (mUIContext.getEnvironment().getVariableValue(RenderFragment.VIEW_PROXY_VAR_NAME) != null)
                {
                    switchToRenderView();
                }
                else
                    Toast.makeText(getActivity(), "Nothing to show.  Set " + RenderFragment.VIEW_PROXY_VAR_NAME + " to a view proxy", Toast.LENGTH_LONG).show();;
                return true;
            case R.id.v2_menu_to_code:
                switchToCodeView();
                return true;
            case R.id.v2_menu_save_all:
                mPage.getApplication().save(true);
                return true;

            case R.id.v2_menu_save_page:
                mPage.savePage();
                return true;
        }
        return super.onOptionsItemSelected(item);
    }
}
