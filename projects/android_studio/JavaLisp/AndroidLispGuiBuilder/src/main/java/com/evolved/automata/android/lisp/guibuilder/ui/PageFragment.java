package com.evolved.automata.android.lisp.guibuilder.ui;


import android.os.Bundle;

import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentTransaction;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Toast;

import com.evolved.automata.android.lisp.guibuilder.MenuHelper;
import com.evolved.automata.android.lisp.guibuilder.R;
import com.evolved.automata.android.lisp.guibuilder.model.CodePage;
import com.evolved.automata.android.lisp.guibuilder.model.Page;


/**
 * Created by Evolved8 on 5/8/17.
 */

public class PageFragment extends Fragment {

    CodePageFragment mCodeFragment;
    RenderFragment mUIFragment;

    Fragment mCurrentFragment = null;

    CodePage mPage;



    Page.PAGE_TYPE mCurrrentPageType = Page.PAGE_TYPE.CODE;

    public PageFragment()
    {

    }

    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);



    }




    public void setPage(CodePage page)
    {
        mPage = page;

    }

    public void updatePage()
    {
        mCodeFragment.setCodePage(mPage);
        if (mCurrrentPageType == Page.PAGE_TYPE.CODE)
        {
            mCodeFragment.updatePage();
        }

    }

    public CodePage getPage()
    {
        return mPage;
    }


    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
    {
        Log.i(".:.:.:.:.:", "onCreateView PageFragment.");
        ViewGroup parent =  (ViewGroup)inflater.inflate(R.layout.v2_page, container, false);

        mCodeFragment = new CodePageFragment();
        mUIFragment = new RenderFragment();

        if (mPage.getUILispContext() == null)
            mPage.defineUIContext(getActivity());

        mUIFragment.setEnvironment(mPage.getUILispContext().getEnvironment(), mPage);
        mCodeFragment.setLispContext(mPage.getUILispContext());
        mCodeFragment.setCodePage(mPage);
        if (mCurrrentPageType == Page.PAGE_TYPE.CODE)
        {
            switchToCodeView();
        }
        else
        {
            switchToRenderView();
        }

        return parent;
    }

    public void switchToRenderView()
    {

        if (mCurrentFragment != mUIFragment)
        {

            Log.i(".:.:.:.:.:", "Starting transaction to switch to RenderFragment");
            FragmentTransaction trans = getChildFragmentManager().beginTransaction();
            if (mCurrentFragment != null)
                trans.remove(mCurrentFragment);
            trans.add(R.id.v2_frag_page_form_container, mUIFragment);
            trans.setTransition(FragmentTransaction.TRANSIT_FRAGMENT_FADE);
            trans.commit();
            mCurrentFragment = mUIFragment;

            mCurrrentPageType = Page.PAGE_TYPE.UI;
        }

    }

    public void switchToCodeView()
    {
        if (mCurrentFragment != mCodeFragment)
        {

            Log.i(".:.:.:.:.:", "Starting transaction to switch to CodePageFragment");
            FragmentTransaction trans = getChildFragmentManager().beginTransaction();
            if (mCurrentFragment != null)
                trans.remove(mCurrentFragment);
            trans.add(R.id.v2_frag_page_form_container, mCodeFragment);
            trans.setTransition(FragmentTransaction.TRANSIT_FRAGMENT_FADE);
            trans.commit();
            mCurrentFragment = mCodeFragment;
            mCurrrentPageType = Page.PAGE_TYPE.CODE;
        }

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

    @Override
    public void onDestroyView()
    {
        Log.d("oooooo", "onDestroyView Page fragment: " + mPage.getTitle());
        super.onDestroyView();
    }

    @Override
    public void onDestroy()
    {
        Log.d("oooooo", "onDestroy Page fragment: " + mPage.getTitle());
        super.onDestroy();
    }

    @Override
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater)
    {
//        inflater.inflate(R.menu.v2_code_display, menu);
//
//        inflater.inflate(R.menu.v2_render_display_menu, menu);
        super.onCreateOptionsMenu(menu, inflater);
    }

    @Override
    public void onPrepareOptionsMenu(Menu menu)
    {
        MenuHelper.updateMenuItemDisplay(menu, this);
        if (mCodeFragment!=null && mCodeFragment.isVisible())
            mCodeFragment.onPrepareOptionsMenu(menu);
        else if (mUIFragment != null && mUIFragment.isVisible())
            mUIFragment.onPrepareOptionsMenu(menu);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item)
    {
        switch (item.getItemId())
        {
            case R.id.v2_menu_render:
                if (mPage.getUILispContext().getEnvironment().getVariableValue(RenderFragment.VIEW_PROXY_VAR_NAME) != null)
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