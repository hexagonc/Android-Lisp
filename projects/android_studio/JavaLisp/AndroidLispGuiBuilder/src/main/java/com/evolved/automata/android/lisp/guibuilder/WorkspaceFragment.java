package com.evolved.automata.android.lisp.guibuilder;

import android.app.Activity;
import android.app.Fragment;
import android.app.FragmentManager;
import android.app.FragmentTransaction;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;
import android.widget.TextView;

import com.evolved.automata.android.widgets.ShadowButton;

import java.util.HashMap;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 5/5/17.
 */

public class WorkspaceFragment extends Fragment {

    Workspace mWorkspace;

    LinkedList<PageFragment> mPages;



    ShadowButton mNavigatePreviousButton;
    ShadowButton mNavigateNextButton;
    ImageButton mAddPageButton;
    TextView mPageTitleView;


    public void setWorkspace(Workspace work)
    {
        mWorkspace = work;

        // Allowing this potential error for now.  Will probably eventually remove UIPage class
        // TODO - figure out whether to keep UIPage, CodePage distinction
        loadAllPages();

    }


    public CodePage getCurrentPage()
    {
        return getCurrentPageFragment().getPage();
    }

    public PageFragment getCurrentPageFragment()
    {
        int currentPageIndex = mWorkspace.getCurrentPageIndex();
        return mPages.get(currentPageIndex);
    }

    @Override
    public void onAttach(Activity activity)
    {
        super.onAttach(activity);
    }


    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
    {
        ViewGroup group = (ViewGroup)inflater.inflate(R.layout.v2_workspace_fragment, container, false);

        mNavigatePreviousButton = (ShadowButton)group.findViewById(R.id.v2_but_history_back);
        mNavigateNextButton = (ShadowButton)group.findViewById(R.id.v2_but_history_forward);
        mAddPageButton = (ImageButton)group.findViewById(R.id.v2_but_add_page);
        mPageTitleView = (TextView)group.findViewById(R.id.v2_txt_page_title);


        mNavigatePreviousButton.setOnClickListener(new View.OnClickListener(){

            @Override
            public void onClick(View v)
            {
                mWorkspace.gotoPrevPage();
                gotoCurrentPage();
            }
        });

        mNavigateNextButton.setOnClickListener(new View.OnClickListener(){

            @Override
            public void onClick(View v)
            {
                mWorkspace.gotoNextPage();
                gotoCurrentPage();
            }
        });

        mAddPageButton.setOnClickListener(new View.OnClickListener(){

            @Override
            public void onClick(View v)
            {
                CodePage page = (CodePage)mWorkspace.addPage();
                PageFragment fragment = new PageFragment();
                fragment.setPage(page);
                mPages.add(mWorkspace.getCurrentPageIndex(), fragment);
                gotoCurrentPage();
            }
        });


        gotoCurrentPage();


        return group;
    }


    boolean deleteCurrentPage()
    {
        boolean success = mWorkspace.deletePage(mWorkspace.getCurrentPageIndex());
        mPages.remove(mWorkspace.getCurrentPageIndex());
        gotoCurrentPage();
        return success;
    }

    void gotoCurrentPage()
    {
        if (mWorkspace.canMoveNextP())
        {
            mNavigateNextButton.setVisibility(View.VISIBLE);
        }
        else
            mNavigateNextButton.setVisibility(View.INVISIBLE);

        if (mWorkspace.canMovePrevP())
        {
            mNavigatePreviousButton.setVisibility(View.VISIBLE);
        }
        else
            mNavigatePreviousButton.setVisibility(View.INVISIBLE);

        CodePage page = getCurrentPage();
        mPageTitleView.setText(page.getTitle());


        FragmentTransaction transaction = getChildFragmentManager().beginTransaction();

        transaction.replace(R.id.v2_frag_page, getCurrentPageFragment());
        transaction.setTransition(FragmentTransaction.TRANSIT_FRAGMENT_OPEN);
        transaction.commit();
    }

    private void loadAllPages()
    {
        mPages = new LinkedList<PageFragment>();

        for (String pageId:mWorkspace.getChildPageIds())
        {
            CodePage page = mWorkspace.getApplication().retrieveCodePage(pageId);
            PageFragment pageFragment = new PageFragment();
            pageFragment.setPage(page);
            mPages.add(pageFragment);
        }
    }

    public void showPageProperties()
    {

        final String fragTag = "page_properties";
        PagePropertiesFragment frag = PagePropertiesFragment.create(new PagePropertiesFragment.OnCompleteListener() {
                                                                        @Override
                                                                        public void close()
                                                                        {
                                                                            FragmentManager fm = getFragmentManager();
                                                                            FragmentTransaction ft = fm.beginTransaction();
                                                                            Fragment f = fm.findFragmentByTag(fragTag);
                                                                            if (f!=null)
                                                                                ft.remove(f).commit();

                                                                        }

                                                                        @Override
                                                                        public void close(HashMap<PagePropertiesFragment.CHANGE_TYPE, PagePropertiesFragment.Change> changes)
                                                                        {
                                                                            FragmentManager fm = getFragmentManager();
                                                                            FragmentTransaction ft = fm.beginTransaction();
                                                                            Fragment f = fm.findFragmentByTag(fragTag);
                                                                            if (f!=null)
                                                                                ft.remove(f).commit();

                                                                            if (changes.containsKey(PagePropertiesFragment.CHANGE_TYPE.TITLE))
                                                                            {
                                                                                PagePropertiesFragment.ChangeTitle cTitle = (PagePropertiesFragment.ChangeTitle)changes.get(PagePropertiesFragment.CHANGE_TYPE.TITLE);

                                                                                CodePage page = getCurrentPage();
                                                                                page.setTitle(cTitle.newTitle());
                                                                                mPageTitleView.setText(cTitle.newTitle());

                                                                            }

                                                                            if (changes.containsKey(PagePropertiesFragment.CHANGE_TYPE.DROPBOX_SYNC_FILE))
                                                                            {
                                                                                PagePropertiesFragment.ChangeDropboxSyncFile cS = (PagePropertiesFragment.ChangeDropboxSyncFile)changes.get(PagePropertiesFragment.CHANGE_TYPE.DROPBOX_SYNC_FILE);
                                                                                CodePage page = getCurrentPage();
                                                                                page.setDropboxPath(cS.getSyncFile());
                                                                            }
                                                                            if (changes.containsKey(PagePropertiesFragment.CHANGE_TYPE.CHANGE_TEXT))
                                                                            {
                                                                                PagePropertiesFragment.ChangeText cT = (PagePropertiesFragment.ChangeText)changes.get(PagePropertiesFragment.CHANGE_TYPE.CHANGE_TEXT);
                                                                                CodePage page = getCurrentPage();
                                                                                page.setExpr(cT.newText());
                                                                                getCurrentPageFragment().updatePage();
                                                                            }
                                                                        }
                                                                    },
                getCurrentPage());
        frag.show(getFragmentManager(), fragTag);
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
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater)
    {
        getCurrentPageFragment().onCreateOptionsMenu(menu, inflater);
    }

    @Override
    public void onPrepareOptionsMenu(Menu menu)
    {
        getCurrentPageFragment().onPrepareOptionsMenu(menu);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item)
    {

        if (item.getItemId() == R.id.v2_menu_page_properties)
        {
            showPageProperties();
            return true;
        }
        else
            return getCurrentPageFragment().onOptionsItemSelected(item);
    }
}