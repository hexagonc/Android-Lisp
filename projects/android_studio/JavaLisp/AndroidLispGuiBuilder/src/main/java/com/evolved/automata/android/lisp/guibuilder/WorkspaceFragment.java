package com.evolved.automata.android.lisp.guibuilder;

import android.app.Activity;

import android.graphics.Color;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentTransaction;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.ImageButton;
import android.widget.ImageView;
import android.widget.TextView;

import com.evolved.automata.android.widgets.ShadowButton;

import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;

import java.util.HashMap;
import java.util.LinkedList;

/**
 * Created by Evolved8 on 5/5/17.
 */

public class WorkspaceFragment extends android.support.v4.app.Fragment {




    Animation mFlashAnimation = null;
    Animation mHideAnimation = null;

    ShadowButton mStatusAlertsButton;


    Workspace mWorkspace;

    LinkedList<PageFragment> mPages;

    ImageView mProgressIcon;

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

        mFlashAnimation = AnimationUtils.loadAnimation(getActivity(), R.anim.v2_flash_alert);
        mHideAnimation = AnimationUtils.loadAnimation(getActivity(), R.anim.hide_flash_alert);

        mNavigatePreviousButton = (ShadowButton)group.findViewById(R.id.v2_but_history_back);
        mNavigateNextButton = (ShadowButton)group.findViewById(R.id.v2_but_history_forward);
        mAddPageButton = (ImageButton)group.findViewById(R.id.v2_but_add_page);
        mPageTitleView = (TextView)group.findViewById(R.id.v2_txt_page_title);
        mStatusAlertsButton = (ShadowButton)group.findViewById(R.id.v2_but_status_more_info);
        mProgressIcon = (ImageView)group.findViewById(R.id.img_progress_icon);
        mStatusAlertsButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v)
            {

                handleStatusAlertClick();
            }
        });


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

    private void showAppStatusDialog()
    {

        final String fragTag = "app_status_log";
        AppStatusLogDialogFragment frag = AppStatusLogDialogFragment.create();

        frag.show(getFragmentManager(), fragTag);
    }


    @Subscribe(threadMode = ThreadMode.MAIN)
    public void onShowEventLogEvent(EventLog.ShowEventLogEvent event)
    {
        showAppStatusDialog();
    }

    @Subscribe(threadMode = ThreadMode.MAIN)
    public void onNewStatusEvent(GlobalStatusAlertEvent event)
    {
        toggleAlertFlash(true);
        switch (event.getType())
        {
            case NEW_ERROR_ENTRIES:
                showNewErrorAlertStatus((NewErrorLogEntriesEvent)event);
                break;
            case NEW_INFO_ENTRIES:
                showNewInfoAlertStatus((NewInfoLogEntriesEvent)event);
                break;
            case NEW_BACKGROUND_RESULT:
                showNewBackgroundAlertStatus((NewBackgroundResultEvent)event);
                break;
        }
    }

    private void toggleAlertFlash(boolean enabledP)
    {
        if (enabledP)
        {
            mStatusAlertsButton.clearAnimation();
            mStatusAlertsButton.setAnimation(mFlashAnimation);
            mFlashAnimation.start();
        }
        else
        {
            mStatusAlertsButton.clearAnimation();
            mStatusAlertsButton.setAnimation(mHideAnimation);
        }
    }


    private void showNewErrorAlertStatus(NewErrorLogEntriesEvent event)
    {
        mStatusAlertsButton.setVisibility(View.VISIBLE);
        mStatusAlertsButton.setTextColor(getResources().getColor(R.color.new_error_status_alert));
        mStatusAlertsButton.setOnClickListener(getErrorStatusClickListener(event));
    }

    private void showNewInfoAlertStatus(NewInfoLogEntriesEvent event)
    {
        mStatusAlertsButton.setVisibility(View.VISIBLE);
        mStatusAlertsButton.setTextColor(getResources().getColor(R.color.new_info_status_alert));
        mStatusAlertsButton.setOnClickListener(getInfoStatusClickListener(event));
    }

    private void showNewBackgroundAlertStatus(NewBackgroundResultEvent event)
    {
        mStatusAlertsButton.setVisibility(View.VISIBLE);
        mStatusAlertsButton.setTextColor(getResources().getColor(R.color.new_background_result_status_alert));
        mStatusAlertsButton.setOnClickListener(getBackgroundResultClickListener(event));
    }

    private void hideAlertStatus()
    {
        toggleAlertFlash(false);
        mStatusAlertsButton.setTextColor(getResources().getColor(android.R.color.black));
        mStatusAlertsButton.setVisibility(View.INVISIBLE);

    }


    View.OnClickListener getErrorStatusClickListener(final NewErrorLogEntriesEvent event)
    {
        return new View.OnClickListener() {
            @Override
            public void onClick(View v)
            {
                hideAlertStatus();
                showAppStatusDialog();
            }
        };
    }

    View.OnClickListener getInfoStatusClickListener(final NewInfoLogEntriesEvent event)
    {
        return new View.OnClickListener() {
            @Override
            public void onClick(View v)
            {
                hideAlertStatus();
                showAppStatusDialog();
            }
        };
    }

    View.OnClickListener getBackgroundResultClickListener(final NewBackgroundResultEvent event)
    {
        return new View.OnClickListener() {
            @Override
            public void onClick(View v)
            {
                hideAlertStatus();
                showAppStatusDialog();
            }
        };
    }


    private void handleStatusAlertClick()
    {
        hideAlertStatus();
    }

    @Deprecated
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
            try
            {
                CodePage page = mWorkspace.getApplication().retrieveCodePage(pageId);
                PageFragment pageFragment = new PageFragment();
                pageFragment.setPage(page);
                mPages.add(pageFragment);
            }
            catch (Exception e)
            {
                e.printStackTrace();
            }

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
                                                                                page.assertTopParseNodeIsInValid();
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
        Tools.registerEventHandler(this);
    }

    @Override
    public void onPause()
    {

        Tools.unRegisterEventHandler(this);
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

    @Subscribe(threadMode = ThreadMode.MAIN)
    public void onProgressEvent(BackgroundProcessEvent event)
    {
        if (event.getType() == BackgroundProcessEvent.TYPE.STARTING)
        {
            mProgressIcon.setVisibility(View.VISIBLE);
        }
        else
            mProgressIcon.setVisibility(View.INVISIBLE);

    }
}
