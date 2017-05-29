package com.evolved.automata.android.lisp.guibuilder;

import android.app.ActionBar;
import android.app.Activity;
import android.app.Fragment;
import android.app.FragmentManager;
import android.app.FragmentTransaction;
import android.content.Context;
import android.graphics.Rect;
import android.os.Bundle;

import android.os.IBinder;
import android.os.ResultReceiver;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewTreeObserver;
import android.view.inputmethod.InputMethodManager;

import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;

import io.reactivex.Observer;
import io.reactivex.annotations.NonNull;
import io.reactivex.disposables.Disposable;

/**
 * Created by Evolved8 on 4/21/17.
 */

public class ALGBBaseActivity extends Activity implements LogHandler {

    public interface KeyboardVisibility
    {
        boolean isVisible();
    }

    Runnable mOnTestComplete = null;

    Page mTestPage;
    ALGB mApplication;
    Workspace mCurrentWorkspace;

    WorkspaceFragment mCurrentWorkspaceFragment;

    HashMap<String, WorkspaceFragment> mWorkspaceMap;
    InputMethodManager mIMM = null;


    @Override
    protected void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.v2_activity_main);
        mIMM = (InputMethodManager)ALGBBaseActivity.this
                .getSystemService(Context.INPUT_METHOD_SERVICE);



        mWorkspaceMap = new HashMap<String, WorkspaceFragment>();


        ActionBar bar = getActionBar();
        bar.show();

        try
        {

            mApplication = ((ALGBApplication)getApplication()).getALGB();
            changeToWorkspace(mApplication.getCurrentWorkspace());

        }
        catch (Exception e)
        {
            logError("Failed creating", e.toString());
            EventLog.get().logSystemError(e);
        }

    }



    private boolean showSoftKeyboardForMainCodeTextP()
    {
        View codeText = findViewById(R.id.v2_edit_main_code);
        if (codeText == null)
            return false;
        return mIMM.showSoftInput(codeText, InputMethodManager.SHOW_IMPLICIT);
    }



    private void changeToWorkspace(Workspace workspace)
    {
        mCurrentWorkspace = workspace;
        String workspaceId = mCurrentWorkspace.getWorkspaceId();

        WorkspaceFragment frag = mWorkspaceMap.get(workspaceId);
        if (frag == null)
        {
            frag = new WorkspaceFragment();
            frag.setWorkspace(workspace);
            mWorkspaceMap.put(workspaceId, frag);
        }

        invalidateOptionsMenu();

        mCurrentWorkspaceFragment = frag;
        FragmentManager manager = getFragmentManager();
        FragmentTransaction ft = manager.beginTransaction();
        ft.replace(R.id.v2_top, mCurrentWorkspaceFragment);
        ft.commit();

    }

    public void setOnTestCompleteHandler(Runnable r)
    {
        mOnTestComplete = r;
    }

    @Override
    protected void onStart()
    {
        super.onStart();
        System.out.println("<><><><<><"+getApplication().getClass().toString());

        //startImeWatcher();

    }

    @Override
    protected void onResume()
    {
        super.onResume();
    }

    @Override
    protected void onPause()
    {
        super.onPause();
    }

    @Override
    protected void onStop()
    {
        //mApplication.getMainhandler().removeCallbacks(mIMEWatcher);
        super.onStop();
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.v2_main, menu);
        if (mCurrentWorkspaceFragment != null)
            mCurrentWorkspaceFragment.onCreateOptionsMenu(menu, getMenuInflater());
        return true;
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu)
    {
        if (mCurrentWorkspaceFragment != null)
            mCurrentWorkspaceFragment.onPrepareOptionsMenu(menu);
        return true;
    }


    @Override
    public boolean onOptionsItemSelected(MenuItem item)
    {

        if (item.getItemId() == R.id.v2_menu_post_test_login)
        {
            Tools.postEvent(new DropboxManager.LoginEvent(DropboxManager.get().getTestClient()));
            return true;
        }
        else if (item.getItemId() == R.id.v2_menu_workspace_management)
        {
            showWorkspaceManager();
            return true;

        }else if (item.getItemId() == R.id.v2_menu_send_test_status_event)
        {
            Tools.postEvent(new NewErrorLogEntriesEvent());
            //Tools.postEvent(new NewBackgroundResultEvent());
            //Tools.postEvent(new NewInfoLogEntriesEvent());
            return true;
        }
        else
        {

            if (mCurrentWorkspaceFragment != null)
            {
                if (mCurrentWorkspaceFragment.onOptionsItemSelected(item))
                    return true;
                else
                    return super.onOptionsItemSelected(item);
            }
            else
                return super.onOptionsItemSelected(item);

        }

    }

    private void showWorkspaceManager()
    {
        final String dialogTag = "workspace management";

        WorkspaceManagementFragment frag = WorkspaceManagementFragment.create(new WorkspaceManagementFragment.ManagementChangeListener() {
            @Override
            public void onClose()
            {
                Fragment f = getFragmentManager().findFragmentByTag(dialogTag);
                FragmentTransaction trans = getFragmentManager().beginTransaction();
                trans.remove(f).commit();
            }

            @Override
            public void onClose(HashMap<WorkspaceManagementFragment.CHANGE_TYPE, WorkspaceManagementFragment.Change> changes)
            {
                Fragment f = getFragmentManager().findFragmentByTag(dialogTag);
                FragmentTransaction trans = getFragmentManager().beginTransaction();
                trans.remove(f).commit();

                String currentWorkspaceId = mCurrentWorkspace.getWorkspaceId();
                String newlySelectedWorkspaceId = null;

                HashMap<String, String> fakeToRealWorkidMap = new HashMap<String, String>();

                if (changes.containsKey(WorkspaceManagementFragment.CHANGE_TYPE.ADD_WORKSPACES))
                {
                    WorkspaceManagementFragment.Change change = changes.get(WorkspaceManagementFragment.CHANGE_TYPE.ADD_WORKSPACES);
                    WorkspaceManagementFragment.AddedWorkspacesChange addedWorkspace = (WorkspaceManagementFragment.AddedWorkspacesChange)change;
                    LinkedList<WorkspaceManagementFragment.WorkspaceData> result = addedWorkspace.getChange();

                    for (WorkspaceManagementFragment.WorkspaceData data:result)
                    {
                        try
                        {
                            Workspace newWorkspace = mApplication.createNewWorkspace();
                            String actualId = newWorkspace.getWorkspaceId();


                            String title = data.getNewTitle();
                            String fakeId = data.getWorkspaceId();

                            newWorkspace.setTitle(title);


                            fakeToRealWorkidMap.put(fakeId, actualId);
                        }
                        catch (Exception e)
                        {
                            logError("showWorkspaceManager", "Error applying workspace management updates: " + e.toString());
                        }

                    }
                }


                if (changes.containsKey(WorkspaceManagementFragment.CHANGE_TYPE.CHANGE_CURRENT_WORKSPACE))
                {
                    WorkspaceManagementFragment.Change change = changes.get(WorkspaceManagementFragment.CHANGE_TYPE.CHANGE_CURRENT_WORKSPACE);
                    WorkspaceManagementFragment.SelectWorkspaceChange selectChange = (WorkspaceManagementFragment.SelectWorkspaceChange)change;
                    String selectedWorkspaceId = selectChange.getChange();
                    if (fakeToRealWorkidMap.containsKey(selectedWorkspaceId))
                        newlySelectedWorkspaceId = fakeToRealWorkidMap.get(selectedWorkspaceId);
                    else
                        newlySelectedWorkspaceId = selectedWorkspaceId;
                }


                if (changes.containsKey(WorkspaceManagementFragment.CHANGE_TYPE.DELETE_WORKSPACES))
                {
                    WorkspaceManagementFragment.Change change = changes.get(WorkspaceManagementFragment.CHANGE_TYPE.DELETE_WORKSPACES);
                    WorkspaceManagementFragment.DeleteWorkspacesChange deleteChanges = (WorkspaceManagementFragment.DeleteWorkspacesChange)change;
                    LinkedList<String> result = deleteChanges.getChange();

                    for (String workspaceId:result)
                    {
                        if (newlySelectedWorkspaceId != null)
                        {
                            mApplication.deleteWorkspace(workspaceId, newlySelectedWorkspaceId);
                        }
                        else
                            mApplication.deleteWorkspace(workspaceId, currentWorkspaceId);
                    }
                }

                if (changes.containsKey(WorkspaceManagementFragment.CHANGE_TYPE.RENAME_WORKSPACES))
                {
                    WorkspaceManagementFragment.Change change = changes.get(WorkspaceManagementFragment.CHANGE_TYPE.RENAME_WORKSPACES);
                    WorkspaceManagementFragment.RenameWorkspacesChange renameChanges = (WorkspaceManagementFragment.RenameWorkspacesChange)change;
                    LinkedList<WorkspaceManagementFragment.WorkspaceData> renamed = renameChanges.getChange();

                    for (WorkspaceManagementFragment.WorkspaceData data:renamed)
                    {
                        Workspace work = mApplication.getWorkspace(data.getWorkspaceId());
                        work.setTitle(data.getNewTitle());
                    }
                }

                if (newlySelectedWorkspaceId != null)
                {
                    mApplication.setCurrentWorkspace(newlySelectedWorkspaceId);
                    Workspace next = mApplication.getCurrentWorkspace();
                    changeToWorkspace(next);
                    invalidateOptionsMenu();
                }

            }
        },
        mApplication);

        frag.show(getFragmentManager(), dialogTag);
    }


    @Override
    public void logError(String tag, String value)
    {
        Log.e(tag, value);
    }

    @Override
    public void logInfo(String tag, String value)
    {
        Log.i(tag, value);
    }

    @Override
    public void logDebug(String tag, String value)
    {
        Log.d(tag, value);
    }
}
