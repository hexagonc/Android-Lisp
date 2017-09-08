package com.evolved.automata.android.lisp.guibuilder;

import android.app.ActionBar;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;



import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.graphics.Rect;
import android.os.Bundle;

import android.os.IBinder;
import android.os.ResultReceiver;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentTransaction;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewTreeObserver;
import android.view.inputmethod.InputMethodManager;
import android.widget.Toast;

import com.dropbox.core.DbxRequestConfig;
import com.dropbox.core.DbxWebAuth;
import com.dropbox.core.android.Auth;
import com.dropbox.core.android.AuthActivity;
import com.dropbox.core.v2.DbxClientV2;
import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.events.EventManager;

import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;

import io.reactivex.Observer;
import io.reactivex.annotations.NonNull;
import io.reactivex.disposables.Disposable;

/**
 * Created by Evolved8 on 4/21/17.
 */

public class ALGBBaseActivity extends AppCompatActivity implements LogHandler {

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


        Toolbar actionBarToolbar = (Toolbar)findViewById(R.id.tb_toolbar_actionbar);
        setSupportActionBar(actionBarToolbar);

        actionBarToolbar.inflateMenu(R.menu.v2_main);

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

        FragmentManager manager = getSupportFragmentManager();
        FragmentTransaction ft = manager.beginTransaction();
        ft.replace(R.id.v2_top, mCurrentWorkspaceFragment);
        ft.setTransition(FragmentTransaction.TRANSIT_FRAGMENT_FADE);
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

        String token = AndroidTools.getStringPreferenceSetting(DROPBOX_ACCESS_TOKEN_KEY, null);

        if (token == null)
        {
            token = Auth.getOAuth2Token();
            AndroidTools.setStringPreferenceSetting(DROPBOX_ACCESS_TOKEN_KEY, token);
            String uId = Auth.getUid();
        }
        String clientName = getResources().getString(R.string.dropbox_app_client_name);
        if (token != null)
        {
            DbxRequestConfig.Builder builder = DbxRequestConfig.newBuilder(clientName);
            DbxRequestConfig config = builder.build();

            AndroidTools.setStringPreferenceSetting(DROPBOX_ACCESS_TOKEN_KEY, token);
            DbxClientV2 client = new DbxClientV2(config, token);
            Tools.postEvent(new DropboxManager.LoginEvent(client));
        }
        else
        {
            EventLog.get().logSystemInfo("Not logged into Dropbox");
        }

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
    public void onBackPressed()
    {

        AlertDialog.Builder aBuilder = new AlertDialog.Builder(this);
        aBuilder.setTitle("Exit Android Lisp GuiBuilder?");
        aBuilder.setMessage("Any unsaved changes will be lost.").setPositiveButton("Ok", new DialogInterface.OnClickListener()
        {

            @Override
            public void onClick(DialogInterface dialogInterface, int i)
            {
                dialogInterface.dismiss();
                superOnBack();
            }
        }).setNegativeButton("Cancel", new DialogInterface.OnClickListener()
        {

            @Override
            public void onClick(DialogInterface dialogInterface, int i)
            {
                dialogInterface.dismiss();
            }
        });
        aBuilder.create().show();;

    }

    private void superOnBack()
    {
        super.onBackPressed();
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.v2_main, menu);

        if (!getResources().getBoolean(R.bool.enable_test_dropbox_access_key))
        {
            menu.removeItem(R.id.v2_menu_post_test_login);
        }
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

    static final int DROBOX_AUTHENTICATION = 312456;

    static final String DROPBOX_ACCESS_TOKEN_KEY = "DROPBOX-ACCESS-TOKEN";
    private void startDropboxAuthentication()
    {

        String accessToken = Auth.getOAuth2Token();
        if (accessToken == null)
        {
            boolean manifestConfiguredP = false;
            String errorMessage = "Another app is conflicting";
            try
            {
                manifestConfiguredP = AuthActivity.checkAppBeforeAuth(this, getString(R.string.enc_dropbox_app_key), false);
            }
            catch (Exception e)
            {
                errorMessage = e.toString();
            }

            if (manifestConfiguredP)
                Auth.startOAuth2Authentication(this, getResources().getString(R.string.enc_dropbox_app_key));
            else
            {
                EventLog.get().logSystemError(new Exception(errorMessage), "Cannot start OAuth authentication");
            }
        }
        else
        {
            Toast.makeText(this, "Retrieved Dropbox", Toast.LENGTH_LONG).show();
        }

    }


    @Override
    public boolean onOptionsItemSelected(MenuItem item)
    {


        if (item.getItemId() == R.id.v2_menu_workspace_management)
        {
            showWorkspaceManager();
            return true;

        }else if (item.getItemId() == R.id.v2_menu_show_event_log)
        {
            EventLog.get().showLog();
            return true;
        }
        else if (item.getItemId() == R.id.v2_start_dropbox_authentication)
        {
            startDropboxAuthentication();
            return true;
        }
        else if (item.getItemId() == R.id.v2_menu_post_test_login)
        {
            String accessToken = getResources().getString(R.string.v2_test_dropbox_token);
            String clientName = getResources().getString(R.string.dropbox_app_client_name);
            if (accessToken != null && accessToken.length() > 0 && clientName != null)
            {
                DbxRequestConfig.Builder builder = DbxRequestConfig.newBuilder(clientName);
                DbxRequestConfig config = builder.build();

                AndroidTools.setStringPreferenceSetting(DROPBOX_ACCESS_TOKEN_KEY, accessToken);
                DbxClientV2 client = new DbxClientV2(config, accessToken);
                Tools.postEvent(new DropboxManager.LoginEvent(client));

            }
            else
                EventLog.get().logSystemError("Dropbox not configured for test login");
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
                //Fragment f = getSupportFragmentManager().findFragmentByTag(dialogTag);
                //FragmentTransaction trans = getSupportFragmentManager().beginTransaction();
                //trans.remove(frag).commit();
            }

            @Override
            public void onClose(HashMap<WorkspaceManagementFragment.CHANGE_TYPE, WorkspaceManagementFragment.Change> changes)
            {
                //Fragment f = getSupportFragmentManager().findFragmentByTag(dialogTag);
                //FragmentTransaction trans = getSupportFragmentManager().beginTransaction();
                //trans.remove(f).commit();

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
