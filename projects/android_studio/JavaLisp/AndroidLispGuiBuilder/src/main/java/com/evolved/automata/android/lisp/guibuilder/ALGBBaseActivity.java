package com.evolved.automata.android.lisp.guibuilder;

import android.app.AlertDialog;


import android.content.Context;
import android.content.DialogInterface;
import android.os.Bundle;

import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentTransaction;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.inputmethod.InputMethodManager;
import android.widget.Toast;

import com.dropbox.core.DbxRequestConfig;
import com.dropbox.core.android.Auth;
import com.dropbox.core.android.AuthActivity;
import com.dropbox.core.v2.DbxClientV2;
import com.evolved.automata.android.AndroidTools;
import com.evolved.automata.android.lisp.guibuilder.events.FindTextEvent;
import com.evolved.automata.android.lisp.guibuilder.events.GoToLineNumber;
import com.evolved.automata.android.lisp.guibuilder.model.ALGB;
import com.evolved.automata.android.lisp.guibuilder.model.Page;
import com.evolved.automata.android.lisp.guibuilder.model.Workspace;
import com.evolved.automata.android.lisp.guibuilder.ui.WorkspaceFragment;
import com.evolved.automata.android.lisp.guibuilder.ui.WorkspaceManagementFragment;
import com.evolved.automata.editor.TextSearchResult;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;

import io.reactivex.Observable;
import io.reactivex.ObservableEmitter;
import io.reactivex.ObservableOnSubscribe;
import io.reactivex.Observer;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.annotations.NonNull;
import io.reactivex.annotations.Nullable;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Cancellable;
import io.reactivex.schedulers.Schedulers;

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
    SettingsFragment mSettingsFragment;
    HashMap<String, WorkspaceFragment> mWorkspaceMap;
    InputMethodManager mIMM = null;

    String mActivityToolbarTitle = "ALGB";

    boolean mShowingSettingsP = false;

    Fragment mCurrentMainFragment = null;

    @Override
    protected void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.v2_activity_main);
        mIMM = (InputMethodManager)ALGBBaseActivity.this
                .getSystemService(Context.INPUT_METHOD_SERVICE);



        DebugHelper.make(this);
        mWorkspaceMap = new HashMap<String, WorkspaceFragment>();

        mSettingsFragment = new SettingsFragment();
        Toolbar actionBarToolbar = (Toolbar)findViewById(R.id.tb_toolbar_actionbar);
        setSupportActionBar(actionBarToolbar);

        actionBarToolbar.inflateMenu(R.menu.v2_main);

        try
        {

            mApplication = ((ALGBApplication)getApplication()).getALGB();
            changeToWorkspace(mApplication.getCurrentWorkspace());
            actionBarToolbar.setNavigationOnClickListener(new View.OnClickListener()
            {

                @Override
                public void onClick(View view)
                {
                    onBackPressed();
                }
            });
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
        mCurrentWorkspaceFragment = frag;
        //invalidateOptionsMenu();
        switchMainFragment(mCurrentWorkspaceFragment, false);

    }

    private void switchMainFragment(Fragment frag, boolean updateBackstack)
    {
        invalidateOptionsMenu();
        mCurrentMainFragment = frag;
        FragmentManager manager = getSupportFragmentManager();
        FragmentTransaction ft = manager.beginTransaction();
        ft.replace(R.id.v2_top, mCurrentMainFragment);
        ft.setTransition(FragmentTransaction.TRANSIT_FRAGMENT_FADE);
        if (updateBackstack)
            ft.addToBackStack("Back");
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

    public void setDefaultTitle()
    {
        setTitle(mActivityToolbarTitle);
    }

    @Override
    protected void onPause()
    {
        super.onPause();
    }

    @Override
    protected void onStop()
    {

        super.onStop();
    }

    @Override
    public void onBackPressed()
    {

        if (mShowingSettingsP)
        {
            mShowingSettingsP = false;

            changeToWorkspace(mApplication.getCurrentWorkspace());
            return;
        }
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

    void showBackOnToolbar()
    {
        Toolbar tb = (Toolbar)findViewById(R.id.tb_toolbar_actionbar);
        tb.setNavigationIcon(R.drawable.ic_chevron_left_white_24dp);

    }

    public void hideBackOnToolbar()
    {
        Toolbar tb = (Toolbar)findViewById(R.id.tb_toolbar_actionbar);
        tb.setNavigationIcon(null);

    }

    private void showSettingsShow()
    {
        mShowingSettingsP = true;
        switchMainFragment(new SettingsFragment(), false);

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

        if (!mApplication.specialDebuggingEnabled())
        {
            menu.removeItem(R.id.debug_options);
        }
        return true;
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu)
    {

        mCurrentMainFragment.onPrepareOptionsMenu(menu);
        /*
        if (mShowingSettingsP && mSettingsFragment != null)
        {
            mSettingsFragment.onPrepareOptionsMenu(menu);
            return true;
        }
        else if (mCurrentWorkspaceFragment != null)
        {

            mCurrentWorkspaceFragment.onPrepareOptionsMenu(menu);
        }
        */
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

            if (!mApplication.specialDebuggingEnabled() && (!Tools.getSampleWorkspaceEnabled() || mApplication.sampleWorkspaceExistsP()))
            {
                showWorkspaceManager();
            }
            else
            {
                ObservableOnSubscribe<Void> observable = new ObservableOnSubscribe<Void>() {
                    @Override
                    public void subscribe(@NonNull ObservableEmitter<Void> e) throws Exception
                    {
                        mApplication.createSampleWorkspaceExists(getResources().getBoolean(R.bool.enable_special_debug));
                        e.onComplete();
                    }
                };

                Observer<Void> showDialog = new Observer<Void>() {


                    @Override
                    public void onSubscribe(@NonNull Disposable d)
                    {

                    }

                    @Override
                    public void onNext(@NonNull Void value)
                    {

                    }

                    @Override
                    public void onError(@NonNull Throwable error)
                    {

                    }

                    @Override
                    public void onComplete()
                    {
                        showWorkspaceManager();
                    }
                };

                Observable.create(observable).subscribeOn(Schedulers.io()).observeOn(AndroidSchedulers.mainThread()).subscribe(showDialog);
            }

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
        else if (item.getItemId() == R.id.show_settings)
        {
            showSettingsShow();
            return true;
        }
        else if (item.getItemId() == R.id.debug_options)
        {
            showDebugOptions();
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

            }

            @Override
            public void onClose(HashMap<WorkspaceManagementFragment.CHANGE_TYPE, WorkspaceManagementFragment.Change> changes)
            {

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
                        String original = data.getNewTitle();
                        setWorkspaceActionbarTitle(original);
                        Workspace work = mApplication.getWorkspace(data.getWorkspaceId());
                        work.setTitle(original);
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

    public void setWorkspaceActionbarTitle(String original)
    {
        Toolbar actionBarToolbar = (Toolbar)findViewById(R.id.tb_toolbar_actionbar);

        actionBarToolbar.setSubtitle(original);

    }

    private void showDebugOptions()
    {
        DebugHelper.get().showDebugOptions();
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
