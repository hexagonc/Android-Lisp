package com.evolved.automata.android.lisp.guibuilder;

import android.app.ActionBar;
import android.app.Activity;
import android.app.Fragment;
import android.app.FragmentManager;
import android.app.FragmentTransaction;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.EditText;

import com.evolved.automata.android.lisp.guibuilder.v2.*;
import com.evolved.automata.android.lisp.guibuilder.v2.DropboxManager;
import com.evolved.automata.android.lisp.guibuilder.v2.FileChooserDialog;

import java.util.HashMap;

import io.reactivex.Observer;
import io.reactivex.annotations.NonNull;
import io.reactivex.disposables.Disposable;

/**
 * Created by Evolved8 on 4/21/17.
 */

public class ALGBBaseActivity extends Activity {


    Runnable mOnTestComplete = null;

    Page mTestPage;
    ALGB mApplication;
    Workspace mCurrentWorkspace;

    WorkspaceFragment mCurrentWorkspaceFragment;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.v2_activity_main);

        FragmentManager manager = getFragmentManager();
        FragmentTransaction ft = manager.beginTransaction();
        mCurrentWorkspaceFragment = new WorkspaceFragment();


        ActionBar bar = getActionBar();
        bar.show();

        try
        {
            mApplication = new ALGB(getApplicationContext());
            mCurrentWorkspace = mApplication.getCurrentWorkspace();
            mCurrentWorkspaceFragment.setWorkspace(mCurrentWorkspace);
            ft.add(R.id.v2_top, mCurrentWorkspaceFragment);
            ft.commit();
        }
        catch (Exception e)
        {
            throw new RuntimeException(e);
        }

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
        super.onStop();
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.v2_main, menu);
        mCurrentWorkspaceFragment.onCreateOptionsMenu(menu, getMenuInflater());
        return true;
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu)
    {
        mCurrentWorkspaceFragment.onPrepareOptionsMenu(menu);
        return true;
    }


    @Override
    public boolean onOptionsItemSelected(MenuItem item)
    {
        if (item.getItemId() == R.id.v2_menu_test_finished)
        {
            if (mOnTestComplete != null)
                mOnTestComplete.run();
            return true;
        }
        else if (item.getItemId() == R.id.v2_menu_post_test_login)
        {
            Tools.postEvent(new DropboxManager.LoginEvent(DropboxManager.get().getTestClient()));
            return true;
        }
        else if (item.getItemId() == R.id.v2_menu_workspace_management)
        {
            showWorkspaceManager();
            return true;

        }
        else if (item.getItemId() == R.id.v2_menu_open_dropbox)
        {
            com.evolved.automata.android.lisp.guibuilder.v2.DropboxManager.Controller controller = DropboxManager.get().getController();
            controller.getFileDialog(this, "Select file to Sync With", "", new DropboxChooserItem.FileItemSelectHandler() {
                        @Override
                        public void onSelected(String filename)
                        {
                            Log.i(".oio..oio..oio.", "Selected file: " + filename);
                        }
                    },
                    new Observer<DropboxManager.DropboxResponse>() {
                        @Override
                        public void onSubscribe(@NonNull Disposable d)
                        {

                        }

                        @Override
                        public void onNext(@NonNull DropboxManager.DropboxResponse dropboxResponse)
                        {

                            FileChooserDialog dialog = ((DropboxManager.FileDialogResponse)dropboxResponse).getDialog();
                            dialog.show();
                        }

                        @Override
                        public void onError(@NonNull Throwable e)
                        {
                            e.printStackTrace();
                        }

                        @Override
                        public void onComplete()
                        {

                        }
                    });
            return true;
        }
        else
        {

            if (mCurrentWorkspaceFragment.onOptionsItemSelected(item))
                return true;
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



            }
        },
        mApplication);

        frag.show(getFragmentManager(), dialogTag);
    }

    public void showDropboxSyncFileDialog()
    {

    }

}
