package com.evolved.automata.android.lisp.guibuilder;

import android.app.ActionBar;
import android.app.Activity;
import android.app.FragmentManager;
import android.app.FragmentTransaction;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.EditText;

/**
 * Created by Evolved8 on 4/21/17.
 */

public class ALGBBaseActivity extends Activity {


    Runnable mOnTestComplete = null;

    Page mTestPage;
    ALGB mApplication;
    Workspace mCurrentWorkspace;


    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.v2_activity_main);

        FragmentManager manager = getFragmentManager();
        FragmentTransaction ft = manager.beginTransaction();
        CodePageFragment cef = new CodePageFragment();
        ft.add(R.id.v2_top, cef);
        ft.commit();

        ActionBar bar = getActionBar();
        bar.show();

        try
        {
            mApplication = new ALGB(getApplicationContext());
            mCurrentWorkspace = mApplication.getCurrentWorkspace();
            mTestPage = mCurrentWorkspace.getCurrentPage();
            if (mTestPage instanceof CodePage)
            {
                cef.setCodePage((CodePage) mTestPage);
            }
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
        else
            return super.onOptionsItemSelected(item);
    }
}
