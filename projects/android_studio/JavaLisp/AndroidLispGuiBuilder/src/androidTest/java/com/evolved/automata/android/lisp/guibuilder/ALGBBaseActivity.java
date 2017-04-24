package com.evolved.automata.android.lisp.guibuilder;

import android.app.Activity;
import android.os.Bundle;
import android.support.annotation.Nullable;

/**
 * Created by Evolved8 on 4/21/17.
 */

public class ALGBBaseActivity extends Activity {


    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

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
}
