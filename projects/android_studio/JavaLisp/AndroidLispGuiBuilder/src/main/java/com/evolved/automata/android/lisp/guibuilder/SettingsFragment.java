package com.evolved.automata.android.lisp.guibuilder;

import android.os.Bundle;

import android.support.annotation.Nullable;
import android.support.v7.preference.PreferenceFragmentCompat;
import android.support.v7.widget.Toolbar;
import android.view.Menu;
import android.view.MenuInflater;

/**
 * Created by Evolved8 on 9/14/17.
 */

public class SettingsFragment extends PreferenceFragmentCompat {

    @Override
    public void onStart()
    {
        super.onStart();
        ALGBBaseActivity baseActivity = (ALGBBaseActivity)getActivity();
        baseActivity.setTitle("Settings");
        baseActivity.setWorkspaceActionbarTitle("");
        baseActivity.showBackOnToolbar();
    }

    public SettingsFragment()
    {

    }

    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);

    }

    @Override
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater)
    {
        super.onCreateOptionsMenu(menu, inflater);
    }

    @Override
    public void onPrepareOptionsMenu(Menu menu)
    {
        MenuHelper.updateMenuItemDisplay(menu, this);
    }


    @Override
    public void onCreatePreferences(Bundle savedInstanceState, String rootKey)
    {
        addPreferencesFromResource(R.xml.preferences);
    }


}
