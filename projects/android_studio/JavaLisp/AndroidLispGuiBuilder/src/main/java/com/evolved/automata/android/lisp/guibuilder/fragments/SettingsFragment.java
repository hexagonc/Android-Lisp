package com.evolved.automata.android.lisp.guibuilder.fragments;


import com.evolved.automata.android.guibuilder.R;

import android.os.Bundle;
import android.preference.PreferenceFragment;

public class SettingsFragment extends PreferenceFragment 
{

	@Override
	public void onCreate(Bundle savedInstanceState) {
		
		super.onCreate(savedInstanceState);
		addPreferencesFromResource(R.xml.settings_top);
	}

}
