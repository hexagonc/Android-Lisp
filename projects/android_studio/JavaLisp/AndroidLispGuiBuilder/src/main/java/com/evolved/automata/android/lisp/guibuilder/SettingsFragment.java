package com.evolved.automata.android.lisp.guibuilder;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;

import android.support.annotation.Nullable;
import android.support.v7.preference.DialogPreference;
import android.support.v7.preference.Preference;
import android.support.v7.preference.PreferenceFragmentCompat;
import android.support.v7.widget.Toolbar;
import android.view.Menu;
import android.view.MenuInflater;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

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

        final Preference pref = findPreference(getString(R.string.pref_int_key_undo_history_length));
        pref.setSummary(getString(R.string.pref_undo_history_summary, Tools.getEditorUndoHistoryLength(getContext())));
        configureAsNumericPreference(pref, R.string.pref_dialog_undo_history_title, R.string.pref_dialog_undo_history_prompt, R.string.pref_int_key_undo_history_length, R.string.pref_dialog_undo_history_error_prompt, 1, 10000, "", "" + Tools.getEditorUndoHistoryLength(getContext()),
                new Runnable()
                {
                    public void run()
                    {
                        pref.setSummary(getString(R.string.pref_undo_history_summary, Tools.getEditorUndoHistoryLength(getContext())));
                    }
                });
    }

    private void configureAsNumericPreference(Preference pref, int titleRes, int promptRes, final int prefKeyString, final int errorStringResourceId, final int minValue, final int maxValue, String hintText, String initialText, final Runnable onComplete)
    {
        final AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
        LinearLayout top = (LinearLayout)getLayoutInflater().inflate(R.layout.vw_numeric_pref, null);
        builder.setTitle(titleRes);

        TextView prompt = (TextView)top.findViewById(R.id.txt_numeric_pref_label);
        prompt.setText(getString(promptRes));

        final EditText numEdit = (EditText)top.findViewById(R.id.edit_numeric_pref_value);

        numEdit.setText(initialText);
        numEdit.setHint(hintText);
        builder.setView(top);

        builder.setPositiveButton(getString(R.string.standard_accept_label), new DialogInterface.OnClickListener()
        {

            @Override
            public void onClick(DialogInterface dialogInterface, int i)
            {
                String text = numEdit.getText().toString();
                Integer value = validateInteger(text,10, minValue, maxValue);
                if (value != null)
                {
                    Tools.setIntegerPreference(prefKeyString, value.intValue());
                    dialogInterface.dismiss();
                    Toast.makeText(getActivity(), "Updated preference value", Toast.LENGTH_SHORT).show();
                    if (onComplete != null)
                        onComplete.run();
                }
                else
                    Toast.makeText(getActivity(), getString(errorStringResourceId), Toast.LENGTH_SHORT).show();
            }
        });

        builder.setNegativeButton(getString(R.string.standard_cancel_label), new DialogInterface.OnClickListener()
        {

            @Override
            public void onClick(DialogInterface dialogInterface, int i)
            {

                dialogInterface.dismiss();
            }
        });

        final AlertDialog dialog = builder.create();
        pref.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
            @Override
            public boolean onPreferenceClick(Preference preference)
            {
                dialog.show();
                return true;
            }
        });
    }

    Integer validateInteger(String value, int maxLength, int minValue, int maxValue)
    {
        value.trim();
        if (value.length()>maxLength)
            return null;
        Pattern p = Pattern.compile("^\\d+$");
        Matcher m = p.matcher(value);
        if (m.matches())
        {
            int intValue = Integer.parseInt(value);
            if (intValue < minValue || intValue > maxValue)
                return null;
            else
                return Integer.valueOf(intValue);
        }
        else
            return null;
    }


}
