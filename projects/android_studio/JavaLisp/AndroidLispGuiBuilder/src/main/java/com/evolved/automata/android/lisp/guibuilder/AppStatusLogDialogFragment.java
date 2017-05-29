package com.evolved.automata.android.lisp.guibuilder;

import android.app.Dialog;
import android.app.DialogFragment;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import com.evolved.automata.android.widgets.ShadowButton;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.UUID;

/**
 * Created by Evolved8 on 5/29/17.
 */

public class AppStatusLogDialogFragment extends DialogFragment {



    ShadowButton mAcceptButton;

    EditText mSummaryView;
    ListView mStatusLogListview;



    ArrayList<EventLog.LogEntry> mStatusLogs = new ArrayList<EventLog.LogEntry>();

    ArrayAdapter<EventLog.LogEntry> mLogEntryAdapter;

    int mStyle = DialogFragment.STYLE_NORMAL;
    int mTheme = 0;

    int mSelectedWorkspacePos = -1;

    String mInitialWorkspaceId;

    public static AppStatusLogDialogFragment create()
    {
        AppStatusLogDialogFragment frag = new AppStatusLogDialogFragment();

        return frag;
    }



    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
    }

    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState)
    {

        return super.onCreateDialog(savedInstanceState);
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState)
    {
        ViewGroup top = (ViewGroup)inflater.inflate(R.layout.app_status_log, container, false);

        mStatusLogListview = (ListView)top.findViewById(R.id.lst_app_status);
        mAcceptButton = (ShadowButton)top.findViewById(R.id.sdw_but_exit_app_status_log);
        mSummaryView = (EditText) top.findViewById(R.id.edit_log_details);


        getDialog().setTitle(R.string.app_status_log_title);
        configureUI();

        return top;
    }

    void configureUI()
    {


        mStatusLogs.addAll(EventLog.get().getEntries());
        mLogEntryAdapter = new ArrayAdapter<EventLog.LogEntry>(getActivity(), 0, mStatusLogs)
        {

            @NonNull
            @Override
            public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent)
            {
                EventLog.LogEntry entry = mStatusLogs.get(position);
                LinearLayout out;
                if (convertView == null)
                {
                    LayoutInflater inflater = getActivity().getLayoutInflater();
                    out = (LinearLayout)inflater.inflate(R.layout.app_status_log_list_item, parent, false);
                }
                else
                {
                    out = (LinearLayout)convertView;

                }
                TextView dateLabel = (TextView)out.findViewById(R.id.txt_log_time);
                dateLabel.setText(entry.getDateTime());
                TextView sourceLabel = (TextView)out.findViewById(R.id.txt_source_label);
                sourceLabel.setText(entry.getType().name());
                TextView summaryLabel = (TextView)out.findViewById(R.id.txt_log_summary);
                summaryLabel.setText(entry.getSummary());
                int color = 0;

                switch (entry.getType())
                {
                    case ERROR:
                        color = getResources().getColor(R.color.new_error_status_alert);
                        break;
                    case INFO:
                        color = getResources().getColor(R.color.new_info_status_alert);
                        break;
                    case RESULT:
                        color = getResources().getColor(R.color.new_background_result_status_alert);
                        break;

                }
                sourceLabel.setTextColor(color);

                return out;
            }
        };

        mStatusLogListview.setAdapter(mLogEntryAdapter);
        mStatusLogListview.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id)
            {
                EventLog.LogEntry entry = mStatusLogs.get(position);
                mSummaryView.setText(entry.getDetailed());


            }
        });



        mAcceptButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v)
            {
                dismiss();
            }
        });


    }




    @Override
    public void onDismiss(DialogInterface dialog)
    {
        super.onDismiss(dialog);
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
    }

    @Override
    public void onPause()
    {
        super.onPause();
    }

    @Override
    public void onStop()
    {
        super.onStop();
    }
}
