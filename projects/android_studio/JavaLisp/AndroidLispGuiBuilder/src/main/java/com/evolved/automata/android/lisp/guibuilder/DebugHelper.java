package com.evolved.automata.android.lisp.guibuilder;

import android.app.AlertDialog;
import android.content.DialogInterface;

import com.evolved.automata.android.lisp.guibuilder.events.FindTextEvent;
import com.evolved.automata.android.lisp.guibuilder.events.GoToLineNumber;
import com.evolved.automata.editor.TextSearchResult;

import java.util.Iterator;

/**
 * Created by Evolved8 on 10/21/17.
 */

public class DebugHelper {

    ALGBBaseActivity mActivity;

    // Create and configure debug events
    String searchText = "setq";
    FindTextEvent searchEvent = new FindTextEvent(searchText);
    GoToLineNumber gotoLine = new GoToLineNumber(10);


    private static DebugHelper helper;

    private DebugHelper(ALGBBaseActivity activity)
    {
        searchEvent = new FindTextEvent(searchText);
        gotoLine = new GoToLineNumber(10);
        mActivity = activity;
    }

    public static DebugHelper make(ALGBBaseActivity activity)
    {
        if (helper == null)
            return helper = new DebugHelper(activity);
        else
            return helper;
    }

    public static DebugHelper get()
    {
        return helper;
    }

    public void showDebugOptions()
    {



        // define event numeric ids
        final int SEARCH_TEXT = 0;
        final int GOTO_LINE = SEARCH_TEXT + 1;

        int count = GOTO_LINE + 1;

        // Create event and list label arrays
        String[] desc = new String[count];
        final Object[] debugEvents = new Object[count];


        // Set events and labels
        debugEvents[SEARCH_TEXT] = searchEvent;
        desc[SEARCH_TEXT] = "search for '" + searchText+ "'";

        debugEvents[GOTO_LINE] = gotoLine;
        desc[GOTO_LINE] = "go to line";


        AlertDialog.Builder builder = new AlertDialog.Builder(mActivity);

        builder.setTitle("Debug Menu");

        builder.setItems(desc, new DialogInterface.OnClickListener()
        {

            @Override
            public void onClick(DialogInterface dialogInterface, int i)
            {

                switch (i)
                {
                    case SEARCH_TEXT:
                    {

                    }
                    break;
                    case GOTO_LINE:
                    {
                        Iterator<TextSearchResult> results = searchEvent.getResults();
                        if (results != null && results.hasNext())
                        {
                            TextSearchResult result = results.next();

                            gotoLine.setLineNumber(result.getLineNumber());
                        }

                    }
                    break;
                }
                Tools.postEvent(debugEvents[i]);
            }
        });
        builder.create().show();
    }
}
