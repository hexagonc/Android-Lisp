package com.evolved.automata.android.lisp.guibuilder;

import android.content.Context;

import java.util.HashSet;

/**
 * Created by Evolved8 on 9/18/17.
 */

public class TutorialManager {

    public static class TutorialSpec
    {
        String _Description;
        String _Contents;
        String _title;

        public TutorialSpec(String title, String description, String contents)
        {
            _Description = description;
            _Contents = contents;
            _title = title;
        }

        public String getTitle()
        {
            return _title;
        }

        public String getContents()
        {
            return _Contents;
        }

        public String getDescription()
        {
            return _Description;
        }
    }

    public static class Tutorials
    {
        TutorialSpec[] _Tutorials = null;

        private Tutorials(TutorialSpec[] t)
        {
            _Tutorials = t;
        }

        public TutorialSpec[] getTutorials()
        {
            return _Tutorials;
        }
    }

    final String mTutorialBasePath = "";
    private static TutorialManager mManager;
    Context mContext;

    Tutorials mTutorials = null;

    private TutorialManager(Context context)
    {
        mContext = context;
    }

    public static TutorialManager make(Context con)
    {
        if (mManager == null)
            mManager = new TutorialManager(con);
        return mManager;
    }

    public static TutorialManager get()
    {
        return mManager;
    }

    public String getAssetTutorialData( String baseFileName)
    {

        HashSet<String> tutorialFiles = Tools.getAssetsInPath(mTutorialBasePath);

        if (tutorialFiles.contains(baseFileName))
        {
            return Tools.getAssertStringData(mTutorialBasePath + baseFileName);
        }
        else
            return null;
    }

    public Tutorials getTutorials()
    {
        if (mTutorials != null)
            return mTutorials;

        int numTutorials;
        String[] descriptions = mContext.getResources().getStringArray(R.array.tutorial_descriptions);
        String[] fileNames = mContext.getResources().getStringArray(R.array.tutorial_filename_names);
        String[] pageNames = mContext.getResources().getStringArray(R.array.tutorial_page_titles);

        numTutorials = descriptions.length;

        TutorialSpec[] tutorials = new TutorialSpec[numTutorials];

        for (int i = 0;i < numTutorials;i++)
        {
            tutorials[i] = new TutorialSpec(pageNames[i], descriptions[i], getAssetTutorialData(fileNames[i]));
        }
        return mTutorials = new Tutorials(tutorials);
    }

}
