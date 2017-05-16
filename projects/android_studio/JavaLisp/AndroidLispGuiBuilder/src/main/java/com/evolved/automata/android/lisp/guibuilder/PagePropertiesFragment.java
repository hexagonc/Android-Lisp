package com.evolved.automata.android.lisp.guibuilder;

import android.app.Dialog;
import android.app.DialogFragment;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;

import java.util.HashMap;

import io.reactivex.Observer;
import io.reactivex.annotations.NonNull;
import io.reactivex.disposables.Disposable;

/**
 * Created by Evolved8 on 5/8/17.
 */

public class PagePropertiesFragment extends DialogFragment {


    public enum CHANGE_TYPE
    {
        TITLE, DROPBOX_SYNC_FILE, CHANGE_TEXT
    }

    public interface Change
    {
        CHANGE_TYPE getType();
        HashMap<CHANGE_TYPE, Change> fill(HashMap<CHANGE_TYPE, Change> map);
    }

    public static class ChangeTitle implements Change
    {
        String title = "";
        public ChangeTitle(String t)
        {
            title = t;
        }

        public CHANGE_TYPE getType()
        {
            return CHANGE_TYPE.TITLE;
        }

        public String newTitle()
        {
            return title;
        }

        public HashMap<CHANGE_TYPE, Change> fill(HashMap<CHANGE_TYPE, Change> map)
        {
            map.put(getType(), this);
            return map;
        }

    }

    public static class ChangeText implements Change
    {
        String text = "";
        public ChangeText(String t)
        {
            text = t;
        }

        public CHANGE_TYPE getType()
        {
            return CHANGE_TYPE.CHANGE_TEXT;
        }

        public String newText()
        {
            return text;
        }

        public HashMap<CHANGE_TYPE, Change> fill(HashMap<CHANGE_TYPE, Change> map)
        {
            map.put(getType(), this);
            return map;
        }

    }

    public static class ChangeDropboxSyncFile implements Change
    {
        String fileName;

        public ChangeDropboxSyncFile(String file)
        {
            fileName = file;
        }

        String getSyncFile()
        {
            return fileName;
        }

        public CHANGE_TYPE getType()
        {
            return CHANGE_TYPE.DROPBOX_SYNC_FILE;
        }

        public HashMap<CHANGE_TYPE, Change> fill(HashMap<CHANGE_TYPE, Change> map)
        {
            map.put(getType(), this);
            return map;
        }
    }


    public interface OnCompleteListener
    {
        void close();
        void close(HashMap<CHANGE_TYPE, Change> changes);
    }


    HashMap<CHANGE_TYPE, Change> mChanges = new HashMap<CHANGE_TYPE, Change>();

    Button mSetNewPathButton;
    Button mAcceptDialogButton;
    Button mCancelDialogButton;
    Button mSyncButton;
    EditText mPageTitleText;
    Button mDropboxSyncPath;

    CodePage mPage;

    int mStyle = DialogFragment.STYLE_NORMAL;

    int mTheme = 0;

    OnCompleteListener mListener;

    public static PagePropertiesFragment create(OnCompleteListener dialogFinishedListener, int style, int theme, CodePage page)
    {
        PagePropertiesFragment frag = new PagePropertiesFragment();

        frag.setPage(page);
        frag.setStyle(style);
        frag.setTheme(theme);
        frag.setCompleteListener(dialogFinishedListener);
        return frag;
    }



    public static PagePropertiesFragment create(OnCompleteListener dialogFinishedListener, CodePage page)
    {

        PagePropertiesFragment frag = new PagePropertiesFragment();
        frag.setPage(page);
        frag.setStyle(DialogFragment.STYLE_NORMAL);
        frag.setTheme(0);

        frag.setCompleteListener(dialogFinishedListener);
        return frag;
    }


    private void setCompleteListener(OnCompleteListener listener)
    {
        mListener = listener;
    }

    private void setPage(CodePage page)
    {
        mPage = page;
    }

    private void setStyle(int style)
    {
        mStyle = style;
    }

    private void setTheme(int theme)
    {
        mTheme = theme;
    }

    @Override
    public Dialog getDialog()
    {
        return super.getDialog();
    }

    @Override
    public void onCreate(Bundle savedInstanceState)
    {

        super.onCreate(savedInstanceState);

        setStyle(mStyle, mTheme);
    }

    boolean canSync()
    {
        return mDropboxSyncPath.getText().toString().length()>0;
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState)
    {
        ViewGroup top =  (ViewGroup)inflater.inflate(R.layout.v2_page_properties, container, false);
        mCancelDialogButton = (Button)top.findViewById(R.id.v2_but_dismiss_page_properties);
        mCancelDialogButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v)
            {
                finishedDialog();
            }
        });

        TextView titleView = (TextView)top.findViewById(R.id.v2_page_properties_title);

        if (mStyle !=  DialogFragment.STYLE_NO_TITLE)
        {
            titleView.setVisibility(View.GONE);
            getDialog().setTitle(R.string.v2_page_properties_title);
        }

        mPageTitleText = (EditText)top.findViewById(R.id.v2_edit_page_title);
        mPageTitleText.setText(mPage.getTitle());


        mAcceptDialogButton = (Button)top.findViewById(R.id.v2_but_accept_page_properties);
        mAcceptDialogButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v)
            {
                accept();
            }
        });

        mDropboxSyncPath = (Button)top.findViewById(R.id.v2_txt_dropbox_sync_filename);
        if (mPage.getDropboxPath() != null)
            mDropboxSyncPath.setText(mPage.getDropboxPath());



        mDropboxSyncPath.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v)
            {
                showDropboxDialog();
            }
        });


        mSetNewPathButton = (Button)top.findViewById(R.id.v2_but_set_dropbox_linked_file);
        mSetNewPathButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v)
            {

            }
        });


        mSyncButton = (Button)top.findViewById(R.id.v2_but_sync_now);
        mSyncButton.setEnabled(canSync());
        mSyncButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v)
            {
                String path = mDropboxSyncPath.getText().toString();
                DropboxManager.get().getController().downloadFile(path, new Observer<DropboxManager.DropboxResponse>() {
                    @Override
                    public void onSubscribe(@NonNull Disposable d)
                    {

                    }

                    @Override
                    public void onNext(@NonNull DropboxManager.DropboxResponse dropboxResponse)
                    {
                        DropboxManager.DownloadTextResponse response = (DropboxManager.DownloadTextResponse)dropboxResponse;
                        (new ChangeText(response.getContents())).fill(mChanges);
                    }

                    @Override
                    public void onError(@NonNull Throwable e)
                    {

                    }

                    @Override
                    public void onComplete()
                    {

                    }
                });

            }
        });
        return top;
    }

    void finishedDialog()
    {
        mListener.close();
    }

    void accept()
    {

        if (!mPageTitleText.getText().toString().equals(mPage.getTitle()))
        {
            new ChangeTitle(mPageTitleText.getText().toString()).fill(mChanges);
        }
        if (!mDropboxSyncPath.getText().toString().equals(mPage.getDropboxPath()))
        {
            new ChangeDropboxSyncFile(mDropboxSyncPath.getText().toString()).fill(mChanges);
        }

        mListener.close(mChanges);
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
    public Dialog onCreateDialog(Bundle savedInstanceState)
    {
        return super.onCreateDialog(savedInstanceState);
    }

    @Override
    public void onDismiss(DialogInterface dialog)
    {
        finishedDialog();
    }

    private void showDropboxDialog()
    {
        String priorSync = mPage.getDropboxPath();
        String basePath = "";
        if (priorSync != null)
        {
            String parent = Tools.getParentFolder(priorSync);
            if (parent.equals("/"))
                parent = "";
            basePath = parent;
        }
        DropboxManager.Controller controller = DropboxManager.get().getController();
        controller.getFileDialog(getActivity(), "Select file to Sync With", basePath, new DropboxChooserItem.FileItemSelectHandler() {
                    @Override
                    public void onSelected(String filename)
                    {
                        Log.i(".oio..oio..oio.", "Selected file: " + filename);

                        mDropboxSyncPath.setText(filename);
                        mSyncButton.setEnabled(canSync());
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
    }
}