package com.evolved.automata.android.lisp.guibuilder;

import android.app.Dialog;
import android.graphics.Color;
import android.view.View;
import android.widget.TextView;

import com.dropbox.core.v2.files.Metadata;

import java.util.ArrayList;


import io.reactivex.Observer;
import io.reactivex.annotations.NonNull;
import io.reactivex.disposables.Disposable;

/**
 * Created by Evolved8 on 5/11/17.
 */

//TODO: Refactor the design for DropboxFileData to better handle case at root directory
public class DropboxChooserItem implements FileChooserItem {


    public interface FileItemSelectHandler
    {
        void onSelected(String filename);
    }



    DropboxFileData mData;

    FileItemSelectHandler mSelectHandler;

    DropboxChooserItem mParent;



    public DropboxChooserItem(DropboxFileData data, FileItemSelectHandler handler)
    {
        mData = data;
        mSelectHandler = handler;

        try
        {
            if (mData.isRoot())
            {
                mParent = null;
            }
            else
            {
                String parentPath = mData.getListableParentName();
                if (parentPath.equals(DropboxFileData.ROOT_LISTABLE_NAME))
                {
                    mParent = new DropboxChooserItem(DropboxFileData.createRootFolderData() , handler);
                }
                else
                {
                    Metadata meta = DropboxManager.get().getController().getMetaData(parentPath);
                    mParent = new DropboxChooserItem(DropboxFileData.from(meta) , handler);
                }
            }

        }
        catch (Exception e)
        {
            throw new RuntimeException(e);
        }
    }

    public DropboxChooserItem(FileItemSelectHandler handler)
    {
        this(DropboxFileData.createRootFolderData(), handler);

    }



    @Override
    public String getFileName()
    {
        return mData.getDisplay();
    }

    @Override
    public String getFileNameShort()
    {
        return mData.getNameShort();
    }

    @Override
    public void onClickListener(Dialog parent)
    {
        if (mSelectHandler != null)
            mSelectHandler.onSelected(mData.getDisplay());
        parent.dismiss();
    }

    @Override
    public void getChildren(final OnChildFilesRequestedListener onChildrenReceived)
    {
        Observer<DropboxManager.DropboxResponse> observer = new Observer<DropboxManager.DropboxResponse>() {
            @Override
            public void onSubscribe(@NonNull Disposable d)
            {

            }

            @Override
            public void onNext(@NonNull DropboxManager.DropboxResponse dropboxResponse)
            {
                try
                {
                    DropboxManager.ListFolderResponse list = (DropboxManager.ListFolderResponse)dropboxResponse;

                    final ArrayList<FileChooserItem> children = new ArrayList<FileChooserItem>();

                    for (DropboxFileData item:list.getChildren())
                    {
                        children.add(new DropboxChooserItem(item, mSelectHandler));
                    }

                    Runnable runable = new Runnable()
                    {
                        public void run()
                        {
                            onChildrenReceived.onChildrenRetrieved(DropboxChooserItem.this, children);
                        }
                    };

                    DropboxManager.get().getMainHandler().post(runable);

                }
                catch (Exception e)
                {
                    Runnable runable = new Runnable()
                    {
                        public void run()
                        {
                            onChildrenReceived.onErrorsRetrievingChildren();
                        }
                    };
                    DropboxManager.get().getMainHandler().post(runable);

                }
            }

            @Override
            public void onError(final @NonNull Throwable e)
            {
                Runnable runable = new Runnable()
                {
                    public void run()
                    {
                        onChildrenReceived.onErrorsRetrievingChildren();
                    }
                };
                DropboxManager.get().getMainHandler().post(runable);
            }

            @Override
            public void onComplete()
            {

            }
        };

        DropboxManager.Controller controller = DropboxManager.get().getController();

        controller.listFolderContent(mData.getListableName(), observer);


    }

    @Override
    public boolean hasChildren()
    {

        return mData.isFolder();
    }

    @Override
    public FileChooserItem getParent()
    {
        return mParent;
    }

    @Override
    public int getViewResource()
    {
        return R.layout.v2_dropbox_folder_item;
    }

    @Override
    public void configureView(View inflatedView)
    {
        TextView t = (TextView)inflatedView;
        t.setText(mData.getNameShort());
        if (mData.isFolder())
        {
            t.setTextColor(Color.parseColor("#FF0000"));
        }
        else
            t.setTextColor(Color.parseColor("#000000"));
    }

    @Override
    public int compare(FileChooserItem f)
    {
        return mData.getDisplay().compareTo(f.getFileName());
    }

    @Override
    public boolean onCreateChildFolder(String name, OnCreateChildFileListener listener)
    {
        return false;
    }

    @Override
    public boolean onCreateChildFile(String name, OnCreateChildFileListener listener)
    {
        return false;
    }
}
