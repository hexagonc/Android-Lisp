package com.evolved.automata.android.lisp.guibuilder;

import com.dropbox.core.v2.files.FileMetadata;
import com.dropbox.core.v2.files.FolderMetadata;
import com.dropbox.core.v2.files.Metadata;

/**
 * Created by Evolved8 on 5/11/17.
 */

public class DropboxFileData {

    public static final String ROOT_DISPLAY_NAME = "/";
    public static final String ROOT_LISTABLE_NAME = "";
    private boolean mIsFolder = false;
    private Metadata mData;

    private DropboxFileData(Metadata data, boolean folderP)
    {
        mData = data;
        mIsFolder =folderP;
    }

    public boolean isFolder()
    {
        return mIsFolder;
    }


    public String getListableParentName()
    {
        if (!isRoot())
        {
            String path = mData.getPathDisplay();
            String parent =Tools.getParentFolder(path);
            if (parent.equals(ROOT_DISPLAY_NAME))
                return  ROOT_LISTABLE_NAME;
            else
                return parent;
        }
        else
            return null;
    }

    public static boolean isRootFolderPath(String path)
    {
        return ROOT_DISPLAY_NAME.equals(path) || ROOT_LISTABLE_NAME.equals(path);
    }

    public String getListableName()
    {
        if (!isRoot())
        {
            return mData.getPathDisplay();
        }
        else
            return ROOT_LISTABLE_NAME;
    }

    public String getNameShort()
    {
        if (mData != null)
            return mData.getName();
        else
            return ROOT_DISPLAY_NAME;
    }


    public static DropboxFileData from(Metadata meta)
    {


        if (meta instanceof FileMetadata)
        {
            return new DropboxFileData(meta, false);
        }
        else if (meta instanceof FolderMetadata)
            return new DropboxFileData(meta, true);
        else
            throw new IllegalArgumentException("Metadata cannot be null");
    }

    public static DropboxFileData createRootFolderData()
    {
        return new DropboxFileData(null, true);
    }

    public boolean isRoot()
    {
        return mData == null;
    }

    public String getDisplay()
    {
        if (mData != null)
        {

            return mData.getPathDisplay();
        }
        else
            return ROOT_DISPLAY_NAME;
    }
}
