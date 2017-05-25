package com.evolved.automata.android.lisp.guibuilder;

import com.dropbox.core.v2.files.Metadata;

import org.greenrobot.eventbus.EventBus;
import org.json.JSONObject;



/**
 * Created by Evolved8 on 5/11/17.
 */

public class Tools {


    static EventBus mMain = EventBus.getDefault();



    public static void postEvent(Object o)
    {
        mMain.post(o);
    }

    public static void postStickyEvent(Object o)
    {
        mMain.postSticky(o);
    }


    public static void registerEventHandler(Object o)
    {

        mMain.register(o);
    }

    public static void unRegisterEventHandler(Object o)
    {
        mMain.unregister(o);
    }



    public static boolean isFolder(Metadata dbxMetaData)
    {
        String sForm = dbxMetaData.toString();
        try
        {
            JSONObject jobject = new JSONObject(sForm);
            String typeName = jobject.getString(".tag");
            return  "folder".equals(typeName);
        }
        catch (Exception e)
        {
            throw new RuntimeException(e.toString());
        }

    }

    public static String getParentFolder(String fullFileName)
    {
        StringBuilder parentReady = new StringBuilder("/"), segment = new StringBuilder();

        for (char c:fullFileName.toCharArray())
        {
            if (c == '/')
            {
                if (parentReady.length() > 1)
                    parentReady.append('/');
                parentReady.append(segment);
                segment = new StringBuilder();
            }
            else
                segment.append(c);
        }

        return parentReady.toString();

    }





}
