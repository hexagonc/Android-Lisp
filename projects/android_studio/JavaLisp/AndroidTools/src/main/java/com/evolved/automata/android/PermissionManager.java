package com.evolved.automata.android;

import android.Manifest;
import android.content.Context;
import android.content.pm.PackageManager;
import android.os.Build;
import android.support.v4.content.*;

/**
 * Created by Evolved8 on 6/26/17.
 */

public class PermissionManager {

    private static PermissionManager mManager;
    Context mContext;
    PackageManager mPackageManager;
    private PermissionManager(Context con)
    {
        mContext = con;
        mPackageManager = con.getPackageManager();
    }

    public PermissionManager create(Context con)
    {
        if (mManager == null)
        {
            mManager = new PermissionManager(con);
        }
        return mManager;
    }

    public PermissionManager get()
    {
        return mManager;
    }

    private boolean hasPermmision(String permissionName)
    {
        if (Build.VERSION.SDK_INT >= 23)
        {
            return ContextCompat.checkSelfPermission(mContext, permissionName) == PackageManager.PERMISSION_GRANTED;
        }
        else
        {
            return PackageManager.PERMISSION_GRANTED == mPackageManager.checkPermission(permissionName, mContext.getPackageName());
        }
    }

    public boolean internetPermittedP()
    {
        return hasPermmision(Manifest.permission.INTERNET);
    }

    public boolean accessCameraPermittedP()
    {
        return hasPermmision(Manifest.permission.CAMERA);
    }

}
