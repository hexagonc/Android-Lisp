package com.evolved.automata.android.lisp.guibuilder;

import android.support.v4.app.Fragment;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;

import java.util.HashMap;
import java.util.HashSet;

/**
 * Created by Evolved8 on 9/14/17.
 */

public class MenuHelper {

    private static class ItemDisplaySpec {
        HashSet<Class> mfragmentsToShow = new HashSet();
        HashSet<Class> mfragmentsToHide = new HashSet();

        ItemDisplaySpec(Class[] fragmentsToShowItem, Class[] fragmentsToHideItem)
        {
            if (fragmentsToShowItem != null)
            {
                for (Class c: fragmentsToShowItem)
                {
                    mfragmentsToShow.add(c);
                }
            }

            if (fragmentsToHideItem != null)
            {
                for (Class c: fragmentsToHideItem)
                {
                    mfragmentsToHide.add(c);
                }
            }

        }


        boolean mustShow(Class frag)
        {
            return mfragmentsToShow.contains(frag);
        }

        boolean mustHide(Class frag)
        {
            return mfragmentsToHide.contains(frag);
        }

    }

    static HashMap<Integer, ItemDisplaySpec> mFragmentMenuSpec = new HashMap<Integer, ItemDisplaySpec>();

    static void addMenuSpec(int menuItemId, Class[] fragmentsToShow, Class[] fragmentsToHide)
    {
        mFragmentMenuSpec.put(Integer.valueOf(menuItemId), new ItemDisplaySpec(fragmentsToShow, fragmentsToHide));
    }

    static void showMenuItem(int menuItemId, Class[] fragmentsToShow)
    {
        mFragmentMenuSpec.put(Integer.valueOf(menuItemId), new ItemDisplaySpec(fragmentsToShow, null));
    }

    static void hideMenuItem(int menuItemId, Class[] fragmentsToHide)
    {
        mFragmentMenuSpec.put(Integer.valueOf(menuItemId), new ItemDisplaySpec(null, fragmentsToHide));
    }

    static {
        addMenuSpec(R.id.v2_action_delete_page, new Class[]{WorkspaceFragment.class}, new Class[]{SettingsFragment.class});
        addMenuSpec(R.id.v2_action_add_page, new Class[]{WorkspaceFragment.class}, new Class[]{SettingsFragment.class});
        addMenuSpec(R.id.v2_menu_page_properties, new Class[]{WorkspaceFragment.class}, new Class[]{SettingsFragment.class});
        addMenuSpec(R.id.v2_menu_workspace_management, new Class[]{WorkspaceFragment.class}, new Class[]{SettingsFragment.class});
        addMenuSpec(R.id.v2_start_dropbox_authentication, new Class[]{WorkspaceFragment.class}, new Class[]{SettingsFragment.class});
        addMenuSpec(R.id.v2_menu_show_event_log, new Class[]{WorkspaceFragment.class}, new Class[]{SettingsFragment.class});

        addMenuSpec(R.id.v2_menu_save_page, new Class[]{WorkspaceFragment.class}, new Class[]{SettingsFragment.class});
        addMenuSpec(R.id.v2_menu_save_all, new Class[]{WorkspaceFragment.class}, new Class[]{SettingsFragment.class});

        addMenuSpec(R.id.show_settings, new Class[]{WorkspaceFragment.class}, new Class[]{SettingsFragment.class});

        addMenuSpec(R.id.v2_menu_to_code, new Class[]{RenderFragment.class}, new Class[]{CodePageFragment.class, SettingsFragment.class});
        addMenuSpec(R.id.v2_menu_render, new Class[]{CodePageFragment.class}, new Class[]{RenderFragment.class, SettingsFragment.class});
    }

    public static void updateMenuItemDisplay(Menu menu, Fragment frag)
    {
        MenuItem item = null;
        int numItems = menu.size(), itemId;
        Class fragClass = frag.getClass();
        for (int i = 0; i < numItems;i++)
        {
            item = menu.getItem(i);
            itemId = item.getItemId();

            ItemDisplaySpec displaySpec = mFragmentMenuSpec.get(Integer.valueOf(itemId));
            if (displaySpec != null)
            {

                if (displaySpec.mustShow(fragClass))
                {
                    item.setVisible(true);
                    View v = item.getActionView();
                    if (v != null)
                        v.setVisibility(View.VISIBLE);
                }
                else if (displaySpec.mustHide(fragClass))
                {
                    item.setVisible(false);
                    View v = item.getActionView();
                    if (v != null)
                        v.setVisibility(View.INVISIBLE);
                }
            }
        }
    }

}
