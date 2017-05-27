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
 * Created by Evolved8 on 5/9/17.
 */

public class WorkspaceManagementFragment extends DialogFragment {

    public enum CHANGE_TYPE
    {
        ADD_WORKSPACES, DELETE_WORKSPACES, RENAME_WORKSPACES, CHANGE_CURRENT_WORKSPACE
    }

    public static abstract class Change
    {
        CHANGE_TYPE _type;

        public CHANGE_TYPE getType()
        {
            return _type;
        }

        public HashMap<CHANGE_TYPE, Change> fill(HashMap<CHANGE_TYPE, Change> map)
        {
            map.put(getType(), this);
            return map;
        }

    }

    public static class RenameWorkspacesChange extends Change
    {
        LinkedList<WorkspaceData> _list;

        public RenameWorkspacesChange(LinkedList<WorkspaceData> list)
        {
            _type = CHANGE_TYPE.RENAME_WORKSPACES;
            _list = list;
        }

        public LinkedList<WorkspaceData> getChange()
        {
            return _list;
        }
    }




    public static class WorkspaceData
    {
        String _workspaceId;
        String _title;
        boolean _isSelectedP = false;
        boolean _isNewP = false;

        public WorkspaceData(String id, String title)
        {
            _title = title;
            _workspaceId = id;
        }

        public String getWorkspaceId()
        {
            return _workspaceId;
        }

        public String getNewTitle()
        {
            return _title;
        }

        public void setSelected(boolean selected)
        {
            _isSelectedP = selected;
        }

        public boolean isSelected()
        {
            return _isSelectedP;
        }

        public void setNewTitle(String title)
        {
            _title = title;
        }

        public void setNew()
        {
            _isNewP = true;
        }

        public boolean isNew()
        {
            return _isNewP;
        }
    }

    public static class AddedWorkspacesChange extends Change
    {
        LinkedList<WorkspaceData> _addedList;

        public AddedWorkspacesChange(LinkedList<WorkspaceData> list)
        {
            _type = CHANGE_TYPE.ADD_WORKSPACES;
            _addedList = list;
        }

        public LinkedList<WorkspaceData> getChange()
        {
            return _addedList;
        }
    }

    public static class DeleteWorkspacesChange extends Change
    {
        LinkedList<String> _deletedIds;

        public DeleteWorkspacesChange(LinkedList<String> list)
        {
            _type = CHANGE_TYPE.DELETE_WORKSPACES;
            _deletedIds = list;
        }

        public LinkedList<String> getChange()
        {
            return _deletedIds;
        }
    }

    public static class SelectWorkspaceChange extends Change
    {
        String _workspaceId;

        boolean _isNewP;

        public SelectWorkspaceChange(String id, boolean isNew)
        {
            _type = CHANGE_TYPE.CHANGE_CURRENT_WORKSPACE;
            _workspaceId = id;
            _isNewP = isNew;
        }

        public String getChange()
        {
            return _workspaceId;
        }

        public boolean isNew()
        {
            return _isNewP;
        }

    }



    LinkedList<String> mWorkspacesToDelete = new LinkedList<String>();
    LinkedList<WorkspaceData> mWorkspacesToCreate = new LinkedList<WorkspaceData>();
    LinkedList<WorkspaceData> mWorkspacesToRename = new LinkedList<WorkspaceData>();


    ShadowButton mAcceptButton;
    ShadowButton mCancelButton;
    ShadowButton mCreateWorkspaceButton;
    ShadowButton mRenameWorkspaceButton;
    ShadowButton mDeleteWorkspaceButton;

    EditText mSelectedWorkspaceText;

    ListView mWorkspaceList;

    WorkspaceData mSelectedData;


    ArrayList<WorkspaceData> mWorkspaceListData = new ArrayList<WorkspaceData>();
    ArrayAdapter<WorkspaceData> mWorkspaceAdapter;


    public interface ManagementChangeListener
    {
        void onClose();
        void onClose(HashMap<CHANGE_TYPE, Change> changes);
    }


    ALGB mApplication;
    ManagementChangeListener mChangeListener;
    HashMap<CHANGE_TYPE, Change> mChanges = new HashMap<CHANGE_TYPE, Change>();

    int mStyle = DialogFragment.STYLE_NORMAL;
    int mTheme = 0;

    int mSelectedWorkspacePos = -1;

    String mInitialWorkspaceId;

    public static WorkspaceManagementFragment create(ManagementChangeListener listener, ALGB app)
    {
        WorkspaceManagementFragment frag = new WorkspaceManagementFragment();
        frag.setChangeListener(listener);
        frag.setApplication(app);
        return frag;
    }

    private void setChangeListener(ManagementChangeListener listener)
    {
        mChangeListener = listener;
    }

    private void setApplication(ALGB app)
    {
        mApplication = app;
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
        ViewGroup top = (ViewGroup)inflater.inflate(R.layout.v2_workspace_manager, container, false);

        mWorkspaceList = (ListView)top.findViewById(R.id.v2_lst_workspaces);
        mCancelButton = (ShadowButton)top.findViewById(R.id.v2_sdw_but_workspace_cancel);
        mAcceptButton = (ShadowButton)top.findViewById(R.id.v2_sdw_but_workspace_accept);
        mCreateWorkspaceButton = (ShadowButton)top.findViewById(R.id.v2_sdw_but_workspace_create);
        mRenameWorkspaceButton = (ShadowButton)top.findViewById(R.id.v2_sdw_but_workspace_rename);
        mDeleteWorkspaceButton = (ShadowButton)top.findViewById(R.id.v2_sdw_but_workspace_delete);
        mSelectedWorkspaceText = (EditText)top.findViewById(R.id.v2_edit_workspace_name);

        /* Use conditional logic to remove or hide this depending on dialog style as
        is done for the PagePropertiesFragment.  Currently, we are not accepting
        different styles for this dialog fragment
         */
        if (mStyle !=  DialogFragment.STYLE_NO_TITLE)
        {
            TextView title = (TextView)top.findViewById(R.id.v2_txt_workspace_manager_header);
            title.setVisibility(View.GONE);
        }



        getDialog().setTitle(R.string.v2_workspace_management_title);
        configureUI();

        return top;
    }

    void configureUI()
    {
        Workspace current = mApplication.getCurrentWorkspace();
        mInitialWorkspaceId = current.getWorkspaceId();

        for (String id:mApplication.getAllWorkspaceId())
        {
            Workspace work = mApplication.getWorkspace(id);
            WorkspaceData data = new WorkspaceData(id, work.getTitle());
            data.setSelected(id.equals(mInitialWorkspaceId));
            mWorkspaceListData.add(data);
        }


        mWorkspaceAdapter = new ArrayAdapter<WorkspaceData>(getActivity(), 0, mWorkspaceListData)
        {

            @NonNull
            @Override
            public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent)
            {
                WorkspaceData data = mWorkspaceListData.get(position);
                LinearLayout out;
                if (convertView == null)
                {
                    LayoutInflater inflater = getActivity().getLayoutInflater();
                    out = (LinearLayout)inflater.inflate(R.layout.v2_workspace_list_item, parent, false);
                }
                else
                {
                    out = (LinearLayout)convertView;

                }
                TextView workTitle = (TextView)out.findViewById(R.id.v2_workspace_item_name);
                ImageView icon = (ImageView)out.findViewById(R.id.v2_icon_workspace_item_selected);

                if (data.isSelected())
                    icon.setVisibility(View.VISIBLE);
                else
                    icon.setVisibility(View.INVISIBLE);
                workTitle.setText(data.getNewTitle());
                return out;
            }
        };

        mWorkspaceList.setAdapter(mWorkspaceAdapter);
        mWorkspaceList.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id)
            {

                WorkspaceData data = mWorkspaceListData.get(position);
                mSelectedWorkspaceText.setText(data.getNewTitle());
                mSelectedWorkspacePos = position;

            }
        });

        mWorkspaceList.setOnItemLongClickListener(new AdapterView.OnItemLongClickListener() {
            @Override
            public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id)
            {
                mSelectedData = mWorkspaceListData.get(position);
                mSelectedWorkspacePos = position;

                for (int i = 0;i < mWorkspaceListData.size();i++)
                {
                    WorkspaceData data = mWorkspaceListData.get(i);
                    data.setSelected(i == position);

                }

                mWorkspaceAdapter.notifyDataSetChanged();
                Toast.makeText(getActivity(), "Set Current Workspace", Toast.LENGTH_SHORT).show();

                return true;
            }
        });

        // Buttons

        mCancelButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v)
            {
                cancelChanges();
            }
        });

        mCreateWorkspaceButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v)
            {
                String title = mSelectedWorkspaceText.getText().toString();
                if (title.trim().length()>0)
                {
                    WorkspaceData data = new WorkspaceData(UUID.randomUUID().toString(), title);
                    data.setNew();
                    mSelectedWorkspaceText.setText("");
                    mSelectedWorkspacePos = mWorkspaceListData.size();
                    mWorkspacesToCreate.add(data);
                    mWorkspaceListData.add(data);

                    mWorkspaceAdapter.notifyDataSetChanged();
                }
                else
                {
                    Toast.makeText(getActivity(), "You must set a title before creating a new workspace", Toast.LENGTH_LONG).show();
                }

            }
        });

        mDeleteWorkspaceButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v)
            {

                if (mSelectedWorkspacePos != -1)
                {
                    if (mWorkspaceListData.size() == 1)
                    {
                        Toast.makeText(getActivity(), "Can't delete last workspace.  Trying renaming or clearing it.", Toast.LENGTH_LONG);
                        return;
                    }


                    WorkspaceData data = mWorkspaceListData.get(mSelectedWorkspacePos);

                    if (data.isSelected())
                    {
                        Toast.makeText(getActivity(), "Can't delete the selected workspace.", Toast.LENGTH_LONG).show();
                    }
                    else
                    {
                        mWorkspacesToCreate.remove(data);
                        mSelectedWorkspaceText.setText("");

                        if (!data.isNew())
                            mWorkspacesToDelete.add(data.getWorkspaceId());
                        mWorkspaceListData.remove(mSelectedWorkspacePos);
                        mSelectedWorkspacePos = -1;

                        mWorkspaceAdapter.notifyDataSetChanged();
                    }


                }
                else
                {
                    Toast.makeText(getActivity(), "No workspace selected", Toast.LENGTH_LONG).show();
                }

            }
        });


        mRenameWorkspaceButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v)
            {

                if (mSelectedWorkspacePos != -1)
                {
                    WorkspaceData oldData = mWorkspaceListData.get(mSelectedWorkspacePos);

                    String oldTitle = oldData.getNewTitle();
                    String newTitle = mSelectedWorkspaceText.getText().toString();
                    if (!oldTitle.equals(newTitle))
                    {
                        oldData.setNewTitle(newTitle);
                        if (!mWorkspacesToRename.contains(oldData))
                            mWorkspacesToRename.add(oldData);
                        mWorkspaceAdapter.notifyDataSetChanged();
                    }
                    else
                        Toast.makeText(getActivity(), "New title is the same as the old", Toast.LENGTH_LONG).show();


                }
                else
                {
                    Toast.makeText(getActivity(), "You must selected a workspace to rename", Toast.LENGTH_LONG).show();
                }

            }
        });

        mAcceptButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v)
            {
                acceptChanges();
            }
        });

    }

    private void cancelChanges()
    {
        mChangeListener.onClose();

    }

    private void acceptChanges()
    {
        if (mWorkspacesToRename.size()>0)
        {
            new RenameWorkspacesChange(mWorkspacesToRename).fill(mChanges);
        }

        if (mWorkspacesToCreate.size()>0)
        {
            new AddedWorkspacesChange(mWorkspacesToCreate).fill(mChanges);
        }

        if (mWorkspacesToDelete.size()>0)
        {
            new DeleteWorkspacesChange(mWorkspacesToDelete).fill(mChanges);
        }

        for (WorkspaceData data:mWorkspaceListData)
        {
            if (data.isSelected() && !data.getWorkspaceId().equals(mInitialWorkspaceId))
            {
                new SelectWorkspaceChange(data.getWorkspaceId(), data.isNew()).fill(mChanges);
            }
        }

        mChangeListener.onClose(mChanges);
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
