package com.evolved.automata.android.lisp.guibuilder;

import com.evolved.automata.KeyValuePair;
import com.evolved.automata.android.lisp.guibuilder.events.EventManager;
import com.evolved.automata.android.lisp.guibuilder.events.ProjectEventListener;
import com.evolved.automata.android.lisp.guibuilder.events.ToolAreaEventListener;
import com.evolved.automata.android.lisp.guibuilder.toolareas.CodeManagementUIInterface;
import com.evolved.automata.android.lisp.guibuilder.workspace.Project;

import android.content.Context;
import android.view.Menu;
import android.view.MenuItem;

public class MenuManager 
{
	private static interface MenuMode
	{
		public boolean onPrepareOptionsMenu(Menu menu);
		public boolean onOptionsItemSelected(MenuItem item);
	}
	
	public enum MenuType
	{
		DEFAULT_AND_LOCAL_STORAGE(new int[]{
											R.id.menu_load_from_local_storage_file,
											R.id.menu_save_as_local_storage_file,
											R.id.menu_save_to_local_storage_file}),
		DEFAULT_AND_DROPBOX(new int[] {
										R.id.menu_save_as_dropbox_file,
										R.id.menu_save_to_last_loaded_dropbox_file,
										R.id.menu_load_dropbox_file,
										R.id.menu_update_current_file_from_dropbox}),
		DEFAULT_AND_CODE_TEMPLATES(new int[] {});
		
		int[] itemIds;
		
		private MenuType(int[] items)
		{
			this.itemIds = items;
		}
		
		public void removeItems(Menu menu)
		{
			for (int itemId:itemIds)
			{
				menu.removeItem(itemId);
			}
		}
	}
	
	MenuType _currentMenuType = MenuType.DEFAULT_AND_LOCAL_STORAGE;
	protected CodeManagementUIInterface _uiInterface;
	
			
	Context _context;
	
	private static MenuManager _manager;
	ProjectEventListener _eventNotifier;
	ToolAreaEventListener _toolAreaListener;
	
	MenuModeCodeEditor _codeEditorMenu;
	MenuModeSettings _settingsMenu;
	MenuModeCodeRendering _renderingMenu;
	MenuMode _currentMode;
	
	
	private MenuManager(Context cont)
	{
		_context = cont;
		_toolAreaListener = new ToolAreaEventListener()
		{

			@Override
			public void localStorageToolsSelected() {
				_currentMenuType = MenuType.DEFAULT_AND_LOCAL_STORAGE;
			}

			@Override
			public void dropboxToolsSelected() {
				_currentMenuType = MenuType.DEFAULT_AND_DROPBOX;
			}

			@Override
			public void codeTemplateToolsSelected() {
				_currentMenuType = MenuType.DEFAULT_AND_CODE_TEMPLATES;
			}
			
		};
		_codeEditorMenu = new MenuModeCodeEditor();
		_settingsMenu = new MenuModeSettings();
		_renderingMenu = new MenuModeCodeRendering();
		EventManager.getInstance().setToolAreaEventListener(_toolAreaListener);
	}
	
	
	public static MenuManager create(Context con)
	{
		if (_manager == null)
		{
			_manager = new MenuManager(con);
			_manager._eventNotifier = EventManager.getInstance().getProjectEventNotifier();
			
		}
		
		
		return _manager;
	}
	
	public static MenuManager get()
	{
		return _manager;
	}
	
	public void setUIInterface(CodeManagementUIInterface ui)
	{
		_uiInterface = ui;
		
	}
	
	public void setCodeEditorMenuType(MenuType type)
	{
		_currentMenuType = type;
	}
	
	public void setCodeEditorMenuMode()
	{
		_currentMode = _codeEditorMenu;
		
	}
	
	
	public void setRenderingMenuMode()
	{
		_currentMode = _renderingMenu;
		
	}
	
	public void setSettingsMenuMode()
	{
		_currentMode = _settingsMenu;
		
	}
	
	public boolean onOptionsItemSelected(MenuItem item)
	{
		if (!_currentMode.onOptionsItemSelected(item))
		{
			switch (item.getItemId())
			{
				case R.id.menu_reset_environment:
					return resetEnvironment(); 
			}
		}
		return false;
	}
	
	
	public boolean onPrepareOptionsMenu(Menu menu) {
		return _currentMode.onPrepareOptionsMenu(menu);
		
	}
	
	
	
	
	private class MenuModeSettings implements MenuMode
	{
		public boolean onPrepareOptionsMenu(Menu menu) {
			MenuType.DEFAULT_AND_CODE_TEMPLATES.removeItems(menu);
			MenuType.DEFAULT_AND_LOCAL_STORAGE.removeItems(menu);
			MenuType.DEFAULT_AND_DROPBOX.removeItems(menu);
			menu.removeItem(R.id.menu_show_settings);
			menu.removeItem(R.id.menu_manage_workspaces);
			menu.removeItem(R.id.menu_remove_code_page);
			return true;
		}
		
		public boolean onOptionsItemSelected(MenuItem item)
		{
			switch (item.getItemId())
			{
				case R.id.menu_show_help:
					return showHelpMenuItem();
				case R.id.menu_reset_environment:
					return resetEnvironment();
					
			}
			return false;
		}
	}
	
	private class MenuModeCodeRendering implements MenuMode
	{
		public boolean onPrepareOptionsMenu(Menu menu) {
			MenuType.DEFAULT_AND_CODE_TEMPLATES.removeItems(menu);
			MenuType.DEFAULT_AND_LOCAL_STORAGE.removeItems(menu);
			MenuType.DEFAULT_AND_DROPBOX.removeItems(menu);
			menu.removeItem(R.id.menu_manage_workspaces);
			menu.removeItem(R.id.menu_remove_code_page);
			return true;
		}
		
		public boolean onOptionsItemSelected(MenuItem item)
		{
			switch (item.getItemId())
			{
				case R.id.menu_show_help:
					return showHelpMenuItem();
				case R.id.menu_reset_environment:
					return resetEnvironment();
					
			}
			return false;
		}
	}
	
	private class MenuModeCodeEditor implements MenuMode
	{
		
		
		public boolean onPrepareOptionsMenu(Menu menu) {
			
			
			switch (_currentMenuType)
			{
				case DEFAULT_AND_CODE_TEMPLATES:
					MenuType.DEFAULT_AND_LOCAL_STORAGE.removeItems(menu);
					MenuType.DEFAULT_AND_DROPBOX.removeItems(menu);
					return true;
				case DEFAULT_AND_DROPBOX:
					MenuType.DEFAULT_AND_LOCAL_STORAGE.removeItems(menu);
					return true;
				case DEFAULT_AND_LOCAL_STORAGE:
					MenuType.DEFAULT_AND_DROPBOX.removeItems(menu);
					return true;
			}
			
			
			
			return false;
		}
		
		public boolean onOptionsItemSelected(MenuItem item)
		{
			switch (item.getItemId())
			{
				case R.id.menu_remove_code_page:
					Project project = CodeManager.get().getCurrentProject();
					if (project != null)
					{
						KeyValuePair<String, String> newPaage = project.deleteCurrentPage();
						if (newPaage == null)
							_eventNotifier.currentCodePageDeleted("", "", false, false);
						else
						{
							if (newPaage.GetKey()!=null && newPaage.GetKey().trim().length()>0)
								_eventNotifier.currentCodePageDeleted(CodeManager.get().getShortFileNameFromPathUrl(newPaage.GetKey()) , newPaage.GetValue(), project.hasPrevPage(), project.hasNextPage());
							else
								_eventNotifier.currentCodePageDeleted("" , newPaage.GetValue(), project.hasPrevPage(), project.hasNextPage());
						}
					}
					return true;
				case R.id.menu_readonly_mode:
					if (item.isChecked())
						item.setChecked(false);
					else
						item.setChecked(true);
					_uiInterface.toggleEditorReadOnlyStatus(item.isChecked());
					
					return true;
				case R.id.menu_load_dropbox_file:
					_uiInterface.loadCurrentCodeEditorFromDropbox();
					return true;
				case R.id.menu_save_as_dropbox_file:
					_uiInterface.saveCurrentCodeEditorToDropbox();
					return true;
				case R.id.menu_load_from_local_storage_file:
					_uiInterface.loadCurrentCodeEditorFromLocalStorage();
					return true;
				case R.id.menu_save_as_local_storage_file:
					_uiInterface.saveCurrentCodeEditorToLocalStorage();
					return true;
				case R.id.menu_manage_workspaces:
					_uiInterface.showProjectManagerDialog();
					return true;
					
			}
			return false;
		}
	}
	
	
	private boolean showSettingsMenuItem()
	{
		return true;
	}
	
	private boolean showHelpMenuItem()
	{
		return true;
	}
	
	private boolean resetEnvironment()
	{
		EventManager.getInstance().getLifeCycleEventNotifier().onResetEnvironmentRequested(this);
		return true;
	}
}
