package com.evolved.automata.android.lisp.guibuilder.workspace;

import java.util.HashMap;

import com.evolved.automata.CorrectableException;
import com.fasterxml.jackson.annotation.JsonIgnore;

public class Workspace 
{
	Project currentProject;
	
	HashMap<String, Project> projectMap = new HashMap<String, Project>();
	
	public Workspace()
	{
		
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if (obj != null && obj instanceof Workspace)
		{
			Workspace w = (Workspace)obj;
			return (currentProject == null && w.currentProject == null || currentProject!=null && currentProject.equals(w.currentProject)) &&
					projectMap.equals(w.projectMap);
		}
		else
			return false;
	}
	
	public Project createNewProject(String name, boolean setAsCurrentProjectP)
	{
		if (projectMap.containsKey(name))
		{
			return null;
		}
		
		Project proj = new Project(name);
		projectMap.put(name, proj);
		if (setAsCurrentProjectP)
			currentProject = proj;
		return proj;
	}
	
	
	
	
	public Project getCurrentProject()
	{
		return currentProject;
	}
	
	public Project switchCurrentProject(String name)
	{
		if (projectMap.containsKey(name))
		{
			return currentProject = projectMap.get(name);
		}
		else
			return null;
	}
	
	public Project deleteCurrentProject(String name)
	{
		Project p = projectMap.get(name);
		if (p != null)
		{
			projectMap.remove(name);
			if (currentProject.getName().equals(name))
			{
				currentProject = null;
			}
			return currentProject;
		}
		return null;
	}
	
	public boolean deleteProject(String name, String defaultProjectName)
	{
		boolean result = hasProject(name);
		projectMap.remove(name);
		if (currentProject.getName().equals(name))
		{
			if (defaultProjectName == null)
				currentProject = null;
			else
				currentProject = projectMap.get(defaultProjectName);
		}
		
		return result;
	}

	public HashMap<String, Project> getProjectMap() {
		return projectMap;
	}

	public void setProjectMap(HashMap<String, Project> projectMap) {
		this.projectMap = projectMap;
	}

	
	public void setCurrentProject(Project currentProject) {
		this.currentProject = currentProject;
	}
	
	public boolean hasProject(String name)
	{
		return projectMap.containsKey(name);
	}
	
	
	@SuppressWarnings("serial")
	public void renameProject(final String oldName, final String newName) throws CorrectableException
	{
		if (!hasProject(oldName))
		{
			throw new IllegalArgumentException("Can't rename old project, " + oldName + ", which doesn't exist");
		}
		
		if (hasProject(newName) && !oldName.equals(newName))
		{
			throw new CorrectableException("Renaming old project name, " + oldName + ", to " + newName + " will delete " + newName + ".  Continue anyway?")
			{

				@Override
				public void fix() {
					projectMap.remove(newName);
					try {
						renameProject(oldName, newName);
					} catch (CorrectableException e) {
						
					}
				}
				
			};
		}
		
		projectMap.put(newName, projectMap.get(oldName));
		projectMap.remove(oldName);
		
	}
}
