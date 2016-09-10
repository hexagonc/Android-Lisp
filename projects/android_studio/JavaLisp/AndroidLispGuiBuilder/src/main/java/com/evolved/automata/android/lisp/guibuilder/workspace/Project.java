package com.evolved.automata.android.lisp.guibuilder.workspace;

import java.util.HashMap;
import java.util.UUID;

import com.evolved.automata.KeyValuePair;
import com.fasterxml.jackson.annotation.JsonIgnore;

public class Project 
{
	
	String name;
	
	HashMap<String, CodePage> pageMap = new HashMap<String, CodePage>();
	
	CodePage currentCodePage;
	
	public Project()
	{
		
	}
	
	public Project(String name)
	{
		this.name = name;
	}
	
	public boolean equals(Object obj)
	{
		if (obj != null && obj instanceof Project)
		{
			Project proj = (Project)obj;
			return (name == proj.name || name!=null && name.equals(proj.name)) &&
					pageMap.equals(proj.pageMap) &&
					(currentCodePage == proj.currentCodePage || (currentCodePage!=null && currentCodePage.equals(proj.currentCodePage)));
		}
		else
			return false;
	}
	
	public String getName()
	{
		return name;
	}
	
	@JsonIgnore
	public int getNumPages()
	{
		return pageMap.size();
	}
	
	/**
	 * When deleting from the front, the current page will be the page after the current page, if present <br/>
	 * When deleting from the end, the current page will be the page before the current page, if present <br/>
	 * When deleting from the middle, the current page will be the page before the current page
	 * @return The current page after delete.  Null if the project is empty
	 */
	public KeyValuePair<String, String> deleteCurrentPage()
	{
		if (currentCodePage != null)
		{
			String nextPageKey = currentCodePage.getNextPage();
			
			String prevPageKey = currentCodePage.getPrevPage();
			
			CodePage nextPage = pageMap.get(nextPageKey), prevPage = pageMap.get(prevPageKey);
			
			pageMap.remove(currentCodePage.getPageKey());
			
			if (prevPage != null)
			{
				prevPage.setNextPage(nextPageKey);
				
				if (nextPage != null)
				{
					nextPage.setPrevPage(prevPageKey);
					currentCodePage = prevPage;
				}
				else
					currentCodePage = prevPage;
				
				return new KeyValuePair<String, String>(currentCodePage.getFileUrl(), currentCodePage.getCode());
			}
			else
			{
				if (nextPage != null)
				{
					nextPage.setPrevPage(null);
					currentCodePage = nextPage;
					return new KeyValuePair<String, String>(currentCodePage.getFileUrl(), currentCodePage.getCode());
				}
				else
				{
					currentCodePage = null;
					return null; 
				}
			}
			
		}
		else
			return null;
	}
	
	public boolean hasPrevPage()
	{
		return currentCodePage!=null && currentCodePage.getPrevPage()!=null;
	}
	
	public boolean hasNextPage()
	{
		return currentCodePage!=null && currentCodePage.getNextPage()!=null;
	}
	
	public boolean hasCurrentPage()
	{
		return currentCodePage != null;
	}
	
	@JsonIgnore
	public KeyValuePair<String, String> getCurrentPage()
	{
		
		if (currentCodePage != null )
		{
			
			KeyValuePair<String, String> kvPair = new KeyValuePair<String, String>(currentCodePage.getFileUrl(), currentCodePage.getCode());
			
			return kvPair;
		}
		else
			return null;
	}
	
	
	public KeyValuePair<String, String> gotoNextPage()
	{
		String nextPageKey = null;
		if (currentCodePage != null && (nextPageKey = currentCodePage.getNextPage())!=null)
		{
			CodePage nextPage = pageMap.get(nextPageKey);
			KeyValuePair<String, String> kvPair = new KeyValuePair<String, String>(nextPage.getFileUrl(), nextPage.getCode());
			currentCodePage = nextPage;
			return kvPair;
		}
		else
			return null;
	}
	
	public KeyValuePair<String, String> gotoPrevPage()
	{
		String prevPageKey = null;
		if (currentCodePage != null && (prevPageKey = currentCodePage.getPrevPage())!=null)
		{
			CodePage prevPage = pageMap.get(prevPageKey);
			KeyValuePair<String, String> kvPair = new KeyValuePair<String, String>(prevPage.getFileUrl(), prevPage.getCode());
			currentCodePage = prevPage;
			return kvPair;
		}
		else
			return null;
	}
	
	
	/**
	 * Returns the offset of the current page after the update versus before
	 * @param code
	 * @return
	 */
	public int updateLastEvaluatedCode(String code)
	{
		return updateLastEvaluatedCode(null, code);
	}
	
	/**
	 * Returns the offset of the current page after the update versus before
	 * @param code
	 * @return
	 */
	public int updateLastEvaluatedCode(String fileUrl, String code)
	{
		if (currentCodePage!=null)
		{
			if (isEquivalentPage(currentCodePage, fileUrl, code))
				return 0;
			
			String nextPageKey = currentCodePage.getNextPage();
			
			String prevPageKey = currentCodePage.getPrevPage();
			
			CodePage nextPage = pageMap.get(nextPageKey), prevPage = pageMap.get(prevPageKey);
			
			if (isEquivalentPage(nextPage, fileUrl, code))
			{
				currentCodePage = nextPage;
				return 1;
			}
			
			if (isEquivalentPage(prevPage, fileUrl, code))
			{
				currentCodePage = prevPage;
				return -1;
			}
			
			// Create new CodePage
			String id = UUID.randomUUID().toString();
			CodePage newPage = new CodePage(id, fileUrl, code);
			pageMap.put(id, newPage);
			
			newPage.setPrevPage(currentCodePage.getPageKey());
			newPage.setNextPage(currentCodePage.getNextPage());
			
			if (nextPage != null)
			{
				nextPage.setPrevPage(id);
			}
			currentCodePage.setNextPage(id);
			currentCodePage = newPage;
			return 1;
		}
		else
		{
			String id = UUID.randomUUID().toString();
			currentCodePage = new CodePage(id, fileUrl, code);
			pageMap.put(id, currentCodePage);
			return 0;
		}
	}
	
	private boolean isEquivalentPage(CodePage page, String url, String code)
	{
		return page!=null && page.getCode().equals(code) && ( (page.getFileUrl() == null && url == null) || (page.getFileUrl()!=null && page.getFileUrl().equals(url)));
	}

	public HashMap<String, CodePage> getPageMap() {
		return pageMap;
	}

	public void setPageMap(HashMap<String, CodePage> pageMap) {
		this.pageMap = pageMap;
	}

	public CodePage getCurrentCodePage() {
		return currentCodePage;
	}

	public void setCurrentCodePage(CodePage currentCodePage) {
		this.currentCodePage = currentCodePage;
	}

	public void setName(String name) {
		this.name = name;
	}
}
