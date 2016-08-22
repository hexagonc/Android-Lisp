package com.evolved.automata.lisp;

import java.util.HashMap;

public class CodePageMap 
{
	HashMap<String, CodePage> pageMap;
	
	String currentPage;

	public HashMap<String, CodePage> getPageMap() {
		return pageMap;
	}

	public void setPageMap(HashMap<String, CodePage> pageMap) {
		this.pageMap = pageMap;
	}

	public String getCurrentPage() {
		return currentPage;
	}

	public void setCurrentPage(String currentPage) {
		this.currentPage = currentPage;
	}
	
	@Override
	public boolean equals(Object oj)
	{
		if (oj!=null && oj instanceof CodePageMap)
		{
			CodePageMap cpm = (CodePageMap)oj;
			return currentPage.equals(cpm.currentPage) && (pageMap == cpm.pageMap || pageMap!=null && pageMap.equals(cpm.pageMap));
		}
		else
			return false;
	}
}
