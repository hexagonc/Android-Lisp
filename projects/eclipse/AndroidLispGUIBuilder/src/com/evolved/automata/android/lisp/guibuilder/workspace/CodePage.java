package com.evolved.automata.android.lisp.guibuilder.workspace;

import com.fasterxml.jackson.annotation.JsonIgnore;



public class CodePage 
{
	private String pageKey = "";
	private String fileUrl = null;
	private String nextPage = null;
	private String prevPage = null;
	private String code = "";
	
	
	public CodePage()
	{
		
	}
	
	public CodePage(String pk, String fU, String c)
	{
		
		pageKey = pk;
		fileUrl = fU;
		code = c;
	}
	
	
	
	public String getPageKey() {
		return pageKey;
	}
	public void setPageKey(String pageKey) {
		this.pageKey = pageKey;
	}
	public String getFileUrl() {
		return fileUrl;
	}
	public void setFileUrl(String fileUrl) {
		this.fileUrl = fileUrl;
	}
	public String getNextPage() {
		return nextPage;
	}
	public void setNextPage(String nextPage) {
		this.nextPage = nextPage;
	}
	public String getPrevPage() {
		return prevPage;
	}
	public void setPrevPage(String prevPage) {
		this.prevPage = prevPage;
	}
	public String getCode() {
		return code;
	}
	public void setCode(String code) {
		this.code = code;
	}
	
	@Override
	public boolean equals(Object comp)
	{
		if (comp instanceof CodePage)
		{
			CodePage that = (CodePage)comp;
			return (code == that.code || (code != null && code.equals(that.code))) &&
					(fileUrl == that.fileUrl || (fileUrl != null && fileUrl.equals(that.fileUrl))) &&
					(pageKey == that.pageKey || (pageKey != null && pageKey.equals(that.pageKey))) &&
					(prevPage == that.prevPage || (prevPage != null && prevPage.equals(that.prevPage))) &&
					(nextPage == that.nextPage || (nextPage != null && nextPage.equals(that.nextPage)));
				   
		}
		else
			return false;
	}
	
	
}
