package com.evolved.automata.android.lisp.guibuilder.test;

import org.junit.Ignore;
import org.junit.Test;

import com.evolved.automata.KeyValuePair;
import com.evolved.automata.android.lisp.guibuilder.workspace.Project;
import com.evolved.automata.android.lisp.guibuilder.workspace.Workspace;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

import android.test.AndroidTestCase;

public class GuiBuilderStandardTests extends AndroidTestCase
{
	
	@Test
	public void testSimpleCommandHistoryNavigation()
	{
		String[] codeList = new String[]{"velocity-lambda", "(start-velocity-monitor)", "(update-power 30)"};
		String[] urlList = new String[]{null, "velocity.lisp",null};
		
		try
		{
			Workspace work = new Workspace();
			String projectName = "default";
			Project project = work.createNewProject(projectName, true); 
			assertTrue("Failure setting workspace", project != null);
			assertTrue("Failure setting workspace name", projectName.equals(project.getName()));
			String prevCode = null;
			KeyValuePair<String, String> kv;
			int i = 0, j;
			String code, url, prevUrl;
			for (i = 0;i<codeList.length;i++)
			{
				code = codeList[i];
				url = urlList[i];
				project.updateLastEvaluatedCode(url, code);
				kv = project.getCurrentPage();
				assertNotNull("Current page not set correctly", kv);
				assertTrue("Page code not set to expected value", code.equals(kv.GetValue()));
				assertTrue("page url not set correctly", (url == null && kv.GetKey() == url || (url != null && url.equals(kv.GetKey()))));
				
				for (j = i - 1;j>=0;j--)
				{
					prevCode = codeList[j];
					prevUrl = urlList[j];
					kv = project.gotoPrevPage();
					assertNotNull("Previous page not set", kv);
					assertEquals("Failed to verify moving back", prevCode, kv.GetValue());
					assertEquals("Previous Page code not set to expected value", prevCode,  kv.GetValue());
					assertTrue("Previous page url not set correctly", (prevUrl == null && kv.GetKey() == prevUrl || (prevUrl != null && prevUrl.equals(kv.GetKey()))));
				}
				
				for (j = 0;j<i;j++)
				{
					kv = project.gotoNextPage();
				}
				
				assertNotNull("Current page not set correctly after rewind", kv);
				assertTrue("Page code not set to expected value after rewind", code.equals(kv.GetValue()));
				assertTrue("page url not set correctly after rewind", (url == null && kv.GetKey() == url || (url != null && url.equals(kv.GetKey()))));
				
				
			}
		}
		catch (Exception e)
		{
			assertTrue("General error: " + e.toString(), false);
		}
		
	}
	
	@Test
	public void testAddingDuplicateCode()
	{
		String[] codeList = new String[]{"velocity-lambda", "(start-velocity-monitor)", "(update-power 30)"};
		String[] urlList = new String[]{null, "velocity.lisp",null};
		
		Workspace workspace = new Workspace();
		Project proj = workspace.createNewProject("default", true);
		int i = 0, delta;
		for (i = 0;i<codeList.length;i++)
		{
			delta = proj.updateLastEvaluatedCode(urlList[i], codeList[i]);
			if (i == 0)
				assertTrue("Failure to verify updating offset", delta == 0);
			else
				assertTrue("Failure to verify updating offset", delta == 1);
			delta = proj.updateLastEvaluatedCode(urlList[i], codeList[i]);
			assertTrue("Verify update offset 0 for duplicates", delta == 0);
		}
		
		assertTrue(proj.getNumPages() == codeList.length);
		for (i = 0; i < codeList.length/2;i++)
			proj.gotoPrevPage();
		
		delta = proj.updateLastEvaluatedCode(urlList[codeList.length/2], codeList[codeList.length/2]);
		assertTrue("Verify update offset 0 for reinserting into middle", delta == 0);
		assertTrue(proj.getNumPages() == codeList.length);
		
		delta = proj.updateLastEvaluatedCode(urlList[codeList.length -1], codeList[codeList.length -1]);
		assertTrue(delta == 1);
	}
	
	@Ignore("Need to come up with a sensible policy for inserting and deleting")
	@Test
	public void testDeletingCodeFromFront()
	{
		String[] baseCodeList = new String[]{"velocity-lambda", "(start-velocity-monitor)", "(update-power 30)"};
		int lastIndex = baseCodeList.length - 1;
		
		Workspace workspace = new Workspace();
		Project proj = workspace.createNewProject("default", true);
		int i = 0, delta;
		for (i = 0;i<baseCodeList.length;i++)
		{
			delta = proj.updateLastEvaluatedCode(baseCodeList[i]);
		}
		
		moveToFrontOfCodeHistory(proj);
		
		proj.deleteCurrentPage();
		
		assertTrue("Failure history size", proj.getNumPages() == baseCodeList.length - 1);
		assertTrue("Failure verify beginning of list", !proj.hasPrevPage() && proj.hasNextPage());
		KeyValuePair<String, String> kv =  proj.getCurrentPage();
		assertTrue("Failure verify first", kv.GetValue().equals(baseCodeList[1]));
		
		proj.updateLastEvaluatedCode(baseCodeList[0]);
		assertTrue("Failure history size after reinsert", proj.getNumPages() == baseCodeList.length);
		assertTrue("Failure verify reinsert", proj.getCurrentPage().GetValue().equals(baseCodeList[0]));
		
		moveToBackOfCodeHistory(proj);
		
		assertTrue("Failure verify move to end", !proj.hasNextPage() && proj.hasPrevPage());
		
		proj.deleteCurrentPage();
		
		assertTrue("Failure history size after end delete", proj.getNumPages() == baseCodeList.length - 1);
		assertTrue("Failure verify at end after delete end", !proj.hasNextPage() && proj.hasPrevPage());
		
		
		proj.updateLastEvaluatedCode(baseCodeList[lastIndex]);
		
		// last is inserted into the middle
		
		assertTrue("Failure history size after reinsert at end", proj.getNumPages() == baseCodeList.length);
		assertTrue("Failure verify at end", !proj.hasNextPage() && proj.hasPrevPage());
		
		// test delete in middle
		
		proj.gotoPrevPage();
		
		assertTrue("Failure verify in middle", proj.hasNextPage() && proj.hasPrevPage());
		proj.deleteCurrentPage();
		
		assertTrue("Failure verify delete in middle", proj.hasNextPage() && !proj.hasPrevPage());
		
		assertTrue("Failure very final value of last page after delete from middle", proj.getCurrentPage().GetValue().equals(baseCodeList[0]));
	}
	
	private void moveToFrontOfCodeHistory(Project proj)
	{
		while (proj.hasPrevPage())
			proj.gotoPrevPage();
	}
	
	private void moveToBackOfCodeHistory(Project proj)
	{
		while (proj.hasNextPage())
			proj.gotoNextPage();
	}
	
	
	@Test
	public void testSerialization()
	{
		String[] codeList = new String[]{"velocity-lambda", "(start-velocity-monitor)", "(update-power 30)"};
		String[] urlList = new String[]{null, "velocity.lisp",null};
		
		Workspace workspace = new Workspace();
		Project proj = workspace.createNewProject("default", true);
		int i = 0, delta;
		for (i = 0;i<codeList.length;i++)
		{
			delta = proj.updateLastEvaluatedCode(urlList[i], codeList[i]);
		}
		
		ObjectMapper om = new ObjectMapper();
		//om.disable(SerializationFeature.FAIL_ON_EMPTY_BEANS);
		try
		{
			String serialized = om.writeValueAsString(workspace);
			
			Workspace neww = om.readValue(serialized, Workspace.class);
			
			assertTrue("Equality failure", workspace.equals(neww));
		}
		catch (Exception e)
		{
			assertTrue("Data serialization failure" + e.toString(), false);
		}
	}
}
