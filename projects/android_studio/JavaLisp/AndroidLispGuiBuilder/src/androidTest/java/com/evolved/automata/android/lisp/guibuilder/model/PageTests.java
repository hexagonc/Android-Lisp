package com.evolved.automata.android.lisp.guibuilder.model;

import android.app.Instrumentation;
import android.content.Context;
import android.support.test.InstrumentationRegistry;
import android.support.test.annotation.UiThreadTest;
import android.support.test.rule.UiThreadTestRule;

import com.evolved.automata.android.lisp.guibuilder.model.ALGB;
import com.evolved.automata.android.lisp.guibuilder.model.CodePage;
import com.evolved.automata.android.lisp.guibuilder.model.Page;
import com.evolved.automata.android.lisp.guibuilder.model.Workspace;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;

import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;

/**
 * Created by Evolved8 on 5/3/17.
 */

public class PageTests {


    String pageKey;


    @Rule
    public UiThreadTestRule mUIThreadTestRule = new UiThreadTestRule();



    @Test
    public void testALGBCreation()
    {
        String errorMessage = "Failed to create app";

        try
        {

            Instrumentation instrumentation =  InstrumentationRegistry.getInstrumentation();
            Context context = instrumentation.getTargetContext();

            ALGB app = new ALGB(context);

            errorMessage = "Failed to create default Workspace";

            Workspace workspace = app.getCurrentWorkspace();

            Assert.assertTrue(errorMessage, workspace != null);

            errorMessage = "failed to create page";

            Page page = workspace.getCurrentPage();
            Assert.assertTrue(errorMessage, page != null);

            String title = page.getTitle();
            errorMessage = "Failed to match page title: expected " + Page.DEFAULT_TITLE + " found " + title;

            Assert.assertTrue(errorMessage, Page.DEFAULT_TITLE.equals(title));

        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testDataAccessibility()
    {
        String errorMessage = "Failed to create ALGB";

        try
        {

            Instrumentation instrumentation =  InstrumentationRegistry.getInstrumentation();
            Context context = instrumentation.getTargetContext();


            ALGB app = new ALGB(context);


            errorMessage = "Failed to test for page existence";

            errorMessage = "failed to create page";
            //Assert.assertTrue(errorMessage, page != null);



        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @UiThreadTest
    @Test
    public void testSavedPages()
    {
        String errorMessage = "Failed to create app";

        try
        {

            Instrumentation instrumentation =  InstrumentationRegistry.getInstrumentation();
            Context context = instrumentation.getTargetContext();

            ALGB app = new ALGB(context),app2;
            app.deleteAllData();

            errorMessage = "Failed to create default Workspace";

            Workspace workspace = app.getCurrentWorkspace();

            Assert.assertTrue(errorMessage, workspace != null);

            errorMessage = "failed to create page";

            Page page = workspace.getCurrentPage();
            Assert.assertTrue(errorMessage, page != null);

            String title = page.getTitle();
            errorMessage = "Failed to match page title: expected " + Page.DEFAULT_TITLE + " found " + title;


            if (page.getPageType() == Page.PAGE_TYPE.CODE)
            {

                CodePage code = (CodePage)page;
                String data = "(+ 12 34) (* 12 (+ 12 12))";
                code.setExpr(data);
                app.save(true);

                app2 = new ALGB(context);

                workspace = app.getCurrentWorkspace();
                page = workspace.getCurrentPage();

                CodePage cp = (CodePage)page;

                String retrieved = cp.getExpr();
                Assert.assertTrue("Retrieved value shuyld be same as saved value", retrieved.equals(data));
            }



        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @UiThreadTest
    @Test
    public void testSavingLocalPageData()
    {
        String errorMessage = "Failed to create app";
        boolean resetWorkspaceDataP = false;
        boolean testingDataDefineP = false;
        String dataTestWorkspaceId;
        String testWorkspaceContext = "test";
        String testWorkspaceKey = "TEST-WORKSPACE";
        String testDataKey = "test-key";
        Value testPageData = NLispTools.makeValue("Test data");
        try
        {

            Instrumentation instrumentation =  InstrumentationRegistry.getInstrumentation();
            Context context = instrumentation.getTargetContext();
            Workspace w;
            ALGB app = new ALGB(context),app2;
            Page p;
            if (resetWorkspaceDataP && app.hasData(testWorkspaceKey, testWorkspaceContext))
            {

                dataTestWorkspaceId = app.getRawData(testWorkspaceKey, testWorkspaceContext);
                errorMessage = "Failed to retrieve prior test Workspace";
                w = app.getWorkspace(dataTestWorkspaceId);

                if (w == null)
                {
                    w = app.createNewWorkspace();
                    app.setRawData(testWorkspaceKey,testWorkspaceContext, w.getWorkspaceId());
                    p = w.getCurrentPage();
                    errorMessage = "Failed to create new test workspace";
                    Assert.assertTrue(errorMessage, app.getWorkspace(w.getWorkspaceId()) != null);
                }
                else
                {
                    p = w.getCurrentPage();
                    errorMessage = "Failed to get current test page";
                    Assert.assertTrue(errorMessage, p != null);
                    errorMessage = "Failed to delete current test page";
                    app.deletePage(p.getPageId());
                    Assert.assertTrue(errorMessage, app.retrievePage(p.getPageId()) == null);
                    p = w.getCurrentPage();

                }
                app.saveAll();


            }
            else if (app.hasRawData(testWorkspaceKey, testWorkspaceContext))
            {
                dataTestWorkspaceId = app.getRawData(testWorkspaceKey, testWorkspaceContext);
                errorMessage = "Failed to retrieve prior Workspace";
                w = app.getWorkspace(dataTestWorkspaceId);
                Assert.assertTrue(errorMessage, w != null);
                p = w.getCurrentPage();
            }
            else
            {
                errorMessage = "Failed to create default Workspace";
                w = app.createNewWorkspace();
                app.setRawData(testWorkspaceKey,testWorkspaceContext, w.getWorkspaceId());
                p = w.getCurrentPage();
                errorMessage = "Failed to save test workspace meta data";
                Assert.assertTrue(errorMessage, app.hasRawData(testWorkspaceKey,testWorkspaceContext));
                app.saveAll();
            }

            errorMessage = "Failed to create initial test page";
            Assert.assertTrue(errorMessage, p != null);

            if (testingDataDefineP)
            {
                errorMessage = "Failed to set page data";
                p.setPageData(testDataKey, testPageData);
                p.savePage();
            }

            errorMessage = "Failed to retrieve page data";
            Value prior = p.getPageData(testDataKey);

            Assert.assertTrue(errorMessage, p.hasPageData(testDataKey));

            errorMessage = "Failed to match page data type";
            Assert.assertTrue(errorMessage, prior.getType() ==  testPageData.getType());

            errorMessage = "Failed to match page data value";
            if (NLispTools.isNumericType(prior))
                Assert.assertTrue(errorMessage, prior.getFloatValue() == testPageData.getFloatValue());
            else
                Assert.assertTrue(errorMessage, prior.equals(testPageData));

        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }




    @Test
    public void testUIPages()
    {
        String errorMessage = "Failed to create app";

        try
        {

            Instrumentation instrumentation =  InstrumentationRegistry.getInstrumentation();
            Context context = instrumentation.getTargetContext();

            ALGB app = new ALGB(context),app2;
            app.deleteAllData();

            errorMessage = "Failed to create default Workspace";

            Workspace workspace = app.getCurrentWorkspace();

            Assert.assertTrue(errorMessage, workspace != null);

            errorMessage = "failed to create page";

            Page page = workspace.getCurrentPage();
            Assert.assertTrue(errorMessage, page != null);

            String title = page.getTitle();
            errorMessage = "Failed to match page title: expected " + Page.DEFAULT_TITLE + " found " + title;


            if (page.getPageType() == Page.PAGE_TYPE.CODE)
            {

                CodePage code = (CodePage)page;
                String data = getTestInput();
                code.setExpr(data);
                app.save(true);

                app2 = new ALGB(context);

                workspace = app2.getCurrentWorkspace();
                page = workspace.getCurrentPage();

                CodePage cp = (CodePage)page;

                String retrieved = cp.getExpr();
                Assert.assertTrue("Retrieved value shuyld be same as saved value", retrieved.equals(data));
            }



        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    private String getTestInput()
    {
        String largeFileName = "/com/evolved/automata/android/lisp/guibuilder/ui_test.lisp";

        InputStreamReader reader = null;
        InputStream istream = null;
        try
        {

            istream = this.getClass().getResourceAsStream(largeFileName);
            reader = new InputStreamReader(istream, Charset.forName("UTF-8"));
            StringBuilder input = new StringBuilder();

            int charValue;

            while ((charValue = reader.read()) != -1)
            {
                input.appendCodePoint(charValue);
            }

            return input.toString();


        }
        catch (Exception e)
        {

            e.printStackTrace();
            throw new RuntimeException(e);
        }
        finally
        {
            if (reader != null)
            {
                try
                {
                    reader.close();
                }
                catch (Exception e2)
                {
                    e2.printStackTrace();
                }
            }
        }
    }
}
