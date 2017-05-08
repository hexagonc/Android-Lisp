package com.evolved.automata.android.lisp.guibuilder;

import android.app.Instrumentation;
import android.content.Context;
import android.support.test.InstrumentationRegistry;

import com.evolved.automata.lisp.Environment;

import org.junit.Assert;
import org.junit.Test;

/**
 * Created by Evolved8 on 5/3/17.
 */

public class PageTests {


    String pageKey;


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



}
