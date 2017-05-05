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
        String errorMessage = "Failed to create context";

        try
        {

            String testDataContext = ":TESTING:";
            String testPageKey = "page 1";

            Instrumentation instrumentation =  InstrumentationRegistry.getInstrumentation();
            Context context = instrumentation.getTargetContext();

            Environment top = new Environment();
            ALGB manager = new ALGB(context);

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


}
