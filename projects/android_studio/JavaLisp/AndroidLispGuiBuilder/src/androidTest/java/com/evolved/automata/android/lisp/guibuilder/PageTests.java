package com.evolved.automata.android.lisp.guibuilder;

import android.app.Instrumentation;
import android.content.Context;
import android.support.test.InstrumentationRegistry;
import android.support.test.annotation.UiThreadTest;
import android.support.test.rule.UiThreadTestRule;

import com.evolved.automata.android.lisp.guibuilder.ALGB;
import com.evolved.automata.android.lisp.guibuilder.CodePage;
import com.evolved.automata.android.lisp.guibuilder.Page;
import com.evolved.automata.android.lisp.guibuilder.Workspace;

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
