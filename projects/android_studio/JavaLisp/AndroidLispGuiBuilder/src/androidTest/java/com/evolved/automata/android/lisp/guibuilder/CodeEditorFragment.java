package com.evolved.automata.android.lisp.guibuilder;

import android.app.Fragment;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;

import com.evolved.automata.android.lisp.guibuilder.v2.LispCodeEditorParseContext;
import com.evolved.automata.android.lisp.guibuilder.v2.LispEditText;
import com.evolved.automata.lisp.editor.TopParseNode;
import com.evolved.automata.lisp.editor.WordCompletor;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;

/**
 * Created by Evolved8 on 4/26/17.
 */

public class CodeEditorFragment extends Fragment {



    WordCompletor mCodeCompletor;
    LispCodeEditorParseContext mCodeEditorParseContext;

    LispEditText mCodeView;
    Button mParentButton;
    Button mChildButton;
    Button mPrevSiblingButton;
    Button mNextSiblingButton;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        mCodeCompletor = new WordCompletor();

        mCodeEditorParseContext = new LispCodeEditorParseContext();
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState)
    {

        ViewGroup top = (ViewGroup)inflater.inflate(R.layout.v2_code_editing, container, false);
        mCodeView = (LispEditText)top.findViewById(R.id.v2_edit_main_code);
        mCodeView.setHorizontallyScrolling(true);

        loadTestInput();

        mParentButton = (Button)top.findViewById(R.id.v2_button_up);
        mNextSiblingButton = (Button)top.findViewById(R.id.v2_but_next);
        mPrevSiblingButton = (Button)top.findViewById(R.id.v2_but_prev);
        mChildButton = (Button)top.findViewById(R.id.v2_but_down);

        mParentButton.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View v)
            {
                mCodeView.moveToParent();
            }
        });

        mNextSiblingButton.setOnClickListener(new View.OnClickListener()
        {
            @Override
            public void onClick(View v)
            {
                mCodeView.moveToNextSiblingToken(true);
            }
        });

        mPrevSiblingButton.setOnClickListener(new View.OnClickListener()
        {
            @Override
            public void onClick(View v)
            {
                mCodeView.moveToPreviousSiblingToken(true);
            }
        });

        mChildButton.setOnClickListener(new View.OnClickListener()
        {
            @Override
            public void onClick(View v)
            {
                mCodeView.moveToFirstChildToken();
            }
        });

        return top;
    }

    private void loadTestInput()
    {
        String largeFileName = "/com/evolved/automata/android/lisp/guibuilder/generated.lisp";
        String errorMessage = "Failed to open large test file";
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


            mCodeView.setText(input.toString());

        }
        catch (Exception e)
        {

            e.printStackTrace();

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
