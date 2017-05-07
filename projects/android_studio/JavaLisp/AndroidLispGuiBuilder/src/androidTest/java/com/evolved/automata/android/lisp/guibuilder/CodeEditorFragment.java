package com.evolved.automata.android.lisp.guibuilder;

import android.app.Fragment;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageButton;

import com.evolved.automata.android.lisp.guibuilder.v2.LispCodeEditorParseContext;
import com.evolved.automata.android.lisp.guibuilder.v2.LispEditText;
import com.evolved.automata.lisp.editor.ParseNode;
import com.evolved.automata.lisp.editor.TopParseNode;
import com.evolved.automata.lisp.editor.WordCompletor;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;

import io.reactivex.Observer;
import io.reactivex.annotations.NonNull;
import io.reactivex.disposables.Disposable;
import io.reactivex.subjects.PublishSubject;
import io.reactivex.subjects.Subject;

/**
 * Created by Evolved8 on 4/26/17.
 */

public class CodeEditorFragment extends Fragment {

    public interface Controller {

    }


    public static class EditorState
    {
        public int _cursorPos;
        public String _text;
        public ParseNode _selection;
    }

    PublishSubject<EditorState> mStateObserver;



    EditorState mCurrentState;

    WordCompletor mCodeCompletor;
    LispCodeEditorParseContext mCodeEditorParseContext;

    LispEditText mCodeView;
    ImageButton mParentButton;
    ImageButton mChildButton;

    ImageButton mPrevSiblingButton;
    ImageButton mCursorLeftButton;
    ImageButton mCursorRightButton;

    ImageButton mNextSiblingButton;

    LispEditText.ControlInterface mController;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        mCodeCompletor = new WordCompletor();

        mCodeEditorParseContext = new LispCodeEditorParseContext();

        mCurrentState = new EditorState();

        mStateObserver = PublishSubject.create();


    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState)
    {

        ViewGroup top = (ViewGroup)inflater.inflate(R.layout.v2_code_editing, container, false);
        mCodeView = (LispEditText)top.findViewById(R.id.v2_edit_main_code);
        mCodeView.setHorizontallyScrolling(true);
        mController = mCodeView.getControlInterface();


        mParentButton = (ImageButton)top.findViewById(R.id.v2_up_to_parent);
        mChildButton = (ImageButton)top.findViewById(R.id.v2_down_to_child);

        mPrevSiblingButton = (ImageButton)top.findViewById(R.id.v2_prev_sibling);
        mCursorLeftButton = (ImageButton)top.findViewById(R.id.v2_cursor_left);
        mCursorRightButton = (ImageButton)top.findViewById(R.id.v2_cursor_right);
        mNextSiblingButton = (ImageButton)top.findViewById(R.id.v2_next_sibling);

        mParentButton.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View v)
            {
                mController.moveToParent();
            }
        });

        mChildButton.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View v)
            {
                mController.moveToFirstChild();
            }
        });

        mPrevSiblingButton.setOnClickListener(new View.OnClickListener()
        {
            @Override
            public void onClick(View v)
            {
                mController.moveToPrevSibling(true);
            }
        });


        mCursorLeftButton.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View v)
            {
                mController.moveCursorLeft();
            }
        });

        mCursorRightButton.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View v)
            {
                mController.moveCursorRight();
            }
        });


        mNextSiblingButton.setOnClickListener(new View.OnClickListener()
        {
            @Override
            public void onClick(View v)
            {
                mController.moveToNextSibling(true);
            }
        });


        mController.setStateListener(new LispEditText.StateListener()
        {

            @Override
            public void onCursorChange(int cursorPos)
            {
                Log.d("<><<><>><><>", "New cursor: " + cursorPos);

            }

            @Override
            public void onTextChange(String newText, int cursorPos)
            {
                // (<>o<>)
                Log.d("<><<><>><><>", "Text change: cursor: " + cursorPos + "\n text" + newText);
            }

            @Override
            public void onSelectionChanged(ParseNode newNode)
            {
                Log.d("<><<><>><><>", "New selection: " + newNode.toString());
            }
        });

        mController.setReadOnly();
        loadTestInput();
        return top;
    }

    private void loadTestInput()
    {
        String largeFileName = "/com/evolved/automata/android/lisp/guibuilder/generated.lisp";

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


            mController.setText(input.toString());


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
