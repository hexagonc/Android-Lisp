package com.evolved.automata.android.lisp.guibuilder;

import android.app.Activity;
import android.app.Fragment;
import android.app.FragmentTransaction;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;
import android.widget.Toast;

import com.evolved.automata.android.widgets.ShadowButton;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.lisp.editor.ParseNode;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;

import io.reactivex.Observer;
import io.reactivex.annotations.NonNull;
import io.reactivex.disposables.Disposable;

/**
 * Created by Evolved8 on 5/5/17.
 */

public class CodePageFragment extends Fragment implements  Observer<CodeEditorFragment.StateChange> {



    CodeEditorFragment mEditorFragment;
    CodeEditorFragment.EditorController mEditorController;
    LispResultFragment mResultFragment;
    LispResultFragment.Controller mResultController;


    ShadowButton mEvalButton;
    ShadowButton mSaveButton;
    ImageButton mClearSelection;

    CodePage mCodePage;

    String mTestData;

    @Override
    public void onAttach(Activity activity)
    {
        super.onAttach(activity);

    }


    public void setCodePage(CodePage page)
    {
        mCodePage = page;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        mEditorFragment = new CodeEditorFragment();
        mResultFragment = new LispResultFragment();
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState)
    {
        ViewGroup parent = (ViewGroup)inflater.inflate(R.layout.v2_code_page, container, false);
        mEvalButton = (ShadowButton)parent.findViewById(R.id.v2_but_eval);
        mSaveButton = (ShadowButton)parent.findViewById(R.id.v2_but_save_page);
        mClearSelection = (ImageButton) parent.findViewById(R.id.v2_but_clear_selection);

        mEvalButton.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View v)
            {
                evaluateSelection();
            }
        });

        mClearSelection.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View v)
            {

            }
        });


        return parent;
    }

    @Override
    public void onStart()
    {
        super.onStart();
        FragmentTransaction transaction = getActivity().getFragmentManager().beginTransaction();
        transaction.add(R.id.v2_result_fragment, mResultFragment);
        transaction.add(R.id.v2_frag_code_editor, mEditorFragment);
        transaction.commit();

        mEditorController = mEditorFragment.getEditorController();

        mResultController = mResultFragment.getController();

        loadTestInput();


    }

    private void evaluateSelection()
    {
        ParseNode selection = mEditorController.getSelection();
        if (selection != null)
        {
            LispContext context = mCodePage.getBasePageLispContext();
            context.evaluateExpression(selection.getValue(), new Observer<Value>() {
                @Override
                public void onSubscribe(@NonNull Disposable d)
                {

                }

                @Override
                public void onNext(@NonNull Value value)
                {
                    mResultController.setResult(value.toString(), false);
                }

                @Override
                public void onError(@NonNull Throwable e)
                {
                    mResultController.setResult(e.toString(), false);
                }

                @Override
                public void onComplete()
                {

                }
            });

        }
        else
            Toast.makeText(getActivity(), "No expression selected", Toast.LENGTH_LONG).show();
    }


    @Override
    public void onResume()
    {
        super.onResume();
        mEditorController.setText(mTestData, 0, this);
    }

    @Override
    public void onPause()
    {
        super.onPause();
    }

    @Override
    public void onStop()
    {
        super.onStop();
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

            mTestData = input.toString();


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

    @Override
    public void onSubscribe(@NonNull Disposable d)
    {

    }

    @Override
    public void onNext(@NonNull CodeEditorFragment.StateChange stateChange)
    {
        System.out.println("New state: " + stateChange._changeType.toString());
    }

    @Override
    public void onError(@NonNull Throwable e)
    {

    }

    @Override
    public void onComplete()
    {

    }
}
