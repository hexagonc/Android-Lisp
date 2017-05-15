package com.evolved.automata.android.lisp.guibuilder;

import android.app.Activity;
import android.app.Fragment;
import android.app.FragmentTransaction;
import android.os.Bundle;
import android.os.Looper;
import android.support.annotation.Nullable;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;
import android.widget.Toast;

import com.evolved.automata.android.widgets.ShadowButton;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.lisp.editor.ParseNode;

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
    ShadowButton mToggleReadOnlyButton;
    ImageButton mClearSelection;

    CodePage mCodePage;

    LispContext mCodeContext;

    static final boolean PRINT_ON_MAINTHREAD_P = false;

    @Override
    public void onAttach(Activity activity)
    {
        super.onAttach(activity);

    }

    SimpleFunctionTemplate getPrintln()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)getPrintln();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);

                StringBuilder sBuilder = new StringBuilder();
                for (int i = 0;i<evaluatedArgs.length;i++)
                {
                    sBuilder.append((evaluatedArgs[i].isString())?evaluatedArgs[i].getString():evaluatedArgs[i].toString());
                }
                final String out = sBuilder.toString();


                if (PRINT_ON_MAINTHREAD_P && Looper.myLooper() != Looper.getMainLooper())
                {
                    Runnable post = new Runnable()
                    {
                        public void run()
                        {
                            mResultController.setResult(out, false);
                        }

                    };

                    mCodePage.getApplication().getMainhandler().post(post);
                }
                else
                    mResultController.setResult(out, false);


                return NLispTools.makeValue(out);
            }
        };
    }


    public void setCodePage(CodePage page)
    {
        mCodePage = page;
    }

    public void setLispContext(LispContext context)
    {
        mCodeContext = context;
        mCodeContext.getEnvironment().mapFunction("println", getPrintln());
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);

    }


    public void updatePage()
    {

        String text = mCodePage.getExpr();
        mEditorFragment.getEditorController().setText(text, mCodePage.getCursorPosition(), new Observer<CodeEditorFragment.StateChange>() {
            @Override
            public void onSubscribe(@NonNull Disposable d)
            {

            }

            @Override
            public void onNext(@NonNull CodeEditorFragment.StateChange stateChange)
            {

            }

            @Override
            public void onError(@NonNull Throwable e)
            {
                e.printStackTrace();
            }

            @Override
            public void onComplete()
            {

            }
        });
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState)
    {
        ViewGroup parent = (ViewGroup)inflater.inflate(R.layout.v2_code_page, container, false);
        mEvalButton = (ShadowButton)parent.findViewById(R.id.v2_but_eval);
        mSaveButton = (ShadowButton)parent.findViewById(R.id.v2_but_save_page);
        mToggleReadOnlyButton = (ShadowButton)parent.findViewById(R.id.v2_but_toggle_readonly);
        mClearSelection = (ImageButton) parent.findViewById(R.id.v2_but_clear_selection);


        mToggleReadOnlyButton.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View v)
            {
                if (mEditorController.isReadOnlyMode())
                {
                    mEditorController.disableReadOnlyMode(CodePageFragment.this);
                }
                else
                    mEditorController.enableReadOnlyMode(CodePageFragment.this);
            }
        });


        mEvalButton.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View v)
            {
                evaluateSelection();
            }
        });


        mSaveButton.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View v)
            {
                mCodePage.savePage();
            }
        });

        mClearSelection.setOnClickListener(new View.OnClickListener()
        {

            @Override
            public void onClick(View v)
            {

            }
        });


        mEditorFragment = new CodeEditorFragment();
        mResultFragment = new LispResultFragment();

        FragmentTransaction transaction = getChildFragmentManager().beginTransaction();
        transaction.add(R.id.v2_result_fragment, mResultFragment);
        transaction.add(R.id.v2_frag_code_editor, mEditorFragment);
        transaction.commit();

        Log.i("-+*+--+*+--+*+-", "CodePage onCreateView - inflated layout and set buttons. started child fragment transactions");
        return parent;
    }

    @Override
    public void onViewCreated(View view, @Nullable Bundle savedInstanceState)
    {
        super.onViewCreated(view, savedInstanceState);

        Log.i("-+*+--+*+--+*+-", "CodePage onViewCreated - started CodeEditor and Result Fragment transactions");
    }

    @Override
    public void onStart()
    {
        super.onStart();
        Log.i("-+*+--+*+--+*+-", "CodePage onStart");
        mEditorController = mEditorFragment.getEditorController();
        mEditorController.setStateObserver(this);
        mResultController = mResultFragment.getController();


    }

    private void updateToggleButton()
    {
        boolean currentReadonlyStatus = mEditorController.isReadOnlyMode();
        String label = "Read-only: " + (currentReadonlyStatus);
        mToggleReadOnlyButton.setText(label);
    }

    private void evaluateSelection()
    {
        ParseNode selection = mEditorController.getSelection();
        if (selection != null)
        {
            LispContext context = mCodeContext;
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

    private void evaluateAll()
    {
        String text = mEditorController.getText();

        if (text != null)
        {
            LispContext context = mCodeContext;
            context.evaluateExpression(text, new Observer<Value>() {
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
            Toast.makeText(getActivity(), "No expression to evaluate", Toast.LENGTH_LONG).show();
    }


    @Override
    public void onResume()
    {
        super.onResume();

        Log.i("-+*+--+*+--+*+-", "CodePage onResume");
    }

    @Override
    public void onPause()
    {
        super.onPause();
        Log.i("-+*+--+*+--+*+-", "CodePage onPause");
    }

    @Override
    public void onStop()
    {
        super.onStop();
        Log.i("-+*+--+*+--+*+-", "CodePage onStop");
    }



    @Override
    public void onSubscribe(@NonNull Disposable d)
    {

    }

    @Override
    public void onNext(@NonNull CodeEditorFragment.StateChange stateChange)
    {

        String code = stateChange._text;
        if (code != null)
        {
            mCodePage.setExpr(code);
            mCodePage.setCursorPosition(stateChange._cursorPos);
        }

        if (stateChange._changeType.contains(CodeEditorFragment.CHANGE_TYPE.READONLY))
        {
            updateToggleButton();
            mCodePage.setReadOnlyMode(stateChange._readOnlyModeP);
        }

        if (stateChange._changeType.contains(CodeEditorFragment.CHANGE_TYPE.VISIBILITY))
        {
            if (stateChange.isVisible)
            {
                String text = mCodePage.getExpr();
                mEditorController.setText(text, Math.max(0, mCodePage.getCursorPosition()) , this);
                if (mCodePage.isReadOnlyEnabled())
                {
                    mEditorController.enableReadOnlyMode(this);
                }
                else
                    mEditorController.disableReadOnlyMode(this);
            }
        }

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
}
