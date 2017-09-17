package com.evolved.automata.android.lisp.guibuilder.ui;

import android.app.Activity;


import android.os.Bundle;
import android.os.Looper;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentTransaction;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Toast;

import com.evolved.automata.android.lisp.guibuilder.ALGBBaseActivity;
import com.evolved.automata.android.lisp.guibuilder.LispEditText;
import com.evolved.automata.android.lisp.guibuilder.LispResultFragment;
import com.evolved.automata.android.lisp.guibuilder.MenuHelper;
import com.evolved.automata.android.lisp.guibuilder.R;
import com.evolved.automata.android.lisp.guibuilder.Tools;
import com.evolved.automata.android.lisp.guibuilder.model.CodePage;
import com.evolved.automata.android.lisp.guibuilder.model.LispContext;
import com.evolved.automata.android.widgets.ShadowButton;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.lisp.editor.ParseNode;
import com.evolved.automata.lisp.editor.TopParseNode;

import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;

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


    CodePage mCodePage;

    LispContext mCodeContext;

    static final boolean PRINT_ON_MAINTHREAD_P = false;


    @Override
    public void onAttach(Activity activity)
    {
        super.onAttach(activity);

    }

    @Override
    public void onPrepareOptionsMenu(Menu menu)
    {
        MenuHelper.updateMenuItemDisplay(menu, this);
        //super.onPrepareOptionsMenu(menu);
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
        mCodePage.assertTopParseNodeIsInValid();
        TopParseNode current = mCodePage.getTopParseNode();

        mEditorFragment.getEditorController().setText(text, mCodePage.getCursorPosition(), current, new Observer<CodeEditorFragment.StateChange>() {
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
        if (current == null)
        {
            mCodePage.requestTopParseNode(new Observer<TopParseNode>()
            {

                @Override
                public void onSubscribe(@NonNull Disposable d)
                {

                }

                @Override
                public void onNext(@NonNull TopParseNode topParseNode)
                {
                    mEditorFragment.getEditorController().setTopParseNode(topParseNode);
                }

                @Override
                public void onError(@NonNull Throwable e)
                {

                }

                @Override
                public void onComplete()
                {

                }
            });
        }
    }




    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState)
    {
        ViewGroup parent = (ViewGroup)inflater.inflate(R.layout.v2_code_page, container, false);



        mEvalButton = (ShadowButton)parent.findViewById(R.id.v2_but_eval);
        mSaveButton = (ShadowButton)parent.findViewById(R.id.v2_but_save_page);
        mToggleReadOnlyButton = (ShadowButton)parent.findViewById(R.id.v2_but_toggle_readonly);

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
                {
                    mEditorController.enableReadOnlyMode(CodePageFragment.this);
                    TopParseNode current = mCodePage.getTopParseNode();
                    if (current == null)
                        updateTopParseNode();
                    else
                    {
                        mEditorFragment.getEditorController().setTopParseNode(current);
                    }

                }
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




        mEditorFragment = new CodeEditorFragment();
        mResultFragment = new LispResultFragment();


        FragmentTransaction transaction = getChildFragmentManager().beginTransaction();
        transaction.add(R.id.v2_result_fragment, mResultFragment);
        transaction.add(R.id.v2_frag_code_editor, mEditorFragment);
        transaction.commit();




        Log.i("-+*+--+*+--+*+-", "CodePage onCreateView - inflated layout and set buttons. started child fragment transactions");
        return parent;
    }

    @Subscribe(threadMode = ThreadMode.MAIN)
    public void onKeyboardVisibilityChange(ALGBBaseActivity.KeyboardVisibility event)
    {
        if (event.isVisible())
        {
            FragmentTransaction trans = getChildFragmentManager().beginTransaction();
            trans.hide(mResultFragment);
            trans.commit();
        }
        else
        {
            FragmentTransaction trans = getChildFragmentManager().beginTransaction();
            trans.show(mResultFragment);
            trans.commit();
        }
    }

    
    @Subscribe(threadMode = ThreadMode.MAIN)
    public void onCodeEditorHeightEvent(LispEditText.HeightEvent heightEvent)
    {
        Log.d("____change___", "" + System.currentTimeMillis() + " heigh " + heightEvent.isLessThanMinimumEditableHeight());
        if (heightEvent.isLessThanMinimumEditableHeight())
        {
            FragmentTransaction trans = getChildFragmentManager().beginTransaction();
            trans.hide(mResultFragment);
            trans.commit();
        }
        else
        {
            FragmentTransaction trans = getChildFragmentManager().beginTransaction();
            trans.show(mResultFragment);
            trans.commit();
        }
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
        mResultController.setResults(mCodePage.getResultHistory(), true);

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
                    try
                    {

                        mResultController.setResult(value.toString(), false);
                        mCodePage.setResultHistory(mResultController.getResults());
                    }
                    catch (Exception e)
                    {
                        CodePageFragment.this.onError(e);
                    }


                }

                @Override
                public void onError(@NonNull Throwable e)
                {
                    mResultController.setResult(e.toString(), false);
                    mCodePage.setResultHistory(mResultController.getResults());
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
        Tools.registerEventHandler(this);
        Log.i("-+*+--+*+--+*+-", "CodePage onResume");
    }

    @Override
    public void onPause()
    {
        Tools.unRegisterEventHandler(this);
        super.onPause();
        Log.i("-+*+--+*+--+*+-", "CodePage onPause");
    }

    @Override
    public void onStop()
    {
        mCodePage.setResultHistory(mResultController.getResults());
        super.onStop();
        Log.i("-+*+--+*+--+*+-", "CodePage onStop");

    }



    @Override
    public void onSubscribe(@NonNull Disposable d)
    {

    }



    // ~o) (o~~o) (o~ ~o) (o~ ~o) (o~ ~o) (o~ ~o) (o~ ~o) (o~ ~o) (o~
    // This is where the CodeEditorFragment notifies the CodePageFragment, and hence the CodePage
    // that there have been changes to the LispEditText.  If there have been text changes and
    // the LispEditText is in read-only mode then it recomputes the cached TopParseNode in the
    // CodePage immediately.  Otherwise, it just invalidates the TopParseNode.  Upon switching from
    // write mode to read-only mode, it checks if the TopParseNode is valid and if not, recomputes it
    // then.
    // ~o) (o~~o) (o~ ~o) (o~ ~o) (o~ ~o) (o~ ~o) (o~ ~o) (o~ ~o) (o~
    @Override
    public void onNext(@NonNull CodeEditorFragment.StateChange stateChange)
    {
        boolean updateTop = false;
        if (stateChange._changeType.contains(CodeEditorFragment.CHANGE_TYPE.READONLY))
        {
            updateToggleButton();
            mCodePage.setReadOnlyMode(stateChange._readOnlyModeP);
            updateTop = mCodePage.isReadOnlyEnabled() && !mCodePage.isTopParseNodeValid();
        }
        updateTop = updateTop || mCodePage.isReadOnlyEnabled() && !mCodePage.isTopParseNodeValid();
        if (stateChange._changeType.contains(CodeEditorFragment.CHANGE_TYPE.TEXT))
        {
            String code = stateChange._text;
            // TODO - need to be able to avoid testing for equality.  This would be done by better handling circular callback sequences
            if (code != null && !code.equals(mCodePage.getExpr()))
            {
                mCodePage.setExpr(code);
                mCodePage.assertTopParseNodeIsInValid();
                updateTop = mCodePage.isReadOnlyEnabled();
            }
        }

        if (stateChange._changeType.contains(CodeEditorFragment.CHANGE_TYPE.CURSOR))
        {
            mCodePage.setCursorPosition(stateChange._cursorPos);
        }

        if (stateChange._changeType.contains(CodeEditorFragment.CHANGE_TYPE.VISIBILITY))
        {
            if (stateChange.isVisible)
            {
                String text = mCodePage.getExpr();

                if (mCodePage.isReadOnlyEnabled())
                {
                    mEditorController.enableReadOnlyMode(this);
                    TopParseNode current = mCodePage.getTopParseNode();
                    if (current == null)
                    {
                        mEditorController.setText(text, Math.max(0, mCodePage.getCursorPosition()), null , this);
                        updateTopParseNode();
                    }
                    else
                    {

                        mEditorController.setText(text, Math.max(0, mCodePage.getCursorPosition()), current , this);
                    }
                    updateTop = false;
                }
                else
                {
                    mEditorController.setText(text, Math.max(0, mCodePage.getCursorPosition()), null , this);
                    mEditorController.disableReadOnlyMode(this);
                }
            }
        }

        if (updateTop)
        {
            updateTopParseNode();
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

    private void updateTopParseNode()
    {
        mCodePage.requestTopParseNode(new Observer<TopParseNode>()
        {

            @Override
            public void onSubscribe(@NonNull Disposable d)
            {

            }

            @Override
            public void onNext(@NonNull TopParseNode topParseNode)
            {
                mEditorFragment.getEditorController().setTopParseNode(topParseNode);
            }

            @Override
            public void onError(@NonNull Throwable e)
            {

            }

            @Override
            public void onComplete()
            {

            }
        });
    }
}
