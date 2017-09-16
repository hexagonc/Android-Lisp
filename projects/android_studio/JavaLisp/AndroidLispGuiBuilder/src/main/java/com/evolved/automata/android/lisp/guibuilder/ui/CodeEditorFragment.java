package com.evolved.automata.android.lisp.guibuilder.ui;

import android.app.Activity;

import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v7.widget.Toolbar;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;

import com.evolved.automata.android.lisp.guibuilder.LispCodeEditorParseContext;
import com.evolved.automata.android.lisp.guibuilder.LispEditText;
import com.evolved.automata.android.lisp.guibuilder.R;
import com.evolved.automata.lisp.editor.ParseNode;
import com.evolved.automata.lisp.editor.TopParseNode;
import com.evolved.automata.lisp.editor.WordCompletor;

import java.util.HashSet;

import io.reactivex.Observable;
import io.reactivex.ObservableEmitter;
import io.reactivex.ObservableOnSubscribe;
import io.reactivex.Observer;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.annotations.NonNull;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Action;
import io.reactivex.functions.Consumer;
import io.reactivex.subjects.PublishSubject;

/**
 * Created by Evolved8 on 4/26/17.
 */

public class CodeEditorFragment extends Fragment {


    public static final String NOT_PRESENT_EXCEPTION_MESSAGE = "code editor not present";

    public enum CHANGE_TYPE
    {
        CURSOR, TEXT, SELECTION, READONLY, VISIBILITY, COMMAND_COMPLETE
    }

    public static class StateChange
    {
        public int _cursorPos;
        public String _text;
        public ParseNode _selection;
        public boolean _readOnlyModeP;

        HashSet<CHANGE_TYPE> _changeType = new HashSet<CHANGE_TYPE>();
        boolean isVisible = true;

    }

    public interface EditorController
    {
        void setTopParseNode(TopParseNode topNode);
        Disposable setStateObserver(Observer<StateChange> observer);
        String getText();
        Disposable setText(String text, int cursor, TopParseNode top, Observer<StateChange> observer);
        Disposable enableReadOnlyMode(Observer<StateChange> observer);
        Disposable disableReadOnlyMode(Observer<StateChange> observer);
        int getCursorPosition();
        ParseNode getSelection();
        boolean isReadOnlyMode();
        boolean isPresentP();

    }


    PublishSubject<StateChange> mStateObserver;
    Observable<StateChange> mExternalObservable;


    boolean isPresentP = false;
    StateChange mLastStateChange;

    WordCompletor mCodeCompletor;
    LispCodeEditorParseContext mCodeEditorParseContext;

    LispEditText mCodeView;

    LispEditText.ControlInterface mController;



    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        mCodeCompletor = new WordCompletor();

        mCodeEditorParseContext = new LispCodeEditorParseContext();

        if (mStateObserver != null)
        {
            Log.i("-+*+--+*+--+*+-", "CodeEditorFragment onCreate with existing state observer");
        }
        else
            Log.i("-+*+--+*+--+*+-", "CodeEditorFragment onCreate WITHOUT existing state observer.  State observables created for first time");

        mLastStateChange = new StateChange();

        mStateObserver = PublishSubject.create();

        mExternalObservable = mStateObserver.observeOn(AndroidSchedulers.mainThread());


    }


    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
    {

        ViewGroup top = (ViewGroup)inflater.inflate(R.layout.v2_code_editing, container, false);

        Toolbar toolbar = (Toolbar)top.findViewById(R.id.toolbar);
        toolbar.inflateMenu(R.menu.code_editor_menu);

        toolbar.setOnMenuItemClickListener(new Toolbar.OnMenuItemClickListener() {
            @Override
            public boolean onMenuItemClick(MenuItem item)
            {
                switch (item.getItemId())
                {
                    case R.id.menu_cursor_left:
                    {
                        mController.moveCursorLeft();
                    }
                    break;
                    case R.id.menu_cursor_right:
                    {
                        mController.moveCursorRight();
                    }
                    break;
                    case R.id.menu_to_parent:
                    {
                        mController.moveToParent();
                    }
                    break;
                    case R.id.menu_to_child:
                    {
                        mController.moveToFirstChild();
                    }
                    break;
                    case R.id.menu_to_prev_sibling:
                    {
                        mController.moveToPrevSibling(true);
                    }
                    break;
                    case R.id.menu_to_next_sibling:
                    {
                        mController.moveToNextSibling(true);
                    }
                    break;
                    case R.id.menu_clear:
                    {

                    }
                    break;
                    case R.id.menu_undo:
                    {

                    }
                    break;
                    case R.id.menu_copy:
                    {

                    }
                    break;
                    case R.id.menu_cut:
                    {

                    }
                    break;
                    case R.id.menu_paste:
                    {

                    }
                    break;

                }
                return true;
            }
        });

        mCodeView = (LispEditText)top.findViewById(R.id.v2_edit_main_code);
        mCodeView.setHorizontallyScrolling(true);
        mController = mCodeView.getControlInterface();




        mController.setStateListener(new LispEditText.StateListener()
        {

            @Override
            public void onCursorChange(int cursorPos)
            {
                mLastStateChange._cursorPos = cursorPos;
                mLastStateChange._changeType = new HashSet<CHANGE_TYPE>()
                {
                    {
                        add(CHANGE_TYPE.CURSOR);
                    }
                };

                mStateObserver.onNext(mLastStateChange);
            }

            @Override
            public void onTextChange(String newText, int cursorPos)
            {
                final boolean cursorChangedP = mLastStateChange._cursorPos != cursorPos;
                mLastStateChange._cursorPos = cursorPos;
                mLastStateChange._text = newText;

                mLastStateChange._changeType = new HashSet<CHANGE_TYPE>()
                {
                    {
                        if (cursorChangedP)
                            add(CHANGE_TYPE.CURSOR);

                        add(CHANGE_TYPE.TEXT);
                    }
                };

                mStateObserver.onNext(mLastStateChange);
            }

            @Override
            public void onSelectionChanged(ParseNode newNode)
            {

                mLastStateChange._selection = newNode;
                mLastStateChange._changeType = new HashSet<CHANGE_TYPE>()
                {
                    {

                        add(CHANGE_TYPE.SELECTION);
                    }
                };

                mStateObserver.onNext(mLastStateChange);
            }

            @Override
            public void onReadOnlyStateChanged(boolean isReadOnly)
            {
                mLastStateChange._readOnlyModeP = isReadOnly;
                mLastStateChange._changeType = new HashSet<CHANGE_TYPE>()
                {
                    {

                        add(CHANGE_TYPE.READONLY);
                    }
                };

                mStateObserver.onNext(mLastStateChange);
            }
        });


        // Hack to handle case where mCodeView height is incorrect when displaying softkeyboard
        // after moving from Render screen of another page
        mCodeView.setReadOnlyState(true);
        Log.i("-+*+--+*+--+*+-", "CodeEditorFragment onCreateView.  Layout inflated and widgets bound");

        return top;
    }

    @Override
    public void onStart()
    {
        super.onStart();
        Log.i("-+*+--+*+--+*+-", "CodeEditorFragment onStart.  View created and attached to hierarchy");

    }

    @Override
    public void onResume()
    {
        super.onResume();
        mLastStateChange.isVisible = true;
        isPresentP = true;
        mLastStateChange._changeType = new HashSet<CHANGE_TYPE>()
        {
            {

                add(CHANGE_TYPE.VISIBILITY);
            }
        };
        mStateObserver.onNext(mLastStateChange);
        Log.i("-+*+--+*+--+*+-", "CodeEditorFragment onResume.  Fragment is visible and can receive messages");
    }

    @Override
    public void onPause()
    {

        mLastStateChange.isVisible = false;
        mLastStateChange._changeType = new HashSet<CHANGE_TYPE>()
        {
            {

                add(CHANGE_TYPE.VISIBILITY);
            }
        };

        isPresentP = false;
        mStateObserver.onNext(mLastStateChange);
        Log.i("-+*+--+*+--+*+-", "CodeEditorFragment onPause.  CodeEditor not visible and should not receive messages");
        super.onPause();
    }

    @Override
    public void onStop()
    {

        Log.i("-+*+--+*+--+*+-", "CodeEditorFragment onStop.  OnStart will be called before can display again");
        super.onStop();
    }

    @Override
    public void onDestroyView()
    {
        Log.i("-+*+--+*+--+*+-", "CodeEditorFragment onDestroyView");
        super.onDestroyView();

    }

    @Override
    public void onDestroy()
    {
        Log.i("-+*+--+*+--+*+-", "CodeEditorFragment onDestroy");
        super.onDestroy();
    }

    @Override
    public void onDetach()
    {
        Log.i("-+*+--+*+--+*+-", "CodeEditorFragment onDetach.  Last callback");
        super.onDetach();


    }

    @Override
    public void onAttach(Activity activity)
    {
        super.onAttach(activity);
        Log.i("-+*+--+*+--+*+-", "CodeEditorFragment onAttach.  Very first callback");

    }



    public EditorController getEditorController()
    {

        return new EditorController() {

            @Override
            public void setTopParseNode(TopParseNode topNode)
            {
                mController.setTopParseNode(topNode);
            }


            @Override
            public Disposable setStateObserver(final Observer<StateChange> observer)
            {

                final Consumer<StateChange> resultConsumer = new Consumer<StateChange>() {
                    @Override
                    public void accept(@NonNull StateChange stateChange) throws Exception
                    {
                        observer.onNext(stateChange);
                    }
                };

                final Consumer<? super Throwable> errorConsumer = new Consumer<Throwable>() {
                    @Override
                    public void accept(@NonNull Throwable throwable) throws Exception
                    {
                        observer.onError(throwable);
                    }
                };

                final Action onComplete = new Action() {

                    @Override
                    public void run() throws Exception
                    {
                        observer.onComplete();
                    }
                };

                return mStateObserver.subscribe(resultConsumer, errorConsumer, onComplete);
            }

            @Override
            public String getText()
            {
                if (isPresentP)
                    return mController.getText();
                else
                    throw new IllegalStateException(NOT_PRESENT_EXCEPTION_MESSAGE);
            }

            public Disposable setText(final String text, final int cursor, final TopParseNode top, final Observer<StateChange> observer)
            {
                if (!isPresentP)
                    throw new IllegalStateException(NOT_PRESENT_EXCEPTION_MESSAGE);

                final Consumer<StateChange> resultConsumer = new Consumer<StateChange>() {
                    @Override
                    public void accept(@NonNull StateChange stateChange) throws Exception
                    {
                        observer.onNext(stateChange);
                    }
                };

                final Consumer<? super Throwable> errorConsumer = new Consumer<Throwable>() {
                    @Override
                    public void accept(@NonNull Throwable throwable) throws Exception
                    {
                        observer.onError(throwable);
                    }
                };

                final Action onComplete = new Action() {

                    @Override
                    public void run() throws Exception
                    {
                        observer.onComplete();
                    }
                };

                Observable<StateChange> worker = Observable.create(new ObservableOnSubscribe<StateChange>() {
                    @Override
                    public void subscribe(@NonNull ObservableEmitter<StateChange> subscriber) throws Exception
                    {
                        if (!subscriber.isDisposed())
                        {
                            try
                            {
                                mController.setText(text, cursor, top);
                                StateChange change = new StateChange();
                                change._text = mController.getText();
                                change._cursorPos = cursor;
                                change._readOnlyModeP = mController.isReadOnlyMode();
                                change._changeType = new HashSet<CHANGE_TYPE>()
                                {
                                    {
                                        add(CHANGE_TYPE.COMMAND_COMPLETE);
                                    }
                                };
                                subscriber.onNext(change);
                            }
                            catch (Exception e)
                            {
                                subscriber.onError(e);
                            }
                        }
                    }
                }).subscribeOn(AndroidSchedulers.mainThread()).observeOn(AndroidSchedulers.mainThread());

                return worker.subscribe(resultConsumer, errorConsumer, onComplete);

            }

            @Override
            public Disposable enableReadOnlyMode(final Observer<StateChange> observer)
            {
                if (!isPresentP)
                    throw new IllegalStateException(NOT_PRESENT_EXCEPTION_MESSAGE);

                final Consumer<StateChange> resultConsumer = new Consumer<StateChange>() {
                    @Override
                    public void accept(@NonNull StateChange stateChange) throws Exception
                    {
                        observer.onNext(stateChange);
                    }
                };

                final Consumer<? super Throwable> errorConsumer = new Consumer<Throwable>() {
                    @Override
                    public void accept(@NonNull Throwable throwable) throws Exception
                    {
                        observer.onError(throwable);
                    }
                };

                final Action onComplete = new Action() {

                    @Override
                    public void run() throws Exception
                    {
                        observer.onComplete();
                    }
                };

                Observable<StateChange> worker = Observable.create(new ObservableOnSubscribe<StateChange>() {
                    @Override
                    public void subscribe(@NonNull ObservableEmitter<StateChange> subscriber) throws Exception
                    {
                        if (!subscriber.isDisposed())
                        {
                            try
                            {
                                mController.setReadOnly();
                                StateChange change = new StateChange();
                                change._text = mController.getText();
                                change._cursorPos = mController.getCursorPos();
                                change._readOnlyModeP = mController.isReadOnlyMode();

                                change._changeType = new HashSet<CHANGE_TYPE>()
                                {
                                    {
                                        add(CHANGE_TYPE.COMMAND_COMPLETE);
                                    }
                                };

                                subscriber.onNext(change);
                            }
                            catch (Exception e)
                            {
                                subscriber.onError(e);
                            }
                        }
                    }
                }).subscribeOn(AndroidSchedulers.mainThread()).observeOn(AndroidSchedulers.mainThread());

                return worker.subscribe(resultConsumer, errorConsumer, onComplete);
            }

            @Override
            public Disposable disableReadOnlyMode(final Observer<StateChange> observer)
            {
                if (!isPresentP)
                    throw new IllegalStateException(NOT_PRESENT_EXCEPTION_MESSAGE);

                final Consumer<StateChange> resultConsumer = new Consumer<StateChange>() {
                    @Override
                    public void accept(@NonNull StateChange stateChange) throws Exception
                    {
                        observer.onNext(stateChange);
                    }
                };

                final Consumer<? super Throwable> errorConsumer = new Consumer<Throwable>() {
                    @Override
                    public void accept(@NonNull Throwable throwable) throws Exception
                    {
                        observer.onError(throwable);
                    }
                };

                final Action onComplete = new Action() {

                    @Override
                    public void run() throws Exception
                    {
                        observer.onComplete();
                    }
                };

                Observable<StateChange> worker = Observable.create(new ObservableOnSubscribe<StateChange>() {
                    @Override
                    public void subscribe(@NonNull ObservableEmitter<StateChange> subscriber) throws Exception
                    {
                        if (!subscriber.isDisposed())
                        {
                            try
                            {
                                mController.disableReadOnly();
                                StateChange change = new StateChange();
                                change._text = mController.getText();
                                change._cursorPos = mController.getCursorPos();
                                change._readOnlyModeP = mController.isReadOnlyMode();
                                change._changeType = new HashSet<CHANGE_TYPE>()
                                {
                                    {
                                        add(CHANGE_TYPE.COMMAND_COMPLETE);
                                    }
                                };

                                subscriber.onNext(change);
                            }
                            catch (Exception e)
                            {
                                subscriber.onError(e);
                            }
                        }
                    }
                }).subscribeOn(AndroidSchedulers.mainThread()).observeOn(AndroidSchedulers.mainThread());

                return worker.subscribe(resultConsumer, errorConsumer, onComplete);
            }

            @Override
            public int getCursorPosition()
            {
                return mController.getCursorPos();
            }

            @Override
            public ParseNode getSelection()
            {
                return mController.getSelection();
            }

            @Override
            public boolean isReadOnlyMode()
            {
                return mController.isReadOnlyMode();
            }

            @Override
            public boolean isPresentP()
            {
                return isPresentP;
            }

        };
    }
}
