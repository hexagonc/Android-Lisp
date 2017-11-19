package com.evolved.automata.android.lisp.guibuilder.events;

import com.evolved.automata.android.lisp.guibuilder.LispEditText;

import io.reactivex.Observer;
import io.reactivex.annotations.NonNull;
import io.reactivex.disposables.Disposable;

/**
 * Created by Evolved8 on 11/13/17.
 */

public abstract class GetSelectionBoundsEvent  {


    public abstract void onSelection(LispEditText.Selection Selection);

}
