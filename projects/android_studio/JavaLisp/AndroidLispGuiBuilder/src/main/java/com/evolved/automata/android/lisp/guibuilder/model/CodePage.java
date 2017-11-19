package com.evolved.automata.android.lisp.guibuilder.model;

import com.evolved.automata.android.lisp.guibuilder.BackgroundProcessEvent;
import com.evolved.automata.android.lisp.guibuilder.EventLog;
import com.evolved.automata.android.lisp.guibuilder.Tools;
import com.evolved.automata.android.lisp.guibuilder.model.ALGB;
import com.evolved.automata.android.lisp.guibuilder.model.Page;
import com.evolved.automata.editor.TextSearchIndex;
import com.evolved.automata.editor.TextSearchResult;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.lisp.editor.ParseNode;
import com.evolved.automata.lisp.editor.TopParseNode;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.UUID;

import io.reactivex.Observable;
import io.reactivex.ObservableEmitter;
import io.reactivex.ObservableOnSubscribe;
import io.reactivex.Observer;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.annotations.NonNull;
import io.reactivex.schedulers.Schedulers;

/**
 * Created by Evolved8 on 5/5/17.
 */

public class CodePage extends Page {

    final String EXPR_KEY;
    final String LOCAL_STORAGE_PATH_KEY;
    final String DROPBOX_PATH_KEY;
    final String CURSOR_POSITION_KEY;
    final String FIND_HISTORY_KEY ;
    final String REPLACE_HISTORY_KEY;


    final String EXPR_KEY_PREFIX = "EXPR";
    final String LOCAL_STORAGE_PATH_KEY_PREFIX = "LOCAL-STORAGE";
    final String DROPBOX_PATH_KEY_PREFIX = "DROPBOX-PATH";
    final String CURSOR_POSITION_KEY_PREFIX = "CURSOR_POSITION_KEY";

    final String TOP_PARSE_NODE_IS_VALID_KEY;
    final String TOP_PARSE_NODE_KEY;

    final String TOP_PARSE_NODE_IS_VALID_KEY_PREFIX = "TOP_PARSE_NODE_IS_VALID_KEY";
    final String TOP_PARSE_NODE_KEY_PREFIX = "TOP_PARSE_NODE_KEY";

    final String FIND_HISTORY_PREFIX = "FIND_HISTORY";
    final String REPLACE_HISTORY_PREFIX = "REPLACE_HISTORY";

    TopParseNode mTopParseNode = null;

    boolean mIsValid = false;

    TextSearchIndex mSearchIndex = null;

    LinkedList<String> mTextFindHistory = new LinkedList<String>();
    LinkedList<String> mTextReplaceHistory = new LinkedList<String>();

    int mMaxSearchHistory = 10;
    boolean mUpdateSearchIndexOnTextUpdateP;


    public LinkedList<String> getSearchHistory()
    {
        return mTextFindHistory;
    }

    public LinkedList<String> getReplaceHistory()
    {
        return mTextReplaceHistory;
    }

    public CodePage(ALGB app)
    {
        super(app);
        EXPR_KEY = mId + "-" + EXPR_KEY_PREFIX;
        LOCAL_STORAGE_PATH_KEY = mId + "-" + LOCAL_STORAGE_PATH_KEY_PREFIX;
        DROPBOX_PATH_KEY = mId + "-" + DROPBOX_PATH_KEY_PREFIX;
        CURSOR_POSITION_KEY = mId + "-" + CURSOR_POSITION_KEY_PREFIX;
        TOP_PARSE_NODE_IS_VALID_KEY = mId + "-" + TOP_PARSE_NODE_IS_VALID_KEY_PREFIX;
        TOP_PARSE_NODE_KEY  = mId + "-" + TOP_PARSE_NODE_KEY_PREFIX;
        FIND_HISTORY_KEY = mId + "-" + FIND_HISTORY_PREFIX;
        REPLACE_HISTORY_KEY = mId + "-" + REPLACE_HISTORY_PREFIX;
        setExpr("");
        setCursorPosition(0);
    }

    public CodePage(ALGB app, String id)
    {
        super(app, id);
        EXPR_KEY = mId + "-" + EXPR_KEY_PREFIX;
        LOCAL_STORAGE_PATH_KEY = mId + "-" + LOCAL_STORAGE_PATH_KEY_PREFIX;
        DROPBOX_PATH_KEY = mId + "-" + DROPBOX_PATH_KEY_PREFIX;
        CURSOR_POSITION_KEY = mId + "-" + CURSOR_POSITION_KEY_PREFIX;
        TOP_PARSE_NODE_IS_VALID_KEY = mId + "-" + TOP_PARSE_NODE_IS_VALID_KEY_PREFIX;
        TOP_PARSE_NODE_KEY  = mId + "-" + TOP_PARSE_NODE_KEY_PREFIX;
        FIND_HISTORY_KEY = mId + "-" + FIND_HISTORY_PREFIX;
        REPLACE_HISTORY_KEY = mId + "-" + REPLACE_HISTORY_PREFIX;
        Value v = mMyData.get(TOP_PARSE_NODE_IS_VALID_KEY);
        mIsValid = v!=null && !v.isNull();

        v = mMyData.get(TOP_PARSE_NODE_KEY);
        if (v != null)
        {
            String serialized = v.getString();

            mTopParseNode = (TopParseNode)ParseNode.deserialize(serialized);

        }

        v = mMyData.get(FIND_HISTORY_KEY);
        if (v != null)
        {

            for (String history:NLispTools.getStringArrayFromValue(v))
            {
                mTextFindHistory.add(history);
            }
        }

        v = mMyData.get(REPLACE_HISTORY_KEY);
        if (v != null)
        {

            for (String history:NLispTools.getStringArrayFromValue(v))
            {
                mTextReplaceHistory.add(history);
            }
        }
    }


    @Override
    public void savePage()
    {
        setTopParseNode();
        setIsParseNodeValid();
        setStringListDataValue(FIND_HISTORY_KEY, mTextFindHistory);
        setStringListDataValue(REPLACE_HISTORY_KEY, mTextReplaceHistory);
        super.savePage();
    }

    public TopParseNode getTopParseNode()
    {
        if (mIsValid)
            return mTopParseNode;
        else
            return null;
    }

    public void updateSearchIndex()
    {
        findText("", null);
    }

    public Iterator<TextSearchResult> findText(String text)
    {
        if (mSearchIndex != null)
        {

            updateSearchHistory(text);
            return mSearchIndex.getSearchIterator(text);
        }
        else
            return null;
    }

    public void updateReplaceHistory(String s)
    {
        if (mTextReplaceHistory.size() > mMaxSearchHistory)
            mTextReplaceHistory.removeFirst();

        if (mTextReplaceHistory.contains(s))
        {
            mTextReplaceHistory.remove(s);
        }
        mTextReplaceHistory.add(s);

    }

    public void updateSearchHistory(String s)
    {
        if (mTextFindHistory.size() > mMaxSearchHistory)
            mTextFindHistory.removeFirst();

        if (mTextFindHistory.contains(s))
        {
            mTextFindHistory.remove(s);
        }
        mTextFindHistory.add(s);

    }

    private void findText(final String text, Observer<Iterator<TextSearchResult>> observer)
    {
        if (mIsValid)
        {
            if (observer != null)
                observer.onNext(findText(text));
        }
        else
        {
            ObservableOnSubscribe<Iterator<TextSearchResult>> obserable = new ObservableOnSubscribe<Iterator<TextSearchResult>>() {
                @Override
                public void subscribe(@NonNull ObservableEmitter<Iterator<TextSearchResult>> innerObserver) throws Exception
                {
                    mSearchIndex = new TextSearchIndex(getExpr());
                    mIsValid = true;
                    if (innerObserver != null)
                        innerObserver.onNext(findText(text));

                }
            };

            Observable.create(obserable).subscribeOn(Schedulers.computation()).observeOn(AndroidSchedulers.mainThread()).subscribe(observer);
        }
    }



    public boolean isTopParseNodeValid()
    {
        return mIsValid;
    }


    public void requestTopParseNode(final Observer<TopParseNode> resultObserver)
    {
        final String processingId = UUID.randomUUID().toString();
        if (mTopParseNode != null && mIsValid)
        {
            resultObserver.onNext(mTopParseNode);
            resultObserver.onComplete();
        }
        else
        {
            ObservableOnSubscribe<TopParseNode> observable = new ObservableOnSubscribe<TopParseNode>()
            {

                @Override
                public void subscribe(@NonNull ObservableEmitter<TopParseNode> observer) throws Exception
                {
                    Tools.postEvent(BackgroundProcessEvent.makeProcessingStartedEvent("Starting to Parse TopNode for " + getTitle(), processingId));
                    try
                    {
                        mTopParseNode = new TopParseNode();
                        mTopParseNode.processAll(getExpr());
                        mSearchIndex = new TextSearchIndex(getExpr());
                        mIsValid = true;
                        setIsParseNodeValid();
                        Tools.postEvent(BackgroundProcessEvent.makeProcessingFinishedEvent("Parsed TopNode for " + getTitle(), processingId));
                        observer.onNext(mTopParseNode);
                        observer.onComplete();
                    }
                    catch(Exception e)
                    {
                        mIsValid = false;
                        Tools.postEvent(BackgroundProcessEvent.makeProcessingErrorEvent("Failed to parse TopNode for " + getTitle(), processingId));
                        EventLog.get().logSystemError(e, "Failed to create TopParseNode for " + getTitle());
                        observer.onError(e);
                    }


                }
            };
            Observable.create(observable).subscribeOn(Schedulers.computation()).observeOn(AndroidSchedulers.mainThread()).subscribe(resultObserver);

        }
    }

    private void setTopParseNode()
    {
        if (mTopParseNode != null)
            mMyData.put(TOP_PARSE_NODE_KEY, NLispTools.makeValue(mTopParseNode.serialize()));
    }

    private void setIsParseNodeValid()
    {

        mMyData.put(TOP_PARSE_NODE_IS_VALID_KEY, NLispTools.makeValue(mIsValid));
    }


    protected void setPageType()
    {
        mMyData.put(TYPE_KEY, NLispTools.makeValue(getPageType().toString()));
    }

    public PAGE_TYPE getPageType()
    {
        return PAGE_TYPE.CODE;
    }

    public void setExpr(String expr)
    {
        mScript = expr;
    }

    public void assertTopParseNodeIsInValid()
    {
        mIsValid = false;
        setIsParseNodeValid();
    }

    public String getExpr()
    {
        return mScript;
    }

    public void setDropboxPath(String dropbox)
    {
        setStringDataValue(DROPBOX_PATH_KEY, dropbox);
    }

    public String getDropboxPath()
    {
        return getStringDataValue(DROPBOX_PATH_KEY);
    }


    public String getLocalStoragePath()
    {
        return getStringDataValue(LOCAL_STORAGE_PATH_KEY);
    }

    public void setLocalStoragePath(String path)
    {
        setStringDataValue(LOCAL_STORAGE_PATH_KEY, path);
    }


    public void setCursorPosition(int pos)
    {
        setLongDataValue(CURSOR_POSITION_KEY, pos);
    }

    public int getCursorPosition()
    {
        Long cursor = getLongDataValue(CURSOR_POSITION_KEY);
        return cursor.intValue();
    }

    public void setTopParseNodeIsValid(boolean isValid)
    {
        mIsValid = isValid;
    }


}
