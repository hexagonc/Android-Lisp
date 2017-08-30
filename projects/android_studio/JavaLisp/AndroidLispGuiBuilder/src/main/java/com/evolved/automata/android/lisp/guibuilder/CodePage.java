package com.evolved.automata.android.lisp.guibuilder;

import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.Value;
import com.evolved.automata.lisp.editor.ParseNode;
import com.evolved.automata.lisp.editor.TopParseNode;

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

    final String EXPR_KEY_PREFIX = "EXPR";
    final String LOCAL_STORAGE_PATH_KEY_PREFIX = "LOCAL-STORAGE";
    final String DROPBOX_PATH_KEY_PREFIX = "DROPBOX-PATH";
    final String CURSOR_POSITION_KEY_PREFIX = "CURSOR_POSITION_KEY";

    final String TOP_PARSE_NODE_IS_VALID_KEY;
    final String TOP_PARSE_NODE_KEY;

    final String TOP_PARSE_NODE_IS_VALID_KEY_PREFIX = "TOP_PARSE_NODE_IS_VALID_KEY";
    final String TOP_PARSE_NODE_KEY_PREFIX = "TOP_PARSE_NODE_KEY";

    TopParseNode mTopParseNode = null;

    boolean mIsValid = false;

    public CodePage(ALGB app)
    {
        super(app);
        EXPR_KEY = mId + "-" + EXPR_KEY_PREFIX;
        LOCAL_STORAGE_PATH_KEY = mId + "-" + LOCAL_STORAGE_PATH_KEY_PREFIX;
        DROPBOX_PATH_KEY = mId + "-" + DROPBOX_PATH_KEY_PREFIX;
        CURSOR_POSITION_KEY = mId + "-" + CURSOR_POSITION_KEY_PREFIX;
        TOP_PARSE_NODE_IS_VALID_KEY = mId + "-" + TOP_PARSE_NODE_IS_VALID_KEY_PREFIX;
        TOP_PARSE_NODE_KEY  = mId + "-" + TOP_PARSE_NODE_KEY_PREFIX;
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
        Value v = mMyData.get(TOP_PARSE_NODE_IS_VALID_KEY);
        mIsValid = v!=null && !v.isNull();

        v = mMyData.get(TOP_PARSE_NODE_KEY);
        if (v != null)
        {
            String serialized = v.getString();
            mTopParseNode = (TopParseNode)ParseNode.deserialize(serialized);
        }
    }


    @Override
    public void savePage()
    {
        setTopParseNode();
        setIsParseNodeValid();
        super.savePage();
    }

    public TopParseNode getTopParseNode()
    {
        if (mIsValid)
            return mTopParseNode;
        else
            return null;
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
