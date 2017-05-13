package com.evolved.automata.android.lisp.guibuilder.v2;

import android.app.Activity;
import android.content.Context;
import android.content.res.Resources;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;

import com.dropbox.core.DbxDownloader;
import com.dropbox.core.DbxException;
import com.dropbox.core.DbxRequestConfig;
import com.dropbox.core.v2.DbxClientV2;
import com.dropbox.core.v2.files.DbxUserFilesRequests;
import com.dropbox.core.v2.files.FileMetadata;
import com.dropbox.core.v2.files.ListFolderResult;
import com.dropbox.core.v2.files.Metadata;
import com.evolved.automata.android.lisp.guibuilder.R;

import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.LinkedList;
import java.util.List;

import io.reactivex.Observable;
import io.reactivex.ObservableEmitter;
import io.reactivex.ObservableOnSubscribe;
import io.reactivex.Observer;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.annotations.NonNull;
import io.reactivex.annotations.Nullable;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Action;
import io.reactivex.functions.Cancellable;
import io.reactivex.functions.Consumer;
import io.reactivex.schedulers.Schedulers;
import io.reactivex.subjects.PublishSubject;

import static com.evolved.automata.android.lisp.guibuilder.v2.DropboxManager.DROPBOX_RESPONSE_TYPE.DOWNLOAD_TEXT_FILE;

/**
 * Created by Evolved8 on 5/11/17.
 */

public class DropboxManager implements Observer<DropboxManager.DropboxEvent> {



    public enum DROPBOX_RESPONSE_TYPE
    {
        GET_FILE_DIALOG, GET_FOLDER_CHILDREN, UPLOAD_FILE, DOWNLOAD_TEXT_FILE
    }

    public static abstract class DropboxResponse
    {
        DROPBOX_RESPONSE_TYPE _type;

        public DROPBOX_RESPONSE_TYPE getType()
        {
            return _type;
        }
    }

    public static class FileDialogResponse extends DropboxResponse
    {
        FileChooserDialog mDialog;

        public FileDialogResponse(FileChooserDialog dialog)
        {
            _type = DROPBOX_RESPONSE_TYPE.GET_FILE_DIALOG;
            mDialog = dialog;
        }

        public FileChooserDialog getDialog()
        {
            return mDialog;
        }
    }


    public static class DownloadTextResponse extends DropboxResponse
    {
        String mFileContents;

        public DownloadTextResponse(String contents)
        {
            _type = DOWNLOAD_TEXT_FILE;
            mFileContents = contents;
        }

        public String getContents()
        {
            return mFileContents;
        }
    }

    public static class ListFolderResponse extends DropboxResponse
    {
        LinkedList<DropboxFileData> _list;
        public ListFolderResponse(List<Metadata> result)
        {
            _type = DROPBOX_RESPONSE_TYPE.GET_FOLDER_CHILDREN;
            _list = new LinkedList<DropboxFileData>();

            for (Metadata meta:result)
            {
                _list.add(DropboxFileData.from(meta));
            }

        }

        public LinkedList<DropboxFileData> getChildren()
        {
            return _list;
        }
    }


    public enum DROPBOX_EVENT_TYPE
    {
        LOGIN, LOGOUT, DROPBOX_UNAVAILABLE
    }

    public static abstract class DropboxEvent
    {
        DROPBOX_EVENT_TYPE _type;

        public DROPBOX_EVENT_TYPE getType()
        {
            return _type;
        }

    }

    public static class LoginEvent extends DropboxEvent
    {
        DbxClientV2 _client;
        public LoginEvent(DbxClientV2 client)
        {
            _type = DROPBOX_EVENT_TYPE.LOGIN;
            _client = client;
        }

        public DbxClientV2 getClient()
        {
            return _client;
        }
    }



    public interface Controller
    {
        Disposable getFileDialog(Activity context, String title, String basePath, DropboxChooserItem.FileItemSelectHandler selectHandler, Observer<DropboxResponse> responseObserver);
        Disposable listFolderContent(String folderPath, Observer<DropboxResponse> responseObserver );
        Disposable downloadFile(String filePath, Observer<DropboxResponse> responseObserver);
        Disposable setEventWatcher(Observer<DropboxEvent> eventObserver);
        Metadata getMetaData(String name) throws DbxException, DropboxException;

    }


    private static DropboxManager mManager;


    Context mContext;

    DbxClientV2 mClient;

    Handler mMainHandler;

    PublishSubject<DropboxEvent> mInternalEventObserver;

    Observable<DropboxEvent> mMainThreadObservable;

    private DropboxManager(Context con)
    {
        mContext = con;
        mMainHandler = new Handler(Looper.getMainLooper());
        mInternalEventObserver = PublishSubject.create();

        mMainThreadObservable = mInternalEventObserver.observeOn(AndroidSchedulers.mainThread());
        mMainThreadObservable.subscribe(this);

        Tools.registerEventHandler(this);

    }

    public static DropboxManager create(Context con)
    {
        if (mManager != null)
        {
            return mManager;
        }
        else
            return mManager = new DropboxManager(con);
    }


    public static DropboxManager get()
    {
        return mManager;
    }

    public void onEvent(DropboxEvent external)
    {
        mInternalEventObserver.onNext(external);
    }

    public Handler getMainHandler()
    {
        return mMainHandler;
    }

    @Override
    public void onSubscribe(@NonNull Disposable d)
    {

    }


    public void onNext(DropboxEvent event)
    {
        // TODO: finish this when login/logout support is ready
        Log.d("<><><><><><>", "Dropbox event: " + event.getType());

        switch (event.getType())
        {
            case LOGIN:
                mClient = ((LoginEvent)event).getClient();
                break;
            case LOGOUT:
                mClient = null;
                break;
        }
    }

    public void onError(Throwable error)
    {
        // TODO: finish this when login/logout support is ready
        Log.e("<><><><><><>", "Dropbox event error: " + error);
    }

    @Override
    public void onComplete()
    {

    }


    public Controller getController()
    {
        return new Controller() {

            Controller _me;
            {
                _me = this;
            }

            @Override
            public Metadata getMetaData(String name) throws DbxException, DropboxException
            {
                if (mClient == null)
                {
                    throw new DropboxException("Can't get metadata if not logged in", DropboxException.EXCEPTION_TYPE.NOT_LOGGED_IN);
                }
                else
                {
                    DbxUserFilesRequests listCommand = mClient.files();
                    return listCommand.getMetadata(name);

                }
            }

            @Override
            public Disposable getFileDialog(final Activity context, final String title, final String basePath, final DropboxChooserItem.FileItemSelectHandler selectHandler, final Observer<DropboxResponse> responseObserver)
            {
                Consumer<DropboxResponse> onResponse = new Consumer<DropboxResponse>() {
                    @Override
                    public void accept(@NonNull DropboxResponse dropboxResponse) throws Exception
                    {
                        responseObserver.onNext(dropboxResponse);
                    }
                };

                Consumer<Throwable> onError = new Consumer<Throwable>() {
                    @Override
                    public void accept(@NonNull Throwable throwable) throws Exception
                    {
                        responseObserver.onError(throwable);
                    }
                };


                Action onComplete = new Action()
                {
                    public void run()
                    {
                        responseObserver.onComplete();
                    }

                };

                Observable<DropboxResponse> observable = Observable.create(new ObservableOnSubscribe<DropboxResponse>() {
                    @Override
                    public void subscribe(@NonNull ObservableEmitter<DropboxResponse> subscriber) throws Exception
                    {
                        if (mClient == null)
                        {
                            subscriber.onError(new DropboxException("Cannot show list dialog  if not logged in", DropboxException.EXCEPTION_TYPE.NOT_LOGGED_IN));
                        }
                        else
                        {

                            try
                            {

                                DropboxChooserItem top;
                                if (DropboxFileData.isRootFolderPath(basePath))
                                {
                                    top = new DropboxChooserItem(DropboxFileData.createRootFolderData(), selectHandler);
                                }
                                else
                                {
                                    Metadata base = _me.getMetaData(basePath);
                                    top = new DropboxChooserItem(DropboxFileData.from(base), selectHandler);
                                }

                                FileChooserDialog fDialog = new FileChooserDialog(context, title, top);

                                subscriber.onNext(new FileDialogResponse(fDialog));
                            }
                            catch (DbxException de)
                            {
                                subscriber.onError(de);
                            }
                        }
                    }
                });




                return observable.subscribeOn(AndroidSchedulers.mainThread()).observeOn(AndroidSchedulers.mainThread()).subscribe(onResponse, onError, onComplete);
            }

            @Override
            public Disposable listFolderContent(final String folderPath, final Observer<DropboxResponse> responseObserver)
            {

                Consumer<DropboxResponse> onResponse = new Consumer<DropboxResponse>() {
                    @Override
                    public void accept(@NonNull DropboxResponse dropboxResponse) throws Exception
                    {
                        responseObserver.onNext(dropboxResponse);
                    }
                };

                Consumer<Throwable> onError = new Consumer<Throwable>() {
                    @Override
                    public void accept(@NonNull Throwable throwable) throws Exception
                    {
                        responseObserver.onError(throwable);
                    }
                };


                Action onComplete = new Action()
                {
                    public void run()
                    {
                        responseObserver.onComplete();
                    }

                };


                Observable<DropboxResponse> observable = Observable.create(new ObservableOnSubscribe<DropboxResponse>() {
                    @Override
                    public void subscribe(@NonNull ObservableEmitter<DropboxResponse> subscriber) throws Exception
                    {
                        if (mClient == null)
                        {
                            subscriber.onError(new DropboxException("Cannot list folder if not logged in", DropboxException.EXCEPTION_TYPE.NOT_LOGGED_IN));
                        }
                        else
                        {

                            DbxUserFilesRequests listCommand = mClient.files();
                            try
                            {
                                ListFolderResult result = listCommand.listFolder(folderPath);

                                List<Metadata> resultBatch = result.getEntries();

                                while (result.getHasMore())
                                {
                                    result = listCommand.listFolderContinue(result.getCursor());
                                    resultBatch.addAll(result.getEntries());

                                }

                                subscriber.onNext(new ListFolderResponse(resultBatch));

                            }
                            catch (DbxException de)
                            {
                                subscriber.onError(de);
                            }

                        }
                    }
                });




                return observable.subscribeOn(Schedulers.io()).subscribe(onResponse, onError, onComplete);
            }

            @Override
            public Disposable downloadFile(final String filePath, final Observer<DropboxResponse> responseObserver)
            {
                Consumer<DropboxResponse> onResponse = new Consumer<DropboxResponse>() {
                    @Override
                    public void accept(@NonNull DropboxResponse dropboxResponse) throws Exception
                    {
                        responseObserver.onNext(dropboxResponse);
                    }
                };

                Consumer<Throwable> onError = new Consumer<Throwable>() {
                    @Override
                    public void accept(@NonNull Throwable throwable) throws Exception
                    {
                        responseObserver.onError(throwable);
                    }
                };


                Action onComplete = new Action()
                {
                    public void run()
                    {
                        responseObserver.onComplete();
                    }

                };



                Observable<DropboxResponse> observable = Observable.create(new ObservableOnSubscribe<DropboxResponse>() {
                    @Override
                    public void subscribe(@NonNull ObservableEmitter<DropboxResponse> subscriber) throws Exception
                    {
                        if (mClient == null)
                        {
                            subscriber.onError(new DropboxException("Cannot download file if not logged in", DropboxException.EXCEPTION_TYPE.NOT_LOGGED_IN));
                        }
                        else
                        {

                            DbxUserFilesRequests request = mClient.files();
                            InputStreamReader ior = null;
                            try
                            {
                                DbxDownloader<FileMetadata> downloader = request.download(filePath);

                                InputStream ios = downloader.getInputStream();

                                StringBuilder sbuilder = new StringBuilder();

                                InputStreamReader reader = new InputStreamReader(ios);
                                int data;
                                while ((data = reader.read())!=-1)
                                {
                                    sbuilder.appendCodePoint(data);
                                }

                                subscriber.onNext(new DownloadTextResponse(sbuilder.toString()));

                            }
                            catch (DbxException de)
                            {
                                subscriber.onError(de);
                            }
                            finally
                            {
                                try
                                {
                                    if (ior != null)
                                        ior.close();
                                }
                                catch (IOException io)
                                {

                                }
                            }

                        }
                    }
                });




                return observable.subscribeOn(Schedulers.io()).observeOn(AndroidSchedulers.mainThread()).subscribe(onResponse, onError, onComplete);

            }

            @Override
            public Disposable setEventWatcher(Observer<DropboxEvent> eventObserver)
            {
                return null;
            }
        };
    }

    public DbxClientV2 getTestClient()
    {
        Resources res = mContext.getResources();
        String accessToken = res.getString(R.string.v2_test_dropbox_token);


        DbxRequestConfig.Builder builder = DbxRequestConfig.newBuilder("AndroidLispGUIBuilder");
        DbxRequestConfig config = builder.build();


        return  new DbxClientV2(config, accessToken);
    }
}
