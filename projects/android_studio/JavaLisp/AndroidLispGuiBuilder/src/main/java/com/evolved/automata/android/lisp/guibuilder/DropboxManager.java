package com.evolved.automata.android.lisp.guibuilder;

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
import com.dropbox.core.v2.files.UploadBuilder;
import com.dropbox.core.v2.files.UploadUploader;
import com.dropbox.core.v2.files.WriteMode;
import com.evolved.automata.android.lisp.AndroidLispInterpreter;
import com.evolved.automata.android.lisp.guibuilder.model.LispContext;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.Lambda;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;

import org.greenrobot.eventbus.Subscribe;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.lang.ref.WeakReference;
import java.nio.charset.Charset;
import java.util.LinkedList;
import java.util.List;

import io.reactivex.Observable;
import io.reactivex.ObservableEmitter;
import io.reactivex.ObservableOnSubscribe;
import io.reactivex.Observer;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.annotations.NonNull;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Action;
import io.reactivex.functions.Consumer;
import io.reactivex.schedulers.Schedulers;
import io.reactivex.subjects.PublishSubject;

import static com.evolved.automata.android.lisp.guibuilder.DropboxManager.DROPBOX_RESPONSE_TYPE.DOWNLOAD_TEXT_FILE;
import static com.evolved.automata.android.lisp.guibuilder.DropboxManager.DROPBOX_RESPONSE_TYPE.UPLOAD_FILE;

/**
 * Created by Evolved8 on 5/11/17.
 */

public class DropboxManager implements Observer<DropboxManager.DropboxEvent> {



    public enum DROPBOX_RESPONSE_TYPE
    {
        SHOW_FILE_DOWNLOAD_DIALOG, GET_FOLDER_CHILDREN, UPLOAD_FILE, DOWNLOAD_TEXT_FILE
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
            _type = DROPBOX_RESPONSE_TYPE.SHOW_FILE_DOWNLOAD_DIALOG;
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


    public static class UploadTextResponse extends DropboxResponse
    {
        String mTargetFileName;

        public UploadTextResponse(String filename)
        {
            _type = UPLOAD_FILE;
            mTargetFileName = filename;
        }

        public String getTargetFilename()
        {
            return mTargetFileName;
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
        Disposable uploadTextFile(String filePath, String contents, boolean overwriteIfExists, Observer<DropboxResponse> responseObserver);
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

    public void addDropboxLispFunctions(Environment env, Activity activity, LispContext lisp)
    {

        env.mapFunction("show-dropbox-download-dialog", showDropboxDownloadDialog(new WeakReference<Activity>(activity), lisp));
        env.mapFunction("download-dropbox-file", downloadDropboxFile(lisp));
        env.mapFunction("upload-dropbox-file", uploadDropboxFile(lisp));
        env.mapFunction("show-dropbox-file-chooser", showDropboxFileChooserDialog(new WeakReference<Activity>(activity), lisp));
    }

    public SimpleFunctionTemplate uploadDropboxFile(final LispContext lisp)
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)uploadDropboxFile(lisp);
            }

            @Override
            public Value evaluate(final Environment env, Value[] evaluatedArgs) {
                checkActualArguments(4, false, false);

                final String fileName = evaluatedArgs[0].getString();
                final String textContents = evaluatedArgs[1].getString();
                boolean overwriteIfExists = !evaluatedArgs[2].isNull();
                Value onResultLambda = evaluatedArgs[3];
                final Lambda lambda = (Lambda) onResultLambda.getLambda();


                Observer<DropboxResponse> uploadHandler = new Observer<DropboxResponse>()
                {

                    @Override
                    public void onSubscribe(@NonNull Disposable d)
                    {

                    }

                    @Override
                    public void onNext(@NonNull DropboxResponse dropboxResponse)
                    {
                        UploadTextResponse uploadResult = (UploadTextResponse)dropboxResponse;
                        String targetFilename = uploadResult.getTargetFilename();
                        Value[] args = new Value[]{NLispTools.makeValue(targetFilename)};
                        lambda.setActualParameters(args);
                        AndroidLispInterpreter interpreter = lisp.getForegroundInterpreter();
                        interpreter.evaluateFunction(lambda, env);
                    }

                    @Override
                    public void onError(@NonNull Throwable e)
                    {
                        Value[] args = new Value[]{Environment.getNull(), NLispTools.makeValue(e.toString())};
                        lambda.setActualParameters(args);
                        AndroidLispInterpreter interpreter = lisp.getForegroundInterpreter();
                        interpreter.evaluateFunction(lambda, env);
                    }

                    @Override
                    public void onComplete()
                    {

                    }
                };

                if (mClient == null)
                {
                    Value[] args = new Value[]{Environment.getNull(), NLispTools.makeValue("Can't access dropbox until user logs in")};
                    try
                    {
                        lambda.setActualParameters(args);
                        lambda.evaluate(env, false);
                    }
                    catch (Exception ex)
                    {
                        EventLog.get().logSystemError(ex, "Error evaluating dropbox upload error handler");
                    }
                    return Environment.getNull();
                }
                else
                {
                    getController().uploadTextFile(fileName, textContents, overwriteIfExists,uploadHandler);
                    return evaluatedArgs[0];
                }

            }
        };
    }


    public SimpleFunctionTemplate downloadDropboxFile(final LispContext lisp)
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)downloadDropboxFile(lisp);
            }

            @Override
            public Value evaluate(final Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);

                final String fileName = evaluatedArgs[0].getString();
                Value onResultLambda = evaluatedArgs[1];
                final Lambda lambda = (Lambda) onResultLambda.getLambda();


                Observer<DropboxResponse> downloadHandler = new Observer<DropboxResponse>()
                {

                    @Override
                    public void onSubscribe(@NonNull Disposable d)
                    {

                    }

                    @Override
                    public void onNext(@NonNull DropboxResponse dropboxResponse)
                    {
                        DownloadTextResponse downloadResult = (DownloadTextResponse)dropboxResponse;
                        String contents = downloadResult.getContents();
                        Value[] args = new Value[]{NLispTools.makeValue(contents), NLispTools.makeValue(fileName)};
                        lambda.setActualParameters(args);
                        AndroidLispInterpreter interpreter = lisp.getForegroundInterpreter();
                        interpreter.evaluateFunction(lambda, env);
                    }

                    @Override
                    public void onError(@NonNull Throwable e)
                    {
                        Value[] args = new Value[]{Environment.getNull(), Environment.getNull(), NLispTools.makeValue(e.toString())};
                        lambda.setActualParameters(args);
                        AndroidLispInterpreter interpreter = lisp.getForegroundInterpreter();
                        interpreter.evaluateFunction(lambda, env);

                    }

                    @Override
                    public void onComplete()
                    {

                    }
                };

                if (mClient == null)
                {
                    Value[] args = new Value[]{Environment.getNull(), Environment.getNull(), NLispTools.makeValue("Can't access dropbox until user logs in")};
                    try
                    {
                        lambda.setActualParameters(args);
                        lambda.evaluate(env, false);
                    }
                    catch (Exception ex)
                    {
                        EventLog.get().logSystemError(ex, "Error evaluating dropbox download error handler");
                    }
                    return Environment.getNull();
                }
                else
                {
                    getController().downloadFile(fileName, downloadHandler);
                    return evaluatedArgs[0];
                }

            }
        };
    }



    public SimpleFunctionTemplate showDropboxFileChooserDialog(final WeakReference<Activity> activityRef, final LispContext lisp)
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) showDropboxFileChooserDialog(activityRef, lisp);
            }

            @Override
            public Value evaluate(final Environment env, Value[] evaluatedArgs) {
                checkActualArguments(2, true, true);

                String basePath = evaluatedArgs[0].getString();
                Value onResultLambda = evaluatedArgs[1];
                final Lambda lambda = (Lambda) onResultLambda.getLambda();


                DropboxChooserItem.FileItemSelectHandler fileSelectedHandler = new DropboxChooserItem.FileItemSelectHandler() {
                    @Override
                    public void onSelected(final String filename)
                    {
                        Value[] args = new Value[]{NLispTools.makeValue(filename)};
                        lambda.setActualParameters(args);
                        AndroidLispInterpreter interpreter = lisp.getForegroundInterpreter();
                        interpreter.evaluateFunction(lambda, env);
                    }
                };

                Observer<DropboxResponse> dialogResponseObserver = new Observer<DropboxResponse>() {
                    @Override
                    public void onSubscribe(@NonNull Disposable d)
                    {

                    }

                    @Override
                    public void onNext(@NonNull DropboxResponse dropboxResponse)
                    {
                        FileDialogResponse dialogResponse = (FileDialogResponse)dropboxResponse;
                        FileChooserDialog chooserDialog = dialogResponse.getDialog();
                        chooserDialog.show();
                    }

                    @Override
                    public void onError(@NonNull Throwable e)
                    {
                        Value[] args = new Value[]{Environment.getNull(), NLispTools.makeValue(e.toString())};
                        lambda.setActualParameters(args);
                        AndroidLispInterpreter interpreter = lisp.getForegroundInterpreter();
                        interpreter.evaluateFunction(lambda, env);
                    }

                    @Override
                    public void onComplete()
                    {

                    }
                };

                Activity activity = activityRef.get();
                if (mClient == null)
                {
                    Value[] args = new Value[]{Environment.getNull(), NLispTools.makeValue("Can't access dropbox until user logs in")};
                    try
                    {
                        lambda.setActualParameters(args);
                        lambda.evaluate(env, false);
                    }
                    catch (Exception ex)
                    {
                        EventLog.get().logSystemError(ex, "Error evaluating dropbox file dialog error handler");
                    }
                    return Environment.getNull();
                }

                if (activity != null)
                {
                    getController().getFileDialog(activity, "Select File to Download", basePath, fileSelectedHandler, dialogResponseObserver);
                    return evaluatedArgs[0];
                }
                else
                {


                    EventLog.get().logSystemError("Can't display dropbox dialog with invalid Activity context");
                    return Environment.getNull();
                }


            }
        };
    }

    public SimpleFunctionTemplate showDropboxDownloadDialog(final WeakReference<Activity> activityRef, final LispContext lisp)
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T) showDropboxDownloadDialog(activityRef, lisp);
            }

            @Override
            public Value evaluate(final Environment env, Value[] evaluatedArgs) {
                checkActualArguments(2, true, true);

                String basePath = evaluatedArgs[0].getString();
                 Value onResultLambda = evaluatedArgs[1];
                final Lambda lambda = (Lambda) onResultLambda.getLambda();


                DropboxChooserItem.FileItemSelectHandler fileSelectedHandler = new DropboxChooserItem.FileItemSelectHandler() {
                    @Override
                    public void onSelected(final String filename)
                    {
                        Observer<DropboxResponse> downloadResponse = new Observer<DropboxResponse>()
                        {

                            @Override
                            public void onSubscribe(@NonNull Disposable d)
                            {

                            }

                            @Override
                            public void onNext(@NonNull DropboxResponse dropboxResponse)
                            {
                                DownloadTextResponse downloadResult = (DownloadTextResponse)dropboxResponse;
                                String contents = downloadResult.getContents();
                                Value[] args = new Value[]{NLispTools.makeValue(contents), NLispTools.makeValue(filename)};
                                lambda.setActualParameters(args);
                                AndroidLispInterpreter interpreter = lisp.getForegroundInterpreter();
                                interpreter.evaluateFunction(lambda, env);
                            }

                            @Override
                            public void onError(@NonNull Throwable e)
                            {
                                Value[] args = new Value[]{Environment.getNull(), Environment.getNull(), NLispTools.makeValue(e.toString())};
                                lambda.setActualParameters(args);
                                AndroidLispInterpreter interpreter = lisp.getForegroundInterpreter();
                                interpreter.evaluateFunction(lambda, env);
                            }

                            @Override
                            public void onComplete()
                            {

                            }
                        };

                        getController().downloadFile(filename, downloadResponse);
                    }
                };

                Observer<DropboxResponse> dialogResponseObserver = new Observer<DropboxResponse>() {
                    @Override
                    public void onSubscribe(@NonNull Disposable d)
                    {

                    }

                    @Override
                    public void onNext(@NonNull DropboxResponse dropboxResponse)
                    {
                        FileDialogResponse dialogResponse = (FileDialogResponse)dropboxResponse;
                        FileChooserDialog chooserDialog = dialogResponse.getDialog();
                        chooserDialog.show();
                    }

                    @Override
                    public void onError(@NonNull Throwable e)
                    {
                        Value[] args = new Value[]{Environment.getNull(), Environment.getNull(), NLispTools.makeValue(e.toString())};
                        lambda.setActualParameters(args);
                        AndroidLispInterpreter interpreter = lisp.getForegroundInterpreter();
                        interpreter.evaluateFunction(lambda, env);
                    }

                    @Override
                    public void onComplete()
                    {

                    }
                };

                Activity activity = activityRef.get();
                if (mClient == null)
                {
                    Value[] args = new Value[]{Environment.getNull(), Environment.getNull(), NLispTools.makeValue("Can't access dropbox until user logs in")};
                    try
                    {
                        lambda.setActualParameters(args);
                        lambda.evaluate(env, false);
                    }
                    catch (Exception ex)
                    {
                        EventLog.get().logSystemError(ex, "Error evaluating dropbox download error handler");
                    }
                    return Environment.getNull();
                }

                if (activity != null)
                {
                    getController().getFileDialog(activity, "Select File to Download", basePath, fileSelectedHandler, dialogResponseObserver);
                    return evaluatedArgs[0];
                }
                else
                {


                    EventLog.get().logSystemError("Can't display dropbox dialog with invalid Activity context");
                    return Environment.getNull();
                }


            }
        };
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

    @Subscribe
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
                    public void subscribe(final @NonNull ObservableEmitter<DropboxResponse> subscriber) throws Exception
                    {
                        if (mClient == null)
                        {
                            subscriber.onError(new DropboxException("Cannot show list dialog  if not logged in", DropboxException.EXCEPTION_TYPE.NOT_LOGGED_IN));
                        }
                        else
                        {

                            try
                            {

                                final DropboxChooserItem top;
                                if (DropboxFileData.isRootFolderPath(basePath))
                                {
                                    top = new DropboxChooserItem(DropboxFileData.createRootFolderData(), selectHandler);
                                    FileChooserDialog fDialog = new FileChooserDialog(context, title, top);

                                    subscriber.onNext(new FileDialogResponse(fDialog));
                                }
                                else
                                {

                                    Observable<DropboxChooserItem> getMetaData = Observable.create(new ObservableOnSubscribe<DropboxChooserItem>() {
                                        @Override
                                        public void subscribe(@NonNull ObservableEmitter<DropboxChooserItem> e) throws Exception
                                        {
                                            Metadata base = _me.getMetaData(basePath);
                                            DropboxChooserItem item = new DropboxChooserItem(DropboxFileData.from(base), selectHandler);
                                            e.onNext(item);
                                            e.onComplete();


                                        }
                                    });

                                    getMetaData.subscribeOn(Schedulers.io()).observeOn(AndroidSchedulers.mainThread()).subscribe(new Observer<DropboxChooserItem>() {
                                        @Override
                                        public void onSubscribe(@NonNull Disposable d)
                                        {

                                        }

                                        @Override
                                        public void onNext(@NonNull DropboxChooserItem item)
                                        {
                                            try
                                            {
                                                FileChooserDialog fDialog = new FileChooserDialog(context, title, item);
                                                subscriber.onNext(new FileDialogResponse(fDialog));
                                            }
                                            catch (Exception e)
                                            {
                                                subscriber.onError(e);
                                            }

                                        }

                                        @Override
                                        public void onError(@NonNull Throwable e)
                                        {
                                            subscriber.onError(e);
                                        }

                                        @Override
                                        public void onComplete()
                                        {

                                        }
                                    });


                                }


                            }
                            catch (Exception de)
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
            public Disposable uploadTextFile(final String filePath, final String contents, final boolean overwriteIfExists, final Observer<DropboxResponse> responseObserver)
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
                            subscriber.onError(new DropboxException("Cannot upload file if not logged in", DropboxException.EXCEPTION_TYPE.NOT_LOGGED_IN));
                        }
                        else
                        {

                            DbxUserFilesRequests request = mClient.files();
                            UploadUploader uploader = null;
                            OutputStream os = null;
                            try
                            {
                                UploadBuilder builder = request.uploadBuilder(filePath);
                                if (overwriteIfExists)
                                    builder.withMode(WriteMode.OVERWRITE);
                                uploader = builder.start();
                                os = uploader.getOutputStream();

                                byte[] data = contents.getBytes(Charset.forName("utf-8"));

                                os.write(data);
                                uploader.finish();
                                subscriber.onNext(new UploadTextResponse(contents));

                            }
                            catch (DbxException de)
                            {
                                subscriber.onError(de);
                            }
                            finally
                            {
                                try
                                {

                                    if (uploader != null)
                                        uploader.close();
                                }
                                catch (Exception io)
                                {

                                }
                            }

                        }
                    }
                });

                return observable.subscribeOn(Schedulers.io()).observeOn(AndroidSchedulers.mainThread()).subscribe(onResponse, onError, onComplete);
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
                                downloader.close();
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
