package com.evolved.automata.desktop;

import com.dropbox.core.DbxDownloader;
import com.dropbox.core.DbxException;
import com.dropbox.core.DbxRequestConfig;
import com.dropbox.core.v2.DbxClientV2;
import com.dropbox.core.v2.files.DbxUserFilesRequests;
import com.dropbox.core.v2.files.FileMetadata;
import com.dropbox.core.v2.files.FolderMetadata;
import com.dropbox.core.v2.files.ListFolderResult;
import com.dropbox.core.v2.files.Metadata;
import com.dropbox.core.v2.files.UploadBuilder;
import com.dropbox.core.v2.files.UploadUploader;
import com.dropbox.core.v2.files.WriteMode;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.List;

/**
 * Created by Evolved8 on 3/26/18.
 */

public class DropboxInterface {

    static DbxClientV2 _client = null;
    public static void addDefaultFunctions(Environment env, String accessToken, String clientName){
        if (_client == null && accessToken != null && accessToken.length() > 0 && clientName != null){
            DbxRequestConfig.Builder builder = DbxRequestConfig.newBuilder(clientName);
            DbxRequestConfig config = builder.build();
            _client = new DbxClientV2(config, accessToken);
        }

        if (_client != null){
            env.mapFunction("create-dropbox-file", createDropboxFile());
            env.mapFunction("create-dropbox-folder", createDropboxFolder());
            env.mapFunction("list-dropbox-folder-contents", listDropboxFolderContents());
            env.mapFunction("upload-dropbox-file", uploadDropboxFile());
            env.mapFunction("download-dropbox-file", downloadDropboxFile());
        }
    }

    public static String createFolder(String path) throws DbxException
    {
        DbxUserFilesRequests files =  _client.files();
        FolderMetadata data = files.createFolder(path);

        return path;
    }

    public static String createFile(String fullfilePath) throws DbxException
    {
        DbxUserFilesRequests files =  _client.files();

        UploadBuilder builder = files.uploadBuilder(fullfilePath);
        UploadUploader loader = builder.start();
        OutputStream ostream = null;
        ostream = loader.getOutputStream();
        FileMetadata response = loader.finish();

        return fullfilePath;
    }

    public static List<Metadata> listFolderContents(String path) throws DbxException
    {
        DbxUserFilesRequests listCommand = _client.files();
        ListFolderResult result = listCommand.listFolder(path);

        List<Metadata> resultBatch = result.getEntries();

        while (result.getHasMore())
        {
            result = listCommand.listFolderContinue(result.getCursor());
            resultBatch.addAll(result.getEntries());

        }
        return resultBatch;
    }

    public static String downloadFile(String filePath) throws DbxException, IOException
    {
        DbxUserFilesRequests request = _client.files();
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
            return sbuilder.toString();

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

    public static String uploadFileToFolder(final String filePath, final String contents, final boolean overwriteIfExists) throws IOException, DbxException
    {
        DbxUserFilesRequests request = _client.files();
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
            return filePath;
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
                io.printStackTrace();
            }
        }
    }


    static SimpleFunctionTemplate createDropboxFile()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)createDropboxFile();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);

                String fullFilePath = evaluatedArgs[0].getString();

                try
                {
                    String path = createFile(fullFilePath);
                    return NLispTools.makeValue(path);
                }
                catch (Exception e)
                {
                    throw new RuntimeException(e);
                }
            }
        };
    }


    static SimpleFunctionTemplate createDropboxFolder()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)createDropboxFolder();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);

                String folder = evaluatedArgs[0].getString();

                try
                {
                    String path = createFolder(folder);
                    return NLispTools.makeValue(path);
                }
                catch (Exception e)
                {
                    throw new RuntimeException(e);
                }
            }
        };
    }


    static SimpleFunctionTemplate listDropboxFolderContents()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)listDropboxFolderContents();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);

                String folder = evaluatedArgs[0].getString();

                try
                {
                    List<Metadata> data = listFolderContents(folder);

                    int length = data.size();

                    Value[] out = new Value[length];
                    int i = 0;
                    for (Metadata meta:data){

                        String path = meta.getPathDisplay();
                        String pathLower = meta.getPathLower();
                        String name = meta.getName();
                        String parentId = meta.getParentSharedFolderId();
                        boolean isFolder =  meta instanceof FolderMetadata;
                        String[] properties = new String[]{name, path, pathLower, parentId};
                        Value[] metaValues = new Value[properties.length+1];
                        for (int j = 0;j <properties.length;j++){
                            metaValues[j]= NLispTools.makeValue(properties[j]);
                        }
                        metaValues[properties.length] = NLispTools.makeValue(isFolder);
                        out[i] = NLispTools.makeValue(metaValues);
                        i++;
                    }
                    return NLispTools.makeValue(out);
                }
                catch (Exception e)
                {
                    throw new RuntimeException(e);
                }
            }
        };
    }


    static SimpleFunctionTemplate uploadDropboxFile()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)uploadDropboxFile();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(2, true, true);

                String filepath = evaluatedArgs[0].getString();
                String content = evaluatedArgs[1].getString();
                boolean overwrite = true;
                overwrite = evaluatedArgs.length <= 2 || !evaluatedArgs[2].isNull();
                try
                {
                    String result = uploadFileToFolder(filepath, content, overwrite);

                    return NLispTools.makeValue(result);
                }
                catch (Exception e)
                {
                    throw new RuntimeException(e);
                }
            }
        };
    }

    static SimpleFunctionTemplate downloadDropboxFile()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws InstantiationException, IllegalAccessException
            {
                return (T)downloadDropboxFile();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);

                String filepath = evaluatedArgs[0].getString();

                try
                {
                    String result = downloadFile(filepath);

                    return NLispTools.makeValue(result);
                }
                catch (Exception e)
                {
                    throw new RuntimeException(e);
                }
            }
        };
    }

}
