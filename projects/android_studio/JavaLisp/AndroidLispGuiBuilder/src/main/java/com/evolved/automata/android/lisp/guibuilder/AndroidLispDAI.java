package com.evolved.automata.android.lisp.guibuilder;

import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteStatement;


import java.sql.SQLException;
import java.util.LinkedList;
import java.util.Locale;

/**
 * Created by Evolved8 on 5/4/17.
 */

public class AndroidLispDAI implements LispDataAccessInterface {

    static String _DEFAULT_CONTEXT = "default";
    static String _INSERT_KEY = "insert into objects (name, context, value, last_access) values (?,?, ?, ?)"; // 1

    static String _TEST_KEY = "select rowid from objects where name = ? and context = ?";
    static String _SELECT_DATA_KEY = "select value, rowid from objects where name = ? and context = ?";
    static String _SELECT_SPECIFIC_DATA_KEY = "select value from objects where rowid = ?";
    static String _UPDATE_SPECIFIC_DATA_KEY = "UPDATE OBJECTS SET value = ?, last_access =  coalesce(?, last_access) where rowid = ?";
    static String _UPDATE_SPECIFIC_ACCESS_DATA_KEY = "UPDATE OBJECTS SET last_access = ? where rowid = ?";


    static String _SELECT_ALL_AGED_KEY = "select name, value from objects where context=? and last_access<?";
    static String _SELECT_ALL_NEW_KEY = "select name, value from objects where context=? and last_access>=?";


    static String _DELETE_DATA_KEY = "delete from objects where name = ? and context=?";
    static String _DELETE_ALL_KEY = "delete from objects";
    static String _DELETE_ALL_CONTEXT_KEY = "delete from objects where context=?";
    static String _SELECT_ALL_NAMES_KEY = "select name from objects where context=?";

    static String _DELETE_ALL_AGED_KEY = "delete from objects where context=? and last_access < ?";


    static String _STANDARD_CREATE_MAIN_TABLE = "create table if not exists objects (name text, context varchar(500),  value text, value_bin blob, last_access integer, primary key (name, context))";


    boolean rebuildTable = false;
    public SQLiteDatabase appDB;

    SQLiteStatement insertStatement = null;
    SQLiteStatement testStatement = null;


    SQLiteStatement deleteStatement =  null;
    SQLiteStatement deleteValueStatement = null;
    SQLiteStatement updateSpecificStatement = null;
    SQLiteStatement updateSpecificAccessStatement = null;
    SQLiteStatement deleteOldDataStatement = null;


    SQLiteStatement statement = null;

    Context mContext;

    public AndroidLispDAI(Context con)
    {
        mContext = con;
        try
        {
            appDB  = con.openOrCreateDatabase("object-map",Context.MODE_PRIVATE,null);
            appDB.setLocale(Locale.getDefault());
            appDB.setLockingEnabled(true);
            appDB.setVersion(1);


            if (rebuildTable){
                SQLiteStatement dropStatement = appDB.compileStatement("drop table if exists objects");
                dropStatement.execute();

            }
            statement = appDB.compileStatement(_STANDARD_CREATE_MAIN_TABLE);
            statement.execute();

            insertStatement = appDB.compileStatement(_INSERT_KEY);
            testStatement = appDB.compileStatement(_TEST_KEY);

            deleteStatement = appDB.compileStatement(_DELETE_ALL_KEY);
            deleteValueStatement = appDB.compileStatement(_DELETE_DATA_KEY);

            updateSpecificStatement = appDB.compileStatement(_UPDATE_SPECIFIC_DATA_KEY);

            updateSpecificAccessStatement = appDB.compileStatement(_UPDATE_SPECIFIC_ACCESS_DATA_KEY);
            deleteOldDataStatement = appDB.compileStatement(_DELETE_ALL_AGED_KEY);

        }
        catch (Exception e)
        {
            throw new RuntimeException(e);
        }
    }

    @Override
    public String getData(String key, String context, boolean updateLastAccessP)
    {
        Long lastAccess = null;
        if (updateLastAccessP)
            lastAccess = Long.valueOf(System.currentTimeMillis());

        Cursor rs = appDB.rawQuery(_SELECT_DATA_KEY, new String[]{key, context});
        try
        {
            if (rs.moveToNext())
            {
                String result = rs.getString(0);

                if (lastAccess != null)
                {
                    Long rowid = rs.getLong(1);
                    updateSpecificAccessStatement.bindLong(1, lastAccess.longValue());
                    updateSpecificAccessStatement.bindLong(2, rowid.longValue());
                    updateSpecificAccessStatement.executeUpdateDelete();
                }

                return result;
            }
            else
                return null;
        }
        finally
        {
            rs.close();
        }
    }

    @Override
    public String getData(String key, String context)
    {
        return getData(key, context, false);
    }

    @Override
    public String[] getAllKeys(String context)
    {
        Cursor rs = appDB.rawQuery(_SELECT_ALL_NAMES_KEY, new String[]{context});

        LinkedList<String> value = new LinkedList<String>();
        try
        {
            while (rs.moveToNext())
            {
                value.add(rs.getString(0));
            }
            return value.toArray(new String[0]);
        }
        finally
        {
            rs.close();
        }

    }

    @Override
    public boolean hasData(String key, String context)
    {
        Cursor cursor = appDB.rawQuery(_TEST_KEY, new String[]{key, context});
        try
        {
            if (cursor.moveToNext())
            {
                return true;
            }
            else
                return false;
        }
        finally
        {
            cursor.close();
        }
    }



    @Override
    public boolean deleteData(String key, String context)
    {
        deleteValueStatement.bindString(1, key);
        deleteValueStatement.bindString(2, context);
        return deleteValueStatement.executeUpdateDelete() > 0;
    }

    @Override
    public int deleteAllData()
    {
        return deleteStatement.executeUpdateDelete();
    }

    @Override
    public int setData(String key, String context, String data)
    {
        Long timeStamp = System.currentTimeMillis();

        Integer rowId;
        int modifyCount = 0;

        try
        {
            rowId = testDataExists(key, context);
            if (rowId != null)
            {
                modifyCount = (int)updateValue(updateSpecificStatement, data, timeStamp, rowId);
            }
            else
            {
                modifyCount = (int)insertUpdateValue(insertStatement, key, context, data, timeStamp);
            }

            return modifyCount;
        }
        catch (Exception e)
        {
            throw new RuntimeException(e);
        }
    }

    public int deleteOldData(String context, long age)
    {
        deleteOldDataStatement.bindString(1, context);
        deleteOldDataStatement.bindLong(2, age);
        return deleteOldDataStatement.executeUpdateDelete();

    }


    public String[] selectOldDataNames(String context, long cutoffTime)
    {
        Cursor rs = appDB.rawQuery(_SELECT_ALL_AGED_KEY, new String[]{context, "" + cutoffTime});

        LinkedList<String> out = new LinkedList<String>();

        try
        {
            while (rs.moveToNext())
            {
                out.add(rs.getString(0));
            }
            return out.toArray(new String[0]);
        }
        finally{
            rs.close();
        }
    }

    public String[] selectNewDataNames(String context, long startTime)
    {
        Cursor rs = appDB.rawQuery(_SELECT_ALL_NEW_KEY, new String[]{context, "" + startTime});

        LinkedList<String> out = new LinkedList<String>();

        try
        {
            while (rs.moveToNext())
            {
                out.add(rs.getString(0));
            }
            return out.toArray(new String[0]);
        }
        finally{
            rs.close();
        }
    }


    private Integer testDataExists(String key, String context)
    {
        Cursor cursor = appDB.rawQuery(_TEST_KEY, new String[]{key, context});
        try
        {
            if (cursor.moveToNext())
            {
                return cursor.getInt(0);
            }
            else
                return null;
        }
        finally
        {
            cursor.close();
        }
    }

    long insertUpdateValue(SQLiteStatement insert,  String name, String context, String value, long timeStamp) throws SQLException
    {
        insert.bindString(1, name);
        insert.bindString(2, context);
        insert.bindString(3, value);
        insert.bindLong(4, timeStamp);



        return insert.executeInsert();
    }


    long updateValue(SQLiteStatement update, String value, Long lastAccess, long rowid) throws SQLException
    {
        update.bindString(1, value);

        if (lastAccess == null)
            update.bindNull(2);
        else
            update.bindLong(2, lastAccess.longValue());
        update.bindLong(3, rowid);


        return update.executeUpdateDelete();
    }

}
