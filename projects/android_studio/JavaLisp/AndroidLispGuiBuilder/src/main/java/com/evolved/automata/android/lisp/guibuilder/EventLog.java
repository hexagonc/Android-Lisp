package com.evolved.automata.android.lisp.guibuilder;

import android.util.Log;

import com.evolved.automata.android.lisp.guibuilder.model.ALGB;
import com.evolved.automata.android.lisp.guibuilder.model.Page;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Calendar;

/**
 * Created by Evolved8 on 5/28/17.
 */

public class EventLog {


    public static class ShowEventLogEvent
    {

    }

    ALGB mApplication;

    private static EventLog mInternalLog;

    public enum LispSourceType
    {
        FOREGROUND, BACKGROUND
    }

    public enum EntryType
    {
        ERROR, INFO, RESULT
    }

    public enum EventSourceType
    {
        SYSTEM, PAGE, UNSPECIFIED, LISP_PAGE, LISP_GLOBAL
    }


    public static abstract class EventSource
    {
        EventSourceType _type;
        protected EventSource(EventSourceType type)
        {
            _type = type;
        }

        public EventSourceType getType()
        {
            return _type;
        }

        public abstract String getNameShort();

        public abstract String getNameLong();

    }

    public static class PageSource extends EventSource
    {
        final Page _page;
        public PageSource(Page page)
        {
            super(EventSourceType.PAGE);
            _page = page;
        }

        public String getNameShort()
        {
            return String.format("Title: %1%s", _page.getTitle());
        }

        public String getNameLong()
        {
            return String.format("Title: %1$s\n Page Id: %2$s\n", _page.getTitle(), _page.getPageId());
        }
    }


    public static class LispPageSource extends EventSource
    {
        final Page _page;
        final LispSourceType _lsType;
        public LispPageSource(LispSourceType lsType, Page page)
        {
            super(EventSourceType.LISP_PAGE);
            _page = page;
            _lsType = lsType;
        }

        public String getNameShort()
        {
            return String.format("[%1$s] Title: %2%s", _lsType.name(), _page.getTitle());
        }

        public String getNameLong()
        {
            return String.format("[%1$s] Title: %2$s\n Page Id: %3$s\n", _lsType.name(), _page.getTitle(), _page.getPageId());
        }
    }

    public static class LispGlobalSource extends EventSource
    {

        public LispGlobalSource()
        {
            super(EventSourceType.LISP_PAGE);

        }

        public String getNameShort()
        {
            return _type.name();

        }

        public String getNameLong()
        {
            return _type.name();
        }
    }

    public static class SystemSource extends EventSource
    {

        public SystemSource()
        {
            super(EventSourceType.SYSTEM);

        }

        public String getNameShort()
        {
            return _type.name();

        }

        public String getNameLong()
        {
            return _type.name();
        }
    }


    public static class UnspecifiedSource extends EventSource
    {
        final String _longDesc;
        final String _shortDesc;
        public UnspecifiedSource(String descShort, String descLong)
        {
            super(EventSourceType.UNSPECIFIED);
            _longDesc = descLong;
            _shortDesc = descShort;
        }

        public String getNameShort()
        {
            return _shortDesc;
        }

        public String getNameLong()
        {
            return _longDesc;
        }
    }


    public static class LogEntry
    {

        final long _time;
        final EntryType _type;
        final EventSource _source;
        final String _summary;
        final String _detail;

        final String _timeString;
        final String _datetimeString;

        public LogEntry(EventSource source, long createTime, EntryType type, String summary, String detail)
        {
            _type = type;
            _time = createTime;
            _source = source;
            _summary = summary;
            _detail = detail;

            Calendar cal = Calendar.getInstance();
            cal.setTimeInMillis(_time);

            int month = cal.get(Calendar.MONTH) + 1;
            int day = cal.get(Calendar.DAY_OF_MONTH);
            int hour = cal.get(Calendar.HOUR_OF_DAY);
            int minute = cal.get(Calendar.MINUTE);
            int seconds = cal.get(Calendar.SECOND);
            int milli = cal.get(Calendar.MILLISECOND);

            String smonth = "" + month;
            String sday =  StringUtils.right( "0" + day, 2);
            String shour = StringUtils.right("0" + hour, 2);
            String sminute = StringUtils.right("0" + minute, 2);
            String sseconds = StringUtils.right("0" + seconds, 2);
            String smilli = ""+ milli;



            _timeString = StringUtils.join(new String[]{shour, sminute, sseconds, smilli}, ':');
            _datetimeString = String.format("%1$s/%2$s ", sday, smonth) + _timeString;
        }


        public EntryType getType()
        {
            return _type;
        }

        public String getSummary()
        {
            return _summary;
        }

        public String getDetailed()
        {
            return _detail;
        }

        public long getCreationTime()
        {
            return _time;
        }

        public String getDateTime()
        {
            return _datetimeString;
        }

        public String getTime()
        {
            return _timeString;
        }

    }

    ArrayList<LogEntry> mLogHistory;

    private EventLog(ALGB app)
    {
        mApplication = app;
        mLogHistory = new ArrayList<LogEntry>();
    }

    public static EventLog create(ALGB app)
    {
        if (mInternalLog == null)
        {
            mInternalLog = new EventLog(app);
        }
        return mInternalLog;
    }

    public static EventLog get()
    {
        return mInternalLog;
    }

    public ArrayList<LogEntry> getEntries()
    {
        return mLogHistory;
    }

    public void clearEntries()
    {
        mLogHistory.clear();
    }

    public void logSystemInfo(String summary)
    {
        logEvent(new EventLog.SystemSource(), EntryType.INFO, summary);
    }


    public void logSystemInfo(String summary, String detail)
    {
        logEvent(new EventLog.SystemSource(), EntryType.INFO, summary, detail);
    }


    public void logSystemError(Throwable e)
    {
        logEvent(new EventLog.SystemSource(), EventLog.EntryType.ERROR, e.toString());
    }


    public void logSystemError(String desc)
    {
        logEvent(new EventLog.SystemSource(), EventLog.EntryType.ERROR, desc, desc);
    }

    public void logSystemError(Throwable e, String desc)
    {
        logEvent(new EventLog.SystemSource(), EventLog.EntryType.ERROR, desc, e.toString());
    }

    public void logEvent(EventSource source, EntryType type, String summary)
    {
        logEvent(source, type, summary, summary);
    }

    public void logEvent(EventSource source, EntryType type, String summary, String detail)
    {
        long createTime = System.currentTimeMillis();

        LogEntry entry = new LogEntry(source, createTime, type, summary, detail);

        mLogHistory.add(entry);

        GlobalStatusAlertEvent event = null;
        switch (type)
        {
            case ERROR:
                event = new NewErrorLogEntriesEvent();
                break;
            case RESULT:
                event = new NewBackgroundResultEvent();
                break;
            case INFO:
                event = new NewInfoLogEntriesEvent();
                break;
        }

        Tools.postEvent(event);

    }

    public void logcatError(String key, String message)
    {
        Log.e(key, message);
    }

    public void logcatDebug(String key, String message)
    {
        Log.d(key, message);
    }

    public void showLog()
    {
        Tools.postEvent(new ShowEventLogEvent());
    }

}
