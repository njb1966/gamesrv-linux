namespace GameSrv.Helpers;

public enum LogLevel
{
    Debug,
    Info,
    Warning,
    Error
}

public class LogEventArgs : EventArgs
{
    public LogLevel Level { get; }
    public string Message { get; }
    public Exception? Exception { get; }

    public LogEventArgs(LogLevel level, string message, Exception? exception = null)
    {
        Level = level;
        Message = message;
        Exception = exception;
    }
}

/// <summary>
/// Static logger replacing RMLib.RMLog.
/// Provides event-based logging so console and file handlers can subscribe.
/// </summary>
public static class Log
{
    public static LogLevel Level { get; set; } = LogLevel.Info;

    public static event EventHandler<LogEventArgs>? Handler;

    public static void Debug(string message)
    {
        if (Level <= LogLevel.Debug)
            Raise(LogLevel.Debug, message);
    }

    public static void Info(string message)
    {
        if (Level <= LogLevel.Info)
            Raise(LogLevel.Info, message);
    }

    public static void Warning(string message)
    {
        if (Level <= LogLevel.Warning)
            Raise(LogLevel.Warning, message);
    }

    public static void Error(string message)
    {
        Raise(LogLevel.Error, message);
    }

    public static void Exception(Exception ex, string message)
    {
        Raise(LogLevel.Error, $"{message}: {ex.Message}", ex);
    }

    private static void Raise(LogLevel level, string message, Exception? ex = null)
    {
        Handler?.Invoke(null, new LogEventArgs(level, message, ex));
    }
}
