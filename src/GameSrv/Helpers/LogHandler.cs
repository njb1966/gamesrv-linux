namespace GameSrv.Helpers;

/// <summary>
/// Log handler that writes to both console and a log file.
/// Buffers file writes and flushes periodically.
/// </summary>
public class LogHandler : IDisposable
{
    private readonly string _logDir;
    private readonly List<string> _buffer = new();
    private readonly object _lock = new();
    private readonly Timer _flushTimer;
    private readonly string _timeFormat;

    public LogHandler(string timeFormat = "G")
    {
        _timeFormat = timeFormat;
        _logDir = Path.Combine(StringHelper.StartupPath, "logs");
        Directory.CreateDirectory(_logDir);
        _flushTimer = new Timer(_ => Flush(), null, TimeSpan.FromSeconds(60), TimeSpan.FromSeconds(60));
        Log.Handler += OnLogEvent;
    }

    private void OnLogEvent(object? sender, LogEventArgs e)
    {
        string timestamp = DateTime.Now.ToString(_timeFormat);
        string line = $"{timestamp}  [{e.Level}]  {e.Message}";

        // Write to console
        var prevColor = Console.ForegroundColor;
        Console.ForegroundColor = e.Level switch
        {
            LogLevel.Debug => ConsoleColor.Gray,
            LogLevel.Info => ConsoleColor.White,
            LogLevel.Warning => ConsoleColor.Yellow,
            LogLevel.Error => ConsoleColor.Red,
            _ => ConsoleColor.White,
        };
        Console.WriteLine(line);
        Console.ForegroundColor = prevColor;

        if (e.Exception != null)
        {
            Console.ForegroundColor = ConsoleColor.DarkRed;
            Console.WriteLine($"  {e.Exception.GetType().Name}: {e.Exception.Message}");
            Console.ForegroundColor = prevColor;
        }

        // Buffer for file
        lock (_lock)
        {
            _buffer.Add(line);
            if (e.Exception != null)
            {
                _buffer.Add($"  {e.Exception.GetType().Name}: {e.Exception.Message}");
                if (e.Exception.StackTrace != null)
                    _buffer.Add($"  {e.Exception.StackTrace}");
            }
        }
    }

    public void Flush()
    {
        List<string> toWrite;
        lock (_lock)
        {
            if (_buffer.Count == 0) return;
            toWrite = new List<string>(_buffer);
            _buffer.Clear();
        }

        try
        {
            string logFile = Path.Combine(_logDir, "gamesrv.log");
            File.AppendAllLines(logFile, toWrite);
        }
        catch
        {
            // If we can't write to log file, silently continue
        }
    }

    public void Dispose()
    {
        _flushTimer.Dispose();
        Log.Handler -= OnLogEvent;
        Flush();
    }
}
