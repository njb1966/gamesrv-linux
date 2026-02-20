using GameSrv.Config;
using GameSrv.Enums;
using GameSrv.Events;
using GameSrv.Helpers;

namespace GameSrv.Server;

/// <summary>
/// Main application orchestrator. Manages the lifecycle of all server threads,
/// timed events, ignored IPs thread, and node management.
/// Ported from RandM.GameSrv.GameSrv.
/// </summary>
public class GameSrvApp : IDisposable
{
    private LogHandler? _logHandler;
    private TimedEventsThread? _timedEventsThread;
    private IgnoredIPsThread? _ignoredIPsThread;
    private GameSrvStatus _status = GameSrvStatus.Stopped;
    private bool _disposed;

    public event EventHandler<GameSrvStatus>? StatusChanged;

    public GameSrvStatus Status => _status;

    public void Start()
    {
        if (_status == GameSrvStatus.Paused)
        {
            Pause(); // Toggle back to running
            return;
        }

        if (_status != GameSrvStatus.Stopped) return;

        UpdateStatus(GameSrvStatus.Starting);

        try
        {
            // Initialize logging
            _logHandler = new LogHandler(ServerConfig.Instance.TimeFormatLog);

            // Load global settings
            ServerConfig.Instance.Init();

            // Start node manager
            NodeManager.Start();

            // Start server threads (telnet, rlogin, websocket)
            ServerThreadManager.StartThreads();

            // Start ignored IPs thread
            _ignoredIPsThread = new IgnoredIPsThread();
            _ignoredIPsThread.Start();

            // Start timed events thread
            _timedEventsThread = new TimedEventsThread();
            _timedEventsThread.Start();

            UpdateStatus(GameSrvStatus.Started);
            Log.Info($"GameSrv is online - listening on port(s) {ServerConfig.Instance.ServerPorts}");
        }
        catch (Exception ex)
        {
            Log.Exception(ex, "Error starting GameSrv");

            // Undo on failure
            _timedEventsThread?.Stop();
            _ignoredIPsThread?.Stop();
            ServerThreadManager.StopThreads();
            NodeManager.Stop();

            UpdateStatus(GameSrvStatus.Stopped);
        }
    }

    public void Stop()
    {
        if (_status != GameSrvStatus.Paused && _status != GameSrvStatus.Started) return;

        UpdateStatus(GameSrvStatus.Stopping);

        _timedEventsThread?.Stop();
        _timedEventsThread?.Dispose();
        _timedEventsThread = null;

        _ignoredIPsThread?.Stop();
        _ignoredIPsThread?.Dispose();
        _ignoredIPsThread = null;

        ServerThreadManager.StopThreads();
        NodeManager.Stop();

        _logHandler?.Dispose();
        _logHandler = null;

        UpdateStatus(GameSrvStatus.Stopped);
        Log.Info("GameSrv has stopped");
    }

    public void Pause()
    {
        if (_status == GameSrvStatus.Paused)
        {
            UpdateStatus(GameSrvStatus.Resuming);
            ServerThreadManager.ResumeThreads();
            UpdateStatus(GameSrvStatus.Started);
        }
        else if (_status == GameSrvStatus.Started)
        {
            UpdateStatus(GameSrvStatus.Pausing);
            ServerThreadManager.PauseThreads();
            UpdateStatus(GameSrvStatus.Paused);
        }
    }

    private void UpdateStatus(GameSrvStatus newStatus)
    {
        _status = newStatus;
        StatusChanged?.Invoke(this, newStatus);

        string message = newStatus switch
        {
            GameSrvStatus.Paused => "Server(s) are paused",
            GameSrvStatus.Pausing => "Server(s) are pausing...",
            GameSrvStatus.Resuming => "Server(s) are resuming...",
            GameSrvStatus.Started => "Server(s) have started",
            GameSrvStatus.Starting => "Server(s) are starting...",
            GameSrvStatus.Stopped => "Server(s) have stopped",
            GameSrvStatus.Stopping => "Server(s) are stopping...",
            _ => $"Status: {newStatus}"
        };
        Log.Info(message);
    }

    public void Dispose()
    {
        if (!_disposed)
        {
            _disposed = true;
            if (_status != GameSrvStatus.Stopped)
                Stop();
        }
    }
}
