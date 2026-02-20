using GameSrv.Config;

namespace GameSrv.Server;

/// <summary>
/// Manages the collection of server listener threads.
/// Ported from RandM.GameSrv.ServerThreadManager.
/// </summary>
public static class ServerThreadManager
{
    private static readonly List<ServerThread> _threads = new();

    public static void StartThreads()
    {
        var config = ServerConfig.Instance;

        if (config.TelnetServerPort > 0)
        {
            var t = new TelnetServerThread(config.TelnetServerIP, config.TelnetServerPort);
            _threads.Add(t);
            t.Start();
        }

        if (config.RLoginServerPort > 0)
        {
            var t = new RLoginServerThread(config.RLoginServerIP, config.RLoginServerPort);
            _threads.Add(t);
            t.Start();
        }

        if (config.WebSocketServerPort > 0)
        {
            var t = new WebSocketServerThread(config.WebSocketServerIP, config.WebSocketServerPort);
            _threads.Add(t);
            t.Start();
        }
    }

    public static void StopThreads()
    {
        foreach (var t in _threads)
            t.Stop();
        foreach (var t in _threads)
            t.Dispose();
        _threads.Clear();
    }

    public static void PauseThreads()
    {
        foreach (var t in _threads)
            t.Paused = true;
    }

    public static void ResumeThreads()
    {
        foreach (var t in _threads)
            t.Paused = false;
    }
}
