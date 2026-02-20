using System.Collections;
using System.Text;
using GameSrv.Config;
using GameSrv.Enums;
using GameSrv.Helpers;
using GameSrv.Session;

namespace GameSrv.Server;

/// <summary>
/// Manages node allocation, active sessions, and who-is-online tracking.
/// Ported from RandM.GameSrv.NodeManager.
/// </summary>
public static class NodeManager
{
    private static readonly Dictionary<int, ClientThread?> _clientThreads = new();
    private static readonly object _listLock = new();

    public static event EventHandler<int>? ConnectionCountChangeEvent;
    public static event EventHandler<NodeEventArgs>? NodeEvent;

    public static int ConnectionCount
    {
        get
        {
            lock (_listLock)
            {
                return _clientThreads.Count(kv => kv.Value != null);
            }
        }
    }

    public static void Start()
    {
        lock (_listLock)
        {
            _clientThreads.Clear();
            for (int node = ServerConfig.Instance.FirstNode; node <= ServerConfig.Instance.LastNode; node++)
            {
                _clientThreads[node] = null;
            }
        }
        UpdateConnectionCount();
        UpdateWhoIsOnlineFile();
    }

    public static void Stop()
    {
        lock (_listLock)
        {
            for (int node = ServerConfig.Instance.FirstNode; node <= ServerConfig.Instance.LastNode; node++)
            {
                if (_clientThreads[node] != null)
                {
                    _clientThreads[node]!.Stop();
                    _clientThreads[node] = null;
                }
            }
        }
        UpdateConnectionCount();
        UpdateWhoIsOnlineFile();
    }

    public static int GetFreeNode(ClientThread clientThread)
    {
        int result = 0;
        bool raise = false;

        lock (_listLock)
        {
            for (int i = ServerConfig.Instance.FirstNode; i <= ServerConfig.Instance.LastNode; i++)
            {
                if (_clientThreads[i] == null)
                {
                    clientThread.FinishEvent += ClientThread_FinishEvent;
                    clientThread.NodeEvent += ClientThread_NodeEvent;
                    clientThread.WhoIsOnlineEvent += ClientThread_WhosOnlineEvent;
                    _clientThreads[i] = clientThread;
                    result = i;
                    raise = true;
                    break;
                }
            }
        }

        if (raise) UpdateConnectionCount();
        return result;
    }

    public static void DisconnectNode(int node)
    {
        bool raise = false;
        if (!IsValidNode(node)) return;

        lock (_listLock)
        {
            if (_clientThreads[node] != null)
            {
                _clientThreads[node]!.Stop();
                _clientThreads[node] = null;
                raise = true;
            }
        }

        if (raise)
        {
            UpdateConnectionCount();
            UpdateWhoIsOnlineFile();
        }
    }

    public static void KillOtherSession(string alias, int node)
    {
        if (string.IsNullOrEmpty(alias)) return;

        int nodeToKill = 0;
        lock (_listLock)
        {
            for (int i = ServerConfig.Instance.FirstNode; i <= ServerConfig.Instance.LastNode; i++)
            {
                if (i != node && _clientThreads[i] != null &&
                    string.Equals(_clientThreads[i]!.Alias, alias, StringComparison.OrdinalIgnoreCase))
                {
                    nodeToKill = i;
                    break;
                }
            }
        }

        if (nodeToKill > 0)
        {
            DisplayAnsi("LOGON_TWO_NODES", nodeToKill);
            DisconnectNode(nodeToKill);
        }
    }

    private static void DisplayAnsi(string ansi, int node)
    {
        if (!IsValidNode(node)) return;
        lock (_listLock)
        {
            _clientThreads[node]?.DisplayAnsi(ansi);
        }
    }

    private static bool IsValidNode(int node)
    {
        return node >= ServerConfig.Instance.FirstNode && node <= ServerConfig.Instance.LastNode;
    }

    private static void ClientThread_FinishEvent(object? sender, EventArgs e)
    {
        if (sender is not ClientThread finished) return;

        bool found = false;
        lock (_listLock)
        {
            for (int i = ServerConfig.Instance.FirstNode; i <= ServerConfig.Instance.LastNode; i++)
            {
                if (_clientThreads[i] == finished)
                {
                    _clientThreads[i]?.Dispose();
                    _clientThreads[i] = null;
                    found = true;
                    break;
                }
            }
        }

        if (found)
        {
            UpdateConnectionCount();
            UpdateWhoIsOnlineFile();
        }
    }

    private static void ClientThread_NodeEvent(object? sender, NodeEventArgs e)
    {
        if (e.EventType == NodeEventType.LogOn)
        {
            if (e.NodeInfo.User.UserId > 0 && !e.NodeInfo.User.AllowMultipleConnections)
            {
                KillOtherSession(e.NodeInfo.User.Alias, e.NodeInfo.Node);
            }
        }

        NodeEvent?.Invoke(sender, e);
        UpdateWhoIsOnlineFile();
    }

    private static void ClientThread_WhosOnlineEvent(object? sender, WhoIsOnlineEventArgs e)
    {
        lock (_listLock)
        {
            for (int i = ServerConfig.Instance.FirstNode; i <= ServerConfig.Instance.LastNode; i++)
            {
                if (_clientThreads[i] == null)
                {
                    e.WhoIsOnline.Add($"WHOSONLINE_{i}_ALIAS", "");
                    e.WhoIsOnline.Add($"WHOSONLINE_{i}_IPADDRESS", "");
                    e.WhoIsOnline.Add($"WHOSONLINE_{i}_STATUS", "Waiting for caller");
                }
                else
                {
                    e.WhoIsOnline.Add($"WHOSONLINE_{i}_ALIAS", _clientThreads[i]!.Alias);
                    e.WhoIsOnline.Add($"WHOSONLINE_{i}_IPADDRESS", _clientThreads[i]!.IPAddress);
                    e.WhoIsOnline.Add($"WHOSONLINE_{i}_STATUS", _clientThreads[i]!.Status);
                }
            }
        }
    }

    private static void UpdateConnectionCount()
    {
        ConnectionCountChangeEvent?.Invoke(null, ConnectionCount);
    }

    private static void UpdateWhoIsOnlineFile()
    {
        try
        {
            var sb = new StringBuilder();
            sb.AppendLine("Node,RemoteIP,User,Status");
            lock (_listLock)
            {
                for (int node = ServerConfig.Instance.FirstNode; node <= ServerConfig.Instance.LastNode; node++)
                {
                    if (_clientThreads[node] == null)
                        sb.AppendLine($"{node}\t\t\tWaiting for caller");
                    else
                        sb.AppendLine($"{node}\t{_clientThreads[node]!.IPAddress}\t{_clientThreads[node]!.Alias}\t{_clientThreads[node]!.Status}");
                }
            }
            string path = Path.Combine(StringHelper.StartupPath, "whoisonline.txt");
            File.WriteAllText(path, sb.ToString());
        }
        catch (Exception ex)
        {
            Log.Exception(ex, "Unable to update whoisonline.txt");
        }
    }
}
