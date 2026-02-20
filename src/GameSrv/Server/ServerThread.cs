using System.Net;
using System.Net.Sockets;
using GameSrv.Enums;
using GameSrv.Helpers;
using GameSrv.Network;
using GameSrv.Session;

namespace GameSrv.Server;

/// <summary>
/// Base class for protocol-specific TCP listeners.
/// Ported from RandM.GameSrv.ServerThread.
/// </summary>
public abstract class ServerThread : IDisposable
{
    protected CancellationTokenSource _cts = new();
    protected Task? _listenerTask;
    protected string _localAddress;
    protected int _localPort;
    protected ConnectionType _connectionType;
    private volatile bool _paused;

    protected ServerThread(string address, int port, ConnectionType connectionType)
    {
        _localAddress = address;
        _localPort = port;
        _connectionType = connectionType;
    }

    public bool Paused
    {
        get => _paused;
        set => _paused = value;
    }

    public void Start()
    {
        _cts = new CancellationTokenSource();
        _listenerTask = Task.Run(() => ExecuteAsync(_cts.Token));
    }

    public void Stop()
    {
        _cts.Cancel();
        try { _listenerTask?.Wait(TimeSpan.FromSeconds(5)); } catch { }
    }

    private async Task ExecuteAsync(CancellationToken ct)
    {
        while (!ct.IsCancellationRequested)
        {
            TcpListener? listener = null;
            try
            {
                var address = IPAddress.Parse(_localAddress);
                listener = new TcpListener(address, _localPort);
                listener.Start();
                Log.Info($"{_connectionType} server listening on {_localAddress}:{_localPort}");

                while (!ct.IsCancellationRequested)
                {
                    // Poll for new connections with 1-second timeout
                    if (listener.Pending())
                    {
                        Socket socket = await listener.AcceptSocketAsync(ct);

                        if (_paused)
                        {
                            // Reject connection when paused
                            try
                            {
                                socket.Send(System.Text.Encoding.ASCII.GetBytes("Server is paused. Please try again later.\r\n"));
                                socket.Close();
                            }
                            catch { }
                            continue;
                        }

                        HandleNewConnection(socket);
                    }
                    else
                    {
                        await Task.Delay(100, ct);
                    }
                }
            }
            catch (OperationCanceledException)
            {
                break;
            }
            catch (Exception ex)
            {
                Log.Exception(ex, $"{_connectionType} server error on port {_localPort}");
                try { listener?.Stop(); } catch { }

                // Retry after 15 seconds
                try { await Task.Delay(15000, ct); } catch (OperationCanceledException) { break; }
            }
            finally
            {
                try { listener?.Stop(); } catch { }
            }
        }
    }

    protected abstract void HandleNewConnection(Socket socket);

    public void Dispose()
    {
        Stop();
        _cts.Dispose();
    }
}

public class TelnetServerThread : ServerThread
{
    public TelnetServerThread(string address, int port)
        : base(address, port, ConnectionType.Telnet) { }

    protected override void HandleNewConnection(Socket socket)
    {
        try
        {
            var connection = new TelnetConnection(socket);

            // Quick carrier detect - if socket closed immediately, it's a portscanner
            if (!connection.Connected)
            {
                Log.Info("No carrier detected (probably a portscanner)");
                connection.Dispose();
                return;
            }

            var client = new ClientThread(connection, ConnectionType.Telnet);
            client.Start();
        }
        catch (Exception ex)
        {
            Log.Exception(ex, "Error handling new telnet connection");
        }
    }
}

public class RLoginServerThread : ServerThread
{
    public RLoginServerThread(string address, int port)
        : base(address, port, ConnectionType.RLogin) { }

    protected override void HandleNewConnection(Socket socket)
    {
        try
        {
            var connection = new RLoginConnection(socket);
            if (!connection.Open())
            {
                Log.Warning("Failed to parse RLogin header from " + connection.GetRemoteIP());
                connection.Dispose();
                return;
            }

            var client = new ClientThread(connection, ConnectionType.RLogin);
            client.Start();
        }
        catch (Exception ex)
        {
            Log.Exception(ex, "Error handling new RLogin connection");
        }
    }
}

public class WebSocketServerThread : ServerThread
{
    public WebSocketServerThread(string address, int port)
        : base(address, port, ConnectionType.WebSocket) { }

    protected override void HandleNewConnection(Socket socket)
    {
        try
        {
            var connection = new WebSocketConnection(socket);
            if (!connection.PerformHandshake())
            {
                Log.Warning("Failed WebSocket handshake from " + connection.GetRemoteIP());
                connection.Dispose();
                return;
            }

            var client = new ClientThread(connection, ConnectionType.WebSocket);
            client.Start();
        }
        catch (Exception ex)
        {
            Log.Exception(ex, "Error handling new WebSocket connection");
        }
    }
}
