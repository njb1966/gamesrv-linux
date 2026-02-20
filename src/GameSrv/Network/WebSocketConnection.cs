using System.Net;
using System.Net.Sockets;
using System.Net.WebSockets;
using System.Security.Cryptography;
using System.Text;

namespace GameSrv.Network;

/// <summary>
/// WebSocket connection with HTTP upgrade handshake.
/// Replaces RMLib.WebSocketConnection.
/// </summary>
public class WebSocketConnection : IConnection
{
    private Socket? _socket;
    private WebSocket? _webSocket;
    private readonly object _writeLock = new();
    private static readonly Encoding _cp437 = Encoding.GetEncoding(437);
    private bool _disposed;
    private readonly CancellationTokenSource _cts = new();
    private readonly byte[] _readBuffer = new byte[4096];

    public bool Connected => _webSocket?.State == WebSocketState.Open;
    public bool ReadTimedOut { get; private set; }
    public string LineEnding { get; set; } = "\r\n";
    public bool StripLF { get; set; }
    public nint Handle => _socket?.Handle ?? IntPtr.Zero;

    public WebSocketConnection() { }

    public WebSocketConnection(Socket socket)
    {
        _socket = socket;
        _socket.NoDelay = true;
    }

    /// <summary>
    /// Perform the HTTP WebSocket upgrade handshake.
    /// Must be called after construction before using the connection.
    /// </summary>
    public bool PerformHandshake(int timeoutMs = 5000)
    {
        if (_socket == null) return false;

        try
        {
            // Read the HTTP upgrade request
            var request = new StringBuilder();
            var buf = new byte[1];
            int elapsed = 0;
            while (elapsed < timeoutMs)
            {
                if (_socket.Poll(100000, SelectMode.SelectRead) && _socket.Available > 0)
                {
                    int n = _socket.Receive(buf, 0, 1, SocketFlags.None);
                    if (n <= 0) return false;
                    request.Append((char)buf[0]);
                    elapsed = 0;

                    // Check for end of HTTP headers
                    if (request.Length >= 4 && request.ToString().EndsWith("\r\n\r\n"))
                        break;
                }
                else
                {
                    elapsed += 100;
                }
            }

            string requestStr = request.ToString();
            if (!requestStr.Contains("Upgrade: websocket", StringComparison.OrdinalIgnoreCase))
                return false;

            // Extract Sec-WebSocket-Key
            string? wsKey = null;
            foreach (string line in requestStr.Split("\r\n"))
            {
                if (line.StartsWith("Sec-WebSocket-Key:", StringComparison.OrdinalIgnoreCase))
                {
                    wsKey = line.Substring(18).Trim();
                    break;
                }
            }

            if (wsKey == null) return false;

            // Compute accept key
            string acceptKey = ComputeAcceptKey(wsKey);

            // Send 101 Switching Protocols response
            string response = "HTTP/1.1 101 Switching Protocols\r\n" +
                              "Upgrade: websocket\r\n" +
                              "Connection: Upgrade\r\n" +
                              $"Sec-WebSocket-Accept: {acceptKey}\r\n" +
                              "\r\n";
            byte[] responseBytes = Encoding.ASCII.GetBytes(response);
            _socket.Send(responseBytes);

            // Create WebSocket from the stream
            var stream = new NetworkStream(_socket, ownsSocket: false);
            _webSocket = WebSocket.CreateFromStream(stream, new WebSocketCreationOptions
            {
                IsServer = true,
                KeepAliveInterval = TimeSpan.FromSeconds(30)
            });

            return true;
        }
        catch
        {
            return false;
        }
    }

    private static string ComputeAcceptKey(string key)
    {
        string combined = key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
        byte[] hash = SHA1.HashData(Encoding.ASCII.GetBytes(combined));
        return Convert.ToBase64String(hash);
    }

    public bool Connect(string host, int port)
    {
        // Outbound WebSocket not supported
        return false;
    }

    public void Close()
    {
        try
        {
            if (_webSocket?.State == WebSocketState.Open)
            {
                _webSocket.CloseAsync(WebSocketCloseStatus.NormalClosure, "", CancellationToken.None)
                    .Wait(TimeSpan.FromSeconds(2));
            }
        }
        catch { }
        try { _socket?.Close(); } catch { }
        _cts.Cancel();
    }

    public string GetRemoteIP()
    {
        try
        {
            if (_socket?.RemoteEndPoint is IPEndPoint ep) return ep.Address.ToString();
        }
        catch { }
        return "0.0.0.0";
    }

    public int GetRemotePort()
    {
        try
        {
            if (_socket?.RemoteEndPoint is IPEndPoint ep) return ep.Port;
        }
        catch { }
        return 0;
    }

    public char? ReadChar(int timeoutMs)
    {
        ReadTimedOut = false;
        if (_webSocket == null || _webSocket.State != WebSocketState.Open) return null;

        try
        {
            using var cts = CancellationTokenSource.CreateLinkedTokenSource(_cts.Token);
            cts.CancelAfter(timeoutMs);

            var segment = new ArraySegment<byte>(_readBuffer);
            var result = _webSocket.ReceiveAsync(segment, cts.Token).GetAwaiter().GetResult();

            if (result.MessageType == WebSocketMessageType.Close)
            {
                Close();
                return null;
            }

            if (result.Count > 0)
            {
                byte b = _readBuffer[0];
                if (StripLF && b == 10) return ReadChar(timeoutMs);
                return (char)b;
            }
            return null;
        }
        catch (OperationCanceledException)
        {
            ReadTimedOut = true;
            return null;
        }
        catch
        {
            return null;
        }
    }

    public string ReadString()
    {
        if (_webSocket == null || _webSocket.State != WebSocketState.Open) return "";

        try
        {
            if (!_socket!.Poll(0, SelectMode.SelectRead) || _socket.Available <= 0) return "";

            var segment = new ArraySegment<byte>(_readBuffer);
            using var cts = CancellationTokenSource.CreateLinkedTokenSource(_cts.Token);
            cts.CancelAfter(100);

            var result = _webSocket.ReceiveAsync(segment, cts.Token).GetAwaiter().GetResult();
            if (result.MessageType == WebSocketMessageType.Close)
            {
                Close();
                return "";
            }

            string text = Encoding.ASCII.GetString(_readBuffer, 0, result.Count);
            if (StripLF) text = text.Replace("\n", "");
            return text;
        }
        catch { return ""; }
    }

    public string ReadLn(char passwordChar, int timeoutMs)
    {
        ReadTimedOut = false;
        var result = new StringBuilder();

        while (true)
        {
            char? ch = ReadChar(timeoutMs);
            if (ch == null)
            {
                ReadTimedOut = true;
                return result.ToString();
            }

            if (ch == '\r' || ch == '\n')
            {
                WriteLn();
                return result.ToString();
            }

            if (ch == '\b' || ch == 127)
            {
                if (result.Length > 0)
                {
                    result.Remove(result.Length - 1, 1);
                    Write("\b \b");
                }
                continue;
            }

            if (ch < 32) continue;

            result.Append(ch);
            Write(passwordChar != '\0' ? passwordChar.ToString() : ch!.Value.ToString());
        }
    }

    public byte[] ReadBytes()
    {
        if (_webSocket == null || _webSocket.State != WebSocketState.Open) return Array.Empty<byte>();

        try
        {
            if (!_socket!.Poll(0, SelectMode.SelectRead) || _socket.Available <= 0)
                return Array.Empty<byte>();

            var segment = new ArraySegment<byte>(_readBuffer);
            using var cts = CancellationTokenSource.CreateLinkedTokenSource(_cts.Token);
            cts.CancelAfter(100);

            var result = _webSocket.ReceiveAsync(segment, cts.Token).GetAwaiter().GetResult();
            if (result.Count <= 0) return Array.Empty<byte>();

            byte[] data = new byte[result.Count];
            Array.Copy(_readBuffer, data, result.Count);
            return data;
        }
        catch { return Array.Empty<byte>(); }
    }

    public bool CanRead(int timeoutMs = 0)
    {
        if (_socket == null || !_socket.Connected) return false;
        try
        {
            return _socket.Poll(timeoutMs * 1000, SelectMode.SelectRead) && _socket.Available > 0;
        }
        catch { return false; }
    }

    public void Write(string text)
    {
        if (string.IsNullOrEmpty(text) || _webSocket == null || _webSocket.State != WebSocketState.Open) return;
        byte[] data = _cp437.GetBytes(text);
        lock (_writeLock)
        {
            try
            {
                _webSocket.SendAsync(new ArraySegment<byte>(data), WebSocketMessageType.Text, true, _cts.Token)
                    .GetAwaiter().GetResult();
            }
            catch { }
        }
    }

    public void WriteLn(string text = "")
    {
        Write(text + "\r\n");
    }

    public void WriteBytes(byte[] data, int count)
    {
        if (data == null || count <= 0 || _webSocket == null || _webSocket.State != WebSocketState.Open) return;
        lock (_writeLock)
        {
            try
            {
                _webSocket.SendAsync(new ArraySegment<byte>(data, 0, count), WebSocketMessageType.Binary, true, _cts.Token)
                    .GetAwaiter().GetResult();
            }
            catch { }
        }
    }

    public void SetBlocking(bool blocking)
    {
        if (_socket != null) _socket.Blocking = blocking;
    }

    public void SendGoAhead()
    {
        // WebSocket doesn't have Go-Ahead; send a null byte as keepalive
        try { Write("\0"); } catch { }
    }

    public void Dispose()
    {
        if (!_disposed)
        {
            _disposed = true;
            _cts.Cancel();
            Close();
            _webSocket?.Dispose();
            _socket?.Dispose();
            _cts.Dispose();
        }
    }
}
