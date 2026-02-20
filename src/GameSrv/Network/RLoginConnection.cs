using System.Net;
using System.Net.Sockets;
using System.Text;

namespace GameSrv.Network;

/// <summary>
/// RLogin protocol connection (RFC 1282).
/// Replaces RMLib.RLoginConnection.
/// </summary>
public class RLoginConnection : IConnection
{
    private Socket? _socket;
    private readonly object _writeLock = new();
    private bool _disposed;

    public string ServerUserName { get; private set; } = "";
    public string ClientUserName { get; private set; } = "";
    public string TerminalType { get; private set; } = "";

    public bool Connected => _socket?.Connected ?? false;
    public bool ReadTimedOut { get; private set; }
    public string LineEnding { get; set; } = "\r\n";
    public bool StripLF { get; set; }
    public nint Handle => _socket?.Handle ?? IntPtr.Zero;

    public RLoginConnection() { }

    public RLoginConnection(Socket socket)
    {
        _socket = socket;
        _socket.NoDelay = true;
    }

    /// <summary>
    /// Parse the RLogin header. Format: \0ClientUser\0ServerUser\0TermType/BaudRate\0
    /// Returns true if header was successfully parsed.
    /// </summary>
    public bool Open(int timeoutMs = 5000)
    {
        if (_socket == null) return false;

        try
        {
            // Read the RLogin header with timeout
            var header = new List<byte>();
            int nullCount = 0;
            int elapsed = 0;

            while (nullCount < 4 && elapsed < timeoutMs)
            {
                if (_socket.Poll(100000, SelectMode.SelectRead) && _socket.Available > 0) // 100ms
                {
                    byte[] buf = new byte[1];
                    int n = _socket.Receive(buf, 0, 1, SocketFlags.None);
                    if (n <= 0) return false;

                    header.Add(buf[0]);
                    if (buf[0] == 0) nullCount++;
                    elapsed = 0; // Reset timeout on data received
                }
                else
                {
                    elapsed += 100;
                }
            }

            if (nullCount < 4) return false;

            // Parse: \0ClientUser\0ServerUser\0TermType/BaudRate\0
            string[] parts = Encoding.ASCII.GetString(header.ToArray()).Split('\0');
            // parts[0] = "" (leading null), parts[1] = ClientUser, parts[2] = ServerUser, parts[3] = TermType/BaudRate
            if (parts.Length >= 4)
            {
                ClientUserName = parts[1];
                ServerUserName = parts[2];
                string termPart = parts[3];
                // Strip baud rate suffix if present (e.g., "xterm/38400" -> "xterm")
                int slashIdx = termPart.IndexOf('/');
                TerminalType = slashIdx >= 0 ? termPart.Substring(0, slashIdx) : termPart;
            }

            // Send acknowledgment
            WriteRawBytes(new byte[] { 0 });
            return true;
        }
        catch
        {
            return false;
        }
    }

    public bool Connect(string host, int port)
    {
        try
        {
            _socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            _socket.NoDelay = true;
            _socket.Connect(host, port);
            return true;
        }
        catch
        {
            return false;
        }
    }

    public void Close()
    {
        try { _socket?.Shutdown(SocketShutdown.Both); } catch { }
        try { _socket?.Close(); } catch { }
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
        if (_socket == null || !_socket.Connected) return null;

        try
        {
            if (!_socket.Poll(timeoutMs * 1000, SelectMode.SelectRead))
            {
                ReadTimedOut = true;
                return null;
            }

            byte[] buf = new byte[1];
            int n = _socket.Receive(buf, 0, 1, SocketFlags.None);
            if (n <= 0) return null;
            if (StripLF && buf[0] == 10) return ReadChar(timeoutMs);
            return (char)buf[0];
        }
        catch
        {
            return null;
        }
    }

    public string ReadString()
    {
        if (_socket == null || !_socket.Connected || _socket.Available <= 0) return "";
        try
        {
            byte[] buf = new byte[_socket.Available];
            int n = _socket.Receive(buf, 0, buf.Length, SocketFlags.None);
            if (n <= 0) return "";
            string result = Encoding.ASCII.GetString(buf, 0, n);
            if (StripLF) result = result.Replace("\n", "");
            return result;
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
            if (passwordChar != '\0')
                Write(passwordChar.ToString());
            else
                Write(ch!.Value.ToString());
        }
    }

    public byte[] ReadBytes()
    {
        if (_socket == null || !_socket.Connected || _socket.Available <= 0)
            return Array.Empty<byte>();

        try
        {
            byte[] buf = new byte[_socket.Available];
            int n = _socket.Receive(buf, 0, buf.Length, SocketFlags.None);
            if (n <= 0) return Array.Empty<byte>();
            if (n < buf.Length) Array.Resize(ref buf, n);
            return buf;
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

    private static readonly Encoding _cp437 = Encoding.GetEncoding(437);

    public void Write(string text)
    {
        if (string.IsNullOrEmpty(text)) return;
        WriteRawBytes(_cp437.GetBytes(text));
    }

    public void WriteLn(string text = "")
    {
        Write(text + "\r\n");
    }

    public void WriteBytes(byte[] data, int count)
    {
        if (data == null || count <= 0) return;
        byte[] toSend = count == data.Length ? data : data.Take(count).ToArray();
        WriteRawBytes(toSend);
    }

    public void SetBlocking(bool blocking)
    {
        if (_socket != null) _socket.Blocking = blocking;
    }

    public void SendGoAhead()
    {
        // RLogin doesn't have Go-Ahead; send a null byte as keepalive
        try { WriteRawBytes(new byte[] { 0 }); } catch { }
    }

    private void WriteRawBytes(byte[] data)
    {
        if (_socket == null || !_socket.Connected) return;
        lock (_writeLock)
        {
            try
            {
                int sent = 0;
                while (sent < data.Length)
                    sent += _socket.Send(data, sent, data.Length - sent, SocketFlags.None);
            }
            catch { }
        }
    }

    public void Dispose()
    {
        if (!_disposed)
        {
            _disposed = true;
            Close();
            _socket?.Dispose();
        }
    }
}
