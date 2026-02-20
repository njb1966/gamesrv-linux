using System.Net;
using System.Net.Sockets;
using System.Text;

namespace GameSrv.Network;

/// <summary>
/// Telnet protocol connection implementing IAC negotiation.
/// Replaces RMLib.TelnetConnection.
/// </summary>
public class TelnetConnection : IConnection
{
    private Socket? _socket;
    private readonly object _writeLock = new();
    private bool _disposed;

    // Telnet protocol constants
    private const byte IAC = 255;
    private const byte DONT = 254;
    private const byte DO = 253;
    private const byte WONT = 252;
    private const byte WILL = 251;
    private const byte SB = 250;
    private const byte GA = 249;
    private const byte SE = 240;

    // Telnet options
    private const byte OPT_ECHO = 1;
    private const byte OPT_SUPPRESS_GA = 3;
    private const byte OPT_TERMINAL_TYPE = 24;
    private const byte OPT_NAWS = 31;
    private const byte OPT_LINEMODE = 34;

    public bool Connected => _socket?.Connected ?? false;
    public bool ReadTimedOut { get; private set; }
    public string LineEnding { get; set; } = "\r\n";
    public bool StripLF { get; set; }
    public nint Handle => _socket?.Handle ?? IntPtr.Zero;

    public TelnetConnection() { }

    public TelnetConnection(Socket socket)
    {
        _socket = socket;
        _socket.NoDelay = true;
        NegotiateOptions();
    }

    private void NegotiateOptions()
    {
        // Server will echo, suppress go-ahead, and request linemode off
        SendCommand(WILL, OPT_ECHO);
        SendCommand(WILL, OPT_SUPPRESS_GA);
        SendCommand(DO, OPT_SUPPRESS_GA);
        SendCommand(DO, OPT_TERMINAL_TYPE);
        SendCommand(DO, OPT_NAWS);
    }

    private void SendCommand(byte command, byte option)
    {
        WriteRawBytes(new byte[] { IAC, command, option });
    }

    public void SendGoAhead()
    {
        try
        {
            WriteRawBytes(new byte[] { IAC, GA });
        }
        catch { /* Ignore send failures */ }
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
        try
        {
            _socket?.Shutdown(SocketShutdown.Both);
        }
        catch { }
        try
        {
            _socket?.Close();
        }
        catch { }
    }

    public string GetRemoteIP()
    {
        try
        {
            if (_socket?.RemoteEndPoint is IPEndPoint ep)
                return ep.Address.ToString();
        }
        catch { }
        return "0.0.0.0";
    }

    public int GetRemotePort()
    {
        try
        {
            if (_socket?.RemoteEndPoint is IPEndPoint ep)
                return ep.Port;
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
            while (true)
            {
                int n = _socket.Receive(buf, 0, 1, SocketFlags.None);
                if (n <= 0) return null;

                byte b = buf[0];

                // Handle IAC sequences
                if (b == IAC)
                {
                    ProcessIAC();
                    // After processing IAC, try to read another byte
                    if (!_socket.Poll(timeoutMs * 1000, SelectMode.SelectRead))
                    {
                        ReadTimedOut = true;
                        return null;
                    }
                    continue;
                }

                // Strip LF if configured
                if (StripLF && b == 10) continue;

                return (char)b;
            }
        }
        catch
        {
            return null;
        }
    }

    public string ReadString()
    {
        if (_socket == null || !_socket.Connected) return "";

        var sb = new StringBuilder();
        try
        {
            while (_socket.Available > 0)
            {
                byte[] buf = new byte[Math.Min(_socket.Available, 4096)];
                int n = _socket.Receive(buf, 0, buf.Length, SocketFlags.None);
                if (n <= 0) break;

                for (int i = 0; i < n; i++)
                {
                    if (buf[i] == IAC && i + 1 < n)
                    {
                        // Skip IAC sequences inline
                        i++; // command byte
                        if (buf[i] == SB)
                        {
                            // Skip until SE
                            while (++i < n && !(buf[i] == IAC && i + 1 < n && buf[i + 1] == SE)) { }
                            if (i + 1 < n) i++; // skip SE
                        }
                        else if (buf[i] >= WILL && buf[i] <= DONT)
                        {
                            if (i + 1 < n) i++; // skip option byte
                        }
                        continue;
                    }

                    if (StripLF && buf[i] == 10) continue;
                    sb.Append((char)buf[i]);
                }
            }
        }
        catch { }
        return sb.ToString();
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

            // Ignore non-printable characters
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

            // Filter out telnet IAC sequences so they don't get
            // forwarded to door games as keyboard input
            var filtered = new List<byte>(n);
            for (int i = 0; i < n; i++)
            {
                if (buf[i] == IAC && i + 1 < n)
                {
                    byte cmd = buf[i + 1];
                    if (cmd == IAC)
                    {
                        // Escaped 0xFF - pass through as literal byte
                        filtered.Add(0xFF);
                        i++;
                    }
                    else if (cmd == SB)
                    {
                        // Subnegotiation - skip until IAC SE
                        i += 2;
                        while (i < n)
                        {
                            if (buf[i] == IAC && i + 1 < n && buf[i + 1] == SE)
                            {
                                i++; // skip SE, loop will i++
                                break;
                            }
                            i++;
                        }
                    }
                    else
                    {
                        // WILL/WONT/DO/DONT + option = 3 bytes total, skip
                        i += 2;
                        if (i >= n) break;
                    }
                }
                else if (buf[i] != IAC)
                {
                    if (StripLF && buf[i] == 10) continue;
                    filtered.Add(buf[i]);
                }
            }

            return filtered.ToArray();
        }
        catch
        {
            return Array.Empty<byte>();
        }
    }

    public bool CanRead(int timeoutMs = 0)
    {
        if (_socket == null || !_socket.Connected) return false;
        try
        {
            return _socket.Poll(timeoutMs * 1000, SelectMode.SelectRead) && _socket.Available > 0;
        }
        catch
        {
            return false;
        }
    }

    private static readonly Encoding _cp437 = Encoding.GetEncoding(437);

    public void Write(string text)
    {
        if (string.IsNullOrEmpty(text)) return;
        byte[] data = _cp437.GetBytes(text);
        WriteRawBytes(data);
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
        if (_socket != null)
            _socket.Blocking = blocking;
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
                {
                    sent += _socket.Send(data, sent, data.Length - sent, SocketFlags.None);
                }
            }
            catch { }
        }
    }

    private void ProcessIAC()
    {
        if (_socket == null) return;

        byte[] buf = new byte[1];
        if (_socket.Receive(buf, 0, 1, SocketFlags.None) <= 0) return;
        byte command = buf[0];

        if (command == IAC)
        {
            // Escaped 0xFF - this is data, not a command
            return;
        }

        if (command >= WILL && command <= DONT)
        {
            // Read option byte
            if (_socket.Receive(buf, 0, 1, SocketFlags.None) <= 0) return;
            byte option = buf[0];

            // Respond to negotiations
            switch (command)
            {
                case DO:
                    if (option == OPT_ECHO || option == OPT_SUPPRESS_GA)
                        SendCommand(WILL, option);
                    else
                        SendCommand(WONT, option);
                    break;
                case DONT:
                    SendCommand(WONT, option);
                    break;
                case WILL:
                    if (option == OPT_TERMINAL_TYPE || option == OPT_NAWS || option == OPT_SUPPRESS_GA)
                        SendCommand(DO, option);
                    else
                        SendCommand(DONT, option);
                    break;
                case WONT:
                    SendCommand(DONT, option);
                    break;
            }
        }
        else if (command == SB)
        {
            // Subnegotiation - read until IAC SE
            var subData = new List<byte>();
            while (true)
            {
                if (_socket.Receive(buf, 0, 1, SocketFlags.None) <= 0) return;
                if (buf[0] == IAC)
                {
                    if (_socket.Receive(buf, 0, 1, SocketFlags.None) <= 0) return;
                    if (buf[0] == SE) break;
                    subData.Add(buf[0]);
                }
                else
                {
                    subData.Add(buf[0]);
                }
            }
            // Subnegotiation data received but not used currently
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
