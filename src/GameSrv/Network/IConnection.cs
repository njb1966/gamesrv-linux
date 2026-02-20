namespace GameSrv.Network;

/// <summary>
/// Interface for all connection types (Telnet, RLogin, WebSocket).
/// Replaces RMLib.TcpConnection base class.
/// </summary>
public interface IConnection : IDisposable
{
    bool Connected { get; }
    bool ReadTimedOut { get; }
    string LineEnding { get; set; }
    bool StripLF { get; set; }

    void Close();
    string GetRemoteIP();
    int GetRemotePort();

    /// <summary>
    /// Read a single character with timeout in milliseconds.
    /// Returns null if timed out.
    /// </summary>
    char? ReadChar(int timeoutMs);

    /// <summary>
    /// Read all available data as a string.
    /// </summary>
    string ReadString();

    /// <summary>
    /// Read a line of text with optional password masking.
    /// passwordChar of '\0' means no masking.
    /// </summary>
    string ReadLn(char passwordChar, int timeoutMs);

    /// <summary>
    /// Read all available data as raw bytes.
    /// </summary>
    byte[] ReadBytes();

    /// <summary>
    /// Check if data is available to read, with optional wait in milliseconds.
    /// </summary>
    bool CanRead(int timeoutMs = 0);

    void Write(string text);
    void WriteLn(string text = "");
    void WriteBytes(byte[] data, int count);

    void SetBlocking(bool blocking);

    /// <summary>
    /// The underlying socket handle. Used in DOOR32.SYS drop files.
    /// </summary>
    nint Handle { get; }

    /// <summary>
    /// Send a Telnet Go-Ahead to detect dropped connections.
    /// Only meaningful for Telnet connections; others can no-op.
    /// </summary>
    void SendGoAhead();

    /// <summary>
    /// Attempt to connect to a remote host (for outbound telnet proxy).
    /// </summary>
    bool Connect(string host, int port);
}
