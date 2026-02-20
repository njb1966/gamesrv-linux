using GameSrv.Auth;
using GameSrv.Doors;
using GameSrv.Enums;
using GameSrv.Network;

namespace GameSrv.Server;

/// <summary>
/// Per-node session state. Replaces RandM.GameSrv.NodeInfo.
/// </summary>
public class NodeInfo
{
    public IConnection? Connection { get; set; }
    public ConnectionType ConnectionType { get; set; }
    public UserInfo User { get; set; } = new UserInfo();
    public DoorInfo? Door { get; set; }
    public int Node { get; set; }
    public DateTime TimeOn { get; set; } = DateTime.Now;
    public TerminalType TerminalType { get; set; } = TerminalType.ANSI;
    public bool UserLoggedOn { get; set; }

    /// <summary>
    /// Total seconds allowed this session (set on login).
    /// </summary>
    public int SecondsThisSession { get; set; } = 3600;

    public int SecondsLeft
    {
        get
        {
            int elapsed = (int)DateTime.Now.Subtract(TimeOn).TotalSeconds;
            return Math.Max(0, SecondsThisSession - elapsed);
        }
    }

    public int MinutesLeft => SecondsLeft / 60;

    /// <summary>
    /// Read timeout based on remaining session time (capped at 5 minutes for idle timeout).
    /// </summary>
    public int ReadTimeout
    {
        get
        {
            int timeout = Math.Min(SecondsLeft, 300) * 1000; // 5 min max idle, in ms
            return Math.Max(timeout, 1000); // At least 1 second
        }
    }

    public int SecondsThisSessionElapsed => (int)DateTime.Now.Subtract(TimeOn).TotalSeconds;
}
