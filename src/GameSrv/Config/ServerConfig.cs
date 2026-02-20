using GameSrv.Enums;
using GameSrv.Helpers;

namespace GameSrv.Config;

/// <summary>
/// Singleton configuration for gamesrv.ini.
/// Replaces RandM.GameSrv.Config.
/// </summary>
public class ServerConfig : ConfigHelper
{
    public string BBSName { get; set; } = "New GameSrv BBS";
    public int FirstNode { get; set; } = 1;
    public int LastNode { get; set; } = 5;
    public int NextUserId { get; set; } = 1;
    public string NewUserPassword { get; set; } = "";
    public string PasswordPepper { get; set; } = "";
    public bool RLoginPromptForCredentialsOnFailedLogOn { get; set; } = false;
    public string RLoginServerIP { get; set; } = "0.0.0.0";
    public int RLoginServerPort { get; set; } = 513;
    public bool RLoginSkipNewUserPrompts { get; set; } = true;
    public bool RLoginValidatePassword { get; set; } = false;
    public string SysopEmail { get; set; } = "root@localhost";
    public string SysopFirstName { get; set; } = "New";
    public string SysopLastName { get; set; } = "Sysop";
    public string TelnetServerIP { get; set; } = "0.0.0.0";
    public int TelnetServerPort { get; set; } = 23;
    public TerminalType TerminalType { get; set; } = TerminalType.ANSI;
    public string TimeFormatLog { get; set; } = "G";
    public string TimeFormatUI { get; set; } = "T";
    public int TimePerCall { get; set; } = 60;
    public string UnixUser { get; set; } = "gamesrv";
    public string WebSocketServerIP { get; set; } = "0.0.0.0";
    public int WebSocketServerPort { get; set; } = 1123;

    // QEMU-specific settings (new)
    public string QemuBinary { get; set; } = "/usr/bin/qemu-system-i386";
    public string FreeDosImage { get; set; } = "freedos/freedos-boot.img";
    public int QemuDefaultMemory { get; set; } = 16;

    private ServerConfig() : base(Path.Combine("config", "gamesrv.ini"))
    {
        Load();

        // Generate password pepper if blank
        if (string.IsNullOrEmpty(PasswordPepper))
        {
            PasswordPepper = StringHelper.RandomString(100);
            Save();
        }
    }

    public void Init()
    {
        Log.Info("Loading Global Settings");

        // Flip nodes if necessary
        if (FirstNode > LastNode)
        {
            (FirstNode, LastNode) = (LastNode, FirstNode);
        }

        // Also try to load QEMU settings from [QEMU] section
        LoadQemuSettings();

        if (!Loaded)
        {
            Log.Info("Unable To Load Global Settings...Will Use Defaults");
            Save();
        }
    }

    private void LoadQemuSettings()
    {
        string filePath = Path.Combine(StringHelper.StartupPath, "config", "gamesrv.ini");
        if (!File.Exists(filePath)) return;

        using var ini = new IniFile(filePath);
        if (!ini.SectionExists("QEMU")) return;

        string binary = ini.ReadString("QEMU", "QemuBinary", "");
        if (!string.IsNullOrEmpty(binary)) QemuBinary = binary;

        string image = ini.ReadString("QEMU", "FreeDosImage", "");
        if (!string.IsNullOrEmpty(image)) FreeDosImage = image;

        int mem = ini.ReadInt("QEMU", "DefaultMemory", 0);
        if (mem > 0) QemuDefaultMemory = mem;
    }

    public string ServerPorts
    {
        get
        {
            var ports = new List<string>();
            if (RLoginServerPort > 0) ports.Add(RLoginServerPort.ToString());
            if (TelnetServerPort > 0) ports.Add(TelnetServerPort.ToString());
            if (WebSocketServerPort > 0) ports.Add(WebSocketServerPort.ToString());
            return string.Join(",", ports);
        }
    }

    #region Singleton
    private static volatile ServerConfig? _instance;
    private static readonly object _lock = new();

    public static ServerConfig Instance
    {
        get
        {
            if (_instance == null)
            {
                lock (_lock)
                {
                    _instance ??= new ServerConfig();
                }
            }
            return _instance;
        }
    }
    #endregion
}
