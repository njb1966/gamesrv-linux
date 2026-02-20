using GameSrv.Config;
using GameSrv.Enums;

namespace GameSrv.Doors;

/// <summary>
/// Door game configuration loaded from doors/{name}.ini.
/// Ported from RandM.GameSrv.DoorInfo.
/// </summary>
public class DoorInfo : ConfigHelper
{
    public string Command { get; set; } = "";
    public int ForceQuitDelay { get; set; } = 5;
    public string Name { get; set; } = "";
    public string Parameters { get; set; } = "";
    public DoorPlatform Platform { get; set; } = DoorPlatform.Unknown;
    public bool WatchDTR { get; set; } = true;

    // QEMU-specific settings
    public string GameImagePath { get; set; } = "";
    public bool SharedState { get; set; } = true;
    public int QemuMemory { get; set; } = 0; // 0 = use default from config
    public int IdleTimeout { get; set; } = 10; // Seconds of no serial data before assuming door exited

    public DoorInfo(string door) : base(Path.Combine("doors", door.ToLower() + ".ini"))
    {
        if (Load("DOOR"))
        {
            // Handle legacy "Native" property for backward compatibility
            if (Platform == DoorPlatform.Unknown)
            {
                using var ini = new IniFile(FileName);
                bool native = ini.ReadBoolean(SectionName, "Native", false);
                Platform = native ? DoorPlatform.Linux : DoorPlatform.DOS;
            }
        }
    }
}
