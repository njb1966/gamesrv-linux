using GameSrv.Config;
using GameSrv.Helpers;

namespace GameSrv.Events;

/// <summary>
/// A single timed event loaded from config/timed-events.ini.
/// Ported from RandM.GameSrv.TimedEvent.
/// </summary>
public class TimedEvent : ConfigHelper
{
    public string Name { get; set; } = "";
    public string Command { get; set; } = "";
    public string Days { get; set; } = "";
    public string Time { get; set; } = "";
    public bool GoOffline { get; set; } = false;

    public TimedEvent(string eventName) : base(Path.Combine("config", "timed-events.ini"))
    {
        Load(eventName);
    }

    public static string[] GetEventNames()
    {
        string path = Path.Combine(StringHelper.StartupPath, "config", "timed-events.ini");
        if (!File.Exists(path)) return Array.Empty<string>();
        using var ini = new IniFile(path);
        return ini.ReadSections();
    }
}
