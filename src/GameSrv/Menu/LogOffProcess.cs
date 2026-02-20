using GameSrv.Config;
using GameSrv.Enums;
using GameSrv.Helpers;
using GameSrv.Session;

namespace GameSrv.Menu;

/// <summary>
/// Logoff process step loaded from config/logoffprocess.ini.
/// Ported from RandM.GameSrv.LogOffProcess.
/// </summary>
public class LogOffProcess : ConfigHelper
{
    public string Name { get; set; } = "";
    public MenuAction Action { get; set; } = MenuAction.Disconnect;
    public string Parameters { get; set; } = "";
    public int RequiredAccess { get; set; } = 0;

    public LogOffProcess(string section) : base(Path.Combine("config", "logoffprocess.ini"))
    {
        Load(section);
    }

    public static string[] GetProcesses()
    {
        string path = Path.Combine(StringHelper.StartupPath, "config", "logoffprocess.ini");
        using var ini = new IniFile(path);
        return ini.ReadSections();
    }

    public static void Run(ClientThread clientThread)
    {
        string[] processes = GetProcesses();
        foreach (string process in processes)
        {
            try
            {
                var lp = new LogOffProcess(process);
                if (!lp.Loaded || clientThread.QuitThread()) continue;

                switch (lp.Action)
                {
                    case MenuAction.Disconnect:
                    case MenuAction.DisplayFile:
                    case MenuAction.DisplayFileMore:
                    case MenuAction.DisplayFilePause:
                    case MenuAction.Pause:
                    case MenuAction.RunDoor:
                        var mo = new MenuOption("", '\0')
                        {
                            Action = lp.Action,
                            Name = lp.Name,
                            Parameters = lp.Parameters,
                            RequiredAccess = lp.RequiredAccess,
                        };
                        if (clientThread.HandleMenuOption(mo))
                            return;
                        break;
                }
            }
            catch (Exception ex)
            {
                Log.Exception(ex, $"Error during logoff process '{process}'");
            }
        }
    }
}
