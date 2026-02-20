using GameSrv.Config;
using GameSrv.Enums;
using GameSrv.Helpers;
using GameSrv.Session;

namespace GameSrv.Menu;

/// <summary>
/// Logon process step loaded from config/logonprocess.ini.
/// Ported from RandM.GameSrv.LogOnProcess.
/// </summary>
public class LogOnProcess : ConfigHelper
{
    public string Name { get; set; } = "";
    public MenuAction Action { get; set; } = MenuAction.Disconnect;
    public string Parameters { get; set; } = "";
    public int RequiredAccess { get; set; } = 0;

    public LogOnProcess(string section) : base(Path.Combine("config", "logonprocess.ini"))
    {
        Load(section);
    }

    public static string[] GetProcesses()
    {
        string path = Path.Combine(StringHelper.StartupPath, "config", "logonprocess.ini");
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
                var lp = new LogOnProcess(process);
                if (!lp.Loaded || clientThread.QuitThread()) continue;

                switch (lp.Action)
                {
                    case MenuAction.Disconnect:
                    case MenuAction.DisplayFile:
                    case MenuAction.DisplayFileMore:
                    case MenuAction.DisplayFilePause:
                    case MenuAction.MainMenu:
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
                Log.Exception(ex, $"Error during logon process '{process}'");
            }
        }
    }
}
