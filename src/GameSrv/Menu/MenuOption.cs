using GameSrv.Config;
using GameSrv.Enums;
using GameSrv.Helpers;

namespace GameSrv.Menu;

/// <summary>
/// A single menu option loaded from menus/{menu}.ini.
/// Ported from RandM.GameSrv.MenuOption.
/// </summary>
public class MenuOption : ConfigHelper
{
    public string Name { get; set; } = "";
    public MenuAction Action { get; set; } = MenuAction.Disconnect;
    public string Parameters { get; set; } = "";
    public int RequiredAccess { get; set; } = 0;

    public MenuOption(string menu, char hotkey)
        : base(Path.Combine("menus", menu.ToLower() + ".ini"))
    {
        if (!string.IsNullOrEmpty(menu))
            Load(hotkey.ToString());
    }

    public static string[] GetHotkeys(string menu)
    {
        string path = Path.Combine(StringHelper.StartupPath, "menus", menu.ToLower() + ".ini");
        using var ini = new IniFile(path);
        return ini.ReadSections();
    }
}
