using GameSrv.Config;
using GameSrv.Enums;
using GameSrv.Helpers;

namespace GameSrv.Menu;

/// <summary>
/// New user registration question from config/newuser.ini.
/// Ported from RandM.GameSrv.NewUserQuestion.
/// </summary>
public class NewUserQuestion : ConfigHelper
{
    public bool Confirm { get; set; } = false;
    public bool Required { get; set; } = false;
    public ValidationType Validate { get; set; } = ValidationType.None;

    public NewUserQuestion(string question) : base(Path.Combine("config", "newuser.ini"))
    {
        Load(question);
    }

    public static string[] GetQuestions()
    {
        string path = Path.Combine(StringHelper.StartupPath, "config", "newuser.ini");
        using var ini = new IniFile(path);
        return ini.ReadSections()
            .Where(x => !string.Equals(x, "alias", StringComparison.OrdinalIgnoreCase) &&
                        !string.Equals(x, "password", StringComparison.OrdinalIgnoreCase))
            .ToArray();
    }
}
