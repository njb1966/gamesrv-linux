using System.Security.Cryptography;

namespace GameSrv.Helpers;

/// <summary>
/// String and path utility methods replacing RMLib.StringUtils.
/// </summary>
public static class StringHelper
{
    private const string RandomChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

    public static string PathCombine(params string[] paths)
    {
        return Path.Combine(paths);
    }

    /// <summary>
    /// Generate a random string of the specified length using cryptographic RNG.
    /// </summary>
    public static string RandomString(int length)
    {
        var bytes = RandomNumberGenerator.GetBytes(length);
        var chars = new char[length];
        for (int i = 0; i < length; i++)
        {
            chars[i] = RandomChars[bytes[i] % RandomChars.Length];
        }
        return new string(chars);
    }

    /// <summary>
    /// Get the application's working directory.
    /// When running via "dotnet run", this is the directory where dotnet run was invoked.
    /// When deployed, set the WorkingDirectory in the systemd service file.
    /// </summary>
    public static string StartupPath { get; set; } = Environment.CurrentDirectory;
}
