using System.Collections.Specialized;
using System.Security.Cryptography;
using System.Text;
using GameSrv.Config;
using GameSrv.Helpers;

namespace GameSrv.Auth;

/// <summary>
/// User data backed by INI file in users/{alias}.ini.
/// Ported from RandM.GameSrv.UserInfo.
/// </summary>
public class UserInfo : ConfigHelper
{
    public int AccessLevel { get; set; } = 10;
    public StringDictionary AdditionalInfo { get; set; } = new();
    public string Alias { get; set; } = "";
    public bool AllowMultipleConnections { get; set; } = false;
    public string PasswordHash { get; set; } = "";
    public string PasswordSalt { get; set; } = StringHelper.RandomString(100);
    public int UserId { get; set; } = 0;

    private static readonly object _registrationLock = new();

    public UserInfo() : base(Path.Combine("users", "default.ini")) { }

    public UserInfo(string alias) : base(Path.Combine("users", SafeAlias(alias.ToLower()) + ".ini"))
    {
        Alias = alias;
        Load("USER");
    }

    public void AbortRegistration()
    {
        if (!string.IsNullOrEmpty(Alias) && !string.Equals(Alias, "NEW", StringComparison.OrdinalIgnoreCase))
        {
            lock (_registrationLock)
            {
                FileHelper.FileDelete(FileName);
            }
        }
    }

    public static string GetPasswordHash(string password, string salt)
    {
        if (password == null) throw new ArgumentNullException(nameof(password));
        if (string.IsNullOrEmpty(salt)) throw new ArgumentNullException(nameof(salt));

        if (string.Equals(ServerConfig.Instance.PasswordPepper.Trim(), "DISABLE", StringComparison.OrdinalIgnoreCase))
            return password;

        byte[] inBytes = Encoding.ASCII.GetBytes(salt + password + ServerConfig.Instance.PasswordPepper);
        byte[] outBytes = SHA512.HashData(inBytes);

        // 1024 iterations of hashing
        for (int i = 0; i < 1024; i++)
        {
            outBytes = SHA512.HashData(outBytes);
        }

        return Convert.ToBase64String(outBytes);
    }

    public static string SafeAlias(string alias)
    {
        char[] invalidChars = Path.GetInvalidFileNameChars();
        foreach (char c in invalidChars)
        {
            string replacement = "_" + ((int)c).ToString() + "_";
            alias = alias.Replace(c.ToString(), replacement);
        }
        return alias;
    }

    public void SaveRegistration()
    {
        Save();
    }

    public void SetPassword(string password)
    {
        PasswordSalt = StringHelper.RandomString(100);
        PasswordHash = GetPasswordHash(password, PasswordSalt);
    }

    public bool StartRegistration(string alias)
    {
        lock (_registrationLock)
        {
            var existing = new UserInfo(alias);
            if (existing.Loaded)
            {
                // Alias already exists
                return false;
            }

            // Reserve the alias by updating the file path and saving
            Alias = alias;
            _filePath = Path.Combine(Helpers.StringHelper.StartupPath, "users", SafeAlias(alias.ToLower()) + ".ini");
            Save();
            return true;
        }
    }

    public bool ValidatePassword(string password)
    {
        return PasswordHash == GetPasswordHash(password, PasswordSalt);
    }
}
