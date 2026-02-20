using GameSrv.Config;

namespace GameSrv.Helpers;

/// <summary>
/// IP address filtering: banned IPs, ignored IPs, RLogin allowlist.
/// Ported from Helpers.cs in the original codebase.
/// </summary>
public static class IpFilter
{
    private static readonly Dictionary<string, DateTime> _tempIgnoredIPs = new();
    private static readonly object _tempIgnoredLock = new();

    public static void AddTempIgnoredIP(string ip)
    {
        lock (_tempIgnoredLock)
        {
            _tempIgnoredIPs[ip] = DateTime.Now;
        }
    }

    public static bool IsBannedIP(string ip)
    {
        try
        {
            string path = Path.Combine(StringHelper.StartupPath, "config", "banned-ips.txt");
            return File.Exists(path) && FileContainsIP(path, ip);
        }
        catch (Exception ex)
        {
            Log.Exception(ex, "Unable to validate client IP against banned-ips.txt");
            return false;
        }
    }

    public static bool IsBannedUser(string alias)
    {
        if (string.IsNullOrWhiteSpace(alias)) return false;

        try
        {
            string path = Path.Combine(StringHelper.StartupPath, "config", "banned-users.txt");
            if (!File.Exists(path)) return false;

            string[] bannedUsers = File.ReadAllLines(path);
            foreach (string bannedUser in bannedUsers)
            {
                if (bannedUser.StartsWith(';')) continue;
                if (string.Equals(bannedUser.Trim(), alias.Trim(), StringComparison.OrdinalIgnoreCase))
                    return true;
            }
        }
        catch (Exception ex)
        {
            Log.Exception(ex, "Unable to validate alias against banned-users.txt");
        }
        return false;
    }

    public static bool IsIgnoredIP(string ip)
    {
        try
        {
            if (IsTempIgnoredIP(ip)) return true;

            string path = Path.Combine(StringHelper.StartupPath, "config", "ignored-ips-combined.txt");
            return File.Exists(path) && FileContainsIP(path, ip);
        }
        catch (Exception ex)
        {
            Log.Exception(ex, "Unable to validate client IP against ignored-ips.txt");
            return false;
        }
    }

    public static bool IsRLoginIP(string ip)
    {
        try
        {
            string path = Path.Combine(StringHelper.StartupPath, "config", "rlogin-ips.txt");
            if (!File.Exists(path)) return true; // No file means any RLogin allowed
            return FileContainsIP(path, ip);
        }
        catch (Exception ex)
        {
            Log.Exception(ex, "Unable to validate client IP against rlogin-ips.txt");
            return true;
        }
    }

    public static bool IsTempIgnoredIP(string ip)
    {
        lock (_tempIgnoredLock)
        {
            if (_tempIgnoredIPs.TryGetValue(ip, out DateTime added))
            {
                if (DateTime.Now.Subtract(added).TotalMinutes >= 10)
                {
                    _tempIgnoredIPs.Remove(ip);
                    return false;
                }
                return true;
            }
            return false;
        }
    }

    public static void CleanExpiredTempIgnoredIPs()
    {
        lock (_tempIgnoredLock)
        {
            var expired = _tempIgnoredIPs
                .Where(kvp => DateTime.Now.Subtract(kvp.Value).TotalMinutes >= 10)
                .Select(kvp => kvp.Key)
                .ToList();
            foreach (var key in expired)
                _tempIgnoredIPs.Remove(key);
        }
    }

    private static bool FileContainsIP(string fileName, string ip)
    {
        string[] connectionOctets = ip.Split('.');
        if (connectionOctets.Length != 4) return false;

        string[] fileIPs = File.ReadAllLines(fileName);
        foreach (string fileIP in fileIPs)
        {
            if (fileIP.StartsWith(';')) continue;

            string[] fileOctets = fileIP.Trim().Split('.');
            if (fileOctets.Length != 4) continue;

            bool match = true;
            for (int i = 0; i < 4; i++)
            {
                if (fileOctets[i] != "*" && fileOctets[i] != connectionOctets[i])
                {
                    match = false;
                    break;
                }
            }
            if (match) return true;
        }
        return false;
    }
}
