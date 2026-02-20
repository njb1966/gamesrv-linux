using System.Text.RegularExpressions;

namespace GameSrv.Helpers;

/// <summary>
/// Background thread that periodically downloads lists of monitoring service IPs
/// to ignore (StatusCake, UptimeRobot) and combines them with the local ignored IPs list.
/// Ported from RandM.GameSrv.IgnoredIPsThread.
/// </summary>
public class IgnoredIPsThread : IDisposable
{
    private CancellationTokenSource? _cts;
    private Task? _task;
    private bool _disposed;

    private static readonly HttpClient _httpClient = new()
    {
        Timeout = TimeSpan.FromSeconds(30)
    };

    public void Start()
    {
        Log.Info("Starting Ignored IPs Thread");

        try
        {
            _cts = new CancellationTokenSource();
            _task = Task.Run(() => Execute(_cts.Token));
        }
        catch (Exception ex)
        {
            Log.Exception(ex, "Error starting IgnoredIPsThread");
        }
    }

    public void Stop()
    {
        Log.Info("Stopping Ignored IPs Thread");

        try
        {
            _cts?.Cancel();
            _task?.Wait(TimeSpan.FromSeconds(5));
        }
        catch (Exception ex)
        {
            Log.Exception(ex, "Error stopping IgnoredIPsThread");
        }
    }

    private void Execute(CancellationToken ct)
    {
        while (!ct.IsCancellationRequested)
        {
            string configDir = Path.Combine(StringHelper.StartupPath, "config");
            string ignoredIPsFile = Path.Combine(configDir, "ignored-ips.txt");
            string combinedFile = Path.Combine(configDir, "ignored-ips-combined.txt");
            string statusCakeFile = Path.Combine(configDir, "ignored-ips-statuscake.txt");
            string uptimeRobotFile = Path.Combine(configDir, "ignored-ips-uptimerobot.txt");

            // Download StatusCake IPs
            try
            {
                string ips = _httpClient.GetStringAsync("https://www.statuscake.com/API/Locations/txt", ct).Result;
                ips = ips.Replace("\r\n", "\n").Replace("\n", "\r\n");
                File.WriteAllText(statusCakeFile, ips);
            }
            catch (Exception ex) when (ex is not OperationCanceledException)
            {
                Log.Debug($"Unable to download StatusCake IPs: {ex.Message}");
            }

            // Download UptimeRobot IPs
            try
            {
                string html = _httpClient.GetStringAsync("https://uptimerobot.com/locations", ct).Result;
                var matches = Regex.Matches(html, @"(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})");

                var ips = new List<string>();
                foreach (Match m in matches)
                    ips.Add(m.Groups[1].Value);

                File.WriteAllText(uptimeRobotFile, string.Join("\r\n", ips));
            }
            catch (Exception ex) when (ex is not OperationCanceledException)
            {
                Log.Debug($"Unable to download UptimeRobot IPs: {ex.Message}");
            }

            // Combine all lists
            try
            {
                var combined = new List<string>();

                if (File.Exists(ignoredIPsFile))
                    combined.Add(File.ReadAllText(ignoredIPsFile));
                if (File.Exists(statusCakeFile))
                    combined.Add(File.ReadAllText(statusCakeFile));
                if (File.Exists(uptimeRobotFile))
                    combined.Add(File.ReadAllText(uptimeRobotFile));

                File.WriteAllText(combinedFile, string.Join("\r\n", combined));
            }
            catch (Exception ex)
            {
                Log.Exception(ex, "Unable to combine Ignored IPs lists");
            }

            // Wait one hour before updating again
            try { Task.Delay(TimeSpan.FromHours(1), ct).Wait(ct); } catch (OperationCanceledException) { break; }
        }
    }

    public void Dispose()
    {
        if (!_disposed)
        {
            _disposed = true;
            Stop();
            _cts?.Dispose();
        }
    }
}
