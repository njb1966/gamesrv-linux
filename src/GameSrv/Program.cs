using System.Runtime.InteropServices;
using System.Text;
using GameSrv.Helpers;
using GameSrv.Server;

namespace GameSrv;

/// <summary>
/// Entry point for GameSrv. Runs as a headless Linux console application.
/// Handles SIGTERM/SIGINT for graceful shutdown (systemd compatible).
/// </summary>
public static class Program
{
    public static void Main(string[] args)
    {
        // Register codepage 437 for ANSI art file support
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

        Console.WriteLine("GameSrv Door Server v2.0.0");
        Console.WriteLine("Linux / .NET 8 Edition");
        Console.WriteLine();

        // Allow overriding the base directory (e.g., --basedir /opt/gamesrv)
        int baseDirIdx = Array.FindIndex(args, a => a.Equals("--basedir", StringComparison.OrdinalIgnoreCase));
        if (baseDirIdx >= 0 && baseDirIdx + 1 < args.Length)
        {
            StringHelper.StartupPath = Path.GetFullPath(args[baseDirIdx + 1]);
        }

        Console.WriteLine($"Base directory: {StringHelper.StartupPath}");
        Console.WriteLine();

        bool debug = args.Contains("--debug", StringComparer.OrdinalIgnoreCase);
        if (debug)
        {
            Log.Level = LogLevel.Debug;
            Log.Info("Debug mode enabled");
        }

        using var cts = new CancellationTokenSource();
        using var app = new GameSrvApp();

        // Handle SIGTERM (systemd stop) and SIGINT (Ctrl+C)
        Console.CancelKeyPress += (_, e) =>
        {
            e.Cancel = true;
            Log.Info("Received SIGINT, shutting down...");
            cts.Cancel();
        };

        PosixSignalRegistration.Create(PosixSignal.SIGTERM, _ =>
        {
            Log.Info("Received SIGTERM, shutting down...");
            cts.Cancel();
        });

        // Start the server
        app.Start();

        if (app.Status != Enums.GameSrvStatus.Started)
        {
            Log.Error("Server failed to start, exiting");
            return;
        }

        // Wait for shutdown signal
        try
        {
            Task.Delay(Timeout.Infinite, cts.Token).Wait();
        }
        catch (AggregateException ex) when (ex.InnerException is TaskCanceledException)
        {
            // Expected when cancellation is requested
        }

        // Graceful shutdown
        app.Stop();
        Console.WriteLine("Goodbye.");
    }
}
