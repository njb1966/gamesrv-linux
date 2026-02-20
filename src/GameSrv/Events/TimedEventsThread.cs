using System.Diagnostics;
using GameSrv.Helpers;

namespace GameSrv.Events;

/// <summary>
/// Background thread that checks for and runs timed events.
/// Ported from RandM.GameSrv.TimedEventsThread.
/// </summary>
public class TimedEventsThread : IDisposable
{
    private readonly List<TimedEvent> _events = new();
    private CancellationTokenSource? _cts;
    private Task? _task;
    private bool _disposed;

    public void Start()
    {
        Log.Info("Starting Timed Events Thread");

        try
        {
            // Load events into memory
            var eventNames = TimedEvent.GetEventNames();
            foreach (var name in eventNames)
            {
                var te = new TimedEvent(name);
                if (te.Loaded) _events.Add(te);
            }

            _cts = new CancellationTokenSource();
            _task = Task.Run(() => Execute(_cts.Token));
        }
        catch (Exception ex)
        {
            Log.Exception(ex, "Error starting TimedEventsThread");
        }
    }

    public void Stop()
    {
        Log.Info("Stopping Timed Events Thread");

        try
        {
            _cts?.Cancel();
            _task?.Wait(TimeSpan.FromSeconds(5));
        }
        catch (Exception ex)
        {
            Log.Exception(ex, "Error stopping TimedEventsThread");
        }
    }

    private void Execute(CancellationToken ct)
    {
        while (!ct.IsCancellationRequested)
        {
            try
            {
                string currentDay = DateTime.Now.DayOfWeek.ToString();
                string currentTime = DateTime.Now.ToString("HH:mm");

                var eventsToRun = _events.Where(e => e.Days.Contains(currentDay) && e.Time == currentTime);
                foreach (var evt in eventsToRun)
                {
                    Log.Info($"Running timed event '{evt.Name}'");

                    try
                    {
                        var psi = new ProcessStartInfo(evt.Command)
                        {
                            WorkingDirectory = StringHelper.StartupPath,
                            UseShellExecute = false,
                            CreateNoWindow = true,
                        };

                        using var process = Process.Start(psi);
                        process?.WaitForExit();
                    }
                    catch (Exception ex)
                    {
                        Log.Exception(ex, $"Error running timed event '{evt.Name}'");
                    }
                }
            }
            catch (Exception ex)
            {
                Log.Exception(ex, "Error in timed events loop");
            }

            // Wait until the next minute rolls around
            int delayMs = (61 - DateTime.Now.Second) * 1000;
            try { Task.Delay(delayMs, ct).Wait(ct); } catch (OperationCanceledException) { break; }
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
