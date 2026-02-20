using System.Diagnostics;
using GameSrv.Helpers;
using GameSrv.Session;

namespace GameSrv.Doors;

/// <summary>
/// Runs native Linux door programs as child processes.
/// </summary>
public class NativeDoorRunner : IDoorRunner
{
    public void Run(ClientThread clientThread, string command, string parameters)
    {
        Log.Info($"Natively launching: {command} {parameters}");

        try
        {
            var psi = new ProcessStartInfo(command, parameters)
            {
                WorkingDirectory = StringHelper.StartupPath,
                UseShellExecute = false,
                CreateNoWindow = true,
            };

            // Set environment variables for the door
            psi.Environment["NODE"] = clientThread.NodeInfo.Node.ToString();
            psi.Environment["GAMESRV_NODE"] = clientThread.NodeInfo.Node.ToString();
            psi.Environment["GAMESRV_USER"] = clientThread.NodeInfo.User.Alias;
            psi.Environment["GAMESRV_TIMELEFT"] = clientThread.NodeInfo.MinutesLeft.ToString();
            psi.Environment["GAMESRV_DROPDIR"] = Path.Combine(StringHelper.StartupPath, "node" + clientThread.NodeInfo.Node);

            using var process = Process.Start(psi);
            if (process == null)
            {
                Log.Error($"Failed to start native door process: {command}");
                return;
            }

            // Wait for process, checking for client disconnect
            while (!process.HasExited)
            {
                if (clientThread.QuitThread())
                {
                    Log.Info("Client disconnected while in native door, terminating process");
                    try
                    {
                        process.Kill(entireProcessTree: true);
                    }
                    catch (Exception ex)
                    {
                        Log.Warning($"Failed to kill native door process: {ex.Message}");
                    }
                    return;
                }
                process.WaitForExit(500);
            }

            Log.Info($"Native door exited with code {process.ExitCode}");
        }
        catch (Exception ex)
        {
            Log.Exception(ex, $"Error running native door '{command}'");
        }
    }
}
