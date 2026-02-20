using System.Collections;
using GameSrv.Config;
using GameSrv.Enums;
using GameSrv.Helpers;
using GameSrv.Session;

namespace GameSrv.Doors;

/// <summary>
/// Door execution dispatcher. Loads door configuration, creates drop files,
/// runs the door via the appropriate runner (QEMU for DOS, native for Linux),
/// and cleans up afterward. Ported from RandM.GameSrv.RunDoor.
/// </summary>
public class RunDoor
{
    private readonly ClientThread _clientThread;

    public RunDoor(ClientThread clientThread)
    {
        _clientThread = clientThread;
    }

    /// <summary>
    /// Load a door by name and run it.
    /// </summary>
    public void Run(string door)
    {
        _clientThread.NodeInfo.Door = new DoorInfo(door);
        if (_clientThread.NodeInfo.Door.Loaded)
        {
            Run();
        }
        else
        {
            Log.Error($"Unable to find door: '{door}'");
        }
    }

    /// <summary>
    /// Run the currently loaded door (NodeInfo.Door must be set).
    /// </summary>
    public void Run()
    {
        try
        {
            // Clear buffers and reset screen
            _clientThread.NodeInfo.Connection!.ReadString();
            _clientThread.ClrScr();

            // Create the node directory and drop files
            DropFileGenerator.CreateDropFiles(_clientThread);

            // Translate command-line substitution variables
            string command = TranslateCLS(_clientThread.NodeInfo.Door!.Command);
            string parameters = TranslateCLS(_clientThread.NodeInfo.Door.Parameters);

            // Dispatch to appropriate runner
            switch (_clientThread.NodeInfo.Door.Platform)
            {
                case DoorPlatform.Linux:
                    new NativeDoorRunner().Run(_clientThread, command, parameters);
                    break;

                case DoorPlatform.DOS:
                    if (File.Exists(ServerConfig.Instance.QemuBinary))
                    {
                        new QemuDoorRunner().Run(_clientThread, command, parameters);
                    }
                    else
                    {
                        Log.Error($"DOS doors require QEMU but '{ServerConfig.Instance.QemuBinary}' was not found");
                    }
                    break;

                default:
                    Log.Error($"Unsupported door platform: {_clientThread.NodeInfo.Door.Platform}");
                    break;
            }
        }
        catch (Exception ex)
        {
            Log.Exception(ex, $"Error while running door '{_clientThread.NodeInfo.Door?.Name}'");
        }
        finally
        {
            try
            {
                _clientThread.ClrScr();
                _clientThread.NodeInfo.Connection!.SetBlocking(true);
                DropFileGenerator.CleanupDropFiles(_clientThread.NodeInfo.Node);
            }
            catch { /* Ignore cleanup errors */ }
        }
    }

    /// <summary>
    /// Translate command-line substitution variables.
    /// Variables prefixed with * are replaced with runtime values.
    /// Variables prefixed with ** map to user/additional info fields.
    /// For DOS doors, paths reference drive letters (D: = node dir with floppy boot).
    /// </summary>
    private string TranslateCLS(string command)
    {
        var ni = _clientThread.NodeInfo;
        string nodeDir = Path.Combine(StringHelper.StartupPath, "node" + ni.Node);

        // For DOS doors, drop files are on D: (second IDE drive, floppy boot layout)
        bool isDos = ni.Door?.Platform == DoorPlatform.DOS;
        string dropFilePrefix = isDos ? @"D:\" : (nodeDir + Path.DirectorySeparatorChar);

        var substitutions = new List<KeyValuePair<string, string>>
        {
            new("**ALIAS", ni.User.Alias),
            new("DOOR32", dropFilePrefix + "DOOR32.SYS"),
            new("DOORSYS", dropFilePrefix + "DOOR.SYS"),
            new("DOORFILE", dropFilePrefix + "DOORFILE.SR"),
            new("DORINFOx", dropFilePrefix + $"DORINFO{ni.Node}.DEF"),
            new("DORINFO1", dropFilePrefix + "DORINFO1.DEF"),
            new("DORINFO", dropFilePrefix + "DORINFO.DEF"),
            new("HANDLE", ni.Connection?.Handle.ToString() ?? "0"),
            new("IPADDRESS", ni.Connection?.GetRemoteIP() ?? "0.0.0.0"),
            new("MINUTESLEFT", ni.MinutesLeft.ToString()),
            new("NODE", ni.Node.ToString()),
            new("**PASSWORD", ni.User.PasswordHash),
            new("SECONDSLEFT", ni.SecondsLeft.ToString()),
            new("SOCKETHANDLE", ni.Connection?.Handle.ToString() ?? "0"),
            new("**USERNAME", ni.User.Alias),
        };

        // Add user additional info fields
        foreach (DictionaryEntry de in ni.User.AdditionalInfo)
        {
            if (de.Key != null && de.Value != null)
                substitutions.Add(new KeyValuePair<string, string>("**" + de.Key.ToString()!, de.Value.ToString()!));
        }

        // Perform substitutions (note: keys are prefixed with * in the template)
        foreach (var sub in substitutions)
        {
            if (sub.Value != null)
                command = command.Replace("*" + sub.Key.ToUpper(), sub.Value, StringComparison.OrdinalIgnoreCase);
        }

        return command;
    }
}
