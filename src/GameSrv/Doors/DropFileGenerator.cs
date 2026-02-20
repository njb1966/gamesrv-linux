using GameSrv.Config;
using GameSrv.Enums;
using GameSrv.Helpers;
using GameSrv.Session;

namespace GameSrv.Doors;

/// <summary>
/// Generates BBS drop files (DOOR.SYS, DOOR32.SYS, DOORFILE.SR, DORINFO.DEF)
/// for door games. Extracted from RandM.GameSrv.RunDoor.CreateNodeDirectory().
/// </summary>
public static class DropFileGenerator
{
    public const string ProductName = "GameSrv";
    public const string Version = "2.0.0";

    /// <summary>
    /// Create the node directory and generate all standard drop files.
    /// </summary>
    public static string CreateDropFiles(ClientThread clientThread)
    {
        var ni = clientThread.NodeInfo;
        string nodeDir = Path.Combine(StringHelper.StartupPath, "node" + ni.Node);
        Directory.CreateDirectory(nodeDir);

        CreateDoorSys(clientThread, nodeDir);
        CreateDoor32Sys(clientThread, nodeDir);
        CreateDoorfileSr(clientThread, nodeDir);
        CreateDorinfodef(clientThread, nodeDir);

        return nodeDir;
    }

    /// <summary>
    /// Clean up drop files from the node directory.
    /// </summary>
    public static void CleanupDropFiles(int node)
    {
        string nodeDir = Path.Combine(StringHelper.StartupPath, "node" + node);
        string[] files = {
            "door.sys", "door32.sys", "doorfile.sr",
            "dorinfo.def", "dorinfo1.def", $"dorinfo{node}.def",
            "external.bat", "node.img", "serial.sock"
        };

        foreach (string file in files)
        {
            try { File.Delete(Path.Combine(nodeDir, file)); } catch { }
        }
    }

    private static void CreateDoorSys(ClientThread clientThread, string nodeDir)
    {
        var ni = clientThread.NodeInfo;
        var lines = new List<string>
        {
            "COM1:",                                                                // 1 - Comm Port
            "57600",                                                                // 2 - Connection Baud Rate
            "8",                                                                    // 3 - Parity
            ni.Node.ToString(),                                                     // 4 - Current Node Number
            "57600",                                                                // 5 - Locked Baud Rate
            "Y",                                                                    // 6 - Screen Display
            "Y",                                                                    // 7 - Printer Toggle
            "Y",                                                                    // 8 - Page Bell
            "Y",                                                                    // 9 - Caller Alarm
            ni.User.Alias,                                                          // 10 - User's Real Name
            "City, State",                                                          // 11 - User's Location
            "555-555-5555",                                                         // 12 - User's Home Phone #
            "555-555-5555",                                                         // 13 - User's Work Phone #
            "PASSWORD",                                                             // 14 - User's Password
            ni.User.AccessLevel.ToString(),                                         // 15 - User's Access Level
            "1",                                                                    // 16 - User's Total Calls
            "00/00/00",                                                             // 17 - User's Last Call Date
            ni.SecondsLeft.ToString(),                                              // 18 - User's Seconds Left This Call
            ni.MinutesLeft.ToString(),                                              // 19 - User's Minutes Left This Call
            "GR",                                                                   // 20 - Graphics Mode
            "24",                                                                   // 21 - Screen Length
            "N",                                                                    // 22 - Expert Mode
            "",                                                                     // 23 - Conferences Registered In
            "",                                                                     // 24 - Conference Exited To Door From
            "00/00/00",                                                             // 25 - User's Expiration Date
            (ni.User.UserId - 1).ToString(),                                        // 26 - User's Record Position (0 based)
            "Z",                                                                    // 27 - User's Default XFer Protocol
            "0",                                                                    // 28 - Total Uploads
            "0",                                                                    // 29 - Total Downloads
            "0",                                                                    // 30 - Total Downloaded Today (kB)
            "0",                                                                    // 31 - Daily Download Limit (kB)
            "00/00/00",                                                             // 32 - User's Birthday
            ni.Door?.Platform == DoorPlatform.DOS ? @"D:\" : nodeDir,                  // 33 - Path To User File
            ni.Door?.Platform == DoorPlatform.DOS ? @"D:\" : nodeDir,                  // 34 - Path To GEN Directory
            $"{ServerConfig.Instance.SysopFirstName} {ServerConfig.Instance.SysopLastName}", // 35 - SysOp's Name
            ni.User.Alias,                                                          // 36 - User's Alias
            "00:00",                                                                // 37 - Next Event Time
            "Y",                                                                    // 38 - Error Correcting Connection
            ni.TerminalType == TerminalType.ASCII ? "N" : "Y",                     // 39 - ANSI Supported
            "Y",                                                                    // 40 - Use Record Locking
            "7",                                                                    // 41 - Default BBS Colour
            "0",                                                                    // 42 - Time Credits (In Minutes)
            "00/00/00",                                                             // 43 - Last New File Scan
            "00:00",                                                                // 44 - Time Of This Call
            "00:00",                                                                // 45 - Time Of Last Call
            "0",                                                                    // 46 - Daily File Limit
            "0",                                                                    // 47 - Files Downloaded Today
            "0",                                                                    // 48 - Total Uploaded (kB)
            "0",                                                                    // 49 - Total Downloaded (kB)
            "No Comment",                                                           // 50 - User's Comment
            "0",                                                                    // 51 - Total Doors Opened
            "0",                                                                    // 52 - Total Messages Left
        };

        File.WriteAllText(Path.Combine(nodeDir, "door.sys"), string.Join("\r\n", lines));
    }

    private static void CreateDoor32Sys(ClientThread clientThread, string nodeDir)
    {
        var ni = clientThread.NodeInfo;
        var lines = new List<string>
        {
            "0",                                                        // 1 - Comm Type (0=Local for QEMU doors)
            "0",                                                        // 2 - Comm Or Socket Handle (N/A inside VM)
            "57600",                                                    // 3 - Baud Rate
            $"{ProductName} v{Version}",                                // 4 - BBSID
            ni.User.UserId.ToString(),                                  // 5 - User's Record Position (1 based)
            ni.User.Alias,                                              // 6 - User's Real Name
            ni.User.Alias,                                              // 7 - User's Handle/Alias
            ni.User.AccessLevel.ToString(),                             // 8 - User's Access Level
            ni.MinutesLeft.ToString(),                                  // 9 - User's Time Left (In Minutes)
            ni.TerminalType switch                                      // 10 - Emulation
            {
                TerminalType.ANSI => "1",
                TerminalType.RIP => "3",
                _ => "0"
            },
            ni.Node.ToString(),                                         // 11 - Current Node Number
        };

        File.WriteAllText(Path.Combine(nodeDir, "door32.sys"), string.Join("\r\n", lines));
    }

    private static void CreateDoorfileSr(ClientThread clientThread, string nodeDir)
    {
        var ni = clientThread.NodeInfo;
        var lines = new List<string>
        {
            ni.User.Alias,                                              // Complete name or handle of user
            ni.TerminalType == TerminalType.ASCII ? "0" : "1",         // ANSI status
            "1",                                                        // IBM Graphic characters
            "24",                                                       // Page length of screen
            "57600",                                                    // Baud Rate
            "1",                                                        // Com Port
            ni.MinutesLeft.ToString(),                                  // Time Limit (in minutes)
            ni.User.Alias,                                              // Real name
        };

        File.WriteAllText(Path.Combine(nodeDir, "doorfile.sr"), string.Join("\r\n", lines));
    }

    private static void CreateDorinfodef(ClientThread clientThread, string nodeDir)
    {
        var ni = clientThread.NodeInfo;
        var lines = new List<string>
        {
            ServerConfig.Instance.BBSName,                              // 1 - BBS Name
            ServerConfig.Instance.SysopFirstName,                       // 2 - Sysop's First Name
            ServerConfig.Instance.SysopLastName,                        // 3 - Sysop's Last Name
            "COM1",                                                     // 4 - Comm Number
            "57600 BAUD,N,8,1",                                         // 5 - Baud Rate
            "0",                                                        // 6 - Networked?
            ni.User.Alias,                                              // 7 - User's First Name / Alias
            "",                                                         // 8 - User's Last Name
            "City, State",                                              // 9 - User's Location
            ni.TerminalType == TerminalType.ASCII ? "0" : "1",         // 10 - User's Emulation
            ni.User.AccessLevel.ToString(),                             // 11 - User's Access Level
            ni.MinutesLeft.ToString(),                                  // 12 - User's Time Left
            "1",                                                        // 13 - Fossil?
        };

        string content = string.Join("\r\n", lines);
        File.WriteAllText(Path.Combine(nodeDir, "dorinfo.def"), content);
        File.WriteAllText(Path.Combine(nodeDir, "dorinfo1.def"), content);
        File.WriteAllText(Path.Combine(nodeDir, $"dorinfo{ni.Node}.def"), content);
    }
}
