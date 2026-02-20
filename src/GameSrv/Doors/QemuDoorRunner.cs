using System.Diagnostics;
using System.Net.Sockets;
using GameSrv.Config;
using GameSrv.Enums;
using GameSrv.Helpers;
using GameSrv.Session;

namespace GameSrv.Doors;

/// <summary>
/// Runs DOS door games inside a QEMU + FreeDOS virtual machine.
/// Each door session gets its own QEMU instance with:
///   C: = FreeDOS boot image (read-only + snapshot)
///   D: = Door game image (shared across nodes for multi-node play)
///   E: = Node drop files + external.bat (per-session)
/// I/O is bridged via QEMU serial port -> Unix domain socket.
/// </summary>
public class QemuDoorRunner : IDoorRunner
{
    public void Run(ClientThread clientThread, string command, string parameters)
    {
        var ni = clientThread.NodeInfo;
        string nodeDir = Path.Combine(StringHelper.StartupPath, "node" + ni.Node);
        string serialSock = Path.Combine(nodeDir, "serial.sock");
        string nodeImg = Path.Combine(nodeDir, "node.img");
        Process? qemuProcess = null;

        try
        {
            // Build the external.bat that will run inside FreeDOS
            CreateExternalBat(clientThread, command, parameters, nodeDir);

            // Build a FAT image containing drop files + external.bat
            BuildNodeImage(nodeImg, nodeDir, ni.Node);

            // Resolve paths
            string freeDosImage = ResolveFreeDosImage();
            string gameImage = ResolveGameImage(ni.Door!);
            int memory = ni.Door!.QemuMemory > 0 ? ni.Door.QemuMemory : ServerConfig.Instance.QemuDefaultMemory;

            // Build QEMU command line
            var args = BuildQemuArgs(freeDosImage, gameImage, nodeImg, serialSock, memory, ni.Door.SharedState);

            Log.Info($"Launching QEMU for node {ni.Node}: {ServerConfig.Instance.QemuBinary} {args}");

            // Clean up any stale socket
            try { File.Delete(serialSock); } catch { }

            // Start QEMU
            var psi = new ProcessStartInfo(ServerConfig.Instance.QemuBinary, args)
            {
                WorkingDirectory = StringHelper.StartupPath,
                UseShellExecute = false,
                CreateNoWindow = true,
                RedirectStandardOutput = false,
                RedirectStandardError = true,
            };

            qemuProcess = Process.Start(psi);
            if (qemuProcess == null)
            {
                Log.Error("Failed to start QEMU process");
                return;
            }

            // Read stderr asynchronously so it doesn't block
            qemuProcess.BeginErrorReadLine();
            qemuProcess.ErrorDataReceived += (_, e) =>
            {
                if (!string.IsNullOrEmpty(e.Data))
                    Log.Debug($"QEMU stderr: {e.Data}");
            };

            Log.Info($"QEMU started with PID {qemuProcess.Id}");

            // Connect to the serial socket and bridge I/O
            BridgeSerialIO(clientThread, qemuProcess, serialSock);
        }
        catch (Exception ex)
        {
            Log.Exception(ex, $"Error running QEMU door for node {ni.Node}");
        }
        finally
        {
            // Ensure QEMU is terminated
            if (qemuProcess != null && !qemuProcess.HasExited)
            {
                Log.Info($"Terminating QEMU process {qemuProcess.Id}");
                try { qemuProcess.Kill(entireProcessTree: true); } catch { }
            }
            qemuProcess?.Dispose();

            // Clean up socket and image
            try { File.Delete(serialSock); } catch { }
            try { File.Delete(nodeImg); } catch { }
        }
    }

    /// <summary>
    /// Create external.bat to be run inside FreeDOS via AUTOEXEC.BAT.
    /// Drive letter mapping (floppy boot):
    ///   A: = FreeDOS boot floppy
    ///   C: = door game files (IDE index 0)
    ///   D: = node directory with drop files (IDE index 1)
    /// </summary>
    private static void CreateExternalBat(ClientThread clientThread, string command, string parameters, string nodeDir)
    {
        var lines = new List<string>
        {
            "@echo off",
            "C:",
            "CD \\",
            // Copy drop files from D: (node image) to C: (game directory)
            "COPY D:\\DOOR.SYS C:\\ >NUL",
            "COPY D:\\DOOR32.SYS C:\\ >NUL",
            "COPY D:\\DORINFO.DEF C:\\ >NUL",
            "COPY D:\\DORINFO1.DEF C:\\ >NUL",
            "COPY D:\\DOORFILE.SR C:\\ >NUL",
            // Door game uses FOSSIL on COM1 for serial I/O directly.
            // No CTTY COM1 needed - avoids DOS noise in the serial stream.
            $"{command} {parameters}".Trim(),
            // Send exit sentinel via FOSSIL (BNU) on COM1 so GameSrv detects exit immediately
            "CTTY COM1",
            "ECHO **DOOREXIT**",
            "CTTY CON",
        };

        File.WriteAllText(Path.Combine(nodeDir, "external.bat"), string.Join("\r\n", lines));
    }

    /// <summary>
    /// Build a small FAT12 image containing the node's drop files and external.bat.
    /// Uses mtools (mformat + mcopy) which must be installed on the system.
    /// </summary>
    private static void BuildNodeImage(string imagePath, string nodeDir, int nodeNum)
    {
        // Create a 1.44MB floppy image
        try { File.Delete(imagePath); } catch { }

        // Use dd to create a blank image, then mformat to format it
        RunCommand("dd", $"if=/dev/zero of={imagePath} bs=1024 count=1440");
        RunCommand("mformat", $"-i {imagePath} -f 1440 ::");

        // Copy all drop files and external.bat into the image
        // Use HashSet to avoid duplicates (e.g. dorinfo1.def when node=1)
        var filesToCopy = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
        {
            "door.sys", "door32.sys", "doorfile.sr",
            "dorinfo.def", "dorinfo1.def", $"dorinfo{nodeNum}.def",
            "external.bat"
        };

        foreach (string file in filesToCopy)
        {
            string srcPath = Path.Combine(nodeDir, file);
            if (File.Exists(srcPath))
            {
                RunCommand("mcopy", $"-o -i {imagePath} {srcPath} ::{file.ToUpper()}");
            }
        }
    }

    private static string ResolveFreeDosImage()
    {
        string image = ServerConfig.Instance.FreeDosImage;
        if (!Path.IsPathRooted(image))
            image = Path.Combine(StringHelper.StartupPath, image);
        return image;
    }

    private static string ResolveGameImage(DoorInfo door)
    {
        string image = door.GameImagePath;
        if (string.IsNullOrEmpty(image))
        {
            // Default: look for game.img in the door's directory
            string doorDir = Path.GetDirectoryName(
                Path.Combine(StringHelper.StartupPath, "doors", door.Name?.ToLower() + ".ini")) ?? "";
            image = Path.Combine(doorDir, door.Name?.ToLower() + ".img");
        }
        if (!Path.IsPathRooted(image))
            image = Path.Combine(StringHelper.StartupPath, image);
        return image;
    }

    private static string BuildQemuArgs(string freeDosImage, string gameImage, string nodeImg,
        string serialSock, int memory, bool sharedState)
    {
        var args = new List<string>
        {
            "-machine pc",
            $"-m {memory}",
            "-display none",
            // Boot from floppy (A:) - FreeDOS boot image (read-only + snapshot per-drive)
            "-boot a",
            $"-drive file={freeDosImage},format=raw,if=floppy,index=0,snapshot=on",
        };

        // C: = Door game files (first IDE hard drive) - NO snapshot so data persists
        if (File.Exists(gameImage))
        {
            string lockOpt = sharedState ? ",file.locking=off" : "";
            args.Add($"-drive file={gameImage},format=raw,if=ide,index=0{lockOpt}");
        }

        // D: = Node drop files + external.bat (second IDE hard drive)
        args.Add($"-drive file={nodeImg},format=raw,if=ide,index=1");

        // Serial port -> Unix domain socket for I/O bridge
        args.Add($"-serial unix:{serialSock},server=on,wait=off");

        // No QEMU monitor, no reboot on exit
        args.Add("-monitor none");
        args.Add("-no-reboot");

        return string.Join(" ", args);
    }

    /// <summary>
    /// Connect to the QEMU serial port via Unix domain socket and bridge I/O
    /// between the serial port and the user's telnet/rlogin/websocket connection.
    /// </summary>
    private static void BridgeSerialIO(ClientThread clientThread, Process qemuProcess, string serialSockPath)
    {
        var ni = clientThread.NodeInfo;

        // Wait for the serial socket to appear (QEMU needs a moment to create it)
        int retries = 0;
        while (!File.Exists(serialSockPath) && retries++ < 50 && !qemuProcess.HasExited)
        {
            Thread.Sleep(100);
        }

        if (!File.Exists(serialSockPath))
        {
            Log.Error($"QEMU serial socket never appeared at {serialSockPath}");
            return;
        }

        // Small delay to let QEMU finish setting up the socket listener
        Thread.Sleep(200);

        using var socket = new Socket(AddressFamily.Unix, SocketType.Stream, ProtocolType.Unspecified);
        var endpoint = new UnixDomainSocketEndPoint(serialSockPath);

        // Retry connection a few times
        bool connected = false;
        for (int i = 0; i < 10 && !connected && !qemuProcess.HasExited; i++)
        {
            try
            {
                socket.Connect(endpoint);
                connected = true;
            }
            catch (SocketException)
            {
                Thread.Sleep(200);
            }
        }

        if (!connected)
        {
            Log.Error("Failed to connect to QEMU serial socket");
            return;
        }

        Log.Info("Connected to QEMU serial socket");

        using var serialStream = new NetworkStream(socket, ownsSocket: false);
        bool dataTransferred;
        int loopsSinceIO = 0;
        Exception? readException = null;

        // Wait for VM to boot and door game to initialize FOSSIL,
        // then drain any boot noise from the serial buffer.
        // This prevents DOS/BIOS output from reaching the user.
        // Also drain the client's input buffer to prevent stale data
        // from being sent to QEMU as keyboard input.
        Log.Info("Waiting for VM boot...");
        byte[] drain = new byte[4096];

        // Wait and keep draining until no data for 500ms
        int quietMs = 0;
        int totalDrained = 0;
        while (quietMs < 2000 && !qemuProcess.HasExited)
        {
            Thread.Sleep(100);
            if (socket.Available > 0)
            {
                int read = serialStream.Read(drain, 0, drain.Length);
                totalDrained += read;
                quietMs = 0; // Reset quiet timer
            }
            else
            {
                quietMs += 100;
            }
        }
        Log.Info($"Drained {totalDrained} bytes of boot noise");

        // Drain any stale client input (telnet negotiation, etc.)
        if (ni.Connection!.CanRead())
        {
            ni.Connection.ReadBytes();
        }

        Log.Info("Serial buffer drained, bridging I/O");

        // Track when we last received serial data from QEMU.
        // When the door exits, FOSSIL output stops. After a timeout
        // of no serial data, we assume the door has exited.
        long lastSerialDataTicks = Environment.TickCount64;
        int serialIdleTimeoutMs = (ni.Door?.IdleTimeout ?? 10) * 1000;

        // Task: serial -> client (read from QEMU, send to user)
        long totalSerialBytes = 0;
        bool doorExited = false; // Set when **DOOREXIT** sentinel is detected
        string sentinelBuffer = ""; // Accumulates tail of serial data to detect sentinel across reads
        const string DoorExitSentinel = "**DOOREXIT**";

        var serialToClient = Task.Run(() =>
        {
            try
            {
                byte[] buffer = new byte[4096];
                while (!clientThread.QuitThread() && !qemuProcess.HasExited)
                {
                    if (socket.Available > 0 || socket.Poll(100_000, SelectMode.SelectRead))
                    {
                        int bytesRead = serialStream.Read(buffer, 0, buffer.Length);
                        if (bytesRead > 0)
                        {
                            totalSerialBytes += bytesRead;
                            if (totalSerialBytes <= bytesRead)
                                Log.Info($"First serial data received: {bytesRead} bytes");
                            lastSerialDataTicks = Environment.TickCount64;

                            // Check for door exit sentinel in the serial data
                            string chunk = System.Text.Encoding.ASCII.GetString(buffer, 0, bytesRead);
                            sentinelBuffer += chunk;

                            // Keep only enough to detect the sentinel spanning two reads
                            if (sentinelBuffer.Length > DoorExitSentinel.Length * 2 + bytesRead)
                                sentinelBuffer = sentinelBuffer[^(DoorExitSentinel.Length + bytesRead)..];

                            if (sentinelBuffer.Contains(DoorExitSentinel))
                            {
                                Log.Info("Door exit sentinel detected");
                                doorExited = true;

                                // Strip the sentinel and any CTTY noise from output to user
                                int sentinelPos = chunk.IndexOf(DoorExitSentinel, StringComparison.Ordinal);
                                if (sentinelPos > 0)
                                {
                                    // Send data before the sentinel
                                    ni.Connection!.WriteBytes(buffer, sentinelPos);
                                }
                                // Don't send the sentinel or anything after it
                                break;
                            }

                            ni.Connection!.WriteBytes(buffer, bytesRead);
                        }
                        else if (bytesRead == 0)
                        {
                            break; // Socket closed
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                readException = ex;
            }
        });

        // Main loop: client -> serial (read from user, send to QEMU)
        while (!clientThread.QuitThread() && !qemuProcess.HasExited)
        {
            dataTransferred = false;

            // Check if door exit sentinel was detected
            if (doorExited)
            {
                Log.Info("Door exit detected via sentinel, ending I/O bridge");
                break;
            }

            // Check for exception in read task
            if (readException != null)
            {
                Log.Debug($"Serial read task ended: {readException.Message}");
                break;
            }

            // Check for dropped carrier
            if (!ni.Connection!.Connected)
            {
                Log.Info("User disconnected while in QEMU door");
                break;
            }

            // Send data from user to QEMU serial
            if (ni.Connection.CanRead())
            {
                byte[] bytes = ni.Connection.ReadBytes();
                if (bytes.Length > 0)
                {
                    try
                    {
                        serialStream.Write(bytes, 0, bytes.Length);
                        serialStream.Flush();
                        dataTransferred = true;
                        lastSerialDataTicks = Environment.TickCount64; // User activity resets idle timer
                    }
                    catch (Exception ex)
                    {
                        Log.Debug($"Serial write error: {ex.Message}");
                        break;
                    }
                }
            }

            if (!dataTransferred)
            {
                loopsSinceIO++;

                // Only check process termination after 300ms of no I/O
                if (loopsSinceIO >= 3 && qemuProcess.HasExited)
                    break;

                // If no serial data from QEMU for the timeout period
                // AND user hasn't sent input recently, the door has likely exited.
                long idleMs = Environment.TickCount64 - lastSerialDataTicks;
                if (idleMs > serialIdleTimeoutMs && !ni.Connection.CanRead())
                {
                    Log.Info($"No serial data for {idleMs}ms - door game has exited (totalSerial={totalSerialBytes}, connected={ni.Connection.Connected})");
                    break;
                }

                // Periodic keepalive to detect dead connections
                if (loopsSinceIO >= 300)
                {
                    switch (ni.ConnectionType)
                    {
                        case ConnectionType.RLogin:
                        case ConnectionType.WebSocket:
                            ni.Connection.Write("\0");
                            break;
                        case ConnectionType.Telnet:
                            ((Network.TelnetConnection)ni.Connection).SendGoAhead();
                            break;
                    }
                    loopsSinceIO = 0;
                }

                // Delay 100ms, breaking early if user sends data
                ni.Connection.CanRead(100);
            }
            else
            {
                loopsSinceIO = 0;
            }
        }

        // Wait briefly for the serial read task to finish
        serialToClient.Wait(TimeSpan.FromSeconds(2));
    }

    /// <summary>
    /// Run an external command and wait for it to complete.
    /// </summary>
    private static void RunCommand(string command, string arguments)
    {
        Log.Debug($"QEMU: Running: {command} {arguments}");

        var psi = new ProcessStartInfo(command, arguments)
        {
            UseShellExecute = false,
            CreateNoWindow = true,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
        };

        using var process = Process.Start(psi);
        if (process == null)
        {
            Log.Error($"Failed to run: {command} {arguments}");
            return;
        }

        // Read one stream async to avoid deadlock
        string stderr = "";
        var stderrTask = Task.Run(() => process.StandardError.ReadToEnd());
        string stdout = process.StandardOutput.ReadToEnd();
        if (stderrTask.Wait(TimeSpan.FromSeconds(10)))
            stderr = stderrTask.Result;

        process.WaitForExit(10_000);

        if (!process.HasExited)
        {
            Log.Warning($"{command} did not exit in time, killing");
            try { process.Kill(); } catch { }
        }
        else if (process.ExitCode != 0)
        {
            Log.Warning($"{command} exited with code {process.ExitCode}: {stderr.Trim()}");
        }
        else
        {
            Log.Debug($"QEMU: {command} completed OK");
        }
    }
}
