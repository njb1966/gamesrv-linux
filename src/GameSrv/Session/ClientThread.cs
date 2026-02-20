using System.Collections;
using System.Collections.Specialized;
using System.Text;
using System.Text.RegularExpressions;
using GameSrv.Auth;
using GameSrv.Config;
using GameSrv.Doors;
using GameSrv.Enums;
using GameSrv.Helpers;
using GameSrv.Menu;
using GameSrv.Network;
using GameSrv.Server;

namespace GameSrv.Session;

/// <summary>
/// Per-connection session handler. Manages the full user lifecycle:
/// connect -> authenticate -> menu -> door -> logoff.
/// Ported from RandM.GameSrv.ClientThread.
/// </summary>
public class ClientThread : IDisposable
{
    private string _currentMenu = "MAIN";
    private Dictionary<char, MenuOption> _currentMenuOptions = new();
    private string _lastDisplayFile = "";
    private readonly NodeInfo _nodeInfo = new();
    private readonly Random _random = new();
    private string _status = "";
    private volatile bool _stop;
    private Task? _task;
    private bool _disposed;

    public event EventHandler? FinishEvent;
    public event EventHandler<NodeEventArgs>? NodeEvent;
    public event EventHandler<WhoIsOnlineEventArgs>? WhoIsOnlineEvent;

    public ClientThread(IConnection connection, ConnectionType connectionType)
    {
        _nodeInfo.Connection = connection;
        _nodeInfo.ConnectionType = connectionType;
        _nodeInfo.TerminalType = ServerConfig.Instance.TerminalType;
    }

    public string Alias => _nodeInfo.User.Alias ?? "";
    public string IPAddress => _nodeInfo.Connection?.GetRemoteIP() ?? "0.0.0.0";
    public string Status => _status;
    public NodeInfo NodeInfo => _nodeInfo;

    public void Start()
    {
        _task = Task.Run(Execute);
    }

    public void Stop()
    {
        _stop = true;
        _nodeInfo.Connection?.Close();
    }

    public bool QuitThread()
    {
        if (_stop) return true;
        if (_nodeInfo.Connection == null || !_nodeInfo.Connection.Connected) return true;
        if (_nodeInfo.Connection.ReadTimedOut) return true;
        if (_nodeInfo.SecondsLeft <= 0) return true;
        return false;
    }

    private void Execute()
    {
        bool shouldRaiseLogOffEvent = false;

        try
        {
            _nodeInfo.Connection!.LineEnding = "\r";
            _nodeInfo.Connection.StripLF = true;

            // Check for ignored IP
            if (IpFilter.IsIgnoredIP(_nodeInfo.Connection.GetRemoteIP()))
            {
                Log.Debug($"Ignored {_nodeInfo.ConnectionType} connection from {_nodeInfo.Connection.GetRemoteIP()}:{_nodeInfo.Connection.GetRemotePort()}");
                return;
            }

            Log.Info($"Incoming {_nodeInfo.ConnectionType} connection from {_nodeInfo.Connection.GetRemoteIP()}:{_nodeInfo.Connection.GetRemotePort()}");

            // Detect terminal type if needed
            if (_nodeInfo.TerminalType == TerminalType.AUTODETECT) GetTerminalType();

            // Check allowlist/blocklist
            if (_nodeInfo.ConnectionType == ConnectionType.RLogin && !IpFilter.IsRLoginIP(_nodeInfo.Connection.GetRemoteIP()))
            {
                Log.Warning($"IP {_nodeInfo.Connection.GetRemoteIP()} doesn't match RLogin IP allowlist");
                return;
            }
            if (IpFilter.IsBannedIP(_nodeInfo.Connection.GetRemoteIP()))
            {
                Log.Warning($"IP {_nodeInfo.Connection.GetRemoteIP()} matches banned IP filter");
                DisplayAnsi("IP_BANNED");
                Thread.Sleep(2500);
                return;
            }
            if (!_nodeInfo.Connection.Connected)
            {
                Log.Info("No carrier detected (probably a portscanner)");
                return;
            }

            // Get free node
            _nodeInfo.Node = NodeManager.GetFreeNode(this);
            if (_nodeInfo.Node == 0)
            {
                DisplayAnsi("SERVER_BUSY");
                Thread.Sleep(2500);
                return;
            }

            shouldRaiseLogOffEvent = true;

            // Check for RUNBBS mode
            _nodeInfo.Door = new DoorInfo("RUNBBS");
            if (_nodeInfo.Door.Loaded)
            {
                _nodeInfo.User.Alias = "Anonymous";
                NodeEvent?.Invoke(this, new NodeEventArgs(_nodeInfo, "Running RUNBBS.INI process", NodeEventType.LogOn));
                _nodeInfo.SecondsThisSession = 86400;
                new RunDoor(this).Run();
                return;
            }

            // Authenticate
            bool authed = _nodeInfo.ConnectionType == ConnectionType.RLogin
                ? AuthenticateRLogin()
                : AuthenticateTelnet();
            if (!authed || QuitThread()) return;

            // Set session time
            _nodeInfo.UserLoggedOn = true;
            _nodeInfo.SecondsThisSession = ServerConfig.Instance.TimePerCall * 60;
            NodeEvent?.Invoke(this, new NodeEventArgs(_nodeInfo, "Logging on", NodeEventType.LogOn));

            // Check for RLogin xtrn= direct door launch
            if (_nodeInfo.Door != null && _nodeInfo.Door.Loaded)
            {
                new RunDoor(this).Run();
                Thread.Sleep(2500);
                return;
            }

            // Logon process
            UpdateStatus("Running Logon Process");
            LogOnProcess.Run(this);
            if (QuitThread()) return;

            // Logoff process
            UpdateStatus("Running Logoff Process");
            LogOffProcess.Run(this);
            if (QuitThread()) return;

            DisplayAnsi("LOGOFF");
            Thread.Sleep(2500);
        }
        catch (Exception ex)
        {
            Log.Exception(ex, "Exception in ClientThread::Execute()");
        }
        finally
        {
            try { _nodeInfo.Connection?.Close(); } catch { }
            if (shouldRaiseLogOffEvent)
            {
                try { NodeEvent?.Invoke(this, new NodeEventArgs(_nodeInfo, "Logging off", NodeEventType.LogOff)); } catch { }
            }
            try { FinishEvent?.Invoke(this, EventArgs.Empty); } catch { }
        }
    }

    #region Authentication

    private bool AuthenticateRLogin()
    {
        var rlogin = (RLoginConnection)_nodeInfo.Connection!;
        string userName = rlogin.ServerUserName;
        string password = rlogin.ClientUserName;
        string termType = rlogin.TerminalType;

        Log.Info($"RLogin auth: user='{userName}' pass='{password}' term='{termType}'");

        if (string.IsNullOrEmpty(userName) || string.IsNullOrEmpty(password))
        {
            Log.Info("RLogin auth failed: empty username or password");
            if (ServerConfig.Instance.RLoginPromptForCredentialsOnFailedLogOn)
                return AuthenticateTelnet();
            DisplayAnsi("RLOGIN_INVALID");
            return false;
        }

        // Check for xtrn= door request
        if (termType.StartsWith("xtrn=", StringComparison.OrdinalIgnoreCase))
        {
            string doorName = termType.Substring(5);
            Log.Info($"RLogin xtrn door request: '{doorName}'");
            _nodeInfo.Door = new DoorInfo(doorName);
            if (!_nodeInfo.Door.Loaded)
            {
                Log.Warning($"RLogin xtrn door not found: '{doorName}'");
                DisplayAnsi("RLOGIN_INVALID_XTRN");
                return false;
            }
        }

        _nodeInfo.User = new UserInfo(userName);
        if (_nodeInfo.User.Loaded)
        {
            if (ServerConfig.Instance.RLoginValidatePassword && !_nodeInfo.User.ValidatePassword(password))
            {
                if (ServerConfig.Instance.RLoginPromptForCredentialsOnFailedLogOn)
                    return AuthenticateTelnet();
                DisplayAnsi("RLOGIN_INVALID_PASSWORD");
                return false;
            }
        }
        else
        {
            // New user - auto-register if configured, otherwise prompt or reject
            if (ServerConfig.Instance.RLoginSkipNewUserPrompts)
            {
                if (IpFilter.IsBannedUser(userName))
                {
                    Log.Warning($"RLogin user not allowed due to banned alias: '{userName}'");
                    return false;
                }
                lock (typeof(UserInfo))
                {
                    if (_nodeInfo.User.StartRegistration(userName))
                    {
                        _nodeInfo.User.SetPassword(password);
                        _nodeInfo.User.UserId = ServerConfig.Instance.NextUserId++;
                        _nodeInfo.User.SaveRegistration();
                        ServerConfig.Instance.Save();
                    }
                    else
                    {
                        Log.Warning($"RLogin user lost a race condition and couldn't register as '{userName}'");
                    }
                }
            }
            else
            {
                return Register(userName, password);
            }
        }

        return true;
    }

    private bool AuthenticateTelnet()
    {
        DisplayAnsi("LOGON_HEADER");

        int failedAttempts = 0;
        while (failedAttempts++ < 3)
        {
            UpdateStatus("Entering Alias");
            DisplayAnsi("LOGON_ENTER_ALIAS");
            string alias = ReadLn().Trim();
            if (QuitThread()) return false;

            if (string.Equals(alias, "NEW", StringComparison.OrdinalIgnoreCase))
            {
                bool canRegister = false;
                if (string.IsNullOrEmpty(ServerConfig.Instance.NewUserPassword))
                {
                    canRegister = true;
                }
                else
                {
                    UpdateStatus("Entering New User Password");
                    DisplayAnsi("NEWUSER_ENTER_NEWUSER_PASSWORD");
                    string newUserPassword = ReadLn('*').Trim();
                    _nodeInfo.Connection!.WriteLn();
                    canRegister = newUserPassword == ServerConfig.Instance.NewUserPassword;
                }

                if (QuitThread()) return false;

                if (canRegister)
                {
                    UpdateStatus("Registering as new user");
                    return Register();
                }
                else
                {
                    UpdateStatus("Entered invalid newuser password");
                }
            }
            else if (IpFilter.IsBannedUser(alias))
            {
                IpFilter.AddTempIgnoredIP(_nodeInfo.Connection!.GetRemoteIP());
                Log.Warning($"IP banned for trying to log in as {alias}");
                DisplayAnsi("USER_BANNED");
                return false;
            }
            else if (!string.IsNullOrEmpty(alias))
            {
                UpdateStatus($"Logging on as {alias}");
                UpdateStatus("Entering Password");
                DisplayAnsi("LOGON_ENTER_PASSWORD");
                string password = ReadLn('*').Trim();
                if (QuitThread()) return false;

                UpdateStatus("Validating Credentials");
                _nodeInfo.User = new UserInfo(alias);
                if (_nodeInfo.User.Loaded && _nodeInfo.User.ValidatePassword(password))
                    return true;
                DisplayAnsi("LOGON_INVALID");
            }
        }

        DisplayAnsi("LOGON_FAILED");
        return false;
    }

    private bool Register(string? defaultUserName = null, string? defaultPassword = null)
    {
        bool registered = false;
        try
        {
            DisplayAnsi("NEWUSER_HEADER");

            // Get alias
            string alias;
            if (string.IsNullOrEmpty(defaultUserName))
            {
                while (true)
                {
                    alias = "";
                    while (string.IsNullOrEmpty(alias) || string.Equals(alias, "NEW", StringComparison.OrdinalIgnoreCase))
                    {
                        DisplayAnsi("NEWUSER_ENTER_ALIAS");
                        alias = ReadLn().Trim();
                        if (QuitThread()) return false;
                    }

                    if (!IpFilter.IsBannedUser(alias) && _nodeInfo.User.StartRegistration(alias))
                        break;
                    DisplayAnsi("NEWUSER_ENTER_ALIAS_DUPLICATE");
                }
            }
            else
            {
                alias = defaultUserName;
            }

            // Get password
            if (string.IsNullOrEmpty(defaultPassword))
            {
                while (true)
                {
                    string password = "";
                    while (string.IsNullOrEmpty(password))
                    {
                        DisplayAnsi("NEWUSER_ENTER_PASSWORD");
                        password = ReadLn('*').Trim();
                        if (QuitThread()) return false;
                    }
                    _nodeInfo.User.SetPassword(password);

                    DisplayAnsi("NEWUSER_ENTER_PASSWORD_CONFIRM");
                    string confirm = ReadLn('*').Trim();
                    if (QuitThread()) return false;

                    if (_nodeInfo.User.ValidatePassword(confirm))
                        break;
                    DisplayAnsi("NEWUSER_ENTER_PASSWORD_MISMATCH");
                }
            }
            else
            {
                _nodeInfo.User.SetPassword(defaultPassword);
            }

            // Loop through registration questions
            string[] questions = NewUserQuestion.GetQuestions();
            foreach (string question in questions)
            {
                var q = new NewUserQuestion(question);

                while (true)
                {
                    if (!DisplayAnsi("NEWUSER_ENTER_" + question))
                    {
                        Log.Error($"Unable to prompt new user for '{question}' since ansi\\newuser_enter_{question.ToLower()}.ans is missing");
                        break;
                    }

                    string answer = ReadLn().Trim();
                    if (QuitThread()) return false;

                    if (q.Required && string.IsNullOrEmpty(answer)) continue;

                    if (!string.IsNullOrEmpty(answer))
                    {
                        bool valid = q.Validate switch
                        {
                            ValidationType.Email => answer.Contains('@') && answer.Contains('.'),
                            ValidationType.Numeric => double.TryParse(answer, out _),
                            ValidationType.TwoWords => answer.Contains(' '),
                            _ => true
                        };

                        if (!valid)
                        {
                            if (!DisplayAnsi("NEWUSER_ENTER_" + question + "_INVALID"))
                                _nodeInfo.Connection!.WriteLn("Input is not valid!");
                            continue;
                        }
                    }

                    if (q.Confirm)
                    {
                        if (!DisplayAnsi("NEWUSER_ENTER_" + question + "_CONFIRM"))
                            _nodeInfo.Connection!.Write("Please re-enter: ");

                        string confirmAnswer = ReadLn().Trim();
                        if (QuitThread()) return false;

                        if (confirmAnswer != answer)
                        {
                            if (!DisplayAnsi("NEWUSER_ENTER_" + question + "_MISMATCH"))
                                _nodeInfo.Connection!.WriteLn("Text does not match!");
                            continue;
                        }
                    }

                    _nodeInfo.User.AdditionalInfo[question] = answer;
                    break;
                }
            }

            registered = true;
            return true;
        }
        finally
        {
            if (registered)
            {
                lock (typeof(UserInfo))
                {
                    _nodeInfo.User.UserId = ServerConfig.Instance.NextUserId++;
                    _nodeInfo.User.SaveRegistration();
                    ServerConfig.Instance.Save();
                }
                DisplayAnsi("NEWUSER_SUCCESS", true);
            }
            else
            {
                _nodeInfo.User.AbortRegistration();
            }
        }
    }

    #endregion

    #region Menu System

    public void MainMenu()
    {
        while (!QuitThread())
        {
            GetCurrentMenu();
            DisplayCurrentMenu();
            UpdateStatus($"At {_currentMenu.ToUpper()} menu");

            string hotKey = ReadChar()?.ToString().ToUpper() ?? "";
            if (!string.IsNullOrEmpty(hotKey) && !QuitThread())
            {
                if (_currentMenuOptions.ContainsKey(hotKey[0]))
                {
                    var mo = _currentMenuOptions[hotKey[0]];
                    if (HandleMenuOption(mo))
                        return;
                }
            }
        }
    }

    public bool HandleMenuOption(MenuOption menuOption)
    {
        if (_nodeInfo.User.AccessLevel < menuOption.RequiredAccess) return false;

        switch (menuOption.Action)
        {
            case MenuAction.ChangeMenu:
                UpdateStatus($"Changing to {menuOption.Parameters.ToUpper()} menu");
                _currentMenu = menuOption.Parameters.ToUpper();
                return false;
            case MenuAction.Disconnect:
                UpdateStatus("Disconnecting");
                _nodeInfo.Connection?.Close();
                return true;
            case MenuAction.DisplayFile:
                UpdateStatus($"Displaying {menuOption.Parameters}");
                DisplayFile(menuOption.Parameters, true, false, false);
                return false;
            case MenuAction.DisplayFileMore:
                UpdateStatus($"Displaying {menuOption.Parameters} (with more)");
                DisplayFile(menuOption.Parameters, true, true, true);
                return false;
            case MenuAction.DisplayFilePause:
                UpdateStatus($"Displaying {menuOption.Parameters} (with pause)");
                DisplayFile(menuOption.Parameters, true, true, false);
                return false;
            case MenuAction.LogOff:
                UpdateStatus("Logging off");
                return true;
            case MenuAction.MainMenu:
                UpdateStatus($"Changing to {menuOption.Parameters.ToUpper()} menu");
                _currentMenu = menuOption.Parameters.ToUpper();
                MainMenu();
                return true;
            case MenuAction.Pause:
                UpdateStatus($"Pausing for {menuOption.Parameters} ms");
                if (int.TryParse(menuOption.Parameters, out int ms))
                    Thread.Sleep(ms);
                return false;
            case MenuAction.RunDoor:
                UpdateStatus($"Running {menuOption.Parameters}");
                new RunDoor(this).Run(menuOption.Parameters);
                return false;
            case MenuAction.Telnet:
                UpdateStatus($"Telnetting to {menuOption.Parameters}");
                TelnetProxy(menuOption.Parameters);
                return false;
        }
        return false;
    }

    private void GetCurrentMenu()
    {
        _currentMenuOptions.Clear();
        string[] hotKeys = MenuOption.GetHotkeys(_currentMenu);

        foreach (string hk in hotKeys)
        {
            char hotKey = hk.ToUpper()[0];
            try
            {
                var mo = new MenuOption(_currentMenu, hotKey);
                if (!mo.Loaded || _nodeInfo.User.AccessLevel < mo.RequiredAccess) continue;

                bool canAdd = true;
                if (mo.Action == MenuAction.RunDoor)
                {
                    var di = new DoorInfo(mo.Parameters);
                    if (di.Loaded)
                    {
                        canAdd = di.Platform switch
                        {
                            DoorPlatform.DOS => IsQemuAvailable(),
                            DoorPlatform.Linux => true,
                            _ => false
                        };
                    }
                    else
                    {
                        canAdd = false;
                    }
                }

                if (canAdd) _currentMenuOptions[hotKey] = mo;
            }
            catch (Exception ex)
            {
                Log.Exception(ex, $"Unable to load '{_currentMenu}' menu option for '{hotKey}'");
            }
        }
    }

    private bool IsQemuAvailable()
    {
        return File.Exists(ServerConfig.Instance.QemuBinary);
    }

    private void DisplayCurrentMenu()
    {
        // Try to find a custom ANSI/ASCII menu file
        var filesToCheck = new List<string>();
        string basePath = StringHelper.StartupPath;

        // Access level specific
        if (_nodeInfo.TerminalType == TerminalType.RIP || _nodeInfo.TerminalType == TerminalType.ANSI)
            filesToCheck.Add(Path.Combine(basePath, "menus", $"{_currentMenu.ToLower()}{_nodeInfo.User.AccessLevel}.ans"));
        filesToCheck.Add(Path.Combine(basePath, "menus", $"{_currentMenu.ToLower()}{_nodeInfo.User.AccessLevel}.asc"));

        // No specified access level
        if (_nodeInfo.TerminalType == TerminalType.RIP || _nodeInfo.TerminalType == TerminalType.ANSI)
            filesToCheck.Add(Path.Combine(basePath, "menus", $"{_currentMenu.ToLower()}.ans"));
        filesToCheck.Add(Path.Combine(basePath, "menus", $"{_currentMenu.ToLower()}.asc"));

        foreach (string file in filesToCheck)
        {
            if (File.Exists(file))
            {
                DisplayFile(file, true, false, false);
                return;
            }
        }

        // Fallback: generate a text-based menu
        ClrScr();
        _nodeInfo.Connection!.WriteLn($"\x1b[1;37m--- {_currentMenu.ToUpper()} MENU ---\x1b[0m");
        _nodeInfo.Connection.WriteLn();

        var sorted = _currentMenuOptions.OrderBy(kv => kv.Key).ToList();
        foreach (var kv in sorted)
        {
            _nodeInfo.Connection.WriteLn($"  \x1b[1;33m{kv.Key}\x1b[1;30m] \x1b[0;37m{kv.Value.Name}");
        }

        _nodeInfo.Connection.WriteLn();
        string timeLeft = $"{_nodeInfo.MinutesLeft}:{(_nodeInfo.SecondsLeft % 60):D2}";
        _nodeInfo.Connection.Write($"\x1b[1;30m[{timeLeft}]\x1b[1;37m Select: \x1b[0m");
    }

    #endregion

    #region Display & I/O

    public void ClrScr()
    {
        switch (_nodeInfo.TerminalType)
        {
            case TerminalType.ANSI:
                _nodeInfo.Connection!.Write("\x1b[0m\x1b[2J\x1b[H");
                break;
            case TerminalType.ASCII:
                _nodeInfo.Connection!.Write("\r\n\x0C");
                break;
            case TerminalType.RIP:
                _nodeInfo.Connection!.Write("\r\n!|*\x1b[0m\x1b[2J\x1b[H");
                break;
        }
    }

    public bool DisplayAnsi(string fileName, bool pauseAtEnd = false)
    {
        if (string.IsNullOrEmpty(fileName)) return false;

        var filesToCheck = new List<string>();
        string basePath = StringHelper.StartupPath;

        if (_nodeInfo.TerminalType == TerminalType.RIP)
            filesToCheck.Add(Path.Combine(basePath, "ansi", fileName.ToLower() + ".rip"));
        if (_nodeInfo.TerminalType == TerminalType.RIP || _nodeInfo.TerminalType == TerminalType.ANSI)
            filesToCheck.Add(Path.Combine(basePath, "ansi", fileName.ToLower() + ".ans"));
        filesToCheck.Add(Path.Combine(basePath, "ansi", fileName.ToLower() + ".asc"));

        foreach (string file in filesToCheck)
        {
            if (File.Exists(file))
                return DisplayFile(file, false, pauseAtEnd, false);
        }

        return false;
    }

    private bool DisplayFile(string fileName, bool clearAtBeginning, bool pauseAtEnd, bool pauseAfter24)
    {
        try
        {
            _lastDisplayFile = fileName;
            if (clearAtBeginning) ClrScr();

            // Normalize path separators for Linux
            fileName = fileName.Replace("\\", "/");

            // Handle @index files
            if (fileName.StartsWith("@"))
            {
                fileName = fileName.Substring(1);
                if (!File.Exists(fileName)) return false;
                string[] fileNames = File.ReadAllLines(fileName);
                fileName = fileNames[_random.Next(0, fileNames.Length)].Replace("\\", "/");
                _lastDisplayFile = fileName;
            }

            // Check for extension based on terminal type
            if (!File.Exists(fileName) && !Path.HasExtension(fileName))
            {
                string[] extensions = _nodeInfo.TerminalType switch
                {
                    TerminalType.RIP => new[] { ".rip", ".ans", ".asc" },
                    TerminalType.ANSI => new[] { ".ans", ".asc" },
                    _ => new[] { ".asc" }
                };
                foreach (string ext in extensions)
                {
                    if (File.Exists(fileName + ext))
                    {
                        fileName = fileName + ext;
                        break;
                    }
                }
            }

            if (!File.Exists(fileName)) return false;

            string text = File.ReadAllText(fileName, Encoding.GetEncoding(437));
            text = TranslateMCI(text, fileName);

            // Strip SAUCE record
            int sauceIdx = text.IndexOf('\x1A');
            if (sauceIdx >= 0) text = text.Substring(0, sauceIdx);

            // Normalize line endings to \r\n for telnet
            text = text.Replace("\r\n", "\n").Replace("\r", "\n").Replace("\n", "\r\n");

            // Handle {PAUSE} markers
            if (text.Contains("{PAUSE}"))
            {
                string[] pages = text.Split(new[] { "{PAUSE}" }, StringSplitOptions.None);
                for (int i = 0; i < pages.Length; i++)
                {
                    _nodeInfo.Connection!.Write(pages[i]);
                    if (i < pages.Length - 1) ReadChar();
                }
            }
            else if (pauseAfter24)
            {
                string[] lines = text.Split(new[] { "\r\n" }, StringSplitOptions.None);
                if (lines.Length <= 24)
                {
                    _nodeInfo.Connection!.Write(text);
                }
                else
                {
                    for (int i = 0; i < lines.Length; i++)
                    {
                        _nodeInfo.Connection!.Write(lines[i]);
                        if (i < lines.Length - 1) _nodeInfo.Connection.WriteLn();

                        if (i % 24 == 23 && i < lines.Length - 1)
                        {
                            _nodeInfo.Connection.Write("<more>");
                            var ch = ReadChar();
                            _nodeInfo.Connection.Write("\b\b\b\b\b\b      \b\b\b\b\b\b");
                            if (ch?.ToString().ToUpper() == "Q") return true;
                        }
                    }
                }
            }
            else
            {
                _nodeInfo.Connection!.Write(text);
            }

            if (pauseAtEnd)
            {
                ReadChar();
                ClrScr();
            }
            return true;
        }
        catch (IOException ex)
        {
            Log.Exception(ex, $"Unable to display '{fileName}'");
            return false;
        }
    }

    private char? ReadChar()
    {
        char? result = _nodeInfo.Connection!.ReadChar(_nodeInfo.ReadTimeout);
        if (result == null && !_stop && _nodeInfo.Connection.Connected)
        {
            if (_nodeInfo.SecondsLeft > 0)
            {
                DisplayAnsi("EXCEEDED_IDLE_LIMIT");
                Thread.Sleep(2500);
                _nodeInfo.Connection.Close();
            }
            else
            {
                DisplayAnsi("EXCEEDED_CALL_LIMIT");
                Thread.Sleep(2500);
                _nodeInfo.Connection.Close();
            }
        }
        return result;
    }

    private string ReadLn(char passwordChar = '\0')
    {
        string result = _nodeInfo.Connection!.ReadLn(passwordChar, _nodeInfo.ReadTimeout);
        if (_nodeInfo.Connection.ReadTimedOut && !_stop && _nodeInfo.Connection.Connected)
        {
            if (_nodeInfo.SecondsLeft > 0)
            {
                DisplayAnsi("EXCEEDED_IDLE_LIMIT");
                Thread.Sleep(2500);
                _nodeInfo.Connection.Close();
            }
            else
            {
                DisplayAnsi("EXCEEDED_CALL_LIMIT");
                Thread.Sleep(2500);
                _nodeInfo.Connection.Close();
            }
        }
        return result;
    }

    #endregion

    #region Terminal Detection

    private void GetTerminalType()
    {
        try
        {
            Thread.Sleep(200);
            _nodeInfo.Connection!.ReadString(); // flush
            _nodeInfo.Connection.Write("\r\n\x1b[s\x1b[255B\x1b[255C\b_\x1b[6n\x1b[u\x1b[!_\x1b[0m_\x1b[2J\x1b[H\x0C\r");

            int i = 0;
            string str = "";
            while (i++ < 50)
            {
                char? c = _nodeInfo.Connection.ReadChar(100);
                if (_nodeInfo.Connection.ReadTimedOut || c == null) continue;
                c = (char)(c & 0x7f);
                if (c == 0) continue;
                i = 0;
                if (string.IsNullOrEmpty(str) && c != '\x1b') continue;
                str += c;
                if (c == 'R') { Thread.Sleep(500); break; }
            }

            while (_nodeInfo.Connection.CanRead(100))
                str += _nodeInfo.Connection.ReadString();

            if (str.ToUpper().Contains("RIPSCRIP"))
                _nodeInfo.TerminalType = TerminalType.RIP;
            else if (Regex.IsMatch(str, @"\x1b\[\d{1,3};\d{1,3}R"))
                _nodeInfo.TerminalType = TerminalType.ANSI;
            else
                _nodeInfo.TerminalType = TerminalType.ASCII;
        }
        catch
        {
            _nodeInfo.TerminalType = TerminalType.ASCII;
        }
    }

    #endregion

    #region MCI Translation

    private string TranslateMCI(string text, string fileName)
    {
        var mci = new StringDictionary
        {
            { "ACCESSLEVEL", _nodeInfo.User.AccessLevel.ToString() },
            { "ALIAS", _nodeInfo.User.Alias },
            { "BBSNAME", ServerConfig.Instance.BBSName },
            { "DATE", DateTime.Now.ToShortDateString() },
            { "FILENAME", fileName.Replace(StringHelper.StartupPath, "") },
            { "GSDIR", StringHelper.StartupPath },
            { "MENUNAME", _currentMenu },
            { "NODE", _nodeInfo.Node.ToString() },
            { "OPERATINGSYSTEM", $"Linux {Environment.OSVersion.Version}" },
            { "SYSOPEMAIL", ServerConfig.Instance.SysopEmail },
            { "SYSOPNAME", $"{ServerConfig.Instance.SysopFirstName} {ServerConfig.Instance.SysopLastName}" },
            { "TIME", DateTime.Now.ToShortTimeString() },
            { "TIMELEFT", $"{_nodeInfo.MinutesLeft}:{(_nodeInfo.SecondsLeft % 60):D2}" },
        };

        foreach (DictionaryEntry de in _nodeInfo.User.AdditionalInfo)
        {
            if (de.Key != null && de.Value != null)
                mci[de.Key.ToString()!] = de.Value.ToString()!;
        }

        // Who's online MCI
        if (text.Contains("WHOSONLINE_"))
        {
            var handler = WhoIsOnlineEvent;
            if (handler != null)
            {
                var args = new WhoIsOnlineEventArgs();
                handler(this, args);
                foreach (DictionaryEntry de in args.WhoIsOnline)
                {
                    if (de.Key != null && de.Value != null)
                        mci[de.Key.ToString()!] = de.Value.ToString()!;
                }
            }
        }

        // Perform MCI translations
        foreach (DictionaryEntry de in mci)
        {
            if (de.Value == null) continue;
            string key = de.Key!.ToString()!.ToUpper();
            string val = de.Value.ToString()!;

            text = text.Replace("{" + key + "}", val);
            for (int pad = 1; pad <= 80; pad++)
            {
                text = text.Replace("{" + key + pad + "}", val.PadRight(pad));
                text = text.Replace("{" + pad + key + "}", val.PadLeft(pad));
            }
        }

        return text;
    }

    #endregion

    #region Telnet Proxy

    private void TelnetProxy(string hostname)
    {
        ClrScr();
        _nodeInfo.Connection!.WriteLn();
        _nodeInfo.Connection.Write(" Connecting to remote server...");

        int port = 23;
        if (hostname.Contains(':'))
        {
            var parts = hostname.Split(':', 2);
            hostname = parts[0];
            if (int.TryParse(parts[1], out int p) && p > 0 && p <= 65535)
                port = p;
        }

        using var remote = new TelnetConnection();
        if (!remote.Connect(hostname, port))
        {
            _nodeInfo.Connection.WriteLn("failed!");
            _nodeInfo.Connection.WriteLn();
            _nodeInfo.Connection.WriteLn(" Looks like the remote server isn't online, please try back later.");
            return;
        }

        _nodeInfo.Connection.WriteLn("connected!");

        while (remote.Connected && !QuitThread())
        {
            bool yield = true;

            if (remote.CanRead())
            {
                _nodeInfo.Connection.Write(remote.ReadString());
                yield = false;
            }

            if (_nodeInfo.Connection.CanRead())
            {
                remote.Write(_nodeInfo.Connection.ReadString());
                yield = false;
            }

            if (yield) Thread.Sleep(1);
        }

        if (_nodeInfo.Connection.Connected && !remote.Connected)
        {
            _nodeInfo.Connection.WriteLn();
            _nodeInfo.Connection.WriteLn();
            _nodeInfo.Connection.WriteLn(" Remote server closed the connection.");
            ReadChar();
        }
    }

    #endregion

    public void UpdateStatus(string newStatus)
    {
        _status = newStatus;
        NodeEvent?.Invoke(this, new NodeEventArgs(_nodeInfo, newStatus, NodeEventType.StatusChange));
    }

    public void Dispose()
    {
        if (!_disposed)
        {
            _disposed = true;
            _nodeInfo.Connection?.Dispose();
        }
    }
}
