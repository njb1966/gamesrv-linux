namespace GameSrv.Config;

/// <summary>
/// Simple INI file parser that preserves comments and ordering.
/// Replaces RMLib.IniFile with compatible behavior.
/// </summary>
public class IniFile : IDisposable
{
    private readonly string _filePath;
    private readonly List<IniLine> _lines = new();


    public IniFile(string filePath)
    {
        _filePath = filePath;
        if (File.Exists(filePath))
        {
            Load();
        }
    }

    private void Load()
    {
        _lines.Clear();
        foreach (string raw in File.ReadAllLines(_filePath))
        {
            string line = raw.TrimEnd();
            if (string.IsNullOrWhiteSpace(line) || line.StartsWith(';'))
            {
                _lines.Add(new IniLine { Type = IniLineType.Other, Raw = line });
            }
            else if (line.StartsWith('[') && line.Contains(']'))
            {
                int end = line.IndexOf(']');
                string section = line.Substring(1, end - 1);
                _lines.Add(new IniLine { Type = IniLineType.Section, Section = section, Raw = line });
            }
            else if (line.Contains('='))
            {
                int eq = line.IndexOf('=');
                string key = line.Substring(0, eq);
                string value = line.Substring(eq + 1);
                // Find which section this belongs to
                string? currentSection = null;
                for (int i = _lines.Count - 1; i >= 0; i--)
                {
                    if (_lines[i].Type == IniLineType.Section)
                    {
                        currentSection = _lines[i].Section;
                        break;
                    }
                }
                _lines.Add(new IniLine
                {
                    Type = IniLineType.KeyValue,
                    Section = currentSection,
                    Key = key,
                    Value = value,
                    Raw = line
                });
            }
            else
            {
                _lines.Add(new IniLine { Type = IniLineType.Other, Raw = line });
            }
        }
    }

    public string[] ReadSections()
    {
        return _lines
            .Where(l => l.Type == IniLineType.Section)
            .Select(l => l.Section!)
            .ToArray();
    }

    public string ReadString(string section, string key, string defaultValue)
    {
        var line = FindKey(section, key);
        return line?.Value ?? defaultValue;
    }

    public int ReadInt(string section, string key, int defaultValue)
    {
        string val = ReadString(section, key, "");
        return int.TryParse(val, out int result) ? result : defaultValue;
    }

    public bool ReadBoolean(string section, string key, bool defaultValue)
    {
        string val = ReadString(section, key, "").ToLower();
        if (val == "true" || val == "1" || val == "yes") return true;
        if (val == "false" || val == "0" || val == "no") return false;
        return defaultValue;
    }

    public T ReadEnum<T>(string section, string key, T defaultValue) where T : struct, Enum
    {
        string val = ReadString(section, key, "");
        if (Enum.TryParse<T>(val, true, out T result)) return result;
        return defaultValue;
    }

    public void WriteString(string section, string key, string value)
    {
        var line = FindKey(section, key);
        if (line != null)
        {
            line.Value = value;
            line.Raw = $"{key}={value}";
        }
        else
        {
            // Find or create section
            EnsureSection(section);
            // Insert after the last key in this section (or after the section header)
            int insertIdx = FindSectionEnd(section);
            _lines.Insert(insertIdx, new IniLine
            {
                Type = IniLineType.KeyValue,
                Section = section,
                Key = key,
                Value = value,
                Raw = $"{key}={value}"
            });
        }
    }

    public void WriteInt(string section, string key, int value)
    {
        WriteString(section, key, value.ToString());
    }

    public void WriteBoolean(string section, string key, bool value)
    {
        WriteString(section, key, value ? "True" : "False");
    }

    public void WriteEnum<T>(string section, string key, T value) where T : struct, Enum
    {
        WriteString(section, key, value.ToString());
    }

    /// <summary>
    /// Returns all key names in a section.
    /// </summary>
    public string[] ReadKeys(string section)
    {
        return _lines
            .Where(l => l.Type == IniLineType.KeyValue &&
                        string.Equals(l.Section, section, StringComparison.OrdinalIgnoreCase))
            .Select(l => l.Key!)
            .ToArray();
    }

    public bool SectionExists(string section)
    {
        return _lines.Any(l => l.Type == IniLineType.Section &&
                              string.Equals(l.Section, section, StringComparison.OrdinalIgnoreCase));
    }

    public void Save()
    {
        string? dir = Path.GetDirectoryName(_filePath);
        if (!string.IsNullOrEmpty(dir))
            Directory.CreateDirectory(dir);
        File.WriteAllLines(_filePath, _lines.Select(l => l.Raw));
    }

    public void Dispose()
    {
        // Nothing to dispose, but kept for compatibility with using() pattern
    }

    private IniLine? FindKey(string section, string key)
    {
        return _lines.FirstOrDefault(l =>
            l.Type == IniLineType.KeyValue &&
            string.Equals(l.Section, section, StringComparison.OrdinalIgnoreCase) &&
            string.Equals(l.Key, key, StringComparison.OrdinalIgnoreCase));
    }

    private void EnsureSection(string section)
    {
        if (!SectionExists(section))
        {
            if (_lines.Count > 0)
                _lines.Add(new IniLine { Type = IniLineType.Other, Raw = "" });
            _lines.Add(new IniLine { Type = IniLineType.Section, Section = section, Raw = $"[{section}]" });
        }
    }

    private int FindSectionEnd(string section)
    {
        int sectionIdx = _lines.FindIndex(l =>
            l.Type == IniLineType.Section &&
            string.Equals(l.Section, section, StringComparison.OrdinalIgnoreCase));
        if (sectionIdx < 0) return _lines.Count;

        // Find the next section or end of file
        for (int i = sectionIdx + 1; i < _lines.Count; i++)
        {
            if (_lines[i].Type == IniLineType.Section)
                return i;
        }
        return _lines.Count;
    }

    private enum IniLineType { Other, Section, KeyValue }

    private class IniLine
    {
        public IniLineType Type { get; set; }
        public string? Section { get; set; }
        public string? Key { get; set; }
        public string? Value { get; set; }
        public string Raw { get; set; } = "";
    }
}
