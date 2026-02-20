using System.Collections.Specialized;
using System.Reflection;

namespace GameSrv.Config;

/// <summary>
/// Base class for INI-backed configuration objects.
/// Replaces RMLib.ConfigHelper with reflection-based property mapping.
/// </summary>
public abstract class ConfigHelper
{
    protected string _filePath;
    private string _sectionName = "CONFIGURATION";

    public bool Loaded { get; private set; }
    public string FileName => _filePath;
    public string SectionName => _sectionName;

    protected ConfigHelper(string relativePath)
    {
        _filePath = Path.Combine(GameSrv.Helpers.StringHelper.StartupPath, relativePath);
    }

    protected ConfigHelper(string basePath, string relativePath)
    {
        _filePath = Path.Combine(basePath, relativePath);
    }

    /// <summary>
    /// Load properties from the INI file section.
    /// </summary>
    protected bool Load(string sectionName = "CONFIGURATION")
    {
        _sectionName = sectionName;

        if (!File.Exists(_filePath))
        {
            Loaded = false;
            return false;
        }

        using var ini = new IniFile(_filePath);
        if (!ini.SectionExists(sectionName))
        {
            Loaded = false;
            return false;
        }

        var properties = GetType().GetProperties(BindingFlags.Public | BindingFlags.Instance);
        var iniKeys = ini.ReadKeys(sectionName);
        var matchedKeys = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

        foreach (var prop in properties)
        {
            if (!prop.CanWrite) continue;
            if (prop.Name == nameof(Loaded) || prop.Name == nameof(FileName) || prop.Name == nameof(SectionName)) continue;

            // Special handling for StringDictionary (AdditionalInfo)
            if (prop.PropertyType == typeof(StringDictionary))
                continue;

            string key = prop.Name;
            string? rawValue = ini.ReadString(sectionName, key, null!);
            if (rawValue == null) continue;

            matchedKeys.Add(key);

            try
            {
                if (prop.PropertyType == typeof(string))
                {
                    prop.SetValue(this, rawValue);
                }
                else if (prop.PropertyType == typeof(int))
                {
                    if (int.TryParse(rawValue, out int intVal))
                        prop.SetValue(this, intVal);
                }
                else if (prop.PropertyType == typeof(bool))
                {
                    string lower = rawValue.ToLower();
                    if (lower == "true" || lower == "1" || lower == "yes")
                        prop.SetValue(this, true);
                    else if (lower == "false" || lower == "0" || lower == "no")
                        prop.SetValue(this, false);
                }
                else if (prop.PropertyType.IsEnum)
                {
                    if (Enum.TryParse(prop.PropertyType, rawValue, true, out object? enumVal))
                        prop.SetValue(this, enumVal);
                }
            }
            catch
            {
                // Skip properties that fail to parse
            }
        }

        // Load unmatched keys into StringDictionary property (AdditionalInfo)
        var dictProp = properties.FirstOrDefault(p => p.PropertyType == typeof(StringDictionary));
        if (dictProp != null)
        {
            var dict = (StringDictionary?)dictProp.GetValue(this) ?? new StringDictionary();
            foreach (string iniKey in iniKeys)
            {
                if (!matchedKeys.Contains(iniKey))
                {
                    string val = ini.ReadString(sectionName, iniKey, "");
                    dict[iniKey] = val;
                }
            }
            dictProp.SetValue(this, dict);
        }

        Loaded = true;
        return true;
    }

    /// <summary>
    /// Save properties back to the INI file section.
    /// </summary>
    public virtual void Save()
    {
        var ini = new IniFile(_filePath);

        var properties = GetType().GetProperties(BindingFlags.Public | BindingFlags.Instance);
        foreach (var prop in properties)
        {
            if (!prop.CanRead) continue;
            if (prop.Name == nameof(Loaded) || prop.Name == nameof(FileName) || prop.Name == nameof(SectionName)) continue;

            if (prop.PropertyType == typeof(StringDictionary))
            {
                var dict = (StringDictionary?)prop.GetValue(this);
                if (dict != null)
                {
                    foreach (string? key in dict.Keys)
                    {
                        if (key != null)
                            ini.WriteString(_sectionName, key, dict[key] ?? "");
                    }
                }
                continue;
            }

            object? value = prop.GetValue(this);
            if (value == null) continue;

            if (prop.PropertyType == typeof(bool))
                ini.WriteString(_sectionName, prop.Name, (bool)value ? "True" : "False");
            else
                ini.WriteString(_sectionName, prop.Name, value.ToString() ?? "");
        }

        ini.Save();
        ini.Dispose();
    }
}
