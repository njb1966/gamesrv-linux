using System.Text;

namespace GameSrv.Helpers;

/// <summary>
/// File I/O utility methods replacing RMLib.FileUtils.
/// </summary>
public static class FileHelper
{
    public static void FileWriteAllText(string path, string contents)
    {
        string? dir = Path.GetDirectoryName(path);
        if (!string.IsNullOrEmpty(dir))
            Directory.CreateDirectory(dir);
        File.WriteAllText(path, contents, Encoding.ASCII);
    }

    public static void FileWriteAllText(string path, string contents, Encoding encoding)
    {
        string? dir = Path.GetDirectoryName(path);
        if (!string.IsNullOrEmpty(dir))
            Directory.CreateDirectory(dir);
        File.WriteAllText(path, contents, encoding);
    }

    public static void FileAppendAllText(string path, string contents)
    {
        File.AppendAllText(path, contents);
    }

    public static string[] FileReadAllLines(string path)
    {
        if (!File.Exists(path)) return Array.Empty<string>();
        return File.ReadAllLines(path);
    }

    public static void FileDelete(string path)
    {
        try
        {
            if (File.Exists(path))
                File.Delete(path);
        }
        catch { /* Ignore delete failures */ }
    }

    public static void FileCopy(string source, string dest)
    {
        if (File.Exists(source))
            File.Copy(source, dest, true);
    }

    public static void DirectoryDelete(string path)
    {
        try
        {
            if (Directory.Exists(path))
                Directory.Delete(path, false);
        }
        catch { /* Ignore delete failures */ }
    }
}
