using GameSrv.Session;

namespace GameSrv.Doors;

/// <summary>
/// Interface for door execution strategies.
/// </summary>
public interface IDoorRunner
{
    /// <summary>
    /// Run the door with the given command and parameters.
    /// </summary>
    void Run(ClientThread clientThread, string command, string parameters);
}
