using GameSrv.Enums;

namespace GameSrv.Server;

public class NodeEventArgs : EventArgs
{
    public NodeInfo NodeInfo { get; }
    public string Status { get; }
    public NodeEventType EventType { get; }

    public NodeEventArgs(NodeInfo nodeInfo, string status, NodeEventType eventType)
    {
        NodeInfo = nodeInfo;
        Status = status;
        EventType = eventType;
    }
}

public class WhoIsOnlineEventArgs : EventArgs
{
    public System.Collections.Specialized.OrderedDictionary WhoIsOnline { get; } = new();
}
