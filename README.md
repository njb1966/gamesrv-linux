GameSrv-Linux: BBS Door Game Server
====================================

A Linux-first rewrite of [GameSrv](https://github.com/rickparrish/GameSrv) by Rick Parrish, rebuilt from the ground up as a .NET 8 application targeting Debian 12 and other modern Linux distributions.

The original GameSrv was a Windows-based C# BBS door game server. This version preserves the spirit of that project — serving classic DOS door games over telnet — but takes an entirely different architectural approach to make it work natively on Linux.

## What's Different

- **Complete .NET 8 rewrite** — no dependency on RMLib or RMLibUI
- **Linux-first** — designed for headless server deployment on Debian 12
- **QEMU + FreeDOS emulation** — runs 16-bit DOS door games inside lightweight QEMU virtual machines with FreeDOS, using BNU FOSSIL for serial I/O
- **Multi-protocol support** — Telnet (port 2323), RLogin (port 5513), and WebSocket (port 1123)
- **Per-node isolation** — each player gets their own QEMU instance with independent serial port bridging
- **Sentinel-based exit detection** — instant clean exit when door games finish, with idle timeout as safety fallback
- **INI-based door configuration** — simple per-game config files for adding new doors
- **WWIV BBS integration** — RLogin bridge script for chaining from WWIV BBS instances

## Working Door Games

- **Legend of the Red Dragon (LORD)** — the classic BBS RPG
- **TradeWars 2002** — interstellar trading and empire-building
- **Barren Realms Elite (BRE)** — multiplayer realm management strategy

## Building

```bash
dotnet build src/GameSrv/GameSrv.csproj
```

## Running

```bash
dotnet run --project src/GameSrv/
```

Requires QEMU (`qemu-system-i386`), mtools, and a FreeDOS boot image.

## Original Project

This project is based on [GameSrv](https://github.com/rickparrish/GameSrv) by **Rick Parrish**. The original C# Windows version (and the even older Delphi version at [SourceForge](http://www.sf.net/projects/rm-gamesrv)) laid the groundwork for what BBS door serving could look like. This Linux port wouldn't exist without that foundation.

The original GameSrv website: http://www.gamesrv.ca/

## License

GameSrv is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
any later version.

GameSrv is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GameSrv. If not, see <http://www.gnu.org/licenses/>.

## Acknowledgements

- **Rick Parrish** — original GameSrv author
- dosxtrn.exe, sbbsexec.dll, sbbsexec.vxd and code ported from Synchronet's xtrn.cpp are copyright Rob Swindell - http://www.synchro.net/copyright.html
