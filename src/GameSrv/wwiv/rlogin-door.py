#!/usr/bin/env python3
"""
rlogin-door.py - Connects WWIV to GameSrv via RLogin protocol (STDIO mode)

Usage: rlogin-door.py <host> <port> <username> [doorname]

If doorname is omitted, user gets the GameSrv menu.
If doorname is specified, user goes directly to that door.

WWIV chain config examples:
  Menu:  python3 /opt/wwiv/rlogin-door.py 157.230.152.152 5513 %U
  LORD:  python3 /opt/wwiv/rlogin-door.py 157.230.152.152 5513 %U LORD
  Exec Mode: STDIO
"""
import sys
import socket
import select
import os
import time

LOGFILE = "/tmp/rlogin-door.log"

def log(msg):
    with open(LOGFILE, "a") as f:
        f.write(f"{time.strftime('%H:%M:%S')} {msg}\n")

def main():
    if len(sys.argv) < 4:
        sys.stderr.write(f"Usage: {sys.argv[0]} <host> <port> <username> [doorname]\n")
        sys.exit(1)

    host = sys.argv[1]
    port = int(sys.argv[2])
    username = sys.argv[3]
    doorname = sys.argv[4] if len(sys.argv) > 4 else None

    log(f"Connecting to {host}:{port} user={username} door={doorname}")

    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
    sock.settimeout(10)
    try:
        sock.connect((host, port))
    except Exception as e:
        log(f"Connection failed: {e}")
        sys.exit(1)

    # Send RLogin header: \0<password>\0<username>\0<termtype>/38400\0
    if doorname:
        termtype = f'xtrn={doorname}/38400'
    else:
        termtype = 'ansi/38400'
    header = (b'\x00' + username.encode() + b'\x00' +
              username.encode() + b'\x00' +
              termtype.encode() + b'\x00')
    sock.sendall(header)
    log("Header sent, waiting for ack")

    # Wait for server acknowledgment (single null byte)
    try:
        sock.settimeout(10)
        ack = sock.recv(1)
        if ack != b'\x00':
            log(f"Bad ack: {ack!r}")
            sys.exit(1)
    except Exception as e:
        log(f"No ack: {e}")
        sys.exit(1)

    log("Ack received, bridging I/O")

    # Set up raw binary I/O on stdin/stdout
    stdin_fd = sys.stdin.fileno()
    stdout_fd = sys.stdout.fileno()
    os.set_blocking(stdin_fd, False)
    sock.setblocking(False)

    total_from_server = 0
    total_from_client = 0

    try:
        while True:
            try:
                readable, _, _ = select.select(
                    [sock, stdin_fd], [], [], 1.0)
            except (ValueError, OSError) as e:
                log(f"Select error: {e}")
                break

            for r in readable:
                if r is sock:
                    try:
                        data = sock.recv(8192)
                        if not data:
                            log(f"Socket EOF. Total from server: {total_from_server}, from client: {total_from_client}")
                            return
                        total_from_server += len(data)
                        os.write(stdout_fd, data)
                    except BlockingIOError:
                        pass
                    except (ConnectionError, OSError) as e:
                        log(f"Socket read error: {e}. Total from server: {total_from_server}")
                        return

                elif r == stdin_fd:
                    try:
                        data = os.read(stdin_fd, 8192)
                        if not data:
                            log(f"Stdin EOF. Total from server: {total_from_server}, from client: {total_from_client}")
                            return
                        # WWIV STDIO sends \n, DOS doors expect \r
                        data = data.replace(b'\n', b'\r')
                        total_from_client += len(data)
                        log(f"Stdin data: {len(data)} bytes: {data!r}")
                        sock.setblocking(True)
                        sock.sendall(data)
                        sock.setblocking(False)
                    except BlockingIOError:
                        pass
                    except (ConnectionError, OSError) as e:
                        log(f"Stdin/send error: {e}. Total from client: {total_from_client}")
                        return
    except KeyboardInterrupt:
        log("KeyboardInterrupt")
    except Exception as e:
        log(f"Unexpected error: {e}")
    finally:
        log(f"Exiting. Total from server: {total_from_server}, from client: {total_from_client}")
        try:
            sock.close()
        except:
            pass

if __name__ == '__main__':
    main()
