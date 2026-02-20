#!/bin/bash
#
# Launch a standalone QEMU FreeDOS VM with curses display for configuring door games.
#
# Usage: ./launch-qemu.sh <game-image>
#
# Example:
#   ./launch-qemu.sh ../doors/lord/lord.img
#
# This gives you a DOS prompt where you can run LORDCFG.EXE, etc.
# Press ESC then 2 to switch between QEMU monitor and VGA in curses mode.
# Press Ctrl+A then X to quit QEMU.
#

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
FREEDOS_IMG="$SCRIPT_DIR/freedos-boot.img"

if [ $# -lt 1 ]; then
    echo "Usage: $0 <game-image>"
    echo ""
    echo "  game-image   Path to the door game's FAT image (e.g. doors/lord/lord.img)"
    echo ""
    echo "Examples:"
    echo "  $0 ../doors/lord/lord.img"
    echo "  $0 ../doors/tw2002/tw2002.img"
    exit 1
fi

GAME_IMG="$1"

if [ ! -f "$FREEDOS_IMG" ]; then
    echo "ERROR: FreeDOS boot image not found: $FREEDOS_IMG"
    exit 1
fi

if [ ! -f "$GAME_IMG" ]; then
    echo "ERROR: Game image not found: $GAME_IMG"
    exit 1
fi

echo "=== Launching QEMU FreeDOS VM ==="
echo "  FreeDOS: $FREEDOS_IMG"
echo "  Game:    $GAME_IMG"
echo ""
echo "  A: = FreeDOS boot floppy"
echo "  C: = Game files ($GAME_IMG)"
echo ""
echo "  To quit: Ctrl+A then X"
echo "  The game image is writable (changes will be saved)."
echo ""

echo "  SHARE.COM and BNU.COM available on A:"
echo "  Type: A:\\SHARE.COM to load file locking"
echo ""

qemu-system-i386 \
    -machine pc \
    -m 16 \
    -boot a \
    -fda "$FREEDOS_IMG" \
    -drive "file=$GAME_IMG,format=raw,if=ide,index=0" \
    -display curses \
    -monitor none \
    -no-reboot
