#!/usr/bin/env bash
set -e

FILE="$1"

if [ -z "$FILE" ]; then
    echo "usage: $0 <file.ru|file.c|file.asm>"
    exit 1
fi

EXT="${FILE##*.}"    # ru, c, asm
BASE="${FILE%.*}"     # filename without extension

case "$EXT" in

    ru)
        ASM="${BASE}.s"
        OBJ="${BASE}.o"
        BIN="${BASE}.ru.out"

        ./rune build "$FILE" -o "$ASM"
        nasm -f elf64 -g -F dwarf "$ASM" -o "$OBJ"
        gcc -no-pie "$OBJ" -o "$BIN" -g
        ;;

    c)
        ASM="${BASE}.s"
        OBJ="${BASE}.o"
        BIN="${BASE}.c.out"

        gcc -S -fverbose-asm -masm=intel -g "$FILE" -o "$ASM"
        gcc -c -g "$ASM" -o "$OBJ"
        gcc -no-pie -g "$OBJ" -o "$BIN"
        ;;

    s)
        OBJ="${BASE}.o"
        BIN="${BASE}.s.out"

        nasm -f elf64 -g -F dwarf "$FILE" -o "$OBJ"
        gcc -no-pie "$OBJ" -o "$BIN" -g
        ;;

    *)
        echo "unsupported extension: $EXT"
        exit 1
        ;;
esac

echo "built: $BIN"

