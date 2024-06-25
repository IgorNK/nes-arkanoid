# Arkanoid for NES

Training project to learn 6502 assembly for NES.

## Build

```
ca65 main.asm -o main.o --debug-info
ld65 main.o -o arkanoid.nes -t nes -dbgfile arkanoid.dbg
```