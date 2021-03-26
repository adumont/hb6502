# Breadboard EEPROM Programmer

- [Breadboard EEPROM Programmer](#breadboard-eeprom-programmer)
- [Introduction](#introduction)
- [Schematics](#schematics)
- [How to use](#how-to-use)
  - [Command line interface](#command-line-interface)
- [Credits](#credits)
- [Datasheets](#datasheets)
- [References](#references)

![](../imgs/programmer.jpg)

# Introduction

This subproject is an EEPROM Programmer on a breadboard using:
- a Sparkfun Arduino Pro Micro
- two SN74HC595 Serial-In Parallel-Out (SIPO) Shift Registers, to minimize the pin needed on the Arduino.
- a AT28C256 32K x8 Paged Parallel EEPROM

# Schematics
![](schematics/EEPROM-Programmer.svg)

# How to use

## Command line interface

Use `./eeprom.py` as a command line interface.

### Dump 256 byte pages as text

`./eeprom.py -d/--dump [ADDR [n [offset]]]` will dump `n` pages of 256 bytes starting at address `ADDR` (in hex). If `ADDR` isn't a multiple of 256, it will dump around `ADDR`. If `offset` is specified, the address showned will be offset by `offset` (in hex).

| Command                      | Meaning                                                                   |
| ---------------------------- | ------------------------------------------------------------------------- |
| `./eeprom.py -d`             | Dump page 0000                                                            |
| `./eeprom.py -d ADDR`        | Dump page at address `ADDR` (hex)                                         |
| `./eeprom.py -d ADDR 2`      | Dump 2 pages starting at address `ADDR` (hex)                             |
| `./eeprom.py -d 0000 1 8000` | Dump 1 page starting at address $0000, and show starting address as $8000 |

```
$ ./eeprom.py --dump 0 1 8000
Connected to programmer on port: /dev/ttyACM0

00008000: 58d8 a9ff 8d03 60a9 aa85 00a9 ffa5 008d  X.....`.........
00008010: 0160 4c12 8040 daba 48e8 e8bd 0001 2910  .`L..@..H.....).
00008020: d003 68fa 404c 2580 a900 8508 a980 8509  ..h.@L%.........
00008030: a900 850a a902 850b a2ff a9ff 8510 a000  ................
00008040: e8f0 0db1 0891 0ac8 d0f6 e609 e60b d0f0  ................
00008050: e610 d0ef 6000 0000 0000 0000 0000 0000  ....`...........
[...]
```

### Erase EEPROM

`./eeprom.py --erase [HH]` write HH (hex) to all the EEPROM. It takes about 13s to erase 32KB.

| Command                  | Meaning                         |
| ------------------------ | ------------------------------- |
| `./eeprom.py --erase`    | Fill the EEPROM with $FF        |
| `./eeprom.py --erase HH` | Fill the EEPROM with `HH` (hex) |

### Upload/Flash a binary file to EEPROM

`./eeprom.py -p/--put ADDR FILE` upload and flash the file FILE at address ADDR. It takes about 7s to flash 16KB.

| Command                        | Meaning                                 |
| ------------------------------ | --------------------------------------- |
| `./eeprom.py --put 0 file.bin` | Flash `file.bin` to EEPROM at address 0 |

### Download a binary file from EEPROM

`./eeprom.py -g/--get ADDR LEN FILE` LEN bytes (dec) from EEPROM startig at address ADDR (hex) to file FILE. It takes about 5.5s to download 16KB.

| Command                               | Meaning                                                            |
| ------------------------------------- | ------------------------------------------------------------------ |
| `./eeprom.py --get 02F0 256 file.bin` | Get 256 bytes from EEPROM starting at $02F0 and save to `file.bin` |
| `./eeprom.py --get 0 32768 file.bin`  | Dump the whole EEPROM (32K) and save it to `file.bin`              |

# Credits

Here are some awesome projects from which I have taken inspiration and sometimes also some code:

- [Build an Arduino EEPROM programmer](https://www.youtube.com/watch?v=K88pgWhEb1M) by Ben Eaters
  - [Source code](https://github.com/beneater/eeprom-programmer#arduino-eeprom-programmer)
- [mkeller0815/MEEPROMMER: EEPROM / EPROM programmer based on Arduino hardware](https://github.com/mkeller0815/MEEPROMMER/)
- [TomNisbet/TommyPROM: Simple Arduino-based EEPROM programmer](https://github.com/TomNisbet/TommyPROM)

# Datasheets

- [SNx4HC595 8-Bit Shift Registers With 3-State Output Registers](../datasheets/SN74HC595.pdf)
- [AT28C256 32K x8 Paged Parallel EEPROM](../datasheets/AT28C256.pdf)

# References

- [Pro Micro & Fio V3 Hookup Guide - learn.sparkfun.com](https://learn.s  parkfun.com/tutorials/pro-micro--fio-v3-hookup-guide/hardware-overview-pro-micro)
- [Arduino - ATmega 32U4 Pin Mapping](https://www.arduino.cc/en/Hacking/PinMapping32u4)
