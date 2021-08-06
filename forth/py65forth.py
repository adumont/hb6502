#!/usr/bin/env python3

import sys
import argparse

from py65.devices.mpu65c02 import MPU as CMOS65C02
from py65.memory import ObservableMemory
from py65.utils import console

# Argument parsing
parser = argparse.ArgumentParser()
parser.add_argument('-r','--rom', help='binary rom file', default="forth.bin")
parser.add_argument('-a','--addr', help='address to load to', default=0x8000)
args = parser.parse_args()

getc_addr=0xF004
putc_addr=0xF001

def load(memory, start_address, bytes):
    memory[start_address:start_address + len(bytes)] = bytes

def putc(address, value):
    try:
        sys.stdout.write(chr(value))
    except UnicodeEncodeError: # Python 3
        sys.stdout.write("?")
    sys.stdout.flush()

def getc(address):
    char = console.getch_noblock(sys.stdin)
    if char:
        byte = ord(char)
    else:
        byte = 0
    return byte

def getByte(address):
    return mpu.memory[address]

def getWord(address):
    return mpu.memory[address] + 256*mpu.memory[address+1]

mpu = CMOS65C02()
mpu.memory = 0x10000 * [0xEA]

addrWidth = mpu.ADDR_WIDTH
# byteWidth = mpu.BYTE_WIDTH
# addrFmt = mpu.ADDR_FORMAT
# byteFmt = mpu.BYTE_FORMAT
# addrMask = mpu.addrMask
# byteMask = mpu.byteMask

m = ObservableMemory(subject=mpu.memory, addrWidth=addrWidth)
m.subscribe_to_write([putc_addr], putc)
m.subscribe_to_read([getc_addr], getc)
mpu.memory = m

if args.addr and str(args.addr).startswith("0x"):
    args.addr = int(args.addr,16)

if args.rom:
    print("Loading %s at $%04X" % ( args.rom, args.addr ) )
    f = open(args.rom, 'rb')
    program = f.read()
    f.close()
else:
    # Dummy prog
    program = [ 0xA9, 97, 0x8D, 0x01, 0xF0 ]

load(mpu.memory, args.addr, program)

print(args)

# print( "RESET:", "$%04X" % (mpu.RESET))

# print( "RESET:", "$%04X" % getWord(mpu.RESET))
# print( "IRQ  :", "$%04X" % getWord(mpu.IRQ))
# print( "NMI  :", "$%04X" % getWord(mpu.NMI))

# Reset: RESET vector => PC
mpu.pc=getWord(mpu.RESET)

while True:
  try:
    while True:
        mpu.step()
  except KeyboardInterrupt:
    print(mpu)