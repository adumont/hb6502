#!/usr/bin/env python3

import sys
import argparse
import time
import threading
from queue import Queue
import signal

from py65.devices.mpu65c02 import MPU as CMOS65C02
from py65.memory import ObservableMemory
from py65.utils import console

# Argument parsing
parser = argparse.ArgumentParser()
parser.add_argument('-r','--rom', help='binary rom file', default="forth.bin")
parser.add_argument('-a','--addr', help='address to load to', default=0x8000)
parser.add_argument('-l','--load', help='forth program to load')
args = parser.parse_args()

getc_addr=0xF004
putc_addr=0xF001

queue = Queue()

def signal_handler(signum, frame):
    exit()

signal.signal(signal.SIGINT, signal_handler)

def cpuThread(ch, queue):
    started = False

    def load(memory, start_address, bytes):
        memory[start_address:start_address + len(bytes)] = bytes

    def putc(address, value):
        if not started:
            return
        try:
            if value==0x08:
                sys.stdout.write(chr(value))
                sys.stdout.write(' ')
            sys.stdout.write(chr(value))
        except UnicodeEncodeError: # Python 3
            sys.stdout.write("?")
        sys.stdout.flush()

    def getc(address):
        if queue.empty():
            return 0
        else:
            c=queue.get()
            return c

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

    mpu.pc=getWord(mpu.RESET)

    started = True

    while True:
        mpu.step()

t=threading.Thread( target=cpuThread, args=("", queue))
t.daemon = True
t.start()

if args.load:
    f = open(args.load, 'r')
    program = f.read()
    f.close()

    for c in program:
        queue.put( ord(c) )

while True:
    char = console.getch(sys.stdin)
    if char:
        queue.put( ord(char) )
