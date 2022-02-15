#!/usr/bin/env python3

import sys
import argparse
import time
import threading
from queue import Queue, Empty
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

class ClearableQueue(Queue):

    def clear(self):
        try:
            while True:
                self.get_nowait()
        except Empty:
            pass

queue = ClearableQueue()
int_queue = Queue()

def signal_handler(signum, frame):
    exit()

signal.signal(signal.SIGINT, signal_handler)

def cpuThread(ch, queue, int_queue):
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

    def nmi():
        # triggers a NMI IRQ in the processor
        # this is very similar to the BRK instruction
        mpu.stPushWord(mpu.pc)
        mpu.p &= ~mpu.BREAK
        mpu.stPush(mpu.p | mpu.UNUSED)
        mpu.p |= mpu.INTERRUPT
        mpu.pc = mpu.WordAt(mpu.NMI)
        mpu.processorCycles += 7

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
        # print("Loading %s at $%04X" % ( args.rom, args.addr ) )
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
        if not int_queue.empty():
            _ = int_queue.get()

            # we remove any input keys in the queue
            queue.clear()
            # and trigger the NMI to reinitialize FORTH
            nmi()

        mpu.step()

t=threading.Thread( target=cpuThread, args=("", queue, int_queue))
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
        if ord(char) == 0x1b:    # ESC
            int_queue.put(0)     # Send an NMI signal to the thread
        else:
            queue.put( ord(char) )
