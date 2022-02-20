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
emu_queue = Queue() # CPU --> Emulator

def signal_handler(signum, frame):
    exit()

signal.signal(signal.SIGINT, signal_handler)

def cpuThread(ch, queue, emu_queue):
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
        if getByte(0x0000) == 0x01:
            # Means the end of compilation, exit the CPU loop
            break

        mpu.step()

    print("Reached end of compilation! Starting the dumping" )

    print("ROM Dictionary:")
    rom_start = getWord(0x0001)
    rom_end   = getWord(0x0003)
    print("  Starts: %04X" % rom_start )
    print("  End   : %04X" % rom_end   )

    f = open("rom.dat", 'wb')
    f.write(bytearray(mpu.memory[rom_start:rom_end]))
    f.close()

    print("RAM Dictionary:")
    ram_start = getWord(0x0005)
    ram_end   = getWord(0x0007)
    print("  Starts: %04X" % ram_start )
    print("  End   : %04X" % ram_end   )

    f = open("ram.dat", 'wb')
    f.write(bytearray(mpu.memory[ram_start:ram_end]))
    f.close()

    LAST = getWord(0x0009)
    print("LAST: %04X" % LAST )

    # f = open("last.dat", 'wb')
    # f.write(bytearray(mpu.memory[0x0009:0x0009+2]))
    # f.close()
    f = open("last.dat", 'w')
    f.write( "; LATEST     \n")
    f.write( "LDA #$%02X   \n" % getByte(0x0009+0) )
    f.write( "STA LATEST   \n" )
    f.write( "LDA #$%02X   \n" % getByte(0x0009+1) )
    f.write( "STA LATEST+1 \n" )

    f.write( "; HERE       \n")
    f.write( "LDA #$%02X   \n" % getByte(0x0007+0) ) # ram_end = RAM's HERE
    f.write( "STA DP       \n" )
    f.write( "LDA #$%02X   \n" % getByte(0x0007+1) )
    f.write( "STA DP+1     \n" )
    f.close()

    # Signal main thread it's the end
    emu_queue.put(1)

# We start the "computer"
t=threading.Thread( target=cpuThread, args=("", queue, emu_queue))
t.daemon = True
t.start()

# We feed the code to the "keyboard" as if the user was typing it
# it's queued into a FIFO so no worries of FORTH taking it's time to
# interpret and compiling
if args.load:
    f = open(args.load, 'r')
    program = f.read()
    f.close()

    for c in program:
        queue.put( ord(c) )

# Now we wait FORTH to signal us it has finished the compilation.
# This happens when we save "1 into 0x0000"
# we need to catch writes to 0x0000 and when that happens, signal back to this thread.

# Once that is done, we need to extract the generated piece of dictionary, and save it as ".BYTE" code
# So we can include it when build our code in stage 2.

    # for that we need to know from where (start) to where (end)
    # the computer has saved that for us at 0x0001 (2 bytes)

while True:
    if not emu_queue.empty():
        print("CPU signaled end of compilation!")
        break
