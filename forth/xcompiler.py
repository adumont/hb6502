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
parser.add_argument('-f','--dfo', help='disable FIND offloading', default=False, action='store_true')
parser.add_argument('-s','--symbols', help='symbols file', default="forth.lbl")
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

# CPU_EXIT_SIGNALS
CPU_EXIT_OK = 0
CPU_EXIT_ERROR = 1

symbols = None
last_lookedup_word = ""

def parseSymbolsFile(filename):
    # file content should look like this:
    # al 00C23D .do_PUSH1
    # al 00C239 .__word_12
    # al 00C239 .h_PUSH1
    # al 00C22E .__word_11
    symbols = {}
    with open(filename) as file:
        for line in file:
            _, a, s = line.split(" ")
            if s.startswith(".__word_") or s.startswith(".h_"):
                # we discard those symbols
                continue
            symbols[int(a,16)]=s.strip()[1:]
    return symbols

def getSymbol(addr):
    return symbols[max(k for k in symbols if k <= addr)]

def getLabelAddr(label):
    return [ k for k in symbols if symbols[k] == label ][0]

if args.symbols:
    symbols=parseSymbolsFile(args.symbols)

# Address of symbols
addr_NEXT = getLabelAddr("NEXT")
addr_IP = 0xFE-2
addr_LATEST = getLabelAddr("LATEST")
addr_do_FIND = getLabelAddr("do_FIND")
addr_do_0BR = getLabelAddr("do_0BR")
addr_numberError = getLabelAddr("numberError")

def cpuThread(ch, queue, emu_queue):
    started = False

    def load(memory, start_address, bytes):
        memory[start_address:start_address + len(bytes)] = bytes

    def getc(address):
        while queue.empty():
            pass
        c = queue.get()
        print(chr(c), end="")
        return c

    def getByte(address):
        return mpu.memory[address]

    def getWord(address):
        return mpu.memory[address] + 256*mpu.memory[address+1]

    def getCountedStr(addr,l=None):
        if l==None:
            l = getByte(addr) # length byte
        ba = mpu.memory[addr+1:addr+l+1]
        return "".join(map(chr,ba))

    def do_0BR():
        if getWord( 2 + mpu.x ) != 0x0000:
            return # not taking the branch, do nothing here
        addr_jump_to = getWord( getWord( addr_IP ) )
        if addr_jump_to == addr_numberError:
            print("FATAL: Word not found:",last_lookedup_word)
            print("COMPILATION ABORTED")
            emu_queue.put(CPU_EXIT_ERROR)
            quit(1)

    def do_FIND():
        # Simulates FIND in python and bypasses our FORTH's FIND.
        # Saves a lot of cycles
        global last_lookedup_word

        addr = getWord( 4 + mpu.x )
        l = getWord( 2 + mpu.x )
        word = "".join(map(chr, mpu.memory[addr:addr+l] ))

        last_lookedup_word = word

        if args.dfo:
            # user forced Disable FIND Offloading
            return

        header = getWord(addr_LATEST)
        while True:
            l = getByte(header+2) # get length & flags

            if l & 0x40: # Hidden word, skip!
                header = getWord(header)
                continue

            l = l &  0x1F # length, no flags
            str = getCountedStr(header+2, l)

            if str == word:
                # put  header addr in 5,X, 4,X (NOS)
                mpu.memory[4+mpu.x] = header & 0xFF
                mpu.memory[5+mpu.x] = header >> 8
                mpu.x = mpu.x + 2   # DROP
                mpu.pc = addr_NEXT  #  JMP NEXT
                break

            header = getWord(header)

            if header == 0:
                mpu.memory[4+mpu.x] = 0
                mpu.memory[5+mpu.x] = 0
                mpu.x = mpu.x + 2
                mpu.pc = addr_NEXT
                break

    mpu = CMOS65C02()
    mpu.memory = 0x10000 * [0xEA]

    addrWidth = mpu.ADDR_WIDTH

    m = ObservableMemory(subject=mpu.memory, addrWidth=addrWidth)
    # m.subscribe_to_write([putc_addr], putc)
    m.subscribe_to_read([getc_addr], getc)
    mpu.memory = m

    if args.addr and str(args.addr).startswith("0x"):
        args.addr = int(args.addr,16)

    f = open(args.rom, 'rb')
    program = f.read()
    f.close()

    load(mpu.memory, args.addr, program)

    mpu.pc=getWord(mpu.RESET)

    started = True

    while True:
        if mpu.pc == addr_do_FIND:
            # fast FIND offloaded in python
            do_FIND()
        elif mpu.pc == addr_do_0BR:
            # check if we've reached the numberError condition
            # means word not found in interpreter --> abort compilation
            do_0BR()

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

    HERE = getWord(0x0007)
    print("HERE: %04X" % HERE )

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

    print(mpu.processorCycles, "clock cycles")

    # Signal main thread it's the end
    emu_queue.put(CPU_EXIT_OK)

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
while emu_queue.empty():
    pass

print("CPU signaled end of compilation!")
quit(emu_queue.get())
