#!/usr/bin/env python3

import sys
import argparse

from py65.devices.mpu65c02 import MPU as CMOS65C02
from py65.memory import ObservableMemory
from py65.utils import console

from PyQt5.QtWidgets import QWidget,QApplication,QLabel,QVBoxLayout
from PyQt5.QtCore import QTimer

# Argument parsing
parser = argparse.ArgumentParser()
parser.add_argument('-r','--rom', help='binary rom file', default="forth.bin")
parser.add_argument('-a','--addr', help='address to load to', default=0x8000)
args = parser.parse_args()

getc_addr=0xF004
putc_addr=0xF001

addrW=0xFE
addrIP=addrW-2
addrG2=addrIP-2
addrG1=addrG2-2

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

def getCountedStr(addr,l=None):
    if l==None:
      l = getByte(addr) # length byte
    ba = mpu.memory[addr+1:addr+l+1]
    return "".join(map(chr,ba))

def getWordName(addr):
    l = getByte(addr+2) # length byte with flags
    imm = l & 0x80      # immediate flag
    l = l & 0x1F        # real length (we mask off the 3 MSB (flags))
    return getCountedStr(addr,l)

# when boot gets to 0:
# - scanForthDict --> update dictionary (H,NAME,Flags,CFA)
# - call scanForthDict when LATEST changes
# - function to find the NAME based on the CFA (usara el d)
# - 

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

forthDict = []
boot = 1
last_ip = 0
last_w = 0
last_latest = 0

def updateDict():
    d = []
    next = 0
    l = 0
    imm = 0
    header = getWord(0x0200) # LATEST
    while True:
        next = getWord(header)
        l = getByte(header+2) # length byte with flags
        imm = l & 0x80        # immediate flag
        l = l & 0x1F          # real length (we mask off the 3 MSB (flags))
        name = getCountedStr(header+2,l)
        w = {'header':header, 'cfa':(header+3+l), 'name':name, 'imm':imm }
        d.append( w )
        # next word:
        if next==0:
            break
        header = next
    return d

def printDict(d):
    for w in d:
        print("$%04X $%04X %s $%02X" % (w['header'], w['cfa'], w['name'], w['imm']))

def wordFromCFA(cfa):
    l = [ i for i in forthDict if i['cfa'] == cfa ]
    if len(l)>0:
        return l[0]
    else:
        return None

def nameFromWord(w):
    # w is a python dict. returned by wordFromCFA
    if w == None:
        return ""
    
    return w['name']
        

class WinForm(QWidget):
    def __init__(self,parent=None):
        super(WinForm, self).__init__(parent)
        self.setWindowTitle('Alex FORTH Debugger')

        layout=QVBoxLayout()

        self.label1=QLabel()
        self.label2=QLabel()
        self.label3=QLabel()
        self.label4=QLabel()
        self.label5=QLabel()
        self.label6=QLabel()
        self.label7=QLabel()
        self.label8=QLabel()

        layout.addWidget(self.label5)
        layout.addWidget(self.label6)
        layout.addWidget(self.label7)
        layout.addWidget(self.label8)
        layout.addWidget(self.label1)
        layout.addWidget(self.label2)
        layout.addWidget(self.label3)
        layout.addWidget(self.label4)

        self.timer0=QTimer()
        self.timer0.timeout.connect(self.mystep)

        self.timerUI=QTimer()
        self.timerUI.timeout.connect(self.updateUI)

        self.setLayout(layout)

        self.timer0.start(0)
        self.timerUI.start(200)

    def updateUI(self):
        self.label1.setText( "LATEST: $%04X" % getWord( 0x0200 ) )
        self.label2.setText( " MODE : $%02X" % getByte( 0x0200+2 ) )
        self.label3.setText( " BOOT : $%02X" % getByte( 0x0200+3 ) )
        self.label4.setText( " HERE : $%04X" % getWord( 0x0200+89 ) )
        w = getWord( addrW )
        name = nameFromWord(wordFromCFA( w ))
        self.label5.setText( "    W : $%04X %s" % (w, name ) )
        ip = getWord( addrIP )
        name = nameFromWord(wordFromCFA( getWord(ip) ))
        self.label6.setText( "   IP : $%04X %s" % (ip, name ) )
        self.label7.setText( "   G2 : $%04X" % getWord( addrG2 ) )
        self.label8.setText( "   G1 : $%04X" % getWord( addrG1 ) )
 
    def mystep(self):
        global boot, forthDict, last_ip, last_w, last_latest

        mpu.step()
        
        ud = False

        if boot == 0: 
            latest = getWord( 0x0200 )
            if last_latest != latest:
                last_latest = latest
                self.label1.setText( "LATEST: $%04X" % latest )
                ud = True

            # ip = getWord( addrIP )
            # if last_ip != ip:
            #     last_ip = ip
            #     name = name = nameFromWord(wordFromCFA( getWord(ip) ))
            #     self.label6.setText( "   IP : $%04X %s" % (ip, name ) )

            # w = getWord( addrW )
            # if last_w != w:
            #     last_w = w
            #     name = nameFromWord(wordFromCFA( w ))
            #     self.label5.setText( "    W : $%04X %s" % (w, name ) )

        if boot == 0 and last_latest != latest:
            last_latest = latest
            ud = True


        if boot==1 and getByte( 0x0200+3 ) == 0x00:
            boot = 0
            ud = True

        if ud:
            forthDict = updateDict()
            # printDict(forthDict)


if __name__ == '__main__':
    app=QApplication(sys.argv)
    form=WinForm()
    form.show()
    sys.exit(app.exec_())
