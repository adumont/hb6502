#!/usr/bin/env python3

import argparse
import serial
import contextlib
import time
import os.path
import logging
from time import sleep

from glob import glob

port = glob("/dev/ttyACM*")

assert( len(port) > 0 )

# logging.basicConfig(format='[%(name)s.%(funcName)s:%(lineno)d] %(levelname)s %(message)s', level=level)

# def log(className):
#   return logging.getLogger(className)

parser = argparse.ArgumentParser(
        description='EEPROM Programmer CLI',
        epilog='@adumont')

task = parser.add_mutually_exclusive_group(required=True)
task.add_argument('-p', '--put', dest="cmd", action="store_const", const="put", help='Write a file to EEPROM (binary)')
task.add_argument('-d', '--dump', dest="cmd", action="store_const", const="dump", help='Dump from EEPROM (ascii)')
task.add_argument('-g', '--get', dest="cmd", action="store_const", const="get", help='Read from EEPROM (binary)')
task.add_argument('--erase', dest="cmd", action="store_const", const="erase", help='Erase EEPROM')
# task.add_argument('-e', '--erase', dest="cmd", action="store_const", const="erase", help='Erase EEPROM')

parser.add_argument("args",nargs="*")

args = parser.parse_args()

ser = serial.Serial(
    port=port[0],
    baudrate=115200,
    parity=serial.PARITY_NONE,
    stopbits=serial.STOPBITS_ONE,
    bytesize=serial.EIGHTBITS,
    timeout=0
)

print("Connected to programmer on port: " + ser.portstr)

def wait_for_prompt(show=True, timeout=0):
  prompt = False
  t0=time.time()
  while not prompt:
    t1=time.time()
    if timeout !=0 and 1000*(t1-t0) > timeout:
      break
    for c in ser.read():
      if c == ord(">"):
        prompt = True
        break
      if show:
        print("%c" % c, end='', flush=True)
    

def main():

  wait_for_prompt(show=False, timeout=200)

  print(vars(args), len(args.args))

  a0 = "" if len(args.args) == 0 else args.args[0]
  a1 = "" if len(args.args) <= 1 else args.args[1]
  a2 = "" if len(args.args) <= 2 else args.args[2]

  if args.cmd == "dump":
    print("d %s %s\r" % ( a0, a1))
    ser.write( str.encode("d %s %s\r" % ( a0, a1) ) )
  elif args.cmd == "erase":
    print("erase %s\r" % a0)
    ser.write( str.encode("erase %s\r" % a0 ) )
  elif args.cmd == "put":
    l = os.stat(a1).st_size
    
    print("put %s %s\r" % ( a0, l))
    ser.write( str.encode("put %s %s\r" % ( a0, l) ) )

    with open(a1, "rb") as f:
      count = 0
      while True:
        c=f.read(64)
        if not c:
          break
        ser.write(c)
        count = count + len(c)
        # print(" %3.00g %%" % (100.0*count/l), end="\r")
        print("  %3.2f %%" % (100.0*count/l), end="\r")
    print()

  elif args.cmd == "get":
    print("get %s %s\r" % ( a0, a1))
    ser.write( str.encode("get %s %s\r" % ( a0, a1)) )
    l=int(a1)
    with open(a2, "wb") as f:
      count = 0
      while True:
        c=ser.read(1)
        if not c:
          continue
        f.write(c)
        count = count+1
        if(count%256) or count == l:
          print("  %3.2f %%" % (100.0*count/l), end="\r")
        if count == l:
          break
    print()

  wait_for_prompt() 
  ser.close()

if __name__== "__main__":
  main()