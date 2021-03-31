#!/usr/bin/env python3

import argparse
import serial
import contextlib
import time
import os.path
import logging
from time import sleep
import math

from glob import glob

def flash(args):
  l = os.stat(args.file).st_size

  if args.len:
    l = min(l, math.ceil(args.len/64)*64)
  
  print("put %s %s\r" % ( args.addr, l))
  ser.write( str.encode("put %s %s\r" % ( args.addr, l) ) )

  with open(args.file, "rb") as f:
    count = 0
    while True:
      c=f.read(64)
      if not c:
        break
      ser.write(c)
      count = count + len(c)
      print("  %3.2f %%" % (100.0*count/l), end="\r")
      if count >= l: break
  print()

def save(args):
  print("get %s %d\r" % ( args.addr, args.len))
  ser.write( str.encode("get %s %d\r" % ( args.addr, args.len)) )
  with open(args.file, "wb") as f:
    count = 0
    while True:
      c=ser.read(1)
      if not c:
        continue
      f.write(c)
      count = count+1
      if(count%256) or count == args.len:
        print("  %3.2f %%" % (100.0*count/args.len), end="\r")
      if count == args.len:
        break
  print()

def dump(args):
  print("d %s %s %s\r" % ( args.addr, args.pages, args.offset))
  ser.write( str.encode("d %s %s %s\r" % ( args.addr, args.pages, args.offset) ) )

def erase(args):
  print("erase %s\r" % args.fill)
  ser.write( str.encode("erase %s\r" % args.fill ) )

parser = argparse.ArgumentParser(
        description='EEPROM Programmer CLI',
        epilog='Written by @adumont')

parser.add_argument('-p', '--port', help='USB port to use' )

subparsers = parser.add_subparsers()

parser_put = subparsers.add_parser('flash', help='Flash a binary file to EEPROM')
parser_put.add_argument('file', help='File to write to EEPROM')
parser_put.add_argument('-a', '--addr', help='Address (hexadecimal), default: 0000', default='0' )
parser_put.add_argument('-l', '--len', type=int, help='Length (bytes, decimal), default: file size', default=None )
parser_put.set_defaults(func=flash)

parser_get = subparsers.add_parser('save', help='Save EEPROM to binary file')
parser_get.add_argument('file', help='File to save as')
parser_get.add_argument('-a', '--addr', help='Address (hexadecimal), default: 0000', default='0' )
parser_get.add_argument('-l', '--len', type=int, help='Length (bytes, decimal)', default=32*1024 )
parser_get.set_defaults(func=save)

parser_dump = subparsers.add_parser('dump', help='Dump EEPROM as text (hex/ascii)')
parser_dump.add_argument('-a', '--addr', help='Address (hexadecimal), default: 0000', default='0' )
parser_dump.add_argument('-n', '--pages', type=int, help='Number of 256B pages to dump (decimal)', default=1 )
parser_dump.add_argument('-o', '--offset', help='Offset to show addresses (hexadecimal), default: 8000', default='8000' )
parser_dump.set_defaults(func=dump)

parser_erase = subparsers.add_parser('erase', help='Erase EEPROM')
parser_erase.add_argument('-f', '--fill', help='Fill byte (hexadecimal), default: ff', default="ff" )
parser_erase.set_defaults(func=erase)

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

if __name__ == '__main__':
  args = parser.parse_args()
  print(vars(args))

  if args.port == None:
    port = glob("/dev/ttyACM*")
    assert( len(port) > 0 )
    port = port[0]
  else:
    port = args.port

  ser = serial.Serial(
      port=port,
      baudrate=115200,
      parity=serial.PARITY_NONE,
      stopbits=serial.STOPBITS_ONE,
      bytesize=serial.EIGHTBITS,
      timeout=0
  )
  print("Connected to programmer on port: " + ser.portstr)

  wait_for_prompt(show=False, timeout=200)

  args.func(args)

  wait_for_prompt()
  ser.close()