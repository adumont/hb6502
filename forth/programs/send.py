#!/usr/bin/env python3

import argparse
import serial
import time
import os.path
from time import sleep
import math
import zlib


from glob import glob

def get_response(show=False):
  exit = False
  b = []

  while True:
    for c in ser.read():
      if c not in (0x0d, 0x0a):
        b.append(c)
      if show:
        print("%c %02X   " % (c,c), end='', flush=True)
      if c == 0x0A:
        exit = True
        break
    if exit:
      break

  return b


def cmd_send(args):
  l = os.stat(args.file).st_size

  batch = 10

  with open(args.file, "r") as f:
    count = 0
    while True:
      data=f.read(batch)
      if not data:
        break
      
      for c in data:
        print(c , end="")
        ser.write( str.encode( c ) )
        if c in [ 0x0A, 0x0D ]:
          sleep(0.8)
        else:
          sleep(0.03)

      count += len(data)

      # print("  %3.2f %%" % (100.0*count/l), end="\r")

      if count >= l: break

  # print("%d bytes written from %04X to %04X, CRC32: %04X.%04X" % (addr-addr_start, addr_start, addr-1, crc32>>16, crc32 & 0xFFFF) )


def cmd_run(args):

  print("run")
  ser.write( str.encode( "run\r") )

  print()

parser = argparse.ArgumentParser(
        description='Cerberus2080 Serial Programmer',
        epilog='Written by @adumont')

parser.add_argument('-p', '--port', help='USB port to use', default="/dev/ttyUSB0" )

subparsers = parser.add_subparsers()

parser_put = subparsers.add_parser('send', help='sends a text program to FORTH')
parser_put.add_argument('file', help='File to send')
parser_put.set_defaults(func=cmd_send)

parser_put = subparsers.add_parser('run', help='Send the run command')
parser_put.set_defaults(func=cmd_run)

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
    port = glob("/dev/ttyUSB*")
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
  print("Connected to Cerberus on port: " + ser.portstr)

  sleep(1)

  # wait_for_prompt(show=False, timeout=200)

  args.func(args)

  # wait_for_prompt()
  ser.close()
