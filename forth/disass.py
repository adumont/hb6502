# Opcodes
# for each, we store the mnemonic and mode id (see 'modes' dict below)

opcodes = {
      0: [ 'brk' ,  9 ],  #   brk   s
      1: [ 'ora' , 11 ],  #   ora   (zp,x)
      4: [ 'tsb' , 10 ],  #   tsb   zp
      5: [ 'ora' , 10 ],  #   ora   zp
      6: [ 'asl' , 10 ],  #   asl   zp
      7: [ 'rmb0', 10 ],  #   rmb0  zp
      8: [ 'php' ,  9 ],  #   php   s
      9: [ 'ora' ,  6 ],  #   ora   #
     10: [ 'asl' ,  5 ],  #   asl   A
     12: [ 'tsb' ,  0 ],  #   tsb   a
     13: [ 'ora' ,  0 ],  #   ora   a
     14: [ 'asl' ,  0 ],  #   asl   a
     15: [ 'bbr0',  8 ],  #   bbr0  r
     16: [ 'bpl' ,  8 ],  #   bpl   r
     17: [ 'ora' , 15 ],  #   ora   (zp),y
     18: [ 'ora' , 14 ],  #   ora   (zp)
     20: [ 'trb' , 10 ],  #   trb   zp
     21: [ 'ora' , 12 ],  #   ora   zp,x
     22: [ 'asl' , 12 ],  #   asl   zp,x
     23: [ 'rmb1', 10 ],  #   rmb1  zp
     24: [ 'clc' ,  7 ],  #   clc   i
     25: [ 'ora' ,  3 ],  #   ora   a,y
     26: [ 'inc' ,  5 ],  #   inc   A
     28: [ 'trb' ,  0 ],  #   trb   a
     29: [ 'ora' ,  2 ],  #   ora   a,x
     30: [ 'asl' ,  2 ],  #   asl   a,x
     31: [ 'bbr1',  8 ],  #   bbr1  r
     32: [ 'jsr' ,  0 ],  #   jsr   a
     33: [ 'and' , 11 ],  #   and   (zp,x)
     36: [ 'bit' , 10 ],  #   bit   zp
     37: [ 'and' , 10 ],  #   and   zp
     38: [ 'rol' , 10 ],  #   rol   zp
     39: [ 'rmb2', 10 ],  #   rmb2  zp
     40: [ 'plp' ,  9 ],  #   plp   s
     41: [ 'and' ,  6 ],  #   and   #
     42: [ 'rol' ,  5 ],  #   rol   A
     44: [ 'bit' ,  0 ],  #   bit   a
     45: [ 'and' ,  0 ],  #   and   a
     46: [ 'rol' ,  0 ],  #   rol   a
     47: [ 'bbr2',  8 ],  #   bbr2  r
     48: [ 'bmi' ,  8 ],  #   bmi   r
     49: [ 'and' , 15 ],  #   and   (zp),y
     50: [ 'and' , 14 ],  #   and   (zp)
     52: [ 'bit' , 12 ],  #   bit   zp,x
     53: [ 'and' , 12 ],  #   and   zp,x
     54: [ 'rol' , 12 ],  #   rol   zp,x
     55: [ 'rmb3', 10 ],  #   rmb3  zp
     56: [ 'sec' ,  7 ],  #   sec   i
     57: [ 'and' ,  3 ],  #   and   a,y
     58: [ 'dec' ,  5 ],  #   dec   A
     60: [ 'bit' ,  2 ],  #   bit   a,x
     61: [ 'and' ,  2 ],  #   and   a,x
     62: [ 'rol' ,  2 ],  #   rol   a,x
     63: [ 'bbr3',  8 ],  #   bbr3  r
     64: [ 'rti' ,  9 ],  #   rti   s
     65: [ 'eor' , 11 ],  #   eor   (zp,x)
     69: [ 'eor' , 10 ],  #   eor   zp
     70: [ 'lsr' , 10 ],  #   lsr   zp
     71: [ 'rmb4', 10 ],  #   rmb4  zp
     72: [ 'pha' ,  9 ],  #   pha   s
     73: [ 'eor' ,  6 ],  #   eor   #
     74: [ 'lsr' ,  5 ],  #   lsr   A
     76: [ 'jmp' ,  0 ],  #   jmp   a
     77: [ 'eor' ,  0 ],  #   eor   a
     78: [ 'lsr' ,  0 ],  #   lsr   a
     79: [ 'bbr4',  8 ],  #   bbr4  r
     80: [ 'bvc' ,  8 ],  #   bvc   r
     81: [ 'eor' , 15 ],  #   eor   (zp),y
     82: [ 'eor' , 14 ],  #   eor   (zp)
     85: [ 'eor' , 12 ],  #   eor   zp,x
     86: [ 'lsr' , 12 ],  #   lsr   zp,x
     87: [ 'rmb5', 10 ],  #   rmb5  zp
     88: [ 'cli' ,  7 ],  #   cli   i
     89: [ 'eor' ,  3 ],  #   eor   a,y
     90: [ 'phy' ,  9 ],  #   phy   s
     93: [ 'eor' ,  2 ],  #   eor   a,x
     94: [ 'lsr' ,  2 ],  #   lsr   a,x
     95: [ 'bbr5',  8 ],  #   bbr5  r
     96: [ 'rts' ,  9 ],  #   rts   s
     97: [ 'adc' , 11 ],  #   adc   (zp,x)
    100: [ 'stz' , 10 ],  #   stz   zp
    101: [ 'adc' , 10 ],  #   adc   zp
    102: [ 'ror' , 10 ],  #   ror   zp
    103: [ 'rmb6', 10 ],  #   rmb6  zp
    104: [ 'pla' ,  9 ],  #   pla   s
    105: [ 'adc' ,  6 ],  #   adc   #
    106: [ 'ror' ,  5 ],  #   ror   A
    108: [ 'jmp' ,  4 ],  #   jmp   (a)
    109: [ 'adc' ,  0 ],  #   adc   a
    110: [ 'ror' ,  0 ],  #   ror   a
    111: [ 'bbr6',  8 ],  #   bbr6  r
    112: [ 'bvs' ,  8 ],  #   bvs   r
    113: [ 'adc' , 15 ],  #   adc   (zp),y
    114: [ 'adc' , 14 ],  #   adc   (zp)
    116: [ 'stz' , 12 ],  #   stz   zp,x
    117: [ 'adc' , 12 ],  #   adc   zp,x
    118: [ 'ror' , 12 ],  #   ror   zp,x
    119: [ 'rmb7', 10 ],  #   rmb7  zp
    120: [ 'sei' ,  7 ],  #   sei   i
    121: [ 'adc' ,  3 ],  #   adc   a,y
    122: [ 'ply' ,  9 ],  #   ply   s
    124: [ 'jmp' ,  1 ],  #   jmp   (a,x)
    125: [ 'adc' ,  2 ],  #   adc   a,x
    126: [ 'ror' ,  2 ],  #   ror   a,x
    127: [ 'bbr7',  8 ],  #   bbr7  r
    128: [ 'bra' ,  8 ],  #   bra   r
    129: [ 'sta' , 11 ],  #   sta   (zp,x)
    132: [ 'sty' , 10 ],  #   sty   zp
    133: [ 'sta' , 10 ],  #   sta   zp
    134: [ 'stx' , 10 ],  #   stx   zp
    135: [ 'smb0', 10 ],  #   smb0  zp
    136: [ 'dey' ,  7 ],  #   dey   i
    137: [ 'bit' ,  6 ],  #   bit   #
    138: [ 'txa' ,  7 ],  #   txa   i
    140: [ 'sty' ,  0 ],  #   sty   a
    141: [ 'sta' ,  0 ],  #   sta   a
    142: [ 'stx' ,  0 ],  #   stx   a
    143: [ 'bbs0',  8 ],  #   bbs0  r
    144: [ 'bcc' ,  8 ],  #   bcc   r
    145: [ 'sta' , 15 ],  #   sta   (zp),y
    146: [ 'sta' , 14 ],  #   sta   (zp)
    148: [ 'sty' , 12 ],  #   sty   zp,x
    149: [ 'sta' , 12 ],  #   sta   zp,x
    150: [ 'stx' , 13 ],  #   stx   zp,y
    151: [ 'smb1', 10 ],  #   smb1  zp
    152: [ 'tya' ,  7 ],  #   tya   i
    153: [ 'sta' ,  3 ],  #   sta   a,y
    154: [ 'txs' ,  7 ],  #   txs   i
    156: [ 'stz' ,  0 ],  #   stz   a
    157: [ 'sta' ,  2 ],  #   sta   a,x
    158: [ 'stz' ,  2 ],  #   stz   a,x
    159: [ 'bbs1',  8 ],  #   bbs1  r
    160: [ 'ldy' ,  6 ],  #   ldy   #
    161: [ 'lda' , 11 ],  #   lda   (zp,x)
    162: [ 'ldx' ,  6 ],  #   ldx   #
    164: [ 'ldy' , 10 ],  #   ldy   zp
    165: [ 'lda' , 10 ],  #   lda   zp
    166: [ 'ldx' , 10 ],  #   ldx   zp
    167: [ 'smb2', 10 ],  #   smb2  zp
    168: [ 'tay' ,  7 ],  #   tay   i
    169: [ 'lda' ,  6 ],  #   lda   #
    170: [ 'tax' ,  7 ],  #   tax   i
    172: [ 'ldy' ,  0 ],  #   ldy   a
    173: [ 'lda' ,  0 ],  #   lda   a
    174: [ 'ldx' ,  0 ],  #   ldx   a
    175: [ 'bbs2',  8 ],  #   bbs2  r
    176: [ 'bcs' ,  8 ],  #   bcs   r
    177: [ 'lda' , 15 ],  #   lda   (zp),y
    178: [ 'lda' , 14 ],  #   lda   (zp)
    180: [ 'ldy' , 12 ],  #   ldy   zp,x
    181: [ 'lda' , 12 ],  #   lda   zp,x
    182: [ 'ldx' , 13 ],  #   ldx   zp,y
    183: [ 'smb3', 10 ],  #   smb3  zp
    184: [ 'clv' ,  7 ],  #   clv   i
    185: [ 'lda' ,  3 ],  #   lda   a,y
    186: [ 'tsx' ,  7 ],  #   tsx   i
    188: [ 'ldy' ,  2 ],  #   ldy   a,x
    189: [ 'lda' ,  2 ],  #   lda   a,x
    190: [ 'ldx' ,  3 ],  #   ldx   a,y
    191: [ 'bbs3',  8 ],  #   bbs3  r
    192: [ 'cpy' ,  6 ],  #   cpy   #
    193: [ 'cmp' , 11 ],  #   cmp   (zp,x)
    196: [ 'cpy' , 10 ],  #   cpy   zp
    197: [ 'cmp' , 10 ],  #   cmp   zp
    198: [ 'dec' , 10 ],  #   dec   zp
    199: [ 'smb4', 10 ],  #   smb4  zp
    200: [ 'iny' ,  7 ],  #   iny   i
    201: [ 'cmp' ,  6 ],  #   cmp   #
    202: [ 'dex' ,  7 ],  #   dex   i
    203: [ 'wai' ,  7 ],  #   wai   i
    204: [ 'cpy' ,  0 ],  #   cpy   a
    205: [ 'cmp' ,  0 ],  #   cmp   a
    206: [ 'dec' ,  0 ],  #   dec   a
    207: [ 'bbs4',  8 ],  #   bbs4  r
    208: [ 'bne' ,  8 ],  #   bne   r
    209: [ 'cmp' , 15 ],  #   cmp   (zp),y
    210: [ 'cmp' , 14 ],  #   cmp   (zp)
    213: [ 'cmp' , 12 ],  #   cmp   zp,x
    214: [ 'dec' , 12 ],  #   dec   zp,x
    215: [ 'smb5', 10 ],  #   smb5  zp
    216: [ 'cld' ,  7 ],  #   cld   i
    217: [ 'cmp' ,  3 ],  #   cmp   a,y
    218: [ 'phx' ,  9 ],  #   phx   s
    219: [ 'stp' ,  7 ],  #   stp   i
    221: [ 'cmp' ,  2 ],  #   cmp   a,x
    222: [ 'dec' ,  2 ],  #   dec   a,x
    223: [ 'bbs5',  8 ],  #   bbs5  r
    224: [ 'cpx' ,  6 ],  #   cpx   #
    225: [ 'sbc' , 11 ],  #   sbc   (zp,x)
    228: [ 'cpx' , 10 ],  #   cpx   zp
    229: [ 'sbc' , 10 ],  #   sbc   zp
    230: [ 'inc' , 10 ],  #   inc   zp
    231: [ 'smb6', 10 ],  #   smb6  zp
    232: [ 'inx' ,  7 ],  #   inx   i
    233: [ 'sbc' ,  6 ],  #   sbc   #
    234: [ 'nop' ,  7 ],  #   nop   i
    236: [ 'cpx' ,  0 ],  #   cpx   a
    237: [ 'sbc' ,  0 ],  #   sbc   a
    238: [ 'inc' ,  0 ],  #   inc   a
    239: [ 'bbs6',  8 ],  #   bbs6  r
    240: [ 'beq' ,  8 ],  #   beq   r
    241: [ 'sbc' , 15 ],  #   sbc   (zp),y
    242: [ 'sbc' , 14 ],  #   sbc   (zp)
    245: [ 'sbc' , 12 ],  #   sbc   zp,x
    246: [ 'inc' , 12 ],  #   inc   zp,x
    247: [ 'smb7', 10 ],  #   smb7  zp
    248: [ 'sed' ,  7 ],  #   sed   i
    249: [ 'sbc' ,  3 ],  #   sbc   a,y
    250: [ 'plx' ,  9 ],  #   plx   s
    253: [ 'sbc' ,  2 ],  #   sbc   a,x
    254: [ 'inc' ,  2 ],  #   inc   a,x
    255: [ 'bbs7',  8 ],  #   bbs7  r
}

# Addressing modes
# for each, we store the mode name and instructions length

modes = {
    0: [ 'a', 3 ],
    1: [ '(a,x)', 3 ],
    2: [ 'a,x', 3 ],
    3: [ 'a,y', 3 ],
    4: [ '(a)', 3 ],
    5: [ 'A', 1 ],
    6: [ '#', 2 ],
    7: [ 'i', 1 ],
    8: [ 'r', 2 ],
    9: [ 's', 1 ],
    10: [ 'zp', 2 ],
    11: [ '(zp,x)', 2 ],
    12: [ 'zp,x', 2 ],
    13: [ 'zp,y', 2 ],
    14: [ '(zp)', 2 ],
    15: [ '(zp),y', 2 ],
}

def isValidOpcode(b):
    return b in opcodes.keys()

# decode an opcode byte b
# returns mnemonic, mode, length
def decode(b):
    if isinstance(b,str):
        b=hex2dec(b)
    if isValidOpcode(b):
        o, m = opcodes[b]
        m, l = modes[m]
        return o, m, l
    else:
        return None,None,None

def hex2dec(s):
    return int(s,16)

def render_instr(_args):
    # args is a list: addr, mnemonic, [op1, op2]
    # addr: the instruction's address. int or hex str
    # mnemonic, op1 & op2 are strings in hex

    # we create a copy of the _args list, because we will modify it
    args=_args[:]

    # we revers the list, so we can pop each item
    # and at the end, the operand's byte are correctly ordered
    addr = args.pop(0)
    if isinstance(addr,str):
        addr=hex2dec(addr)

    opcode = args.pop(0)

    # if isinstance(opcode,str):
    #     opcode=hex2dec(opcode)

    comment = ""

    unknown = ".."

    o,m,l = decode(opcode)

    miss = 0

    while True:
        miss = l - len(args) - 1 # how many operands bytes we are missing
        if miss >= 0:
            break
        # too much args, remove
        args.pop()

    args.reverse()
    operand="".join(args)
    args.reverse() # now we need it again in the chronological order, for hexdump

    operand = unknown*miss + operand

    hexdump = " ".join( [opcode] + args + [ unknown for _ in range(miss) ])

    #if o.startswith("b") and o not in ["bit", "brk"] :
    if m == "r" :
        if miss==0 :
            dest = addr + 2
            operand = hex2dec(operand)
            if operand > 0x7f:
                dest -= 0x100 - operand
            else:
                dest += operand
            operand = "%04X" % dest
        else:
            # add an extra unknown byte
            operand = unknown + operand
    
    if l>1:
        operand = "$"+operand

    if m == "#":
        operand = "#"+operand
    if ',x' in m:
        operand += ",x"
    if m.startswith('('):
        operand = "(" + operand + ")"
    if m.endswith('y'):
        operand += ",y"
    if m == "A":
        operand = "A"
    
    # return ("%04X: %-10s %s %-8s %-4s %s " % (addr, hexdump.upper(), o, operand, m, comment))
    return ("%04X: %s %-8s " % (addr, o, operand))
