# Memory and Dictionary

## @ ( addr -- x )
Fetch a 16-bit value from memory at `addr`.

```
LDA 2,X     ; LO byte of addr
STA W
LDA 3,X     ; HI byte of addr
STA W+1
LDA (W)     ; read LO
STA 2,X
LDY #1
LDA (W),y   ; read HI
STA 3,X
JMP NEXT
```

**Note:** The address on the stack is overwritten with the value read.

## ! ( x addr -- )
Store a 16-bit value at `addr`.

```
LDA 2,X     ; LO byte of addr
STA W
LDA 3,X     ; HI byte of addr
STA W+1
LDA 4,X     ; value LO
STA (W)
LDY #1
LDA 5,X     ; value HI
STA (W),y
JMP do_DROP  ; drop addr and value (2 cells)
```

## C@ ( addr -- byte )
Fetch a single byte from memory. HI byte of result is zeroed.

```
LDA (2,X)   ; read byte
STA 2,X
STZ 3,X
JMP NEXT
```

## C! ( byte addr -- )
Store a single byte at `addr`.

```
LDA 4,X     ; byte value
STA (2,X)   ; store at addr
BRA end_do_STORE  ; shared drop code with !
```

The shared `end_do_STORE` does `JMP do_DROP` (drops 2 cells).

## HERE ( -- addr )
Push the Dictionary Pointer (DP) onto the stack. This is the address of the next free byte in the dictionary.

```
LDA DP
STA 0,X
LDA DP+1
STA 1,X
JMP DEX2_NEXT
```

## HERE++ ( -- )
Advance HERE by 2 bytes (one cell). Equivalent to `HERE 2 + DP!`.

```
CLC
LDA DP
ADC #2
STA DP
BCC @skip
INC DP+1
@skip:
JMP NEXT
```

## DP! ( addr -- )
Set the Dictionary Pointer from the stack.

```
LDA 2,X     ; LO
STA DP
LDA 3,X     ; HI
STA DP+1
JMP do_DROP
```

## ALLOT ( n -- )
Advance HERE by n bytes. Defined as `HERE + DP!`.

```
: ALLOT  HERE + DP! ;
```

## , ( x -- )
Commit a 16-bit value to the dictionary at HERE, then advance HERE by 2.

```
LDA 2,X
STA (DP)    ; store LO at HERE
LDA 3,X
LDY #1
STA (DP),y  ; store HI at HERE+1
INX         ; drop value
INX
BRA do_HEREPP  ; advance HERE by 2
```

## C, ( byte -- )
Commit a single byte to the dictionary at HERE, then advance HERE by 1.

```
LDA 2,X     ; byte value
INX         ; drop value
INX
STA (DP)    ; store at HERE
INC DP      ; HERE += 1
BNE @skip
INC DP+1
@skip:
JMP NEXT
```

**Note:** The `shortcut_ccomma` label allows other primitives to jump directly to the store-and-advance code.

## LATEST ( -- addr )
Push the address of the `LATEST` system variable.

```
LDA #<LATEST
STA 0,X
LDA #>LATEST
STA 1,X
JMP DEX2_NEXT
```

## LAST ( -- addr )
Push the address of the most recently defined word in the dictionary (the value stored in LATEST).

```
LDA #<LATEST
STA G1
LDA #>LATEST
STA G1+1
LDA (G1)     ; read LATEST value
STA 0,X
LDY #1
LDA (G1),y
STA 1,X
JMP DEX2_NEXT
```

## >CFA ( hdr -- cfa )
Convert a header address (HDR) to a Code Field Address (CFA). Skips past the link field (2 bytes) and the counted string name (length byte + characters).

```
CLC
LDA 2,X     ; header addr
ADC #2      ; skip link field
STA W
LDA 3,X
ADC #0
STA W+1
LDA (W)     ; read length+flags byte
AND #$1F    ; mask out flags, keep length
INA         ; +1 for the length byte itself
CLC
ADC W       ; add to header+2
STA 2,X
LDA W+1
ADC #0
STA 3,X
JMP NEXT
```

**Note:** The length byte stores flags in the upper 3 bits: bit 7 = IMMEDIATE flag, bit 6 = HIDDEN flag.

## CMOVE ( src dst len -- )
Copy `len` bytes from `src` to `dst`. Copies in byte-by-byte fashion from high to low address.

```
LDY 2,X     ; len -> Y
LDA 6,X     ; src -> G1
STA G1
LDA 7,X
STA G1+1
LDA 4,X     ; dst -> G2
STA G2
LDA 5,X
STA G2+1
@loop:
DEY
LDA (G1),Y
STA (G2),Y
CPY #0
BNE @loop
TXA         ; drop 3 cells (6 bytes)
CLC
ADC #6
TAX
JMP NEXT
```

**Note:** Copies backwards (from high address to low), so it handles overlapping regions correctly when src > dst.

## VAR ( -- addr )
Pushes the address right after the JMP instruction of the current word definition (i.e., `W+3`). Used by `VARIABLE` to provide the PFA (Parameter Field Address).

```
CLC
LDA W
ADC #3
STA 0,X
LDA W+1
ADC #0
STA 1,X
JMP DEX2_NEXT
```
