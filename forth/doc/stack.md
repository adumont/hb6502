# Stack Operations

## CLS ( -- )
Clear the data stack. Resets X to DTOP.

```
LDX #DTOP
JMP NEXT
```

## DROP ( x -- )
Remove the top cell from the data stack.

```
INX
INX
JMP NEXT
```

## 2DROP ( x1 x2 -- )
Remove two cells from the data stack.

**Implementation:** Defined in threaded Forth as `DROP DROP`.

## DUP ( x -- x x )
Duplicate the top cell.

```
DEX
DEX
LDA 2,X     ; copy old TOS to new slot
STA 0,X
LDA 3,X
STA 1,X
JMP NEXT
```

## 2DUP ( x1 x2 -- x1 x2 x1 x2 )
Duplicate the top two cells.

```
DEX
DEX
LDA 7,X     ; x2 HI -> new TOS
STA 3,X
LDA 6,X     ; x2 LO
STA 2,X
LDA 5,X     ; x1 HI
STA 1,X
LDA 4,X     ; x1 LO
STA 0,X
JMP DEX2_NEXT
```

## OVER ( x1 x2 -- x1 x2 x1 )
Copy the second cell to the top.

```
LDA 4,X     ; x1 LO
STA 0,X
LDA 5,X     ; x1 HI
STA 1,X
JMP DEX2_NEXT
```

## SWAP ( x1 x2 -- x2 x1 )
Exchange the top two cells.

```
LDA 2,X
LDY 4,X
STY 2,X
STA 4,X
LDA 3,X
LDY 5,X
STY 3,X
STA 5,X
JMP NEXT
```

## ROT ( x1 x2 x3 -- x2 x3 x1 )
Rotate the third cell to the top.

```
LDA 6,X     ; save x1
STA W
LDA 7,X
STA W+1
LDA 4,X     ; x2 -> slot 2
STA 6,X
LDA 5,X
STA 7,X
LDA 2,X     ; x3 -> slot 1
STA 4,X
LDA 3,X
STA 5,X
LDA W       ; x1 -> TOS
STA 2,X
LDA W+1
STA 3,X
JMP NEXT
```

## -ROT ( x1 x2 x3 -- x3 x1 x2 )
Rotate the top cell to third position. Defined in threaded code as `ROT ROT`.

## ?DUP ( x -- 0 | x x )
Duplicate only if non-zero.

```
LDA 2,X
ORA 3,X
BEQ @skip
JMP do_DUP
@skip:
JMP NEXT
```

## NIP ( x1 x2 -- x2 )
Drop the second cell. Defined as `SWAP DROP`.

## PICK ( n -- x )
Copy the nth cell (0-indexed) from the stack to the top. Defined as `1+ 2* SP@ + @`.

## DEPTH ( -- n )
Return the number of cells on the data stack. Defined as `DTOP SP@ - 2/`.

## SP@ ( -- addr )
Push the current data stack pointer address onto the stack. Because X points one slot below ToS, we add 2.

```
TXA
INA
INA
STA 0,X
STZ 1,X
JMP DEX2_NEXT
```

## SP! ( addr -- )
Set the data stack pointer from ToS. Used for stack reset.

```
LDA 2,X
TAX
JMP DEX2_NEXT
```

## RP@ ( -- addr )
Push the return stack pointer onto the data stack. Calculated by transferring SP to A.

```
STZ 1,X
TXA
TAY
TSX
TXA
STA 0,Y
TYA
TAX
JMP DEX2_NEXT
```

## RP! ( addr -- )
Set the return stack pointer from ToS.

```
STX G1      ; save X
LDA 2,X     ; load new SP value
TAX
TXS         ; set S register
LDX G1      ; restore X
JMP do_DROP
```

## 1 ( -- 1 )
Push the constant 1.

```
LDA #1
STA 0,X
STZ 1,X
JMP DEX2_NEXT
```

## 0 ( -- 0 )
Push the constant 0.

```
STZ 0,X
STZ 1,X
JMP DEX2_NEXT
```
