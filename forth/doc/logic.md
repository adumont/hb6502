# Logic and Comparison

## Comparisons

### 0= ( n -- flag )
True if n is zero. Flag is $FFFF for true, $0000 for false.

```
LDA 2,X
ORA 3,X
BEQ @true
; false:
STZ 2,X
STZ 3,X
JMP NEXT
@true:
LDA #$FF
STA 2,X
STA 3,X
JMP NEXT
```

### 0< ( n -- flag )
True if n is negative (MSB set).

```
LDA 3,X
BMI @true
BRA @false      ; falls through to the same 0= false path
```

Shares the `true`/`false` code with `0=`.

### U< ( n1 n2 -- flag )
Unsigned less-than comparison.

```
LDA 5,X     ; n1 HI
CMP 3,X     ; cmp n2 HI
BCC @true
BNE @false
LDA 4,X     ; HI equal, compare LO
CMP 2,X
BCC @true
@false:
STZ 5,X
STZ 4,X
JMP do_DROP
@true:
LDA #$FF
STA 5,X
STA 4,X
JMP do_DROP
```

### = ( n1 n2 -- flag )
Equal. Defined as `- 0=`.

### < ( n1 n2 -- flag )
Signed less-than. Defined as `- 0<`.

### > ( n1 n2 -- flag )
Signed greater-than. Defined as `SWAP <`.

### <= ( n1 n2 -- flag )
Signed less-or-equal. Defined as `> 0=`.

### >= ( n1 n2 -- flag )
Signed greater-or-equal. Defined as `SWAP <=`.

## Bitwise

### AND ( x1 x2 -- x )
Bitwise AND.

```
LDA 2,X
AND 4,X
STA 4,X
LDA 3,X
AND 5,X
STA 5,X
JMP do_DROP
```

### OR ( x1 x2 -- x )
Bitwise OR.

```
LDA 2,X
ORA 4,X
STA 4,X
LDA 3,X
ORA 5,X
STA 5,X
JMP do_DROP
```

### XOR ( x1 x2 -- x )
Bitwise XOR.

```
LDA 2,X
EOR 4,X
STA 4,X
LDA 3,X
EOR 5,X
STA 5,X
JMP do_DROP
```

### NOT ( x -- ~x )
Bitwise complement (ones' complement). Each bit is inverted.

```
LDA 2,X
EOR #$FF
STA 2,X
LDA 3,X
EOR #$FF
STA 3,X
JMP NEXT
```
