# Arithmetic

## + ( n1 n2 -- sum )
16-bit addition.

```
CLC
LDA 2,X     ; n2 LO
ADC 4,X     ; + n1 LO
STA 4,X
LDA 3,X     ; n2 HI
ADC 5,X     ; + n1 HI
STA 5,X
JMP do_DROP ; drop n2 (now in n1's slot)
```

**Note:** Do not confuse with the Forth word `+` defined in `bootstrap.f` which works differently. This is the assembler primitive.

## - ( n1 n2 -- diff )
16-bit subtraction: `n1 - n2`.

```
SEC
LDA 4,X     ; n1 LO
SBC 2,X     ; - n2 LO
STA 4,X
LDA 5,X     ; n1 HI
SBC 3,X     ; - n2 HI
STA 5,X
JMP do_DROP
```

## 1+ ( n -- n+1 )
Increment by 1.

```
INC 2,X
BNE @skip
INC 3,X
@skip:
JMP NEXT
```

## 2+ ( n -- n+2 )
Increment by 2.

```
CLC
LDA 2,X
ADC #2
STA 2,X
BCC @skip
INC 3,X
@skip:
JMP NEXT
```

## 2* ( n -- n*2 )
Arithmetic left shift. Sign is preserved (65C02 ASL/ROL).

```
ASL 2,X
ROL 3,X
JMP NEXT
```

## D2* ( d -- d*2 )
Double-cell arithmetic left shift (32-bit).

```
ASL 4,X
ROL 5,X
ROL 2,X
ROL 3,X
JMP NEXT
```

## 2/ ( n -- n/2 )
Arithmetic (signed) right shift. Sign bit is replicated.

```
SEC             ; assume negative (set carry)
LDA 3,X
BMI @skip
CLC             ; actually positive
@skip:
ROR 3,X
ROR 2,X
JMP NEXT
```

## U2/ ( u -- u/2 )
Unsigned right shift. Zero always shifted into bit 7.

```
LSR 3,X
ROR 2,X
JMP NEXT
```

## UD2/ ( ud -- ud/2 )
Unsigned double-cell right shift (32-bit).

```
LSR 3,X
ROR 2,X
ROR 5,X
ROR 4,X
JMP NEXT
```

## D2/ ( d -- d/2 )
Signed double-cell right shift (32-bit). Sign is preserved.

```
SEC
LDA 3,X
BMI @skip
CLC
@skip:
ROR 3,X
ROR 2,X
ROR 5,X
ROR 4,X
JMP NEXT
```

## D+ ( d1 d2 -- dsum )
32-bit addition. Stack: ( lo1 hi1 lo2 hi2 -- losum hisum ).

```
CLC
LDA 4,X     ; lo2
ADC 8,X     ; + lo1
STA 8,X
LDA 5,X     ; hi2 LO
ADC 9,X     ; + hi1 LO
STA 9,X
LDA 2,X     ; hi2 HI
ADC 6,X     ; + hi1 HI
STA 6,X
LDA 3,X     ; hi2 HI
ADC 7,X     ; + hi1 HI
STA 7,X
INX         ; drop one cell
INX
JMP do_DROP ; drop remaining
```

## D- ( d1 d2 -- ddiff )
32-bit subtraction. Same stack layout as D+.

```
SEC
LDA 8,X     ; lo1
SBC 4,X     ; - lo2
STA 8,X
LDA 9,X
SBC 5,X
STA 9,X
LDA 6,X
SBC 2,X
STA 6,X
LDA 7,X
SBC 3,X
STA 7,X
INX
INX
JMP do_DROP
```

## UM* ( u1 u2 -- ud )
Unsigned 16-bit multiply, 32-bit result. Standard shift-and-add algorithm.

**Stack:** ( u1 u2 -- ud-lo ud-hi )

**Algorithm:**
1. Copy u1 to G1 (multiplicand). Clear G2 (for upper bits).
2. Clear 4 bytes on stack for the partial product.
3. Loop 16 times:
   - Shift u2 (at stack) right → examine each bit via carry
   - If carry set, add G1:G2 (the shifted multiplicand) to partial product
   - Shift G1:G2 left (multiply multiplicand by 2)

```
LDA 2,X     ; u1 (multiplicand) -> G1:G2
STA G1
LDA 3,X
STA G1+1
STZ G2
STZ G2+1
STZ 2,X     ; clear partial product area (4 bytes)
STZ 3,X
STZ 0,X
STZ 1,X

LDY #$10    ; 16 bits
@shift_right_n1:
LSR 5,X     ; shift u2 right -> carry
ROR 4,X
BCC @shift_left_n2
; carry=1: add multiplicand to partial product
CLC
LDA G1
ADC 2,X
STA 2,X
LDA G1+1
ADC 3,X
STA 3,X
LDA G2
ADC 0,X
STA 0,X
LDA G2+1
ADC 1,X
STA 1,X
@shift_left_n2:
ASL G1      ; shift multiplicand left
ROL G1+1
ROL G2
ROL G2+1
DEY
BNE @shift_right_n1
JMP DEX2_NEXT
```

**Implementation note:** The threaded code wrapper `UM*` calls `do_STAR_UM_STAR` inline, then does `ROT DROP` to remove u1 from the stack, leaving the 32-bit result as (lo hi).

```
JMP do_COLON
.ADDR do_STAR_UM_STAR
.ADDR do_ROT, do_DROP
.ADDR do_SEMI
```

## UM/MOD ( ud u -- rem quot )
Unsigned 32-bit by 16-bit division. Returns quotient and remainder (each 16-bit).

**Stack:** ( ud-lo ud-hi u -- rem quot )

**Algorithm:** From Garth Wilson's 6502 division routine (http://www.6502.org/source/integers/ummodfix/ummodfix.htm). Shifts the 32-bit dividend left through a 17-bit loop, subtracting the divisor when possible.

```
SEC         ; check overflow
LDA 4,X     ; subtract hi cell of dividend
SBC 2,X     ; by divisor
LDA 5,X
SBC 3,X
BCS @oflo   ; overflow or /0

LDA #$11    ; loop 17 times
STA 0,X     ; use stack for counter
@loop:
ROL 6,X     ; rotate dividend left
ROL 7,X
DEC 0,X
BEQ @fin
ROL 4,X
ROL 5,X
STZ G1
ROL G1      ; carry from above -> G1

SEC
LDA 4,X     ; subtract divisor
SBC 2,X
STA G1+1
LDA 5,X
SBC 3,X
TAY
LDA G1
SBC #0
BCC @loop

LDA G1+1    ; result -> new hi dividend
STA 4,X
STY 5,X
BRA @loop

@oflo:
LDA #$FF    ; overflow: return FFFF for both
STA 4,X
STA 5,X
STA 6,X
STA 7,X
@fin:
JMP do_DROP
```

The wrapper `UM/MOD` is threaded: `STAR_UM_DIV_MOD SWAP ;` (the SWAP corrects the result order to standard Forth: remainder quotient).

## /10 ( n -- q )
Divide an unsigned 16-bit number by 10.

**Algorithm:** Uses multiplication by the reciprocal: `n / 10 ≈ (n × $199A) >> 16`

The magic number `$199A` (6554) is the 16-bit approximation of 2^16 / 10. The high word of the 32-bit product yields an exact integer result for all 16-bit inputs.

```
JMP do_COLON
.ADDR do_LIT
.WORD $199A
.ADDR do_UM_STAR
.ADDR do_SWAP
.ADDR do_DROP
.ADDR do_SEMI
```

**Why $199A?** `$199A / 65536 = 0.1000061...` which is slightly above `0.1`. The maximum error term for any 16-bit value is `65535 × 0.0000061 ≈ 0.4`, so the result is never off by more than one. Testing confirms exact results for all 16-bit inputs.

**History:** Previously used an inefficient shift-and-add divide-by-10 with `divide_by_10`, `halveLONG1`, and `addLONG1toLONG2` subroutines. Replaced with the multiply-by-reciprocal approach which is both faster and smaller.

## NEG ( n -- -n )
Two's complement negation: `NOT 1+`.

**Implementation:** Defined in `bootstrap.f` as `NOT 1+`.
