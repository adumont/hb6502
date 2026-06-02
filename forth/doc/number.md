# Numbers and Base

## BASE ( -- addr )
Push the address of the BASE variable. Used to read or change the current number base.

```
LDA #<BASE
STA 0,X
LDA #>BASE
STA 1,X
JMP DEX2_NEXT
```

## DEC ( -- )
Set base to decimal (10).

```
LDA #$0A
STA BASE
JMP NEXT
```

## HEX ( -- )
Set base to hexadecimal (16).

```
LDA #$10
STA BASE
JMP NEXT
```

## OCT ( -- )
Set base to octal (8).

```
LDA #$08
STA BASE
JMP NEXT
```

## BIN ( -- )
Set base to binary (2).

```
LDA #$02
STA BASE
JMP NEXT
```

## NUMBER ( addr len -- value flag )
Parse a string as a number in the current BASE. Handles prefix characters for forced base.

**Return values:**
- `value, 0` on error
- `value, nonzero` on success

**Prefix characters:**
| Prefix | Base | Example |
|--------|------|---------|
| `#` | Decimal | `#1234` |
| `$` | Hexadecimal | `$ABCD` |
| `%` | Binary | `%1010` |
| `o` | Octal | `o777` |

Without prefix, the current BASE is used.

**Algorithm:**
1. Save string addr in W, length in G1
2. If first char is a prefix, force the base and advance Y
3. Loop: multiply current result (G2) by BASE, add digit value

```
NUMBER:
LDA 4,X     ; addr -> W
STA W
LDA 5,X
STA W+1
LDA 2,X     ; len -> G1
STA G1
STZ G2      ; result = 0
STZ G2+1
LDY #0
PHX         ; save X on stack (needs X for base)
LDA (W),Y   ; check first char for prefix
CMP #'#'    ; decimal
BEQ @prefixed_base10
CMP #'$'    ; hex
BEQ @prefixed_base16
CMP #'%'    ; binary
BEQ @prefixed_base2
CMP #'o'    ; octal
BEQ @prefixed_base8
LDX BASE    ; no prefix: use current base
BRA @read_digit
@next:
JSR mulG2xBASE  ; G2 *= BASE
@read_digit:
LDA (W),Y
JSR nibble_asc_to_value  ; convert char to digit
BCS @err     ; invalid digit -> error
CLC
ADC G2       ; G2 += digit
STA G2
LDA G2+1
ADC #0
STA G2+1
INY
CPY G1       ; all chars done?
BNE @next
; success
PLX          ; restore X
LDA G2
STA 4,X
LDA G2+1
STA 5,X
TXA          ; non-zero flag
STA 2,X
STA 3,X
JMP NEXT
@err:        ; error
PLX
STZ 3,X      ; return 0 0
STZ 2,X
JMP NEXT
```

## mulG2xBASE -- (internal)
Multiply G2 by the base value in X register. Uses optimized shift sequences for common bases.

```
mulG2xBASE:
CPX #$10    ; hex: shift left 4 times (= *16)
BEQ @base16
CPX #$0A    ; decimal: separate handler
BEQ @base10
CPX #$02    ; binary: shift left once (= *2)
BEQ @base2
CPX #$08    ; octal: shift left 3 times (= *8)
BEQ @base8

@base16:    ; *16 = <<4
ASL G2
ROL G2+1
@base8:     ; *8 = <<3 (fall through from *16)
ASL G2
ROL G2+1
ASL G2
ROL G2+1
@base2:     ; *2 = <<1
ASL G2
ROL G2+1
RTS

@base10:    ; *10 = (*2) + (*8) = (<<1) + (<<3)
ASL G2
ROL G2+1
; save *2 in BCD
LDA G2
STA BCD
LDA G2+1
STA BCD+1
; *8 (shift left 3 more from *2 = total *16... wait, shift from original)
ASL G2
ROL G2+1
ASL G2
ROL G2+1
; G2 is now *8, add *2 saved in BCD
CLC
LDA BCD
ADC G2
STA G2
LDA BCD+1
ADC G2+1
STA G2+1
RTS
```

**Algorithm for *10:** `n * 10 = n * 2 + n * 8 = (n << 1) + (n << 3)`
- Shift left once to get n*2, save in BCD
- Shift from n*2 left twice more to get n*8 (3 total from original)
- Add BCD (n*2) to G2 (n*8) = n*10

## nibble_asc_to_value -- (internal)
Convert an ASCII hex character to a nibble value (0-15). Returns CLC on success, SEC on error.

```
CMP #'0'     ; below '0'?
BMI @err
CMP #'F'+1   ; above 'F'?
BPL @err
CMP #'9'+1   ; is 0-9?
BMI @conv
CMP #'A'     ; is A-F?
BPL @conv
@err:        ; invalid
SEC
RTS
@conv:
CMP #$41     ; A-F?
BMI @less
SBC #$37     ; convert 'A' to $0A
@less:
AND #$0F     ; mask nibble
CLC
RTS
```
