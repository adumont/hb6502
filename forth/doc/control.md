# Control Flow Primitives

## LIT ( -- x )
Push the next 16-bit literal from the threaded code onto the stack. The literal is embedded immediately after the LIT address in the threaded word. IP is advanced past the literal.

```
LDA (IP)    ; read LO from code stream
STA 0,X
LDY #1
LDA (IP),y  ; read HI from code stream
STA 1,X
CLC         ; advance IP by 2
LDA IP
ADC #2
STA IP
BCC @skip
INC IP+1
@skip:
JMP DEX2_NEXT
```

## CLIT ( -- char )
Push the next 8-bit literal from the threaded code. The byte is embedded right after CLIT. HI byte of result is zero. IP advances by 1.

```
LDA (IP)    ; read byte from code stream
STA 0,X
STZ 1,X     ; HI = 0
INC IP      ; advance IP by 1
BNE @skip
INC IP+1
@skip:
JMP DEX2_NEXT
```

## LITSTR ( -- addr )
Push the address of a counted string embedded in the code stream. The string length is at (IP), followed by the string bytes. IP advances past the string and the count byte.

```
LDA IP
STA 0,X
LDY #1
LDA IP+1
STA 1,X
LDA (IP)    ; string length
INA         ; +1 for the count byte itself
CLC
ADC IP
STA IP
BCC @skip
INC IP+1
@skip:
JMP DEX2_NEXT
```

Compiled by `S(` and compiled versions of `S"`.

## JUMP ( -- )
Unconditional branch. Fetches the next 2 bytes from the code stream and sets IP to that address. Effectively a `goto` for threaded code.

```
LDY #1
LDA (IP),y  ; read HI byte of target addr
TAY
LDA (IP)    ; read LO byte
STA IP
TYA
STA IP+1
JMP NEXT
```

**Note:** The target address is the 2 bytes immediately following JUMP in the code stream. After execution, IP points to the target, and NEXT will fetch the next word from there.

## 0BR ( flag -- )
Conditional branch. If flag is zero, jump to the address in the next 2 bytes (like JUMP). If non-zero, skip over those 2 bytes and continue.

```
LDA 2,X     ; check if flag == 0
ORA 3,X
BNE @not0
INX         ; drop flag
INX
BRA do_JUMP ; jump to target (reuses JUMP code)
@not0:
CLC         ; skip over the 2-byte target address
LDA IP
ADC #2
STA IP
BCC @skip
INC IP+1
@skip:
JMP do_DROP ; drop flag and continue
```

## COMPILE ( addr -- )
Compile a word address into the dictionary. Takes the CFA/XT from the stack and writes it at HERE, then advances HERE by 2. Also known as `LIT,` (literal compile).

```
LDY #1
LDA (IP)    ; read the address to compile from code stream
STA (DP)    ; store at HERE
LDA (IP),y
STA (DP),y
CLC         ; advance IP past the address
LDA IP
ADC #2
STA IP
BCC @skip2
INC IP+1
@skip2:
CLC         ; advance HERE by 2
LDA DP
ADC #2
STA DP
BCC @skip
INC DP+1
@skip:
JMP NEXT
```

**Note:** This is the primitive that implements `,` (comma) for compilation. Unlike the regular `,` which takes the value from the stack, COMPILE reads it from the code stream. This is what makes compiling words like `LIT` possible: `COMPILE LIT` will embed the address of `do_LIT` into the word being defined.

## COLON ( -- )
Push IP onto the return stack, then set IP to `W+3`, starting execution of the word body. Used at the start of every threaded definition.

```
LDA IP+1    ; push IP (HI then LO) to return stack
PHA
LDA IP
PHA
CLC         ; W+3 -> IP (skip the JMP opcode at the start)
LDA W
ADC #3
STA IP
LDA W+1
ADC #0
STA IP+1
JMP NEXT
```

**Note:** Words defined with the `defword` macro always start with a `JMP do_COLON` (opcode $4C). The 3-byte JMP is skipped to reach the actual threaded code body. The return address on the R-stack allows EXIT to return to the caller.

## EXIT ( -- )
Pop IP from the return stack, returning execution to the caller. Used at the end of a threaded word (like `;` in Forth).

```
PLA         ; pop IP (LO then HI) from return stack
STA IP
PLA
STA IP+1
JMP NEXT
```
