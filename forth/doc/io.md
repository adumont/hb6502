# I/O and Parsing

## KEY ( -- char )
Get the next character from the input buffer. If the buffer is exhausted, refills it by calling `getline`.

```
_KEY:
@retry:
LDY INP_IDX
CPY INP_LEN       ; reached end?
BEQ @eos
LDA INPUT,Y       ; read char
INC INP_IDX
RTS
@eos:
JSR getline       ; refill buffer
BRA @retry
```

The Forth word `KEY` wraps this in a stack push:
```
JSR _KEY
STA 0,X
STZ 1,X
JMP DEX2_NEXT
```

## GETC ( -- char )
Get a character directly from the hardware I/O (not the input buffer). Blocks until a character is available.

```
JSR getc
STA 0,X
STZ 1,X
JMP DEX2_NEXT
```

`getc` on the emulator reads from `IO_AREA+4` ($F004). Returns 0 if no char available.

## EMIT ( char -- )
Send a single character to the output device.

```
LDA 2,X
JSR putc
JMP do_DROP
```

`putc` on the emulator writes to `IO_AREA+1` ($F001).

## CR ( -- )
Print a carriage return (`$0A`) and line feed (`$0D`).

```
_crlf:
LDA #$0A
JSR putc
LDA #$0D
JMP putc
```

## SPACE ( -- )
Print a space character (`$20`).

```
LDA #' '
JSR putc
JMP NEXT
```

## TYPE ( addr len -- )
Print a string of `len` characters starting at `addr`.

```
LDA 2,X     ; len (1 byte)
BEQ @exit   ; zero length -> exit
STA G1      ; save length
LDY #0
LDA 4,X     ; addr -> W
STA W
LDA 5,X
STA W+1
@loop:
LDA (W),y  ; read char
JSR putc   ; print it
INY
CPY G1     ; done?
BNE @loop
@exit:
INX
INX
JMP do_DROP
```

## . ( n -- )
Print a 16-bit value as 4 hex digits followed by a space.

```
LDA 3,X     ; HI byte
JSR print_byte
cprint:     ; also entry point for C.
LDA 2,X     ; LO byte
JSR print_byte
LDA #' '
JSR putc
JMP do_DROP
```

**print_byte** converts a byte to 2 hex nibbles:
```
print_byte:
TAY          ; save for LO nibble
LSR          ; HI nibble
LSR
LSR
LSR
JSR print_nibble
TYA
AND #$0F     ; LO nibble
; fall through to print_nibble
```

**print_nibble** converts a nibble (0-15) to ASCII hex:
```
print_nibble:
CMP #$0A
BCC @skip
ADC #$66     ; adjust A-F to ASCII
@skip:
EOR #$30     ; convert 0-9 to ASCII
JMP putc
```

## C. ( char -- )
Print a single byte (2 hex digits) followed by a space.

```
LDA 2,X
JSR print_byte   ; prints HI nibble + LO nibble
LDA #' '
JSR putc
JMP do_DROP
```

## D. ( lo hi -- )
Print a 32-bit double value as 8 hex digits.

```
LDA 3,X     ; hi word HI byte
JSR print_byte
LDA 2,X     ; hi word LO byte
JSR print_byte
INX         ; drop hi word
INX
JMP do_PRINT ; fall through to "." (prints lo word)
```

## WORD ( -- addr len )
Skip leading delimiters (space = `$20`), then read characters until the delimiter, newline, or `\` comment. Returns the address and length of the token.

```
lda #' '
bra _parse
```

Shared parsing code with PARSE:

**_parse:**
1. Skip all leading delimiter characters
2. Return `0 0` if newline encountered immediately
3. Skip `\` comments to end of line
4. Copy characters to INPUT buffer starting from current position
5. Return (addr, len)

The token address points into the INPUT buffer.

## PARSE ( sep -- addr len )
Like WORD but uses the delimiter from the stack instead of space.

```
lda 2,x     ; get separator from stack
inx
inx
_parse:     ; shared with WORD
```

## INPUT ( -- )
Read a line of input from the user. Called to refill the input buffer.

```
JSR getline
JMP NEXT
```

**getline** (internal subroutine):
1. Resets `INP_IDX` to 0
2. If in boot mode (`BOOT ≠ 0`), calls `boot_refill` instead
3. Loops reading characters via `getc`:
   - `CTRL-D` ($04): trigger BRK
   - Backspace (`$08` or `$7F`): erase last character
   - Newline/CR: end input
   - Stores each character in INPUT buffer, echoes via `putc`
4. Sets `INP_LEN` to the number of characters read

**boot_refill**: Reads one token at a time from the bootstrap code block (`BOOT_PRG`), copying it into the INPUT buffer. Stops at `$00` (end of bootstrap), clearing the BOOT flag to enter interactive mode.

## S( ( -- addr )
Read a string from input until `)` is encountered. The string is stored as a counted string.

In execute mode: stored temporarily at HERE+$FF, returns counted string address.
In compile mode: compiles a `LITSTR` with the string embedded in the word.

```
S( Hello) COUNT TYPE  prints "Hello"
```

Compiles to (in a definition):
```
LITSTR "Hello"
COUNT
TYPE
```
