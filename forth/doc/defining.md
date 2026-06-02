# Defining Words

## : ( -- )
Create a new word header, enter compilation mode. Takes the next word from input as the name.

**Implementation:** The `FCOLON` word is the actual `:`:

```
: FCOLON CODE  *COMMIT_JMP COMPILE COLON RBRAC ;
```

1. `CODE` creates a header for the next word in input
2. `*COMMIT_JMP` compiles a `JMP $4C` opcode
3. `COMPILE COLON` compiles `do_COLON` address
4. `RBRAC` sets MODE to 0 (compile mode)

## ; ( -- )
End a word definition, compile EXIT, return to execute mode.

**Implementation:** The `SEMICOLON` word is the actual `;`:

```
: SEMICOLON REVEAL COMPILE EXIT LBRAC ;
```

1. `REVEAL` clears the HIDDEN flag on the word
2. `COMPILE EXIT` compiles `do_EXIT` address
3. `LBRAC` sets MODE to 1 (execute mode)

## :NONAME ( -- xt )
Create an anonymous word (no header, not in dictionary). Leaves its CFA on the stack.

```
: NONAME  HERE  *COMMIT_JMP COMPILE COLON ;
```

`HERE` leaves the future word's address on the stack. Then it compiles `JMP` and `do_COLON` like a regular `:`. The word has no name and can't be found by `FIND`.

## CREATE ( -- )
Create an empty word header. Unlike `:`, CREATE does NOT enter compile mode. It creates a header with a JMP to do_COLON, but the code field contains `CREATED` which leaves the PFA on the stack and returns.

```
: CREATE  CODE *COMMIT_JMP COMPILE COLON COMPILE CREATED COMPILE do_SEMI REVEAL ;
```

1. `CODE` creates a header
2. `*COMMIT_JMP` compiles JMP
3. `COMPILE COLON` compiles do_COLON
4. `COMPILE CREATED` compiles the *CREATED subroutine
5. `COMPILE do_SEMI` compiles EXIT as default action (can be patched by DOES>)
6. `REVEAL` reveals the word

### *CREATED

The runtime action for created words: takes the address of the cell after the JMP from the return stack, adds 2 to get the PFA, then fetches the DOES> action address and pushes it to the return stack.

```
: *CREATED  R> DUP 2+ SWAP @ >R ;
```

## DOES> ( -- )
Patch the last defined word so its CREATED action jumps to the DOES> clause.

```
: DOES>  LAST >CFA 5 +  R>  SWAP ! ;
```

1. `LAST >CFA` gets the CFA of the last word
2. `5 +` advances past JMP + COLON + CREATED (5 bytes) to point at the action address cell
3. `R>` gets the address of the next word (the DOES> clause start)
4. `SWAP !` stores it, patching the created word

When the created word executes, *CREATED will now jump to the DOES> clause instead of EXIT.

## VARIABLE ( -- )
Create a new variable. Like CREATE but also allocates one cell (2 bytes).

```
: VARIABLE  CODE *COMMIT_JMP COMPILE VAR  HERE++ REVEAL ;
```

1. `CODE` creates a header
2. `*COMMIT_JMP` compiles JMP
3. `COMPILE VAR` compiles `do_VAR` which pushes the PFA address
4. `HERE++` allocates 2 bytes of storage (initialized to 0 by default)
5. `REVEAL` reveals the word

## MARKER ( -- )
Create a "FORGET" word. When later executed, it restores LATEST and HERE to their values at the time MARKER was called, effectively forgetting all words defined since then.

```
: MARKER  HERE LAST  S( FORGET) COUNT  *HEADER *COMMIT_JMP
  COMPILE COLON
  LITERAL  COMPILE LATEST COMPILE !
  LITERAL  COMPILE DP COMPILE !
  COMPILE EXIT
  REVEAL ;
```

1. Creates a header named "FORGET"
2. Encodes the current LATEST and HERE values as literals, followed by `LATEST ! DP !`
3. When FORGET executes, it sets LATEST and HERE back to those saved values

## CODE ( -- )
Create a word header from the next token in input. Used as the foundation for defining words.

```
: CODE  WORD *HEADER ;
```

`*HEADER` does the heavy lifting:
1. `HERE` saves current DP
2. `LAST ,` compiles the previous LATEST as the link field
3. `LATEST !` updates LATEST
4. Creates the name field with HIDDEN flag set
5. Allocates space for the name

## ;CODE ( -- )
Used after CODE to reveal the word and compile a JMP NEXT, creating an assembly-language primitive.

```
: ;CODE  REVEAL *COMMIT_JMP COMPILE NEXT ;
```

## Header structure

Each word in the dictionary has this layout:

```
LFA (2 bytes)  — Link Field: address of previous word (linked list)
NFA (1 byte)   — Name Field: length byte (bits 7-6: flags, bits 4-0: length)
                 Bit 7: IMMEDIATE flag
                 Bit 6: HIDDEN flag
NFA (n bytes)  — Name characters
CFA (2+ bytes) — Code Field: begins with JMP $4C to word's code
PFA (variable) — Parameter Field (used by VARIABLE, CREATE'd words)
```

### Header macros

`defword label, name, flags` — Creates a full header + code field:

```
.macro defword label, name, flags
  h_{label}:                    ; header label
  __word_{n}:                   ; word number label
  .addr __word_{n-1}            ; link to previous word
  CString name, flags           ; counted string name
  do_{label}:                   ; entry point label
.endmacro
```

`noheader label` — Creates just an entry point (no dictionary entry):

```
.macro noheader label
  do_{label}:                   ; just the label
.endmacro
```

## REVEAL ( -- )
Clear the HIDDEN flag on the latest word. Called by `;` and `;CODE`.

```
JSR _LenOfLastWord
AND #($FF-HIDDEN_FLAG)
STA (W),Y
JMP NEXT
```

## HIDDEN ( -- )
Set the HIDDEN flag on the latest word.

```
JSR _LenOfLastWord
ORA #HIDDEN_FLAG
STA (W),Y
JMP NEXT
```

## HIDE ( hdr -- )
Hide a specific word by its header address (as returned by FIND).

```
JSR _getWordLen
ORA #HIDDEN_FLAG
STA (W),Y
JMP do_DROP
```

## UNHIDE ( hdr -- )
Unhide a specific word by its header address.

```
JSR _getWordLen
AND #($FF-HIDDEN_FLAG)
STA (W),Y
JMP do_DROP
```

## SETIMM ( hdr -- )
Set the IMMEDIATE flag on a word by its header address.

```
JSR _getWordLen
ORA #IMMEDIATE_FLAG
STA (W),Y
JMP do_DROP
```

## GETIMM ( hdr -- flag )
Return 0 if the word is IMMEDIATE, non-zero otherwise.

```
JSR _getWordLen
STZ 3,X     ; clear HI
BMI @isImm
STA 2,X     ; not immediate: return length (non-zero)
JMP NEXT
@isImm:
STZ 2,X     ; immediate: return 0
JMP NEXT
```

**Note:** The IMMEDIATE check is done via the MSB of the length byte.
