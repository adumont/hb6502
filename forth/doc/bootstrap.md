# Bootstrap Words

Words defined in `bootstrap.f`. These are compiled by the cross-compiler (during stage 2) and become part of the ROM dictionary.

## Basic Utilities

### UNDEF ( -- )
Undefine the last word in the dictionary. Sets LATEST to the previous word's link (LFA).

```
: UNDEF  LAST @ LATEST ! ;
```

### ? ( addr -- )
Print the value at an address (fetch and print). Shorthand for `@ .`.

```
: ?  @ . ;
```

### = ( n1 n2 -- flag )
Signed equality test.

```
: =  - 0= ;
```

### NEG ( n -- -n )
Two's complement negation.

```
: NEG  NOT 1+ ;
```

### IMMEDIATE ( -- )
Mark the most recently defined word as IMMEDIATE.

```
: IMMEDIATE  LAST SETIMM ;
```

### ' ( -- xt )
Find the next word in input and return its CFA (execution token).

```
: '  WORD FIND >CFA ;
```

### ['] ( -- xt )
Like `'` but compiles the CFA as a literal (for use inside definitions).

```
: [']  ' LITERAL ; IMMEDIATE
```

### [,] ( xt -- )
Immediate version of `,` (comma). Takes an XT from the stack and commits it to the dictionary.

```
: [,]  , ; IMMEDIATE
```

### POSTPONE ( -- )
Compile the compilation semantics of the next word. Used to write defining words that compile other words.

```
: POSTPONE  ' , ; IMMEDIATE
```

### +! ( n addr -- )
Add n to the value stored at addr (like `@ + !`).

```
: +!  SWAP OVER @ + SWAP ! ;
```

## Comparator operators

### < ( n1 n2 -- flag )
Signed less-than.

```
: <  - 0< ;
```

### > ( n1 n2 -- flag )
Signed greater-than.

```
: >  SWAP < ;
```

### <= ( n1 n2 -- flag )
Signed less-or-equal.

```
: <=  > 0= ;
```

### >= ( n1 n2 -- flag )
Signed greater-or-equal.

```
: >=  SWAP <= ;
```

### D< ( d1 d2 -- flag )
Signed double less-than. Subtracts and checks sign of result.

```
: D<  D- NIP 0< ;
```

## Branch helpers

### JUMP> ( -- addr )
Leave a placeholder address for a forward jump/branch. Equivalent to `HERE 0 ,`.

```
: JUMP>  HERE HERE++ ;
```

### >HERE ( addr -- )
Fill a placeholder (left by JUMP>) with the current HERE value.

```
: >HERE  HERE SWAP ! ;
```

## Control structures (IMMEDIATE)

These are documented in detail in [loops.md](loops.md). Quick reference:

```
: IF       COMPILE 0BR JUMP> ; IMMEDIATE
: ELSE     COMPILE JUMP JUMP> SWAP >HERE ; IMMEDIATE
: THEN     >HERE ; IMMEDIATE
: BEGIN    HERE ; IMMEDIATE
: AGAIN    COMPILE JUMP , ; IMMEDIATE
: UNTIL    COMPILE 0BR , ; IMMEDIATE
: WHILE    POSTPONE IF ; IMMEDIATE
: REPEAT   COMPILE JUMP SWAP , >HERE ; IMMEDIATE
```

### ( -- )
Inline comment. Reads characters from input until `)` is found.

```
: (  BEGIN KEY 29 = UNTIL ; IMMEDIATE
```
(Character 29 = `)`)

## Stack and data

### NIP ( x1 x2 -- x2 )
Drop the second item on the stack.

```
: NIP  SWAP DROP ;
```

### PICK ( n -- x )
Copy the nth item (0-indexed) from the data stack.

```
: PICK  1+ 2* SP@ + @ ;
```

### DEPTH ( -- n )
Return the number of items on the data stack.

```
: DEPTH  DTOP SP@ - 2/ ;
```

### UM+ ( u1 u2 -- ud )
Unsigned 16-bit addition returning a 17-bit result as a double.

```
: UM+  0 SWAP 0 D+ ;
```

### >D ( n -- d )
Extend a signed single to a signed double. Sign-extends: positive values become 0, negative become $FFFF.

```
: >D  DUP 0< 0= 0= ;
```

(The `0= 0=` converts any nonzero to $FFFF, zero to 0.)

### M+ ( d n -- d )
Add a signed single to a signed double.

```
: M+  >D D+ ;
```

### DNEG ( d -- -d )
Negate a signed double (two's complement).

```
: DNEG  SWAP NOT SWAP NOT 1 0 D+ ;
```

### * ( n1 n2 -- n )
Signed 16-bit multiply (low 16 bits of result). Uses UM* and drops the high word.

```
: *  UM* DROP ;
```

## Division

### /MOD ( n d -- rem quot )
Signed divide n by d, return remainder and quotient.

```
: /MOD  SWAP 0 ROT UM/MOD ;
```

### MOD ( n d -- rem )
Return remainder of n / d.

```
: MOD  /MOD DROP ;
```

### / ( n d -- quot )
Return quotient of n / d.

```
: /  /MOD NIP ;
```

## Number display

### U. ( u -- )
Print unsigned 16-bit number in the current BASE. Recursive: divides by BASE, prints quotient first, then remainder.

```
: U.
   RECURSIVE
   BASE C@ /MOD
   ?DUP IF U. THEN
   DUP A < IF
       30       ( digits 0-9 )
   ELSE
       A - 41   ( letters A-Z )
   THEN
   + EMIT
;
```

### . ( n -- )
Print signed 16-bit number. If SIGNED variable is set, prints negative sign for negative numbers. In base 10, uses the printer word `.` (for hex output, uses U.).

```
: .
   SIGNED IF
     DUP 0< IF CHAR - EMIT NEG THEN
   THEN
   10 BASE C@ = IF
     .           ( calls assembler PRINT for hex display )
   ELSE
     U. SPACE
   THEN
;
```

## Display formatting

### S. ( n -- )
Print a signed number with sign character.

```
: S.  DUP 0< IF 2D EMIT NEG THEN . ;
```

### DS. ( d -- )
Print a signed double number with sign character.

```
: DS.  DUP 0< IF 2D EMIT DNEG THEN D. ;
```

## Defining words

### VALUE ( n -- )
Create a variable initialized to n, whose execution returns the value (not the address).

```
: VALUE  CREATE , DOES> @ ;
```

**Example:** `5 VALUE FIVE` creates a word `FIVE` that returns 5.

### CONSTANT ( n -- )
Same as VALUE. Defined as:

```
: CONSTANT  VALUE ;
```

### TO ( -- )
Set the value of a VALUE (or any variable). At runtime stores a value into the variable found by name.

```
: TO  ' 7 + ?EXEC IF ! ELSE LITERAL COMPILE ! THEN ; IMMEDIATE
```

**Example:** `10 TO FIVE` sets FIVE to 10.

### DEFER ( -- )
Create a deferred word (a placeholder that can be assigned later).

```
: DEFER  CREATE 0 , DOES> @ EXEC ;
```

**Example:** `DEFER HOOK` creates HOOK. Later, `' SOMEWORD IS HOOK` assigns it.

### IS ( xt -- )
Assign a word to a DEFER. IMMEDIATE version of TO for DEFERs.

```
: IS  POSTPONE TO ; IMMEDIATE
```

## Heap / locals

### HEAP ( n -- )
Allocate n bytes in the heap (top of RAM). The heap grows downward from BP.

```
: HEAP
   BP @ DUP ROT 2+ -
   DUP -ROT ! BP !
;
```

### 'HEAP ( -- addr )
Return the start address of the current heap frame's data area (skipping the 2-byte link field).

```
: 'HEAP  BP @ 2+ ;
```

### -HEAP ( -- )
Deallocate the current heap frame by restoring BP from the link pointer.

```
: -HEAP  BP DUP @ @ SWAP ! ;
```

### LOCALS ( n -- )
Allocate n cells for local variables.

```
: LOCALS  2* HEAP ;
```

### -LOCALS ( -- )
Deallocate local variables.

```
: -LOCALS  -HEAP ;
```

### L@ ( n -- value )
Defining word: creates a named local getter.

```
: L@  CREATE C, DOES> C@ BP @ + @ ;
```

### L! ( n -- value )
Defining word: creates a named local setter.

```
: L!  CREATE C, DOES> C@ BP @ + ! ;
```

Pre-defined locals: `x y z t` with getters (`x y z t`) and setters (`x! y! z! t!`).

## Debugging

### DUMP ( addr1 addr2 -- )
Dump memory contents from addr1 to addr2 as hex bytes.

```
: DUMP  SWAP DUP . ?DO I C@ C. LOOP ;
```

### .LDUMP ( -- )
Dump the local stack (from BP to $4000).

```
: .LDUMP  BP @ 4000 DUMP ;
```

### WORDS ( -- )
List all words in the dictionary. Shows address, CFA, name, and whether it's IMMEDIATE (*). Pauses every 10 words.

```
: WORDS
   0 LATEST
   BEGIN @ DUP WHILE
     DUP . DUP >CFA . .NAME CR
     SWAP 1+ DUP 10 = IF
       GETC 20 OR 71 = IF
         2DROP EXIT
       THEN DROP 0
     THEN SWAP
   REPEAT 2DROP
;
```

### SEE ( -- )
Decompile a Forth word. Takes the name from input, shows addresses, XTs, and word names.

```
: SEE  WORD FIND >CFA (SEE) ;
```

The `(SEE)` worker decompiles from the CFA until EXIT.

## Printing

### .( ( -- )
Print a string during compilation (immediate word). Like `S(` but executes TYPE immediately.

```
: .(  [ ' S( , ] ?EXEC IF TYPE ELSE COMPILE TYPE THEN ; IMMEDIATE
```

### CHAR ( -- char )
Read the next character from input and either execute immediately or compile as a literal.

```
: CHAR  KEY ?EXEC IF EXIT THEN LITERAL ; IMMEDIATE
```

## Decompiler helpers

### >HDR ( xt -- hdr/0 )
Given an XT (execution token), find the corresponding header address. Returns 0 if not found.

```
: >HDR
   LATEST
   BEGIN
     @ DUP
     DUP IF
       >CFA 2 PICK =
     ELSE
       0=
     THEN
   UNTIL
   NIP
;
```

### >NAME ( xt -- )
Print the name of the word at the given XT, or the hex address if not found.

```
: >NAME
   DUP >HDR
   ?DUP IF .NAME 2DROP ELSE . THEN
;
```

## Memory display

### FREE ( -- )
Print the number of free bytes between HERE and BP.

```
: FREE  BASE C@ BP @ 2+ HERE - DEC . .( BYTES FREE) CR BASE C! ;
```

### .S ( -- )
Print the entire data stack without modifying it.

```
: .S  DEPTH DUP IF 1+ DUP 1 DO DUP I - PICK . LOOP CR THEN DROP ;
```

## System utilities

### RECURSIVE ( -- )
Reveal the latest word (used at the start of a recursive definition to allow self-reference).

```
: RECURSIVE  REVEAL ; IMMEDIATE
```

### /MOD ( n d -- rem quot )
Already documented above. Also: `MOD` and `/`.

### MARKER ( -- )
Runs at the end of bootstrap.f to create the `FORGET` word. After MARKER, calling `FORGET` restores LATEST and HERE to the values they had at MARKER time, erasing all words defined afterwards.
