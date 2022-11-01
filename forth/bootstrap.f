\ Alex FORTH for 6502
\ Copyright (C) 2021-2022 Alexandre Dumont <adumont@gmail.com>
\ SPDX-License-Identifier: GPL-3.0-only

\ DON'T REMOVE THIS CODE block
>ROM
HERE 1 !        \ we save the start of ROM area at 0001
>RAM
HERE 5 !        \ we save the start of RAM area at 0005
>ROM
\ ----------------------------

: UNDEF LAST @ LATEST ! ; \ undefine last word

: ? @ . ;
: = - 0= ;
: NEG NOT 1+ ; \ ( N -- -N ) Negate N (returns -N)
: IMMEDIATE LAST SETIMM ; \ ; sets the latest word IMMEDIATE
: ' WORD FIND >CFA ;
: ['] ' LITERAL ; IMMEDIATE
: [,] , ; IMMEDIATE \ take an XT on ToS, commit to dict
: POSTPONE ' , ; IMMEDIATE

: +! SWAP OVER @ + SWAP ! ;

\ Comparison operators ( a b -- f ) for signed cells
: < - 0< ;
: > SWAP < ;
: <= > 0= ;
: >= SWAP <= ;

: JUMP> HERE HERE++ ; \ Put HERE on the stack, and increment leaving an unfilled placeholder for a forward branch/jump, to be filled by >HERE. Equivalent to " HERE 0 , "
: >HERE HERE SWAP ! ; \ Store current HERE in the jump/branch placeholder earlier left by JUMP>

\ condition IF do-this ELSE do-that THEN
: IF COMPILE 0BR JUMP> ; IMMEDIATE
: ELSE COMPILE JUMP JUMP> SWAP >HERE ; IMMEDIATE
: THEN >HERE ; IMMEDIATE

\ BEGIN do-stuff AGAIN
: BEGIN HERE ; IMMEDIATE \ HERE leaves the address for a backward branch/jump
: AGAIN COMPILE JUMP , ; IMMEDIATE \ unconditional backward jump

\ BEGIN do-stuff condition UNTIL
: UNTIL COMPILE 0BR , ; IMMEDIATE \ conditional backward jump

\ BEGIN condition WHILE do-stuff REPEAT
: WHILE POSTPONE IF ; IMMEDIATE
: REPEAT COMPILE JUMP SWAP , >HERE ; IMMEDIATE

\ Inline comments ( )
: ( BEGIN KEY 29 = UNTIL ; IMMEDIATE \ ; now we can use ( ) inline comments!

: >D DUP 0< 0= 0= ; \ ; Extends signed cell into signed double (0= 0= will convert any non 0 into FFFF)

: UM+ 0 SWAP 0 D+ ;

: NIP SWAP DROP ;
: TUCK ( a b -- b a b ) SWAP OVER ;
: PICK 1+ 2* SP@ + @ ;
: DEPTH DTOP SP@ - 2/ ;

: MIN ( a b -- min ) 2DUP < IF DROP ELSE NIP THEN ;
: MAX ( a b -- max ) 2DUP < IF NIP ELSE DROP THEN ;

: 2SWAP >R -ROT R> -ROT ;
: 2ROT >R >R 2SWAP R> R> 2SWAP ;
: 2>R R> -ROT SWAP >R >R >R ;
: 2R> R> R> R> SWAP ROT >R ;

: D< D- NIP 0< ; \ Signed double less
: M+ >D D+ ;

: DNEG SWAP NOT SWAP NOT 1 0 D+ ;

: S. DUP 0< IF 2D EMIT NEG THEN . ;
: DS. DUP 0< IF 2D EMIT DNEG THEN D. ;

: * UM* DROP ;

: VALUE CREATE , DOES> @ ;
: CONSTANT VALUE ;
: TO ' 7 + ?EXEC IF ! ELSE COMPILE LIT , COMPILE ! THEN ; IMMEDIATE
: DEFER CREATE 0 , DOES> @ EXEC ;
: IS POSTPONE TO ; IMMEDIATE

>RAM

VARIABLE BP \ ; defines BP variable (Base Pointer)
_BP BP !

0 VALUE SIGNED \ change to non-0 for signed output

>ROM

\ Allocates a new n-bytes frame in the heap (top of RAM)
\ first cell is a pointer to the previous frame
\ next n-bytes are allocated for user data
: HEAP ( n -- )
  BP @ DUP ROT 2+ -
  DUP -ROT ! BP !
;

: 'HEAP BP @ 2+ ; \ returns the address of the start of the heap frame's data
: -HEAP BP DUP @ @ SWAP ! ;

: LOCALS 2* HEAP ; \ ( n -- ) Allocates cells for n local variables
: -LOCALS -HEAP ;  \ Deallocates local variables

: L@ CREATE C, DOES> C@ BP @ + @ ; \  ; ( n -- value) defining word to get local var n
: L! CREATE C, DOES> C@ BP @ + ! ; \  ; ( n -- value) defining word to save to local var n

2 L@ x  2 L! x!
4 L@ y  4 L! y!
6 L@ z  6 L! z!
8 L@ t  8 L! t!

: RECURSIVE REVEAL ; IMMEDIATE

: DUMP SWAP DUP . ?DO I C@ C. LOOP ; \ ( addr1 addr2 -- ) dumps memory from addr1 to addr2
: HDUMP BP @ DUP 2+ SWAP @ DUMP ; \ dump the heap/locals area (1 frame, only data, not the frame pointer)

: .NAME DUP 2+ DUP C@ DUP 40 AND >R 1F AND SWAP
  1+ SWAP TYPE R> IF SPACE 2A EMIT THEN ;
: WORDS 0 LATEST BEGIN @ DUP WHILE DUP . DUP >CFA .
  .NAME CR SWAP 1+ DUP 10 = IF GETC 20 OR 71 =
  IF 2DROP EXIT THEN DROP 0 THEN SWAP REPEAT 2DROP ;

: .( [ ' S( , ] ?EXEC IF TYPE ELSE COMPILE TYPE THEN ; IMMEDIATE

: STRING CREATE HERE -ROT 1+ DUP ALLOT SWAP 1 - -ROT CMOVE ; \ Example: S( Alex) STRING NAME
: CHAR KEY ?EXEC IF EXIT THEN LITERAL ; IMMEDIATE \ Example: CHAR " EMIT

: >HDR ( XT -- HDR/0 ) \ 0 if not found
  LATEST
  BEGIN
    @ DUP
    DUP
    IF
      >CFA
      2 PICK
      =
    ELSE
      0= \ force exit leaving FALSE on ToS
    THEN
  UNTIL
  NIP
;

\ Print the name or the ADDR if not found
: >NAME ( XT -- )
  DUP >HDR
  ?DUP IF
    .NAME 2DROP
  ELSE
    .
  THEN
;

\ Call with SEE WORDNAME , for example SEE T
: SEE
  2 LOCALS \ x is pointer to XT, y is XT
  WORD FIND >CFA \ get CFA of name to decompile
  \  x . S( : ) TYPE x >NAME CR
  1+ x! \ skip 4C JMP  // TODO: what if not a colon word??
  BEGIN
    x .
    x @ y!
    y .
    y >NAME
    x 2+ x!
    y ['] CLIT = IF
      SPACE x C@ C. CR
      x 1+ x!
    ELSE
      y ['] LITSTR = IF
      x DUP COUNT DUP -ROT
      SPACE CHAR " EMIT TYPE CHAR " EMIT CR
      1+ + x!
      ELSE
        CR
      THEN
    THEN
    y ['] EXIT =
  UNTIL
  -LOCALS
;

\ Base Numbers

\ Only positive numbers ( 0 instead of >D for signed doubles... problem with FFFF)
: /MOD SWAP 0 ROT UM/MOD ;

: MOD /MOD DROP ;
: / /MOD NIP ;

: U. ( u -- )
   RECURSIVE
   \ BREAK
   BASE C@ /MOD    ( width rem quot )
   ?DUP IF        ( if quotient <> 0 then )
      U.          ( print the quotient )
   THEN

   ( print the remainder )
   DUP A < IF
      30    ( decimal digits 0..9 )
   ELSE
      A -    ( hex and beyond digits A..Z )
      41
   THEN
   +
   EMIT
;

: .
  SIGNED IF
    DUP 0< IF
      CHAR - EMIT
      NEG
    THEN
  THEN

  10 BASE C@ = IF
    .
  ELSE
    U. SPACE
  THEN
;

: FREE BASE C@ BP @ 2+ HERE - DEC . .( BYTES FREE) CR BASE C! ;

: .S DEPTH DUP IF 1+ DUP 1 DO DUP I - PICK . LOOP CR THEN DROP ;

\ FLOATS

\ Low level Float Register encoding, 7 bytes
\ 0  1  2  3   4   5   6   7
\ [  :  :  :  ][  ][  ][  ]
\  \_________/   \   \   +--> exponent
\            \    \   +-----> exponent's sign (1: negative)
\             \    +--> mantissa's sign (1: negative)
\              +--> mantissa 4 bytes (BCD)

: FREG CREATE 7 ALLOT ; \ 7 bytes per float register
: .MANT      ; IMMEDIATE \ Mantisa 4 bytes BCD (does nothing, compiles nothing)
: .SIGN 4 +  ; \ Sign 1 byte
: .EXPS 5 +  ; \ Exponent sign 1 byte
: .EXP  6 +  ; \ Exponent 1 byte

\ low level F! , should be renamed _F!
: F!
  3 LOCALS
  x! \ float structrure addr
  y!
  z!
  \ store sign
  z 0< IF 1 ELSE 0 THEN x .SIGN C!
  \ store exponent sign
  z 2* 0< IF 1 ELSE 0 THEN x .EXPS C!
  \ store exponent
  z <> %00111111 AND x .EXP C!
  \ store mantisa
  x .MANT
  z $FF AND
  OVER C!

  1+
  y <> OVER !

  0 SWAP 2+ C!
  -LOCALS
;

\ low level test Float equals to 0
: _F0= ( 'f -- flag )
  \ we only check if mantissa is 0
  .MANT DUP @
  SWAP 2+ @
  OR 0=
;

\ low level Fetch Float Signed Exp, return the signed exponent from a low level float register
\ TODO: rename to _FSEXP@
: FSEXP@ \ return the signed exponent of a float (8 bit 2's complement )
  ( 'float -- signed_exp )
  DUP .EXP C@ SWAP .EXPS C@ IF NEG THEN
;

\ low level Store Float Signed Exp, stores the signed exponent to a low level float register
\ TODO: rename to _FSEXP!
: FSEXP! \ stores the signed exponent back to a float reg
  ( signed_exp 'float -- )
  SWAP
  DUP 0< IF 1 SWAP NEG ELSE 0 SWAP THEN
  -ROT OVER .EXPS C! .EXP C!
;

\ low level F>>
: _F>> ( addr )
  DUP .MANT BCDSR
  \ increment exponent
  DUP FSEXP@ 1+ SWAP FSEXP!
;

: _F<< ( addr )
  DUP .MANT BCDSL
  \ increment exponent
  DUP FSEXP@ 1 - SWAP FSEXP!
;

\ low level "float align" (to max exp)
: _FALIGN ( x y -- ) \ too float register addr
  4 LOCALS
  y! x!

  \ if x or y are 0, we exit, nothing to align
  x _F0= y _F0= OR IF
    -LOCALS EXIT
  THEN

  x FSEXP@ z! \ x's signed exponent
  y FSEXP@ t! \ y's signed exponent
  z t = IF -LOCALS EXIT THEN
  z t < IF
    \ we need to raise x's exponent z to t
    t z -
    8 MIN
    0 DO x _F>> LOOP
  ELSE
    \ we need to lower x's exponent z to t
    z t -
    8 MIN
    0 DO y _F>> LOOP
  THEN
  -LOCALS
;

\ low level _F+
: _F+
  ( 'f1 'f2 -- ) \ leaves the result in 'f2
  2 LOCALS

  y! x!

  \ if x == 0, then result already in y, exit
  x _F0= IF
    -LOCALS EXIT
  THEN

  \ if y == 0, then result already is x, copy x to y
  y _F0= IF
    x y 7 CMOVE
    -LOCALS EXIT
  THEN

  \ align both floats (to max exponent)
  x y _FALIGN
  \ check the sign of both F1 and F2
  x .SIGN C@
  y .SIGN C@
  XOR IF
    \ F1 and F2 are different sign, we substract the mantissas

    \ find which one is bigger!
    x .MANT
    y .MANT
    FRM>? IF
      \ F1 is bigger
      x .MANT y .MANT FRM-
      \ deal with the carry - HOW
      DROP

      \ keep the sign of F1
      x .SIGN C@ y .SIGN C!
    ELSE
      \ F2 is bigger
      y .MANT x .MANT FRM-

      \ for some reason, result is in x, so we need to copy from x to y... ugly hack
      x .MANT y .MANT 4 CMOVE
      \ deal with the carry - HOW
      DROP

      \ keep the sign of F2
      \ FR2 .SIGN C@ FR2 .SIGN C! \ no need to do this
    THEN

    \ if y is not float 0
    y .MANT DUP @ SWAP 2+ @ OR
    IF
      \ we need to _F<< FR2 as long as 1rst digit is 0
      BEGIN
        y .MANT C@ F0 AND 0=
      WHILE
        y _F<<
      REPEAT

    ELSE \ y's mantissa is 0 --> reset y to 0
      0 0 y F!
    THEN

  ELSE
    \ F1 and F2 are same sign, we add the mantissas
    x .MANT y .MANT FRM+ ( carry )
    IF \ carry is 1?
      y _F>>
      y FRM1! \ store the carry in the mantissa
    THEN
  THEN
  -LOCALS
;

\ low level F@ , should be renamed _F@
: F@
  7 HEAP 'HEAP    \ be careful as we use HEAP and LOCALS
  2 LOCALS y! x!  \ LOCALS must be declared AFTER HEAP!

  x .EXP C@ %00111111 AND
  x .EXPS C@ IF  %01000000 OR THEN
  x .SIGN C@ IF  %10000000 OR THEN
  <>

  \ take care of rounding if least significant byte is >50
  x .MANT 3 + C@ $50 >=
  IF
    x y 7 CMOVE   \ copy x to y, fast, we have the signs and exponents
    y .MANT 0 OVER ! \ erase y's mantissa 2 most significant bytes
    2+ 1 SWAP !   \ place 1 in correct place in y's mantissa (3rd byte)
    \ 0 x 4 + C!  \ erase least significant byte of x. Uncessary, we'd sum it to 0 and ignore afterwards
    y x _F+
  THEN

  x .MANT SWAP OVER
  C@ OR

  SWAP 1+ @ <>

  -LOCALS
  -HEAP
;

\ High level F>>
: F>>
  7 HEAP
  'HEAP >R
  R@ F!  \ unpack the float from stack to heap
  R@ _F>>
  R> F@  \ repack the float from heap to stack
  -HEAP
;

\ High level F<<
: F<<
  7 HEAP
  'HEAP >R
  R@ F!  \ unpack the float from stack to heap
  R@ _F<<
  R> F@  \ repack the float from heap to stack
  -HEAP
;

:  MTABLE CREATE #16 ALLOT DOES> SWAP 2* 2* + ;

>RAM

\ 3 temporary float registers
FREG FR1
FREG FR2
FREG FR3

\ used in Float division
MTABLE MULTI
0 MULTI VALUE 'MULTI \ base add of MULTI table
0 VALUE DIGIT

>ROM

\ high level F+
: F+ ( f1 f2 ) \ takes two unpacked floats
  #14 HEAP \ reserves space for 2 unpacked floats
  'HEAP >R
  R@ 7 + F! \ unpack float f2 at 'heap+7
  R@ F!     \ unpack float f1 there at 'heap

  R@ DUP 7 +
  _F+

  \ result in f2
  R> 7 + F@ \ repack f2
  -HEAP
;

\ low level F2* doubles a float
: _F2* ( 'f  -- )
  >R
  R@ .MANT
  BCD2* \ call the primitive to duplicate the mantissa, retuns carry
  IF \ carry is 1?
    R@ _F>>
    R@ FRM1! \ store the carry in the mantissa
  THEN
  R> DROP
;

\ high level F2* doubles a float
: F2* ( f -- f*2 )
  #7 HEAP \ allocate space for 1 float register in heap (7 bytes)
  'HEAP >R \ keep 'heap in R as a temp variable
  R@ F! \ unpack the float in heap
  R@ _F2* \ call low level _F2*
  R> F@ \ repack the float and leave it on the stack
  -HEAP
;

\ low level F2/ (divide float by 2)
: _F2/ ( 'f -- )
  >R

  R@ .MANT
  BCD2/ \ call the primitive to halve- the mantissa

  \ we need to _F<< the float as long as 1rst digit is 0
  BEGIN
    R@ .MANT C@ F0 AND 0=
  WHILE
    R@ _F<<
  REPEAT

  R> DROP
;

\ high level F2/ (divide float by 2)
: F2/ ( f -- f/2 )
  #7 HEAP \ allocate space for 1 float register in heap (7 bytes)
  'HEAP >R \ keep 'heap in R as a temp variable
  R@ F! \ unpack the float in heap
  R@ _F2/ \ call low level _F2/
  R> F@ \ repack the float and leave it on the stack
  -HEAP
;

\ Low level Float right justify! (move digits all to the most right)
\ being the low level version, we use 4 bytes precision
: _FRJ ( 'f -- )
  >R
  BEGIN
    R@ .MANT 3 + C@ $0F AND 0=
  WHILE
    R@ _F>>
  REPEAT
  \ we'll consider the mantissa as integer so we have to reduce the exp by 7:
  R> DUP FSEXP@ 7 - SWAP FSEXP!
  \ R> DROP
;

\ High level Float right justify! (move digits all to the most right)
\ TODO: is it ever needed at all?
: FRJ ( f -- f )
  \ problem here is in the float register on the heap we have 4 byte mantissa
  \ but once repacked we have only 3 byte mantissa...
  \ multiplication should be in the heap from start to finish
  #7 HEAP \ allocate space for 1 float register in heap (7 bytes)
  'HEAP F! \ unpack the float in heap

  \ we need to _F<< the float as long as 1rst digit is 0
  BEGIN
    'HEAP .MANT 2 + C@ $0F AND 0=
  WHILE
    'HEAP _F>>
  REPEAT

  'HEAP F@ \ repack the float and leave it on the stack
  -HEAP
;

\ high level FNEG
: FNEG ( f -- -f )
  SWAP DUP 0< IF
    7FFF AND \ clear the negative bit
  ELSE
    8000 OR \ set the negative bit
  THEN
  SWAP
;

\ High level F-
: F- ( f1 f2 ) FNEG F+ ;

: locDBG
  CR
  .(   x: ) x DUP 7 + DUMP CR
  .(   y: ) y DUP 7 + DUMP CR
  .(   z: ) z DUP 7 + DUMP CR
  .S CR
  GETC DROP
;

: _F* ( f1 f2 f3 -- )
  \ multiply f1 x f2 leaving result in f3
  3 LOCALS \ f1 -> x, f2 -> y, f3 -> z
  z! y! x!

  \ flip y's sign if x's negative
  x .SIGN C@ IF
    y .SIGN C@ 1 XOR y .SIGN C!
  THEN

  \ we prepare x, right justify it (as integer). exponent is also adjusted accordingly
  x _FRJ

  \ add x' exponent to y's exponent
  x FSEXP@ y FSEXP@ +
  y FSEXP!

  \ store 0 in z
  0 0 z F!

  \ if x or y are 0, we exit (z is already cleared so 0 is the correct result)
  x _F0= y _F0= OR
  IF
    -LOCALS EXIT
  THEN

  BEGIN
    x .MANT 3 + C@ 1 AND \ test if x if odd
    IF
      y z _F+ \ Add y to z
    THEN

    x .MANT BCD2/  \ divide x by 2
    y _F2*      \ multiply y by 2

    \ exit clause: x == 0
    x _F0=
  UNTIL

  -LOCALS
;

: F* ( f1 f2 -- f3 )
  #21 HEAP 'HEAP >R
  R@ 7 + F!
  R@ F!

  R@      \ 'f1
  DUP 7 + \ 'f2
  DUP 7 + \ 'f3

  _F*

  R> #14 + F@ \ repack result
  -HEAP
;

\ high level Float SQuare
: FSQ 2DUP F* ;

\ Table of 2's powers up to 8
CREATE T2^ 1 C, 2 C, 4 C, 8 C,

: BT \ Build multiples table
   FR2 .MANT
   4 0
   DO
      I MULTI TUCK 4 CMOVE
      I IF
         DUP BCD2* DROP
      THEN
   LOOP DROP
;

\ compute next digit in float division
: NEXTDIGIT
   0 TO DIGIT
   4 0 DO
      3 I - >R
      R@ MULTI FR1 .MANT FRM>? 0=
      IF
         DIGIT R@ T2^ + C@ + TO DIGIT
         R@ MULTI FR3 .MANT 4 CMOVE \ copy the 2 MULTI to FR3 mantissa

         FR1 .MANT FR3 .MANT FRM- DROP  \ substract, results in FR3's mantissa
         FR3 .MANT FR1 .MANT 4 CMOVE    \ copy result back into FR1's mantissa
      THEN
      R> DROP
   LOOP
   FR1 .MANT BCDSL
;

: STOREDIGIT ( addr -- )
   \ stores the DIGIT in the mantissa at addr (displacing it to the left)
   .MANT >R
   R@ BCDSL
   R> 3 + DUP C@
   DIGIT OR SWAP C!
;

: FINV
  \ reserve 7 bytes for the result
  7 HEAP

  FR2 F!
  0010 0 FR1 F! \ store 1 in FR1

  \ copies FR2's sign to result
  FR2 .SIGN C@ 'HEAP .SIGN  C!

  FR1 _FRJ
  FR2 _FRJ

  \ build tables of multiples
  BT

  \ start with the exponent
  FR2 FSEXP@

  BEGIN
    NEXTDIGIT
    DIGIT 0=
  WHILE
    1+   \ While the division gives a 0, we reduce the exponent by 1
  REPEAT

  \ store the result's exponent
  NEG 'HEAP FSEXP!

  \ once we get here, DIGIT isn't 0 anymore --> first digit

  'HEAP DUP STOREDIGIT
  7 0 DO
    DUP NEXTDIGIT STOREDIGIT
  LOOP
  DROP

  'HEAP F@
  -HEAP
;

: F/ FINV F* ;

: D16* $10 * SWAP $10 UM* ROT + ;

: PARSE-FLOAT ( ADDR COUNT -- F )
  \ MANDATORY: YOU NEED TO USE . AND E in the string!
  \ parse float at addr/count into a float F
  0 0 FR1 F! \ clears FR1
  2 LOCALS
  0 x! \ flag to know if we are parsing mantisa (0) or exponent (1)
  0 y! \ number of non-zero digits before the decimal "."

  0 -ROT 0 -ROT \ start with a 0 0 mantissa

  OVER + SWAP DO
    I C@ >R \ R@ holds the char

    \ is it a digit?
    R@ $2F > R@ $3A < AND IF

      R@ CHAR 0 -

      x IF
        \ x=1, we're parsing exponent
        SWAP #10 * +
      ELSE
        \ x=0, we're parsing mantissa
        y 6 < IF
            -ROT D16* ROT M+
            y 1+ y! \ 1 more digit to y counter
        ELSE
            DROP
        THEN
      THEN

    ELSE
      \ is it a "-"
      R@ CHAR - = IF
        1 FR1
        x IF .EXPS ELSE .SIGN THEN
        C!
      THEN

      \ is it an "E"
      R@ CHAR E = IF
        1 x!
        0 \ now leave a 0 on the stack (to build the exponent)
      THEN

      R@ CHAR . = IF
        y 1 - FR1 .EXP C!
      THEN
    THEN

    R> DROP
  LOOP

  \ correct and store the exponent
  FR1 .EXP C@
  SWAP FR1 .EXPS IF NEG + NEG ELSE + THEN FR1 .EXP C!

  \ store the mantissa
  BEGIN
    DUP $F0 AND 0=
  WHILE
    D16*
  REPEAT
  FR1 .MANT SWAP OVER C!
  1+ SWAP <> SWAP !

  FR1 F@

  -LOCALS
;

: S>F WORD PARSE-FLOAT ;

: PI ( piFloat -- ) 0031 4159 ;

: DBG
  .( FR1: ) FR1 DUP 7 + DUMP .(  .M: ) FR1 .MANT . CR
  .( FR2: ) FR2 DUP 7 + DUMP .(  .M: ) FR2 .MANT . CR
  .( FR3: ) FR3 DUP 7 + DUMP .(  .M: ) FR3 .MANT . CR
;

: TEST
.( ADDITION ) CR
  .(  9 -1 F+ => )
  0090 0
  8010 0
  F+ F. CR

  .( -9  1 F+ => )
  8090 0
  0010 0
  F+ F. CR

  .( -9 -1 F+ => )
  8090 0
  8010 0
  F+ F. CR

  .(  9  1 F+ => )
  0090 0
  0010 0
  F+ F. CR

CR .( SUBSTRACTION ) CR
  .(  9 -1 F- => )
  0090 0
  8010 0
  F- F. CR

  .( -9  1 F- => )
  8090 0
  0010 0
  F- F. CR

  .( -9 -1 F- => )
  8090 0
  8010 0
  F- F. CR

  .(  9  1 F- => )
  0090 0
  0010 0
  F- F. CR

  .(  15  10 F- => )
  0115 0
  0110 0
  F- F. CR

  .(  9.0  9.1 F- => )
  0090 0
  0091 0
  F- F. CR

  .(  9.1  9.0 F- => )
  0091 0
  0090 0
  F- F. CR

CR .( MUL/DIV ) CR
  .( 1-PI/PI => )
  10 0 \ 1E0 in Float
  PI PI F/ F- F. CR
;


>RAM
MARKER

\ DON'T REMOVE THIS CODE block
>ROM
HERE 3 !    \ we save the end of ROM area at 0003
>RAM
HERE 7 !    \ we save the end of RAM area at 0007

LAST 9 !    \ we save LAST at 0009

1 0 C!      \ we signal the end of bootstrap compilation by saving 1 in 0000
