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
: 0< 8000 AND ; \ ( N -- F ) Is N strictly negative? Returns non 0 (~true) if N<0
: IMMEDIATE LAST SETIMM ; \	; sets the latest word IMMEDIATE
: ' WORD FIND >CFA ;
: ['] ' COMPILE LIT , ; IMMEDIATE
: [,] , ; IMMEDIATE \ take an XT on ToS, commit to dict

: +! SWAP OVER @ + SWAP ! ;

\ Comparison operators ( a b -- f ) for signed cells
: < - 0< ;
: > SWAP < ;
: <= > 0= ;
: >= SWAP <= ;

: IF COMPILE 0BR HERE HERE++ ; IMMEDIATE
: THEN HERE SWAP ! ; IMMEDIATE
: ELSE COMPILE JUMP HERE HERE++ SWAP HERE SWAP ! ; IMMEDIATE

: BEGIN HERE ; IMMEDIATE
: AGAIN COMPILE JUMP , ; IMMEDIATE

: UNTIL COMPILE 0BR , ; IMMEDIATE

: ( BEGIN KEY 29 = UNTIL ; IMMEDIATE \ ; now we can use ( ) inline comments!

: WHILE COMPILE 0BR HERE HERE++ ; IMMEDIATE
: REPEAT COMPILE JUMP SWAP , HERE SWAP ! ; IMMEDIATE

: >D DUP 0< 0= 0= ; \ ; Extends signed cell into signed double (0= 0= will convert any non 0 into FFFF)

: UM+ 0 SWAP 0 D+ ;

: NIP SWAP DROP ;
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
: POSTPONE ' , ; IMMEDIATE
: IS POSTPONE TO ; IMMEDIATE

>RAM

VARIABLE BP \ ; defines BP variable (Base Pointer)
_BP BP !

0 VALUE SIGNED \ change to non-0 for signed output

>ROM

: LOCALS  1+ BP @ DUP ROT 2* - DUP -ROT ! BP ! ; \ ( n -- ) Allocates n local variables
: -LOCALS BP DUP @ @ SWAP ! ;  \ Dellocates n local variables

: L@ BP @ + @ ; \  ; ( n -- value) helper word to get local var n
: L! BP @ + ! ; \  ; ( n -- value) helper word to save to local var n

: x 2 L@ ; : x! 2 L! ;
: y 4 L@ ; : y! 4 L! ;
: z 6 L@ ; : z! 6 L! ;
: t 8 L@ ; : t! 8 L! ;


: RECURSIVE REVEAL ; IMMEDIATE

: DUMP SWAP DUP . ?DO I C@ C. LOOP ; \ ( addr1 addr2 -- ) dumps memory from addr1 to addr2
: .LDUMP BP @ 4000 DUMP ; \ dump local stack

: .NAME DUP 2+ DUP C@ DUP 40 AND >R 1F AND SWAP
  1+ SWAP TYPE R> IF SPACE 2A EMIT THEN ;
: WORDS 0 LATEST BEGIN @ DUP WHILE DUP . DUP >CFA .
  .NAME CR SWAP 1+ DUP 10 = IF GETC 20 OR 71 =
  IF 2DROP EXIT THEN DROP 0 THEN SWAP REPEAT 2DROP ;

: .( [ ' S( , ] ?EXEC IF TYPE ELSE COMPILE TYPE THEN ; IMMEDIATE

: STRING CREATE HERE -ROT 1+ DUP ALLOT SWAP 1 - -ROT CMOVE ; \ Example: S( Alex) STRING NAME
: CHAR ?EXEC IF KEY ELSE COMPILE CLIT KEY C, THEN ; IMMEDIATE \ Example: CHAR " EMIT

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

: FREG CREATE 7 ALLOT ; \ 7 bytes per float register
: .SIGN      ; \ Sign 1 byte
: .MANT 1+   ; \ Mantisa 4 bytes BCD
: .EXPS 5 +  ; \ Exponent sign 1 byte
: .EXP  6 +  ; \ Exponent 1 byte

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
  z $FF AND
  x 1+ C!

  x 2+
  y <> OVER !

  0 SWAP 2+ C!
  -LOCALS
;

: F@
  1 LOCALS x!
  x .EXP C@ %00111111 AND
  x .EXPS C@ IF  %01000000 OR THEN
  x .SIGN C@ IF  %10000000 OR THEN
  <>
  x 1+ SWAP OVER
  C@ OR

  SWAP 1+ @ <>

  -LOCALS
;

: FSEXP@ \ return the signed exponent of a float (8 bit 2's complement )
  ( 'float -- signed_exp )
  DUP .EXP C@ SWAP .EXPS C@ IF NEG THEN
;

: FSEXP! \ stores the signed exponent back to a float var
  ( signed_exp 'float )
  SWAP
  DUP 0< IF 1 SWAP NEG ELSE 0 SWAP THEN
  -ROT OVER .EXPS C! .EXP C!
;

: F>> ( addr )
  DUP .MANT BCDSR
  \ increment exponent
  DUP FSEXP@ 1+ SWAP FSEXP!
;

: F<< ( addr )
  DUP .MANT BCDSL
  \ increment exponent
  DUP FSEXP@ 1 - SWAP FSEXP!
;

: FALIGN ( x y -- ) \ too float register addr
  4 LOCALS
  y! x!
  x FSEXP@ z! \ x's signed exponent
  y FSEXP@ t! \ y's signed exponent
  z t = IF EXIT THEN
  z t < IF
    \ we need to raise x's exponent z to t
    t z -
    8 MIN
    0 DO x F>> LOOP
  ELSE
    \ we need to lower x's exponent z to t
    z t -
    8 MIN
    0 DO y F>> LOOP
  THEN
  -LOCALS
;

>RAM
\ 2 temporary float registers
FREG FR1
FREG FR2
>ROM

: F+ ( f1 f2 )
  \ store both flots in floats register FR1 and FR2
  FR2 F!
  FR1 F!
  \ align both floats (to max exponent)
  FR1 FR2 FALIGN
  \ check the sign of both F1 and F2
  FR1 .SIGN C@
  FR2 .SIGN C@
  XOR IF
    \ F1 and F2 are different sign, we substract the mantissas

    \ find which one is bigger!
    FR1 1+
    FR2 1+
    FRM>? IF
      \ F1 is bigger
      FR1 1+ FR2 1+ FRM-
      \ deal with the carry - HOW
      DROP

      \ keep the sign of F1
      FR1 .SIGN C@ FR2 .SIGN C!
    ELSE
      \ F2 is bigger
      FR2 1+ FR1 1+ FRM-

      \ for some reason, result is in FR1, so we need to copy from FR1 to FR2... ugly hack
      FR1 1+ FR2 1+ 4 CMOVE
      \ deal with the carry - HOW
      DROP

      \ keep the sign of F2
      \ FR2 .SIGN C@ FR2 .SIGN C! \ no need to do this
    THEN

    \ we need to F<< FR2 as long as 1rst digit is 0
    BEGIN
      FR2 1+ C@ F0 AND 0=
    WHILE
      FR2 F<<
    REPEAT

  ELSE
    \ F1 and F2 are same sign, we add the mantissas
    FR1 1+ FR2 1+ FRM+ ( carry )
    IF \ carry is 1?
      FR2 F>>
      FR2 FRM1! \ store the carry in the mantissa
    THEN
  THEN
  FR2 F@
;

: FNEG ( f -- -f )
  SWAP DUP 0< IF
    7FFF AND \ clear the negative bit
  ELSE
    8000 OR \ set the negative bit
  THEN
  SWAP
;

: F- ( f1 f2 ) FNEG F+ ;

: DBG
  .( FR1: ) FR1 DUP 7 + DUMP .(  .M: ) FR1 .MANT . CR
  .( FR2: ) FR2 DUP 7 + DUMP .(  .M: ) FR2 .MANT . CR
;

: TEST
.( ADDITION ) CR
  .(  9 -1 F+ => )
  0090 0000
  8010 0000
  F+ F. CR

  .( -9  1 F+ => )
  8090 0000
  0010 0000
  F+ F. CR

  .( -9 -1 F+ => )
  8090 0000
  8010 0000
  F+ F. CR

  .(  9  1 F+ => )
  0090 0000
  0010 0000
  F+ F. CR

CR .( SUBSTRACTION ) CR
  .(  9 -1 F- => )
  0090 0000
  8010 0000
  F- F. CR

  .( -9  1 F- => )
  8090 0000
  0010 0000
  F- F. CR

  .( -9 -1 F- => )
  8090 0000
  8010 0000
  F- F. CR

  .(  9  1 F- => )
  0090 0000
  0010 0000
  F- F. CR

  .(  15  10 F- => )
  0115 0000
  0110 0000
  F- F. CR

  .(  9.0  9.1 F- => )
  0090 0000
  0091 0000
  F- F. CR

  .(  9.1  9.0 F- => )
  0091 0000
  0090 0000
  F- F.

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
