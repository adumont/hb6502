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

: FVAR CREATE #10 ALLOT ;
: .SIGN      ; \ Sign 1 byte
: .MANT 1+   ; \ Mantisa 4 bytes BCD
: .EXPS 8 + ; \ Exponent sign 1 byte
: .EXP  9 + ; \ Exponent 1 byte

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
  x .MANT x! \ x now contains MANT addr
  z $FF AND
  DUP 2/ 2/ 2/ 2/ x C!
  $0F AND x 1+ C!

  y <> $FF AND
  DUP 2/ 2/ 2/ 2/ x 2+ C!
  $0F AND x 3 + C!

  y $FF AND
  DUP 2/ 2/ 2/ 2/ x 4 + C!
  $0F AND x 5 + C!

  0 x 6 + C!
  -LOCALS
;

: F@
  1 LOCALS x!
  x .EXP C@ %00111111 AND
  x .EXPS C@ IF  %01000000 OR THEN
  x .SIGN C@ IF  %10000000 OR THEN
  <>
  x .MANT x! \ x now contains MANT addr

  x C@ 2* 2* 2* 2* OR
  x 1+ C@ OR

  x 2+  C@ 2* 2* 2* 2*
  x 3 + C@ OR <>

  x 4 + C@ 2* 2* 2* 2* OR
  x 5 + C@ OR

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

: F>>
  1 LOCALS x!
  0 ( 0 )
  x .MANT ( 0 'mant )
  DUP DUP 1+ ( 0 'mant 'mant 'mant+1 )
  6 ( 0 'mant 'mant 'mant+1 6 )
  CMOVE ( 0 'mant )
  C! ( )
  \ increment exponent
  x DUP FSEXP@ 1+ SWAP FSEXP!
  -LOCALS
;

: F<<
  1 LOCALS x!
  x .MANT DUP 1+ ( 'mant 'mant+1 )
  HERE ( 'mant 'mant+1 here )
  6 CMOVE
  HERE SWAP 6 CMOVE
  0 x 7 + C!

  \ decrement exponent
  x DUP FSEXP@ 1 - SWAP FSEXP!
  -LOCALS
;

: FALIGN ( x y -- ) \ too var floats addr
  \ TODO: this is not efficient. if there's more than 6 of diff between the exponents
  \ we'll loose all the significant digits and do F>>'s for nothing!
  4 LOCALS
  y! x!
  x FSEXP@ z! \ x's signed exponent
  y FSEXP@ t! \ y's signed exponent
  z t = IF EXIT THEN
  z t < IF
    \ we need to raise x's exponent z to t
    t z - 0 DO x F>> LOOP
  ELSE
    \ we need to lower x's exponent z to t
    z t - 0 DO y F>> LOOP
  THEN
  -LOCALS
;

: FRMANT+ ( 'x 'y -- addr carry ) \ add mantissas of 2 float registers x y, leave the 7 digits in t
  3 LOCALS y! x!
  0 z! \ carry
  8 0 DO
    x .MANT 7 I - + C@
    y .MANT 7 I - + C@
    +
    z +
    DUP 9 > IF
      6 +
      1 z!
      F AND \ we remove the carry from the byte
    ELSE
      0 z!
    THEN
    HERE 7 I - + C! \ store the resulting digit in HERE+7-I
  LOOP
  \ returns addr and carry
  HERE z
  -LOCALS
;

>RAM
\ 2 temp float registers
FVAR TMPF1
FVAR TMPF2
>ROM

: F+ ( f1 f2 )
  TMPF2 F!
  TMPF1 F!
  TMPF1 TMPF2 FALIGN
  TMPF1 TMPF2 FRMANT+ ( addr carry )
  SWAP ( carry addr )
  TMPF2 .MANT 7 CMOVE
  IF \ carry is 1?
  TMPF2 F>>
  1 TMPF2 .MANT C! \ store the carry in the mantissa
  THEN
  TMPF2 F@
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
